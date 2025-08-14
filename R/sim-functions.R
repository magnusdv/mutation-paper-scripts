library(pedsuite)
library(pedmut)
library(norSTR)
library(tidyverse)
library(mirai)


# Hard-coded parameters ---------------------------------------------------

# STR database
CODIS = norwayDB[markerSets$codis]


# Mutation models to consider
MODELS = list(
  Equal = list(model = "equal", rate = 0.001),
  Stepwise = list(model = "stepwise", rate = 0.001, rate2 = 1e-6, range = 0.1)
)


# -------------------------------------------------------------------------


# Main function
mutsim = function(ped, ids, db = CODIS, altPed = NULL, N = 1000, 
                  cores = NULL, seed = 1234) {
  
  st = Sys.time()
  
  # Attach database
  ped = setMarkers(ped, locusAttr = db)
  
  # Setup parallelisation
  daemons(cores %||% 0)
  on.exit(daemons(0), add = TRUE)
  
  cat("Dataset 1: No exclusions\n")
  zeroExcl = profileSim(ped, ids = ids, N = N, seed = seed, verbose = FALSE)
  
  # Generate many candidate datasets with integer-sized exclusions
  integerCandids = ped |> 
    setMutmod(model = "stepwise", rate = 0.1, rate2 = 0, range = 0.1) |>
    profileSim(ids = ids, N = 10*N, seed = seed, verbose = FALSE) |>
    setMutmod(model = NULL)
  
  # Count number of exclusions (under H1)
  likMat = likParallel(integerCandids)
  nExcl = colSums(likMat == 0)
  
  # Exclusions under altPed
  if(!is.null(altPed)) {
    likMatAlt = map(integerCandids, \(x) transferMarkers(x, altPed)) |> likParallel()
    nExclAlt = colSums(likMatAlt == 0)
    nExcl[nExclAlt > 0] = 99  # avoid those with exclusions in altPed
  }
  
  cat("Candidates with integer exclusions generated:\n")
  print(table(nExcl))
  
  cat("Dataset 2: Exactly 1 exclusion (int-sized)\n")
  oneExcl = integerCandids[which(nExcl == 1)[1:N]]  # first N with exactly 1
  
  cat("Dataset 3: Exactly 2 exclusions (both int-sized)\n")
  twoExcl = integerCandids[which(nExcl == 2)[1:N]]  # first N with exactly 2
  
  # Generate many candidate datasets with non-integer exclusions
  microCandids = ped |> 
    setMutmod(model = "stepwise", rate = 0, rate2 = 0.1, range = 0.1) |>
    profileSim(ids = ids, N = 10*N, seed = seed, verbose = FALSE) |>
    setMutmod(model = NULL)
  
  # Count exclusions
  likMicro = likParallel(microCandids)
  nMicro = colSums(likMicro == 0)
  
  # Exclusions under altPed
  if(!is.null(altPed)) {
    likMicroAlt = map(microCandids, \(x) transferMarkers(x, altPed)) |> likParallel()
    nMicroAlt = colSums(likMicroAlt == 0)
    nMicro[nMicroAlt > 0] = 99  # avoid those with exclusions in altPed
  }
  
  cat("Candidates with integer exclusions generated:\n")
  print(table(nMicro))
  
  cat("Dataset 4: Exactly 1 exclusion (non-int)\n")
  oneMicro = microCandids[which(nMicro == 1)[1:N]]  # first N with exactly 1
  
  cat("All 4 datasets generated; starting LR calculations\n")
  
  ### Main calculations: logLRs under each model + all transformations
  
  int0 = logLR(zeroExcl, models = MODELS)
  int1 = logLR(oneExcl, models = MODELS)
  int2 = logLR(twoExcl, models = MODELS)
  micro1 = logLR(oneMicro, models = MODELS)
  
  cat("Simulation completed in", format(Sys.time() - st, digits = 3), "\n")
  
  # Return as list
  list(
    int0 = int0,
    int1 = int1,
    int2 = int2,
    micro1 = micro1
  )
}


# Helper functions --------------------------------------------------------

# Summarise output from `mutsim()`: Compute D = log2(LRR) = log2(LR[M] / LR[R])
calcD = function(sims) {
  
  # Transformations present in the data
  levs = c("DA", "PM", "PR", "BA", "MH") |> 
    intersect(names(sims[[1]]))
  
  sims |>
    bind_rows(.id = "Exclusions") |>
    mutate(across(all_of(levs), ~ M - .)) |>
    pivot_longer(all_of(levs), names_to = "Transform", values_to = "D") |>
    mutate(
      Transform = factor(Transform, levels = levs) |> 
        fct_recode(`MH*` = "MH", `BA*` = "BA"),
      Model = fct_inorder(Model),
      Exclusions = fct_inorder(Exclusions) |> 
        fct_recode("No exclusions"    = "int0",
                   "1 excl (int)"     = "int1",
                   "2 excl's (int)"   = "int2",
                   "1 excl (non-int)" = "micro1")
    )
}


# Parallel version of sapply(xlist, likelihood)
likParallel = function(xlist) {
  liklist = xlist |> map(in_parallel(\(x) pedprobr::likelihood(x)))
  do.call(cbind, liklist)
}
  
# Set mutation model and compute total log-likelihoods for each sim
loglikMut = function(x, argsM, transform = NULL) {

  # Set model; return NULL if fail
  if(!is.null(argsM)) {
    tryCatch(
      x <- map(x, in_parallel(\(xx)
        do.call(pedtools::setMutmod, c(argsM, list(x = xx, transform = transform))),
        argsM = argsM, transform = transform)),
      error = \(e) NULL) %||% return()
  }

  # Compute log-likelihoods
  map_dbl(x, in_parallel(\(xx) sum(pedprobr::likelihood(xx, logbase = 2))))
}


# Function for computing logLRs under different transformations
logLR = function(x, models, altPed = NULL, includeDawid = FALSE) {
  
  cat("Starting logLR at", format(Sys.time()), "\n")

  # Unrelated alternative
  altPed = altPed %||% singletons(id = typedMembers(x[[1]]))
  alt = lapply(x, transferMarkers, to = altPed)

  # Baseline (no model)
  G = loglikMut(x, argsM = NULL) - loglikMut(alt, argsM = NULL)

  # If Dawid: Do separately (not included in `models`)
  DA = NULL
  if(includeDawid) {
    dawid = list(model = "dawid", rate = 0.001, rate2 = 1e-6, range = 0.1)
    DA = loglikMut(x, argsM = dawid) - loglikMut(alt, argsM = dawid)
  }

  results = lapply(models, function(argsM) {
    cat("Model:", argsM$model, "\n")

    # Model without transformation
    M = loglikMut(x, argsM) - loglikMut(alt, argsM)

    # With transformations
    MH = loglikMut(x, argsM, transform = "MH") - loglikMut(alt, argsM, transform = "MH")
    BA = loglikMut(x, argsM, transform = "BA") - loglikMut(alt, argsM, transform = "BA")
    PR = loglikMut(x, argsM, transform = "PR") - loglikMut(alt, argsM, transform = "PR")
    PM = loglikMut(x, argsM, transform = "PM") - loglikMut(alt, argsM, transform = "PM")

    # Return logLR
    tibble(G = G, DA = DA, M = M, MH = MH, BA = BA,
           PR = if(length(PR)) PR else NULL,
           PM = if(length(PM)) PM else NULL)
  })

  bind_rows(results, .id = "Model")
}


