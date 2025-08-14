library(pedsuite)
library(here)

source(here("R/sim-functions.R"))

nsims = 1000
cores = 10


# Case A: Paternity duo ------------------------------------------------------

ped = nuclearPed()
ids = c(1,3)

# Generate sims
duodata = mutsim(ped, ids, N = nsims, cores = cores)

# Save in output folder
saveRDS(duodata, here("output/data/duo-data.rds"))



# Case B: Sib3 vs sib2+half ---------------------------------------------------------

ped = nuclearPed(3)
ids = c(3:5)
altPed = nuclearPed(2) |> addSon(2, id = 5)

# Inspect peds
plotPedList(list(ped, altPed), hatched = ids)

# Generate sims
sibdata = mutsim(ped, ids, altPed = altPed, N = nsims, cores = cores)

# Save in output folder
saveRDS(sibdata, here("output/data/sib-data.rds"))

