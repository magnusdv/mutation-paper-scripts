suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
  library(here)
  source(here("R/sim-functions.R"))  # for `calcD()`
})


makeSummary = function(x) {
  x |>
    calcD() |>
    summarise(
      min = min(D), mean = mean(D),
      max = max(D), sd = sd(D),
      .by = c(Exclusions, Model, Transform)
    ) |>
    mutate(
      Transform = factor(Transform, levels = c("BA*","MH*","PM","PR")),
      Exclusions = fct_relabel(Exclusions, ~ sub("excl ", "exclusion ", .x)),
      Exclusions = fct_relabel(Exclusions, ~ sub("excl's ", "exclusions ", .x)),
      range = sprintf("(%.2f, %.2f)", min, max),
      meansd = sprintf("%.3f (%.2f)", mean, sd),
      .keep = "unused"
    ) |>
    pivot_wider(
      id_cols = c(Exclusions, Transform),
      names_from = Model,
      values_from = c(range, meansd),
      names_glue = "{Model}|{.value}",
      names_vary = "slowest"
    ) |>
    arrange(Exclusions, as.integer(Transform))
}


# Convert to gt table
makeGT = function(x) {
  x |>
    dplyr::rename_with(\(s) s |>
                         str_replace_all("Equal",    "'equal'") |>
                         str_replace_all("Stepwise", "'stepwise'") |>
                         str_replace_all("meansd",   "mean (sd)")
    ) |>
    gt(rowname_col = "Transform", groupname_col = "Exclusions") |>
    opt_vertical_padding(0.5) |>
    tab_options(table.width = NULL) |>
    tab_spanner_delim(delim = "|") |>
    tab_stub_indent(rows = everything(), indent = 3)
}


# Function to clean up LaTeX output from gt tables
as_latex_clean = function(x) {
  x |>
    as_latex() |>
    str_remove_all("\\[2.5pt\\]") |>
    str_remove_all("\\\\addlinespace") |>
    str_replace_all("tabular\\*", "tabular") |>
    str_replace_all("hspace\\*", "hspace") |>
    str_remove("\\|") |>
    str_remove("\\{\\\\linewidth\\}") |>
    str_remove("@\\{\\\\extracolsep\\{\\\\fill\\}\\}") |>
    str_replace("\\\\fontsize.*selectfont", "\\\\centering\\\\small")
}


# f-ratios for all transformations of a given mutation model for a single marker
getFratios = function(afreq, modelparams, transforms = c("BA","MH","PM","PR")) {
  modelparams$afreq = afreq

  # Untransformed model
  M = do.call(mutationMatrix, modelparams)

  # Transform and find f
  sapply(transforms, function(trans) {
    R = do.call(mutationMatrix, append(modelparams, list(transform = trans)))
    max(c(M/R, R/M))
  })
}
