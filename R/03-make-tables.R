suppressPackageStartupMessages({
  library(here)
  library(clipr)
  source(here("R/table-functions.R"))
})

# Case A Table ------------------------------------------------------------

# Load dataset
duodata = readRDS(here("output/data/duo-data.rds"))

# Make summary table
tabA = duodata |> makeSummary() |> print()

# Save as TSV
write_tsv(tabA, here("output/tables/duo-summary.tsv"))

# GT version
gtA = tabA |> makeGT()
gtA

# Copy LaTeX version to clipboard (for inclusion in .tex)
gtA |> as_latex_clean() |> clipr::write_clip()


# Case B ------------------------------------------------------------

# Load dataset
sibdata = readRDS(here("output/data/sib-data.rds"))

# Make summary table
tabB = sibdata |> makeSummary() |> print()

# Save as TSV
write_tsv(tabB, here("output/tables/sib-summary.tsv"))

# GT version
gtB = tabB |> makeGT() |> print()

# Copy LaTeX version to clipboard (for inclusion in .tex)
gtB |> as_latex_clean() |> clipr::write_clip()



# f-ratio table -----------------------------------------------------------

# Uses MODELS from sim-functions.R

fdata = map(MODELS, \(mod) 
    map_dfr(CODIS, \(m) getFratios(m, mod), .id = "Marker")
  ) |> 
  map(~ rename(.x, "BA*" = BA, "MH*" = MH)) |> 
  bind_rows(.id = "Model") |> 
  mutate(across(`BA*`:PR, ~ round(.x, 3))) |> 
  pivot_wider(
    names_from = Model,
    values_from = c("BA*", "MH*", PM, PR),
    names_glue = "{Model}|{.value}", 
    names_vary = "slowest"
  ) |> print()


# Save as TSV
write_tsv(fdata, here("output/tables/f-ratios.tsv"))

# GT version
ftab = fdata |> 
  gt() |> 
  opt_vertical_padding(0.5) |>
  tab_options(table.width = NULL) |> 
  tab_spanner_delim(delim = "|") |>
  fmt(
    columns = where(is.numeric),
    fns = \(x) ifelse(x < 1e4, 
                      sprintf("%.1f", x), 
                      sprintf("%.1e", x) |> str_remove(fixed("+")))
  )
ftab

# Copy LaTeX version to clipboard
ftab |> as_latex_clean() |> clipr::write_clip()
