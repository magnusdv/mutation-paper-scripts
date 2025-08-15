library(here)
source(here("R/sim-functions.R"))
source(here("R/plot-functions.R"))


# Simulation figure: Case A (paternity duo) ----------------------------------

duodata = readRDS(here("output/data/duo-data.rds"))

pA = plotDensities(duodata, cut = 3)
pA

ggsave(here("output/figures/duoSims.pdf"), pA, width = 9, height = 5)



# Case B: Siblings --------------------------------------------------------

sibdata = readRDS(here("output/data/sib-data.rds"))

# Doesn't look good
plotDensities(sibdata, cut = 2)

# Attempt second version
plotDensities2(sibdata)

# Best: Specifying limits of each panel
lims = tribble(
  ~Exclusions, ~xmin, ~xmax, ~ymin, ~ymax,
  "No exclusions", -0.2, 0.25, NA, NA,
  "1 excl (int)", -2, 3, 0,   1.5,
  "2 excl's (int)", -3, 4, 0,   1.5,
  "1 excl (non-int)", -3, 5, 0,  1.5
)

pB = plotDensities2(sibdata, panel_limits = lims)
pB

ggsave(here("output/figures/sibSims.pdf"), pB, width = 9, height = 5)
