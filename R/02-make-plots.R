
source("sim-functions.R")
source("plot-functions.R")


# Simulation figure: Case A (paternity duo) ----------------------------------

duodata = readRDS(here("output/data/duo-data.rds"))

pA = plotDensities(duodata, cut = 1.7) 
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
  "No exclusions", -0.14, 0.14, NA, NA,
  "1 excl (int)", -1.9, 1.9, 0,   3,
  "2 excl's (int)", -1.9, 1.9, 0,   3,
  "1 excl (non-int)", -1.9, 1.9, 0,  3
)

pB = plotDensities2(sibdat, panel_limits = lims)
pB

ggsave(here("output/figures/sibSims.pdf"), pB, width = 9, height = 5)