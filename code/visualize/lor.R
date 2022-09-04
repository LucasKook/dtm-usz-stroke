
# Code to produce Figure 6

# Dep ---------------------------------------------------------------------

library(tidyverse)
library(ggbeeswarm)
library(colorspace)
library(patchwork)
library(tram)

# Params ------------------------------------------------------------------

source(file.path("code", "functions", "functions_DE.R"))

in_dir <- "intermediate-results"
out_dir <- "figures"

if (!dir.exists(out_dir))
  dir.create(out_dir)

# Load results ------------------------------------------------------------

indivnll <- read.csv(file.path(in_dir, "lor_nll.csv"))
indivnll$mod <- factor(indivnll$mod, levels = c("sils", "silscs", "cils"))

# Plot --------------------------------------------------------------------

ornll <- pl_or(indiv = indivnll, width = 0.5, refline = TRUE,
               weighted = FALSE, members = FALSE, lbetvar = T,
               pooled_only = T)
ornll
ggsave(file.path(out_dir, "figure6.pdf"), height = 6.5, width = 9)
