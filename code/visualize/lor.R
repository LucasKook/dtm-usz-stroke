
# Code to produce Figure 6

# Dep ---------------------------------------------------------------------

library(tidyverse)
library(ggbeeswarm)
library(colorspace)
library(patchwork)
library(tram)

# Params ------------------------------------------------------------------

source("code/functions/functions_DE.R")

in_dir <- "code/results/"
out_dir <- "code/results/figures/"

# Load results ------------------------------------------------------------

indivnll <- read.csv(paste0(in_dir, "lor_nll.csv"))
indivnll$mod <- factor(indivnll$mod, levels = c("sils", "silscs", "cils"))

# Plot --------------------------------------------------------------------

ornll <- pl_or(indiv = indivnll, width = 0.5, refline = TRUE,
               weighted = FALSE, members = FALSE, lbetvar = T,
               pooled_only = T)
ornll
ggsave(paste0(out_dir, "figure6.pdf"), height = 6.5, width = 9)
