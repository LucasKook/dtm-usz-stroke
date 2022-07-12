# Test performance and calibration plots of all models
# Andrea Goetschi
# May 2022

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(colorspace)
library(ggbeeswarm)
library(patchwork)

# Paths -------------------------------------------------------------------

source("functions/functions_DE.R")

in_dir <- "results/"
out_dir <- "results/figures/"

fname_silscsnll <- "stroke_silscs_lossnll_wsyes_augyes"
fname_cilsnll <- "stroke_cils_lossnll_wsyes_augyes"
fname_cilsmrsblnll <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"
fname_sicsnll <- "stroke_sics_lossnll_wsyes_augyes"
fname_cinll <- "stroke_ci_lossnll_wsno_augyes"
fname_cimrsbinarynll <- "stroke_cimrsbinary_lossnll_wsno_augyes"

fname_sils <- "stroke_sils"
fname_si <- "stroke_si"

# Params ------------------------------------------------------------------

spl <- 6
ens <- 5

# Results performance -----------------------------------------------------

## NLL

### SILSCS

met_silscsnll <- read.csv(file = paste0(in_dir, "met_", fname_silscsnll, ".csv"))
met_silscsnll <- met_silscsnll[met_silscsnll$topn == ens, -2]
met_silscsnll$mod <- "silscs"

indiv_silscsnll <- read.csv(file = paste0(in_dir, "indivmet_", fname_silscsnll, ".csv"))
indiv_silscsnll$mod <- "silscs"

### CILS

met_cilsnll <- read.csv(file = paste0(in_dir, "met_", fname_cilsnll, ".csv"))
met_cilsnll <- met_cilsnll[met_cilsnll$topn == ens, -2]
met_cilsnll$mod <- "cils"

indiv_cilsnll <- read.csv(file = paste0(in_dir, "indivmet_", fname_cilsnll, ".csv"))
indiv_cilsnll$mod <- "cils"

### CILS MRS BL

met_cilsmrsblnll <- read.csv(file = paste0(in_dir, "met_", fname_cilsmrsblnll, ".csv"))
met_cilsmrsblnll <- met_cilsmrsblnll[met_cilsmrsblnll$topn == ens, -2]
met_cilsmrsblnll$mod <- "cilsmrsbl"

indiv_cilsmrsblnll <- read.csv(file = paste0(in_dir, "indivmet_", fname_cilsmrsblnll, ".csv"))
indiv_cilsmrsblnll$mod <- "cilsmrsbl"

### SICS

met_sicsnll <- read.csv(file = paste0(in_dir, "met_", fname_sicsnll, ".csv"))
met_sicsnll <- met_sicsnll[met_sicsnll$topn == ens, -2]
met_sicsnll$mod <- "sics"

indiv_sicsnll <- read.csv(file = paste0(in_dir, "indivmet_", fname_sicsnll, ".csv"))
indiv_sicsnll$mod <- "sics"

### CI

met_cinll <- read.csv(file = paste0(in_dir, "met_", fname_cinll, ".csv"))
met_cinll <- met_cinll[met_cinll$topn == ens, -2]
met_cinll$mod <- "ci"

indiv_cinll <- read.csv(file = paste0(in_dir, "indivmet_", fname_cinll, ".csv"))
indiv_cinll$mod <- "ci"

### CI MRS BINARY

met_cimrsbinarynll <- read.csv(file = paste0(in_dir, "met_", fname_cimrsbinarynll, ".csv"))
met_cimrsbinarynll <- met_cimrsbinarynll[met_cimrsbinarynll$topn == ens, -2]
met_cimrsbinarynll$mod <- "cimrsbin"

indiv_cimrsbinarynll <- read.csv(file = paste0(in_dir, "indivmet_", fname_cimrsbinarynll, ".csv"))
indiv_cimrsbinarynll$mod <- "cimrsbin"

### SI

met_sinll <-  read.csv(file = paste0(in_dir, "met_", fname_si, ".csv"))
met_sinll$mod <- "si"

### SILS

met_silsnll <-  read.csv(file = paste0(in_dir, "met_", fname_sils, ".csv"))
met_silsnll$mod <- "sils"

# Bootstrap confidence intervals ------------------------------------------

## Binary

# ci nll
f_nll_nw <- list.files(in_dir, pattern = "^binboot.*nll.*\\.csv$") # ^: start, $: end, .*: any pattern,
lys_nll_nw <- lapply(f_nll_nw, function(x) read.csv(paste0(in_dir, x)))
cibin_nll_nw <- do.call("rbind", lys_nll_nw)

# nll is similar to binary nll here
cibin_nll_nw <- cibin_nll_nw %>% 
  mutate(metric = replace(metric, metric == "nll", "binnll"),
         metric = replace(metric, metric == "eacc", "ebinacc"),
         metric = replace(metric, metric == "dnll", "dbinnll"),
         metric = replace(metric, metric == "deacc", "debinacc"),
         mod = replace(mod, mod == "cimrsbinary", "cimrsbin"))


## Ordinal

# ci nll
f_nll_nw <- list.files(in_dir, pattern = "^boot.*nll.*\\.csv$") # ^: start, $: end, .*: any pattern,
lys_nll_nw <- lapply(f_nll_nw, function(x) read.csv(paste0(in_dir, x)))
ciord_nll_nw <- do.call("rbind", lys_nll_nw)

# Combine -----------------------------------------------------------------

met_negloglik <- bindr(pat1 = "met", pat2 = "nll")
indiv_negloglik <- bindr(pat1 = "indiv", pat2 = "nll")

# Reorder levels ----------------------------------------------------------

bin_metrics <- c("binnll", "brier", "eauc", "ebinacc")
ord_metrics <- c("nll", "rps", "eqwk")
mods <- c("si", "sils", "sics", "silscs", "cimrsbin", "ci", "cilsmrsbl", "cils")

met_negloglik <- met_negloglik %>% mutate(method = ifelse(method == "linear" & mod %in% c("si", "sils"), "ref", method))
met_negloglik <- relev(met_negloglik, "metric", c(bin_metrics, ord_metrics))
met_negloglik <- relev(met_negloglik, "method", c("trafo", "avg", "ref"))
met_negloglik <- relev(met_negloglik, "mod", mods)
met_negloglik$spl <- factor(met_negloglik$spl)
indiv_negloglik <- relev(indiv_negloglik, "metric", c(bin_metrics, ord_metrics))


# Results calibration -----------------------------------------------------

avg_nll <- read.csv(paste0(in_dir, "cal_avgnll.csv"))
binavg_nll <- read.csv(paste0(in_dir, "bincal_avgnll.csv"))

spl_nll <- read.csv(paste0(in_dir, "cal_splnll.csv"))
binspl_nll <- read.csv(paste0(in_dir, "bincal_splnll.csv"))

# Prep --------------------------------------------------------------------

# Ordinal

ordmodlev <- c("cils", "cilsmrsbl", "ci", "silscs", "sics")

avgnll <- avg_nll %>% filter(!(mod %in% c("si", "sils"))) %>% 
  mutate(mod = factor(mod, levels = ordmodlev),
         method = factor(method, levels = c("avg", "linear",
                                            "avgll", "log-linear",
                                            "avgtrf", "trafo")))

splnll <- spl_nll %>% filter(!(mod %in% c("si", "sils"))) %>% 
  mutate(mod = factor(mod, levels = ordmodlev),
         method = factor(method, levels = c("avg", "linear",
                                            "avgll", "log-linear",
                                            "avgtrf", "trafo")))


avgrefnll <- avg_nll %>% filter(mod %in% c("sils")) %>% 
  mutate(mod = factor(mod, levels = c("sils"))) %>%
  select(-c("method"))

splrefnll <- spl_nll %>% filter(mod %in% c("sils")) %>% 
  mutate(mod = factor(mod, levels = c("sils"))) %>%
  select(-c("method"))


# Binary

binmodlev <- c("cils", "cilsmrsbl", "ci", "cimrsbinary", "silscs", "sics")

binavgnll <- binavg_nll %>% filter(!(mod %in% c("si", "sils"))) %>% 
  mutate(mod = factor(mod, levels = binmodlev),
         method = factor(method, levels = c("avg", "linear",
                                            "avgll", "log-linear",
                                            "avgtrf", "trafo")))

binsplnll <- binspl_nll %>% filter(!(mod %in% c("si", "sils"))) %>% 
  mutate(mod = factor(mod, levels = binmodlev),
         method = factor(method, levels = c("avg", "linear",
                                            "avgll", "log-linear",
                                            "avgtrf", "trafo")))

binavgrefnll <- binavg_nll %>% filter(mod %in% c("sils")) %>% 
  mutate(mod = factor(mod, levels = c("sils"))) %>%
  select(-c("method"))

binsplrefnll <- binspl_nll %>% filter(mod %in% c("sils")) %>% 
  mutate(mod = factor(mod, levels = c("sils"))) %>%
  select(-c("method"))

# Binary performance Plots ------------------------------------------------

## absolute

pl_binnll <- pl_met(spl_met = met_negloglik %>% filter(method == "trafo" | mod %in% c("si", "sils")),
                    metrics = bin_metrics,
                    ci = cibin_nll_nw %>% filter(method == "trafo" | mod %in% c("si", "sils")),
                    ref = c("si", "sils"),
                    rel = FALSE,
                    legend = FALSE,
                    weighted = FALSE,
                    ebarwidth = 0.2)

## relative

pl_binnll_rel <- pl_met(spl_met = met_negloglik %>% filter(method == "trafo" | mod %in% c("si", "sils")),
                        metrics = bin_metrics,
                        ci = cibin_nll_nw %>% filter(method == "trafo" | mod == "si"),
                        ref = "si",
                        rel = TRUE,
                        legend = FALSE,
                        weighted = FALSE,
                        ebarwidth = 0.2)
pl_binnll_rel
ggsave(paste0(out_dir, "figureB2.pdf"), height = 3, width = 11.5)

# Ordinal performance plots -----------------------------------------------

## abosulte

pl_ordnll <- pl_met(spl_met = met_negloglik %>% filter(method == "trafo" | mod %in% c("si", "sils")),
                    metrics = ord_metrics,
                    ci = ciord_nll_nw %>% filter(method == "trafo" | mod %in% c("si", "sils")),
                    ref = c("si", "sils"),
                    rel = FALSE,
                    legend = FALSE,
                    weighted = FALSE,
                    ebarwidth = 0.2)

## relative

met_negloglik <- met_negloglik  %>% filter(mod != "cimrsbin") %>%
  mutate(mod = factor(mod, c("si", "sils", "sics", "silscs", "ci", "cilsmrsbl", "cils")))

pl_ordnll_rel <- pl_met(spl_met = met_negloglik %>% filter(method == "trafo" | mod %in% c("si", "sils")),
                        metrics = ord_metrics,
                        ci = ciord_nll_nw %>% filter(method == "trafo" | mod == "si"),
                        ref = "si",
                        rel = TRUE,
                        legend = FALSE,
                        weighted = FALSE,
                        ebarwidth = 0.2)
pl_ordnll_rel
ggsave(paste0(out_dir, "figureB3.pdf"), height = 3, width = 8.8)

# Calibration plots -------------------------------------------------------

# Binary

# ci for each split
binspl_nll <- pl_cal(avg = binavgnll %>% filter(method == "trafo", weights == "equal"), 
                     avg_ref = binavgrefnll %>% filter(weights == "equal"),
                     spl_ref = binsplrefnll %>% filter(weights == "equal"),
                     spl = binsplnll %>% filter(method == "trafo", weights == "equal"),
                     legend = FALSE, weighted = FALSE,
                     psize = 0.8, lsize = 0.4, ebarsize = 0.4, 
                     strokep = TRUE)
binspl_nll

# Ordinal

# ci for each split
spl_nll <- pl_cal(avg = avgnll %>% filter(method == "trafo", weights == "equal"), 
                  avg_ref = avgrefnll %>% filter(weights == "equal"),
                  spl_ref = splrefnll %>% filter(weights == "equal"),
                  spl = splnll %>% filter(method == "trafo", weights == "equal"),
                  legend = FALSE, weighted = FALSE,
                  psize = 0.8, lsize = 0.4, ebarsize = 0.4, 
                  strokep = T, ncol = 3)
spl_nll

# Combine plots -----------------------------------------------------------

# binary

plbin <- pl_binnll + labs(tag = "A") + 
         binspl_nll + labs(tag = "B")
plbin + plot_layout(ncol = 1, heights = c(1, 1.7))
ggsave(paste0(out_dir, "figure4.pdf"), height = 7, width = 8.5)

# ordinal

plord <- pl_ordnll + labs(tag = "A") + 
         spl_nll + labs(tag = "B")
plord + plot_layout(ncol = 1, heights = c(1, 1.7))
ggsave(paste0(out_dir, "figure5.pdf"), height = 8, width = 7.5)
