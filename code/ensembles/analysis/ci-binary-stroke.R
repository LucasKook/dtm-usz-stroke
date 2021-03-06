# Bootstrap confidence intervals (binary mRS)
# Andrea Goetschi
# May 2022

# Dependencies ------------------------------------------------------------

library(etram)
library(boot)

# Directories -------------------------------------------------------------

source("functions/functions_DE.R")
in_dir <- out_dir <- "results/"

# Params ------------------------------------------------------------------

fname_silscsnll <- "stroke_silscs_lossnll_wsyes_augyes"
fname_cilsnll <- "stroke_cils_lossnll_wsyes_augyes"
fname_cilsmrsblnll <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"
fname_cilsmrsblrps <- "stroke_cilsmrsbl_lossrps_wsyes_augyes"
fname_sicsnll <- "stroke_sics_lossnll_wsyes_augyes"
fname_cinll <- "stroke_ci_lossnll_wsno_augyes"
fname_cirps <- "stroke_ci_lossrps_wsno_augyes"
fname_cimrsbinarynll <- "stroke_cimrsbinary_lossnll_wsno_augyes"

fname_sinll <- "stroke_si"
fname_silsnll <- "stroke_sils"

nsemi <- 6
K <- 2

# Load results ------------------------------------------------------------

## all CDF
cdf_files <- list.files(path = in_dir,
                        pattern = paste0("stroke_merged_bincdf.*\\.csv$"))
cdf_files <- lapply(cdf_files, function(fname) {
  read.csv(paste0(in_dir, fname))
})
all_cdf <- do.call("rbind", cdf_files)

## all Y
all_y <- read.csv(paste0(in_dir, "stroke_merged_biny.csv"))

## Load results of reference model SI-LS
met_sils <-  read.csv(file = paste0(in_dir, "met_", fname_silsnll, ".csv"))
met_sils <- met_sils %>% filter(!(metric %in% c("nll", "eacc"))) %>% 
  mutate(metric = replace(metric, metric == "binnll", "nll"),
         metric = replace(metric, metric == "ebinacc", "eacc"))

# Confidence interval -----------------------------------------------------

args_nll <- data.frame(cdf_all = "all_cdf", y_true_all = "all_y",
                       met_ref = "met_sils",
                       binary = TRUE,
                       mod = c(rep(c("silscs", "cils", "cilsmrsbl", 
                                     "sics", "ci", "cimrsbinary"), each = 10),
                               rep(c("sils", "si"), each = 1)),
                       meth = c(rep(c("linear", "log-linear", "trafo", "avg",
                                      "linear", "log-linear", "trafo",
                                      "avg", "avgll", "avgtrf"), nsemi),
                                rep(NA, 2)),
                       K = K, loss = "nll",
                       weighted = c(rep(c(rep(FALSE, 4), rep(TRUE, 6)), nsemi),
                                    rep(c(FALSE), 2)),
                       avg = c(rep(c(FALSE, FALSE, FALSE, TRUE,
                                     FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), nsemi),
                               rep(FALSE, 2)),
                       fname = c(rep(c(fname_silscsnll, fname_cilsnll, fname_cilsmrsblnll,
                                       fname_sicsnll, fname_cinll, fname_cimrsbinarynll), each = 10),
                                 rep(c(fname_silsnll, fname_sinll), each = 1)),
                       in_dir = in_dir,
                       out_dir = out_dir)

do.call(Map, c(f = boot_ci, args_nll))
