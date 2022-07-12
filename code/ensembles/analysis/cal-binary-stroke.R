# Calibration stroke data (binary mRS)
# Andrea Goetschi
# April 2022

# Deps --------------------------------------------------------------------

library(etram)
library(caret)
library(Hmisc)

# Params ------------------------------------------------------------------

source("functions/functions_DE.R")
in_dir <- out_dir <- "results/"

fname_silscsnll <- "stroke_silscs_lossnll_wsyes_augyes"
fname_cilsnll <- "stroke_cils_lossnll_wsyes_augyes"
fname_cilsmrsblnll <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"
fname_sicsnll <- "stroke_sics_lossnll_wsyes_augyes"
fname_cinll <- "stroke_ci_lossnll_wsno_augyes"
fname_cimrsbinarynll <- "stroke_cimrsbinary_lossnll_wsno_augyes"

fname_silsnll <- "stroke_sils"

nsemi <- 6
K <- 2

# Functions ---------------------------------------------------------------

cutf <- function(x) {
  quantile(x, c(0.25, 0.5, 0.75))
}

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

# Calibration -------------------------------------------------------------

args_nll <- data.frame(cdf_all = "all_cdf", y_true_all = "all_y",
                       mod = c(rep(c("silscs", "cils", "cilsmrsbl",
                                     "sics", "ci", "cimrsbinary"), each = 10),
                               rep(c("sils"), each = 2)),
                       meth = c(rep(c("linear", "log-linear", "trafo", "avg",
                                      "linear", "log-linear", "trafo",
                                      "avg", "avgll", "avgtrf"), nsemi),
                                rep(NA, 2)),
                       K = K, loss = "nll",
                       weighted = c(rep(c(rep(FALSE, 4), rep(TRUE, 6)), nsemi),
                                    rep(c(FALSE, TRUE), each = 1)),
                       avg = c(rep(c(FALSE, FALSE, FALSE, TRUE,
                                     FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), nsemi),
                               rep(FALSE, 2)),
                       cuts_fun = "cutf",
                       fname = c(rep(c(fname_silscsnll, fname_cilsnll, fname_cilsmrsblnll,
                                       fname_sicsnll, fname_cinll, fname_cimrsbinarynll), each = 10),
                                 rep(c(fname_silsnll), each = 2)),
                       in_dir = in_dir)

res_nll <- do.call(Map, c(f = calc_calibration, args_nll))
res_nll <- bind_rows(res_nll)

# Save results ------------------------------------------------------------

write.csv(res_nll, file = paste0(out_dir, "bincal_avgnll.csv"))
