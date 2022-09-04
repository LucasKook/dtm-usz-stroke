
# Calculate calibration for models evaluated using ordinal mRS

# Deps --------------------------------------------------------------------

library(etram)
library(caret)
library(Hmisc)

# Params ------------------------------------------------------------------

source(file.path("code", "functions", "functions_DE.R"))
in_dir <- "intermediate-results"
out_dir <- "results"

if (!dir.exists(out_dir))
  dir.create(out_dir)

fname_silscsnll <- "stroke_silscs_lossnll_wsyes_augyes"
fname_cilsnll <- "stroke_cils_lossnll_wsyes_augyes"
fname_cilsmrsblnll <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"
fname_sicsnll <- "stroke_sics_lossnll_wsyes_augyes"
fname_cinll <- "stroke_ci_lossnll_wsno_augyes"
fname_silsnll <- "stroke_sils"

nsemi <- 5
K <- 7

# Functions ---------------------------------------------------------------

cutf <- function(x) {
  quantile(x, c(0.25, 0.5, 0.75))
}

# Load results ------------------------------------------------------------

## all CDF
cdf_files <- list.files(path = in_dir, pattern = "stroke_merged_cdf.*\\.csv$")
cdf_files <- lapply(cdf_files, function(fname) {
  read.csv(file.path(in_dir, fname))
})
all_cdf <- do.call("rbind", cdf_files)

## all Y
all_y <- read.csv(file.path(in_dir, "stroke_merged_y.csv"))

# Calibration -------------------------------------------------------------

args_nll <- data.frame(cdf_all = "all_cdf", y_true_all = "all_y",
                       mod = c(rep(c("silscs", "cils", "cilsmrsbl",
                                     "sics", "ci"), each = 10),
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
                                       fname_sicsnll, fname_cinll), each = 10),
                                 rep(c(fname_silsnll), each = 2)),
                       in_dir = in_dir)


res_nll <- do.call(Map, c(f = calc_calibration, args_nll))
res_nll <- bind_rows(res_nll)

# Save results ------------------------------------------------------------

write.csv(res_nll, file = file.path(out_dir, "cal_avgnll.csv"))
