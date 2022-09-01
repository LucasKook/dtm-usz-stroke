# All performance metrics of equally weighted ensembles
# Andrea Goetschi
# April 2022

# Dependencies ------------------------------------------------------------

library(etram)

# Directories -------------------------------------------------------------

source("functions/functions_DE.R")
in_dir <- out_dir <- "results/"

# Params ------------------------------------------------------------------

splits <- 6
ensembles <- 5
K <- 7

fname_silscsnll <- "stroke_silscs_lossnll_wsyes_augyes"
fname_cilsnll <- "stroke_cils_lossnll_wsyes_augyes"
fname_cilsmrsblnll <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"
fname_sicsnll <- "stroke_sics_lossnll_wsyes_augyes"
fname_cinll <- "stroke_ci_lossnll_wsno_augyes"
fname_cimrsbinarynll <- "stroke_cimrsbinary_lossnll_wsno_augyes"

fname_sinll <- "stroke_si"
fname_silsnll <- "stroke_sils"

# Load results ------------------------------------------------------------

## all CDF
cdf_files <- list.files(path = in_dir,
                        pattern = paste0("stroke_merged_cdf.*\\.csv$"))
cdf_files <- lapply(cdf_files, function(fname) {
  read.csv(paste0(in_dir, fname))
})
all_cdf <- do.call("rbind", cdf_files)

## all binary CDF
bincdf_files <- list.files(path = in_dir,
                           pattern = paste0("stroke_merged_bincdf.*\\.csv$"))
bincdf_files <- lapply(bincdf_files, function(fname) {
  read.csv(paste0(in_dir, fname))
})
all_bincdf <- do.call("rbind", bincdf_files)

## all Y
all_y <- read.csv(paste0(in_dir, "stroke_merged_y.csv"))
all_biny <- read.csv(paste0(in_dir, "stroke_merged_biny.csv"))

## CDFs all splits

### SI-LS-CS
cdftest_silscsnll <- load_lys_cdf_all(all_cdf, m = "silscs", K = K, l = "nll", t = "test")
cdfval_silscsnll <- load_lys_cdf_all(all_cdf, m = "silscs", K = K, l = "nll", t = "val")

### CI-LS
cdftest_cilsnll <- load_lys_cdf_all(all_cdf, m = "cils", K = K, l = "nll", t = "test")
cdfval_cilsnll <- load_lys_cdf_all(all_cdf, m = "cils", K = K, l = "nll", t = "val")

cdftest_cilsmrsblnll <- load_lys_cdf_all(all_cdf, m = "cilsmrsbl", K = K, l = "nll", t = "test")
cdfval_cilsmrsblnll <- load_lys_cdf_all(all_cdf, m = "cilsmrsbl", K = K, l = "nll", t = "val")

### SI-CS
cdftest_sicsnll <- load_lys_cdf_all(all_cdf, m = "sics", K = K, l = "nll", t = "test")
cdfval_sicsnll <- load_lys_cdf_all(all_cdf, m = "sics", K = K, l = "nll", t = "val")

### CI
cdftest_cinll <- load_lys_cdf_all(all_cdf, m = "ci", K = K, l = "nll", t = "test")
cdfval_cinll <- load_lys_cdf_all(all_cdf, m = "ci", K = K, l = "nll", t = "val")

cdftest_cimrsbinarynll <- load_lys_cdf_all(all_bincdf, m = "cimrsbinary", K = 2, l = "nll", t = "test")
cdfval_cimrsbinarynll <- load_lys_cdf_all(all_bincdf, m = "cimrsbinary", K = 2, l = "nll", t = "val")

### SI
cdftest_si <- load_lys_cdf_all(all_cdf, m = "si", K = K, l = "nll", t = "test")

### SI-LS
cdftest_sils <- load_lys_cdf_all(all_cdf, m = "sils", K = K, l = "nll", t = "test")

## Y true

y_true_all <- load_y_true_all(all_y, K = K, t = "test")
y_true_val_all <- load_y_true_all(all_y, K = K, t = "val")

y_true_all_bin <- load_y_true_all(all_biny, K = 2, t = "test")
y_true_val_all_bin <- load_y_true_all(all_biny, K = 2, t = "val")

# Evaluation --------------------------------------------------------------

## SILSCS ########################################################

met_silscsnll_all <- get_metrics_allspl(lys_cdf_all = cdftest_silscsnll, y_true_all = y_true_all, 
                                        type = "all", topk = TRUE, order_metric = "nll",
                                        lys_cdf_val_all = cdfval_silscsnll, y_true_val_all = y_true_val_all,
                                        metrics = "all")

indi_silscsnll_all <- get_indiv_metrics_allspl(lys_cdf_all = cdftest_silscsnll, y_true_all = y_true_all,
                                               metrics = "all")

write.csv(met_silscsnll_all, file = paste0(out_dir, "met_", fname_silscsnll, ".csv"), row.names = FALSE)
write.csv(indi_silscsnll_all, file = paste0(out_dir, "indivmet_", fname_silscsnll, ".csv"), row.names = FALSE)

## CILS ##########################################################

met_cilsnll_all <- get_metrics_allspl(lys_cdf_all = cdftest_cilsnll, y_true_all = y_true_all,
                                      type = "all", topk = TRUE, order_metric = "nll",
                                      lys_cdf_val_all = cdfval_cilsnll, y_true_val_all = y_true_val_all,
                                      metrics = "all")

indi_cilsnll_all <- get_indiv_metrics_allspl(lys_cdf_all = cdftest_cilsnll, y_true_all = y_true_all,
                                             metrics = "all")

write.csv(met_cilsnll_all, file = paste0(out_dir, "met_", fname_cilsnll, ".csv"), row.names = FALSE)
write.csv(indi_cilsnll_all, file = paste0(out_dir, "indivmet_", fname_cilsnll, ".csv"), row.names = FALSE)

#

met_cilsmrsblnll_all <- get_metrics_allspl(lys_cdf_all = cdftest_cilsmrsblnll, y_true_all = y_true_all,
                                           type = "all", topk = TRUE, order_metric = "nll",
                                           lys_cdf_val_all = cdfval_cilsmrsblnll, y_true_val_all = y_true_val_all,
                                           metrics = "all")

indi_cilsmrsblnll_all <- get_indiv_metrics_allspl(lys_cdf_all = cdftest_cilsmrsblnll, y_true_all = y_true_all,
                                                  metrics = "all")

write.csv(met_cilsmrsblnll_all, file = paste0(out_dir, "met_", fname_cilsmrsblnll, ".csv"), row.names = FALSE)
write.csv(indi_cilsmrsblnll_all, file = paste0(out_dir, "indivmet_", fname_cilsmrsblnll, ".csv"), row.names = FALSE)

## SICS ##########################################################

met_sicsnll_all <- get_metrics_allspl(lys_cdf_all = cdftest_sicsnll, y_true_all = y_true_all,
                                      type = "all", topk = TRUE, order_metric = "nll",
                                      lys_cdf_val_all = cdfval_sicsnll, y_true_val_all = y_true_val_all,
                                      metrics = "all")

indi_sicsnll_all <- get_indiv_metrics_allspl(lys_cdf_all = cdftest_sicsnll, y_true_all = y_true_all,
                                             metrics = "all")

write.csv(met_sicsnll_all, file = paste0(out_dir, "met_", fname_sicsnll, ".csv"), row.names = FALSE)
write.csv(indi_sicsnll_all, file = paste0(out_dir, "indivmet_", fname_sicsnll, ".csv"), row.names = FALSE)

## CI ############################################################

met_cinll_all <- get_metrics_allspl(lys_cdf_all = cdftest_cinll, y_true_all = y_true_all,
                                    type = "all", topk = TRUE, order_metric = "nll",
                                    lys_cdf_val_all = cdfval_cinll, y_true_val_all = y_true_val_all,
                                    metrics = "all")

indi_cinll_all <- get_indiv_metrics_allspl(lys_cdf_all = cdftest_cinll, y_true_all = y_true_all,
                                           metrics = "all")

write.csv(met_cinll_all, file = paste0(out_dir, "met_", fname_cinll, ".csv"), row.names = FALSE)
write.csv(indi_cinll_all, file = paste0(out_dir, "indivmet_", fname_cinll, ".csv"), row.names = FALSE)

# 

met_cimrsbinarynll_all <- get_metrics_allspl(lys_cdf_all = cdftest_cimrsbinarynll, y_true_all = y_true_all_bin,
                                             type = "all", topk = TRUE, order_metric = "nll",
                                             lys_cdf_val_all = cdfval_cimrsbinarynll, y_true_val_all = y_true_val_all_bin,
                                             metrics = "all", cutoff = 1)

indi_cimrsbinarynll_all <- get_indiv_metrics_allspl(lys_cdf_all = cdftest_cimrsbinarynll, y_true_all = y_true_all_bin,
                                                    metrics = "all", cutoff = 1)

write.csv(met_cimrsbinarynll_all, file = paste0(out_dir, "met_", fname_cimrsbinarynll, ".csv"), row.names = FALSE)
write.csv(indi_cimrsbinarynll_all, file = paste0(out_dir, "indivmet_", fname_cimrsbinarynll, ".csv"), row.names = FALSE)

## SI ###########################################################

met_si_all <- get_metrics_allspl(lys_cdf_all = cdftest_si, y_true_all = y_true_all, type = "all",
                                 topk = FALSE, metrics = "all")

write.csv(met_si_all[met_si_all$method == "linear", ],
          file = paste0(out_dir, "met_", fname_si, ".csv"), row.names = FALSE)

## SILS #########################################################

met_sils_all <- get_metrics_allspl(lys_cdf_all = cdftest_sils, y_true_all = y_true_all, type = "all",
                                   topk = FALSE, metrics = "all")

write.csv(met_sils_all[met_sils_all$method == "linear", ],
          file = paste0(out_dir, "met_", fname_sils, ".csv"), row.names = FALSE)
