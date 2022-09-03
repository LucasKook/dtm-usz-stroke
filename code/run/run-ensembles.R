# Run all experiments
# Andrea Goetschi
# April 2022

# Reproducibility ---------------------------------------------------------

set.seed(7426346)

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(dplyr)
library(etram)

# GPU ---------------------------------------------------------------------

# devs <- tf$config$list_physical_devices("GPU")
# # devs[1] for GPU 0, devs[2] for GPU 1, list() for none
# tf$config$set_visible_devices(devs[2], "GPU")

# Paths ------------------------------------------------------------------

im_path <- "~/data-sets/stroke-lh/dicom-3d.h5"
path <- "~/data-sets/stroke-lh/baseline_data_zurich_prepared.csv"
out_dir <- "results/"

# Params ------------------------------------------------------------------

bs <- 6
lr <- 5*10^-5
epochs <- 160
spl <- 6
ens <- 5

fml_cond <- mrs3 ~ age + sexm + nihss_baseline + mrs_before + stroke_beforey + tia_beforey + 
  rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay + rf_smokery +
  rf_atrial_fibrillationy + rf_chdy

fml_uncond <- mrs3 ~ 1

fml_mrsbl <- mrs3 ~ mrs_before

fml_binary <- mrs3_unfavorable ~ 1

# Data --------------------------------------------------------------------

dat <- load_data("stroke", path = path, im_path = im_path)
tab_dat <- dat$tab_dat
im <- dat$im

# save_ridx(nsplts = spl, prptest = 0.1, prpval = 0.1, tab_dat = tab_dat,
#           fml = fml_cond, out_dir = out_dir, fname = "stroke")
ridx <- get_ridx(out_dir, "stroke")

# Run experiments ---------------------------------------------------------

## SI-LS-CS ------------------------------------------------------------------

fname <- "stroke_silscs_lossnll_wsyes_augyes"

ensemble(mod = "silscs",
         fml = fml_cond, tab_dat = tab_dat, im = im, ridx = ridx,
         splits = spl, ensembles = ens, nn = cnn_stroke, input_shape = dim(im)[2:5],
         bs = bs, lr = lr, epochs = epochs,
         loss = "nll",
         ws = TRUE, augment = TRUE,
         out_dir = out_dir, fname = fname)

## CI-LS ------------------------------------------------------------------

fname <- "stroke_cils_lossnll_wsyes_augyes"

ensemble(mod = "cils",
         fml = fml_cond, tab_dat = tab_dat, im = im, ridx = ridx,
         splits = spl, ensembles = ens, nn = cnn_stroke, input_shape = dim(im)[2:5],
         bs = bs, lr = lr, epochs = epochs,
         loss = "nll",
         ws = TRUE, augment = TRUE,
         out_dir = out_dir, fname = fname)

fname <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"

ensemble(mod = "cils",
         fml = fml_mrsbl, tab_dat = tab_dat, im = im, ridx = ridx,
         splits = spl, ensembles = ens, nn = cnn_stroke, input_shape = dim(im)[2:5],
         bs = bs, lr = lr, epochs = epochs,
         loss = "nll",
         ws = TRUE, augment = TRUE,
         out_dir = out_dir, fname = fname)

## SI-CS ------------------------------------------------------------------

fname <- "stroke_sics_lossnll_wsyes_augyes"

ensemble(mod = "sics",
         fml = fml_uncond, tab_dat = tab_dat, im = im, ridx = ridx,
         splits = spl, ensembles = ens, nn = cnn_stroke, input_shape = dim(im)[2:5],
         bs = bs, lr = lr, epochs = epochs,
         loss = "nll",
         ws = TRUE, augment = TRUE,
         out_dir = out_dir, fname = fname)

## CI ---------------------------------------------------------------------

fname <- "stroke_ci_lossnll_wsno_augyes"

ensemble(mod = "ci",
         fml = fml_uncond, tab_dat = tab_dat, im = im, ridx = ridx,
         splits = spl, ensembles = ens, nn = cnn_stroke, input_shape = dim(im)[2:5],
         bs = bs, lr = lr, epochs = epochs,
         loss = "nll",
         ws = FALSE, augment = TRUE,
         out_dir = out_dir, fname = fname)

# CI mRS binary -----------------------------------------------------------

fname <- "stroke_cimrsbinary_lossnll_wsno_augyes"

ensemble(mod = "ci",
         fml = fml_binary, tab_dat = tab_dat, im = im, ridx = ridx,
         splits = spl, ensembles = ens, nn = cnn_stroke, input_shape = dim(im)[2:5],
         bs = bs, lr = lr, epochs = epochs,
         loss = "nll",
         ws = FALSE, augment = TRUE,
         out_dir = out_dir, fname = fname)

## SI ---------------------------------------------------------------------

fname <- "stroke_si"

ensemble(mod = "si",
         fml = fml_uncond, tab_dat = tab_dat, ridx = ridx, splits = spl,
         out_dir = out_dir, fname = fname)


## SI-LS ------------------------------------------------------------------

fname <- "stroke_sils"

ensemble(mod = "sils",
         fml = fml_cond, tab_dat = tab_dat, ridx = ridx, splits = spl,
         out_dir = out_dir, fname = fname)
