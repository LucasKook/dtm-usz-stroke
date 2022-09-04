# GAM mRS
# LK, Nov 2021

set.seed(24101968)

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(ontram)
library(tidyverse)
library(tram)
library(mgcv)

out <- file.path("results", "gam-polr")
if (!dir.exists(out))
  dir.create(out, recursive = TRUE)

# Params ------------------------------------------------------------------

fm <- mrs3 ~ sexm + nihss_baseline + mrs_before + stroke_beforey + tia_beforey +
  ich_beforey + rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay + rf_smokery +
  rf_atrial_fibrillationy + rf_chdy

## Paths
bpath <- file.path("data", "dicom-3d.h5")
bpathx <- file.path("data", "baseline_data_zurich_prepared.csv")

# Read data ---------------------------------------------------------------

pat <- data.frame(p_id = h5read(file = bpath, "/pat"))
dat <- read_csv(bpathx, na = c("NA")) %>%
  left_join(pat, .) %>%
  mutate(mrs3 = as.integer(mrs3 + 1L),
         mrs_before = factor(mrs_before, levels = unique(na.omit(mrs_before)),
                             labels = 0:4)) %>%
  na.omit()
polr_dat <- dat %>% mutate(mrs3 = ordered(mrs3 - 1L))

tm <- Polr(update(fm, . ~ age + .), data = polr_dat)
gm <- gam(update(fm, . ~ s(age) + .), data = dat, family = ocat(R = 7))

plot(gm)
nage <- seq(-4, 2, length.out = 1e4)
lines(x = nage, coef(tm)["age"] * nage - mean(coef(tm)["age"] * nage), col = 2)

# Keras model -------------------------------------------------------------

X <- ontram:::.rm_int(model.matrix(fm, polr_dat))
age <- ontram:::.rm_int(model.matrix(~ age, polr_dat))
Y <- to_categorical(model.response(model.frame(fm, polr_dat)))

B <- 50
preds <- list()

for (run in 1:B) {
  ## Bootstrap
  idx <- sample.int(nrow(Y), nrow(Y) - 1L, replace = TRUE)
  tY <- Y[idx,]
  tX <- Y[idx,]
  tage <- age[idx,,drop=FALSE]

  ## Model
  mbl <- mod_baseline(ncol(tY))
  msh <- mod_shift(ncol(tX))
  tmp <- get_weights(msh)
  tmp[[1]][] <- matrix(coef(tm)[names(coef(tm)) != "age"], ncol = 1)
  set_weights(msh, tmp)
  mim <- keras_model_sequential() %>%
    layer_dense(input_shape = c(1L), units = 16L, activation = "relu",
                kernel_regularizer = regularizer_l2(1e-3)) %>%
    layer_dense(units = 16L, activation = "relu", kernel_regularizer = regularizer_l2(1e-3)) %>%
    layer_dense(units = 1L, kernel_regularizer = regularizer_l2(1e-3))
  m <- k_ontram(mod_baseline = mbl, list_of_shift_models = list(mim, msh))

  ## Loss
  loss <- k_ontram_loss(ncol(Y))

  ## Compile
  compile(m, optimizer = optimizer_adam(learning_rate = 1e-3), loss = loss)

  ## Fit
  fit_k_ontram(m, x = list(tage, tX), y = tY, batch_size = 1/7 * nrow(tX),
               epochs = 1600, validation_split = 1/7, callbacks = list(callback_early_stopping()))

  ## Re-compile
  compile(m, optimizer = optimizer_adam(learning_rate = 1e-4), loss = loss)
  fit(m, x = list(tage, tX), y = tY, batch_size = 1/7 * nrow(tX),
      epochs = 800, validation_split = 1/7, callbacks = list(callback_early_stopping()))

  save_model_weights_hdf5(m, filepath = file.path(out, paste0("callback", run, ".h5")))

  ## Age effect
  eff_age <- c(mim(matrix(nage, ncol = 1))$numpy())
  preds[[run]] <- eff_age - mean(eff_age)

}

## Write plotting data
nd <- dat[rep(1, length(nage)),]
nd$age <- nage
names(preds) <- 1:length(preds)
gpred <- predict(gm, type = "terms", newdata = nd, se.fit = TRUE)
gpred0 <- gpred$fit[, "s(age)"] - mean(gpred$fit[, "s(age)"])
pdat <- data.frame(
  age = nage,
  polr = coef(tm)["age"] * nage - mean(coef(tm)["age"] * nage),
  gam = gpred0,
  gam_lower = gpred0 - 1.96 * gpred$se.fit[, "s(age)"],
  gam_upper = gpred0 + 1.96 * gpred$se.fit[, "s(age)"],
  nn = bind_cols(preds)
)

write.csv(pdat, file.path(out, "pdat.csv"), row.names = FALSE, quote = FALSE)
