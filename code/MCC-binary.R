# Stroke y/n with augmentation
# LK
# Nov 2021

set.seed(24101968)

mrs_binary <- FALSE

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(ontram)
library(tidyverse)
library(tram)
theme_set(theme_bw() + theme(legend.position = "top"))

# Params ------------------------------------------------------------------

out <- "MCC-binary"

if (!dir.exists(out))
  dir.create(out)

# Funs --------------------------------------------------------------------

.batch_subset <- function (obj, idx, dim) {
  ndim <- length(dim)
  if (ndim == 2L) {
    ret <- obj[idx, , drop = FALSE]
  }
  else if (ndim == 3L) {
    ret <- obj[idx, , , drop = FALSE]
  }
  else if (ndim == 4L) {
    ret <- obj[idx, , , , drop = FALSE]
  } else if (ndim == 5L) {
    ret <- obj[idx, , , , , drop = FALSE]
  }
  return(ret)
}

.rm_int <- function(x) {
  if (all(x[, 1] == 1))
    return(x[, -1L, drop = FALSE])
  return(x)
}

cnn3d_stroke <- function(ord = NULL, dims = c(128, 128, 28, 1), llact = "linear",
                         llbias = FALSE) {
  keras_model_sequential() %>%
    layer_conv_3d(filters = 32, kernel_size = c(3, 3, 3), padding = "same",
                  activation = "relu", input_shape = dims) %>%
    layer_max_pooling_3d(pool_size = c(2, 2, 2)) %>%
    layer_conv_3d(filters = 32,
                  kernel_size = c(3, 3, 3), padding = "same", activation = "relu") %>%
    layer_max_pooling_3d(pool_size = c(2, 2, 2)) %>%
    layer_conv_3d(filters = 64, kernel_size = c(3, 3, 3), padding = "same",
                  activation = "relu") %>%
    layer_max_pooling_3d(pool_size = c(2, 2, 2)) %>%
    layer_conv_3d(filters = 64, kernel_size = c(3, 3, 3), padding = "same",
                  activation = "relu") %>%
    layer_max_pooling_3d(pool_size = c(2, 2, 2)) %>%
    layer_flatten() %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(0.3) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(0.3) %>%
    layer_dense(units = ifelse(is.null(ord), 1L, ord), use_bias = llbias,
                activation = llact)
}

# Params ------------------------------------------------------------------

fm <- mrs3_binary ~ age + sexm + nihss_baseline + mrs_before + stroke_beforey + tia_beforey +
  ich_beforey + rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay + rf_smokery +
  rf_atrial_fibrillationy + rf_chdy

## Paths
bpath <- "../data/dicom-3d.h5"
bpathx <- "../data/baseline_data_zurich_prepared.csv"

# Read data ---------------------------------------------------------------

timg <- h5read(file = bpath, "/X")
timg <- aperm(timg, 4:1)
pat <- data.frame(p_id = h5read(file = bpath, "/pat"))
dat <- read_csv(bpathx, na = c("NA")) %>%
  left_join(pat, .) %>%
  mutate(mrs3 = ordered(mrs3, levels = 0:6),
         mrs_before = factor(mrs_before, levels = unique(na.omit(mrs_before)),
                             labels = 0:4),
         mrs3_binary = factor(ifelse(mrs3 <= "2", "favourable", "unfavourable")))

y <- model.response(model.frame(fm, dat))

# Model matrices ----------------------------------------------------------

tmf <- model.frame(fm, data = dat)
tX <- .rm_int(model.matrix(fm, data = dat))
timg <- .batch_subset(timg, as.numeric(row.names(tX)), dim(timg))
tY <- abs(as.numeric(y) - 2)

# Model -------------------------------------------------------------------

B <- 10
nep <- 100
bs <- 6
tlr <- 1e-4

for (run in 1:B) {
  ## Splits
  idx <- sample.int(length(tY), ntrain <- floor(0.8 * length(tY)))
  vidx <- sample(setdiff(1:length(tY), idx), nvalid <- floor(0.1 * length(tY)))
  tidx <- setdiff(1:length(tY), c(idx, vidx))
  ntest <- length(tidx)

  img_train <- .batch_subset(timg, idx, dim(timg))
  img_valid <- .batch_subset(timg, vidx, dim(timg))
  img_test <- .batch_subset(timg, tidx, dim(timg))

  ## Messages
  cat("Run", run, "\n")
  cat("Training on", ntrain, "samples\n")
  cat("Validating on", nvalid, "samples\n")
  cat("Testing on", ntest, "samples\n")

  ## Name of callback
  tnm <- file.path(out, paste0("callback", run, ".h5"))

  ## Build, compile and fit model
  m <- cnn3d_stroke(llact = "sigmoid", llbias = TRUE)
  compile(m, optimizer = optimizer_adam(lr = tlr),
          loss = "binary_crossentropy",
          metrics = c("AUC", "mse", "accuracy"))

  datagen <- image_data_generator(
    rotation_range = 20,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    shear_range = 0.15,
    zoom_range = 0.15,
    # horizontal_flip = TRUE,
    fill_mode = "nearest"
  )
  gen <- flow_images_from_data(img_train, generator = datagen,
                               batch_size = dim(img_train)[1], shuffle = FALSE)

  m_hist <- list()
  for (epo in 1:nep) {
    aimg <- array(generator_next(gen), dim = c(dim(img_train), 1))
    m_hist[[epo]] <- fit(m, x = aimg, y = tY[idx], batch_size = bs, epochs = 1L,
                         validation_data = list(array(img_valid,
                                                      dim = c(dim(img_valid), 1)),
                                                tY[vidx]))
    if (epo == 1) {
      save_model_weights_hdf5(m, filepath = tnm)
      best_val_loss <- m_hist[[epo]]$metrics$val_loss
    } else if (m_hist[[epo]]$metrics$val_loss < best_val_loss) {
      save_model_weights_hdf5(m, filepath = tnm)
      best_val_loss <- m_hist[[epo]]$metrics$val_loss
    }
    gc()
  }

  ## History
  ldf <- lapply(m_hist, as.data.frame) %>% bind_rows(.id = "epo")

  ## Load best model
  tmp <- load_model_weights_hdf5(object = m, filepath = tnm)

  ## Evaluate, save, and plot
  tpreds <- evaluate(m, x = array(img_test, dim = c(dim(img_test), 1)), y = tY[tidx])
  tpreds <- as.data.frame(t(tpreds))
  write.csv(ldf, file = file.path(out, paste0("history-metrics", run, ".csv")),
            quote = FALSE, row.names = FALSE)
  write.csv(tpreds, file = file.path(out, paste0("test-metrics", run, ".csv")),
            quote = FALSE, row.names = FALSE)
}
