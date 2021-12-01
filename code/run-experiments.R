# Stroke application experiments
# Lucas Kook
# Oct 2021

set.seed(24101968)

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(ontram)
library(dplyr)
library(stringr)
library(tram)
library(ggplot2)

# FUNs --------------------------------------------------------------------

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

cs_warm_start <- function(m, mt, K) {
  tmp <- get_weights(m$mod_baseline)
  cfx <- coef(mt, with_baseline = TRUE)
  tmp[[1]][] <- ontram:::.to_gamma(cfx[1:(K - 1)])
  set_weights(m$mod_baseline, tmp)
  tmp2 <- get_weights(m$mod_shift)
  tmp2[[1]][] <- coef(mt)
  set_weights(m$mod_shift, tmp2)
  return(invisible(m))
}

ci_warm_start <- function(m, mt) {
  tmp2 <- get_weights(m$mod_shift)
  tmp2[[1]][] <- coef(mt)
  set_weights(m$mod_shift, tmp2)
  return(invisible(m))
}

cswox_warm_start <- function(m, mt, K) {
  tmp <- get_weights(m$mod_baseline)
  cfx <- coef(mt, with_baseline = TRUE)
  tmp[[1]][] <- ontram:::.to_gamma(cfx[1:(K - 1)])
  set_weights(m$mod_baseline, tmp)
  return(invisible(m))
}

eval_polr <- function(m, nd, resp = variable.names(m$model, "response")) {
  ridx <- which(colnames(nd) == resp)
  preds <- t(predict(m, newdata = nd[-ridx], type = "distr"))
  dens <- t(apply(cbind(0, preds), 1, diff))
  preds <- apply(dens, 1, which.max) - 1

  acc <- ontram::get_accuracy(nd[, resp], preds)
  cmat <- ontram::get_confmat(nd[, resp], factor(preds, levels = 0:6))
  qwk <- ontram::compute_kappa(cmat, weights = ontram::weight_scheme(7, 2))
  return(c("acc" = acc, "qwk" = qwk))
}

eval_all <- function(opreds, target) {
  preds <- t(apply(opreds, 1, diff))
  cpred <- ordered(apply(preds, 1, function(x) which.max(x)[1]), levels = 1:7, labels = 0:6)
  ctarg <- ordered(apply(target, 1, function(x) which.max(x)[1]), levels = 1:7, labels = 0:6)

  nll <- - sum(target * log(preds + 1e-12)) / nrow(preds)
  cmat <- ontram::get_confmat(ctarg, cpred)
  acc <- sum(diag(cmat)) / nrow(target)
  qwk <- ontram::compute_kappa(cmat, weights = ontram::weight_scheme(7, 2))
  return(c("nll" = nll, "acc" = acc, "qwk" = qwk))
}

run_experiment <- function(mod = c("cs", "ci", "cswox", "mcc"), fml = fm, B = 50,
                           nep = 100, bs = 1, lr = 5e-5, valid_size = 40,
                           test_size = 40, oup = mod, Y = tY, X = tX, img = timg,
                           mf = tmf, split = TRUE, ldat = NULL, augment = TRUE,
                           warmstart = TRUE) {

  mod <- match.arg(mod)

  if (!dir.exists(oup))
    dir.create(oup)

  pdf(file.path(oup, paste0(mod, ".pdf")))

  N <- nrow(Y)
  train_size <- N - valid_size - test_size

  ## Empty data structures
  tlls <- lls <- vlls <- tllsr <- llsr <- vllsr <- llp <- llpv <- llpt <- numeric(B)
  tacc <- acc <- vacc <- taccr <- accr <- vaccr <- taccp <- accp <- vaccp <- numeric(B)
  tqwk <- qwk <- vqwk <- tqwkr <- qwkr <- vqwkr <- tqwkp <- qwkp <- vqwkp <- numeric(B)
  llb <- vllb <- tllb <- lla <- vlla <- tlla <- numeric(B)
  cf <- cfr <- cfpolr <- matrix(nrow = B, ncol = ncol(X) + ncol(Y) - 1)

  ## Run splits
  for (run in 1:B) {
    cat("Run", run, "\n")

    if (split) {
      ### Split
      idx <- sample(1:nrow(Y), train_size)
      vidx <- sample((1:nrow(Y))[-idx], test_size)
      tidx <- (1:nrow(Y))[-c(idx, vidx)]

      Y_train <- Y[idx,]
      # Stop if no events for a category for training
      if (any(colSums(Y_train) == 0)) {
        message("Not all outcome levels present in training data.")
        next
      }
      X_train <- X[idx,]
      img_train <- ontram:::.batch_subset(img, idx, dim(img))

      Y_valid <- Y[vidx,]
      X_valid <- X[vidx,]
      img_valid <- ontram:::.batch_subset(img, vidx, dim(img))

      Y_test <- Y[tidx,]
      X_test <- X[tidx,]
      img_test <- ontram:::.batch_subset(img, tidx, dim(img))
    } else {
      Y_train <- ldat$Y_train
      Y_valid <- ldat$Y_valid
      Y_test <- ldat$Y_test

      X_train <- ldat$X_train
      X_valid <- ldat$X_valid
      X_test <- ldat$X_test

      img_train <- ldat$img_train
      img_valid <- ldat$img_valid
      img_test <- ldat$img_test
    }

    ### Prepare inputs
    if (mod == "cs") {
      train_inp <- list(matrix(1, nrow = nrow(X_train)), img_train, X_train)
      valid_inp <- list(matrix(1, nrow = nrow(X_valid)), img_valid, X_valid)
      test_inp <- list(matrix(1, nrow = nrow(X_test)), img_test, X_test)
      imdim <- 2L
    } else if (mod == "ci") {
      train_inp <- list(img_train, matrix(1, nrow = nrow(X_train)), X_train)
      valid_inp <- list(img_valid, matrix(1, nrow = nrow(X_valid)), X_valid)
      test_inp <- list(img_test, matrix(1, nrow = nrow(X_test)), X_test)
      imdim <- 1L
    } else if (mod == "cswox") {
      dummy_in_train <- matrix(1, nrow = nrow(X_train))
      dummy_in_valid <- matrix(1, nrow = nrow(X_valid))
      dummy_in_test <- matrix(1, nrow = nrow(X_test))
      train_inp <- list(dummy_in_train, img_train, dummy_in_train)
      valid_inp <- list(dummy_in_valid, img_valid, dummy_in_valid)
      test_inp <- list(dummy_in_test, img_test, dummy_in_test)
      imdim <- 2L
    } else if (mod == "mcc") {
      train_inp <- img_train
      valid_inp <- img_valid
      test_inp <- img_test
    }

    if (split) {
      tdat <- data.frame(mrs3 = ordered(model.response(mf)), X)
      train_dat <- tdat[idx,]
      valid_dat <- tdat[vidx,]
      test_dat <- tdat[tidx,]
    } else {
      train_dat <- ldat$train
      # Stop if no events for a category for training
      if (any(table(train_dat$mrs3) == 0)) {
        message("Not all outcome levels present in training data.")
        next
      }
      valid_dat <- ldat$valid
      test_dat <- ldat$test
    }

    # Tram for warmstart ------------------------------------------------------

    tm <- Polr(mrs3 ~ ., data = train_dat)
    llp[run] <- - logLik(tm) / nrow(train_dat)
    llpv[run] <- - logLik(tm, newdata = valid_dat) / nrow(valid_dat)
    llpt[run] <- - logLik(tm, newdata = test_dat) / nrow(test_dat)
    colnames(cfpolr) <- names(coef(as.mlt(tm)))
    cfpolr[run,] <- coef(as.mlt(tm))
    tmp <- eval_polr(tm, train_dat)
    tmpv <- eval_polr(tm, valid_dat)
    tmpt <- eval_polr(tm, test_dat)
    accp[run] <- tmp["acc"]
    qwkp[run] <- tmp["qwk"]
    vaccp[run] <- tmpv["acc"]
    vqwkp[run] <- tmpv["qwk"]
    taccp[run] <- tmpt["acc"]
    tqwkp[run] <- tmpt["qwk"]

    tnm <- file.path(oup, paste0("callback_", run, ".h5"))

    # Model -----------------------------------------------------------------

    if (mod == "cs") {
      mbl <- k_mod_baseline(ncol(Y), name = "baseline")
      msh <- mod_shift(ncol(X), name = "shift")
      mim <- cnn3d_stroke(ord = NULL, dims = dim(img)[-1])
      m <- k_ontram(mbl, list(mim, msh))
    } else if (mod == "ci") {
      mbl <- cnn3d_stroke(ord = ncol(Y) - 1L, dims = dim(img)[-1])
      mbl <- keras_model(mbl$input, layer_trafo_intercept()(mbl$output))
      msh <- mod_shift(ncol(X), name = "shift")
      mim <- mod_shift(1, kernel_initializer = initializer_zeros(), trainable = FALSE)
      m <- k_ontram(mbl, list(mim, msh))
    } else if (mod == "cswox") {
      mbl <- k_mod_baseline(ncol(Y), name = "baseline")
      msh <- mod_shift(1L, kernel_initializer = initializer_zeros(),
                       trainable = FALSE, name = "shift")
      mim <- cnn3d_stroke(ord = NULL, dims = dim(img)[-1])
      m <- k_ontram(mbl, list(mim, msh))
    } else if (mod == "mcc") {
      m <- cnn3d_stroke(ord = ncol(Y), dims = dim(img)[-1],
                        llact = "softmax", llbias = TRUE)
    }

    ### Compile model
    if (mod != "mcc") {
      # loss <- k_ontram_loss(ncol(Y))
      loss <- k_ontram_rps(ncol(Y))
      met <- metric_nll(ncol(Y))
      compile(m, optimizer = optimizer_adam(lr = lr), loss = loss,
              metric = c(met))
    } else {
      loss <- "categorical_crossentropy"
      compile(m, optimizer = optimizer_adam(lr = lr), loss = loss,
              metrics = c("acc", ontram::get_metric(ncol(Y), 2)))
    }

    if (mod != "mcc") {
      ### Compute logLik before warm start
      llb[run] <- evaluate(m, x = train_inp, y = Y_train, batch_size = bs)
      vllb[run] <- evaluate(m, x = valid_inp, y = Y_valid, batch_size = bs)
      tllb[run] <- evaluate(m, x = test_inp, y = Y_test, batch_size = bs)

      ### Warm start
      if (warmstart) {
        if (mod == "cs") {
          cs_warm_start(list(mod_baseline = mbl, mod_shift = msh), tm, ncol(Y))
        } else if (mod == "ci") {
          ci_warm_start(list(mod_shift = msh), tm)
        } else if (mod == "cswox") {
          cswox_warm_start(list(mod_baseline = mbl), tm, ncol(Y))
        }

        ### Compute logLik after warm start
        lla[run] <- evaluate(m, x = train_inp, y = Y_train, batch_size = bs)
        vlla[run] <- evaluate(m, x = valid_inp, y = Y_valid, batch_size = bs)
        tlla[run] <- evaluate(m, x = test_inp, y = Y_test, batch_size = bs)
      }
    } else {
      mtrcs <- evaluate(m, x = train_inp, y = Y_train, batch_size = bs)
      llb[run] <- mtrcs[["loss"]]
      vmtrcs <- evaluate(m, x = valid_inp, y = Y_valid, batch_size = bs)
      vllb[run] <- vmtrcs[["loss"]]
      tmtrcs <- evaluate(m, x = test_inp, y = Y_test, batch_size = bs)
      tllb[run] <- tmtrcs[["loss"]]
    }

    # Compile and fit -------------------------------------------------------

    if (!augment) {
      hist <- fit(m, x = train_inp, y = Y_train, batch_size = bs, epochs = nep,
                  validation_data = list(valid_inp, Y_valid), shuffle = TRUE,
                  callbacks = list(callback_model_checkpoint(tnm, monitor = "val_loss",
                                                             save_best_only = TRUE,
                                                             save_weights_only = TRUE)))

      p <- plot(hist, theme_bw = TRUE)
      print(p + labs(subtitle = paste("Run", run)))
    } else {
      datagen <- image_data_generator(
        rotation_range = 20,
        width_shift_range = 0.2,
        height_shift_range = 0.2,
        shear_range = 0.15,
        zoom_range = 0.15,
        # horizontal_flip = TRUE,
        fill_mode = "nearest"
      )
      gen <- flow_images_from_data(img_train[,,,,,drop = TRUE], generator = datagen,
                                   batch_size = dim(img_train)[1], shuffle = FALSE)

      m_hist <- list()
      for (epo in 1:nep) {
        cat("Epoch", epo, "\n")
        aimg <- array(generator_next(gen), dim = dim(img_train))
        if (mod != "mcc") {
          train_inp[[imdim]] <- aimg
        } else {
          train_inp <- aimg
        }
        m_hist[[epo]] <- fit(m, x = train_inp, y = Y_train, batch_size = bs,
                             epochs = 1L, validation_data = list(valid_inp, Y_valid),
                             shuffle = TRUE)
        current_val_loss <- m_hist[[epo]]$metrics$val_loss
        if (epo == 1) {
          save_model_weights_hdf5(m, filepath = tnm)
          best_val_loss <- m_hist[[epo]]$metrics$val_loss
        } else if (!is.na(current_val_loss < best_val_loss) &&
                   current_val_loss < best_val_loss) {
          cat("Saving better model\n")
          save_model_weights_hdf5(m, filepath = tnm)
          best_val_loss <- m_hist[[epo]]$metrics$val_loss
        }

        ## Garbage collection
        gc()
      }

      ## History
      m_hist <- lapply(m_hist, as.data.frame) %>% bind_rows(.id = "epo")
      write.csv(m_hist, file = file.path(oup, paste0("history", run, ".csv")),
                quote = FALSE, row.names = FALSE)
    }

    # Load best model
    tmp <- try(load_model_weights_hdf5(object = m, filepath = tnm))

    if (inherits(tmp, "try-error"))
      next

    # Post train ------------------------------------------------------------

    ### Write data
    train_dat$train <- "train"
    valid_dat$train <- "validation"
    test_dat$train <- "test"

    if (mod == "cs") {
      lls[run] <- evaluate(m, x = train_inp, y = Y_train, batch_size = bs)
      vlls[run] <- evaluate(m, x = valid_inp, y = Y_valid, batch_size = bs)
      tlls[run] <- evaluate(m, x = test_inp, y = Y_test, batch_size = bs)
      colnames(cf) <- colnames(cfpolr)
      blw <- get_weights_by_name(m, "baseline")
      sw <- get_weights_by_name(m, "shift")
      cf[run,] <- c(ontram:::.to_theta(blw), sw)

      train_dat$eta <- predict(mim, x = img_train, batch_size = bs)
      valid_dat$eta <- predict(mim, x = img_valid, batch_size = bs)
      test_dat$eta <- predict(mim, x = img_test, batch_size = bs)

      ### Refit
      om <- try(Polr(mrs3 ~ . - train, data = train_dat, fixed = c("eta" = 1)))

      if (!inherits(om, "try-error")) {
        llsr[run] <- - logLik(om) / nrow(train_dat)
        vllsr[run] <- - logLik(om, newdata = valid_dat,
                               parm = coef(as.mlt(om))) / nrow(valid_dat)
        tllsr[run] <- - logLik(om, newdata = test_dat,
                               parm = coef(as.mlt(om))) / nrow(test_dat)
        colnames(cfr) <- colnames(cfpolr)
        cfr[run,] <- coef(as.mlt(om))[-length(coef(as.mlt(om)))]
        rtmp <- eval_polr(om, train_dat)
        rtmpv <- eval_polr(om, valid_dat)
        rtmpt <- eval_polr(om, test_dat)
        accr[run] <- rtmp["acc"]
        qwkr[run] <- rtmp["qwk"]
        vaccr[run] <- rtmpv["acc"]
        vqwkr[run] <- rtmpv["qwk"]
        taccr[run] <- rtmpt["acc"]
        tqwkr[run] <- rtmpt["qwk"]

        mltom <- as.mlt(om)
        coef(mltom) <- c(cf[run,], "eta" = 1)
        mtmp <- eval_polr(mltom, train_dat)
        mtmpv <- eval_polr(mltom, valid_dat)
        mtmpt <- eval_polr(mltom, test_dat)
        acc[run] <- mtmp["acc"]
        qwk[run] <- mtmp["qwk"]
        vacc[run] <- mtmpv["acc"]
        vqwk[run] <- mtmpv["qwk"]
        tacc[run] <- mtmpt["acc"]
        tqwk[run] <- mtmpt["qwk"]
      }
    } else if (mod == "cswox") {
      lls[run] <- evaluate(m, x = train_inp, y = Y_train, batch_size = bs)
      vlls[run] <- evaluate(m, x = valid_inp, y = Y_valid, batch_size = bs)
      tlls[run] <- evaluate(m, x = test_inp, y = Y_test, batch_size = bs)

      colnames(cf) <- colnames(cfpolr)
      cf[run,] <- c(ontram:::.to_theta(get_weights_by_name(m, "baseline")),
                    rep(NA, ncol(X)))

      train_dat$eta <- predict(mim, x = img_train, batch_size = bs)
      valid_dat$eta <- predict(mim, x = img_valid, batch_size = bs)
      test_dat$eta <- predict(mim, x = img_test, batch_size = bs)

      ### Refit
      om <- try(Polr(mrs3 ~ . - train, data = train_dat, fixed = c("eta" = 1)))
      mltom <- try(as.mlt(Polr(mrs3 ~ eta, data = train_dat, fixed = c("eta" = 1))))

      if (!inherits(om, "try-error")) {
        llsr[run] <- - logLik(om) / nrow(train_dat)
        vllsr[run] <- - logLik(om, newdata = valid_dat,
                               parm = coef(as.mlt(om))) / nrow(valid_dat)
        tllsr[run] <- - logLik(om, newdata = test_dat,
                               parm = coef(as.mlt(om))) / nrow(test_dat)
        colnames(cfr) <- colnames(cfpolr)
        cfr[run,] <- coef(as.mlt(om))[-length(coef(as.mlt(om)))]
        rtmp <- eval_polr(om, train_dat)
        rtmpv <- eval_polr(om, valid_dat)
        rtmpt <- eval_polr(om, test_dat)
        accr[run] <- rtmp["acc"]
        qwkr[run] <- rtmp["qwk"]
        vaccr[run] <- rtmpv["acc"]
        vqwkr[run] <- rtmpv["qwk"]
        taccr[run] <- rtmpt["acc"]
        tqwkr[run] <- rtmpt["qwk"]

        coef(mltom) <- c(cf[run, 1:6], "eta" = 1)
        lls[run] <- - logLik(mltom, newdata = train_dat,
                             parm = coef(mltom)) / nrow(train_dat)
        vlls[run] <- - logLik(mltom, newdata = valid_dat,
                              parm = coef(mltom)) / nrow(valid_dat)
        tlls[run] <- - logLik(mltom, newdata = test_dat,
                              parm = coef(mltom)) / nrow(test_dat)
        mtmp <- eval_polr(mltom, train_dat)
        mtmpv <- eval_polr(mltom, valid_dat)
        mtmpt <- eval_polr(mltom, test_dat)
        acc[run] <- mtmp["acc"]
        qwk[run] <- mtmp["qwk"]
        vacc[run] <- mtmpv["acc"]
        vqwk[run] <- mtmpv["qwk"]
        tacc[run] <- mtmpt["acc"]
        tqwk[run] <- mtmpt["qwk"]
      }
    } else if (mod == "ci") {
      train_preds <- predict(m, x = train_inp, batch_size = bs)
      write.csv(train_preds, file.path(oup, paste0("train_predictions", run, ".csv")),
                quote = FALSE, row.names = FALSE)
      valid_preds <- predict(m, x = valid_inp, batch_size = bs)
      write.csv(valid_preds, file.path(oup, paste0("valid_predictions", run, ".csv")),
                quote = FALSE, row.names = FALSE)
      test_preds <- predict(m, x = test_inp, batch_size = bs)
      write.csv(test_preds, file.path(oup, paste0("test_predictions", run, ".csv")),
                quote = FALSE, row.names = FALSE)

      colnames(cf) <- colnames(cfpolr)
      sw <- get_weights_by_name(m, "shift")
      cf[run,] <- c(rep(NA, ncol(Y_train) - 1L), sw)

      train_metrics <- eval_all(train_preds, Y_train)
      valid_metrics <- eval_all(valid_preds, Y_valid)
      test_metrics <- eval_all(test_preds, Y_test)

      lls[run] <- train_metrics["nll"]
      acc[run] <- train_metrics["acc"]
      qwk[run] <- train_metrics["qwk"]
      vlls[run] <- valid_metrics["nll"]
      vacc[run] <- valid_metrics["acc"]
      vqwk[run] <- valid_metrics["qwk"]
      tlls[run] <- test_metrics["nll"]
      tacc[run] <- test_metrics["acc"]
      tqwk[run] <- test_metrics["qwk"]
    } else if (mod == "mcc") {
      mtrcs <- evaluate(m, x = img_train, y = Y_train, batch_size = bs)
      lls[run] <- mtrcs[["loss"]]
      acc[run] <- mtrcs[["acc"]]
      qwk[run] <- mtrcs[["qwk"]]
      vmtrcs <- evaluate(m, x = img_valid, y = Y_valid, batch_size = bs)
      vlls[run] <- vmtrcs[["loss"]]
      vacc[run] <- vmtrcs[["acc"]]
      vqwk[run] <- vmtrcs[["qwk"]]
      tmtrcs <- evaluate(m, x = img_test, y = Y_test, batch_size = bs)
      tlls[run] <- tmtrcs[["loss"]]
      tacc[run] <- tmtrcs[["acc"]]
      tqwk[run] <- tmtrcs[["qwk"]]
    }

    ## Write data
    write_dat <- bind_rows(train_dat, valid_dat, test_dat)
    write.csv(write_dat, file.path(oup, paste0("data_fold", run, ".csv")),
              quote = FALSE, row.names = FALSE)

  }

  logLiks <- data.frame(lr = lr, epoch = nep, batch_size = bs, run = 1:B,
                        ll_ontram_train = lls, ll_ontram_valid = vlls, ll_ontram_test = tlls,
                        ll_ontram_train_retrained = llsr, ll_ontram_valid_retrained = vllsr,
                        ll_ontram_test_retrained = tllsr,
                        ll_polr_train = llp, ll_polr_valid = llpv, ll_polr_test = llpt,
                        ll_ontram_train_init = llb, ll_ontram_valid_init = vllb,
                        ll_ontram_test_init = tllb, ll_ontram_train_warmstart = lla,
                        ll_ontram_valid_warmstart = vlla, ll_ontram_test_warmstart = tlla,
                        acc_ontram_train = acc, acc_ontram_valid = vacc, acc_ontram_test = tacc,
                        acc_ontram_train_retrained = accr,
                        acc_ontram_valid_retrained = vaccr,
                        acc_ontram_test_retrained = taccr,
                        acc_polr_train = accp, acc_polr_valid = vaccp, acc_polr_test = taccp,
                        qwk_ontram_train = qwk, qwk_ontram_valid = vqwk, qwk_ontram_test = tqwk,
                        qwk_ontram_train_retrained = qwkr,
                        qwk_ontram_valid_retrained = vqwkr,
                        qwk_ontram_test_retrained = tqwkr,
                        qwk_polr_train = qwkp, qwk_polr_valid = vqwkp,
                        qwk_polr_test = tqwkp)

  write.csv(logLiks, file.path(oup, "logLiks.csv"), quote = FALSE, row.names = FALSE)
  write.csv(cf, file.path(oup, "cf.csv"), quote = FALSE, row.names = FALSE)
  write.csv(cfpolr, file.path(oup, "cfpolr.csv"), quote = FALSE, row.names = FALSE)
  write.csv(cfr, file.path(oup, "cfr.csv"), quote = FALSE, row.names = FALSE)

  dev.off()
}
