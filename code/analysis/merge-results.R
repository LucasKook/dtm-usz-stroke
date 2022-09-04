
# Merge validation & test predictions, the observed outcome
# and log odds-ratios for all models separately into one file 

# Dependencies ------------------------------------------------------------

library(readr)
library(rhdf5)
library(etram)
library(tidyr)
library(stringr)

# Directories -------------------------------------------------------------

im_path <- file.path("data", "dicom-3d.h5")
path <- file.path("data", "baseline_data_zurich_prepared.csv")
in_dir <- "intermediate-results"
out_dir <- "results"

if (!dir.exists(out_dir))
  dir.create(out_dir)

# Params ------------------------------------------------------------------

splits <- 6
ensembles <- 5
nvars <- 15

fname_silscsnll <- "stroke_silscs_lossnll_wsyes_augyes"
fname_cilsnll <- "stroke_cils_lossnll_wsyes_augyes"
fname_cilsmrsblnll <- "stroke_cilsmrsbl_lossnll_wsyes_augyes"
fname_sicsnll <- "stroke_sics_lossnll_wsyes_augyes"
fname_cinll <- "stroke_ci_lossnll_wsno_augyes"
fname_cimrsbinarynll <- "stroke_cimrsbinary_lossnll_wsno_augyes"

fname_sinll <- "stroke_si"
fname_silsnll <- "stroke_sils"

# Functions ---------------------------------------------------------------

bindr <- function(pat1 = "met", pat2 = NULL) {
  obj <- ls(pattern = pat1, envir = .GlobalEnv)
  if (!is.null(pat2)) {
    obj <- grep(pattern = pat2, x = obj, value = TRUE)
  }
  ret <- bind_rows(mget(obj, envir = .GlobalEnv))
  return(ret)
}

extract_w <- function(w_all, meth = c("linear", "log-linear", "trafo"), ens = 5) {
  meth <- match.arg(meth)
  w_all %>% 
    filter(method  == meth) %>% 
    select(1:all_of(ens)) %>%
    t() %>% as.data.frame() %>% as.list()
}

bincdf <- function(lys_cdf_all) {
  lapply(lys_cdf_all, function(lys_cdf) {
    lys_cdf <- lapply(lys_cdf, as.matrix)
    lapply(lys_cdf, function(cdf){
      data.frame(cbind(cdf[, 3], 1))
    })
  })
}

# Read data ---------------------------------------------------------------

dat <- load_data("stroke", path = path, im_path = im_path)
tab_dat <- dat$tab_dat
y <- model.matrix(~ 0 + mrs3, data = tab_dat)
ybin <- model.matrix(~ 0 + mrs3_unfavorable, data = tab_dat)

ridx <- get_ridx(in_dir, fname = "stroke")

# Combine single CDFs -----------------------------------------------------

p <- grep("fname", names(.GlobalEnv), value = TRUE)
l <- do.call("list", mget(p))

lapply(l, function(x) {
  mname <- str_split(x, "_|\\.", simplify = FALSE)[[1]][2]
  if (x == "stroke_si" | x == "stroke_sils") {
    cdf_val <- list_cdfs(in_dir, x, splits, NULL, "val")
    cdf_test <- list_cdfs(in_dir, x, splits, NULL, "test")
    bincdf_test <- bincdf(cdf_test)
    e <- 1
  } else {
    cdf_val <- list_cdfs(in_dir, x, splits, ensembles, "val")
    cdf_test <- list_cdfs(in_dir, x, splits, ensembles, "test")
    if (x != "stroke_cimrsbinary_lossnll_wsno_augyes") {
      bincdf_test <- bincdf(cdf_test)
    }
    e <- seq_len(ensembles)
  }
  
  for (s in seq_len(splits)) {
    for (m in e) {
      cdf_val[[s]][[m]]$mem <- cdf_test[[s]][[m]]$mem <- m
      if (x != "stroke_cimrsbinary_lossnll_wsno_augyes") {
        bincdf_test[[s]][[m]]$mem <- m
      }
    }
    cdf_val[[s]] <- do.call("rbind", cdf_val[[s]])
    cdf_test[[s]] <- do.call("rbind", cdf_test[[s]])
    if (x != "stroke_cimrsbinary_lossnll_wsno_augyes") {
      bincdf_test[[s]] <- do.call("rbind", bincdf_test[[s]])
      bincdf_test[[s]]$spl <- s
      bincdf_test[[s]]$type <- "test"
      bincdf_test[[s]]$loss <- "nll"
      bincdf_test[[s]]$mod <- mname
    }
    cdf_val[[s]]$spl <- cdf_test[[s]]$spl <- s
    cdf_val[[s]]$type <- "val"
    cdf_test[[s]]$type <- "test"
    cdf_val[[s]]$loss <- cdf_test[[s]]$loss <- "nll"
    cdf_val[[s]]$mod <- cdf_test[[s]]$mod <- mname
  }
  cdf_val <- do.call("rbind", cdf_val)
  cdf_test <- do.call("rbind", cdf_test)
  if (x != "stroke_cimrsbinary_lossnll_wsno_augyes") {
    colnames(cdf_val)[1:7] <- colnames(cdf_test)[1:7] <- c("V2", "V3", "V4", "V5", "V6", "V7", "V8")
    bincdf_test <- do.call("rbind", bincdf_test)
    colnames(bincdf_test)[1:2] <- c("X1", "X2")
    bincdf_test <- bind_rows(bincdf_test)
  } else {
    colnames(cdf_val)[1:2] <- colnames(cdf_test)[1:2] <- c("X1", "X2")
  }
  
  cdf <- bind_rows(cdf_val, cdf_test)
  
  if (x != "stroke_cimrsbinary_lossnll_wsno_augyes") {
    write.csv(cdf, file.path(out_dir, paste0("stroke_merged_cdf_", mname, ".csv")),
                             row.names = FALSE)
    write.csv(bincdf_test, file.path(out_dir, 
                                     paste0("stroke_merged_bincdf_", mname, ".csv")),
                                     row.names = FALSE)
  } else {
    write.csv(cdf, file.path(out_dir, paste0("stroke_merged_bincdf_", mname, ".csv")),
                             row.names = FALSE)
  }
})

# Combine true Y ----------------------------------------------------------

merge_y <- function(y, t, ridx, splits) {
  y1 <- y[ridx[ridx$spl == 1 & ridx$type == t, "idx"], ]
  y2 <- y[ridx[ridx$spl == 2 & ridx$type == t, "idx"], ]
  y3 <- y[ridx[ridx$spl == 3 & ridx$type == t, "idx"], ]
  y4 <- y[ridx[ridx$spl == 4 & ridx$type == t, "idx"], ]
  y5 <- y[ridx[ridx$spl == 5 & ridx$type == t, "idx"], ]
  y6 <- y[ridx[ridx$spl == 6 & ridx$type == t, "idx"], ]
  l <- list(y1, y2, y3, y4, y5, y6)
  
  for (s in seq_len(splits)) {
    l[[s]] <- as.data.frame(l[[s]])
    l[[s]]$spl <- s
    l[[s]]$type <- t
  }
  out <- do.call("rbind", l)
  return(out)
}

y_true_val_all <- merge_y(y, "val", ridx, splits)
ybin_true_val_all <- merge_y(ybin, "val", ridx, splits)
y_true_test_all <- merge_y(y, "test", ridx, splits)
ybin_true_test_all <- merge_y(ybin, "test", ridx, splits)

# Combine -----------------------------------------------------------------

y_all <- bindr("y_true")
ybin_all <- bindr("ybin_true")
write.csv(y_all, file.path(out_dir, "stroke_merged_y.csv"), row.names = FALSE)
write.csv(ybin_all, file.path(out_dir, "stroke_merged_biny.csv"), row.names = FALSE)

# Combine log OR ----------------------------------------------------------

merge_lor <- function(fn) {
  mname <- str_split(fn, "_|\\.", simplify = FALSE)[[1]][2]
  f <- list.files(path = in_dir,
                  pattern = paste0(fn, "_lor"))
  lor <-  lapply(f, function(fname) {
    read.csv(file.path(in_dir, fname))
  })

  lor <- do.call("rbind", lor)
  lor <- lor %>% gather(key = "var", value = "lor")
  lor$var <- factor(lor$var)
  lor$mod <- mname
  if (mname == "sils") {
    lor$spl <- factor(rep(1:splits, nvars))
    indivlor <- lor %>% mutate(w = 1)
  } else {
    lor$spl <- factor(rep(1:splits, each = ensembles))
    
    w <- read.csv(file.path(in_dir, paste0("w_", fn, ".csv")))
    w_trf <- extract_w(w, meth = "trafo")
    w_trf <- w_trf %>% do.call("rbind", .)
    
    indivlor <- lor
    indivlor$w <- NA
    for (s in seq_len(splits)) {
      for (v in unique(indivlor$var)) {
        indivlor[indivlor$spl == s & indivlor$var == v, "w"] <- w_trf[s, ]
      }
    }
  }
  return(indivlor)
}

indivlor_sils <- merge_lor(fname_silsnll)
indivlor_silscs <- merge_lor(fname_silscsnll)
indivlor_cils <- merge_lor(fname_cilsnll)

# Combine -----------------------------------------------------------------

indivnll <- bindr(pat1 = "indivlor")
write.csv(indivnll, file = file.path(out_dir, "lor_nll.csv"), row.names = FALSE)
