# Merge all val, test CDF of all models, Y, log OR
# Andrea Goetschi
# May 2022

# Dependencies ------------------------------------------------------------

library(readr)
library(rhdf5)
library(etram)
library(tidyr)

# Directories -------------------------------------------------------------

im_path <- "~/data-sets/stroke-lh/dicom-3d.h5"
path <- "~/data-sets/stroke-lh/baseline_data_zurich_prepared.csv"
in_dir <- out_dir <- "results/"

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

### SI-LS-CS

## NLL

cdfval_silscsnll <- list_cdfs(in_dir, fname_silscsnll, splits, ensembles, "val")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdfval_silscsnll[[s]][[m]]$mem <- m
  }
  cdfval_silscsnll[[s]] <- do.call("rbind", cdfval_silscsnll[[s]])
  cdfval_silscsnll[[s]]$spl <- s
  cdfval_silscsnll[[s]]$type <- "val"
  cdfval_silscsnll[[s]]$loss <- "nll"
  cdfval_silscsnll[[s]]$mod <- "silscs"
}
cdfval_silscsnll <- do.call("rbind", cdfval_silscsnll)

cdftest_silscsnll <- list_cdfs(in_dir, fname_silscsnll, splits, ensembles, "test")
bincdftest_silscsnll <- bincdf(cdftest_silscsnll)
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdftest_silscsnll[[s]][[m]]$mem <- m
    bincdftest_silscsnll[[s]][[m]]$mem <- m
  }
  cdftest_silscsnll[[s]] <- do.call("rbind", cdftest_silscsnll[[s]])
  bincdftest_silscsnll[[s]] <- do.call("rbind", bincdftest_silscsnll[[s]])
  cdftest_silscsnll[[s]]$spl <- bincdftest_silscsnll[[s]]$spl <- s
  cdftest_silscsnll[[s]]$type <- bincdftest_silscsnll[[s]]$type <- "test"
  cdftest_silscsnll[[s]]$loss <- bincdftest_silscsnll[[s]]$loss <- "nll"
  cdftest_silscsnll[[s]]$mod <- bincdftest_silscsnll[[s]]$mod <- "silscs"
}
cdftest_silscsnll <- do.call("rbind", cdftest_silscsnll)
bincdftest_silscsnll <- do.call("rbind", bincdftest_silscsnll)

cdf_silscs <- bind_rows(cdfval_silscsnll, cdftest_silscsnll)
bincdf_silscs <- bind_rows(bincdftest_silscsnll)

write.csv(cdf_silscs, paste0(out_dir, "stroke_merged_cdf_silscs.csv"), row.names = FALSE)
write.csv(bincdf_silscs, paste0(out_dir, "stroke_merged_bincdf_silscs.csv"), row.names = FALSE)


### CI-LS

## NLL

cdfval_cilsnll <- list_cdfs(in_dir, fname_cilsnll, splits, ensembles, "val")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdfval_cilsnll[[s]][[m]]$mem <- m
  }
  cdfval_cilsnll[[s]] <- do.call("rbind", cdfval_cilsnll[[s]])
  cdfval_cilsnll[[s]]$spl <- s
  cdfval_cilsnll[[s]]$type <- "val"
  cdfval_cilsnll[[s]]$loss <- "nll"
  cdfval_cilsnll[[s]]$mod <- "cils"
}
cdfval_cilsnll <- do.call("rbind", cdfval_cilsnll)

cdftest_cilsnll <- list_cdfs(in_dir, fname_cilsnll, splits, ensembles, "test")
bincdftest_cilsnll <- bincdf(cdftest_cilsnll)
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdftest_cilsnll[[s]][[m]]$mem <- m
    bincdftest_cilsnll[[s]][[m]]$mem <- m
  }
  cdftest_cilsnll[[s]] <- do.call("rbind", cdftest_cilsnll[[s]])
  bincdftest_cilsnll[[s]] <- do.call("rbind", bincdftest_cilsnll[[s]])
  cdftest_cilsnll[[s]]$spl <- bincdftest_cilsnll[[s]]$spl <- s
  cdftest_cilsnll[[s]]$type <- bincdftest_cilsnll[[s]]$type <- "test"
  cdftest_cilsnll[[s]]$loss <- bincdftest_cilsnll[[s]]$loss <- "nll"
  cdftest_cilsnll[[s]]$mod <- bincdftest_cilsnll[[s]]$mod <- "cils"
}
cdftest_cilsnll <- do.call("rbind", cdftest_cilsnll)
bincdftest_cilsnll <- do.call("rbind", bincdftest_cilsnll)

cdf_cils <- bind_rows(cdfval_cilsnll, cdftest_cilsnll)
bincdf_cils <- bind_rows(bincdftest_cilsnll)

write.csv(cdf_cils, paste0(out_dir, "stroke_merged_cdf_cils.csv"), row.names = FALSE)
write.csv(bincdf_cils, paste0(out_dir, "stroke_merged_bincdf_cils.csv"), row.names = FALSE)


### CI-LS mRS BL

## NLL

cdfval_cilsmrsblnll <- list_cdfs(in_dir, fname_cilsmrsblnll, splits, ensembles, "val")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdfval_cilsmrsblnll[[s]][[m]]$mem <- m
  }
  cdfval_cilsmrsblnll[[s]] <- do.call("rbind", cdfval_cilsmrsblnll[[s]])
  cdfval_cilsmrsblnll[[s]]$spl <- s
  cdfval_cilsmrsblnll[[s]]$type <- "val"
  cdfval_cilsmrsblnll[[s]]$loss <- "nll"
  cdfval_cilsmrsblnll[[s]]$mod <- "cilsmrsbl"
}
cdfval_cilsmrsblnll <- do.call("rbind", cdfval_cilsmrsblnll)

cdftest_cilsmrsblnll <- list_cdfs(in_dir, fname_cilsmrsblnll, splits, ensembles, "test")
bincdftest_cilsmrsblnll <- bincdf(cdftest_cilsmrsblnll)
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdftest_cilsmrsblnll[[s]][[m]]$mem <- m
    bincdftest_cilsmrsblnll[[s]][[m]]$mem <- m
  }
  cdftest_cilsmrsblnll[[s]] <- do.call("rbind", cdftest_cilsmrsblnll[[s]])
  bincdftest_cilsmrsblnll[[s]] <- do.call("rbind", bincdftest_cilsmrsblnll[[s]])
  cdftest_cilsmrsblnll[[s]]$spl <- bincdftest_cilsmrsblnll[[s]]$spl <- s
  cdftest_cilsmrsblnll[[s]]$type <- bincdftest_cilsmrsblnll[[s]]$type <- "test"
  cdftest_cilsmrsblnll[[s]]$loss <- bincdftest_cilsmrsblnll[[s]]$loss <- "nll"
  cdftest_cilsmrsblnll[[s]]$mod <- bincdftest_cilsmrsblnll[[s]]$mod <- "cilsmrsbl"
}
cdftest_cilsmrsblnll <- do.call("rbind", cdftest_cilsmrsblnll)
bincdftest_cilsmrsblnll <- do.call("rbind", bincdftest_cilsmrsblnll)

cdf_cilsmrsbl <- bind_rows(cdfval_cilsmrsblnll, cdftest_cilsmrsblnll)
bincdf_cilsmrsbl <- bind_rows(bincdftest_cilsmrsblnll)

write.csv(cdf_cilsmrsbl, paste0(out_dir, "stroke_merged_cdf_cilsmrsbl.csv"), row.names = FALSE)
write.csv(bincdf_cilsmrsbl, paste0(out_dir, "stroke_merged_bincdf_cilsmrsbl.csv"), row.names = FALSE)


### SI-CS

## NLL

cdfval_sicsnll <- list_cdfs(in_dir, fname_sicsnll, splits, ensembles, "val")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdfval_sicsnll[[s]][[m]]$mem <- m
  }
  cdfval_sicsnll[[s]] <- do.call("rbind", cdfval_sicsnll[[s]])
  cdfval_sicsnll[[s]]$spl <- s
  cdfval_sicsnll[[s]]$type <- "val"
  cdfval_sicsnll[[s]]$loss <- "nll"
  cdfval_sicsnll[[s]]$mod <- "sics"
}
cdfval_sicsnll <- do.call("rbind", cdfval_sicsnll)

cdftest_sicsnll <- list_cdfs(in_dir, fname_sicsnll, splits, ensembles, "test")
bincdftest_sicsnll <- bincdf(cdftest_sicsnll)
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdftest_sicsnll[[s]][[m]]$mem <- m
    bincdftest_sicsnll[[s]][[m]]$mem <- m
  }
  cdftest_sicsnll[[s]] <- do.call("rbind", cdftest_sicsnll[[s]])
  bincdftest_sicsnll[[s]] <- do.call("rbind", bincdftest_sicsnll[[s]])
  cdftest_sicsnll[[s]]$spl <- bincdftest_sicsnll[[s]]$spl <- s
  cdftest_sicsnll[[s]]$type <- bincdftest_sicsnll[[s]]$type <- "test"
  cdftest_sicsnll[[s]]$loss <- bincdftest_sicsnll[[s]]$loss <- "nll"
  cdftest_sicsnll[[s]]$mod <- bincdftest_sicsnll[[s]]$mod <- "sics"
}
cdftest_sicsnll <- do.call("rbind", cdftest_sicsnll)
bincdftest_sicsnll <- do.call("rbind", bincdftest_sicsnll)

cdf_sics <- bind_rows(cdfval_sicsnll, cdftest_sicsnll)
bincdf_sics <- bind_rows(bincdftest_sicsnll)

write.csv(cdf_sics, paste0(out_dir, "stroke_merged_cdf_sics.csv"), row.names = FALSE)
write.csv(bincdf_sics, paste0(out_dir, "stroke_merged_bincdf_sics.csv"), row.names = FALSE)


### CI

## NLL

cdfval_cinll <- list_cdfs(in_dir, fname_cinll, splits, ensembles, "val")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdfval_cinll[[s]][[m]]$mem <- m
  }
  cdfval_cinll[[s]] <- do.call("rbind", cdfval_cinll[[s]])
  cdfval_cinll[[s]]$spl <- s
  cdfval_cinll[[s]]$type <- "val"
  cdfval_cinll[[s]]$loss <- "nll"
  cdfval_cinll[[s]]$mod <- "ci"
}
cdfval_cinll <- do.call("rbind", cdfval_cinll)

cdftest_cinll <- list_cdfs(in_dir, fname_cinll, splits, ensembles, "test")
bincdftest_cinll <- bincdf(cdftest_cinll)
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    cdftest_cinll[[s]][[m]]$mem <- m
    bincdftest_cinll[[s]][[m]]$mem <- m
  }
  cdftest_cinll[[s]] <- do.call("rbind", cdftest_cinll[[s]])
  bincdftest_cinll[[s]] <- do.call("rbind", bincdftest_cinll[[s]])
  cdftest_cinll[[s]]$spl <- bincdftest_cinll[[s]]$spl <- s
  cdftest_cinll[[s]]$type <- bincdftest_cinll[[s]]$type <- "test"
  cdftest_cinll[[s]]$loss <- bincdftest_cinll[[s]]$loss <- "nll"
  cdftest_cinll[[s]]$mod <- bincdftest_cinll[[s]]$mod <- "ci"
}
cdftest_cinll <- do.call("rbind", cdftest_cinll)
bincdftest_cinll <- do.call("rbind", bincdftest_cinll)

cdf_ci <- bind_rows(cdfval_cinll, cdftest_cinll)
bincdf_ci <- bind_rows(bincdftest_cinll)

write.csv(cdf_ci, paste0(out_dir, "stroke_merged_cdf_ci.csv"), row.names = FALSE)
write.csv(bincdf_ci, paste0(out_dir, "stroke_merged_bincdf_ci.csv"), row.names = FALSE)


### CI mRS Binary

## NLL

bincdfval_cimrsbinarynll <- list_cdfs(in_dir, fname_cimrsbinarynll, splits, ensembles, "val")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    bincdfval_cimrsbinarynll[[s]][[m]]$mem <- m
  }
  bincdfval_cimrsbinarynll[[s]] <- do.call("rbind", bincdfval_cimrsbinarynll[[s]])
  bincdfval_cimrsbinarynll[[s]]$spl <- s
  bincdfval_cimrsbinarynll[[s]]$type <- "val"
  bincdfval_cimrsbinarynll[[s]]$loss <- "nll"
  bincdfval_cimrsbinarynll[[s]]$mod <- "cimrsbinary"
}
bincdfval_cimrsbinarynll <- do.call("rbind", bincdfval_cimrsbinarynll)

bincdftest_cimrsbinarynll <- list_cdfs(in_dir, fname_cimrsbinarynll, splits, ensembles, "test")
for (s in seq_len(splits)) {
  for (m in seq_len(ensembles)) {
    bincdftest_cimrsbinarynll[[s]][[m]]$mem <- m
  }
  bincdftest_cimrsbinarynll[[s]] <- do.call("rbind", bincdftest_cimrsbinarynll[[s]])
  bincdftest_cimrsbinarynll[[s]]$spl <- s
  bincdftest_cimrsbinarynll[[s]]$type <- "test"
  bincdftest_cimrsbinarynll[[s]]$loss <- "nll"
  bincdftest_cimrsbinarynll[[s]]$mod <- "cimrsbinary"
}
bincdftest_cimrsbinarynll <- do.call("rbind", bincdftest_cimrsbinarynll)

bincdf_cimrsbinary <- bind_rows(bincdfval_cimrsbinarynll, 
                                bincdftest_cimrsbinarynll)
colnames(bincdf_cimrsbinary) <- colnames(bincdf_ci)
write.csv(bincdf_cimrsbinary, paste0(out_dir, "stroke_merged_bincdf_cimrsbinary.csv"), row.names = FALSE)


### SI-LS

## NLL

cdfval_silsnll <- list_cdfs(in_dir, fname_silsnll, splits, ensembles = NULL, "val")
for (s in seq_len(splits)) {
  for (m in 1) {
    cdfval_silsnll[[s]][[m]]$mem <- m
  }
  cdfval_silsnll[[s]] <- do.call("rbind", cdfval_silsnll[[s]])
  cdfval_silsnll[[s]]$spl <- s
  cdfval_silsnll[[s]]$type <- "val"
  cdfval_silsnll[[s]]$loss <- "nll"
  cdfval_silsnll[[s]]$mod <- "sils"
}
cdfval_silsnll <- do.call("rbind", cdfval_silsnll)
colnames(cdfval_silsnll) <- colnames(cdfval_cinll)

cdftest_silsnll <- list_cdfs(in_dir, fname_silsnll, splits, ensembles = NULL, "test")
bincdftest_silsnll <- bincdf(cdftest_silsnll)
for (s in seq_len(splits)) {
  for (m in 1) {
    cdftest_silsnll[[s]][[m]]$mem <- m
    bincdftest_silsnll[[s]][[m]]$mem <- m
  }
  cdftest_silsnll[[s]] <- do.call("rbind", cdftest_silsnll[[s]])
  bincdftest_silsnll[[s]] <- do.call("rbind", bincdftest_silsnll[[s]])
  cdftest_silsnll[[s]]$spl <- bincdftest_silsnll[[s]]$spl <- s
  cdftest_silsnll[[s]]$type <- bincdftest_silsnll[[s]]$type <- "test"
  cdftest_silsnll[[s]]$loss <- bincdftest_silsnll[[s]]$loss <- "nll"
  cdftest_silsnll[[s]]$mod <- bincdftest_silsnll[[s]]$mod <- "sils"
}
cdftest_silsnll <- do.call("rbind", cdftest_silsnll)
colnames(cdftest_silsnll) <- colnames(cdftest_cinll)
bincdftest_silsnll <- do.call("rbind", bincdftest_silsnll)

cdf_sils <- bind_rows(cdfval_silsnll, cdftest_silsnll)
bincdf_sils <- bind_rows(bincdftest_silsnll)

write.csv(cdf_sils, paste0(out_dir, "stroke_merged_cdf_sils.csv"), row.names = FALSE)
write.csv(bincdf_sils, paste0(out_dir, "stroke_merged_bincdf_sils.csv"), row.names = FALSE)


### SI

## NLL

cdfval_sinll <- list_cdfs(in_dir, fname_sinll, splits, ensembles = NULL, "val")
for (s in seq_len(splits)) {
  for (m in 1) {
    cdfval_sinll[[s]][[m]]$mem <- m
  }
  cdfval_sinll[[s]] <- do.call("rbind", cdfval_sinll[[s]])
  cdfval_sinll[[s]]$spl <- s
  cdfval_sinll[[s]]$type <- "val"
  cdfval_sinll[[s]]$loss <- "nll"
  cdfval_sinll[[s]]$mod <- "si"
}
cdfval_sinll <- do.call("rbind", cdfval_sinll)

cdftest_sinll <- list_cdfs(in_dir, fname_sinll, splits, ensembles = NULL, "test")
bincdftest_sinll <- bincdf(cdftest_sinll)
for (s in seq_len(splits)) {
  for (m in 1) {
    cdftest_sinll[[s]][[m]]$mem <- m
    bincdftest_sinll[[s]][[m]]$mem <- m
  }
  cdftest_sinll[[s]] <- do.call("rbind", cdftest_sinll[[s]])
  bincdftest_sinll[[s]] <- do.call("rbind", bincdftest_sinll[[s]])
  cdftest_sinll[[s]]$spl <- bincdftest_sinll[[s]]$spl <- s
  cdftest_sinll[[s]]$type <- bincdftest_sinll[[s]]$type <- "test"
  cdftest_sinll[[s]]$loss <- bincdftest_sinll[[s]]$loss <- "nll"
  cdftest_sinll[[s]]$mod <- bincdftest_sinll[[s]]$mod <- "si"
}
cdftest_sinll <- do.call("rbind", cdftest_sinll)
bincdftest_sinll <- do.call("rbind", bincdftest_sinll)

cdf_si <- bind_rows(cdfval_sinll, cdftest_sinll)
bincdf_si <- bind_rows(bincdftest_sinll)

write.csv(cdf_si, paste0(out_dir, "stroke_merged_cdf_si.csv"), row.names = FALSE)
write.csv(bincdf_si, paste0(out_dir, "stroke_merged_bincdf_si.csv"), row.names = FALSE)


# Combine true Y ----------------------------------------------------------

yval_1 <- y[ridx[ridx$spl == 1 & ridx$type == "val", "idx"], ]
yval_2 <- y[ridx[ridx$spl == 2 & ridx$type == "val", "idx"], ]
yval_3 <- y[ridx[ridx$spl == 3 & ridx$type == "val", "idx"], ]
yval_4 <- y[ridx[ridx$spl == 4 & ridx$type == "val", "idx"], ]
yval_5 <- y[ridx[ridx$spl == 5 & ridx$type == "val", "idx"], ]
yval_6 <- y[ridx[ridx$spl == 6 & ridx$type == "val", "idx"], ]
y_true_val_all <- list(yval_1, yval_2, yval_3, yval_4, yval_5, yval_6)

ybinval_1 <- ybin[ridx[ridx$spl == 1 & ridx$type == "val", "idx"], ]
ybinval_2 <- ybin[ridx[ridx$spl == 2 & ridx$type == "val", "idx"], ]
ybinval_3 <- ybin[ridx[ridx$spl == 3 & ridx$type == "val", "idx"], ]
ybinval_4 <- ybin[ridx[ridx$spl == 4 & ridx$type == "val", "idx"], ]
ybinval_5 <- ybin[ridx[ridx$spl == 5 & ridx$type == "val", "idx"], ]
ybinval_6 <- ybin[ridx[ridx$spl == 6 & ridx$type == "val", "idx"], ]
ybin_true_val_all <- list(ybinval_1, ybinval_2, ybinval_3, ybinval_4, ybinval_5, ybinval_6)

for (s in seq_len(splits)) {
  y_true_val_all[[s]] <- as.data.frame(y_true_val_all[[s]])
  ybin_true_val_all[[s]] <- as.data.frame(ybin_true_val_all[[s]])
  y_true_val_all[[s]]$spl <- ybin_true_val_all[[s]]$spl <- s
  y_true_val_all[[s]]$type <- ybin_true_val_all[[s]]$type <- "val"
}
y_true_val_all <- do.call("rbind", y_true_val_all)
ybin_true_val_all <- do.call("rbind", ybin_true_val_all)

ytest_1 <- y[ridx[ridx$spl == 1 & ridx$type == "test", "idx"], ]
ytest_2 <- y[ridx[ridx$spl == 2 & ridx$type == "test", "idx"], ]
ytest_3 <- y[ridx[ridx$spl == 3 & ridx$type == "test", "idx"], ]
ytest_4 <- y[ridx[ridx$spl == 4 & ridx$type == "test", "idx"], ]
ytest_5 <- y[ridx[ridx$spl == 5 & ridx$type == "test", "idx"], ]
ytest_6 <- y[ridx[ridx$spl == 6 & ridx$type == "test", "idx"], ]
y_true_test_all <- list(ytest_1, ytest_2, ytest_3, ytest_4, ytest_5, ytest_6)

ybintest_1 <- ybin[ridx[ridx$spl == 1 & ridx$type == "test", "idx"], ]
ybintest_2 <- ybin[ridx[ridx$spl == 2 & ridx$type == "test", "idx"], ]
ybintest_3 <- ybin[ridx[ridx$spl == 3 & ridx$type == "test", "idx"], ]
ybintest_4 <- ybin[ridx[ridx$spl == 4 & ridx$type == "test", "idx"], ]
ybintest_5 <- ybin[ridx[ridx$spl == 5 & ridx$type == "test", "idx"], ]
ybintest_6 <- ybin[ridx[ridx$spl == 6 & ridx$type == "test", "idx"], ]
ybin_true_test_all <- list(ybintest_1, ybintest_2, ybintest_3, ybintest_4, ybintest_5, ybintest_6)

for (s in seq_len(splits)) {
  y_true_test_all[[s]] <- as.data.frame(y_true_test_all[[s]])
  ybin_true_test_all[[s]] <- as.data.frame(ybin_true_test_all[[s]])
  y_true_test_all[[s]]$spl <- ybin_true_test_all[[s]]$spl <- s
  y_true_test_all[[s]]$type <- ybin_true_test_all[[s]]$type <- "test"
}
y_true_test_all <- do.call("rbind", y_true_test_all)
ybin_true_test_all <- do.call("rbind", ybin_true_test_all)

# Combine -----------------------------------------------------------------

y_all <- bindr("y_true")
ybin_all <- bindr("ybin_true")
write.csv(y_all, paste0(out_dir, "stroke_merged_y.csv"), row.names = FALSE)
write.csv(ybin_all, paste0(out_dir, "stroke_merged_biny.csv"), row.names = FALSE)

# Combine log OR ----------------------------------------------------------

# sils
sils_files <- list.files(path = in_dir,
                         pattern = paste0(fname_silsnll, "_lor"))

sils_lor <- lapply(sils_files, function(fname) {
  read.csv(paste0(in_dir, fname))
})

sils_lor <- do.call("rbind", sils_lor)
sils_lor <- sils_lor %>% gather(key = "var", value = "lor")
sils_lor$var <- factor(sils_lor$var)
sils_lor$mod <- "sils"
sils_lor$spl <- factor(rep(1:splits, nvars))
silsnll_indivlor <- sils_lor %>% mutate(w = 1)

# silscs nll
silscsnll_files <- list.files(path = in_dir,
                              pattern = paste0(fname_silscsnll, "_lor"))

silscsnll_lor <- lapply(silscsnll_files, function(fname) {
  read.csv(paste0(in_dir, fname))
})

# Trafo weights
w_silscsnll <- read.csv(paste0(in_dir, "w_", fname_silscsnll, ".csv"))
w_silscsnll_trf <- extract_w(w_silscsnll, meth = "trafo")
w_silscsnll_trf <- w_silscsnll_trf %>% do.call("rbind", .)

silscsnll_indivlor <- do.call("rbind", silscsnll_lor)
silscsnll_indivlor <- silscsnll_indivlor %>% gather(key = "var", value = "lor")
silscsnll_indivlor$var <- factor(silscsnll_indivlor$var)
silscsnll_indivlor$mod <- factor("silscs")
silscsnll_indivlor$spl <- factor(rep(1:splits, each = ensembles))

silscsnll_indivlor$w <- NA
for (s in seq_len(splits)) {
  for (v in unique(silscsnll_indivlor$var)) {
    silscsnll_indivlor[silscsnll_indivlor$spl == s & silscsnll_indivlor$var == v, "w"] <- w_silscsnll_trf[s, ]
  }
}

# cils nll
cilsnll_files <- list.files(path = in_dir,
                            pattern = paste0(fname_cilsnll, "_lor"))

cilsnll_lor <- lapply(cilsnll_files, function(fname) {
  read.csv(paste0(in_dir, fname))
})

# Trafo weights
w_cilsnll <- read.csv(paste0(in_dir, "w_", fname_cilsnll, ".csv"))
w_cilsnll_trf <- extract_w(w_cilsnll, meth = "trafo")
w_cilsnll_trf <- w_cilsnll_trf %>% do.call("rbind", .)

cilsnll_indivlor <- do.call("rbind", cilsnll_lor)
cilsnll_indivlor <- cilsnll_indivlor %>% gather(key = "var", value = "lor")
cilsnll_indivlor$var <- factor(cilsnll_indivlor$var)
cilsnll_indivlor$mod <- factor("cils")
cilsnll_indivlor$spl <- factor(rep(1:splits, each = ensembles))

cilsnll_indivlor$w <- NA
for (s in seq_len(splits)) {
  for (v in unique(cilsnll_indivlor$var)) {
    cilsnll_indivlor[cilsnll_indivlor$spl == s & cilsnll_indivlor$var == v, "w"] <- w_cilsnll_trf[s, ]
  }
}

# Combine -----------------------------------------------------------------

indivnll <- bindr(pat1 = "indivlor", pat2 = "nll")
write.csv(indivnll, file = paste0(out_dir, "lor_nll.csv"), row.names = FALSE)
