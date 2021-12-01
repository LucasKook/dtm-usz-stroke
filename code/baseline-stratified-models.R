# Baseline mRS stratified ontram
# LK, Nov 2021

set.seed(24101968)

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(ontram)
library(tidyverse)
library(tram)
library(ggpubr)
library(ggbeeswarm)
theme_set(theme_bw() + theme(legend.position = "top"))

out <- "polr-strat"
if (!dir.exists(out))
  dir.create(out)

# Funs --------------------------------------------------------------------

bin_nll <- function(x, p) {
  - mean(x * log(p) + (1 - x) * log(1 - p))
}

binary_eval_polr <- function(y_true, pred_probs) {
  nll <- bin_nll(y_true, pred_probs)
  acc <- mean(y_true == round(pred_probs))
  if (length(pred_probs) == 1L)
    pred_probs <- rep(pred_probs, length(y_true))
  auc <- try(pROC::auc(y_true, pred_probs))
  if (inherits(auc, "try-error"))
    auc <- 0.5
  brier <- mean((y_true - pred_probs)^2)
  c("nll" = nll, "brier" = brier, "acc" = acc, "auc" = auc)
}

# Params ------------------------------------------------------------------

fm <- mrs3 ~ age + sexm + nihss_baseline + mrs_before + stroke_beforey + tia_beforey +
  ich_beforey + rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay + rf_smokery +
  rf_atrial_fibrillationy + rf_chdy

## Paths
bpath <- "../data/dicom-3d.h5"
bpathx <- "../data/baseline_data_zurich_prepared.csv"

# Read data ---------------------------------------------------------------

pat <- data.frame(p_id = h5read(file = bpath, "/pat"))
dat <- read_csv(bpathx, na = c("NA")) %>%
  left_join(pat, .) %>%
  mutate(mrs3 = ordered(mrs3, levels = 0:6),
         mrs_before = factor(mrs_before, levels = unique(na.omit(mrs_before)),
                             labels = 0:4)) %>%
  na.omit()

# Model matrices ----------------------------------------------------------

tmf <- model.frame(fm, data = dat)
tX <- ontram:::.rm_int(model.matrix(fm, data = dat))

B <- 30
splits <- c(0.8, 0.1) # train, valid, test = 1 - train - valid
ll1 <- llv1 <- llt1 <- ll2 <- llv2 <- llt2 <- ll3 <- llv3 <- llt3 <- ll4 <-
  llv4 <- llt4 <- ll5 <- llv5 <- llt5 <- numeric(B)

unco_binpreds <- list()
cond_binpreds <- list()

for (run in 1:B) {
  cat("Run", run)

  idx <- sample.int(nrow(dat), floor(splits[1] * nrow(dat)))
  vidx <- sample(setdiff(1:nrow(dat), idx), ceiling(splits[2] * nrow(dat)))
  tidx <- setdiff(1:nrow(dat), c(idx, vidx))

  trdat <- dat[idx,]
  vdat <- dat[vidx,]
  tdat <- dat[tidx,]

  tr_preds <- binary_eval_polr(trdat$mrs3 <= "2", p = mean(trdat$mrs3 <= "2"))
  va_preds <- binary_eval_polr(vdat$mrs3 <= "2", p = mean(trdat$mrs3 <= "2"))
  te_preds <- binary_eval_polr(tdat$mrs3 <= "2", p = mean(trdat$mrs3 <= "2"))
  unco_binpreds[[run]] <- bind_rows(tr_preds, va_preds, te_preds) %>%
    mutate(set = c("train", "valid", "test"))

  if (any(c(table(trdat$mrs3) == 0, table(trdat$mrs_before) == 0))) {
    message("Not all levels in present training set. Skipping.")
    next
  }

  # Stratified
  cat(" Model 1")
  m1 <- Polr(mrs3 | 0 + mrs_before ~ age + sexm + nihss_baseline + stroke_beforey +
               tia_beforey + ich_beforey + rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay +
               rf_smokery + rf_atrial_fibrillationy + rf_chdy, data = trdat)
  ll1[run] <- - logLik(m1) / nrow(trdat)
  llv1[run] <- - logLik(m1, newdata = vdat) / nrow(vdat)
  llt1[run] <- - logLik(m1, newdata = tdat) / nrow(tdat)

  # Conditional
  cat(" Model 2")
  m2 <- Polr(mrs3 ~ age + sexm + nihss_baseline + stroke_beforey + mrs_before +
               tia_beforey + ich_beforey + rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay +
               rf_smokery + rf_atrial_fibrillationy + rf_chdy, data = trdat)
  ll2[run] <- - logLik(m2) / nrow(trdat)
  llv2[run] <- - logLik(m2, newdata = vdat) / nrow(vdat)
  llt2[run] <- - logLik(m2, newdata = tdat) / nrow(tdat)

  m2_tr <- colSums(predict(m2, type = "density", newdata = trdat %>% select(-mrs3))[1:3,])
  m2_va <- colSums(predict(m2, type = "density", newdata = vdat %>% select(-mrs3))[1:3,])
  m2_te <- colSums(predict(m2, type = "density", newdata = tdat %>% select(-mrs3))[1:3,])
  ctr_preds <- binary_eval_polr(trdat$mrs3 <= 2, p = m2_tr)
  cva_preds <- binary_eval_polr(vdat$mrs3 <= 2, p = m2_va)
  cte_preds <- binary_eval_polr(tdat$mrs3 <= 2, p = m2_te)
  cond_binpreds[[run]] <- bind_rows(ctr_preds, cva_preds, cte_preds) %>%
    mutate(set = c("train", "valid", "test"))

  # Stratified unconditional
  cat(" Model 3")
  m3 <- Polr(mrs3 | 0 + mrs_before ~ 1, data = trdat)
  ll3[run] <- - logLik(m3) / nrow(trdat)
  llv3[run] <- - logLik(m3, newdata = vdat) / nrow(vdat)
  llt3[run] <- - logLik(m3, newdata = tdat) / nrow(tdat)

  # Marginal
  cat(" Model 4")
  m4 <- Polr(mrs3 ~ mrs_before, data = trdat)
  ll4[run] <- - logLik(m4) / nrow(trdat)
  llv4[run] <- - logLik(m4, newdata = vdat) / nrow(vdat)
  llt4[run] <- - logLik(m4, newdata = tdat) / nrow(tdat)

  # Unconditional
  cat(" Model 5\n")
  m5 <- Polr(mrs3 ~ 1, data = trdat)
  ll5[run] <- - logLik(m5) / nrow(trdat)
  llv5[run] <- - logLik(m5, newdata = vdat) / nrow(vdat)
  llt5[run] <- - logLik(m5, newdata = tdat) / nrow(tdat)
}

logLiks <- data.frame(
  ll_strat_train = ll1,
  ll_strat_valid = llv1,
  ll_strat_test = llt1,
  ll_cond_train = ll2,
  ll_cond_valid = llv2,
  ll_cond_test = llt2,
  ll_sunc_train = ll3,
  ll_sunc_valid = llv3,
  ll_sunc_test = llt3,
  ll_marg_train = ll4,
  ll_marg_valid = llv4,
  ll_marg_test = llt4,
  ll_unco_train = ll5,
  ll_unco_valid = llv5,
  ll_unco_test = llt5
)

unco_bin_logLiks <- bind_rows(unco_binpreds, .id = "run")
cond_bin_logLiks <- bind_rows(cond_binpreds, .id = "run")

write.csv(logLiks, file.path(out, "logLiks.csv"), row.names = FALSE, quote = FALSE)
write.csv(unco_bin_logLiks, file.path(out, "unco-bin-nll.csv"), row.names = FALSE, quote = FALSE)
write.csv(cond_bin_logLiks, file.path(out, "cond-bin-nll.csv"), row.names = FALSE, quote = FALSE)
