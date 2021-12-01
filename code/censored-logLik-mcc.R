# censored logLik mcc
# Lucas Kook
# Nov 21

set.seed(24101968)
inp <- "results/MCC"

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(ontram)
library(dplyr)
library(stringr)
library(tram)
library(ggplot2)
theme_set(theme_bw() + theme(legend.position = "top"))

# Read --------------------------------------------------------------------

files <- list.files(inp, pattern = "model-pred", full.names = TRUE) %>%
  str_sort(numeric = TRUE)

dfiles <- list.files(inp, pattern = "data_fold", full.names = TRUE) %>%
  str_sort(numeric = TRUE)

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

# Preds -------------------------------------------------------------------

nlls <- sapply(seq_along(files), function(tfile) {
  preds <- read_csv(files[tfile], show_col_types = FALSE)
  tdat <- read_csv(dfiles[tfile], show_col_types = FALSE) %>%
    mutate(mrs3 = ordered(mrs3, levels = 0:6))
  fdat <- full_join(dat, tdat)
  stopifnot(all(fdat$p_id == dat$p_id))
  tY <- as.numeric(fdat$mrs3 <= 2)
  binpreds <- apply(preds, 1, function(pmf) sum(pmf[1:3]))
  auc <- pROC::auc(tY, binpreds)
  data.frame(p_id = fdat$p_id,
             nll = tY * log(binpreds) + (1 - tY) * log(1 - binpreds),
             data = fdat$train, mrs_binary = tY, binpred = binpreds,
             auc = rep(auc, length(tY)), acc = as.numeric(round(binpreds) == tY),
             brier = (tY - binpreds)^2)
}, simplify = FALSE)

pdat <- nlls %>%
  bind_rows(.id = "run") %>%
  mutate(data = factor(data, levels = c("train", "validation", "test")))

write.csv(pdat, file.path(inp, "pdat-censored-nll.csv"), row.names = FALSE,
          quote = FALSE)
