# Sample size extrapolation
# LK
# Oct 21

source("k_ontram-experiments.R", echo = TRUE)

# Params ------------------------------------------------------------------

fm <- mrs3 ~ age + sexm + nihss_baseline + mrs_before + stroke_beforey + tia_beforey +
  ich_beforey + rf_hypertoniay + rf_diabetesy + rf_hypercholesterolemiay + rf_smokery +
  rf_atrial_fibrillationy + rf_chdy

## Paths
bpath <- "../data/dicom-3d.h5"
bpathx <- "../data/baseline_data_zurich_prepared.csv"

# Read data ---------------------------------------------------------------

timg <- h5read(file = bpath, "/X")
timg <- array(aperm(timg, 4:1), dim = c(dim(timg)[4:1], 1))
y <- h5read(file = bpath, "/Y_pat")
pat <- data.frame(p_id = h5read(file = bpath, "/pat"))
dat <- read_csv(bpathx, na = c("NA")) %>%
  left_join(pat, .) %>%
  mutate(mrs3 = ordered(mrs3, levels = 0:6),
         mrs_before = factor(mrs_before, levels = unique(na.omit(mrs_before)),
                             labels = 0:4))

# Model matrices ----------------------------------------------------------

tmf <- model.frame(fm, data = dat)
tY <- to_categorical(model.response(tmf))
tX <- ontram:::.rm_int(model.matrix(fm, data = dat))
timg <- ontram:::.batch_subset(timg, as.numeric(row.names(tX)), dim(timg))

# Run ---------------------------------------------------------------------

N <- nrow(tX)
ns <- floor(N / seq(4, 1, -0.5))
ts <- ceiling(ns * 0.8)
vs <- ceiling(ns * 0.1)
tes <- ns - ts - vs

BB <- 30
tmod <- "ci"

fix_idx <- which(apply(tY, 1, which.max) == 6)

for (tns in seq_along(ns)) {
  cat("Sample size: ", ns[tns], "\n")
  for (bb in seq_len(BB)) {
    cat("Run: ", bb, "\n")
    idx <- sample.int(N, ns[tns])
    idx <- union(idx, fix_idx)
    if (length(idx) > ns[tns]) {
      idx <- idx[-1:-(length(idx) - ns[tns])]
    }

    nY <- tY[idx, , drop = FALSE]
    # Stop if no events for a category for training
    if (any(colSums(nY) == 0)) {
      message("Not all outcome levels present in data.")
      next
    }
    nX <- tX[idx, , drop = FALSE]
    nimg <- ontram:::.batch_subset(timg, idx, dim(timg))
    nmf <- tmf[idx, , drop = FALSE]

    run_experiment(mod = tmod, B = 1, nep = 100, bs = 6, lr = 5e-5,
                   valid_size = vs[tns], test_size = tes[tns],
                   oup = paste0(tmod, "_n", ns[tns], "_run", bb),
                   Y = nY, X = nX, img = nimg, mf = nmf)
  }
}
