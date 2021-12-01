# Run MCC

source("run-experiments.R", echo = TRUE)

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

run_experiment("mcc", B = 30, nep = 150, bs = 6, lr = 5e-5)
