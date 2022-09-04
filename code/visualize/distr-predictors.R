
# Code to produce Figure B1
# Figure B1 cannot be reproduced since the data 
# is not available (sensitive data)

# Deps --------------------------------------------------------------------

library(rhdf5)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Paths ------------------------------------------------------------------

path_img <- file.path("data", "dicom-3d.h5")
path_tab <- file.path("data", "baseline_data_zurich_prepared.csv")
path_tab_raw <- file.path("data", "baseline_data_DWI.csv")
out_dir <- "figures"

if (!dir.exists(out_dir))
  dir.create(out_dir)

# Functions ---------------------------------------------------------------

vis_bin <- function(var, xlab) {
  ggplot(dat_xy, aes(x = dat_xy[, var], fill = mrs3)) +
    geom_bar(width = 0.8, position = position_stack(reverse = TRUE)) +
    theme_bw() +
    labs(y = "Count", x = xlab) +
    guides(fill = guide_legend(title = "mRS", nrow = 1)) +
    theme(axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          axis.text = element_text(size = 11))
}

# Data --------------------------------------------------------------------

# tabular data
pat <- data.frame(p_id = h5read(file = path_img, "/pat"))
dat_xy <- read_csv(path_tab, na = c("NA")) %>%
  left_join(pat, .) %>%
  mutate(mrs3 = ordered(mrs3, levels = 0:6), # 0:6
         mrs_before = factor(mrs_before, levels = unique(na.omit(mrs_before)),
                             labels = 0:4))

dat_xy_raw <- read.table(path_tab_raw, sep = " ") %>%
  left_join(pat, .)
ind_na <- which(is.na(dat_xy), arr.ind = TRUE)
row_na <- unique(ind_na[, 1])
dat_xy_raw <- dat_xy_raw[-row_na, ]

# remove rows where all entries are NA
dat_xy <- dat_xy[rowSums(is.na(dat_xy)) != ncol(dat_xy) - 1, ] # no NA anymore
dat_xy_raw$mrs3 <- dat_xy$mrs3

# factor vars
dat_xy$sexm <- with(dat_xy, factor(ifelse(sexm == max(sexm), "m", "f")))
dat_xy$stroke_beforey <- with(dat_xy, factor(ifelse(stroke_beforey == max(stroke_beforey), "yes", "no")))
dat_xy$tia_beforey <- with(dat_xy, factor(ifelse(tia_beforey == max(tia_beforey), "yes", "no")))
dat_xy$ich_beforey <- with(dat_xy, factor(ifelse(ich_beforey == max(ich_beforey), "yes", "no")))
dat_xy$rf_hypertoniay <- with(dat_xy, factor(ifelse(rf_hypertoniay == max(rf_hypertoniay), "yes", "no")))
dat_xy$rf_diabetesy <- with(dat_xy, factor(ifelse(rf_diabetesy == max(rf_diabetesy), "yes", "no")))
dat_xy$rf_hypercholesterolemiay <- with(dat_xy, 
                                        factor(ifelse(rf_hypercholesterolemiay == max(rf_hypercholesterolemiay),
                                                      "yes", "no")))
dat_xy$rf_smokery <- with(dat_xy, factor(ifelse(rf_smokery == max(rf_smokery), "yes", "no")))
dat_xy$rf_atrial_fibrillationy <- with(dat_xy, 
                                       factor(ifelse(rf_atrial_fibrillationy == max(rf_atrial_fibrillationy), 
                                                     "yes", "no")))
dat_xy$rf_chdy <- with(dat_xy, factor(ifelse(rf_chdy == max(rf_chdy), "yes", "no")))

# Visualization -----------------------------------------------------------

# binary predictors
pl_sexm <- vis_bin("sexm", "Sex")
pl_stroke_beforey <- vis_bin("stroke_beforey", "Prior stroke")
pl_tia_beforey <- vis_bin("tia_beforey", "Prior TIA")
pl_rf_hypertoniay <- vis_bin("rf_hypertoniay", "Hypertension")
pl_rf_diabetesy <- vis_bin("rf_diabetesy", "Diabetes")
pl_rf_hypercholesterolemiay <- vis_bin("rf_hypercholesterolemiay", "Hypercholesterolemia")
pl_rf_smokery <- vis_bin("rf_smokery", "Smoking")
pl_rf_atrial_fibrillationy <- vis_bin("rf_atrial_fibrillationy", "Atrial fibrillation")
pl_rf_chdy <- vis_bin("rf_chdy", "CHD")
pl_mrs_before <- ggplot(dat_xy, aes(x = dat_xy[, "mrs_before"], fill = mrs3)) +
  geom_bar(width = 0.8, position = position_stack(reverse = TRUE)) +
  theme_bw() +
  labs(y = "Count", x = "mRS before event") +
  guides(fill = guide_legend(title = "mRS", nrow = 1)) +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 11))

# continuous predictors
pl_age <- ggplot(dat_xy_raw, aes(x = "", y = dat_xy_raw[, "age"])) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1, size = 0.5) +
  coord_flip() +
  theme_bw() +
  labs(y = "Age") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

pl_nihss_baseline <- ggplot(dat_xy_raw, aes(x = nihss, fill = mrs3)) +
  geom_bar(width = 0.8, position = position_stack(reverse = TRUE)) +
  labs(x = "NIHSS at admission", y = "Count") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 11))

# Code to produce Figure B1
ggarrange(pl_mrs_before, pl_nihss_baseline, pl_stroke_beforey,
          pl_tia_beforey, pl_rf_hypertoniay, pl_rf_diabetesy,
          pl_rf_hypercholesterolemiay, pl_rf_smokery, pl_rf_atrial_fibrillationy,
          pl_rf_chdy, pl_sexm, pl_age,
          ncol = 4, nrow = 3,
          legend = "top", common.legend = TRUE)
ggsave(file.path(out_dir, "figureB1.pdf"), width = 13, height = 8)
