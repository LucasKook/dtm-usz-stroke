# Code for reproducing Fig 7
# LK, Nov 21

# Deps --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
library(rhdf5)
theme_set(theme_bw() + theme(legend.position = "top"))

inp <- ifelse(dir.exists("results"), "results", "intermediate-results")
files <- file.path(inp, "gam-polr.csv")

out_dir <- "figures"

if (!dir.exists(out_dir))
  dir.create(out_dir)

# Read --------------------------------------------------------------------

dat <- read_csv(files) %>%
  gather("model", "nn", -age, -starts_with("gam"), -polr)

bpath <- "~/data-sets/stroke-lh/dicom-3d.h5"
bpathx <- "~/data-sets/stroke-lh/baseline_data_zurich_prepared.csv"
pat <- data.frame(p_id = h5read(file = bpath, "/pat"))
odat <- read_csv(bpathx, na = c("NA")) %>%
  left_join(pat, .) %>%
  mutate(mrs3 = as.integer(mrs3 + 1L),
         mrs_before = factor(mrs_before, levels = unique(na.omit(mrs_before)),
                             labels = 0:4)) %>%
  na.omit()

# Vis ---------------------------------------------------------------------

ggplot(dat, aes(x = age, y = nn, group = model, color = "black")) +
  geom_line(alpha = 0.2) +
  geom_line(aes(y = gam, color = "darkblue"), data = filter(dat, model == "nn.1")) +
  geom_line(aes(y = gam_lower, color = "darkblue"), lty = 2, data = filter(dat, model == "nn.1")) +
  geom_line(aes(y = gam_upper, color = "darkblue"), lty = 2, data = filter(dat, model == "nn.1")) +
  geom_line(aes(y = polr, color = "darkred"), data = filter(dat, model == "nn.1")) +
  labs(color = "Model", y = expression(hat(beta)(age)), x = "age (standardized)") +
  scale_color_manual(name = "Model", values = c("black", "darkblue", "darkred"),
                     labels = parse(text = c("SI*'-'*LS[tilde(x)]*'-'*CS[age]", "GAM", "SI*'-'*LS[x]"))) +
  ylim(-2, 2) +
  geom_rug(aes(x = age), inherit.aes = FALSE, data = odat, alpha = 0.3)

ggsave(file.path(out_dir, "figure7.pdf"), height = 4.5, width = 6)
