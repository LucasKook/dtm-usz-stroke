# Vis sample size extrapolation experiment
# LK
# Oct 2021

# Deps --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
theme_set(theme_bw() + theme(legend.position = "top"))

inp <- file.path("intermediate-results", "CI-next-lr4")
out <- "figures"

if (!dir.exists(out))
  dir.create(out)

if (grepl("wox", inp)) {
  levs <-
    c(
      "SI*'-'*LS[x]",
      "SI*'-'*CS[B]~init",
      "SI*'-'*CS[B]~warm",
      "SI*'-'*CS[B]",
      "SI*'-'*CS[B]~offset"
    )
} else if (grepl("CS", inp)) {
  levs <-
    c(
      "SI*'-'*LS[x]",
      "SI*'-'*CS[B]-LS[x]~init",
      "SI*'-'*CS[B]*'-'*LS[x]~warm",
      "SI*'-'*CS[B]*'-'*LS[x]",
      "SI*'-'*CS[B]*'-'*LS[x]~offset"
    )
} else if (grepl("CI", inp)) {
  levs <-
    c(
      "SI*'-'*LS[x]",
      "CI[B]-LS[x]~init",
      "CI[B]-LS[x]~warm",
      "CI[B]-LS[x]",
      "CI[B]-LS[x]~offset"
    )
} else if (grepl("MCC", inp)) {
  levs <-
    c(
      "SI*'-'*LS[x]",
      "MCC~init",
      "MCC~warm",
      "MCC",
      "MCC~offset"
    )
}

files <- list.files(inp, pattern = "logLik", full.names = TRUE, recursive = TRUE)

# Read --------------------------------------------------------------------

dat <- tibble(file = files) %>%
  mutate(dat = map(file, ~ read_csv(.x, show_col_types = FALSE))) %>%
  separate(file, into = c("jnk", "mod", "info", "ll"), sep = "/") %>%
  separate(info, into = c("mod", "n", "orun"), sep = "_") %>%
  mutate(n = str_extract(n, "[0-9]+"), orun = str_extract(orun, "[0-9]+")) %>%
  unnest(c(dat)) %>%
  select(-ll) %>%
  gather("method", "value", starts_with("ll"), starts_with("acc"), starts_with("qwk")) %>%
  mutate(set = factor(case_when(
    grepl("test", method) ~ "test",
    grepl("valid", method) ~ "valid",
    TRUE ~ "train"
  ), levels = c("train", "valid", "test")),
  model = factor(case_when(
    grepl("polr", method) ~ levs[1],
    grepl("retrained", method) ~ levs[5],
    grepl("init", method) ~ levs[2],
    grepl("warmstart", method) ~ levs[3],
    TRUE ~ levs[4]
  ), levels = levs),
  metric = factor(case_when(
    grepl("ll", method) ~ "nll",
    grepl("acc", method) ~ "acc",
    grepl("qwk", method) ~ "qwk"
  ), levels = c("nll", "acc", "qwk"))) %>%
  filter(value > 0)

ns <- unique(as.numeric(dat$n))
nlabs <- paste0(round(ns / max(ns), digits = 2L), "n")
nlabs <- ifelse(nlabs == "1n", "n", nlabs)
names(nlabs) <- ns

# Vis ---------------------------------------------------------------------

pdat <- dat %>% filter(metric == "nll",
                       !grepl("offset|init|warm", model),
                       set == "test")

p1 <- ggplot(pdat, aes(x = n, y = value, linetype = model, color = ordered(n))) +
  geom_boxplot(outlier.size = rel(0.75), position = position_dodge(width = 1)) +
  geom_quasirandom(alpha = 0.3, size = rel(1), dodge.width = 1) +
  labs(x = element_blank(), color = "sample size", y = "test negative log-likelihood") +
  scale_x_discrete(labels = nlabs) +
  scale_linetype_discrete(labels = parse(text = levels(droplevels(pdat$model)))) +
  scale_color_viridis_d(guide = guide_legend(nrow = 1L),
                        labels = nlabs)

dat %>%
  group_by(lr, epoch, batch_size, model, set, metric, n) %>%
  summarise(mean(value, na.rm = TRUE),
            median(value, na.rm = TRUE))

# LRT ---------------------------------------------------------------------

nll_polr <- dat %>%
  filter(metric == "nll", model == "SI*'-'*LS[x]") %>%
  select(set, orun, n, value) %>%
  rename(nll_polr = value)

fdat <- left_join(dat %>% filter(metric == "nll"), nll_polr) %>%
  group_by(lr, epoch, batch_size, model, set, metric, n) %>%
  mutate(lrt = value - nll_polr) %>%
  filter(model != "SI*'-'*LS[x]", !grepl("offset", model)) %>%
  filter(set == "test", !grepl("offset|init|warm", model))

p2 <- ggplot(fdat,
       aes(x = model, y = lrt, color = ordered(n))) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_boxplot(outlier.size = rel(0.75), position = position_dodge(width = 1)) +
  geom_quasirandom(alpha = 0.3, size = rel(1), dodge.width = 1) +
  labs(x = element_blank(), color = "sample size",
       y = expression(SI*'-'*LS[x]~worse%<-%NLLR%->%SI*'-'*LS[x]~better)) +
  scale_x_discrete(labels = parse(text = levels(droplevels(fdat$model)))) +
  scale_color_viridis_d(guide = guide_legend(nrow = 1L))

ggarrange(p1, p2, common.legend = TRUE)

ggsave(file.path(out, "figureB4.pdf"), height = 4, width = 9)
