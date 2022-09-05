
# CRAN packages

if (!require(remotes))
  install.packages("remotes")
if (!require(Hmisc))
  install.packages("Hmisc")
if (!require(boot))
  install.packages("boot")
if (!require(caret))
  install.packages("caret")
if (!require(colorspace))
  install.packages("colorspace")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(etram))
  install.packages("etram")
if (!require(ggbeeswarm))
  install.packages("ggbeeswarm")
if (!require(ggplot2))
  install.packages("ggplot2")
if (!require(ggpubr))
  install.packages("ggpubr")
if (!require(mgcv))
  install.packages("mgcv")
if (!require(ontram))
  install.packages("ontram")
if (!require(patchwork))
  install.packages("patchwork")
if (!require(readr))
  install.packages("readr")
if (!require(rhdf5))
  install.packages("rhdf5")
if (!require(stringr))
  install.packages("stringr")
if (!require(tidyr))
  install.packages("tidyr")
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(tram))
  install.packages("tram")
if (!require(tram))
  install.packages("stringr")

# BIOC packages

if (!require("BiocManager"))
  install.packages("BiocManager")

if (!require("rhdf5"))
  BiocManager::install("rhdf5")

if (!require("reticulate"))
  install.packages("reticulate")

nenv <- "r-reticulate"
conda_create(nenv, python_version = "3.6")
use_condaenv(nenv)

if (!require("tensorflow"))
  install.packages("tensorflow")

if (!require("keras"))
  install.packages("keras")

install_keras(tensorflow = "2.2.0gpu", envname = nenv)

# Github packages

remotes::install_github("LucasKook/ontram-pkg", ref = "v0.0-1")
require(ontram)

remotes::install_github("LucasKook/interpretable-deep-ensembles/etram", ref = "v0.0.2")
require(etram)

# writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
