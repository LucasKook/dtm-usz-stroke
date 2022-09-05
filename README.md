
Deep transformation models for predicting functional outcome after acute ischemic stroke
========================================================================================

Code for reproducing the results in [arXiv:2206.13302 "Deep transformation
models for predicting functional outcome after acute ischemic
stroke"](https://arxiv.org/abs/2206.13302)

## Install dependencies

Deep transformation ensembles can be fitted using the `ontram` package. All
necessary dependencies can be installed via
```
make dependencies
```
or by sourcing `dependencies.R` manually. Note that this requires `conda` to be
present.

## Reproduce results

Due to the restricted access to the medical data and the high computational cost
of fitting deep transformation models (and their ensemble versions), we ensure
three "levels of reproducibility".

1. Full reproducibility `make full-repro`: Fits _all models from scratch_ and
   reproduces all figures in the paper. Only possible to run on a machine with
   GPU and >64GB RAM.

2. Partial reproducibility `make partial-repro`: Takes results from
   `intermediate-results/` objects and reproduces the _analysis_ (i.e.,
   bootstrap CIs) and _figures_. Possible to run on a machine with >=16GB of
   RAM and without GPU.

3. Figure reproducibility `make figure-repro`: Reproduces the _figures_ with the
   `intermediate-results` saved in this repository. Possible to run on a standard
   machine.

## Folder structure

`./code`: Contains code for fitting all models, producing all intermediate
results and all figures from those intermediate results. The code is structured
into folder for fitting models (`run/`), analyzing the fitted models
(`analyze/`) and producing figures (`visualize/`). Extra functions for running
experiments and analyses, which were not included in the `ontram` or `etram`
packages, are contained in `functions/`. Below we describe each file in more
detail.


- `analysis/`: Analysis code

  - `cal-binary-stroke.R`: Calibration for binarized functional outcome.
  - `cal-ordinal-stroke.R`: Calibration for ordinal functional outcome.
  - `ci-binary-stroke.R`: Bootstrap confidence intervals for binarized
    functional outcome prediction.
  - `ci-ordinal-stroke.R`: Bootstrap confidence intervals for ordinal functional
    outcome prediction.
  - `merge-stroke.R`: Script for merging the results files. Not needed for
    reproducing the figures from `intermediate-results/`.
  - `perf-stroke.R`: Script for computing all performance metrics for all
    models.

- `functions/`: Functions are loaded from these separate files for fitting or 
  analyzing models.

  - `functions_DE.R`: Functions for deep transformation ensembles, including
    bootstrap CI.
  - `k_ontram-functions.R`: Functions for running
    `../run/sample-size-extrapolation.R`.

- `run/`: Run scripts for fitting models.

  - `run-ensembles.R`: Main file for producing the results in the manuscript.
    Contains code for fitting all deep transformation ensembles to the stroke
    data.
  - `sample-size-extrapolation.R`: Code for producing the data for Fig. B4 on
    sample size considerations in the stroke data.

- `visualize/`: Scripts for producing all figures from the manuscript from the
  intermediate results.

  - `distr-predictors.R`: Code for reproducing Figure B1. Can't be reproduced
    because the data are not openly available.
  - `gam-polr.R`: Code for reproducing Figure 7.
  - `lor.R`: Code for reproducing Figure 6.
  - `perf-calpl.R`: Code for reproducing Figures 4, 5, B2, B3 on performance and
    calibration of deep transformation ensembles.
  - `sample-size-extrapolation.R`: Code for reproducing Figure B4.

- `data/`: TODO

- `intermediate-results/`: Intermediate results for reproducing all figures and
  analyses from the manuscript.

- `dependencies.R`: File for installing all dependencies.

## Session info

```
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=de_CH.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=de_CH.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=de_CH.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=de_CH.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] BiocManager_1.30.16 forcats_0.5.1       purrr_0.3.4         tibble_3.1.8        tidyverse_1.3.1     tidyr_1.1.4        
 [7] stringr_1.4.0       rhdf5_2.38.0        readr_2.0.2         patchwork_1.1.1     mgcv_1.8-38         nlme_3.1-152       
[13] ggpubr_0.4.0        ggbeeswarm_0.6.0    etram_0.0-1         tram_0.7-2          mlt_1.4-2           basefun_1.1-3      
[19] variables_1.1-1     ontram_0.1.0        tensorflow_2.9.0    keras_2.9.0         dplyr_1.0.10        colorspace_2.0-2   
[25] caret_6.0-92        boot_1.3-28         Hmisc_4.6-0         ggplot2_3.3.6       Formula_1.2-4       survival_3.2-13    
[31] lattice_0.20-45     remotes_2.4.1      

loaded via a namespace (and not attached):
  [1] readxl_1.3.1         backports_1.4.1      alabama_2022.4-1     plyr_1.8.6           splines_4.1.2       
  [6] listenv_0.8.0        tfruns_1.5.0         BB_2019.10-1         TH.data_1.1-1        digest_0.6.28       
 [11] foreach_1.5.1        htmltools_0.5.2      fansi_1.0.3          magrittr_2.0.3       checkmate_2.0.0     
 [16] cluster_2.1.2        tzdb_0.1.2           openxlsx_4.2.4       recipes_0.2.0        globals_0.14.0      
 [21] modelr_0.1.8         gower_1.0.0          sandwich_3.0-2       hardhat_0.2.0        jpeg_0.1-9          
 [26] rvest_1.0.2          haven_2.4.3          xfun_0.27            crayon_1.4.1         jsonlite_1.8.0      
 [31] zeallot_0.1.0        zoo_1.8-10           iterators_1.0.13     glue_1.6.2           gtable_0.3.0        
 [36] ipred_0.9-12         car_3.0-11           Rhdf5lib_1.16.0      future.apply_1.8.1   abind_1.4-5         
 [41] scales_1.1.1         mvtnorm_1.1-3        DBI_1.1.1            rstatix_0.7.0        Rcpp_1.0.9          
 [46] htmlTable_2.3.0      reticulate_1.26      foreign_0.8-81       stats4_4.1.2         lava_1.6.10         
 [51] prodlim_2019.11.13   httr_1.4.2           htmlwidgets_1.5.4    RColorBrewer_1.1-2   ellipsis_0.3.2      
 [56] pkgconfig_2.0.3      dbplyr_2.1.1         nnet_7.3-16          utf8_1.2.2           tidyselect_1.1.2    
 [61] rlang_1.0.5          reshape2_1.4.4       polynom_1.4-1        munsell_0.5.0        cellranger_1.1.0    
 [66] tools_4.1.2          cli_3.3.0            generics_0.1.3       broom_0.7.9          fastmap_1.1.0       
 [71] fs_1.5.0             ModelMetrics_1.2.2.2 knitr_1.36           zip_2.2.0            future_1.23.0       
 [76] whisker_0.4          xml2_1.3.2           compiler_4.1.2       rstudioapi_0.14      beeswarm_0.4.0      
 [81] curl_4.3.2           png_0.1-7            ggsignif_0.6.3       reprex_2.0.1         coneproj_1.16       
 [86] stringi_1.7.5        Matrix_1.4-0         vctrs_0.4.1          rhdf5filters_1.6.0   pillar_1.8.1        
 [91] lifecycle_1.0.1      data.table_1.14.2    orthopolynom_1.0-6   R6_2.5.1             latticeExtra_0.6-29 
 [96] gridExtra_2.3        rio_0.5.27           vipor_0.4.5          parallelly_1.29.0    codetools_0.2-18    
[101] MASS_7.3-54          assertthat_0.2.1     withr_2.5.0          multcomp_1.4-20      parallel_4.1.2      
[106] hms_1.1.1            quadprog_1.5-8       grid_4.1.2           rpart_4.1-15         timeDate_3043.102   
[111] class_7.3-19         carData_3.0-4        pROC_1.18.0          numDeriv_2016.8-1.1  lubridate_1.8.0     
[116] base64enc_0.1-3     
```
