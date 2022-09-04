
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
or by sourcing `dependencies.R` manually.

## Reproduce results

Due to the restricted access to the medical data and the high computational cost
of fitting deep transformation models (and their ensemble versions), we ensure
three "levels of reproducibility".

1. Full reproducibility `make full-repro`: Fits _all models from scratch_ and
   reproduces all figures in the paper. Not possible to run on a machine without
   access to at least one GPU and >64GB RAM.

2. Partial reproducibility `make partial-repro`: Takes results from
   `intermediate-results/` objects and reproduces the _analysis_ (i.e.,
   bootstrap CIs) and _figures_.  Possible to run on a machine with >=16GB of
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

## Session info

TODO
