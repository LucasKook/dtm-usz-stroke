
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

1. Full reproducibility `make full-repro`: Fits all models from scratch and
   reproduces all figures in the paper. Not possible to run on a machine without
   access to at least one GPU.

2. Partial reproducibility `make partial-repro`: Takes predictions from fitted
   objects and reproduces the _analysis_ (i.e., bootstrap CIs) and _figures_.
   Possible to run on a machine with >=16GB of RAM and without GPU.

3. Figure reproducibility `make figure-repro`: Reproduces the figures with the
   intermediate results saved in this repository. Possible to run on a standard
   machine.

## Folder structure

`./code`: 


- `analysis/`:

  - `cal-binary-stroke.R`:
  - `cal-ordinal-stroke.R`:
  - `ci-binary-stroke.R`:
  - `ci-ordinal-stroke.R`:
  - `merge-stroke.R`:
  - `perf-stroke.R`:

- `functions/functions_DE.R`:

- `run/`:

  - `run-stroke.R`:
  - `sample-size-extrapolation.R`: Code for producing the data for Fig. 7.

- `visualize/`:

  - `distr-predictors.R`:
  - `distr-predictors.R`:
  - `distr-predictors.R`:

## Session info

TODO
