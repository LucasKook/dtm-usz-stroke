# dtm-usz-stroke

Code for reproducing the results in "Deep transformation models for predicting
functional outcome after acute ischemic stroke".

We use the following convention for the names of the models used in the
manuscript:

| Manuskript     | Code     |
| -------------- | -------- |
| CI[B]-LS[x]    | `ci`     |
| CI[B]-LS[mRS]  | `ci-mrs` |
| SI-CS[B]-LS[x] | `cs`     |
| SI-CS[B]       | `cs-wox` |
| MCC            | `mcc`    |


In this `README`, we briefly explain the functionalities of all scripts.

- `baseline-stratified-models.R`: Generates predictions for the unconditional
  (SI) model and some other baseline-adjusted Polr models.

- `censored-logLik*.R`: Scripts for generating the binary mRS predictions from
  all ordinal models.

- `<model>.R`: Scripts for running the experiments for all models in the above
  table.

- `run-experiments.R`: Main script containing the functions for running the
  experiments. This script is called by all `<model>.R` scripts.

- `gam-polr.R`: Contains the code for producing the data for Fig. 6 from the
  manuscript.

- `sample-size-extrapolation.R`: Code for producing the data for Fig. 7.
