# Calibration Curves for Clinical Prediction Models

A clinical prediction model should produce *calibrated* risk predictions, which means the predicted probabilities should align with observed probabilities. There are various ways of assessing calibration ([this paper](https://pubmed.ncbi.nlm.nih.gov/26772608/) covers calibration in more detail). `pmcalibration` implements calibration curves for binary and (right censored) time-to-event outcomes and calculates metrics used to assess the correspondence between predicted and observed outcome probabilities (the 'integrated calibration index' or $ICI$, aka $E_{avg}$, as well as $E_{50}$, $E_{90}$, and $E_{max}$).

A goal of `pmcalibration` is to implement a range of methods for estimating a smooth relationship between predicted and observed probabilities and to provide confidence intervals for calibration metrics (via bootstrapping or simulation based inference).

To install:

```
install.packages("pmcalibration")
```

To install development version:

```         
devtools::install_github("https://github.com/stephenrho/pmcalibration")
```
