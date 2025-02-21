# pmcalibration 0.2.0

Several minor changes as well as some bigger changes to the default plotting function and to the functions used for assessing 'weak' calibration (`logistic_cal`) 

* Fixed bug when `smooth = "none"`
* Predictions (`p`) that are <= 0 or >= 1 are now excluded and produce a warning.
* `pmcalibration` output now includes `p_c` slot containing the value of the calibration curve for each observation.
* Changed `pmcalibration` default smooth to `"gam"` instead of `"none"`.
* Improved plot function to show risk distribution (for binary outcomes);
* Added likelihood ratio tests to `logistic_cal` functions that test overall 'weak' calibration.
* Added `plot` argument to `pmcalibration` so it produces a plot by default (without having to make separate call);
* Moved vignette material to readme (internal validation material has been developed into pminternal package, hopefully on cran soon).
* `get_cc` renamed `get_curve`.

# pmcalibration 0.1.0

* Initial CRAN submission.
