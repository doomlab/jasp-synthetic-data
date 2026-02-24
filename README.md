# jaspSyntheticData

jaspSyntheticData is a lightweight JASP module that produces synthetic datasets by resampling the originally selected variables. It keeps the categorical joint distribution intact by sampling entire rows with replacement and optionally adds jitter to numeric columns. The same synthetic data can be previewed inside JASP or exported straight to disk while decoding the encoded column names so that the resulting CSV uses the user-facing labels.

## Highlights

- Automatically resamples the selected variables (adaptive to `rowCountMode`/`n` options) to mirror the empirical joint distribution.
- Classifies variables as _continuous_ or _categorical_ and presents that metadata back as a JaspTable for easy inspection.
- Supports optional jitter on numeric columns so generated data stays realistic while preventing exact copies.
- Exported CSV files decode the internal column names so the saved file uses the labels displayed in the JASP interface.

## Installation

```bash
R CMD INSTALL . --preclean --no-multiarch --with-keep.source jaspSyntheticData
```

## Usage

1. Start JASP and register the module in development mode (see [jasp-adding-module.md](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md)).
2. Open a dataset, select the _Synthetic data_ analysis, and choose which columns you want to resample.
3. Configure options such as _row count mode_, _seed_, and _jitter fraction_, and run the analysis.
4. Preview the synthetic dataset inside JASP or press _Export_ to save the decoded CSV.

## Development

Modify `R/syntheticData.R` to tweak resampling, jitter, or export logic. The `tests` directory contains headless scenarios that instantiate the analysis without JASP; use `devtools::test()` or `renv::restore()` if you need reproducible environments.

## Reference

[Adding your own modules to JASP](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md)
