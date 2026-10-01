# jaspSyntheticData

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23075931.svg)](https://doi.org/10.5281/zenodo.23075931)

jaspSyntheticData is a JASP module for generating synthetic versions of a dataset. It gives researchers a point-and-click route to sharing data that cannot be released in its original form, such as data restricted by participant confidentiality or consent. Synthesis is done with the [synthpop](https://www.synthpop.org.uk/) R package, and the module reports utility measures so users can judge how closely the synthetic data match the original before sharing them.

## Highlights

- **Synthesis with synthpop.** Each variable is generated from the variables before it, using CART (the synthpop default), conditional trees, or parametric models chosen by variable type.
- **Utility by variable.** A table of the propensity score mean squared error (pMSE) and standardized pMSE (S_pMSE) for each variable. pMSE values near 0 and S_pMSE values near 1 indicate good utility.
- **Distribution comparison plots.** One figure comparing the original and synthetic distributions of every selected variable.
- **Overall utility (optional).** pMSE and S_pMSE from a model using all variables at once, which checks whether relationships between variables are preserved.
- **Export.** Save the synthetic dataset as a CSV file with the variable names shown in JASP.

Utility is computed on the final synthetic dataset, so the measures describe the file you save.

## How the synthetic data are built

1. Character columns are converted to factors, and numeric columns with few distinct values (5 or fewer) are treated as categorical.
2. `synthpop::syn()` generates several synthetic datasets with the chosen method.
3. One synthetic dataset is selected at random and kept whole, so every row's values come from the same synthesis draw.
4. Within each combination of categorical values, numeric columns are rescaled so their means and standard deviations match the original data, and values are kept within the observed range. Columns that contain only whole numbers in the original data (such as ages or rating scales) are rounded back to whole numbers.
5. Discrete numeric columns are snapped back to observed values.

A single selected variable is generated without synthpop by sampling from its observed distribution.

## Options

| Option | Description |
|---|---|
| Variables | The columns to synthesize. The synthesis order follows the list from top to bottom. |
| Row count | Keep the original number of rows or set a new total. |
| Random seed | Set this for reproducible output. |
| Synthesis method | CART, conditional trees, or parametric. |
| Utility by variable | Per-variable pMSE and S_pMSE table (on by default). |
| Distribution comparison plots | Original vs. synthetic distributions (on by default). |
| Overall utility | All-variable pMSE and S_pMSE (off by default; can be slow on large datasets). |
| Save as… | File path for the exported CSV. |

## Installation

Install the module from source:

```bash
R CMD INSTALL . --preclean --no-multiarch --with-keep.source
```

To load it in JASP during development, see [Adding your own modules to JASP](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md).

## Usage

1. Open a dataset in JASP and choose **Synthetic Data** from the module menu.
2. Move the variables you want to synthesize into the **Variables** box.
3. Set the seed, synthesis method, and any other options.
4. Check the utility table and comparison plots to judge whether the synthetic data are close enough to the original for your purpose.
5. Under **Save synthetic dataset**, pick a file path to export the CSV.

## Development

The analysis lives in `R/syntheticData.R` and the interface in `inst/qml/SyntheticData.qml`. Tests are in `tests/testthat` and can be run with:

```r
pkgload::load_all(".")
testthat::test_dir("tests/testthat", load_package = "none")
```

Plots can only be rendered inside JASP's graphics backend, so tests that check plot output run through `jaspTools::runAnalysis()` and are skipped if jaspTools is not installed.

## Citation

If you use this module, please cite it using the metadata in [`CITATION.cff`](CITATION.cff), or use the "Cite this repository" button on GitHub:

Buchanan, E. M. (2026). *jaspSyntheticData: A JASP module for generating synthetic data* [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.23075931

Please also cite synthpop:

Nowok, B., Raab, G. M., & Dibben, C. (2016). synthpop: Bespoke creation of synthetic data in R. *Journal of Statistical Software, 74*(11), 1–26. https://doi.org/10.18637/jss.v074.i11

## License

GPL (>= 2)
