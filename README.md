
<!-- README.md is generated from README.Rmd. Please edit that file -->

# feedeffir

<!-- badges: start -->
<!-- badges: end -->

The goal of feedeffir is to process feed efficiency files and calculate
total dry matter intake, metabolic body weight, delta body weight, and
milk energy.

## Installation

You can install the development version of feedeffir from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("GMBog/feedeffir")
```

feedeffir handles two main types of data:

1.  **Raw Data**:
    - **Description**: This includes unprocessed datasets directly from
      the farm. Examples are raw milk weights from AFI system, feed
      intake files from RIC, and milk composition files.
    - **Usage**: Functions that operate on raw data are designed for
      in-depth analysis, including data preprocessing and compilation.
2.  **Compiled Feed Efficiency Files**:
    - **Description**: These datasets are compiled from raw data and
      include metrics such as milk weights, milk composition, and feed
      intakes.
    - **Usage**: Functions targeting compiled files focus on analyzing
      feed efficiency and related metrics. These functions typically
      require data that has already been processed and aggregated from
      raw inputs.

### How to Use

- To work with **raw data**, use functions like `process_VRfiles()`,
  `process_AFI_milkw()`, `compile_milkcomp_files()`, and
  `compile_bw_files()` to preprocess and create the initial datasets.
- To analyze **compiled feed efficiency files**, use functions such as
  `process_intakes()`, `process_bw()`, and `calculate_milke()` which
  expect data compiled data.

For detailed examples of how to use these functions, refer to the
[Examples](#examples) section.

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(feedeffir)
```
