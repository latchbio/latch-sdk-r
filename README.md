
<!-- README.md is generated from README.Rmd. Please edit that file -->

# latch

<!-- badges: start -->

[![R-CMD-check](https://github.com/latchbio/latch-sdk-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/latchbio/latch-sdk-r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

### Latest release from CRAN

``` r
pak::pak("latch")
```

or

``` r
install.packages("latch")
```

### Development version from GitHub

``` r
pak::pak("latchbio/latch-sdk-r")
```

or

``` r
# install.packages("devtools")
devtools::install_github("latchbio/latch-sdk-r")
```

## Example

``` r
library(latch)

acc <- latch::Account$current()
cat("Current account is ")
acc$print()

projects <- acc$list_registry_projects()
cat("Latch Registry for", acc$get_display_name(), "\n")
for (proj in projects) {
  cat(proj$get_display_name(), "\n")
  tables <- proj$list_tables()
  for (table in tables) {
    cat("  -", table$get_display_name(), "\n")
  }
}
```

## Copying/License

This software and text is quad-licensed and downstream users are free to
use any of the provided licenses.

**Available licenses:**

| Name                     | Requirements | [OSI](https://opensource.org/) Approved | Notes                                                                  |
|--------------------------|--------------|-----------------------------------------|------------------------------------------------------------------------|
| [MIT](./mit.license)     | Attribution  | :white_check_mark: Yes                  | Most commonly recognized and understood                                |
| [BSD0](./bsd0.license)   | None         | :white_check_mark: Yes                  | Unencumbered license allowed at Google                                 |
| [CC0](./copying)         | None         | :x: No                                  | Preferred way of dedicating software to the public domain              |
| [Unlicense](./unlicense) | None         | :white_check_mark: Yes                  | OSI approved public domain dedication. **Questionable legal standing** |
