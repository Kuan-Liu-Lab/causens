# causens

<!-- badges: start -->
[![R-CMD-check](https://github.com/Kuan-Liu-Lab/causens/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Kuan-Liu-Lab/causens/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Kuan-Liu-Lab/causens/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Kuan-Liu-Lab/causens?branch=main)
<!-- badges: end -->

## Overview

causens is an R package that will allow to perform various sensitivity
analysis methods within the context of causal inference.

## Installation

```{r}
# The easiest way is to install from CRAN via:
install.packages("causens")

# Alternatively, you can install the latest version directly from GitHub
install.packages("devtools")
library(devtools)
devtools::install_github("Kuan-Liu-Lab/causens")
library(causens)
```

## Usage

```{r}
library(causens)
```

## Citing

Please cite our software using:

```
@Manual{,
  title = {causens: Perform causal sensitivity analyses using various statistical
methods},
  author = {Larry Dong, Kuan Liu},
  year = {2023},
  note = {R package version 0.0.1, 
https://kuan-liu-lab.github.io/causens/},
  url = {https://github.com/Kuan-Liu-Lab/causens/},
}
```

## Getting help

Please report bugs by opening an
[issue](https://github.com/Kuan-Liu-Lab/causens/issues/new). If you have
a question regarding the usage of `causens`, start a
[discussion](https://github.com/Kuan-Liu-Lab/causens/discussions/new/choose).
