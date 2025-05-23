# causens <a href="https://kuan-liu-lab.github.io/causens/"> <img src="man/figures/logo.png" align="right" height="160" /> </a>

<!-- badges: start -->
  [![R-CMD-check](https://github.com/Kuan-Liu-Lab/causens/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Kuan-Liu-Lab/causens/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

_Why is it that more shark attacks occur when more ice cream is sold? The answer: both are related to the weather, here an unmeasured confounder._

## Overview

`{causens}` is an R package that will allow to perform various sensitivity
analysis methods to adjust for unmeasured confounding within the context of 
causal inference. Currently, we provide the following methods:

- Sensitivity function + propensity score ([Li et al. (2011)](https://pubmed.ncbi.nlm.nih.gov/21659349/), [Brumback et al. (2004)](https://onlinelibrary.wiley.com/doi/10.1002/sim.1657))
- Bayesian parametric sensitivity analysis ([McCandless et Gustafson (2017), Section 2.2](https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.7298))
- Monte Carlo sensitivity analysis ([McCandless et Gustafson (2017), Section 2.3](https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.7298))

## Installation

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("Kuan-Liu-Lab/causens")
library(causens)
```

## Quickstart

``` r
library(causens)

# Simulate data
data <- simulate_data(N = 10000, seed = 123, alpha_uz = 1,
                      beta_uy = 1, treatment_effects = 1)

# Treatment model is incorrect since U is "missing"
causens_sf(Z ~ X.1 + X.2 + X.3, "Y", data = data, c1 = 0.25, c0 = 0.25)$estimated_ate
```

## Citing

Please cite our software using:

```
@Manual{,
  title = {causens: Perform Causal Sensitivity Analyses Using Various Statistical Methods},
  author = {Larry Dong and Yushu Zou and Kuan Liu},
  year = {2024},
  note = {R package version 0.0.3, https://github.com/Kuan-Liu-Lab/causens},
  url = {https://kuan-liu-lab.github.io/causens/},
}
```

## Getting help or contributing

Please report bugs by opening an
[issue](https://github.com/Kuan-Liu-Lab/causens/issues/new). If you have
a question regarding the usage of `causens`, please open a
[discussion](https://github.com/Kuan-Liu-Lab/causens/discussions/new/choose). 
If you would like to contribute to the package, please open a pull request.
