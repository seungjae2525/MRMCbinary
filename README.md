# MRMCbinary

<!-- badges: start -->
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active/)
[![Package version](https://img.shields.io/badge/GitHub-1.0.0-orange.svg)](https://github.com/seungjae2525/MRMCbinary/)
[![minimal R version](https://img.shields.io/badge/R-v4.2.0+-blue.svg)](https://cran.r-project.org/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/seungjae2525/MRMCbinary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seungjae2525/MRMCbinary/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# MRMCbinary: Evaluating Diagnostic Accuracy of Binary Medical Tests in Multi-reader Multi-case Study

## Description
This is the source code for the `MRMCbinary` package in R. 
`MRMCbinary` is a package aimed at analyzing the performance of diagnostic tests for binary outcome.
 
### Reference
Lee, S., Jang, S., and Lee, W. Evaluating Diagnostic Accuracy of Binary Medical Tests in Multi-reader Multi-case Study.

## Installation
### Current GitHub release:
Installation using R package `devtools`:

```r
if (!require("devtools")) { install.packages("devtools") } # if devtools not already installed
devtools::install_github("seungjae2525/MRMCbinary")
library(MRMCbinary)
```

### Bug Reports:
You can also report bugs on GitHub under [Issues](https://github.com/seungjae2525/MRMCbinary/issues/).
