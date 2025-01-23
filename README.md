# MRMCbinary

<!-- badges: start -->
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active/)
[![Package version](https://img.shields.io/badge/GitHub-1.0.0-orange.svg)](https://github.com/seungjae2525/MRMCbinary/)
[![minimal R version](https://img.shields.io/badge/R-v4.2.0+-blue.svg)](https://cran.r-project.org/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/seungjae2525/MRMCbinary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seungjae2525/MRMCbinary/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Multi-Reader Multi-Case Analysis of Binary Diagnostic Tests

## Description
This is the source code for the `MRMCbinary` package in R. 
`MRMCbinary` is a package aimed at comparing the performance of diagnostic tests (i.e., sensitivity and specificity) for binary outcome in multi-reader multi-case (MRMC) study.
 
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

## Example
This is a basic example which shows you how to use `MRMCbinary` package:

``` r
## Load MRMCbinary package
library(MRMCbinary)

## Load example data
data(VanDyke)

## Return the first parts of an object
head(VanDyke)

## Extract Unique readers
unique(VanDyke$reader)

## Extract unique modalities
unique(VanDyke$treatment)

## Create binary test results (Y_ijk)
VanDyke$Y <- as.numeric(VanDyke$rating >= 3)

## Example usage of MRMCbinary function:
# When comparing the sensitivities and specificities between modalities
modality_result <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
                              Case = case, D = truth, Y = Y, effect = "Modality",
                              interaction = NULL,
                              reference.Modality = "1", reference.Reader = "1")
print(modality_result)
summary(modality_result, digits = 3)

# When comparing the sensitivities and specificities between readers
reader_result <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
                            Case = case, D = truth, Y = Y, effect = "Reader",
                            interaction = NULL,
                            reference.Modality = "1", reference.Reader = "1")
print(reader_result)
summary(reader_result, digits = 3)

# When comparing the sensitivities and specificities between modalities and between readers together
#  not considering interaction between modalities and readers
both_result_wo_int <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
                                 Case = case, D = truth, Y = Y, effect = "Both",
                                 interaction = FALSE,
                                 reference.Modality = "1", reference.Reader = "1")
print(both_result_wo_int)
summary(both_result_wo_int, digits = 3)

# When comparing the sensitivities and specificities between modalities and between readers together
#  considering interaction between modalities and readers
both_result_with_int <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
                                   Case = case, D = truth, Y = Y, effect = "Both",
                                   interaction = TRUE,
                                   reference.Modality = "1", reference.Reader = "1")
print(both_result_with_int)
summary(both_result_with_int, digits = 3)
```

### Bug Reports:
You can also report bugs on GitHub under [Issues](https://github.com/seungjae2525/MRMCbinary/issues/).
