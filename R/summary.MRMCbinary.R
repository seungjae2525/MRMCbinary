#' @title Summary for \code{MRMCbinary} objects
#'
#' @description Summary the results for object of class \code{MRMCbinary}.
#'
#' @param object An object for class \code{MRMCbinary}.
#' @param digits Summary digits. Default: max(1L, getOption("digits") - 3L).
#' @param ... Further arguments (currently not used).
#'
#' @details Summary the results for object of class \code{MRMCbinary}.
#' In the conditional logistic regression results, the odds ratio, confidence interval of the odds ratio, and P value are reported.
#'
#' @examples
#' ## Load example data
#' data(VanDyke)
#'
#' ## Return the first parts of an object
#' head(VanDyke)
#'
#' ## Extract unique modalities
#' unique(VanDyke$treatment)
#'
#' ## Extract Unique readers
#' unique(VanDyke$reader)
#'
#' ## Create binary test results (Y_ijk)
#' VanDyke$Y <- as.numeric(VanDyke$rating >= 3)
#'
#' ## Example usage of MRMCbinary function:
#' # When comparing the sensitivities and specificities between modalities
#' modality_result <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                               Case = case, D = truth, Y = Y, effect = "Modality",
#'                               interaction = NULL,
#'                               reference.Modality = "1", reference.Reader = NULL)
#' summary(modality_result, digits = 3)
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                             Case = case, D = truth, Y = Y, effect = "Reader",
#'                             interaction = NULL,
#'                             reference.Modality = NULL, reference.Reader = "1")
#' summary(reader_result, digits = 3)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                  Case = case, D = truth, Y = Y, effect = "Both",
#'                                  interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#' summary(both_result_wo_int, digits = 3)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                    Case = case, D = truth, Y = Y, effect = "Both",
#'                                    interaction = TRUE,
#'                                    reference.Modality = "1", reference.Reader = "1")
#' summary(both_result_with_int, digits = 3)
#'
#' @keywords summary
#'
#' @seealso
#'  \code{\link[MRMCbinary]{MRMCbinary}}, \code{\link[MRMCbinary]{print.MRMCbinary}}, \code{\link[base]{summary}}
#'
#' @export

summary.MRMCbinary <- function(object, digits = max(1L, getOption("digits") - 3L), ...) {
  x <- object
  if (!inherits(x, "MRMCbinary")){
    stop("Argument 'x' must be an object of class \"MRMCbinary\".")
  }

  ## Data Structure
  cat("\n")
  cat("###### 1. Data Structure \n")
  cat(paste0("The total number of modalities : ", x$n.modality, "\n"))
  cat(paste0("The total number of readers    : ", x$n.reader, "\n"))
  cat(paste0("The total number of cases      : ", x$n.case, "\n"))
  cat("\n")

  ## Variable List
  cat("###### 2. Variable List \n")
  if (x$effect == "Both") {
    cat(paste0("Modality                        : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                          : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                            : ", as.character(x$args$Case), "\n"))
    cat(paste0("True disease status (D)         : ", as.character(x$args$D), "\n"))
    cat(paste0("Binary diagnostic test (Y)      : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Modality variable : ", x$reference.Modality, "\n"))
    cat(paste0("Referecen for Reader variable   : ", x$reference.Reader, "\n"))
  } else if (x$effect == "Modality") {
    cat(paste0("Modality                        : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                          : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                            : ", as.character(x$args$Case), "\n"))
    cat(paste0("True disease status (D)         : ", as.character(x$args$D), "\n"))
    cat(paste0("Binary diagnostic test (Y)      : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Modality variable : ", x$reference.Modality, "\n"))
  } else if (x$effect == "Reader") {
    cat(paste0("Modality                      : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                        : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                          : ", as.character(x$args$Case), "\n"))
    cat(paste0("True disease status (D)       : ", as.character(x$args$D), "\n"))
    cat(paste0("Binary diagnostic test (Y)    : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Reader variable : ", x$reference.Reader, "\n"))
  }
  cat("\n")

  ## Analysis Description
  cat("###### 3. Analysis Description \n")
  if (x$effect == "Both") {
    cat(paste0("Effect to compare       : Both (Modality effect and Reader effect) \n"))
    cat(paste0("Use of interaction term : ", x$interaction, "\n"))
  } else {
    cat(paste0("Effect to compare: ", x$effect, " effect \n"))
  }
  cat("\n")

  ## Summary results
  cat("###### 4. Analysis Result \n")
  if (x$effect == "Both") {
    ##########################################################################
    ## Sensitivity with interaction term between modality and reader
    cat("#### 4.1. Sensitivity \n")

    cat("## 4.1.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 1, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_sen <- round(x$CLR_sen, digits = digits)
    temp_CLR_sen_p <- format_p(xx = x$CLR_sen$P.value, digits = digits)
    mat_CLR_sen <- data.frame(format_f(xx = temp_CLR_sen$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_sen$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_sen$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_sen$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_sen_p)
    colnames(mat_CLR_sen) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_sen) <- rownames(temp_CLR_sen)
    base::print(mat_CLR_sen)
    cat("\n")

    cat("## 4.1.2. Test from Conditional Logistic Regression \n")
    temp_test_sen <- rbind(x$CLR_LRT_sen, x$CLR_Score_sen, x$CLR_Wald_sen)
    temp_test_sen_p <- format_p(xx = temp_test_sen$P.value, digits = digits)
    temp_test_sen <- round(temp_test_sen, digits = digits)
    mat_test_sen <- data.frame(format_f(xx = temp_test_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_sen$DF,
                               temp_test_sen_p)
    colnames(mat_test_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_sen) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_sen)
    cat("\n")

    if (x$interaction == TRUE) {
      cat("## 4.1.3. Cochran's Q Test \n")

      temp_Q_MN_sen <- round(x$Q_MN_sen, digits = digits)
      temp_Q_MN_sen_p <- format_p(xx = x$Q_MN_sen$P.value, digits = digits)
      mat_Q_MN_sen <- data.frame(format_f(xx = temp_Q_MN_sen$Statistic,
                                          numer = NULL, denom = NULL,
                                          percentage = FALSE, digits = digits),
                                 temp_Q_MN_sen$DF,
                                 temp_Q_MN_sen_p)
      colnames(mat_Q_MN_sen) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN_sen) <- c("")
      base::print(mat_Q_MN_sen)
      cat("\n")

    }

    ##########################################################################
    ## Specificity with interaction term between modality and reader
    cat("#### 4.2. Specificity \n")

    cat("## 4.2.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 0, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_spe <- round(x$CLR_spe, digits = digits)
    temp_CLR_spe_p <- format_p(xx = x$CLR_spe$P.value, digits = digits)
    mat_CLR_spe <- data.frame(format_f(xx = temp_CLR_spe$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_spe$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_spe$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_spe$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_spe_p)
    colnames(mat_CLR_spe) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_spe) <- rownames(temp_CLR_spe)
    base::print(mat_CLR_spe)
    cat("\n")

    cat("## 4.2.2. Test from Conditional Logistic Regression \n")
    temp_test_spe <- rbind(x$CLR_LRT_spe, x$CLR_Score_spe, x$CLR_Wald_spe)
    temp_test_spe_p <- format_p(xx = temp_test_spe$P.value, digits = digits)
    temp_test_spe <- round(temp_test_spe, digits = digits)
    mat_test_spe <- data.frame(format_f(xx = temp_test_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_spe$DF,
                               temp_test_spe_p)
    colnames(mat_test_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_spe) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_spe)
    cat("\n")

    if (x$interaction == TRUE) {
      cat("## 4.2.3. Cochran's Q Test \n")

      temp_Q_MN_spe <- round(x$Q_MN_spe, digits = digits)
      temp_Q_MN_spe_p <- format_p(xx = x$Q_MN_spe$P.value, digits = digits)
      mat_Q_MN_spe <- data.frame(format_f(xx = temp_Q_MN_spe$Statistic,
                                          numer = NULL, denom = NULL,
                                          percentage = FALSE, digits = digits),
                                 temp_Q_MN_spe$DF,
                                 temp_Q_MN_spe_p)
      colnames(mat_Q_MN_spe) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN_spe) <- c("")
      base::print(mat_Q_MN_spe)
      cat("\n")
    }

  } else {
    ##########################################################################
    ## Sensitivity
    cat("#### 4.1. Sensitivity \n")

    cat("## 4.1.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 1, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_sen <- round(x$CLR_sen, digits = digits)
    temp_CLR_sen_p <- format_p(xx = x$CLR_sen$P.value, digits = digits)
    mat_CLR_sen <- data.frame(format_f(xx = temp_CLR_sen$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_sen$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_sen$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_sen$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_sen_p)
    colnames(mat_CLR_sen) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_sen) <- rownames(temp_CLR_sen)

    base::print(mat_CLR_sen)
    cat("\n")

    cat("## 4.1.2. Test from Conditional Logistic Regression \n")
    temp_test_sen <- rbind(x$CLR_LRT_sen, x$CLR_Score_sen, x$CLR_Wald_sen)
    temp_test_sen_p <- format_p(xx = temp_test_sen$P.value, digits = digits)
    temp_test_sen <- round(temp_test_sen, digits = digits)
    mat_test_sen <- data.frame(format_f(xx = temp_test_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_sen$DF,
                               temp_test_sen_p)
    colnames(mat_test_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_sen) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_sen)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("## 4.1.3. McNemar's Test \n")
      } else {
        cat("## 4.1.3. Cochran's Q Test \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("## 4.1.3. McNemar's Test \n")
      } else {
        cat("## 4.1.3. Cochran's Q Test \n")
      }
    }

    temp_Q_MN_sen <- round(x$Q_MN_sen, digits = digits)
    temp_Q_MN_sen_p <- format_p(xx = x$Q_MN_sen$P.value, digits = digits)
    mat_Q_MN_sen <- data.frame(format_f(xx = temp_Q_MN_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_Q_MN_sen$DF,
                               temp_Q_MN_sen_p)
    colnames(mat_Q_MN_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN_sen) <- c("")
    base::print(mat_Q_MN_sen)
    cat("\n")

    ##########################################################################
    ## Specificity
    cat("#### 4.2. Specificity \n")

    cat("## 4.2.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 0, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_spe <- round(x$CLR_spe, digits = digits)
    temp_CLR_spe_p <- format_p(xx = x$CLR_spe$P.value, digits = digits)
    mat_CLR_spe <- data.frame(format_f(xx = temp_CLR_spe$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_spe$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_spe$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_spe$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_spe_p)
    colnames(mat_CLR_spe) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_spe) <- rownames(temp_CLR_spe)

    base::print(mat_CLR_spe)
    cat("\n")

    cat("## 4.2.2. Test from Conditional Logistic Regression \n")
    temp_test_spe <- rbind(x$CLR_LRT_spe, x$CLR_Score_spe, x$CLR_Wald_spe)
    temp_test_spe_p <- format_p(xx = temp_test_spe$P.value, digits = digits)
    temp_test_spe <- round(temp_test_spe, digits = digits)
    mat_test_spe <- data.frame(format_f(xx = temp_test_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_spe$DF,
                               temp_test_spe_p)
    colnames(mat_test_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_spe) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_spe)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("## 4.2.3. McNemar's Test \n")
      } else {
        cat("## 4.2.3. Cochran's Q Test \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("## 4.2.3. McNemar's Test \n")
      } else {
        cat("## 4.2.3. Cochran's Q Test \n")
      }
    }

    temp_Q_MN_spe <- round(x$Q_MN_spe, digits = digits)
    temp_Q_MN_spe_p <- format_p(xx = x$Q_MN_spe$P.value, digits = digits)
    mat_Q_MN_spe <- data.frame(format_f(xx = temp_Q_MN_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_Q_MN_spe$DF,
                               temp_Q_MN_spe_p)
    colnames(mat_Q_MN_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN_spe) <- c("")
    base::print(mat_Q_MN_spe)
    cat("\n")

  }

  # cat("\n")
  invisible(x)
}
