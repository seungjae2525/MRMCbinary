#' @title Summary for \code{MRMCbinary} objects
#'
#' @description Summary the results for object of class \code{MRMCbinary}.
#'
#' @param object An object for class \code{MRMCbinary}.
#' @param digits Summary digits. Default: max(1L, getOption("digits") - 3L).
#' @param ... Further arguments (currently not used).
#'
#' @details Summary the results for object (\code{MRMCbinary} of class \code{MRMCbinary}.
#'
#' @examples
#' ## Load example data
#' data(VanDyke)
#'
#' ## Return the first parts of an object
#' head(VanDyke)
#'
#' ## Extract Unique readers
#' unique(VanDyke$reader)
#'
#' ## Extract unique modalities
#' unique(VanDyke$treatment)
#'
#' ## Create binary test results (Y_ijk)
#' VanDyke$Y <- as.numeric(VanDyke$rating >= 3)
#'
#' ## Example usage of MRMCbinary function:
#' # When comparing the sensitivities and specificities between modalities
#' modality_result <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                               Case = case, D = truth, Y = Y, effect = "Modality",
#'                               interaction = NULL,
#'                               reference.Modality = "1", reference.Reader = "1")
#' summary(modality_result, digits = 3)
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                             Case = case, D = truth, Y = Y, effect = "Reader",
#'                             interaction = NULL,
#'                             reference.Modality = "1", reference.Reader = "1")
#' summary(reader_result, digits = 3)
#'
#' # When comparing the sensitivities and specificities between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                                  Case = case, D = truth, Y = Y, effect = "Both",
#'                                  interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#' summary(both_result_wo_int, digits = 3)
#'
#' # When comparing the sensitivities and specificities between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
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

  cat("\nclogit formula:  ",
      paste(paste0("survival::clogit(", x$formula, ", data, method=\"exact\")"),
            sep = "\n", collapse = "\n"), "\n\n", sep = "")

  if (x$effect == "Both") {
    ##########################################################################
    ## Sensitivity with interaction term between modality and reader
    cat("\"Sensitivity\" \n")

    cat("Conditional logistic regression results: \n")
    temp_CLR_sen <- round(x$CLR_sen, digits=digits)
    temp_CLR_sen_p <- format_f(xx = temp_CLR_sen$P.value,
                               numer = NULL, denom = NULL,
                               percentage = FALSE, digits=digits)
    temp_CLR_sen_p <- ifelse(temp_CLR_sen_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                             paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                             temp_CLR_sen_p)
    mat_CLR_sen <- data.frame(format_f(xx = temp_CLR_sen$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              format_f(xx = temp_CLR_sen$SE,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              paste0("[", format_f(xx = temp_CLR_sen$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits=digits),
                                     ", ", format_f(xx = temp_CLR_sen$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits=digits), "]"),
                              temp_CLR_sen_p)
    colnames(mat_CLR_sen) <- c("Odds ratio", "Standard error", "Confidence interval", "P value")
    rownames(mat_CLR_sen) <- paste0(c(rep(paste0("Modality", x$reference.Modality), x$n.modality-1),
                                      rep(paste0("Reader", x$reference.Reader), x$n.reader-1)),
                                    " vs ", rownames(temp_CLR_sen))
    base::print(mat_CLR_sen)
    cat("\n")

    cat("Test from conditional logistic regression: \n")
    temp_test_sen <- round(rbind(x$CLR_LRT_sen, x$CLR_Score_sen, x$CLR_Wald_sen), digits=digits)
    temp_test_sen_p <- format_f(xx = temp_test_sen$P.value,
                                numer = NULL, denom = NULL,
                                percentage = FALSE, digits=digits)
    temp_test_sen_p <- ifelse(temp_test_sen_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                              paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                              temp_test_sen_p)
    mat_test_sen <- data.frame(format_f(xx = temp_test_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits=digits),
                               temp_test_sen$DF,
                               temp_test_sen_p)
    colnames(mat_test_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_sen) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_sen)
    cat("\n")

    if (x$interaction == TRUE) {

      if (x$effect == "Modality") {
        if (x$n.modality == 2) {
          cat("McNemar's test: \n")
        } else {
          cat("Cochran's Q test: \n")
        }
      } else if (x$effect == "Reader") {
        if (x$n.reader == 2) {
          cat("McNemar's test: \n")
        } else {
          cat("Cochran's Q test: \n")
        }
      }

      temp_Q_MN_sen <- round(x$Q_MN_sen, digits=digits)
      temp_Q_MN_sen_p <- format_f(xx = temp_Q_MN_sen$P.value,
                                  numer = NULL, denom = NULL,
                                  percentage = FALSE, digits=digits)
      temp_Q_MN_sen_p <- ifelse(temp_Q_MN_sen_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                                paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                                temp_Q_MN_sen_p)
      mat_Q_MN_sen <- data.frame(format_f(xx = temp_Q_MN_sen$Statistic,
                                          numer = NULL, denom = NULL,
                                          percentage = FALSE, digits=digits),
                                 temp_Q_MN_sen$DF,
                                 temp_Q_MN_sen_p)
      colnames(mat_Q_MN_sen) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN_sen) <- c("Q")
      base::print(mat_Q_MN_sen)
      cat("\n\n")

    }

    ##########################################################################
    ## Specificity with interaction term between modality and reader
    cat("\"Specificity\" \n")

    cat("Conditional logistic regression results: \n")
    temp_CLR_spe <- round(x$CLR_spe, digits=digits)
    temp_CLR_spe_p <- format_f(xx = temp_CLR_spe$P.value,
                               numer = NULL, denom = NULL,
                               percentage = FALSE, digits=digits)
    temp_CLR_spe_p <- ifelse(temp_CLR_spe_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                             paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                             temp_CLR_spe_p)
    mat_CLR_spe <- data.frame(format_f(xx = temp_CLR_spe$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              format_f(xx = temp_CLR_spe$SE,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              paste0("[", format_f(xx = temp_CLR_spe$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits=digits),
                                     ", ", format_f(xx = temp_CLR_spe$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits=digits), "]"),
                              temp_CLR_spe_p)
    colnames(mat_CLR_spe) <- c("Odds ratio", "Standard error", "Confidence interval", "P value")
    rownames(mat_CLR_spe) <- paste0(c(rep(paste0("Modality", x$reference.Modality), x$n.modality-1),
                                      rep(paste0("Reader", x$reference.Reader), x$n.reader-1)),
                                    " vs ", rownames(temp_CLR_spe))
    base::print(mat_CLR_spe)
    cat("\n")

    cat("Test from conditional logistic regression: \n")
    temp_test_spe <- round(rbind(x$CLR_LRT_spe, x$CLR_Score_spe, x$CLR_Wald_spe), digits=digits)
    temp_test_spe_p <- format_f(xx = temp_test_spe$P.value,
                                numer = NULL, denom = NULL,
                                percentage = FALSE, digits=digits)
    temp_test_spe_p <- ifelse(temp_test_spe_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                              paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                              temp_test_spe_p)
    mat_test_spe <- data.frame(format_f(xx = temp_test_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits=digits),
                               temp_test_spe$DF,
                               temp_test_spe_p)
    colnames(mat_test_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_spe) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_spe)
    cat("\n")

    if (x$interaction == TRUE) {
      if (x$effect == "Modality") {
        if (x$n.modality == 2) {
          cat("McNemar's test: \n")
        } else {
          cat("Cochran's Q test: \n")
        }
      } else if (x$effect == "Reader") {
        if (x$n.reader == 2) {
          cat("McNemar's test: \n")
        } else {
          cat("Cochran's Q test: \n")
        }
      }

      temp_Q_MN_spe <- round(x$Q_MN_spe, digits=digits)
      temp_Q_MN_spe_p <- format_f(xx = temp_Q_MN_spe$P.value,
                                  numer = NULL, denom = NULL,
                                  percentage = FALSE, digits=digits)
      temp_Q_MN_spe_p <- ifelse(temp_Q_MN_spe_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                                paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                                temp_Q_MN_spe_p)
      mat_Q_MN_spe <- data.frame(format_f(xx = temp_Q_MN_spe$Statistic,
                                          numer = NULL, denom = NULL,
                                          percentage = FALSE, digits=digits),
                                 temp_Q_MN_spe$DF,
                                 temp_Q_MN_spe_p)
      colnames(mat_Q_MN_spe) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN_spe) <- c("Q")
      base::print(mat_Q_MN_spe)
      cat("\n")
    }


  } else {
    ##########################################################################
    ## Sensitivity
    cat("\"Sensitivity\" \n")

    cat("Conditional logistic regression results: \n")
    temp_CLR_sen <- round(x$CLR_sen, digits=digits)
    temp_CLR_sen_p <- format_f(xx = temp_CLR_sen$P.value,
                               numer = NULL, denom = NULL,
                               percentage = FALSE, digits=digits)
    temp_CLR_sen_p <- ifelse(temp_CLR_sen_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                             paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                             temp_CLR_sen_p)
    mat_CLR_sen <- data.frame(format_f(xx = temp_CLR_sen$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              format_f(xx = temp_CLR_sen$SE,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              paste0("[", format_f(xx = temp_CLR_sen$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits=digits),
                                     ", ", format_f(xx = temp_CLR_sen$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits=digits), "]"),
                              temp_CLR_sen_p)
    colnames(mat_CLR_sen) <- c("Odds ratio", "Standard error", "Confidence interval", "P value")
    if (x$effect == "Modality") {
      rownames(mat_CLR_sen) <- paste0(rep(paste0("Modality", x$reference.Modality), x$n.modality-1),
                                      " vs ", rownames(temp_CLR_sen))
    } else {
      rownames(mat_CLR_sen) <- paste0(rep(paste0("Reader", x$reference.Reader), x$n.reader-1),
                                      " vs ", rownames(temp_CLR_sen))
    }

    base::print(mat_CLR_sen)
    cat("\n")

    cat("Test from conditional logistic regression: \n")
    temp_test_sen <- round(rbind(x$CLR_LRT_sen, x$CLR_Score_sen, x$CLR_Wald_sen), digits=digits)
    temp_test_sen_p <- format_f(xx = temp_test_sen$P.value,
                                numer = NULL, denom = NULL,
                                percentage = FALSE, digits=digits)
    temp_test_sen_p <- ifelse(temp_test_sen_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                              paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                              temp_test_sen_p)
    mat_test_sen <- data.frame(format_f(xx = temp_test_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits=digits),
                               temp_test_sen$DF,
                               temp_test_sen_p)
    colnames(mat_test_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_sen) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_sen)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("McNemar's test: \n")
      } else {
        cat("Cochran's Q test: \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("McNemar's test: \n")
      } else {
        cat("Cochran's Q test: \n")
      }
    }

    temp_Q_MN_sen <- round(x$Q_MN_sen, digits=digits)
    temp_Q_MN_sen_p <- format_f(xx = temp_Q_MN_sen$P.value,
                                numer = NULL, denom = NULL,
                                percentage = FALSE, digits=digits)
    temp_Q_MN_sen_p <- ifelse(temp_Q_MN_sen_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                              paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                              temp_Q_MN_sen_p)
    mat_Q_MN_sen <- data.frame(format_f(xx = temp_Q_MN_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits=digits),
                               temp_Q_MN_sen$DF,
                               temp_Q_MN_sen_p)
    colnames(mat_Q_MN_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN_sen) <- c("Q")
    base::print(mat_Q_MN_sen)
    cat("\n\n")

    ##########################################################################
    ## Specificity
    cat("\"Specificity\" \n")

    cat("Conditional logistic regression results: \n")
    temp_CLR_spe <- round(x$CLR_spe, digits=digits)
    temp_CLR_spe_p <- format_f(xx = temp_CLR_spe$P.value,
                               numer = NULL, denom = NULL,
                               percentage = FALSE, digits=digits)
    temp_CLR_spe_p <- ifelse(temp_CLR_spe_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                             paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                             temp_CLR_spe_p)
    mat_CLR_spe <- data.frame(format_f(xx = temp_CLR_spe$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              format_f(xx = temp_CLR_spe$SE,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits=digits),
                              paste0("[", format_f(xx = temp_CLR_spe$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits=digits),
                                     ", ", format_f(xx = temp_CLR_spe$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits=digits), "]"),
                              temp_CLR_spe_p)
    colnames(mat_CLR_spe) <- c("Odds ratio", "Standard error", "Confidence interval", "P value")
    if (x$effect == "Modality") {
      rownames(mat_CLR_spe) <- paste0(rep(paste0("Modality", x$reference.Modality), x$n.modality-1),
                                      " vs ", rownames(temp_CLR_spe))
    } else {
      rownames(mat_CLR_spe) <- paste0(rep(paste0("Reader", x$reference.Reader), x$n.reader-1),
                                      " vs ", rownames(temp_CLR_spe))
    }

    base::print(mat_CLR_spe)
    cat("\n")

    cat("Test from conditional logistic regression: \n")
    temp_test_spe <- round(rbind(x$CLR_LRT_spe, x$CLR_Score_spe, x$CLR_Wald_spe), digits=digits)
    temp_test_spe_p <- format_f(xx = temp_test_spe$P.value,
                                numer = NULL, denom = NULL,
                                percentage = FALSE, digits=digits)
    temp_test_spe_p <- ifelse(temp_test_spe_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                              paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                              temp_test_spe_p)
    mat_test_spe <- data.frame(format_f(xx = temp_test_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits=digits),
                               temp_test_spe$DF,
                               temp_test_spe_p)
    colnames(mat_test_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_spe) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_spe)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("McNemar's test: \n")
      } else {
        cat("Cochran's Q test: \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("McNemar's test: \n")
      } else {
        cat("Cochran's Q test: \n")
      }
    }

    temp_Q_MN_spe <- round(x$Q_MN_spe, digits=digits)
    temp_Q_MN_spe_p <- format_f(xx = temp_Q_MN_spe$P.value,
                                numer = NULL, denom = NULL,
                                percentage = FALSE, digits=digits)
    temp_Q_MN_spe_p <- ifelse(temp_Q_MN_spe_p == paste0("0.", paste0(rep("0", digits), collapse = "")),
                              paste0(paste0("< 0.", paste0(rep("0", digits-1), collapse = "")), "1"),
                              temp_Q_MN_spe_p)
    mat_Q_MN_spe <- data.frame(format_f(xx = temp_Q_MN_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits=digits),
                               temp_Q_MN_spe$DF,
                               temp_Q_MN_spe_p)
    colnames(mat_Q_MN_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN_spe) <- c("Q")
    base::print(mat_Q_MN_spe)
    cat("\n")

  }

  # cat("\n")
  invisible(x)
}
