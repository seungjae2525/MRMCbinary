#' @title Print for \code{MRMCbinary} objects
#'
#' @description Print the results for object of class \code{MRMCbinary}.
#'
#' @param x An object for class \code{MRMCbinary}.
#' @param ... Further arguments (currently not used).
#'
#' @details Print the results for object of class \code{MRMCbinary}).
#' In the conditional logistic regression results, "Estimate" corresponds to the log odds ratio and "SE" corresponds to the standard error of the log odds ratio.
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
#' print(modality_result)
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                             Case = case, D = truth, Y = Y, effect = "Reader",
#'                             interaction = NULL,
#'                             reference.Modality = NULL, reference.Reader = "1")
#' print(reader_result)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                  Case = case, D = truth, Y = Y, effect = "Both",
#'                                  interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#' print(both_result_wo_int)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                    Case = case, D = truth, Y = Y, effect = "Both",
#'                                    interaction = TRUE,
#'                                    reference.Modality = "1", reference.Reader = "1")
#' print(both_result_with_int)
#'
#' @keywords print
#'
#' @seealso
#'  \code{\link[MRMCbinary]{MRMCbinary}}, \code{\link[MRMCbinary]{summary.MRMCbinary}}, \code{\link[base]{print}}
#'
#' @export

print.MRMCbinary <- function(x, ...) {
  if (!inherits(x, "MRMCbinary")){
    stop("Argument 'x' must be an object of class \"MRMCbinary\".")
  }

  cat("\nclogit formula:  ",
      paste(paste0("survival::clogit(", x$formula, ", data, method=\"exact\")"),
            sep = "\n", collapse = "\n"), "\n\n", sep = "")

  if (x$effect == "Both") {
    if (x$interaction == TRUE) {

      ############################################################################
      ## Sensitivity with interaction term between modality and reader
      cat("\"Sensitivity\" \n")

      cat("Conditional logistic regression results: \n")
      x$CLR_sen$Estimate <- log (x$CLR_sen$Estimate)
      base::print(x$CLR_sen[, c(1,2,5)])
      cat("\n")

      cat("Likelihood ratio test from conditional logistic regression: \n")
      base::print(x$CLR_LRT_sen)
      cat("\n")

      cat("Score test from conditional logistic regression: \n")
      base::print(x$CLR_Score_sen)
      cat("\n")

      cat("Wald test from conditional logistic regression: \n")
      base::print(x$CLR_Wald_sen)
      cat("\n")

      cat("Cochran's Q test: \n")
      base::print(x$Q_MN_sen)
      cat("\n\n")


      ############################################################################
      ## Specificity with interaction term between modality and reader
      cat("\"Specificity\" \n")

      cat("Conditional logistic regression results: \n")
      x$CLR_spe$Estimate <- log (x$CLR_spe$Estimate)
      base::print(x$CLR_spe[, c(1,2,5)])
      cat("\n")

      cat("Likelihood ratio test from conditional logistic regression: \n")
      base::print(x$CLR_LRT_spe)
      cat("\n")

      cat("Score test from conditional logistic regression: \n")
      base::print(x$CLR_Score_spe)
      cat("\n")

      cat("Wald test from conditional logistic regression: \n")
      base::print(x$CLR_Wald_spe)
      cat("\n")

      cat("Cochran's Q test: \n")
      base::print(x$Q_MN_spe)
      cat("\n")

    } else {

      ############################################################################
      ## Sensitivity
      cat("\"Sensitivity\" \n")

      cat("Conditional logistic regression results: \n")
      x$CLR_sen$Estimate <- log (x$CLR_sen$Estimate)
      base::print(x$CLR_sen[, c(1,2,5)])
      cat("\n")

      cat("Likelihood ratio test from conditional logistic regression: \n")
      base::print(x$CLR_LRT_sen)
      cat("\n")

      cat("Score test from conditional logistic regression: \n")
      base::print(x$CLR_Score_sen)
      cat("\n")

      cat("Wald test from conditional logistic regression: \n")
      base::print(x$CLR_Wald_sen)
      cat("\n\n")


      ############################################################################
      ## Specificity
      cat("\"Specificity\" \n")

      cat("Conditional logistic regression results: \n")
      x$CLR_spe$Estimate <- log (x$CLR_spe$Estimate)
      base::print(x$CLR_spe[, c(1,2,5)])
      cat("\n")

      cat("Likelihood ratio test from conditional logistic regression: \n")
      base::print(x$CLR_LRT_spe)
      cat("\n")

      cat("Score test from conditional logistic regression: \n")
      base::print(x$CLR_Score_spe)
      cat("\n")

      cat("Wald test from conditional logistic regression: \n")
      base::print(x$CLR_Wald_spe)
      cat("\n")

    }

  } else {
    ############################################################################
    ## Sensitivity
    cat("\"Sensitivity\" \n")

    cat("Conditional logistic regression results: \n")
    x$CLR_sen$Estimate <- log (x$CLR_sen$Estimate)
    base::print(x$CLR_sen[, c(1,2,5)])
    cat("\n")

    cat("Likelihood ratio test from conditional logistic regression: \n")
    base::print(x$CLR_LRT_sen)
    cat("\n")

    cat("Score test from conditional logistic regression: \n")
    base::print(x$CLR_Score_sen)
    cat("\n")

    cat("Wald test from conditional logistic regression: \n")
    base::print(x$CLR_Wald_sen)
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
    base::print(x$Q_MN_sen)
    cat("\n\n")

    ############################################################################
    ## Specificity
    cat("\"Specificity\" \n")

    cat("Conditional logistic regression results: \n")
    x$CLR_spe$Estimate <- log (x$CLR_spe$Estimate)
    base::print(x$CLR_spe[, c(1,2,5)])
    cat("\n")

    cat("Likelihood ratio test from conditional logistic regression: \n")
    base::print(x$CLR_LRT_spe)
    cat("\n")

    cat("Score test from conditional logistic regression: \n")
    base::print(x$CLR_Score_spe)
    cat("\n")

    cat("Wald test from conditional logistic regression: \n")
    base::print(x$CLR_Wald_spe)
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
    base::print(x$Q_MN_spe)
  }

  # cat("\n")
  invisible(x)
}
