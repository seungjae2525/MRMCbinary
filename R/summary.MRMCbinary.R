#' @title Summary for \code{MRMCbinary} objects
#'
#' @description Summary the results for object of class \code{MRMCbinary}.
#'
#' @param object An object for class \code{MRMCbinary}.
#' @param digits Number of significant digits. Default: \code{max(1L, getOption("digits") - 3L)}.
#' @param ... Further arguments (currently not used).
#'
#' @details Summary the results for object of class \code{MRMCbinary}.
#' From the conditional logistic regression results, the odds ratio, confidence interval of the odds ratio, and P value are reported.
#'
#' @return No return value, called for side effects.
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
#'                               Case = case, D = truth, Y = Y, measure = "All",
#'                               effect = "Modality", interaction = NULL,
#'                               reference.Modality = "1", reference.Reader = NULL)
#' summary(modality_result, digits = 3)
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                             Case = case, D = truth, Y = Y, measure = "All",
#'                             effect = "Reader", interaction = NULL,
#'                             reference.Modality = NULL, reference.Reader = "1")
#' summary(reader_result, digits = 3)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                  Case = case, D = truth, Y = Y, measure = "All",
#'                                  effect = "Both", interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#' summary(both_result_wo_int, digits = 3)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                    Case = case, D = truth, Y = Y, measure = "All",
#'                                    effect = "Both", interaction = TRUE,
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
  if (!inherits(object, "MRMCbinary")){
    stop("Argument 'object' must be an object of class \"MRMCbinary\".")
  }

  if (object$measure == "All") {
    all_MRMCbinary_ss(object = object, digits = digits)
  } else {
    sens_spec_MRMCbinary_ss(object = object, digits = digits)
  }

  invisible(object)
}
