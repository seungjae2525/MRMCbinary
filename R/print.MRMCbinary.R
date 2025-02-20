#' @title Print for \code{MRMCbinary} objects
#'
#' @description Print the results for object of class \code{MRMCbinary}.
#'
#' @param x An object for class \code{MRMCbinary}.
#' @param ... Further arguments (currently not used).
#'
#' @details Print the results for object of class \code{MRMCbinary}.
#' From the conditional logistic regression results, "Estimate" corresponds to the log odds ratio and "SE" corresponds to the standard error of the log odds ratio.
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
#' print(modality_result)
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                             Case = case, D = truth, Y = Y, measure = "All",
#'                             effect = "Reader", interaction = NULL,
#'                             reference.Modality = NULL, reference.Reader = "1")
#' print(reader_result)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                  Case = case, D = truth, Y = Y, measure = "All",
#'                                  effect = "Both", interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#' print(both_result_wo_int)
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                    Case = case, D = truth, Y = Y, measure = "All",
#'                                    effect = "Both", interaction = TRUE,
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

  if (x$measure == "All") {
    all_MRMCbinary_pp(x = x)
  } else {
    sens_spec_MRMCbinary_pp(x = x)
  }

  # cat("\n")
  invisible(x)
}
