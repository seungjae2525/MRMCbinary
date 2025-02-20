MRMCbinary <- function(...) UseMethod("MRMCbinary")

#' @title Multi-reader multi-case analysis of binary diagnostic tests
#'
#' @description \code{MRMCbinary()} is the main function of \code{MRMCbinary} package and
#' can be used to compare sensitivity and specificity of diagnostic tests for binary outcomes in multi-reader multi-case (MRMC) studies.
#'
#' @param data A data frame in which contains the modality identifiers (\code{Modality}), the reader identifiers (\code{Reader}), the case identifiers (\code{Case}), the true disease status (\code{D}), and the binary diagnostic test result (\code{Y}).
#' @param Modality Variable of the modality identifiers.
#' @param Reader Variable of the reader identifiers.
#' @param Case Variable of the case identifiers.
#' @param D Variable of the true disease status. It should be set the value to 1 for cases diseased and to 0 for those non-diseased.
#' @param Y Variable of the binary diagnostic test result. It should be set the value to 1 for cases diagnosed as positive and to 0 for those diagnosed as negative.
#' @param measure Diagnostic accuracy measure (one of \code{"All"}, \code{"Sensitivity"}, and \code{"Specificity"}).
#' @param effect Effect to compare sensitivity and specificity (one of \code{"Modality"}, \code{"Reader"}, and \code{"Both"}). See \bold{Details}.
#' @param interaction When evaluating the interaction effect between modality and reader, \code{interaction = TRUE}, otherwise \code{interaction = FALSE}. Specify only when \code{effect} is \code{"Both"}. Default: \code{NULL}. See \bold{Details}.
#' @param reference.Modality Reference in variable of the modality identifiers.
#' @param reference.Reader Reference in variable of the reader identifiers.
#'
#' @return An object of class \code{MRMCbinary}. The object is a data.frame with the following components:
#' \item{CLR_sen}{Conditional logistic regression results for sensitivity.}
#' \item{CLR_LRT_sen}{Likelihood ratio test from the conditional logistic regression results for sensitivity.}
#' \item{CLR_Score_sen}{Score test from the conditional logistic regression results for sensitivity.}
#' \item{CLR_Wald_sen}{Wald test from the conditional logistic regression results for sensitivity.}
#' \item{Q_MN_sen}{Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result for sensitivity. This is only reported if (1) \code{effect = "Modality"}, (2) \code{effect = "Reader"}, or (3) \code{effect = "Both"} and \code{interaction = TRUE}.}
#' \item{CLR_spe}{Conditional logistic regression results for specificity.}
#' \item{CLR_LRT_spe}{Likelihood ratio test from the conditional logistic regression results for specificity.}
#' \item{CLR_Score_spe}{Score test from the conditional logistic regression results for specificity.}
#' \item{CLR_Wald_spe}{Wald test from the conditional logistic regression results for specificity.}
#' \item{Q_MN_spe}{Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result for specificity. This is only reported if (1) \code{effect = "Modality"}, (2) \code{effect = "Reader"}, or (3) \code{effect = "Both"} and \code{interaction = TRUE}.}
#' \item{formula}{Formula used in the conditional logistic regression.}
#' \item{args}{List of arguments used in the \code{MRMCbinary} function.}
#' \item{n.modality}{Total number of modalities.}
#' \item{n.reader}{Total number of readers.}
#' \item{n.case}{Total number of cases.}
#' \item{effect}{Effect to compare sensitivity and specificity.}
#' \item{measure}{Diagnostic accuracy measure.}
#' \item{interaction}{This is only included in the \code{MRMCbinary} object when \code{effect = "Both"}. If one want to evaluate the interaction effect between modality and reader in the conditional logistic regression, \code{interaction = TRUE}, otherwise \code{interaction = FALSE}.}
#' \item{reference.Modality}{Reference in variable of the modality identifiers.}
#' \item{reference.Reader}{Reference in variable of the reader identifiers.}
#' \item{n.diseased}{The number of diseased cases. If \code{measure = "Specificity"}, then n.diseased is NULL.}
#' \item{n.nondiseased}{The number of non-diseased cases. If \code{measure = "Sensitivity"}, then n.nondiseased is NULL.}
#' \item{n.pos.diseased}{The number of test positive cases among diseased cases. If \code{measure = "Specificity"}, then n.pos.diseased is NULL.}
#' \item{n.pos.nondiseased}{The number of test positive cases among non-diseased cases. If \code{measure = "Sensitivity"}, then n.pos.nondiseased is NULL.}
#' The results for the \code{MRMCbinary} are printed with the \code{\link[MRMCbinary]{print.MRMCbinary}} function.
#' Also, the results for the \code{MRMCbinary} are summarized with the \code{\link[MRMCbinary]{summary.MRMCbinary}} function.
#'
#' @details There are three effects that can be evaluated:
#'
#'   * \code{effect = "Modality"}: This is used when the goal is to exclusively evaluate the effects of multiple modalities.
#'   And, Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result is reported.
#'   When \code{effect = "Modality"}, \code{interaction} must be set to NULL.
#'   * \code{effect = "Reader"}: This is used when the goal is to exclusively evaluate the effects of multiple readers.
#'   And, Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result is reported.
#'   When \code{effect = "Reader"}, \code{interaction} must be set to NULL.
#'   * \code{effect = "Both"}: This is used when the goal is to simultaneously evaluate the effects of multiple modalities and multiple readers.
#'   In this case, \code{interaction} must be specified (TRUE or FALSE).
#'   If one want to evaluate the interaction effect between modality and reader in the conditional logistic regression, set \code{interaction = TRUE}, otherwise \code{interaction = FALSE}.
#'   When \code{interaction = TRUE}, Cochran's Q test result is reported.
#'   However, when \code{interaction = FALSE}, Cochran's Q test or McNemar's test result is not reported.
#'
#' See Lee et al. (2025) for details.
#'
#' @examples
#' ## Load example data
#' data(VanDyke)
#'
#' ## Return the first parts of an object
#' head(VanDyke)
#'
#' ## See unique readers
#' unique(VanDyke$reader)
#'
#' ## See unique modalities
#' unique(VanDyke$treatment)
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
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                             Case = case, D = truth, Y = Y, measure = "All",
#'                             effect = "Reader", interaction = NULL,
#'                             reference.Modality = NULL, reference.Reader = "1")
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                  Case = case, D = truth, Y = Y, measure = "All",
#'                                  effect = "Both", interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#'
#' # When comparing the sensitivities and specificities
#' #  between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Modality = treatment, Reader = reader,
#'                                    Case = case, D = truth, Y = Y, measure = "All",
#'                                    effect = "Both", interaction = TRUE,
#'                                    reference.Modality = "1", reference.Reader = "1")
#'
#' @seealso
#'  \code{\link[MRMCbinary]{print.MRMCbinary}}, \code{\link[MRMCbinary]{summary.MRMCbinary}}
#'
#' @references
#' Lee, S., Jang, S., and Lee, W. Evaluating Diagnostic Accuracy of Binary Medical Tests in Multi-reader Multi-case Study.
#'
#' @keywords methods
#'
#' @export

MRMCbinary <- function(data, Modality, Reader, Case, D, Y, measure,
                       effect, interaction = NULL,
                       reference.Modality = NULL, reference.Reader = NULL) {
  ## List of arguments for MRMCbinary function
  args <- eval(substitute(alist(Modality = Modality, Reader = Reader, Case = Case, D = D, Y = Y)))

  if (is.null(args$Modality)) stop("Variable name for \"Modality\" must be specified.")
  if (is.null(args$Reader)) stop("Variable name for \"Reader\" must be specified.")
  if (is.null(args$Case)) stop("Variable name for \"Case\" must be specified.")
  if (is.null(args$Y)) stop("Variable name for \"Y\" must be specified.")

  ## Check if "args" is included in the column name of data!
  if (as.character(args$Modality) %notin% colnames(data)) {
    stop("Modality variable must be included in data.")
  }
  if (as.character(args$Reader) %notin% colnames(data)) {
    stop("Reader variable must be included in data.")
  }
  if (as.character(args$Case) %notin% colnames(data)) {
    stop("Case variable must be included in data.")
  }

  if (is.null(args$D) & measure %notin% c("Sensitivity", "Specificity")) {
    stop("If D = NULL, then \"measure\" must be specified as \"Sensitivity\" or \"Specificity\".")
  }
  if (measure %in% c("Sensitivity", "Specificity")) {
    if (!is.null(args$D) & measure == "Sensitivity") {
      if (all(data[[as.character(args$D)]] != 1)) {
        stop("If measure == \"Sensitivity\" but D != NULL, then true disease status variable (i.e., D) should be set the value to 1 for all cases.")
      }
    } else if (!is.null(args$D) & measure == "Specificity") {
      if (all(data[[as.character(args$D)]] != 0)) {
        stop("If measure == \"Specificity\" but D != NULL, then true disease status variable (i.e., D) should be set the value to 0 for all cases.")
      }
    }
  } else if (measure == "All") {
    if (as.character(args$D) %notin% colnames(data)) {
      stop("If measure == \"All\", then true disease status variable (i.e., D) must be included in data.")
    }
    if (all(data[[as.character(args$D)]] %notin% c(0, 1))) {
      stop("If measure == \"All\", then true disease status variable (i.e., D) should be set the value to 1 for cases diseased and to 0 for those non-diseased.")
    }
    if (all(data[[as.character(args$D)]] == 1)) {
      stop("All cases are diseased. If measure == \"All\", data should consist of both diseased and non-diseased cases.")
    }
    if (all(data[[as.character(args$D)]] == 0)) {
      stop("All cases are non-diseased. If measure == \"All\", data should consist of both diseased and non-diseased cases.")
    }
  } else {
    stop("\"measure\" should be specified as \"All\", \"Sensitivity\", or \"Specificity\").")
  }

  if (as.character(args$Y) %notin% colnames(data)) {
    stop("Diagnostic test result variable (i.e., Y) must be included in data.")
  }
  if (all(data[[as.character(args$Y)]] %notin% c(0, 1))) {
    stop("Diagnostic test result variable (i.e., Y) should be set the value to 1 for cases diagnosed as positive and to 0 for those diagnosed as negative.")
  }

  ## Extract unique values for Modality, Reader, and Case
  Modalities <- unique(data[[as.character(args$Modality)]])
  Readers <- unique(data[[as.character(args$Reader)]])
  Cases <- unique(data[[as.character(args$Case)]])

  ## Date check!
  # Fully crossed MRMC ?
  if (length(Modalities) * length(Readers) * length(Cases) != nrow(data)) {
    stop("The current data are not from a balanced (fully crossed) MRMC study.")
  }
  # Number of modalities >= 2 ?
  if (length(Modalities) < 2) {
    stop("The number of modalities must be greater than or equal to 2.")
  }
  # Number of readers >= 2 ?
  if (length(Readers) < 2) {
    stop("The number of readers must be greater than or equal to 2.")
  }

  ## Warnings when effect = "Modality"
  if (effect == "Modality") {
    if (!is.null(reference.Reader)) {
      warning(paste0("If effect = \"Modality\", then \"reference.Reader\" is not used. \"reference.Reader\" is set to NULL."))
      reference.Reader <- NULL
    }

    # When reference.Modality is NULL
    if (is.null(reference.Modality)) {
      warning(paste0("\"reference.Modality\" is NULL. \"reference.Modality\" is set to the first modality (i.e., ", Modalities[1], ")."))
      reference.Modality <- Modalities[1]
    }
    # Is reference.Modality a value contained in the Modality variable?
    if (reference.Modality %notin% Modalities) {
      stop("Reference of Modality variable must be included in data.")
    }
  }

  ## Warnings when effect = "Reader"
  if (effect == "Reader") {
    if (!is.null(reference.Modality)) {
      warning(paste0("If effect = \"Reader\", then \"reference.Modality\" is not used. \"reference.Modality\" is set to NULL."))
      reference.Modality <- NULL
    }

    # When reference.Reader is NULL
    if (is.null(reference.Reader)) {
      warning(paste0("\"reference.Reader\" is NULL. \"reference.Reader\" is set to the first reader (i.e., ", Readers[1], ")."))
      reference.Reader <- Readers[1]
    }
    # Is reference.Reader a value contained in the Reader variable?
    if (reference.Reader %notin% Readers) {
      stop("Reference of Reader variable must be included in data.")
    }
  }

  ## For interaction
  if (effect %in% c("Modality", "Reader") & !is.null(interaction)) {
    warning("If \"effect\" is \"Modality\" or \"Reader\", then \"interaction\" sholud be NULL.")
    interaction <- NULL
  }
  if (effect == "Both") {
    if (is.null(interaction)) {
      stop("If \"effect\" is \"Both\", then \"interaction\" sholud be either TRUE or FALSE.")
    }
  }

  ## Make data for MRMC analysis
  if (measure == "All") {
    all_data <- make_MRMCdata_all(data = data, args = args, effect = effect)
    Final_result <- analysis_MRMC_all(all_data = all_data, effect = effect,
                                      interaction = interaction,
                                      Modalities = Modalities, Readers = Readers)
    n.diseased <- nrow(all_data[[1]])
    n.nondiseased <- nrow(all_data[[2]])
    n.pos.diseased <- sum(all_data[[1]]$Y)
    n.pos.nondiseased <- sum(all_data[[2]]$Y)

  } else {
    all_data <- make_MRMCdata_sens_spec(data = data, args = args, effect = effect,
                                        measure = measure,
                                        n.modality = length(Modalities),
                                        n.reader = length(Readers))
    Final_result <- analysis_MRMC_sens_spec(all_data = all_data, effect = effect,
                                            interaction = interaction,
                                            Modalities = Modalities, Readers = Readers)
    n.nd_d_cases <- nrow(all_data[[1]])
    n.pos <- sum(all_data[[1]]$Y)
  }

  #
  Final_result$args <- args
  Final_result$n.modality <- length(Modalities)
  Final_result$n.reader <- length(Readers)
  Final_result$n.case <- length(Cases)
  Final_result$effect <- effect
  Final_result$measure <- measure
  Final_result$interaction <- interaction
  Final_result$reference.Modality <- reference.Modality
  Final_result$reference.Reader <- reference.Reader

  if (measure == "All") {
    Final_result$n.diseased <- n.diseased / (length(Modalities) * length(Readers))
    Final_result$n.nondiseased <- n.nondiseased / (length(Modalities) * length(Readers))
    Final_result$n.pos.diseased <- n.pos.diseased / (length(Modalities) * length(Readers))
    Final_result$n.pos.nondiseased <- n.pos.nondiseased / (length(Modalities) * length(Readers))
  } else if (measure == "Sensitivity") {
    Final_result$n.diseased <- n.nd_d_cases / (length(Modalities) * length(Readers))
    Final_result$n.pos.diseased <- n.pos / (length(Modalities) * length(Readers))
  } else {
    Final_result$n.nondiseased <- n.nd_d_cases / (length(Modalities) * length(Readers))
    Final_result$n.pos.nondiseased <- n.pos / (length(Modalities) * length(Readers))
  }

  class(Final_result) <- c("MRMCbinary")
  return(Final_result)
}


