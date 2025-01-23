MRMCbinary <- function(...) UseMethod("MRMCbinary")

#' @title Multi-reader multi-case analysis of binary diagnostic tests
#'
#' @description \code{MRMCbinary()} is the main function of \code{MRMCbinary} package and
#' can be used to compare sensitivity and specificity of diagnostic tests for binary outcome in multi-reader multi-case (MRMC) study.
#'
#' @param data A data frame in which contains the reader identifiers (Reader), modality identifiers (Modality), case identifiers (Case), true disease status (D), and binary diagnostic test result (Y).
#' @param Reader Variable of reader identifiers.
#' @param Modality Variable of modality identifiers.
#' @param Case Variable of case identifiers.
#' @param D Variable of true disease status. It should be set the value to 1 for cases diseased and to 0 for those non-diseased.
#' @param Y Variable of binary diagnostic test result. It should be set the value to 1 for cases diagnosed as positive and to 0 for those diagnosed as negative.
#' @param effect Effect one wants to evaluate (one of "Modality", "Reader", and "Both"). See details.
#' @param interaction If one want to evaluate the interaction effect between modality and reader, interaction = "TRUE", otherwise "FALSE". Specify only when effect is "Both". Default: NULL. See details.
#' @param reference.Modality Reference in Variable of modality identifiers.
#' @param reference.Reader Reference in Variable of reader identifiers.
#'
#' @return An object of class \code{MRMCbinary}. The object is a data.frame with the following components:
#' \item{CLR_sen}{Conditional logistic regression results for sensitivity.}
#' \item{CLR_LRT_sen}{Likelihood ratio test from the conditional logistic regression results for sensitivity.}
#' \item{CLR_Score_sen}{Score test from the conditional logistic regression results for sensitivity.}
#' \item{CLR_Wald_sen}{Wald test from the conditional logistic regression results for sensitivity.}
#' \item{Q_MN_sen}{Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result for sensitivity. This is only reported if (1) effect = "Modality", (2) effect = "Reader", or (3) effect = "Both" and interaction = TRUE.}
#' \item{CLR_spe}{Conditional logistic regression results for specificity.}
#' \item{CLR_LRT_spe}{Likelihood ratio test from the conditional logistic regression results for specificity.}
#' \item{CLR_Score_spe}{Score test from the conditional logistic regression results for specificity.}
#' \item{CLR_Wald_spe}{Wald test from the conditional logistic regression results for specificity.}
#' \item{Q_MN_spe}{Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result for specificity. This is only reported if (1) effect = "Modality", (2) effect = "Reader", or (3) effect = "Both" and interaction = TRUE.}
#' \item{formula}{Formula used in the conditional logistic regression.}
#' \item{n.reader}{Total number of readers.}
#' \item{n.modality}{Total number of modalities.}
#' \item{effect}{Effect one wants to evaluate.}
#' \item{interaction}{This is only reported if effect = "Both". If one want to evaluate the interaction effect between modality and reader in the conditional logistic regression, interaction = "TRUE", otherwise "FALSE".}
#' \item{reference.Modality}{Reference in variable of modality identifiers.}
#' \item{reference.Reader}{Reference in variable of reader identifiers.}
#' The results for the \code{MRMCbinary} are printed with the \code{\link[MRMCbinary]{print.MRMCbinary}} function.
#' Also, the results for the \code{MRMCbinary} are summarized with the \code{\link[MRMCbinary]{summary.MRMCbinary}} function.
#'
#' @details There are three effects that can be evaluated:
#'
#'   * effect = "Modality": This is used when the goal is to exclusively evaluate the effect of multiple modalities.
#'   And, Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result are reported.
#'   When effect = "Modality", "interaction" must be set to NULL.
#'   * effect = "Reader": This is used when the goal is to exclusively evaluate the effect of multiple readers.
#'   And, Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result are reported.
#'   When effect = "Reader", "interaction" must be set to NULL.
#'   * effect = "Both": This is used when the goal is to simultaneously evaluate the effect of multiple modalities and multiple readers.
#'   In this case, "interaction" must be specified (TRUE or FALSE).
#'   If one want to evaluate the interaction effect between modality and reader in the conditional logistic regression, interaction = "TRUE", otherwise "FALSE".
#'   When interaction = "TRUE", Cochran's Q test (when the number of modalities is greater than 2) or McNemar's test (when the number of modalities is equal to 2) result are reported.
#'   However, when interaction = "FASLE", Cochran's Q test or McNemar's test result are not reported.
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
#' modality_result <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                               Case = case, D = truth, Y = Y, effect = "Modality",
#'                               interaction = NULL,
#'                               reference.Modality = "1", reference.Reader = "1")
#'
#' # When comparing the sensitivities and specificities between readers
#' reader_result <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                             Case = case, D = truth, Y = Y, effect = "Reader",
#'                             interaction = NULL,
#'                             reference.Modality = "1", reference.Reader = "1")
#'
#' # When comparing the sensitivities and specificities between modalities and between readers together
#' #  not considering interaction between modalities and readers
#' both_result_wo_int <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                                  Case = case, D = truth, Y = Y, effect = "Both",
#'                                  interaction = FALSE,
#'                                  reference.Modality = "1", reference.Reader = "1")
#'
#' # When comparing the sensitivities and specificities between modalities and between readers together
#' #  considering interaction between modalities and readers
#' both_result_with_int <- MRMCbinary(data = VanDyke, Reader = reader, Modality = treatment,
#'                                    Case = case, D = truth, Y = Y, effect = "Both",
#'                                    interaction = TRUE,
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

MRMCbinary <- function(data, Reader, Modality, Case, D, Y, effect,
                       interaction = NULL,
                       reference.Modality = NULL, reference.Reader = NULL) {
  ## List of arguments for MRMCbinary function
  args <- eval(substitute(alist(Reader = Reader, Modality = Modality, Case = Case, D = D, Y = Y)))

  ## Check if "args" is included in the column name of data!
  `%notin%` <- function(x, y) !(x %in% y)

  if (as.character(args$Reader) %notin% colnames(data)) {
    stop("\n Error: Reader variable must be included in data.")
  }
  if (as.character(args$Modality) %notin% colnames(data)) {
    stop("\n Error: Modality variable must be included in data.")
  }
  if (as.character(args$Case) %notin% colnames(data)) {
    stop("\n Error: Case variable must be included in data.")
  }
  if (as.character(args$D) %notin% colnames(data)) {
    stop("\n Error: True disease status variable (i.e., D) must be included in data.")
  }
  if (as.character(args$Y) %notin% colnames(data)) {
    stop("\n Error: Diagnostic test result variable (i.e., Y) must be included in data.")
  }
  if (all(data[[as.character(args$D)]] %notin% c(0, 1))) {
    stop("\n Error: True disease status variable (i.e., D) should be set the value to 1 for cases diseased and to 0 for those non-diseased.")
  }
  if (all(data[[as.character(args$Y)]] %notin% c(0, 1))) {
    stop("\n Error: Diagnostic test result variable (i.e., Y) should be set the value to 1 for cases diagnosed as positive and to 0 for those diagnosed as negative.")
  }

  ## Extract unique values for Reader and Modality
  Cases <- unique(data[[as.character(args$Case)]])
  Readers <- unique(data[[as.character(args$Reader)]])
  Modalities <- unique(data[[as.character(args$Modality)]])

  ## Date check!
  # Fully crossed MRMC ?
  if (length(Readers) * length(Modalities) * length(Cases) != nrow(data)) {
    stop("\n Error: The current data are not from a fully crossed MRMC study.")
  }
  # Number of readers >= 2 ?
  if (length(Readers) < 2) {
    stop("\n Error: The number of readers must be greater than or equal to 2.")
  }
  # Number of modalities >= 2 ?
  if (length(Modalities) < 2) {
    stop("\n Error: The number of modalities must be greater than or equal to 2.")
  }
  # When reference.Reader is NULL
  if (is.null(reference.Reader)) {
    warning(paste0("\"reference.Reader\" is NULL. \"reference.Reader\" is set to the first reader (i.e., ", Readers[1], ")."))
    reference.Reader <- Readers[1]
  }
  # Is reference.Reader a value contained in the Reader variable?
  if (reference.Reader %notin% Readers) {
    stop("\n Error: Reference of Reader variable must be included in data.")
  }
  # When reference.Modality is NULL
  if (is.null(reference.Modality)) {
    warning(paste0("\"reference.Modality\" is NULL. \"reference.Modality\" is set to the first modality (i.e., ", Modalities[1], ")."))
    reference.Modality <- Modalities[1]
  }
  # Is reference.Modality a value contained in the Modality variable?
  if (reference.Modality %notin% Modalities) {
    stop("\n Error: Reference of Modality variable must be included in data.")
  }

  ## Warnings
  if (effect %in% c("Modality", "Reader") & !is.null(interaction)) {
    warning("If \"effect\" is \"Modality\" or \"Reader\", then \"interaction\" sholud be NULL.")
    interaction <- NULL
  }
  if (effect == "Both") {
    if (is.null(interaction)) {
      stop("\n Error: If \"effect\" is \"Both\", then \"interaction\" sholud be either TRUE or FALSE.")
    }
  }

  ## Make data for MRMC analysis
  all_data <- make_MRMCdata(data = data, args = args, effect = effect)

  ## Split data
  data_sen_clr <- all_data[[1]]
  data_spe_clr <- all_data[[2]]
  data_sen_q <- all_data[[3]]
  data_spe_q <- all_data[[4]]

  ##
  if (effect == "Modality") {
    CLR_result_sen <- survival::clogit(formula = Y ~ Modality + strata(Case, Reader),
                                       data = data_sen_clr)
    CLR_result_spe <- survival::clogit(formula = Y ~ Modality + strata(Case, Reader),
                                       data = data_spe_clr)
    if (length(Modalities) == 2) {
      Q_MN_result_sen <- stats::mcnemar.test(x = table(data_sen_q[, -c(1,2)]), correct = FALSE)
      Q_MN_result_spe <- stats::mcnemar.test(x = table(data_spe_q[, -c(1,2)]), correct = FALSE)
    } else {
      Q_MN_result_sen <- DescTools::CochranQTest(y = as.matrix(data_sen_q[, -c(1,2)]))
      Q_MN_result_spe <- DescTools::CochranQTest(y = as.matrix(data_spe_q[, -c(1,2)]))
    }
    ##
    Final_result <- result_mat(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
                               Q_MN_result_sen = Q_MN_result_sen, Q_MN_result_spe = Q_MN_result_spe,
                               effect = effect, interaction = interaction)

    Final_result$formula <- as.character(CLR_result_spe$userCall)[2]

  } else if (effect == "Reader") {
    CLR_result_sen <- survival::clogit(formula = Y ~ Reader + strata(Case, Modality),
                                       data = data_sen_clr)
    CLR_result_spe <- survival::clogit(formula = Y ~ Reader + strata(Case, Modality),
                                       data = data_spe_clr)
    if (length(Readers) == 2) {
      Q_MN_result_sen <- stats::mcnemar.test(x = table(data_sen_q[, -c(1,2)]), correct = FALSE)
      Q_MN_result_spe <- stats::mcnemar.test(x = table(data_spe_q[, -c(1,2)]), correct = FALSE)
    } else {
      Q_MN_result_sen <- DescTools::CochranQTest(y = as.matrix(data_sen_q[, -c(1,2)]))
      Q_MN_result_spe <- DescTools::CochranQTest(y = as.matrix(data_spe_q[, -c(1,2)]))
    }
    ##
    Final_result <- result_mat(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
                               Q_MN_result_sen = Q_MN_result_sen, Q_MN_result_spe = Q_MN_result_spe,
                               effect = effect, interaction = interaction)

    Final_result$formula <- as.character(CLR_result_spe$userCall)[2]

  } else if (effect == "Both") {
    if (interaction == TRUE) {
      CLR_result_sen <- survival::clogit(formula = Y ~ Modality * Reader + strata(Case),
                                         data = data_sen_clr)
      CLR_result_spe <- survival::clogit(formula = Y ~ Modality * Reader + strata(Case),
                                         data = data_spe_clr)

      Q_MN_result_sen <- DescTools::CochranQTest(y = as.matrix(data_sen_q[, -c(1)]))
      Q_MN_result_spe <- DescTools::CochranQTest(y = as.matrix(data_spe_q[, -c(1)]))

      ##
      Final_result <- result_mat(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
                                 Q_MN_result_sen = Q_MN_result_sen, Q_MN_result_spe = Q_MN_result_spe,
                                 effect = effect, interaction = interaction)

      Final_result$formula <- as.character(CLR_result_spe$userCall)[2]
    } else {
      CLR_result_sen <- survival::clogit(formula = Y ~ Modality + Reader + strata(Case),
                                         data = data_sen_clr)
      CLR_result_spe <- survival::clogit(formula = Y ~ Modality + Reader + strata(Case),
                                         data = data_spe_clr)

      ##
      Final_result <- result_mat(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
                                 effect = effect, interaction = interaction)

      Final_result$formula <- as.character(CLR_result_spe$userCall)[2]
    }

  } else {
    stop("\n Error: effect must be one of \"Modality\", \"Reader\", or \"Both\".")
  }

  #
  Final_result$n.reader <- length(Readers)
  Final_result$n.modality <- length(Modalities)
  Final_result$effect <- effect
  Final_result$interaction <- interaction
  Final_result$reference.Modality <- reference.Modality
  Final_result$reference.Reader <- reference.Reader

  class(Final_result) <- c("MRMCbinary")
  return(Final_result)
}


