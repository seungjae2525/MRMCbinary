MRMCbinary <- function(...) UseMethod("MRMCbinary")

#' @title Sensitivity range of the difference in adjusted RMST
#'
#' @description \code{MRMCbinary()} is the main function of \code{MRMCbinary} package and
#' performs the analysis the performance of diagnostic tests for binary outcome.
#'
#' @param data A data frame in which contains the reader identifiers (Reader), modality identifiers (Modality), case identifiers (Case), true disease status (D), and binary diagnostic test result (Y).
#' @param Reader Variable of reader identifiers.
#' @param Modality Variable of modality identifiers.
#' @param Case Variable of case identifiers.
#' @param D Variable of true disease status.
#' @param Y Variable of binary diagnostic test result.
#' @param effect Effect one wants to evaluate (one of "Modality", "Reader", and "Both").
#' @param interaction If one want to evaluate the interaction effect between modality and reader, interaction = "TRUE", otherwise "FALSE". Specify only when effect is "both". Default: NULL.
#' @param reference.Modality Reference in Variable of modality identifiers.
#' @param reference.Reader Reference in Variable of reader identifiers.
#'
#' @return An object of class \code{MRMCbinary}. The object is a data.frame with the following components:
#' \item{N}{Total number of subjects}
#' \item{N.exposed}{The number of subjects in exposed group}
#' The results for the \code{MRMCbinary} are printed with the XXXXX function.
#' To generate the plot of results for the \code{MRMCbinary}, use the XXXXX function.
#'
#' @details There are four possible methods for our sensitivity analysis.
#'
#' In general settings,
#'   * methods="Approx": In general survival analysis setting, if the censoring rate is less than 0.7,
#'   the approximate optimization method can be recommended
#'   because it is much faster than and very accurate as the direct optimization method.
#'   * methods="Optim": If the censoring rate is greater than 0.7, the direct optimization method can be used as an alternative
#'   because it is implemented as fast as the approximate optimization method.
#'
#' In special settings, some analytic results can be obtained.
#'   * methods="LP1": When a closed cohort where all subjects are followed up from the same entry time and
#'   only administrative censoring is allowed at the end of follow-up is considered,
#'   high-dimensional optimization problems can be expressed as the well-known linear programming problems,
#'   and thus one can use the analytic solutions for computing the sensitivity range.
#'   * methods="LP2": Similarly, when the minimum value of censoring times in each group is longer than or equal to tau,
#'   the optimization problems are also transformed to well-known linear programming problems,
#'   and thus one can use the analytic solutions for computing the sensitivity range.
#'
#' See Lee et al. (2025) for details.
#'
#' @examples
#' ## Load example data
#' data(VanDyke)
#'
#' ## Return the First Parts of an Object
#' head(VanDyke)
#'
#' ## Create binary test results (Y_ijk)
#' VanDyke$Y <- as.numeric(VanDyke$rating >= 3)
#'
#' @seealso
#'  \code{\link[MRMCbinary]{print.MRMCbinary}}, \code{\link[MRMCbinary]{summary.MRMCbinary}}
#'
#' @references
#' Lee, S., Jang, S., and Lee, W. Evaluating Diagnostic Accuracy of Binary Medical Tests in Multi-reader Multi-case Study. submitted.
#'
#' @keywords methods
#'
#' @export

MRMCbinary <- function(data, Reader, Modality, Case, D, Y, effect,
                       interaction=NULL,
                       reference.Modality, reference.Reader) {
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
  # Is reference.Reader a value contained in the Reader variable?
  if (reference.Reader %notin% Readers) {
    stop("\n Error: Reference of Reader variable must be included in data.")
  }
  # Is reference.Modality a value contained in the Modality variable?
  if (reference.Modality %notin% Modalities) {
    stop("\n Error: Reference of Modality variable must be included in data.")
  }

  ## Warnings
  if (effect %in% c("Modality", "Reader") & !is.null(interaction)) {
    warning("\n Warning: If \"effect\" is \"Modality\" or \"Reader\", then \"interaction\" sholud be NULL.")
    interaction <- NULL
  }
  if (effect == "Both") {
    if (is.null(interaction)) {
      stop("\n Error: If \"effect\" is \"Both\", then \"interaction\" sholud be either TRUE or FALSE.")
    }
  }

  ## Make data for MRMC analysis
  all_data <- make_MRMCdata(data=data, args=args, effect=effect)

  ## Split data
  data_sen_clr <- all_data[[1]]
  data_spe_clr <- all_data[[2]]
  data_sen_q <- all_data[[3]]
  data_spe_q <- all_data[[4]]

  ##
  if (effect == "Modality") {
    CLR_result_sen <- survival::clogit(formula = Y ~ Modality + survival::strata(Case, Reader),
                                       data=data_sen_clr)
    CLR_result_spe <- survival::clogit(formula = Y ~ Modality + survival::strata(Case, Reader),
                                       data=data_spe_clr)
    if (length(Modalities) == 2) {
      Q_MN_result_sen <- stats::mcnemar.test(x=table(data_sen_q[, -c(1,2)]), correct=FALSE)
      Q_MN_result_spe <- stats::mcnemar.test(x=table(data_spe_q[, -c(1,2)]), correct=FALSE)
    } else {
      Q_MN_result_sen <- DescTools::CochranQTest(y=as.matrix(data_sen_q[, -c(1,2)]))
      Q_MN_result_spe <- DescTools::CochranQTest(y=as.matrix(data_spe_q[, -c(1,2)]))
    }
    ##
    Final_result <- result_mat(CLR_result_sen, CLR_result_spe,
                               Q_MN_result_sen, Q_MN_result_spe,
                               effect=effect, interaction=interaction)

    Final_result$formula <- as.character(CLR_result_spe$userCall)[2]

  } else if (effect == "Reader") {
    CLR_result_sen <- survival::clogit(formula = Y ~ Reader + survival::strata(Case, Modality),
                                       data=data_sen_clr)
    CLR_result_spe <- survival::clogit(formula = Y ~ Reader + survival::strata(Case, Modality),
                                       data=data_spe_clr)
    if (length(Readers) == 2) {
      Q_MN_result_sen <- stats::mcnemar.test(x=table(data_sen_q[, -c(1,2)]), correct=FALSE)
      Q_MN_result_spe <- stats::mcnemar.test(x=table(data_spe_q[, -c(1,2)]), correct=FALSE)
    } else {
      Q_MN_result_sen <- DescTools::CochranQTest(y=as.matrix(data_sen_q[, -c(1,2)]))
      Q_MN_result_spe <- DescTools::CochranQTest(y=as.matrix(data_spe_q[, -c(1,2)]))
    }
    ##
    Final_result <- result_mat(CLR_result_sen, CLR_result_spe,
                               Q_MN_result_sen, Q_MN_result_spe,
                               effect=effect, interaction=interaction)

    Final_result$formula <- as.character(CLR_result_spe$userCall)[2]

  } else if (effect == "Both") {
    if (interaction == TRUE) {
      CLR_result_sen <- survival::clogit(formula = Y ~ Modality * Reader + survival::strata(Case),
                                         data=data_sen_clr)
      CLR_result_spe <- survival::clogit(formula = Y ~ Modality * Reader + survival::strata(Case),
                                         data=data_spe_clr)

      Q_MN_result_sen <- DescTools::CochranQTest(y=as.matrix(data_sen_q[, -c(1)]))
      Q_MN_result_spe <- DescTools::CochranQTest(y=as.matrix(data_spe_q[, -c(1)]))

      ##
      Final_result <- result_mat(CLR_result_sen, CLR_result_spe,
                                 Q_MN_result_sen, Q_MN_result_spe,
                                 effect=effect, interaction=interaction)

      Final_result$formula <- as.character(CLR_result_spe$userCall)[2]
    } else {
      CLR_result_sen <- survival::clogit(formula = Y ~ Modality + Reader + survival::strata(Case),
                                         data=data_sen_clr)
      CLR_result_spe <- survival::clogit(formula = Y ~ Modality + Reader + survival::strata(Case),
                                         data=data_spe_clr)

      ##
      Final_result <- result_mat(CLR_result_sen, CLR_result_spe,
                                 Q_MN_result_sen, Q_MN_result_spe,
                                 effect=effect, interaction=interaction)

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


