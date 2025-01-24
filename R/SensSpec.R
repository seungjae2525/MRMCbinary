#' @title Calculate sensitivity and specificity
#'
#' @description \code{SensSpec()} is the function that calculates overall sensitivity and specificity, modality specific sensitivity and specificity, and modality specific and reader specific sensitivity and specificity.
#'
#' @param data A data frame in which contains the modality identifiers (Modality), reader identifiers (Reader), case identifiers (Case), true disease status (D), and binary diagnostic test result (Y).
#' @param Modality Variable of modality identifiers.
#' @param Reader Variable of reader identifiers.
#' @param Case Variable of case identifiers.
#' @param D Variable of true disease status. It should be set the value to 1 for cases diseased and to 0 for those non-diseased.
#' @param Y Variable of binary diagnostic test result. It should be set the value to 1 for cases diagnosed as positive and to 0 for those diagnosed as negative.
#' @param percentage Whether to report results as decimals or percentage points. Default: FALSE.
#' @param digits The number of significant digits. Default: max(1L, getOption("digits") - 3L).
#'
#' @return An object of class \code{SensSpec}. The object is a data.frame with the following components:
#' \item{Overall Result}{Overall sensitivity and specificity}
#' \item{Modality-specific Result}{Modality-specific sensitivity and specificity}
#' \item{Reader-specific Modality-specific Result}{Reader-specific and modality-specific sensitivity and specificity}
#' \item{digits}{The number of significant digits}
#' The results for the \code{SensSpec} are printed with the \code{\link[MRMCbinary]{print.SensSpec}} function.
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
#' ## Example usage of SensSpec function:
#' # Report results as decimals
#' senspe_result1 <- SensSpec(data = VanDyke, Modality = treatment,
#'                            Reader = reader, Case = case,
#'                            D = truth, Y = Y, percentage = FALSE, digits = 3)
#'
#' # Report results as percentage points
#' senspe_result2 <- SensSpec(data = VanDyke, Modality = treatment,
#'                            Reader = reader, Case = case,
#'                            D = truth, Y = Y, percentage = TRUE, digits = 1)
#'
#' @seealso
#'  \code{\link[MRMCbinary]{print.SensSpec}}
#'
#' @references
#' Yerushalmy, J. (1947). Statistical Problems in Assessing Methods of Medical Diagnosis, with Special Reference to X-Ray Techniques. *Public Health Reports (1896-1970)*, 62(40), 1432–1449.
#'
#' @keywords methods
#'
#' @export

SensSpec <- function(data, Modality, Reader, Case, D, Y,
                     percentage=FALSE,
                     digits = max(1L, getOption("digits") - 3L)) {
  ## List of arguments for SensSpec function
  args <- eval(substitute(alist(Modality = Modality, Reader = Reader, Case = Case, D = D, Y = Y)))

  ## Check if "args" is included in the column name of data!
  `%notin%` <- function(x, y) !(x %in% y)

  if (as.character(args$Modality) %notin% colnames(data)) {
    stop("\n Error: Modality variable must be included in data.")
  }
  if (as.character(args$Reader) %notin% colnames(data)) {
    stop("\n Error: Reader variable must be included in data.")
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

  ## Extract unique values for Modality and Reader
  Modalities <- unique(data[[as.character(args$Modality)]])
  Readers <- unique(data[[as.character(args$Reader)]])

  # Number of modalities >= 2 ?
  if (length(Modalities) < 2) {
    stop("\n Error: The number of modalities must be greater than or equal to 2.")
  }
  # Number of readers >= 2 ?
  if (length(Readers) < 2) {
    stop("\n Error: The number of readers must be greater than or equal to 2.")
  }

  ##############################################################################
  ## Overall sensitivity and specificity
  subset_sen <- with(data, data[get(args$D) == 1, ])
  subset_sen_temp <- subset_sen[[as.character(args$Y)]] == 1
  overall_sensitivity <- format_f(xx = mean(subset_sen_temp),
                                  numer = sum(subset_sen_temp),
                                  denom = length(subset_sen_temp),
                                  percentage = percentage,
                                  digits = digits)

  subset_spe <- with(data, data[get(args$D) == 0, ])
  subset_spe_temp <- subset_spe[[as.character(args$Y)]] == 0
  overall_specitivity <- format_f(xx = mean(subset_spe_temp),
                                  numer = sum(subset_spe_temp),
                                  denom = length(subset_spe_temp),
                                  percentage = percentage,
                                  digits = digits)
  overall_result <- data.frame(Sensitivity = overall_sensitivity,
                               Specificity = overall_specitivity)

  ##############################################################################
  ## Calculate Modality-specific sensitivity and specificity (ignoring Reader)
  modality_results <- lapply(Modalities, function(t) {
    subset <- with(data, data[get(args$Modality) == t, ])

    sensitivity <- if (any(subset[[as.character(args$D)]] == 1)) {
      subset_sen_temp <- subset[subset[[as.character(args$D)]] == 1, as.character(args$Y)]
      format_f(xx = mean(subset_sen_temp),
               numer = sum(subset_sen_temp), denom = length(subset_sen_temp),
               percentage = percentage, digits = digits)
    } else {
      c("NaN (0/0)")
    }

    specificity <- if (any(subset[[as.character(args$D)]] == 0)) {
      subset_spe_temp <- subset[subset[[as.character(args$D)]] == 0, as.character(args$Y)] == 0
      format_f(xx = mean(subset_spe_temp),
               numer = sum(subset_spe_temp), denom = length(subset_spe_temp),
               percentage = percentage, digits = digits)
    } else {
      c("NaN (0/0)")
    }

    return(data.frame(Modality = t, Sensitivity = sensitivity, Specificity = specificity))
  })

  ##############################################################################
  ## Combine Modality-wise results into a single dataframe
  modality_results <- do.call(rbind, modality_results)

  ## Initialize a list to store results
  results <- vector("list", length(Readers) * length(Modalities))
  idx <- 1

  ## Loop through each Reader and Modality combination
  for (r in Readers) {
    for (t in Modalities) {
      # Subset data for the specific Reader and Modality
      subset <- with(data, data[get(args$Reader) == r & get(args$Modality) == t, ])

      # Calculate sensitivity and specificity
      sensitivity <- if (any(subset[[as.character(args$D)]] == 1)) {
        subset_sen_temp <- subset[subset[[as.character(args$D)]] == 1, as.character(args$Y)]
        format_f(xx = mean(subset_sen_temp),
                 numer = sum(subset_sen_temp), denom = length(subset_sen_temp),
                 percentage = percentage, digits = digits)
      } else {
        c("NaN (0/0)")
      }

      specificity <- if (any(subset[[as.character(args$D)]] == 0)) {
        subset_spe_temp <- subset[subset[[as.character(args$D)]] == 0, as.character(args$Y)] == 0
        format_f(xx = mean(subset_spe_temp),
                 numer = sum(subset_spe_temp), denom = length(subset_spe_temp),
                 percentage = percentage, digits = digits)
      } else {
        c("NaN (0/0)")
      }

      # Append the results
      results[[idx]] <- data.frame(Reader = r, Modality = t,
                                   Sensitivity = sensitivity, Specificity = specificity)
      idx <- idx + 1
    }
  }

  ## Combine all Reader-Modality results into a single dataframe
  reader_results <- do.call(rbind, results)

  ##############################################################################
  ## Return the results
  final.result <- list(`Overall Result` = overall_result,
                       `Modality-specific Result` = modality_results,
                       `Reader-specific Modality-specific Result` = reader_results,
                       digits = digits, Readers = Readers, Modalities = Modalities)

  class(final.result) <- c("SensSpec")
  return(final.result)
}





