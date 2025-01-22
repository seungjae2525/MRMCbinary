#' @title Calculate sensitivity and specificity
#'
#' @description \code{SensSpec()} is the function that calculates overall sensitivity and specificity, modality specific sensitivity and specificity, and modality specific and reader specific sensitivity and specificity.
#'
#' @param data A data frame in which contains the reader identifiers (Reader), modality identifiers (Modality), case identifiers (Case), true disease status (D), and binary diagnostic test result (Y).
#' @param Reader Variable of reader identifiers.
#' @param Modality Variable of modality identifiers.
#' @param Case Variable of case identifiers.
#' @param D Variable of true disease status.
#' @param Y Variable of binary diagnostic test result.
#' @param digits Summary digits. Default: max(1L, getOption("digits") - 3L).
#'
#' @return An object of class \code{SensSpec}. The object is a data.frame with the following components:
#' \item{N}{Total number of subjects}
#' \item{N.exposed}{The number of subjects in exposed group}
#' The results for the \code{SensSpec} are printed with the xxxx function.
#' To generate the plot of results for the \code{SensSpec}, use the xxxx function.
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
#'  \code{\link[MRMCbinary]{print.SensSpec}}
#'
#' @keywords methods
#'
#' @export

SensSpec <- function(data, Reader, Modality, Case, D, Y,
                     digits=max(1L, getOption("digits") - 3L)) {
  args <- eval(substitute(alist(Reader = Reader, Modality = Modality, Case = Case, D = D, Y = Y)))

  ## Extract unique values for Reader and Modality
  Readers <- unique(data[[as.character(args$Reader)]])
  Modalities <- unique(data[[as.character(args$Modality)]])

  ##############################################################################
  ## Overall sensitivity and specificity
  subset_sen <- with(data, data[get(args$D) == 1, ])
  subset_sen_temp <- subset_sen[[as.character(args$Y)]] == 1
  overall_sensitivity <- format_f(xx=mean(subset_sen_temp),
                                  numer=sum(subset_sen_temp),
                                  denom=length(subset_sen_temp), digits=digits)

  subset_spe <- with(data, data[get(args$D) == 0, ])
  subset_spe_temp <- subset_spe[[as.character(args$Y)]] == 0
  overall_specitivity <- format_f(xx=mean(subset_spe_temp),
                                  numer=sum(subset_spe_temp),
                                  denom=length(subset_spe_temp), digits=digits)
  overall_result <- data.frame(Sensitivity = overall_sensitivity,
                               Specificity = overall_specitivity)

  ##############################################################################
  ## Calculate Modality-specific sensitivity and specificity (ignoring Reader)
  modality_results <- lapply(Modalities, function(t) {
    subset <- with(data, data[get(args$Modality) == t, ])

    sensitivity <- if (any(subset[[as.character(args$D)]] == 1)) {
      subset_sen_temp <- subset[subset[[as.character(args$D)]] == 1, as.character(args$Y)]
      format_f(xx=mean(subset_sen_temp),
               numer=sum(subset_sen_temp),
               denom=length(subset_sen_temp), digits=digits)
    } else {
      c("NaN (0/0)")
    }

    specificity <- if (any(subset[[as.character(args$D)]] == 0)) {
      subset_spe_temp <- subset[subset[[as.character(args$D)]] == 0, as.character(args$Y)] == 0
      format_f(xx=mean(subset_spe_temp),
               numer=sum(subset_spe_temp),
               denom=length(subset_spe_temp), digits=digits)
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
        format_f(xx=mean(subset_sen_temp),
                 numer=sum(subset_sen_temp),
                 denom=length(subset_sen_temp), digits=digits)
      } else {
        c("NaN (0/0)")
      }

      specificity <- if (any(subset[[as.character(args$D)]] == 0)) {
        subset_spe_temp <- subset[subset[[as.character(args$D)]] == 0, as.character(args$Y)] == 0
        format_f(xx=mean(subset_spe_temp),
                 numer=sum(subset_spe_temp),
                 denom=length(subset_spe_temp), digits=digits)
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
                       digits = digits)

  class(final.result) <- c("SensSpec")
  return(final.result)
}





