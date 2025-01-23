#' @title Print for \code{MRMCbinary} objects
#'
#' @description Print the results for object of class \code{MRMCbinary}.
#'
#' @param x An object for class \code{MRMCbinary}.
#' @param ... Further arguments (currently not used).
#'
#' @details Print the results for object (\code{MRMCbinary} of class \code{MRMCbinary}.
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
#' ## Example usage of SensSpec function:
#' senspe_result <- SensSpec(data = VanDyke, Reader = reader,
#'                           Modality = treatment, Case = case,
#'                           D = truth, Y = Y, digits = 3)
#' print(senspe_result)
#'
#' @keywords print
#'
#' @seealso
#'  \code{\link[MRMCbinary]{SensSpec}}, \code{\link[base]{print}}
#'
#' @export

print.SensSpec <- function (x, ...){
  n_K <- nrow(x$`Modality-specific Result`)
  n_J <- nrow(x$`Reader-specific Modality-specific Result`)/n_K

  if (!inherits(x, "SensSpec")){
    stop("Argument 'x' must be an object of class \"SensSpec\".")
  }

  ##
  xx1 <- x$`Overall Result`
  xx1_digits_sen <- abs(nchar(xx1$Sensitivity)[1] - 11)
  xx1_digits_spe <- abs(nchar(xx1$Specificity)[1] - 11)
  xx2 <- x$`Modality-specific Result`
  xx2_digits_sen <- abs(nchar(xx2$Sensitivity)[1] - 11)
  xx2_digits_spe <- abs(nchar(xx2$Specificity)[1] - 11)
  xx3 <- x$`Reader-specific Modality-specific Result`
  xx3_digits_sen <- abs(nchar(xx3$Sensitivity)[1] - 11)
  xx3_digits_spe <- abs(nchar(xx3$Specificity)[1] - 11)

  ## Overall
  rownames(xx1) <- ""
  cat("\n")
  cat("Overall Results:")
  cat("\n")
  print(xx1)


  ## Modality-specific sensitivity and specificity (ignoring Reader)
  cat("\n")
  cat("Modality-specific Results: \n")
  x22_senspe <- data.frame(matrix(c(xx2$Sensitivity, "   |   ", xx2$Specificity), ncol=c(n_K+1+n_K)))
  x22_senspe <- rbind(c(paste0("Modality", 1:n_K), "   |   ", paste0("Modality", 1:n_K)), x22_senspe)
  colnames(x22_senspe) <- c(paste0("Sensitivity", paste(rep(" ", xx2_digits_sen), collapse = "")), rep("", n_K - 1), "   |   ",
                            paste0("Specificity", paste(rep(" ", xx2_digits_spe), collapse = "")), rep("", n_K - 1))
  rownames(x22_senspe) <- c("", " ")
  print(x22_senspe)


  ## Reader-specific Modality-specific sensitivity and specificity
  cat("\n")
  cat("Modality-specific Results: \n")

  for (jj in 1:n_J) {
    xx3_temp <- xx3[xx3$Reader == jj, ]

    cat(paste0("For Reader", jj, ":"), "\n")
    xx3_tempsenspe <- data.frame(matrix(c(xx3_temp$Sensitivity, "   |   ", xx3_temp$Specificity), ncol=c(n_K+1+n_K)))
    xx3_tempsenspe <- rbind(c(paste0("Modality", 1:n_K), "   |   ", paste0("Modality", 1:n_K)), xx3_tempsenspe)
    colnames(xx3_tempsenspe) <- c(paste0("Sensitivity", paste(rep(" ", xx3_digits_sen), collapse = "")), rep("", n_K - 1), "   |   ",
                                  paste0("Specificity", paste(rep(" ", xx3_digits_spe), collapse = "")), rep("", n_K - 1))
    rownames(xx3_tempsenspe) <- c("", " ")
    print(xx3_tempsenspe)

  }

  # invisible(x)
  invisible(NULL)
}


