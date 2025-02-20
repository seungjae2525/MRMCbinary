####
all_MRMCbinary_pp <- function(x) {
  ##
  cat("\nclogit formula:  ",
      paste(paste0("survival::clogit(", x$formula, ", data, method=\"exact\")"),
            sep = "\n", collapse = "\n"), "\n\n", sep = "")

  if (x$effect == "Both") {
    if (x$interaction == TRUE) {

      ############################################################################
      ## Sensitivity with interaction term between modality and reader
      cat("\"Sensitivity\" \n")

      cat("Conditional logistic regression results: \n")
      x$CLR_sen$Estimate <- log(x$CLR_sen$Estimate)
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
      x$CLR_spe$Estimate <- log(x$CLR_spe$Estimate)
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
      x$CLR_sen$Estimate <- log(x$CLR_sen$Estimate)
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
      x$CLR_spe$Estimate <- log(x$CLR_spe$Estimate)
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
    x$CLR_sen$Estimate <- log(x$CLR_sen$Estimate)
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
    x$CLR_spe$Estimate <- log(x$CLR_spe$Estimate)
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
}


####
sens_spec_MRMCbinary_pp <- function(x) {
  ##
  cat("\nclogit formula:  ",
      paste(paste0("survival::clogit(", x$formula, ", data, method=\"exact\")"),
            sep = "\n", collapse = "\n"), "\n\n", sep = "")

  if (x$effect == "Both") {
    if (x$interaction == TRUE) {

      ############################################################################
      ## MRMC with interaction term between modality and reader
      cat(paste0(x$measure, "\n"))

      cat("Conditional logistic regression results: \n")
      x$CLR$Estimate <- log(x$CLR$Estimate)
      base::print(x$CLR[, c(1,2,5)])
      cat("\n")

      cat("Likelihood ratio test from conditional logistic regression: \n")
      base::print(x$CLR_LRT)
      cat("\n")

      cat("Score test from conditional logistic regression: \n")
      base::print(x$CLR_Score)
      cat("\n")

      cat("Wald test from conditional logistic regression: \n")
      base::print(x$CLR_Wald)
      cat("\n")

      cat("Cochran's Q test: \n")
      base::print(x$Q_MN)
      cat("\n\n")

    } else {

      ############################################################################
      cat(paste0(x$measure, "\n"))

      cat("Conditional logistic regression results: \n")
      x$CLR$Estimate <- log(x$CLR$Estimate)
      base::print(x$CLR[, c(1,2,5)])
      cat("\n")

      cat("Likelihood ratio test from conditional logistic regression: \n")
      base::print(x$CLR_LRT)
      cat("\n")

      cat("Score test from conditional logistic regression: \n")
      base::print(x$CLR_Score)
      cat("\n")

      cat("Wald test from conditional logistic regression: \n")
      base::print(x$CLR_Wald)
      cat("\n\n")

    }

  } else {
    ############################################################################
    cat(paste0(x$measure, "\n"))

    cat("Conditional logistic regression results: \n")
    x$CLR$Estimate <- log(x$CLR$Estimate)
    base::print(x$CLR[, c(1,2,5)])
    cat("\n")

    cat("Likelihood ratio test from conditional logistic regression: \n")
    base::print(x$CLR_LRT)
    cat("\n")

    cat("Score test from conditional logistic regression: \n")
    base::print(x$CLR_Score)
    cat("\n")

    cat("Wald test from conditional logistic regression: \n")
    base::print(x$CLR_Wald)
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
    base::print(x$Q_MN)
  }
}

