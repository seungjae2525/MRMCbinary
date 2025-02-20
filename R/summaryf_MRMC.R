####
all_MRMCbinary_ss <- function(object, digits) {
  x <- object

  ## Data Structure
  cat("\n")
  cat("###### 1. Data Structure \n")
  cat(paste0("The total number of modalities   : ", x$n.modality, "\n"))
  cat(paste0("The total number of readers      : ", x$n.reader, "\n"))
  cat(paste0("The total number of cases        : ", x$n.case, "\n"))
  cat(paste0("The number of diseased cases     : ", x$n.diseased, "\n"))
  cat(paste0("The number of non-diseased cases : ", x$n.nondiseased, "\n"))
  cat("\n")

  ## Variable List
  cat("###### 2. Variable List \n")
  if (x$effect == "Both") {
    cat(paste0("Modality                        : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                          : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                            : ", as.character(x$args$Case), "\n"))
    cat(paste0("True disease status (D)         : ", as.character(x$args$D), "\n"))
    cat(paste0("Binary diagnostic test (Y)      : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Modality variable : ", x$reference.Modality, "\n"))
    cat(paste0("Referecen for Reader variable   : ", x$reference.Reader, "\n"))
  } else if (x$effect == "Modality") {
    cat(paste0("Modality                        : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                          : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                            : ", as.character(x$args$Case), "\n"))
    cat(paste0("True disease status (D)         : ", as.character(x$args$D), "\n"))
    cat(paste0("Binary diagnostic test (Y)      : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Modality variable : ", x$reference.Modality, "\n"))
  } else if (x$effect == "Reader") {
    cat(paste0("Modality                      : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                        : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                          : ", as.character(x$args$Case), "\n"))
    cat(paste0("True disease status (D)       : ", as.character(x$args$D), "\n"))
    cat(paste0("Binary diagnostic test (Y)    : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Reader variable : ", x$reference.Reader, "\n"))
  }
  cat("\n")

  ## Analysis Description
  cat("###### 3. Analysis Description \n")
  if (x$effect == "Both") {
    cat(paste0("Effect to compare       : Both (Modality effect and Reader effect) \n"))
    cat(paste0("Use of interaction term : ", x$interaction, "\n"))
  } else {
    cat(paste0("Effect to compare: ", x$effect, " effect \n"))
  }
  cat("\n")

  ## Summary results
  cat("###### 4. Analysis Result \n")
  if (x$effect == "Both") {
    ##########################################################################
    ## Sensitivity with interaction term between modality and reader
    cat("#### 4.1. Sensitivity \n")

    cat("## 4.1.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 1, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_sen <- round(x$CLR_sen, digits = digits)
    temp_CLR_sen_p <- format_p(xx = x$CLR_sen$P.value, digits = digits)
    mat_CLR_sen <- data.frame(format_f(xx = temp_CLR_sen$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_sen$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_sen$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_sen$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_sen_p)
    colnames(mat_CLR_sen) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_sen) <- rownames(temp_CLR_sen)
    base::print(mat_CLR_sen)
    cat("\n")

    cat("## 4.1.2. Test from Conditional Logistic Regression \n")
    temp_test_sen <- rbind(x$CLR_LRT_sen, x$CLR_Score_sen, x$CLR_Wald_sen)
    temp_test_sen_p <- format_p(xx = temp_test_sen$P.value, digits = digits)
    temp_test_sen <- round(temp_test_sen, digits = digits)
    mat_test_sen <- data.frame(format_f(xx = temp_test_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_sen$DF,
                               temp_test_sen_p)
    colnames(mat_test_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_sen) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_sen)
    cat("\n")

    if (x$interaction == TRUE) {
      cat("## 4.1.3. Cochran's Q Test \n")

      temp_Q_MN_sen <- round(x$Q_MN_sen, digits = digits)
      temp_Q_MN_sen_p <- format_p(xx = x$Q_MN_sen$P.value, digits = digits)
      mat_Q_MN_sen <- data.frame(format_f(xx = temp_Q_MN_sen$Statistic,
                                          numer = NULL, denom = NULL,
                                          percentage = FALSE, digits = digits),
                                 temp_Q_MN_sen$DF,
                                 temp_Q_MN_sen_p)
      colnames(mat_Q_MN_sen) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN_sen) <- c("")
      base::print(mat_Q_MN_sen)
      cat("\n")

    }

    ##########################################################################
    ## Specificity with interaction term between modality and reader
    cat("#### 4.2. Specificity \n")

    cat("## 4.2.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 0, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_spe <- round(x$CLR_spe, digits = digits)
    temp_CLR_spe_p <- format_p(xx = x$CLR_spe$P.value, digits = digits)
    mat_CLR_spe <- data.frame(format_f(xx = temp_CLR_spe$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_spe$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_spe$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_spe$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_spe_p)
    colnames(mat_CLR_spe) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_spe) <- rownames(temp_CLR_spe)
    base::print(mat_CLR_spe)
    cat("\n")

    cat("## 4.2.2. Test from Conditional Logistic Regression \n")
    temp_test_spe <- rbind(x$CLR_LRT_spe, x$CLR_Score_spe, x$CLR_Wald_spe)
    temp_test_spe_p <- format_p(xx = temp_test_spe$P.value, digits = digits)
    temp_test_spe <- round(temp_test_spe, digits = digits)
    mat_test_spe <- data.frame(format_f(xx = temp_test_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_spe$DF,
                               temp_test_spe_p)
    colnames(mat_test_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_spe) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_spe)
    cat("\n")

    if (x$interaction == TRUE) {
      cat("## 4.2.3. Cochran's Q Test \n")

      temp_Q_MN_spe <- round(x$Q_MN_spe, digits = digits)
      temp_Q_MN_spe_p <- format_p(xx = x$Q_MN_spe$P.value, digits = digits)
      mat_Q_MN_spe <- data.frame(format_f(xx = temp_Q_MN_spe$Statistic,
                                          numer = NULL, denom = NULL,
                                          percentage = FALSE, digits = digits),
                                 temp_Q_MN_spe$DF,
                                 temp_Q_MN_spe_p)
      colnames(mat_Q_MN_spe) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN_spe) <- c("")
      base::print(mat_Q_MN_spe)
      cat("\n")
    }

  } else {
    ##########################################################################
    ## Sensitivity
    cat("#### 4.1. Sensitivity \n")

    cat("## 4.1.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 1, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_sen <- round(x$CLR_sen, digits = digits)
    temp_CLR_sen_p <- format_p(xx = x$CLR_sen$P.value, digits = digits)
    mat_CLR_sen <- data.frame(format_f(xx = temp_CLR_sen$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_sen$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_sen$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_sen$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_sen_p)
    colnames(mat_CLR_sen) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_sen) <- rownames(temp_CLR_sen)

    base::print(mat_CLR_sen)
    cat("\n")

    cat("## 4.1.2. Test from Conditional Logistic Regression \n")
    temp_test_sen <- rbind(x$CLR_LRT_sen, x$CLR_Score_sen, x$CLR_Wald_sen)
    temp_test_sen_p <- format_p(xx = temp_test_sen$P.value, digits = digits)
    temp_test_sen <- round(temp_test_sen, digits = digits)
    mat_test_sen <- data.frame(format_f(xx = temp_test_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_sen$DF,
                               temp_test_sen_p)
    colnames(mat_test_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_sen) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_sen)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("## 4.1.3. McNemar's Test \n")
      } else {
        cat("## 4.1.3. Cochran's Q Test \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("## 4.1.3. McNemar's Test \n")
      } else {
        cat("## 4.1.3. Cochran's Q Test \n")
      }
    }

    temp_Q_MN_sen <- round(x$Q_MN_sen, digits = digits)
    temp_Q_MN_sen_p <- format_p(xx = x$Q_MN_sen$P.value, digits = digits)
    mat_Q_MN_sen <- data.frame(format_f(xx = temp_Q_MN_sen$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_Q_MN_sen$DF,
                               temp_Q_MN_sen_p)
    colnames(mat_Q_MN_sen) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN_sen) <- c("")
    base::print(mat_Q_MN_sen)
    cat("\n")

    ##########################################################################
    ## Specificity
    cat("#### 4.2. Specificity \n")

    cat("## 4.2.1. Conditional Logistic Regression Result \n")
    ## Report formula
    cat("\nclogit formula: ",
        paste(paste0("survival::clogit(", x$formula, ", \n",
                     "                                 data = data[data$D == 0, ], method=\"exact\")"),
              sep = "\n", collapse = "\n"), "\n\n", sep = "")
    temp_CLR_spe <- round(x$CLR_spe, digits = digits)
    temp_CLR_spe_p <- format_p(xx = x$CLR_spe$P.value, digits = digits)
    mat_CLR_spe <- data.frame(format_f(xx = temp_CLR_spe$Estimate,
                                       numer = NULL, denom = NULL,
                                       percentage = FALSE, digits = digits),
                              # format_f(xx = temp_CLR_spe$SE,
                              #          numer = NULL, denom = NULL,
                              #          percentage = FALSE, digits = digits),
                              paste0("[", format_f(xx = temp_CLR_spe$Lower.ci,
                                                   numer = NULL, denom = NULL,
                                                   percentage = FALSE, digits = digits),
                                     ", ", format_f(xx = temp_CLR_spe$Upper.ci,
                                                    numer = NULL, denom = NULL,
                                                    percentage = FALSE, digits = digits), "]"),
                              temp_CLR_spe_p)
    colnames(mat_CLR_spe) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR_spe) <- rownames(temp_CLR_spe)

    base::print(mat_CLR_spe)
    cat("\n")

    cat("## 4.2.2. Test from Conditional Logistic Regression \n")
    temp_test_spe <- rbind(x$CLR_LRT_spe, x$CLR_Score_spe, x$CLR_Wald_spe)
    temp_test_spe_p <- format_p(xx = temp_test_spe$P.value, digits = digits)
    temp_test_spe <- round(temp_test_spe, digits = digits)
    mat_test_spe <- data.frame(format_f(xx = temp_test_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_test_spe$DF,
                               temp_test_spe_p)
    colnames(mat_test_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test_spe) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test_spe)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("## 4.2.3. McNemar's Test \n")
      } else {
        cat("## 4.2.3. Cochran's Q Test \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("## 4.2.3. McNemar's Test \n")
      } else {
        cat("## 4.2.3. Cochran's Q Test \n")
      }
    }

    temp_Q_MN_spe <- round(x$Q_MN_spe, digits = digits)
    temp_Q_MN_spe_p <- format_p(xx = x$Q_MN_spe$P.value, digits = digits)
    mat_Q_MN_spe <- data.frame(format_f(xx = temp_Q_MN_spe$Statistic,
                                        numer = NULL, denom = NULL,
                                        percentage = FALSE, digits = digits),
                               temp_Q_MN_spe$DF,
                               temp_Q_MN_spe_p)
    colnames(mat_Q_MN_spe) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN_spe) <- c("")
    base::print(mat_Q_MN_spe)
    cat("\n")

  }

  # cat("\n")
  invisible(x)
}



####
sens_spec_MRMCbinary_ss <- function(object, digits) {
  x <- object

  ## Data Structure
  cat("\n")
  if (x$measure == "Sensitivity") {
    cat("###### 1. Data Structure \n")
    cat(paste0("The total number of modalities : ", x$n.modality, "\n"))
    cat(paste0("The total number of readers    : ", x$n.reader, "\n"))
    cat(paste0("The total number of cases      : ", x$n.case, "\n"))
    cat(paste0("The number of diseased cases   : ", x$n.diseased, "\n"))
  } else {
    cat("###### 1. Data Structure \n")
    cat(paste0("The total number of modalities   : ", x$n.modality, "\n"))
    cat(paste0("The total number of readers      : ", x$n.reader, "\n"))
    cat(paste0("The total number of cases        : ", x$n.case, "\n"))
    cat(paste0("The number of non-diseased cases : ", x$n.nondiseased, "\n"))
  }
  cat("\n")

  ## Variable List
  cat("###### 2. Variable List \n")
  if (x$effect == "Both") {
    cat(paste0("Modality                        : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                          : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                            : ", as.character(x$args$Case), "\n"))
    if (!is.null(x$args$D)) {
      cat(paste0("True disease status (D)         : ", as.character(x$args$D), "\n"))
    }
    cat(paste0("Binary diagnostic test (Y)      : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Modality variable : ", x$reference.Modality, "\n"))
    cat(paste0("Referecen for Reader variable   : ", x$reference.Reader, "\n"))
  } else if (x$effect == "Modality") {
    cat(paste0("Modality                        : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                          : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                            : ", as.character(x$args$Case), "\n"))
    if (!is.null(x$args$D)) {
      cat(paste0("True disease status (D)         : ", as.character(x$args$D), "\n"))
    }
    cat(paste0("Binary diagnostic test (Y)      : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Modality variable : ", x$reference.Modality, "\n"))
  } else if (x$effect == "Reader") {
    cat(paste0("Modality                      : ", as.character(x$args$Modality), "\n"))
    cat(paste0("Reader                        : ", as.character(x$args$Reader), "\n"))
    cat(paste0("Case                          : ", as.character(x$args$Case), "\n"))
    if (!is.null(x$args$D)) {
      cat(paste0("True disease status (D)       : ", as.character(x$args$D), "\n"))
    }
    cat(paste0("Binary diagnostic test (Y)    : ", as.character(x$args$Y), "\n"))
    cat(paste0("Reference for Reader variable : ", x$reference.Reader, "\n"))
  }
  cat("\n")

  ## Analysis Description
  cat("###### 3. Analysis Description \n")
  if (x$effect == "Both") {
    cat(paste0("Effect to compare       : Both (Modality effect and Reader effect) \n"))
    cat(paste0("Use of interaction term : ", x$interaction, "\n"))
  } else {
    cat(paste0("Effect to compare: ", x$effect, " effect \n"))
  }
  cat("\n")

  ## Summary results
  cat("###### 4. Analysis Result \n")
  if (x$effect == "Both") {
    ##########################################################################
    ## MRMC model with interaction term between modality and reader
    cat(paste0("#### 4.1. ", x$measure, " \n"))
    cat("## 4.1.1. Conditional Logistic Regression Result \n")
    ## Report formula
    if (!is.null(x$args$D)) {
      cat("\nclogit formula: ",
          paste(paste0("survival::clogit(", x$formula, ", \n",
                       "                                 data = data[data$D == 1, ], method=\"exact\")"),
                sep = "\n", collapse = "\n"), "\n\n", sep = "")
    } else {
      cat("\nclogit formula: ",
          paste(paste0("survival::clogit(", x$formula, ", \n",
                       "                                 data = data, method=\"exact\")"),
                sep = "\n", collapse = "\n"), "\n\n", sep = "")
    }
    temp_CLR <- round(x$CLR, digits = digits)
    temp_CLR_p <- format_p(xx = x$CLR$P.value, digits = digits)
    mat_CLR <- data.frame(format_f(xx = temp_CLR$Estimate,
                                   numer = NULL, denom = NULL,
                                   percentage = FALSE, digits = digits),
                          # format_f(xx = temp_CLR$SE,
                          #          numer = NULL, denom = NULL,
                          #          percentage = FALSE, digits = digits),
                          paste0("[", format_f(xx = temp_CLR$Lower.ci,
                                               numer = NULL, denom = NULL,
                                               percentage = FALSE, digits = digits),
                                 ", ", format_f(xx = temp_CLR$Upper.ci,
                                                numer = NULL, denom = NULL,
                                                percentage = FALSE, digits = digits), "]"),
                          temp_CLR_p)
    colnames(mat_CLR) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR) <- rownames(temp_CLR)
    base::print(mat_CLR)
    cat("\n")

    cat("## 4.1.2. Test from Conditional Logistic Regression \n")
    temp_test <- rbind(x$CLR_LRT, x$CLR_Score, x$CLR_Wald)
    temp_test_p <- format_p(xx = temp_test$P.value, digits = digits)
    temp_test <- round(temp_test, digits = digits)
    mat_test <- data.frame(format_f(xx = temp_test$Statistic,
                                    numer = NULL, denom = NULL,
                                    percentage = FALSE, digits = digits),
                           temp_test$DF,
                           temp_test_p)
    colnames(mat_test) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test)
    cat("\n")

    if (x$interaction == TRUE) {
      cat("## 4.1.3. Cochran's Q Test \n")

      temp_Q_MN <- round(x$Q_MN, digits = digits)
      temp_Q_MN_p <- format_p(xx = x$Q_MN$P.value, digits = digits)
      mat_Q_MN <- data.frame(format_f(xx = temp_Q_MN$Statistic,
                                      numer = NULL, denom = NULL,
                                      percentage = FALSE, digits = digits),
                             temp_Q_MN$DF,
                             temp_Q_MN_p)
      colnames(mat_Q_MN) <- c("Statistic", "Degree of freedom", "P value")
      rownames(mat_Q_MN) <- c("")
      base::print(mat_Q_MN)
      cat("\n")

    }

  } else {
    ##########################################################################
    cat(paste0("#### 4.1. ", x$measure, " \n"))

    cat("## 4.1.1. Conditional Logistic Regression Result \n")
    ## Report formula
    if (!is.null(x$args$D)) {
      cat("\nclogit formula: ",
          paste(paste0("survival::clogit(", x$formula, ", \n",
                       "                                 data = data[data$D == 1, ], method=\"exact\")"),
                sep = "\n", collapse = "\n"), "\n\n", sep = "")
    } else {
      cat("\nclogit formula: ",
          paste(paste0("survival::clogit(", x$formula, ", \n",
                       "                                 data = data, method=\"exact\")"),
                sep = "\n", collapse = "\n"), "\n\n", sep = "")
    }
    temp_CLR <- round(x$CLR, digits = digits)
    temp_CLR_p <- format_p(xx = x$CLR$P.value, digits = digits)
    mat_CLR <- data.frame(format_f(xx = temp_CLR$Estimate,
                                   numer = NULL, denom = NULL,
                                   percentage = FALSE, digits = digits),
                          # format_f(xx = temp_CLR$SE,
                          #          numer = NULL, denom = NULL,
                          #          percentage = FALSE, digits = digits),
                          paste0("[", format_f(xx = temp_CLR$Lower.ci,
                                               numer = NULL, denom = NULL,
                                               percentage = FALSE, digits = digits),
                                 ", ", format_f(xx = temp_CLR$Upper.ci,
                                                numer = NULL, denom = NULL,
                                                percentage = FALSE, digits = digits), "]"),
                          temp_CLR_p)
    colnames(mat_CLR) <- c("Odds ratio", "Confidence interval", "P value") # "Standard error"
    rownames(mat_CLR) <- rownames(temp_CLR)

    base::print(mat_CLR)
    cat("\n")

    cat("## 4.1.2. Test from Conditional Logistic Regression \n")
    temp_test <- rbind(x$CLR_LRT, x$CLR_Score, x$CLR_Wald)
    temp_test_p <- format_p(xx = temp_test$P.value, digits = digits)
    temp_test <- round(temp_test, digits = digits)
    mat_test <- data.frame(format_f(xx = temp_test$Statistic,
                                    numer = NULL, denom = NULL,
                                    percentage = FALSE, digits = digits),
                           temp_test$DF,
                           temp_test_p)
    colnames(mat_test) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_test) <- c("Likelihood ratio test", "Score test", "Wald test")
    base::print(mat_test)
    cat("\n")

    if (x$effect == "Modality") {
      if (x$n.modality == 2) {
        cat("## 4.1.3. McNemar's Test \n")
      } else {
        cat("## 4.1.3. Cochran's Q Test \n")
      }
    } else if (x$effect == "Reader") {
      if (x$n.reader == 2) {
        cat("## 4.1.3. McNemar's Test \n")
      } else {
        cat("## 4.1.3. Cochran's Q Test \n")
      }
    }

    temp_Q_MN <- round(x$Q_MN, digits = digits)
    temp_Q_MN_p <- format_p(xx = x$Q_MN$P.value, digits = digits)
    mat_Q_MN <- data.frame(format_f(xx = temp_Q_MN$Statistic,
                                    numer = NULL, denom = NULL,
                                    percentage = FALSE, digits = digits),
                           temp_Q_MN$DF,
                           temp_Q_MN_p)
    colnames(mat_Q_MN) <- c("Statistic", "Degree of freedom", "P value")
    rownames(mat_Q_MN) <- c("")
    base::print(mat_Q_MN)
    cat("\n")
  }
}

