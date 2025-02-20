####
result_mat_all <- function(CLR_result_sen, CLR_result_spe,
                           Q_MN_result_sen, Q_MN_result_spe, effect, interaction) {

  ##
  summary_CLR_sen <- data.frame(summary(CLR_result_sen)$coefficients)
  CLR_coef_sen <- exp(coef(CLR_result_sen))
  CLR_SE_sen <- summary_CLR_sen[,3]
  CLR_ci_sen <- data.frame(exp(confint(CLR_result_sen)))
  CLR_PV_sen <- summary_CLR_sen[,5]
  CLR_LRT_sen <- summary(CLR_result_sen)$logtest
  CLR_Score_sen <- summary(CLR_result_sen)$sctest
  CLR_Wald_sen <- summary(CLR_result_sen)$waldtest

  mat_CLR_sen <- data.frame(Estimate = CLR_coef_sen, SE = CLR_SE_sen,
                            Lower.ci = CLR_ci_sen[,1], Upper.ci = CLR_ci_sen[,2],
                            P.value = CLR_PV_sen)
  mat_CLR_LRT_sen <- data.frame(Statistic = CLR_LRT_sen[1], DF = CLR_LRT_sen[2],
                                P.value = CLR_LRT_sen[3])
  rownames(mat_CLR_LRT_sen) <- "LRT"
  mat_CLR_Score_sen <- data.frame(Statistic = CLR_Score_sen[1], DF = CLR_Score_sen[2],
                                  P.value = CLR_Score_sen[3])
  rownames(mat_CLR_Score_sen) <- "Score"
  mat_CLR_Wald_sen <- data.frame(Statistic = CLR_Wald_sen[1], DF = CLR_Wald_sen[2],
                                 P.value = CLR_Wald_sen[3])
  rownames(mat_CLR_Wald_sen) <- "Wald"

  ##
  summary_CLR_spe <- data.frame(summary(CLR_result_spe)$coefficients)
  CLR_coef_spe <- exp(coef(CLR_result_spe))
  CLR_SE_spe <- summary_CLR_spe[,3]
  CLR_ci_spe <- data.frame(exp(confint(CLR_result_spe)))
  CLR_PV_spe <- summary_CLR_spe[,5]
  CLR_LRT_spe <- summary(CLR_result_spe)$logtest
  CLR_Score_spe <- summary(CLR_result_spe)$sctest
  CLR_Wald_spe <- summary(CLR_result_spe)$waldtest

  mat_CLR_spe <- data.frame(Estimate = CLR_coef_spe, SE = CLR_SE_spe,
                            Lower.ci = CLR_ci_spe[,1], Upper.ci = CLR_ci_spe[,2],
                            P.value = CLR_PV_spe)
  mat_CLR_LRT_spe <- data.frame(Statistic = CLR_LRT_spe[1], DF = CLR_LRT_spe[2],
                                P.value = CLR_LRT_spe[3])
  rownames(mat_CLR_LRT_spe) <- "LRT"
  mat_CLR_Score_spe <- data.frame(Statistic = CLR_Score_spe[1], DF = CLR_Score_spe[2],
                                  P.value = CLR_Score_spe[3])
  rownames(mat_CLR_Score_spe) <- "Score"
  mat_CLR_Wald_spe <- data.frame(Statistic = CLR_Wald_spe[1], DF = CLR_Wald_spe[2],
                                 P.value = CLR_Wald_spe[3])
  rownames(mat_CLR_Wald_spe) <- "Wald"

  ##
  if (effect == "Both") {
    if (interaction == FALSE) {
      return(list(CLR_sen = mat_CLR_sen, CLR_LRT_sen = mat_CLR_LRT_sen,
                  CLR_Score_sen = mat_CLR_Score_sen, CLR_Wald_sen = mat_CLR_Wald_sen,

                  CLR_spe = mat_CLR_spe, CLR_LRT_spe = mat_CLR_LRT_spe,
                  CLR_Score_spe = mat_CLR_Score_spe, CLR_Wald_spe = mat_CLR_Wald_spe))
    } else {
      ##
      Q_MN_stat_sen <- Q_MN_result_sen$statistic
      Q_MN_DF_sen <- Q_MN_result_sen$parameter
      Q_MN_p_sen <- Q_MN_result_sen$p.value
      mat_Q_MN_sen <- data.frame(Statistic = Q_MN_stat_sen, DF = Q_MN_DF_sen,
                                 P.value = Q_MN_p_sen)

      ##
      Q_MN_stat_spe <- Q_MN_result_spe$statistic
      Q_MN_DF_spe <- Q_MN_result_spe$parameter
      Q_MN_p_spe <- Q_MN_result_spe$p.value
      mat_Q_MN_spe <- data.frame(Statistic = Q_MN_stat_spe, DF = Q_MN_DF_spe,
                                 P.value = Q_MN_p_spe)

      return(list(CLR_sen = mat_CLR_sen, CLR_LRT_sen = mat_CLR_LRT_sen,
                  CLR_Score_sen = mat_CLR_Score_sen, CLR_Wald_sen = mat_CLR_Wald_sen,
                  Q_MN_sen = mat_Q_MN_sen,

                  CLR_spe = mat_CLR_spe, CLR_LRT_spe = mat_CLR_LRT_spe,
                  CLR_Score_spe = mat_CLR_Score_spe, CLR_Wald_spe = mat_CLR_Wald_spe,
                  Q_MN_spe = mat_Q_MN_spe))
    }
  } else {
    ##
    Q_MN_stat_sen <- Q_MN_result_sen$statistic
    Q_MN_DF_sen <- Q_MN_result_sen$parameter
    Q_MN_p_sen <- Q_MN_result_sen$p.value
    mat_Q_MN_sen <- data.frame(Statistic = Q_MN_stat_sen, DF = Q_MN_DF_sen,
                               P.value = Q_MN_p_sen)

    ##
    Q_MN_stat_spe <- Q_MN_result_spe$statistic
    Q_MN_DF_spe <- Q_MN_result_spe$parameter
    Q_MN_p_spe <- Q_MN_result_spe$p.value
    mat_Q_MN_spe <- data.frame(Statistic = Q_MN_stat_spe, DF = Q_MN_DF_spe,
                               P.value = Q_MN_p_spe)

    return(list(CLR_sen = mat_CLR_sen, CLR_LRT_sen = mat_CLR_LRT_sen,
                CLR_Score_sen = mat_CLR_Score_sen, CLR_Wald_sen = mat_CLR_Wald_sen,
                Q_MN_sen = mat_Q_MN_sen,

                CLR_spe = mat_CLR_spe, CLR_LRT_spe = mat_CLR_LRT_spe,
                CLR_Score_spe = mat_CLR_Score_spe, CLR_Wald_spe = mat_CLR_Wald_spe,
                Q_MN_spe = mat_Q_MN_spe))
  }
}



####
result_mat_sens_spec <- function(CLR_result, Q_MN_result, effect, interaction) {

  ##
  summary_CLR <- data.frame(summary(CLR_result)$coefficients)
  CLR_coef <- exp(coef(CLR_result))
  CLR_SE <- summary_CLR[,3]
  CLR_ci <- data.frame(exp(confint(CLR_result)))
  CLR_PV <- summary_CLR[,5]
  CLR_LRT <- summary(CLR_result)$logtest
  CLR_Score <- summary(CLR_result)$sctest
  CLR_Wald <- summary(CLR_result)$waldtest

  mat_CLR <- data.frame(Estimate = CLR_coef, SE = CLR_SE,
                        Lower.ci = CLR_ci[,1], Upper.ci = CLR_ci[,2],
                        P.value = CLR_PV)
  mat_CLR_LRT <- data.frame(Statistic = CLR_LRT[1], DF = CLR_LRT[2],
                            P.value = CLR_LRT[3])
  rownames(mat_CLR_LRT) <- "LRT"
  mat_CLR_Score <- data.frame(Statistic = CLR_Score[1], DF = CLR_Score[2],
                              P.value = CLR_Score[3])
  rownames(mat_CLR_Score) <- "Score"
  mat_CLR_Wald <- data.frame(Statistic = CLR_Wald[1], DF = CLR_Wald[2],
                             P.value = CLR_Wald[3])
  rownames(mat_CLR_Wald) <- "Wald"

  ##
  if (effect == "Both") {
    if (interaction == FALSE) {
      return(list(CLR = mat_CLR, CLR_LRT = mat_CLR_LRT,
                  CLR_Score = mat_CLR_Score, CLR_Wald = mat_CLR_Wald))
    } else {
      ##
      Q_MN_stat <- Q_MN_result$statistic
      Q_MN_DF <- Q_MN_result$parameter
      Q_MN_p <- Q_MN_result$p.value
      mat_Q_MN <- data.frame(Statistic = Q_MN_stat, DF = Q_MN_DF,
                             P.value = Q_MN_p)

      return(list(CLR = mat_CLR, CLR_LRT = mat_CLR_LRT,
                  CLR_Score = mat_CLR_Score, CLR_Wald = mat_CLR_Wald,
                  Q_MN = mat_Q_MN))
    }
  } else {
    ##
    Q_MN_stat <- Q_MN_result$statistic
    Q_MN_DF <- Q_MN_result$parameter
    Q_MN_p <- Q_MN_result$p.value
    mat_Q_MN <- data.frame(Statistic = Q_MN_stat, DF = Q_MN_DF,
                           P.value = Q_MN_p)

    return(list(CLR = mat_CLR, CLR_LRT = mat_CLR_LRT,
                CLR_Score = mat_CLR_Score, CLR_Wald = mat_CLR_Wald,
                Q_MN = mat_Q_MN))
  }
}

