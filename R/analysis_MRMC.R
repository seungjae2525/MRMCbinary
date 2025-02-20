####
analysis_MRMC_all <- function(all_data, effect, interaction,
                              Modalities, Readers) {
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
    Final_result <- result_mat_all(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
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
    Final_result <- result_mat_all(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
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
      Final_result <- result_mat_all(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
                                     Q_MN_result_sen = Q_MN_result_sen, Q_MN_result_spe = Q_MN_result_spe,
                                     effect = effect, interaction = interaction)

      Final_result$formula <- as.character(CLR_result_spe$userCall)[2]
    } else {
      CLR_result_sen <- survival::clogit(formula = Y ~ Modality + Reader + strata(Case),
                                         data = data_sen_clr)
      CLR_result_spe <- survival::clogit(formula = Y ~ Modality + Reader + strata(Case),
                                         data = data_spe_clr)

      ##
      Final_result <- result_mat_all(CLR_result_sen = CLR_result_sen, CLR_result_spe = CLR_result_spe,
                                     effect = effect, interaction = interaction)

      Final_result$formula <- as.character(CLR_result_spe$userCall)[2]
    }

  } else {
    stop("\n Error: effect must be one of \"Modality\", \"Reader\", or \"Both\".")
  }

  return(Final_result)
}



####
analysis_MRMC_sens_spec <- function(all_data, effect, interaction,
                                    Modalities, Readers) {
  ## Split data
  data_clr <- all_data[[1]]
  data_q <- all_data[[2]]

  ##
  if (effect == "Modality") {
    CLR_result <- survival::clogit(formula = Y ~ Modality + strata(Case, Reader),
                                   data = data_clr)
    if (length(Modalities) == 2) {
      Q_MN_result <- stats::mcnemar.test(x = table(data_q[, -c(1,2)]), correct = FALSE)
    } else {
      Q_MN_result <- DescTools::CochranQTest(y = as.matrix(data_q[, -c(1,2)]))
    }
    ##
    Final_result <- result_mat_sens_spec(CLR_result = CLR_result,
                                         Q_MN_result = Q_MN_result,
                                         effect = effect, interaction = interaction)

    Final_result$formula <- as.character(CLR_result$userCall)[2]

  } else if (effect == "Reader") {
    CLR_result <- survival::clogit(formula = Y ~ Reader + strata(Case, Modality),
                                   data = data_clr)
    if (length(Readers) == 2) {
      Q_MN_result <- stats::mcnemar.test(x = table(data_q[, -c(1,2)]), correct = FALSE)
    } else {
      Q_MN_result <- DescTools::CochranQTest(y = as.matrix(data_q[, -c(1,2)]))
    }
    ##
    Final_result <- result_mat_sens_spec(CLR_result = CLR_result,
                                         Q_MN_result = Q_MN_result,
                                         effect = effect, interaction = interaction)

    Final_result$formula <- as.character(CLR_result$userCall)[2]

  } else if (effect == "Both") {
    if (interaction == TRUE) {
      CLR_result <- survival::clogit(formula = Y ~ Modality * Reader + strata(Case),
                                     data = data_clr)

      Q_MN_result <- DescTools::CochranQTest(y = as.matrix(data_q[, -c(1)]))

      ##
      Final_result <- result_mat_sens_spec(CLR_result = CLR_result,
                                           Q_MN_result = Q_MN_result,
                                           effect = effect, interaction = interaction)

      Final_result$formula <- as.character(CLR_result$userCall)[2]
    } else {
      CLR_result <- survival::clogit(formula = Y ~ Modality + Reader + strata(Case),
                                     data = data_clr)

      ##
      Final_result <- result_mat_sens_spec(CLR_result = CLR_result,
                                           effect = effect, interaction = interaction)

      Final_result$formula <- as.character(CLR_result$userCall)[2]
    }

  } else {
    stop("\n Error: effect must be one of \"Modality\", \"Reader\", or \"Both\".")
  }

  return(Final_result)
}

