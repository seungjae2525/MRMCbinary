####
make_MRMCdata_all <- function(data, args, effect) {
  ##
  dat <- data.frame(Modality = data[[as.character(args$Modality)]],
                    Reader = data[[as.character(args$Reader)]],
                    Case = data[[as.character(args$Case)]],
                    D = data[[as.character(args$D)]],
                    Y = data[[as.character(args$Y)]])
  dat <- dat[order(dat$Case, dat$Reader, dat$Modality, dat$D), ]; rownames(dat) <- NULL

  ## Change variables to factor variables
  dat$Modality <- factor(dat$Modality)
  dat$Reader <- factor(dat$Reader)
  dat$Case <- factor(dat$Case)

  ## Extract unique values for Reader and Modality
  Modalities <- unique(dat$Modality)
  Readers <- unique(dat$Reader)

  dat_sens <- dat[dat$D == 1, ]; dat_sens$D <- NULL
  dat_spec <- dat[dat$D == 0, ]; dat_spec$D <- NULL

  ##
  if (effect == "Modality") {
    ## Long format data for sensitivity
    dat_sens_long <- reshape(data = dat_sens, idvar = c("Case", "Reader"),
                             timevar = "Modality", direction = "wide")
    colnames(dat_sens_long) <- c("Reader", "ID",
                                 paste0("Modality", 1:length(Modalities)))

    ## Long format data for specificity
    dat_spec_long <- reshape(data = dat_spec, idvar = c("Case", "Reader"),
                             timevar = "Modality", direction = "wide")
    colnames(dat_spec_long) <- c("Reader", "ID",
                                 paste0("Modality", 1:length(Modalities)))
  } else if (effect == "Reader") {
    ## Long format data for sensitivity
    dat_sens_long <- reshape(data = dat_sens, idvar = c("Case", "Modality"),
                             timevar = "Reader", direction = "wide")
    colnames(dat_sens_long) <- c("Modality", "ID",
                                 paste0("Reader", 1:length(Readers)))

    ## Long format data for specificity
    dat_spec_long <- reshape(data = dat_spec, idvar = c("Case", "Modality"),
                             timevar = "Reader", direction = "wide")
    colnames(dat_spec_long) <- c("Modality", "ID",
                                 paste0("Reader", 1:length(Readers)))
  } else if (effect == "Both") {
    ## Long format data for sensitivity
    dat_sens_new <- dat_sens
    dat_sens_new$Modality_Reader <- interaction(dat_sens_new$Modality, dat_sens_new$Reader, sep = "_")
    dat_sens_new$Modality <- NULL
    dat_sens_new$Reader <- NULL
    dat_sens_long <- reshape(data = dat_sens_new, idvar = "Case",
                             timevar = "Modality_Reader", direction = "wide")
    colnames(dat_sens_long) <- c("ID",
                                 c(outer(X = paste0("Modality", 1:length(Modalities)),
                                         Y = paste0("Reader", 1:length(Readers)),
                                         FUN = function(x, y) paste0(x, "_", y))))

    ## Long format data for specificity
    dat_spec_new <- dat_spec
    dat_spec_new$Modality_Reader <- interaction(dat_spec_new$Modality, dat_spec_new$Reader, sep = "_")
    dat_spec_new$Modality <- NULL
    dat_spec_new$Reader <- NULL
    dat_spec_long <- reshape(data = dat_spec_new, idvar = "Case",
                             timevar = "Modality_Reader", direction = "wide")
    colnames(dat_spec_long) <- c("ID",
                                 c(outer(X = paste0("Modality", 1:length(Modalities)),
                                         Y = paste0("Reader", 1:length(Readers)),
                                         FUN = function(x, y) paste0(x, "_", y))))
  } else {
    stop("\n Error: effect must be one of \"Modality\", \"Reader\", or \"Both\".")
  }

  return(list(dat_sens, dat_spec, dat_sens_long, dat_spec_long))
}



####
make_MRMCdata_sens_spec <- function(data, args, effect, measure,
                                    n.modality, n.reader) {
  ##
  if (!is.null(args$D)) {
    filter_value <- if (measure == "Sensitivity") 1 else 0

    full_dat <- data.frame(Modality = data[[as.character(args$Modality)]],
                           Reader = data[[as.character(args$Reader)]],
                           Case = data[[as.character(args$Case)]],
                           D = data[[as.character(args$D)]],
                           Y = data[[as.character(args$Y)]])

    total_cases  <- nrow(full_dat) / (n.modality * n.reader)
    target_cases <- sum(full_dat$D == filter_value) / (n.modality * n.reader)

    if (nrow(data) != sum(data[[as.character(args$D)]] == filter_value)) {
      type_text <- ifelse(filter_value == 1, "diseased (i.e., D = 1)",
                          "non-diseased (i.e., D = 0)")

      warning(paste0("The analysis is only conducted on ", target_cases,
                     " cases with the ", type_text, " among ", total_cases, " cases."))
    }

    dat <- subset(full_dat, D == filter_value)
    dat$D <- NULL

  } else {
    dat <- data.frame(Modality = data[[as.character(args$Modality)]],
                      Reader = data[[as.character(args$Reader)]],
                      Case = data[[as.character(args$Case)]],
                      Y = data[[as.character(args$Y)]])
  }
  dat <- dat[order(dat$Case, dat$Reader, dat$Modality), ]; rownames(dat) <- NULL

  ## Change variables to factor variables
  dat$Modality <- factor(dat$Modality)
  dat$Reader <- factor(dat$Reader)
  dat$Case <- factor(dat$Case)

  ## Extract unique values for Reader and Modality
  Modalities <- unique(dat$Modality)
  Readers <- unique(dat$Reader)

  ##
  if (effect == "Modality") {
    ## Long format data
    dat_long <- reshape(data = dat, idvar = c("Case", "Reader"),
                        timevar = "Modality", direction = "wide")
    colnames(dat_long) <- c("Reader", "ID",
                            paste0("Modality", 1:length(Modalities)))
  } else if (effect == "Reader") {
    ## Long format data
    dat_long <- reshape(data = dat, idvar = c("Case", "Modality"),
                        timevar = "Reader", direction = "wide")
    colnames(dat_long) <- c("Modality", "ID",
                            paste0("Reader", 1:length(Readers)))
  } else if (effect == "Both") {
    ## Long format data
    dat_new <- dat
    dat_new$Modality_Reader <- interaction(dat_new$Modality, dat_new$Reader, sep = "_")
    dat_new$Modality <- NULL
    dat_new$Reader <- NULL
    dat_long <- reshape(data = dat_new, idvar = "Case",
                        timevar = "Modality_Reader", direction = "wide")
    colnames(dat_long) <- c("ID",
                            c(outer(X = paste0("Modality", 1:length(Modalities)),
                                    Y = paste0("Reader", 1:length(Readers)),
                                    FUN = function(x, y) paste0(x, "_", y))))
  } else {
    stop("\n Error: effect must be one of \"Modality\", \"Reader\", or \"Both\".")
  }

  return(list(dat, dat_long))
}

