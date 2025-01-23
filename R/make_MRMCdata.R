make_MRMCdata <- function(data, args, effect) {
  ##
  dat <- data.frame(Reader=data[[as.character(args$Reader)]],
                    Modality=data[[as.character(args$Modality)]],
                    Case=data[[as.character(args$Case)]],
                    D=data[[as.character(args$D)]],
                    Y=data[[as.character(args$Y)]])

  ## Change variables to factor variables
  dat$Reader <- factor(dat$Reader)
  dat$Modality <- factor(dat$Modality)
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





