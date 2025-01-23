format_f <- function(xx, numer = NULL, denom = NULL, percentage = FALSE, digits) {
  if (digits < 0) {
    stop("\n Error: digits must be greater than or equal to 0.")
  }

  if (is.null(numer) & is.null(denom)) {
    re <- sprintf(paste0('%.', digits, 'f'), round(xx, digits))
  } else {
    if (percentage == TRUE) {
      re <- paste0(sprintf(paste0('%.', digits, 'f'), round(xx*100, digits)),
                   "% (", numer, "/", denom, ")")
    } else {
      re <- paste0(sprintf(paste0('%.', digits, 'f'), round(xx, digits)),
                   " (", numer, "/", denom, ")")
    }
  }

  return(re)
}

