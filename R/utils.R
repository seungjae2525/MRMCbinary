`%notin%` <- function(x, y) !(x %in% y)

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

format_p <- function(xx, digits) {
  if (length(xx) > 1) {
    return(sapply(xx, format_p, digits = digits))
  }
  format_string <- paste0("%.", digits, "f")

  if (xx < 10^(-digits)) {
    return(paste0("< ", 10^(-digits)))
  } else if (xx >= 10^(-digits) && xx < 1) {
    return(sprintf(format_string, xx))
  } else {
    return(sprintf(format_string, 1))
  }
}



