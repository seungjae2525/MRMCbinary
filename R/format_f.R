format_f <- function(xx, numer=NULL, denom=NULL, digits) {

  if (is.null(numer) & is.null(denom)) {
    re <- sprintf(paste0('%.', digits, 'f'), round(xx, digits))
  } else {
    re <- paste0(sprintf(paste0('%.', digits, 'f'), round(xx, digits)),
                 " (", numer, "/", denom, ")")
  }

  return(re)
}
