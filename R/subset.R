#' Create a subset of an object of class \dQuote{umbrella}
#'
#' @param x an object of class \dQuote{umbrella}.
#' @param subset logical expression indicating elements or rows to keep: missing values are taken as \code{FALSE}.
#' @param ... other arguments that can be passed to the function
#'
#' @return
#' Return an object of class \dQuote{umbrella} with the results of some factors removed.
#'
#' @exportS3Method
#'
#' @export subset.umbrella
#'
#' @md
subset.umbrella = function (x, subset, ...) {
  r <- if (missing(subset)) {
    rep(TRUE, length(x))
  } else {
    subset & !is.na(subset)
  }
  for (i in length(x):1) {
    if (!(i %in% which(r))) {
      x[[i]] = NULL
    }
  }
  return(x)
}
