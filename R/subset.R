#' Create a subset of an object of class \dQuote{umbrella}
#'
#' @param x an object of class \dQuote{umbrella}.
#' @param subset logical expression indicating elements or rows to keep: missing values are taken as \code{FALSE}.
#' @param ... other arguments that can be passed to the function
#'
#' @return
#' Returns an object of class \dQuote{umbrella} with the results of some factors removed.
#'
#' @exportS3Method
#'
#' @export subset.umbrella
#'
#' @md
#'
#' @examples
#' ### perform calculations required for an umbrella review
#' umb <- umbrella(df.OR)
#'
#' ### subset the results to factors "ADHD" & "dyslexia"
#' subset.umb <- subset.umbrella(umb, unique(df.OR$factor) %in% c("ADHD", "dyslexia"))
#'
#' summary(subset.umb)

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
