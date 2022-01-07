#' Union of two objects of class \dQuote{umbrella}
#'
#' Combine the factors included in two umbrella objects
#' @param x an object of class \dQuote{umbrella}.
#' @param y an object of class \dQuote{umbrella}.
#' @param ... other arguments that can be passed to the function
#'
#' @details
#' This function allows to combine the results of two objects of class \dQuote{umbrella}.
#' This function is particularly useful when different specifications are used to analyze different factors.
#' It is not possible to union two objects of class \dQuote{umbrella} with different classifications.
#'
#' @return
#' Return an object of class\dQuote{umbrella}, with the factors of the two objects of class \dQuote{umbrella}.
#'
#' @export union.umbrella
#'
#' @md
#'
#' @examples
#' \donttest{
#' ### union raw umbrella objects
#' umb1 <- umbrella(df.SMD, method.var = "REML")
#' umb2 <- umbrella(df.OR, method.var = "PM")
#' umb.union <- union.umbrella(umb1, umb2)
#' summary(add.evidence(umb.union, criteria = "GRADE"))
#'
#' ### union umbrella objects after applying stratification of evidence
#' umb1 <- add.evidence(umbrella(df.SMD), criteria = "GRADE")
#' umb2 <- add.evidence(umbrella(df.OR), criteria = "GRADE")
#' umb3 <- add.evidence(umbrella(df.IRR), criteria = "GRADE")
#' umb.union <- union.umbrella(union.umbrella(umb1, umb2), umb3)
#' summary(umb.union)
#' }
union.umbrella = function(x, y, ...) {
  if (!is.null(attr(x, "criteria")) & !is.null(attr(y, "criteria"))) {
    if (attr(y, "criteria") != attr(x, "criteria")) {
      stop("The umbrellas must have the same evidence criteria")
    }
  }
  for(factor_i in names(y)) {
    if (!is.null(x[[factor_i]])) {
      warning(paste0("Overwriting factor '", factor_i, "' from first umbrella"))
    }
    x[[factor_i]] = y[[factor_i]]
  }
  class(x) = "umbrella"
  return(x)
}
