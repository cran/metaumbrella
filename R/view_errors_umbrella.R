#' Detect incorrect formatting of a dataset
#'
#' This function checks the formatting of the dataset to ensure it can be passed to the functions of the metaumbrella package.
#'
#' @param data a dataframe
#' @param return the type of information returned by the function. Must be either "messages", "data_and_messages", or "data".
#'
#' @details
#' The functions included in the \pkg{metaumbrella} package require very specific formatting of the dataset (see \code{\link{metaumbrella-package}}).
#'  The \code{view.errors.umbrella()} function checks that a dataframe meets all requirements of the functions of the \pkg{metaumbrella} package.
#'  If this functions finds some formatting issues, several error messages will be produced and the cells / columns in which the errors occurred will be identified.
#'
#' @return Depending on the value passed to the \code{return} argument, different information is returned:
#' \tabular{ll}{
#'  \code{messages} \tab returns global messages describing the different formatting issues.\cr
#'  \tab \cr
#'  \code{data} \tab returns the rows of the original dataset with formatting issues (see below).\cr
#'  \tab \cr
#'  \code{data_and_messages} \tab returns both (i) global messages describing the different formatting issues and\cr
#'  \tab (ii) the rows of the original dataset with formatting issues (see below).\cr
#'}
#'
#'  When returning a dataset (i.e., when "data" or "data_and_messages" are asked in the return argument), the rows with problematic formatting are identified and
#'  the two new columns are added (\code{column_errors} and \code{column_type_errors}), which identify formatting issues.
#'  A 'WARNING' value in the \code{column_errors} column indicates a potential issue that should be checked but that do not prevent calculations.
#'  An 'ERROR' value in the \code{column_errors} column indicates an issue that must be solved before running calculations.
#'
#' @export view.errors.umbrella
#'
#' @md
#'
#' @examples
#' df.errors1 <- df.errors2 <- df.errors3 <- df.errors4 <- df.OR
#'
#' ### include some unknown measures
#' df.errors1$measure[c(1,4,12)] <- "unknown_measure"
#' view.errors.umbrella(df.errors1)
#'
#' ### include some not numeric inputs while expected
#' df.errors2$value[c(2,13,15)] <- c("a", "b", "c")
#' view.errors.umbrella(df.errors2)
#'
#' ### make the lower bound of a confidence interval > to the value
#' df.errors3$ci_lo[c(12,14,21)] <- c(5,6,7)
#' view.errors.umbrella(df.errors3)
#'
#' ### create errors in sample sizes
#' df.errors4$n_cases_exp[c(5,10,15)] <- c(100, 200, 300)
#' view.errors.umbrella(df.errors4)
view.errors.umbrella <- function(data, return = "data_and_messages") {
  if (!return %in% c("message",
                     "messages",
                     "data",
                     "data_and_messages",
                     "data_and_message")) {
    stop("The return value should be either 'messages', 'data' or 'data_and_messages'.")
  }
  checkings <- .check_data(data)

  if (return %in% c("message", "messages")) {
      message(attr(checkings, "message"))
    } else if (return %in% c("data_and_message", "data_and_messages")) {
      df <- attr(checkings, "data")
      df <- df[which(!df$column_errors %in% c(NA, "") | !df$column_type_errors %in% c(NA, "")), ]
      message(attr(checkings, "message"))
      if (nrow(df) > 0) return(df)
    } else if (return == "data") {
      df <- attr(checkings, "data")
      df <- df[which(!df$column_errors %in% c(NA, "") | !df$column_type_errors %in% c(NA, "")), ]
      return(df)
  }
}
