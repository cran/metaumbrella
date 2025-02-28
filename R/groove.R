#' Overlap in primary studies across reviews
#'
#' This function allows to estimate the primary study overlap across reviews
#'
#' @param x an 'umbrella' object
#' @param ID a character variable indicating whether the overlap should be looked across 'factor' or across 'meta_review'.
#' @param presentation the type of presentation for the overlap. Can be precise percentages ('%') or symbols ('+').
#' @param cut_off a vector of 3 cut-off values used only if the 'presentation' argument is set as '+'.
#' @param enhanced a logical variable indicating whether you want a narrative description of the information stored in the rows/columns returned by this function.
#'
#' @return
#' This function returns a dataframe with 1 row/column per factor or meta-review.
#' As in a correlation matrix, the cell at the intersection of a row and a column contains the desired information (i.e. the overlap between primary studies for the column and row names).
#'
#' @references Pérez-Bracchiglione, J., Meza, N., Bangdiwala, S. I., Niño de Guzmán, E., Urrútia, G., Bonfill, X., & Madrid, E. (2022). Graphical Representation of Overlap for OVErviews: GROOVE tool. Research synthesis methods, 13(3), 381–388. https://doi.org/10.1002/jrsm.1557
#'
#' @md
#'
#' @export overlap.prim
#'
#' @examples
#' df.SMD$author[22:32] <- df.SMD$author[1:11]
#' df.SMD$year[22:32] <- df.SMD$year[1:11]
#' overlap.prim(umbrella(df.SMD),
#'              presentation = "+", cut_off = c(.05,.15,.25))
overlap.prim = function(x, ID = "factor", presentation = "%", cut_off = c(.05,.10,.15), enhanced = TRUE) {
  if (!presentation %in% c("+", "%")) {
    stop("In the 'overlap.prim' function, the 'presentation' argument should be set as '%' or '+'")
  }

  if (!enhanced %in% c(TRUE, FALSE)) {
    stop("In the 'overlap.prim' function, the 'enhanced' argument should be logical (TRUE or FALSE)")
  }

  if (ID == "factor") {
    list_dat = lapply(x, function(x) x$x)
    gr_dat = matrix(nrow = length(list_dat), ncol = length(list_dat))
    colnames(gr_dat) <- rownames(gr_dat) <- names(list_dat)
  } else if (ID == "meta_review") {
    list_dat_raw_transit = do.call(rbind, lapply(x, function(x) x$x[,1:4]))
    id_study = paste(list_dat_raw_transit$meta_review, list_dat_raw_transit$author, list_dat_raw_transit$year)
    list_dat_raw = subset(id_study, !duplicated(id_study))
    list_dat = split(list_dat_raw, f = list_dat_raw$meta_review)
  } else {
    stop("In the 'overlap.prim' function, the 'ID' argument should be set as 'factor' or 'meta_review'")
  }

  n_col = n_row = ifelse(enhanced, length(list_dat) + 1, length(list_dat))
  n_col_m1 = n_row_m1 = ifelse(enhanced, length(list_dat), length(list_dat) - 1)
  gr_dat = matrix(nrow = n_col, ncol = n_col)

  if (enhanced) {
    colnames(gr_dat) <- rownames(gr_dat) <- c(names(list_dat), "information")
  } else {
    colnames(gr_dat) <- rownames(gr_dat) <- names(list_dat)
  }

  for (i in 1:(length(list_dat)-1)) {
    dat1 = list_dat[[i]]
    for (y in (i+1):length(list_dat)) {
      dat2 = list_dat[[y]]
      author_1 = paste(dat1$author, dat1$year)
      author_2 = paste(dat2$author, dat2$year)
      overlap1_2 = round(sum(author_1 %in% author_2) / length(author_1), 3)
      overlap2_1 = round(sum(author_2 %in% author_1) / length(author_2), 3)
      gr_dat[i, y] <- overlap1_2
      gr_dat[y, i] <- overlap2_1
    }
  }

  if (enhanced) {

    gr_dat[1:n_row_m1, n_col] <- paste0(
      "This row shows the % of studies in factor/meta_review '",
      rownames(gr_dat)[1:n_row_m1],
      "' included in other factors/meta_reviews.")

    for (i in 1:n_row_m1) {
      # print(i)
      if (nrow(gr_dat) <= 5) {
        gr_dat[nrow(gr_dat), i] <- paste0(
          "This column shows the % of studies in factors/meta_reviews '",
          paste(rownames(gr_dat)[!rownames(gr_dat) %in% c("information", rownames(gr_dat)[i])], collapse = "', '"),
          "' included in the factor/meta_review '", colnames(gr_dat)[i], "'")
      } else {
        gr_dat[nrow(gr_dat), i] <- paste0(
          "This columns shows the % of studies in factors/meta_reviews '",
          paste(tail(rownames(gr_dat)[!rownames(gr_dat) %in% c("information", rownames(gr_dat)[i])], 4), collapse = "', "),
          "', [...], included in the factor/meta_review '", colnames(gr_dat)[i], "'")
      }
    }

  }
  diag(gr_dat) <- 1
  if (presentation == "+") {
    gr_dat_numeric <- matrix(NA, nrow = nrow(gr_dat), ncol = ncol(gr_dat))

    for (i in 1:nrow(gr_dat)) {
      for (j in 1:ncol(gr_dat)) {
        if (is.numeric(gr_dat[i,j]) ||
            (is.character(gr_dat[i,j]) && !is.na(suppressWarnings(as.numeric(gr_dat[i,j]))))) {
          gr_dat_numeric[i,j] <- as.numeric(gr_dat[i,j])
        }
      }
    }

    gr_dat[!is.na(gr_dat_numeric) & gr_dat_numeric >= cut_off[3]] <- "+++"
    gr_dat[!is.na(gr_dat_numeric) & gr_dat_numeric < cut_off[3] & gr_dat_numeric >= cut_off[2]] <- "++"
    gr_dat[!is.na(gr_dat_numeric) & gr_dat_numeric < cut_off[2] & gr_dat_numeric >= cut_off[1]] <- "+"
    gr_dat[!is.na(gr_dat_numeric) & gr_dat_numeric < cut_off[1]] <- "-"
  }
  gr_dat_dat = as.data.frame(gr_dat)
  return(gr_dat_dat)
}
