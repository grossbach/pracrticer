#' Convert Column Names to Column Indices.
#'
#' @param x A data.frame with practice hours for each age bracket, one  line per
#'   subject.
#' @param cols Either \code{NULL} (the default), using all columns of x as age
#'   brackets; or a numerical vector indexing the age bracket columns to use; or
#'   a regular expression matching the names of the age bracket columns.
#'
#' @noRd
#'
#' @return A vector with the indices of the column names \code{cols} of
#'   \code{x}.
col_names_to_indices_ <- function(x, cols) {
  stopifnot(is.data.frame(x),
            is.character(cols))
  names_x <- names(x)
  val <- match(cols, names_x)
  if (length(val) == 0) {
    ## Maybe cols is a regexp?
    val <- grep(cols, names_x, perl = TRUE)
    if (length(val) == 0)
      stop("Column names not found in x")
  }
  return(val)
}
#' Column Names Of Age Brackets.
#'
#' @param x A data.frame (also) containing the subjective and retrospectively
#'   provided practice times of each participant.
#' @param cols A numerical vector of column indices of \code{x}.
#'
#' @noRd
#'
#' @return A character vector of column names of \code{x}.
bracket_colnames_ <- function(x, cols) {
  if (is.character(cols)) {
    if (length(cols) == 1) {
      # Either ONE col name was provided...
      col_names <- names(x)[match(cols,
                                  names(x))] #
      # ... or it was a regexp...
      if (is.na(col_names))
        col_names <- grep(cols,
                          names(x),
                          perl = TRUE,
                          value = TRUE)
    } else {
      # ... or column names were passed explicitly...
      col_names <- match(cols, names(x))
    }
  } else if (is.numeric(cols)) {
    col_names <- names(x)[cols]
  }
  if (any(is.na(col_names)))
    stop("No matching column names found.")
  return(col_names)
}
