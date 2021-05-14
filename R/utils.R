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
      col_names <- names(x)[match(cols, names(x))]
    }
  } else if (is.numeric(cols)) {
    col_names <- names(x)[cols]
  }
  if (any(is.na(col_names)))
    stop("No matching column names found.")
  return(col_names)
}
#' Last Column With (Non-NA) Content.
#'
#' Returns the column index of the right-most non-NA entry of each row in a
#' data.frame.
#'
#' @param x A data.frame.
#' @param cols Either a numerical vector indexing the age bracket columns, or a
#'   character vector with age bracket column names.
#'
#' @return A vector with age bracket column indices.
#' @noRd
#'
#' @examples
#' d_f <- data.frame(a = c("a", "b", NA), b = c(1, NA, 3))
#' col_lastentry(d_f)
col_lastentry_ = function(x, cols = 1:ncol(x)) {
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
  idx = ifelse(is.na(x[cols]),
               0L,
               col(x))
  lastentries <- apply(idx, 1, max)
  names(lastentries) <- NULL
  lastentries <- lastentries + min(cols) - 1 # add offset
  return(lastentries)
}
#' First Column With (Non-NA) Content.
#'
#' Returns the column index of the left-most non-NA entry of each row in a
#' data.frame.
#'
#' @param x A data.frame.
#' @param cols Either a numerical vector indexing the age bracket columns, or a
#'   character vector with age bracket column names.
#'
#' @return A vector with age bracket column indices.
#' @export
#'
#' @examples
#' d_f <- data.frame(a = c("a", "b", NA), b = c(1, NA, 3))
#' col_firstentry(d_f)
col_firstentry_ = function(x, cols = 1:ncol(x)) {
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
  idx = ifelse(is.na(x[cols]),
               0L,
               col(x))
  firstentries <- apply(idx, 1, min)
  names(firstentries) <- NULL
  firstentries <- firstentries + min(cols) - 1 # add offset
  return(firstentries)
}
#' Alignment Column Index.
#'
#' Return the column indices where each row fulfills a criterion.
#'
#' @param x A data.frame (also) containing the subjective and retrospectively
#'   provided practice times, one row per participant.
#' @param cols Either a numerical vector indexing the bracket columns, or a
#'   character string with the names of age bracket columns. If not provided,
#'   the entire data.frame is assumed to only contain age bracket columns.
#' @param timepoint A character string with the criterion to find in each row.
#'   Currently, only "first_entry" (for the first, or left-most non-NA entry in
#'   a row), and "last_entry" (for the last, or right-most non-NA entry in a
#'   row) are allowed.
#'
#' @noRd
#' @return
#'
#' @examples
#' d_f <- data.frame(`10-12` = c(NA, 1.5, 1.0, NA),
#'                   `13-14` = c(1.5, 3, 0.75, 1),
#'                   `15-16` = c(2.5, 4, 1.5, NA)
#'                   check.names = FALSE)
#' align_col_idx_(d_f, timepoint = "last_entry")
align_col_idx_ <- function(x, cols = 1:ncol(x), timepoint = character(0)) {
  stopifnot(is.data.frame(x),
            (is.character(cols) | is.numeric(cols)),
            is.character(timepoint))
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
  switch(timepoint,
         last_entry = col_lastentry_(x, cols),
         first_entry = col_firstentry_(x, cols))
}
#' Pad \code{data.frame} Rows with \code{NA} Values.
#'
#' Given a \code{data.frame} with rows that are (at least partially) left-
#' and/or right-padded with \code{NA} values (i.e. the number of non-\code{NA}
#' entries differs between rows), and a criterion which returns \code{TRUE} for
#' each row, this function returns \code{x} left- and/or right-padded with
#' \code{NA} values.
#'
#' @param x A data.frame (also) containing the subjective and retrospectively
#'   provided practice times, one row per participant.
#' @param cols A numerical vector of size \code{ncol(x)} indexing those columns
#'   of \code{x} that contain mean daily hours information; or a character
#'   string with the names of those columns containing age bracket averages. If
#'   omitted, all columns are assumed to contained practice time data.
#' @param padd A numerical vector of length \code{nrow(x)} with the number of
#'   \code{NA}s to pad each ro with.
#' @param where Either a character string determining whether to align the data
#'   set with the participants' right-most ("right") or left-most entry
#'   ("left"); or if \code{where} is numeric it is meant to be a column
#'   index, on which the data set gets aligned with.
#'
#' @return A \code{data.frame} padded with \code{NA}s on the left and/or on the
#'   right side.
#'
#' @examples
#' d_f <- data.frame(`10-12` = c(NA, 1.5, 1.0, NA),
#'                   `13-14` = c(1.5, NA, 0.75, 1),
#'                   `15-16` = c(2.5, NA, 1.5, NA),
#'                   check.names = FALSE)
#' temporal_align_(d_f, padd = c(3, 1, 3, 2), where = "right")
temporal_align_ <- function(x, cols = 1:ncol(x), padd = numeric(nrow(x)), where = NULL) {
  ncols <- ncol(x[cols])
  aligned <- matrix(NA,
                    nrow = nrow(x),
                    ncol = ncols)
  for (n in 1:nrow(x)) {
    if (is.character(where)) {
      if (where == "left") {
        aligned[n, 1:padd[n]] <- x[n, 1:padd[n]]
      } else if (where == "right") {
        if (is.na(x[n, ncols])) {
          aligned[n, (ncols - padd[n] + 1):ncols] <- unlist(x[n, cols[n]:padd[n]])
        } else {
          aligned[n, ] <- unlist(x[n, cols])
        }
      } else {
        stop("Wrong where argument.")
      }
    } else if (is.numeric(where)) {
      stop("Not yet implemented.")
    } else {
      stop("Wrong where argument.")
    }
  }
  aligned <- as.data.frame(aligned)
  if (is.character(where)) {
    if (where == "right") {
      names(aligned) <- paste(-(ncols - 1):0)
    } else if (where == "left") {
      names(aligned) <- paste(0:(ncols - 1))
    }
  } else if (is.numeric(where)) {
    stop("Not implemented")
  }
  df <- cbind(x[-cols], aligned)
  return(df)
}
