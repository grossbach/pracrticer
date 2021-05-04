#' Days Lived Since Date-of-birth.
#'
#' Calculate age in days from date of birth and, e.g. today's date, to day
#' precision.
#'
#' @param dob Date of birth.
#' @param dte If not provided, the value returned by \code{Sys.Date()}. The date
#'   between which and \code{dob} the age shall be calculated.
#'
#' @return The age as \code{lubridate} period.
#' @export
#' @author Garret Grolemund, https://stackoverflow.com/users/436363/garrett
#'   https://stackoverflow.com/questions/3611314/calculate-ages-in-r
#' @examples
#' days_lived(lubridate::ymd("1995-01-28"), lubridate::ymd("2021-01-27")) # "25y 11m 30d 0H 0M 0S"
#' days_lived(as.Date("2020-02-28"), as.Date("2021-02-27")) # "11m 30d 0H 0M 0S"
days_lived <- function(dob, dte = Sys.Date()) {
  interv <- lubridate::interval(dob,
                                dte)
  age <- lubridate::as.period(interv,
                              unit = "years")
  return(age)
}
#' Days Since Last Birthday.
#'
#' @param dob Date of birth (\code{Date} object).
#' @param today Today's date (\code{Date} object).
#'
#' @return Days since last birthday (\code{lubridate::interval} object).
#' @export
#'
#' @examples
#' dob <- lubridate::ymd("1966-06-25")
#' today <- lubridate::ymd("2021-06-26")
#' days_since_bday(dob, today)
#' days_since_bday(lubridate::ymd("2011-02-28"), lubridate::ymd("2011-03-01"))
#' days_since_bday(lubridate::ymd("2012-02-28"), lubridate::ymd("2012-03-01")) # 2012 was a leap year.
#' days_since_bday(lubridate::ymd("2012-02-28"), lubridate::ymd("2013-02-27"))
days_since_bday <- function(dob, today = Sys.Date()) {
  ## to do:
  ## Test dob and today for valid format (proper class &c.)
  current_year <- lubridate::year(today)
  month_dob <- lubridate::month(dob)
  day_dob <- lubridate::day(dob)
  bday_current_year <- lubridate::ymd(paste(current_year,
                                            month_dob,
                                            day_dob,
                                            sep = "-"))
  date_diff <- lubridate::as.period(lubridate::interval(bday_current_year,
                                                        today),
                                    unit = "days")
  pos_idx <- date_diff >= 0
  neg_idx <- date_diff < 0
  days <- vector(mode = "integer",
                 length = length(dob)) # There must be a more elegant
  days <- lubridate::as.period(days)   # way of doing this!?
  days[neg_idx] <- lubridate::as.period(lubridate::interval(lubridate::ymd(paste(current_year - 1,
                                                                                 month_dob,
                                                                                 day_dob,
                                                                                 sep = "-")),
                                                            today),
                                        unit = "days")[neg_idx]
  days[pos_idx] <- date_diff[pos_idx]
  return(days)
}
#' Age Bracket Multiplier.
#'
#' @param x A data.frame (also) containing the subjective and retrospectively
#'   provided practice times of each participant.
#' @param cols Either a numerical vector indexing the bracket columns, or a
#'   character string with the names of age bracket columns. If not provided,
#'   the entire data.frame is assumed to only contain age bracket columns.
#' @param sep A character string used to separate the beginning and ending ages
#'   in age bracket column names.
#'
#' @return A numerical vector the same length as \code{nrow(df)}, providing the
#'   indices of the right-most non-NA entry in each row of \code{df}.
#'
#' @export
#'
#' @examples
#' d_f <- data.frame(`10-12` = c(1.5, 1.0, 0.75), `13-14` = c(3, 0.75, 1.0), check.names = FALSE)
#' bracket_multipliers(d_f)
bracket_multipliers <- function(x, cols = 1:ncol(x), sep = "-") {
  if (is.character(cols))
    cols <- col_names_to_indices(x, cols)
  nrow_df <- nrow(x)
  ddf <- as.data.frame(matrix(NA,
                              nrow = nrow_df,
                              ncol = length(cols)))
  names_ddf <- names(x)[cols]
  durations <- names_to_durations(names_ddf,
                                  sep = sep)
  ddf <- as.data.frame(matrix(durations,
                              nrow = nrow_df,
                              ncol = ncol(ddf),
                              byrow = TRUE))
  names(ddf) <- paste("multiplier",
                      names_ddf,
                      sep = "_")
  return(ddf)
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
#' @export
#'
#' @examples
#' d_f <- data.frame(a = c("a", "b", NA), b = c(1, NA, 3))
#' col_lastentry(d_f)
col_lastentry = function(x, cols = 1:ncol(x)) {
  if (is.character(cols)) {
    cols <- col_names_to_indices(x, col)
  }
  idx = ifelse(is.na(x),
               0L,
               col(x))
  lastentries <- apply(idx, 1, max)
  names(lastentries) <- NULL
  return(lastentries)
}
#' Bracket Hours.
#'
#' Calculates the hours subjects have been practice during age brackets.
#'
#' @param x A data.frame (also) containing the subjective and retrospectively
#'   provided practice times of each participant.
#' @param cols Either NULL, then all columns are assumed to contain only age
#'   bracket averages; or a numerical vector of size \code{ncol(x)} indexing
#'   those columns of \code{x} that contain mean daily hours information; or a
#'   character string with the names of those columns containing age bracket
#'   averages.
#' @param bracket_mult A numerical vector giving the durations of each age
#'   bracket.
#' @param append A logic value determining whether the function is to return the
#'   yearly hours appended to \code{x} (TRUE; default), or just the yearly
#'   hours.
#'
#' @return If \code{append = TRUE} a data.frame of size \code{nrow(x) x (ncol(x)
#'   + length(bracket_idx))}. If \code{append = FALSE} a data.frame of size
#'   \code{nrow(x) x length(bracket_idx)}.
#' @export
#'
#' @examples
#' d_f <- data.frame(ID = c(1, 2), `10-12` = c(1.5, 1.0), `13-14` = c(3, 0.75), check.names = FALSE)
#' index <- c(2, 3)
#' bracket_hours(d_f, index, c(3, 2))
bracket_hours <- function(x, cols = 1:ncol(x), bracket_mult = NULL, append = TRUE) {
  if (is.character(cols))
      cols <- col_names_to_indices(x, cols)
  bracket_hours <- x[ , cols] * bracket_mult * 365.25
  names(bracket_hours) <- paste("Hours",
                                names(x[cols]),
                                sep = "_")
  if (append) {
    cbind(x,
          bracket_hours)
  } else {
    bracket_hours
  }
}
#' Convert Column Names to Age Bracket Durations.
#'
#' @param col_names A character vector of age brackets.
#' @param sep A character used to separate the lower and upper bound of the
#'   bracket ranges.
#'
#' @return A numerical vector with age bracket durations (usually in years).
#' @export
#'
#' @examples
#' d_f <- data.frame(Names = c("Carl", "Blue", "Perkins"),
#'                   `10-12` = c(1.5, 1.0, 0.75),
#'                   `13-14` = c(3, 0.75, 1.0),
#'                   check.names = FALSE)
#' names_to_durations(d_f, "^\\d{2}-\\d{2}", sep = "-")
#' names_to_durations(d_f, names(d_f)[2:3])
#' names_to_durations(d_f, 2:3)
names_to_durations <- function(x, cols = 1:ncol(x), sep = "-") {
  if (is.character(cols)) {
    ## Does it contain col names or a regexp?
    col_names <- names(x)[match(cols, names(x))]
    if (all(is.na(col_names))) {
      ## it's a regexp!
      if (length(cols) == 1)
        col_names <- grep(cols, names(x), value = TRUE)
      if (length(col_names) == 0)
        stop("No matches of cols in names(x)")
    } else if (any(is.na(col_names)))
      stop("Not all column names provide are existing.")
  } else if (is.numeric(cols)) {
    col_names <- names(x)[cols]
  }
  if (any(is.na(col_names)))
    stop("Some column names were NA.")
  n_cols <- length(col_names)
  duration <- vector(mode = "integer", length = n_cols)
  for (n in 1:length(col_names)) {
    split_name <- strsplit(col_names[n],
                           split = sep)
    lb <- strtoi(split_name[[1]][1])
    ub <- strtoi(split_name[[1]][2])
    duration[n] <- length(lb:ub)
  }
  return(duration)
}
#' Plot Deliberate Practice Hours Over Age Brackets.
#'
#' @param x A data.frame with practice hours for each age bracket, one  line per
#'   subject.
#' @param cols Either \code{NULL} (the default), using all columns of x as age
#'   brackets; or a numerical vector indexing the age bracket columns to use; or
#'   a regular expression matching the names of the age bracket columns.
#' @param ID A vector of size \code{nrow(x)} with unique subject IDs.
#' @param Group A grouping vector of size \code{nrow(x)}.
#' @param legend Logical. When \code{FALSE} (default) or \code{"none"}, no
#'   legend is shown. All possible values for
#'   \code{ggplot2::theme(legend.position)} are allowed.
#'
#' @export
#'
#' @examples
#'
plot_it <- function(x, cols = 1:ncol(x), ID = "ID", Group = NULL, legend = FALSE) {
  if (is.character(cols)) {
    cols <- col_names_to_indices(x, cols)
  }
  x_lng <- tidyr::pivot_longer(x,
                               cols = cols,
                               names_to = "AgeBracket",
                               values_to = "PracticeHours")
  x_lng <- dplyr::mutate(x_lng,
                         AgeBracket = factor(AgeBracket,
                                             ordered = TRUE,
                                             levels = unique(AgeBracket),
                                             labels = unique(AgeBracket)))
  lg <- if(is.logical(legend)) {
    ifelse(!isTRUE(legend),
           "none",
           stop())
  } else if(is.character(legend)) {
    lg <- legend
  }
  ggplot2::ggplot(x_lng, ggplot2::aes(AgeBracket, PracticeHours,
                                      color = ID, group = ID)) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::theme(legend.position = lg)
}
spread_brackets <- function(x, cols = 1:ncol(x)) {
  if (is.character(cols)) {
    if (length(cols) == 1) {
      col_names <- match(cols, names(x)) # ONE col name was provided
      if (is.na(col_names)) # was is a regexp?
        col_names <- grep(cols, names(x), perl = TRUE, value = TRUE)
      if (any(is.na(col_names)))
        stop("No matching column names found.")
    }
    # col_idx <- col_names_to_indices(x, cols) # do we need this?
  } else if (is.numeric(cols)) {
    col_names <- names(x)[cols]
  }
  bracket_durations <- names_to_durations() # do we need this?
}
#' Convert Column Names to Column Indices.
#'
#' @param x A data.frame with practice hours for each age bracket, one  line per
#'   subject.
#' @param cols Either \code{NULL} (the default), using all columns of x as age
#'   brackets; or a numerical vector indexing the age bracket columns to use; or
#'   a regular expression matching the names of the age bracket columns.
#'
#' @return A vector with indices.
#' @export
#'
#' @examples
col_names_to_indices <- function(x, cols) {
  stopifnot(is.data.frame(x),
            is.character(cols))
  names_x <- names(x)
  val <- match(cols, names_x)
  if (length(val == 0)) {
    ## Maybe cols is a regexp?
    val <- grep(cols, names_x, perl = TRUE)
    if (length(val) == 0)
      stop("Column names not found in x")
  }
  return(val)
}
