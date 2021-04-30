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
#' @param df A data.frame (also) containing the subjective and retrospectively
#'   provided practice times of each participant.
#' @param bracket_idx A numerical vector indexing the bracket columns. If not
#'   provided, the entire data.frame is assumed to only contain age bracket
#'   columns.
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
bracket_multipliers <- function(df, bracket_idx = 1:ncol(df), sep = "-") {
  nrow_df <- nrow(df)
  ddf <- as.data.frame(matrix(NA,
                              nrow = nrow_df,
                              ncol = length(bracket_idx)))
  names_ddf <- names(df)[bracket_idx]
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
#'
#' @return A vector with column names.
#' @export
#'
#' @examples
#' d_f <- data.frame(a = c("a", "b", NA), b = c(1, NA, 3))
#' col_lastentry(d_f)
col_lastentry = function(x) {
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
#' @param bracket_idx Optional numerical vector of size \code{ncol(x)} indexing
#'   those columns of \code{x} that contain mean daily hours information. Not
#'   necessary if \code{x} only has columns with daily age bracket means.
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
bracket_hours <- function(x, bracket_idx = 1:ncol(x), bracket_mult = NULL, append = TRUE) {
  bracket_hours <- x[ , bracket_idx] * bracket_mult * 365.25
  names(bracket_hours) <- paste("Hours",
                                names(x[bracket_idx]),
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
#' d_f <- data.frame(`10-12` = c(1.5, 1.0, 0.75), `13-14` = c(3, 0.75, 1.0), check.names = FALSE)
#' names_to_durations(names(d_f), sep = "-")
names_to_durations <- function(col_names = NULL, sep = NULL) {
  n_names <- length(col_names)
  duration <- vector(mode = "integer", length = n_names)
  for (n in 1:length(col_names)) {
    split_name <- strsplit(col_names[n],
                           split = sep)
    lb <- strtoi(split_name[[1]][1])
    ub <- strtoi(split_name[[1]][2])
    duration[n] <- length(lb:ub)
  }
  return(duration)
}
