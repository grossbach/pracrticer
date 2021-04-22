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
