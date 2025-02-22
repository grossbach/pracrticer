% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/praktikr.R
\name{days_since_bday}
\alias{days_since_bday}
\title{Days Since Last Birthday.}
\usage{
days_since_bday(dob, today = Sys.Date())
}
\arguments{
\item{dob}{Date of birth (\code{Date} object).}

\item{today}{Today's date (\code{Date} object) (defaults to
\code{Sys.Date()}.}
}
\value{
Days since last birthday (\code{lubridate::interval} object).
}
\description{
Days Since Last Birthday.
}
\examples{
dob <- lubridate::ymd("1966-06-25")
today <- lubridate::ymd("2021-06-26")
days_since_bday(dob, today)
days_since_bday(lubridate::ymd("2011-02-28"), lubridate::ymd("2011-03-01"))
days_since_bday(lubridate::ymd("2012-02-28"), lubridate::ymd("2012-03-01")) # 2012 was a leap year.
days_since_bday(lubridate::ymd("2012-02-28"), lubridate::ymd("2013-02-27"))
}
