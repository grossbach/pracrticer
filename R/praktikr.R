#' Days Lived Since Date Of Birth.
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
  # interv <- lubridate::interval(!!rlang::enquo(dob),
  #                               dte)
  interv <- lubridate::interval(dob,
                                dte)
  age <- lubridate::as.period(interv,
                              unit = "days")
  return(age)
}
#' Days Since Last Birthday.
#'
#' @param dob Date of birth (\code{Date} object).
#' @param today Today's date (\code{Date} object) (defaults to
#'   \code{Sys.Date()}.
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
  month_dob <- lubridate::month(dob) # see ?`!!`
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
  days <- lubridate::as.period(days)            # way of doing this!?
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
#' d_f <- data.frame(`10-12` = c(1.5, 1.0, 0.75),
#'                   `13-14` = c(3, 0.75, 1.0),
#'                   check.names = FALSE)
#' bracket_multipliers(d_f)
bracket_multipliers <- function(x, cols = 1:ncol(x), sep = "-") {
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
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
#' Bracket Hours.
#'
#' Calculates the hours subjects have been practicing during age brackets.
#'
#' @param x A data.frame (also) containing the subjective and retrospectively
#'   provided practice times, one row per participant.
#' @param cols A numerical vector of size \code{ncol(x)} indexing those columns
#'   of \code{x} that contain mean daily hours information; or a character
#'   string with the names of those columns containing age bracket averages. If
#'   omitted, all columns are assumed to contained practice time data.
#' @param multiplier A numerical vector giving the duration of each age
#'   bracket.
#' @param append A logic value determining whether the function is to return the
#'   yearly hours appended to \code{x}, or just the yearly
#'   hours (FALSE; default).
#'
#' @return If \code{append = TRUE} a data.frame of size \code{nrow(x) x (ncol(x)
#'   + length(bracket_idx))}. If \code{append = FALSE} a data.frame of size
#'   \code{nrow(x) x length(bracket_idx)}.
#' @export
#'
#' @examples
#' d_f <- data.frame(ID = c(1, 2),
#'                   `10-12` = c(1.5, 1.0),
#'                   `13-14` = c(3, 0.75),
#'                   check.names = FALSE)
#' bracket_duration <- c(3, 2)
#' bracket_hours(d_f, "\\d{2}-\\d{2}", bracket_duration)
bracket_hours <- function(x, cols = 1:ncol(x), multiplier = NULL, append = FALSE) {
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
  bracket_hours <- x[ , cols] * multiplier * 365.25
  names(bracket_hours) <- names(x[cols])
  if (append) {
    cbind(x,
          bracket_hours)
  } else {
    cbind(x[-cols],
          bracket_hours)
  }
}
#' Convert Column Names to Age Bracket Durations.
#'
#' @param col_names A character vector with the names of those columns of a
#'   data.frame containing age bracket averages.
#' @param sep A character used to separate the lower and the upper bound of the
#'   bracket ranges.
#'
#' @return A numerical vector with age bracket durations (usually in years).
#' @export
#'
#' @examples
#' var_names <- c("10-12", "13-14")
#' names_to_durations(var_names, sep = "-")
names_to_durations <- function(col_names, sep = "-") {
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
#' @param x A data.frame with practice hours for each age bracket, one line per
#'   subject (wide format).
#' @param cols Either a numerical vector indexing the age bracket columns to
#'   use, or a regular expression matching the names of the age bracket columns,
#'   or a character vector with the bracket column names. If not provided, all
#'   columns of \code{x} are treated as containing age bracket information.
#' @param ID A vector of size \code{nrow(x)} with unique subject IDs.
#' @param Group A grouping vector of size \code{nrow(x)}.
#' @param legend Logical. When \code{FALSE} (default) or \code{"none"}, no
#'   legend is shown. All possible values for
#'   \code{ggplot2::theme(legend.position)} are allowed.
#'
#' @export
#'
#' @examples
#' d_f <- data.frame(ID = LETTERS[1:3],
#'                   `10-12` = c(1.5, 1.0, 0.75),
#'                   `13-14` = c(3, 0.75, 1.0),
#'                   check.names = FALSE)
#' ## Equivalent calls:
#' plot_it(d_f, "\\d{2}-\\d{2}")
#' plot_it(d_f, c("10-12", "13-14"))
#' plot_it(d_f, c(2, 3))
#' plot_it(d_f[ , 2:3])

plot_it <- function(x, cols = 1:ncol(x), ID = "ID", Group = NULL, legend = FALSE) {
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
  x_lng <- tidyr::pivot_longer(x,
                               cols = cols,
                               names_to = "Age",
                               values_to = "PracticeHours")
  x_lng <- dplyr::mutate(x_lng,
                         Age = factor(Age,
                                      ordered = TRUE,
                                      levels = unique(Age),
                                      labels = unique(Age)))
  ## show legend?
  lg <- if(is.logical(legend)) {
    lg <- ifelse(isTRUE(legend),
                 "right",
                 "none")
  } else if(is.character(legend)) {
    lg <- legend
  }
  ## how many x axis labels?
  n_levels <- nlevels(x_lng$Age)
  if (n_levels <= 10) {
    breaks <- levels(x_lng$Age)
  } else if (n_levels > 10 & n_levels <= 75) {
    breaks <- paste(round(1:n_levels / 5) * 5)
  } else if (n_levels > 75) {
    breaks <- paste(round(1:n_levels / 10) * 10)
  }
  ggplot2::ggplot(x_lng, ggplot2::aes(Age, PracticeHours,
                                      color = ID,
                                      group = ID)) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::theme(legend.position = lg) +
    ggplot2::scale_x_discrete(breaks = breaks)
}
#' Spread Brackets.
#'
#' @param x A data.frame with practice hours for each age bracket, one  line per
#'   subject.
#' @param cols Either a numerical vector indexing the age bracket columns to
#'   use, or a regular expression matching the names of the age bracket columns,
#'   or a character vector with the bracket column names. If not provided, all
#'   columns of \code{x} are treated as containing age bracket information.
#' @param prefix A character to be prefixed to new column names.
#' @param postfix A character to be appended to new column names.
#'
#' @return The \code{data.frame x}, but with the columns of each age bracket
#'   repeated as often as the duration of the corresponding age bracket. The
#'   original age bracket columns are being removed.
#' @export
#'
#' @examples
#' d_f <- data.frame(`10-12` = c(1.5, 1.0, 0.75),
#'                   `13-14` = c(3, 0.75, 1.0),
#'                   check.names = FALSE)
#' spread_brackets(d_f, "\\d{1,2}-\\d{1,2}")
#' spread_brackets(d_f, prefix = "age")
#' spread_brackets(d_f, postfix = "yrs")
#'
spread_brackets <- function(x, cols = 1:ncol(x), prefix = "", postfix = "") {
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x, cols)
    cols <- col_names_to_indices_(x, bracket_colnames)
  }
  df <- x
  bracket_colnames <- bracket_colnames_(x, cols)
  brackets <- x[bracket_colnames]
  df2 <- data.frame(dump = rep(NA, nrow(df)))
  for (col in bracket_colnames) {
    bounds <- strsplit(names(brackets[col]), "-")
    lb <- as.numeric(bounds[[1]][1])
    ub <- as.numeric(bounds[[1]][2])
    new_cols <- paste0(prefix, lb:ub, postfix)
    col_data <- dplyr::pull(brackets, col)
    attr(col_data, "names") <- NULL
    df2 <- data.frame(matrix(col_data,
                             nrow = length(col_data),
                             ncol = length(new_cols)))
    if (any(grepl("dump", names(df2)))) {
      df2$dump <- NULL
    }
    names(df2) <- new_cols
    df[col] <- NULL
    df <- cbind(df, df2)
  }
  return(df)
}
#' Align Data Set to Event.
#'
#' Given a \code{data.frame} with rows that are (at least partially) left-
#' and/or right-padded with \code{NA} values (i.e. the number of non-\code{NA}
#' entries differs between rows), this function aligns the data set according to
#' \code{align}.
#'
#' @param x A data.frame (also) containing subjective and retrospectively
#'   provided practice times, one row per participant.
#' @param cols A numerical vector of size \code{ncol(x)} indexing those columns
#'   of \code{x} that contain mean daily hours information; or a character
#'   string with the names of those columns containing age bracket averages. If
#'   omitted, all columns are assumed to contained practice time data.
#' @param align Either a character string determining whether to align the data
#'   set with each participants' first (\code{align = "first_entry"}) or last
#'   entry (\code{align = "last_entry"}), or a numeric vector indexing each
#'   participant's column with which to align the data set.
#'
#' @return
#' @export
#'
#' @examples
#' d_f <- data.frame(`10-12` = c(NA, 1.5, 1.0, NA),
#'                   `13-14` = c(1.5, 3, 0.75, 1),
#'                   `15-16` = c(2.5, 4, 1.5, NA)
#'                   check.names = FALSE)
#' align_to(d_f, align = "last_entry")
#' align_to(d_f, align = "first_entry")
#' align_to(d_f, align = c(2, 2, 1))
align_to <- function(x, cols = 1:ncol(x), align = NULL) {
  stopifnot(is.data.frame(x),
            (is.character(cols) | is.numeric(cols)),
            is.character(align))
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x,
                                          cols)
    cols <- col_names_to_indices_(x,
                                  bracket_colnames)
  }
  aligncol_idx <- align_col_idx_(x,
                                 cols = cols,
                                 timepoint = align)
  n_cols <- max(aligncol_idx)
  if (is.character(align)) {
    if (align == "last_entry" | align == "first_entry") {
      diffs <- n_cols - aligncol_idx
      if (align == "first_entry") {
        w <- "left"
      } else if (align == "last_entry") {
        w <- "right"
      }
    } else {
      stop("Wrong align value.")
    }
  } else if (is.numeric(align)) {
    stop("Not implemented.")
  }
  aligned <- temporal_align_(x,
                             cols = cols,
                             padd = diffs,
                             where = w)
}
#' Exact, Leap Year-aware Duration.
#'
#'
#'
#' @param x A data.frame (also) containing subjective and retrospectively
#'   provided practice times, one row per participant.
#' @param cols A numerical vector of size \code{ncol(x)} indexing those columns
#'   of \code{x} that contain mean daily hours information; or a character
#'   string with the names of those columns containing age bracket averages. If
#'   omitted, all columns are assumed to contained practice time data.
#' @param align Either a character string determining whether to align the data
#'   set with each participants' first (\code{align = "first_entry"}) or last
#'   entry (\code{align = "last_entry"}), or a numeric vector indexing each
#'   participant's column with which to align the data set.
#' @param dos Optional (character string) column name with dates of filling in
#'   the questionnaire (date of study).
#' @param dob Optional (character string) column name with dates of birth.
#' @param dop Optional (character string) column name with dates of
#'   practice commencement (date of practice).
#'
#' @return A \code{data.frame} with exact practice times.
#' @export
#'
#' @examples
exact_duration <- function(x, cols = 1:ncol(x), align = NULL, dos = NULL, dob = NULL, pract_start = NULL) {
  stopifnot(is.data.frame(x),
            (is.character(cols) | is.numeric(cols)),
            is.character(align))
  if (is.character(cols)) {
    bracket_colnames <- bracket_colnames_(x,
                                          cols)
    cols <- col_names_to_indices_(x,
                                  bracket_colnames)
  }
  if (!is.null(dob) & !is.null(dos)) {
    x <- dplyr::mutate(x,
                       Age = days_lived(dob,
                                        dos),
                       DaysSinceLastBday = days_since_bday(dob,
                                                           dos))
    cols <- cols + 2
  }
  y <- x[cols]
}
my1st <- function(df, arg) {
  my2nd(df, !!enquo(arg))
}
my2nd <- function(d_f, variable) {
  dplyr::summarise(d_f, mean = mean(!!enquo(variable)))
  }
