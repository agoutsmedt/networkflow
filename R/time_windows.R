# Internal helpers for the time windows of build_dynamic_networks().
#
# A time variable is turned into an integer index in a given unit (the year
# for years, months since year 0 for months...). Windows are then intervals
# of consecutive indices: `window` units wide, starting every `step` units.
# The user's column is never modified: only the index is used to select the
# nodes (or the links) of each window.

time_units <- c("year", "quarter", "month", "week", "day")

# Is the variable made of dates (Date, POSIXct, or ISO character dates such as
# "2001-03" or "2001-03-15")? Years written as numbers or as "2001" are not.
is_date_variable <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(TRUE)
  }
  if (is.character(x) || is.factor(x)) {
    values <- as.character(x)
    values <- values[!is.na(values)]
    return(length(values) > 0 && all(grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?([ T].*)?$", values)))
  }
  FALSE
}

# Convert to Date; "2001-03" is read as the first day of the month.
as_date_flexible <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  x <- as.character(x)
  short <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}$", x)
  x[short] <- paste0(x[short], "-01")
  as.Date(substr(x, 1, 10), optional = TRUE)
}

# Integer index of each value, in `unit` (dates) or in the units of the
# variable itself (numbers, including years written as text). Missing or
# invalid values give NA.
time_index <- function(x, unit = NULL) {
  if (!is_date_variable(x)) {
    return(suppressWarnings(as.integer(as.character(x))))
  }
  dates <- as_date_flexible(x)
  year <- as.integer(format(dates, "%Y"))
  month <- as.integer(format(dates, "%m"))
  switch(
    unit,
    year = year,
    quarter = year * 4L + (month - 1L) %/% 3L,
    month = year * 12L + (month - 1L),
    week = as.integer(floor((as.numeric(dates) - 4) / 7)), # weeks start on Mondays (1970-01-05)
    day = as.integer(as.numeric(dates))
  )
}

# Label of one index in a unit: "2001", "2001-Q2", "2001-04", the Monday of a
# week, a day.
index_label <- function(index, unit = NULL) {
  if (is.null(unit)) {
    return(as.character(index))
  }
  switch(
    unit,
    year = as.character(index),
    quarter = sprintf("%d-Q%d", index %/% 4L, index %% 4L + 1L),
    month = sprintf("%d-%02d", index %/% 12L, index %% 12L + 1L),
    week = format(as.Date(index * 7 + 4, origin = "1970-01-01")),
    day = format(as.Date(index, origin = "1970-01-01"))
  )
}

# Label of a window: "1975-1984" for years and numbers (kept from earlier
# versions), "2001-01 to 2001-12" for the other units, and the label of the
# unit itself for a window of one unit ("2001", "2001-Q2").
window_label <- function(start, window, unit = NULL) {
  if (window == 1) {
    return(index_label(start, unit))
  }
  end <- start + window - 1L
  if (is.null(unit) || unit == "year") {
    return(paste0(start, "-", end))
  }
  paste(index_label(start, unit), "to", index_label(end, unit))
}

# Starts of the windows: every `step` units from `first`, until a window
# reaches `last`. The last window may be shorter than the others (the caller
# warns). A step of 1 slides the window one unit at a time; a step equal to
# the window gives successive windows.
window_starts <- function(first, last, window, step) {
  starts <- seq(first, last, by = step)
  complete <- starts + window - 1L >= last
  if (any(complete)) {
    starts <- starts[seq_len(which(complete)[1])]
  }
  starts
}

# Check a positive whole number given by the user.
check_positive_integer <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x != round(x)) {
    cli::cli_abort("{.arg {arg}} must be a positive whole number.", call = call)
  }
  as.integer(x)
}
