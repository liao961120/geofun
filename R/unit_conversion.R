#' Degree-Minute-Second to Decimal
deg2deci <- function(degree = 0, min = 0, sec = 0) {
  stopifnot(degree >= 0)
  stopifnot(min >= 0 && min <= 60)
  stopifnot(sec >= 0 && sec <= 60)

  degree + min/60 + sec/3600
}

#' Decimal to Degree-Minute-Second
#'
#' @param x numeric vector. A list of numeric values in degree
#'   to be converted to degree-minute-second.
#'
#' @return A list of numeric vectors with length 3.
deci2deg <- function(x) {
  stopifnot(is.numeric(x))

  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- deci2deg_atom(x[i])
  }

  return(out)
}

#' @keywords internal
deci2deg_atom <- function(x = 121.53) {
  stopifnot(x >= 0)

  degree <- as.integer(x)
  minute_deci <- (x - degree) * 60
  minute <- as.integer(minute_deci)
  second <- (minute_deci - minute) * 60
  return(c(degree = degree, minute = minute, second = second))
}


#' r.a.d to Degree (in decimal)
#'
#' @param rad numeric vector. A list of numeric values in r.a.d
#'   to be converted to degree.
#'
#' @return A numeric vector of length equal to the input.
rad2deg <- function(rad = pi) return(rad * 180/pi)

#' Degree to r.a.d
#'
#' @param degree numeric vector. A list of numeric values in degree
#'   to be converted to r.a.d.
#'
#' @return A numeric vector of length equal to the input.
deg2rad <- function(degree = 180) return(degree * pi/180)
