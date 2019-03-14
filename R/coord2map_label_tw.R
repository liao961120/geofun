#' Get Map Information from Geographic Coordinates
#'
#' @param long numeric. The longtitute.
#' @param lat numeric. The latitute.
#' @param scale integer. One of \code{5}, \code{25}, \code{50},
#'   and \code{100}, corresponding to map size of \code{1/5,000},
#'   \code{1/25,000}, \code{50,000}, and \code{100,000}, respectively.
#'
#' @return A list of map information.
#'  \describe{
#'   \item{\code{mapLabel}}{Map Label (Taiwan)}
#'   \item{\code{long_range}}{Longitudinal range covered by the map.}
#'   \item{\code{lat_range}}{Latitudinal range covered by the map.}
#'  }
#'
#' @export
coord2mapLabel <- function(long = 120.5, lat = 23.45, scale = c(5, 25, 50, 100)) {
  stopifnot(scale[1] %in%  c(100, 50, 25, 5))

  # Create function from text
  call_fun <- paste0('coord2mapLabel_', scale[1])
  f <- eval(rlang::sym(call_fun))
  out <- f(long, lat)

  # Special cases: `coord2mapLabel_50()`, `coord2mapLabel_25()`
  if (scale[1] %in% c(25, 50)) {
    mapLabel <- out$mapLabel
    quadr_idx <- ifelse(scale[1] == 25,
                        substr(mapLabel, 8, nchar(mapLabel)),
                        substr(mapLabel, 6, nchar(mapLabel))
                        )

    rng <- out[paste0('quadr', quadr_idx)][[1]]
    long_rng = c(rng['left'], rng['right'])
    lat_rng = c(rng['bottom'], rng['top'])

    # Restructure output
    out_o <- out
    out <- list(mapLabel = mapLabel,
                long_range = unname(long_rng),
                lat_range = unname(lat_rng))
    out <- append(out, out_o[names(out_o) != 'mapLabel'])
  }

  return(out)
}

#' @keywords internal
coord2mapLabel_100 <- function(long = 120.5, lat = 23.45) {
  origin <- c(73.5, 14) # long, lat
  grid_space <- 0.5

  easting <- ceiling({temp <- (long - origin[1]) / grid_space})
  # Deal with origin case
  ifelse(easting == temp, {easting <- easting + 1}, "")

  northing <- ceiling({temp <- (lat - origin[2]) / grid_space})
  # Deal with origin case
  ifelse(northing == temp, {northing <- northing + 1}, "")

  map_long_left <- origin[1] + (easting - 1) * grid_space
  map_lat_bottom <- origin[2] + (northing - 1) * grid_space
  map_range_long <- c(map_long_left, map_long_left + grid_space)
  map_range_lat <- c(map_lat_bottom, map_lat_bottom + grid_space)

  return(list(mapLabel = paste0(easting, northing),
              long_range = map_range_long, lat_range = map_range_lat))
}

#' @keywords internal
coord2mapLabel_50 <- function(long = 120.5, lat = 23.45) {
  # Extract data from map_100
  map_range_100 <- coord2mapLabel_100(long, lat)

  # map_50 parameters
  grid_space <- 0.25
  origin <- c(map_range_100$long_range[1], map_range_100$lat_range[1])

  four_digit <- paste0(substr(map_range_100$mapLabel, start = 1, stop = 2),
                       substr(map_range_100$mapLabel, start = 3, stop = 4))
  quadr1 <- as_quadr(1, origin, grid_space)
  quadr2 <- as_quadr(2, origin, grid_space)
  quadr3 <- as_quadr(3, origin, grid_space)
  quadr4 <- as_quadr(4, origin, grid_space)
  lst <- list(quadr1, quadr2, quadr3, quadr4)

  # Find which quadrant the given coord falls in
  fallin_quadr <- fall_in(long, lat, lst)
  if (is.null(fallin_quadr)) stop('There is a bug, cant find point in quadr')

  # Return data
  map_50 <- list(mapLabel = paste(four_digit, fallin_quadr, sep = '-'),
                 quadr1 = quadr1, quadr2 = quadr2,
                 quadr3 = quadr3, quadr4 = quadr4)
}

#' @keywords internal
coord2mapLabel_25 <- function(long = 120.5, lat = 23.45) {
  # Extract data from map_50
  map_50 <- coord2mapLabel_50(long, lat)
  four_digit <- substr(map_50$mapLabel, start = 1, stop = 4)
  quadr_idx <- substr(map_50$mapLabel, start = 6, stop = 6)
  map_range_25 <- map_50[[paste0('quadr', quadr_idx)]]

  # map_25 parameters
  grid_space <- 0.125
  origin <- unname(c(map_range_25['left'], map_range_25['bottom']))

  quadr1 <- as_quadr(1, origin, grid_space)
  quadr2 <- as_quadr(2, origin, grid_space)
  quadr3 <- as_quadr(3, origin, grid_space)
  quadr4 <- as_quadr(4, origin, grid_space)
  lst <- list(quadr1, quadr2, quadr3, quadr4)

  # Find which quadrant the given coord falls in
  fallin_quadr <- c('NE', 'SE', 'SW', 'NW')[fall_in(long, lat, lst)]
  if (is.null(fallin_quadr)) stop('There is a bug, cant find point in quadr')

  # Return data
  map_25 <- list(mapLabel = paste(four_digit, quadr_idx, fallin_quadr, sep = '-'),
                 quadrNE = quadr1, quadrSE = quadr2,
                 quadrSW = quadr3, quadrNW = quadr4)
}

#' @keywords internal
coord2mapLabel_5 <- function(long = 120.5, lat = 23.45) {
  # Extract data from map_50
  map_50 <- coord2mapLabel_50(long, lat)
  four_digit <- substr(map_50$mapLabel, start = 1, stop = 4)
  quadr_idx <- substr(map_50$mapLabel, start = 6, stop = 6)
  map_range_50 <- map_50[[paste0('quadr', quadr_idx)]]

  # Index fallin grid cell
    # by dividing map_50 horizontally / vertically with 11 lines
  hl_pos <- seq2(map_range_50['left'], map_range_50['right'], length.out = 11)
  vl_pos <- seq2(map_range_50['bottom'], map_range_50['top'], length.out = 11)
  cell <- fall_in_100(long, lat, hl_pos, vl_pos)
  cell_idx <- cell$idx
  cell_rng <- cell$range

  # Return data
  map_5 <- list(mapLabel = paste(four_digit, quadr_idx, cell_idx, sep = '-'),
                long_range = unname(cell_rng[c('left', 'right')]),
                lat_range = unname(cell_rng[c('bottom', 'top')])
                )
}




##################### Helpers ###########################

seq2 <- function(from, to, ...) seq(from = from, to = to, ...)


fall_in <- function(long, lat, lst) {
  for (i in seq_along(lst)) {
    if (lst[[i]][1] <= long && long < lst[[i]][2] &&
        lst[[i]][3] <= lat && lat < lst[[i]][4]) {
      return(i)
    }
  }
  return(NULL)
}


as_quadr <- function(quadr = 1, origin, grid_space) {
  lookup <- list(
    q1 = list(lo = 1, lg = 1, ro = 1, rg = 2,
              bo = 2, bg = 1, to = 2, tg = 2),
    q2 = list(lo = 1, lg = 1, ro = 1, rg = 2,
              bo = 2, bg = 0, to = 2, tg = 1),
    q3 = list(lo = 1, lg = 0, ro = 1, rg = 1,
              bo = 2, bg = 0, to = 2, tg = 1),
    q4 = list(lo = 1, lg = 0, ro = 1, rg = 1,
              bo = 2, bg = 1, to = 2, tg = 2)
  )

  q <- lookup[[paste0('q', quadr)]]

  left <- origin[q$lo] + q$lg*grid_space
  right <- origin[q$ro] + q$rg*grid_space
  bottom <- origin[q$bo] + q$bg*grid_space
  top <- origin[q$to] + q$tg*grid_space

  return(c(left = left, right = right, bottom = bottom, top = top))
}


fall_in_100 <- function(long, lat, hl_pos, vl_pos) {
  for (i in 1:(length(hl_pos) - 1)) {
    if (hl_pos[i] <= long && long < hl_pos[i + 1]) {
      h_rng <- c(left = hl_pos[i], right = hl_pos[i + 1])
      h_idx <- i
      break
    }
  }
  for (i in 1:(length(vl_pos) - 1)) {
    if (vl_pos[i] <= lat && lat < vl_pos[i + 1]) {
      v_rng <- c(bottom = vl_pos[i], top = vl_pos[i + 1])
      v_idx <- 10 - i
      break
    }
  }
  cell_idx <- 10 * v_idx + h_idx

  return(list(idx = paste0(formatC(cell_idx, width = 3, flag = '0')),
              range = c(h_rng, v_rng))
         )
}
