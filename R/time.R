
#' get the year of iso week
#'
#' @inheritParams  lubridate::year
#' @return the year of the date in iso week
#' @export
yisoweek <- function(x) {
    xday <- lubridate::make_datetime(lubridate::year(x), lubridate::month(x), lubridate::day(x))
    dn <- 1 + (lubridate::wday(x) + 5)%%7
    nth <- xday + lubridate::ddays(4 - dn)
    jan1 <- lubridate::make_datetime(lubridate::year(nth), 1, 1)
    lubridate::year(jan1)
}


#' Generate a tbl of day week month quarter semester year.
#' Mainly used for joining data
#'
#' @inheritParams base::paste
#' @inheritParams lubridate::quarter
#' @export
days_df <- function(sep = "~", with_year = TRUE) {
  tibble::tibble(day = seq(as.Date("1910/1/1"),
                           lubridate::today() + 7,
                           by = "day")) %>%
    dplyr::mutate(week = lubridate::isoweek(day),
           yweek = yisoweek(day)) %>%
    dplyr::group_by(week, yweek) %>%
    dplyr::mutate(weekstart = min(day),
           weekend = max(day)) %>%
    dplyr::mutate(weekrange = paste(format(weekstart, "%y%m%d"),
                             format(weekend, "%y%m%d"),
                             sep = sep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      month = lubridate::month(day),
      quarter = lubridate::quarter(day),
      quarter_wy = lubridate::quarter(day, with_year = TRUE),
      semester = lubridate::semester(day),
      semester_wy = lubridate::semester(day, with_year = TRUE),
      year = lubridate::year(day)
    )}


#' First day of this month
#'
#' @export
day_1_this_month <- function() {
  lubridate::ymd(paste(
    lubridate::year(lubridate::today()),
    lubridate::month(lubridate::today()),
    "01"
  ))
}


#' Frst day of this year
#'
#' @export
day_1_this_year <- function() {
  lubridate::ymd(paste(
    lubridate::year(lubridate::today()),
    "01",
    "01"
  ))
}
