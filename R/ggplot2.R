#' point smooth geom with prefered theme
#'
#' @inheritParams scales::dollar_format
#' @inheritParams ggplot2::margin
#' @export
geom_point_smooth <- function(prefix = "$", suffix = "", angle = 45) {
    list(ggplot2::geom_point(),
         ggplot2::geom_smooth(ggplot2::aes(group = 1)),
         ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = prefix, suffix = suffix)),
         ggplot2::theme(axis.text.x = element_text(angle = angle))
         )
}


#' ecdf geom with prefered theme
#'
#' @inheritParams geom_point_smooth
#' @inheritParams ggplot2::ylab
#' @export
geom_ecdf_s <- function(prefix = "$", suffix = "", label = "") {
    list(
      ggplot2::stat_ecdf(),
      ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = prefix, suffix = suffix)),
      ggplot2::scale_y_continuous(labels = scales::percent),
      ggplot2::ylab(label=label),
      ggtech::theme_tech(theme = "airbnb"))
}

#' point line geom with prefered theme
#'
#' @inheritParams geom_point_smooth
#' @inheritParams ggplot2::scale_color_brewer
#' @inheritParams ggplot2::geom_line
#' @export
geom_point_line_s <- function(prefix = "$", suffix = "", palette = "Paired", ...) {
    list(
      ggplot2::geom_point(),
      ggplot2::geom_line(...),
      ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = prefix, suffix = suffix)),
      ggplot2::scale_color_brewer(palette = palette),
      ggtech::theme_tech(theme = "airbnb"))
}


#' fliped bar geom with perfered theme
#'
#' @inheritParams geom_point_smooth
#' @inheritParams ggplot2::scale_fill_gradient
#' @export
geom_flip_bar_s <- function(prefix = "$", suffix = "", low = "#00AFBB", high = "#FC4E07") {
    list(
      ggplot2::geom_bar(stat = "identity"),
      ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = prefix, suffix = suffix),
                                  sec.axis = ggplot2::dup_axis()),
      ggplot2::scale_fill_gradient(low = low, high = high),
      ggplot2::coord_flip(),
      ggtech::theme_tech(theme = "airbnb"))
}

#' filled bar geom with prefered theme
#'
#' @inheritParams geom_point_smooth
#' @inheritParams ggplot2::scale_fill_brewer
#' @inheritParams ggplot2::geom_bar
#' @export
geom_fill_bar_s <- function(prefix = "$", suffix = "", palette = "Paired", position = "dodge") {
    list(
      ggplot2::geom_bar(stat = "identity", position = position),
      ggplot2::scale_fill_brewer(palette = palette),
      ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = prefix, suffix = suffix)),
      ggtech::theme_tech(theme = "airbnb"))
}


