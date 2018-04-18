
#' tech theme (copy from ggtech)
#'
#' @param theme the theme you want to use
#' @param tech_key tech theme key
#' @export
#' @author Ricardo Bion
#' @title Tech themes  for ggplot2

theme_tech <- function(theme="airbnb", tech_key = list(
  airbnb = list(
    family_title="Circular Air Bold"
    , family_text = "Circular Air Medium"
    , colour_title = "#F14000"
    , colour_text = "#535353"),
  facebook = list(
    family_title="Facebook Letter Faces"
    , family_text = "Facebook Letter Faces"
    , colour_title = "#3D579D"
    , colour_text = "#535353"),
  google = list(
    family_title="Product Sans"
    , family_text = "Product Sans"
    , colour_title = "#dd4b39"
    , colour_text = "black"),
  etsy = list(
    family_title="."
    , family_text = "."
    , colour_title = "#F14000"
    , colour_text = "#535353"),
  twitter = list(
    family_title="PicoBlackAl"
    , family_text = "[z] Arista Light"
    , colour_title = "#5380E4"
    , colour_text = "black")
)) {

  theme_classic() +
    theme(text=element_text(size=18, family=tech_key[[theme]]$family_text)) +
    theme(legend.title=element_blank()) +
    theme(plot.title = element_text(size = 25, colour = tech_key[[theme]]$colour_title, family=tech_key[[theme]]$family_title)) +
    theme(plot.subtitle = element_text(size = 15, colour = tech_key[[theme]]$colour_title, family=tech_key[[theme]]$family_title)) +
    theme(axis.text.x=element_text(color=tech_key[[theme]]$colour_text)) +
    theme(axis.text.y=element_text(color=tech_key[[theme]]$colour_text)) +
    theme(axis.title.x=element_text(color=tech_key[[theme]]$colour_text, vjust=0)) +
    theme(axis.title.y=element_text(color=tech_key[[theme]]$colour_text, vjust=1.25)) +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color=tech_key[[theme]]$colour_text, size = 0.5),
          axis.line.y = element_line(color=tech_key[[theme]]$colour_text, size = 0.5)) +
    theme(line = element_line(color=tech_key[[theme]]$colour_text)) +
    theme(rect = element_rect(color=tech_key[[theme]]$colour_text)) +
    theme(axis.ticks.x = element_line(color=tech_key[[theme]]$colour_text)) +
    theme(axis.ticks.y = element_line(color=tech_key[[theme]]$colour_text))

}



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
      theme_tech(theme = "airbnb"))
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
      theme_tech(theme = "airbnb"))
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
      theme_tech(theme = "airbnb"))
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
      theme_tech(theme = "airbnb"))
}


