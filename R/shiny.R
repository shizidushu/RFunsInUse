#' shiny download module UI
#'
#' @description refer to https://shiny.rstudio.com/articles/modules.html
#' @inheritParams shiny::NS
#' @inheritParams shiny::downloadButton
#' @export
downloadfileOutput <- function(namespace, label) {
  ns <- shiny::NS(namespace)
  shiny::downloadButton(outputId = ns("downloadfilename"), label= label)
}

#' shiny download module Server
#'
#' @inheritParams shiny::downloadHandler
#' @inheritParams writexl::write_xlsx
#' @param input refer to \url{https://shiny.rstudio.com/articles/modules.html}
#' @param output refer to \url{https://shiny.rstudio.com/articles/modules.html}
#' @param session refer to \url{https://shiny.rstudio.com/articles/modules.html}
#' @export
downloadfile <- function(input,
                         output,
                         session,
                         x,
                         filename) {
    output$downloadfilename <- shiny::downloadHandler(
      filename = filename,
      content = function(path) {writexl::write_xlsx(x, path)},
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
}
