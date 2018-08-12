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


#' Concatenate shiny input value into character string
#' 
#' @param input shiny input object. Always should be input
#' @param exclude_start_chr a character vector. If the name of input start with the character, exclude its value(s)
#' @export
get_shiny_input_values <- function(input, exclude_start_chr = "table") {
  input_names <- names(input)
  
  if(!rlang::is_character(exclude_start_chr)) stop("exclude_start_chr is not character")
  
  for (i in seq_along(exclude_start_chr)) {
    input_names <- input_names[!startsWith(input_names, exclude_start_chr[i])] 
  }
  
  input_names %>%
    purrr::map(~input[[.x]] %>%
                 paste(sep = "_", collapse = "")) %>%
    paste0(sep = "_", collapse = "")
}