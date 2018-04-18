#' summary table of mesures of location
#'
#' @param df dataframe
#' @param var variable that you compute measures for
#' @inheritParams knitr::kable
#' @export
common_summary_table <- function(df,var, digits = 2, caption = NULL) {
  var <- rlang::enquo(var)
  df %>%
    dplyr::summarise("min" = min(rlang::UQ(var)),
              "25%<=" = stats::quantile(rlang::UQ(var), probs = 0.25),
              "50%<=" = stats::quantile(rlang::UQ(var), probs = 0.5),
              "mean" = mean(rlang::UQ(var)),
              "75%<=" = stats::quantile(rlang::UQ(var), probs = 0.75),
              "max" = max(rlang::UQ(var))
    ) %>%
    knitr::kable(digits = digits, caption = caption)
}


#' summarise the deleted order data
#'
#' @param df a tbl or dataframe
#' @inheritParams rlang::quos
#' @export
summarise_del <- function(df, ...) {
  group_by <- rlang::quos(...)

  df %>%
    dplyr::group_by(rlang::UQS(group_by)) %>%
    dplyr::summarise(
      FactOrderModifyAnalysisIncreaseStockQuantity = sum(FactOrderModifyAnalysisIncreaseStockQuantity),
      amount_variation = sum(amount_variation) %>% abs(),
      quantity_variation = sum(quantity_variation) %>% abs() ) %>%
    dplyr::arrange(rlang::UQS(group_by)) %>%
    dplyr::ungroup()
}


#' calculate the deletion rate for deleted order data
#'
#' @param df a tbl or dataframe
#' @export
calc_del_rate <- function(df) {
  df %>%
    dplyr::mutate(
      amt_del_rate = amount_variation / FactOrdersSalesAmount,
      qty_del_rate = quantity_variation / FactOrdersProductQuantity)
}



#' Create an HTML table widget using the DataTables library with Chinese menu
#'
#' @inheritParams DT::datatable
#' @param pageLength the length of page
#' @param lengthMenu the menu of length
#' @param buttons buttons shows
#' @export
datatable_cn <-
  function(data,
           caption = NULL,
           rownames = FALSE,
           colnames,
           extensions = "Buttons",
           pageLength = pageLength,
           lengthMenu = c(5, 10 ^ (1:5)),
           buttons = c('copy', 'csv', 'excel', 'pdf', 'print') ) {
    DT::datatable(
      data = data,
      caption = caption,
      rownames = rownames,
      colnames = colnames,
      extensions = extensions,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
        dom = 'lBfrtip',
        pageLength = pageLength,
        lengthMenu = lengthMenu,
        buttons =buttons
      )
    )
  }

#' calculate sku rank
#'
#' @param df a tbl or dataframe
#' @inheritParams dplyr::group_by
#' @param amount_weight defaults to 0.2
#' @param quantity_weight defaults to 0.8
#' @export
cal_rank_score <- function(df,
                           ...,
                           amount_weight = 0.2,
                           quantity_weight = 0.8) {
  group_by <- rlang::quos(...)

  df %>%
    dplyr::group_by(rlang::UQS(group_by)) %>%
    dplyr::mutate(
      amt_scaled = scale(FactOrderDetailsAmount),
      qty_scaled = scale(FactOrderDetailsQuantity)
    ) %>%
    dplyr::mutate(score = amount_weight * amt_scaled + quantity_weight *
                    qty_scaled) %>%
    dplyr::arrange(dplyr::desc(score), .by_group = TRUE) %>%
    dplyr::mutate(rank = row_number()) %>%
    dplyr::select(
      -amt_scaled,-qty_scaled,-score,-FactOrderDetailsAmount,-FactOrderDetailsQuantity
    ) %>%
    dplyr::ungroup()
}
