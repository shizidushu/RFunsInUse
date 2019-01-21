#' Add image url prefix to image column
#'
#' @param df a tbl or dataframe
#' @param df_image_url_col_name the colname of image url column
#' @param image_url_prefix the prefix for the image url column
#' @return df
#' @export
fix_image_link <- function(df,
                               df_image_url_col_name,
                               image_url_prefix = NULL) {
  col_name <- rlang::ensym(df_image_url_col_name)
  df %>%
    dplyr::mutate(!!col_name := dplyr::case_when(
      is.na(!!col_name) | stringi::stri_length(!!col_name) == 0  ~ NA_character_,
      TRUE ~ paste0(image_url_prefix,!!col_name)
    ))
}


#' download image of the image column
#' @inheritParams fix_image_link
#' @param df_image_title_col_name the colname of the image title (usually the sku)
#' @param lsp_label the common label of lsp (linshi product); There is no image for lsp
#' @param image_file_extension the file extension of the image
#' @param image_path where the image locates; default to ~/images
#' @return df
#' @export
download_image_url <- function(df,
                               df_image_title_col_name,
                               df_image_url_col_name,
                               lsp_label = "LSP",
                               image_file_extension = ".JPG",
                               image_path = "~/images") {
  for (i in 1:nrow(df)) {
    # the sku which images will be downloaded and its path to be saved
    sku <-df[[df_image_title_col_name]][i]
    location_to_save <- paste0(file.path(image_path, sku), image_file_extension)
    image_url <- df[[df_image_url_col_name]][i]
    # check if image_url is NA or length of image_url is 0 
    # or the sku is NA, length of zero, temporary SKU without images, its image has been downloaded; otherwise download
    pred <- any(is.na(image_url),
                stringi::stri_length(image_url) ==0,
                is.na(sku),
                stringi::stri_length(sku)== 0,
                stringr::str_detect(sku, pattern = lsp_label),
                file.exists(location_to_save)
    )
    # testing
    print(paste(!pred, sku, image_url))
    # download the image
    if (!pred) {tryCatch({curl::curl_download(url = image_url, location_to_save)}, error=function(e){})}
    
  }
  df
}



#' Create a workbook for dataframe with sku image
#'
#' @inheritParams download_image_url
#' @param image_col_name the colname for the image in df
#' @inheritParams openxlsx::setColWidths
#' @inheritParams openxlsx::setRowHeights
#' @inheritParams openxlsx::insertImage
#' @return an openxlsx workbook
#' @export
create_workbook_sku_image <- function(df,
                                      df_image_title_col_name,
                                      df_image_url_col_name,
                                      image_col_name = "Image",
                                      widths = 27,
                                      heights = 144,
                                      image_file_extension = ".JPG",
                                      width = 5,
                                      height = 5,
                                      units = "cm",
                                      dpi = 300,
                                      image_path = "~/images") {
  ## 清空图片链接列内容
  df[[image_col_name]] <- " "
  ## 重命图片链接列为图片列，该列之后会被用作写入/展示图片
  names(df)[names(df) == df_image_url_col_name] <- image_col_name
  ## 创建工作簿
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  ## 将数据写入工作簿
  openxlsx::writeDataTable(wb, 1, df)
  ## 寻找图片列的位置
  col_pos <- which(image_col_name == colnames(df))
  ## 设置高度和宽度
  openxlsx::setColWidths(wb, 1, cols = col_pos, widths = widths)
  openxlsx::setRowHeights(wb, 1, rows = 2:(nrow(df) + 1), heights = heights)
  ## 将图片写入工作簿 其中开始的行加上一是因为标题行占去的一列
  ## 图片应该在之前已经下载，临时SKU没有图片，故写入图片之前检查一下是否存在图片
  for (i in 1:nrow(df)) {
    if (file.exists(paste0(file.path(image_path, df[[df_image_title_col_name]][i]), image_file_extension))) {
      openxlsx::insertImage(
        wb,
        "Sheet 1",
        paste0(file.path(image_path, df[[df_image_title_col_name]][i]), image_file_extension),
        width = width,
        height = height,
        units = units,
        dpi = dpi,
        startRow = i + 1,
        startCol = col_pos
      )
    }
  }
  wb
}


#' Write dataframe with image url to excel file with image column
#'
#' @inheritParams create_workbook_sku_image
#' @param filename a file name to write to
#' @param image_path the path to store images locally, defaults to "/tmp/images"
#' @param ... arguments passed to \code{\link{create_workbook_sku_image}} 
#' @export
write_excel_image <- function(df,
                              filename = tempfile(fileext = ".xlsx"),
                              df_image_title_col_name,
                              df_image_url_col_name,
                              image_col_name,
                              image_path = "/tmp/images",
                              ...
) {
  # create the dir if it doesn't exists
  dir.create(image_path, showWarnings = FALSE)
  # download images to temp image dir
  hfun::download_image_url(df = df,
                           df_image_title_col_name = df_image_title_col_name,
                           df_image_url_col_name = df_image_url_col_name,
                           image_path = image_path)
  # create workbook
  wb <- hfun::create_workbook_sku_image(df = df,
                                        df_image_title_col_name = df_image_title_col_name,
                                        df_image_url_col_name = df_image_url_col_name,
                                        image_col_name = image_col_name,
                                        image_path = image_path,
                                        ...)
  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  
  filename
}
