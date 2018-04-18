#' Read biee sql statements from file
#'
#' @inheritParams base::readLines
#' @param ... user defined variables and corresponding parameters in sql statements
#' @return sql statement with user defined variables
read_biee_sql <- function(con, ...) {
    # 读取SQL语句并分行
    query <- paste(readLines(con), collapse = "\n")

    # 列出变量参数
    var_list <- list(...)

    # 如果用户有补充参数，则替代SQL语句中的变量参数
    if (length(var_list) > 0) {
        for (i in 1:length(var_list)) {

            pattern <- names(var_list)[i]
            ## 为其中每一个参数都加上单引号，以适用于SQL语句
            replacement <- paste0("'", var_list[[i]], "'", collapse = ", ")

            query <- stringr::str_replace(query, pattern, replacement)
        }
    }

    query
}


#' Fetch data from database
#'
#' @inheritParams read_biee_sql
#' @inheritParams DBI::dbGetQuery
#' @export

fetch_biee_sql <- function(conn, con, ...) {
    # 读取SQL语句
    query <- read_biee_sql(con, ...)
    # 获取数据
    df <- DBI::dbGetQuery(conn, query)  # get data from biee
    # 移除colnames中的特殊字符
    colnames(df) <- stringr::str_replace_all(colnames(df), "[^[:alnum:]]", "")

    df
}
