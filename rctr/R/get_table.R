#' Pull table from database
#'
#' @param table_name table to pull
#' @param limit maximum number of rows to pull
#' @param dataset BigQuery project ID
#'
#' @return table
#' @export
get_table = function(table_name, limit = NULL,
                        dataset = Sys.getenv("bq_dataSet")){
  limit_str = ifelse(is.null(limit), "", stringr::str_c("limit ", limit))
  data.table(pull_data(glue::glue(
    "select * from {dataset}.{table_name} {limit_str}"
  )))
}
