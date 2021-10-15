#' Pull data from database
#'
#' Evaluate sql statement and return result as a data.table. Ensure that you
#' have authorized with BigQuery e.g., with bigrquery::bq_auth().
#'
#' @param sql sql statement to run
#' @param projectID BigQuery project ID, by default will use Sys.getenv("bq_projectID")
#'
#' @return data.table populated with query results
#' @export
pull_data = function(sql, projectID = Sys.getenv("bq_projectID")){
  tb <- bigrquery::bq_project_query(projectID, sql)
  res = bigrquery::bq_table_download(tb)
  return(data.table::data.table(res))
}
