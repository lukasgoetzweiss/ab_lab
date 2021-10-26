#' push data to database
#'
#' Wrapper for bigQueryR::bqr_upload_data that uses BigQuery environment
#' variables by default.
#'
#' @param tableId Name of table in GCP
#' @param upload_data data.table to upload
#' @param dataSet name of BigQuery data set
#' @param projectID BigQuery project ID
#' @param writeDisposition see ?bigQueryR::bqr_upload_data
#' @param schema see ?bigQueryR::bqr_upload_data
#'
#' @return NULL
#' @export
#'
#' @examples
#' push_data("db_table", local_data_table)
push_data = function(tableId, upload_data,
                     dataSet = Sys.getenv("bq_dataSet"),
                     projectID = Sys.getenv("bq_projectID"),
                     writeDisposition = "WRITE_APPEND",
                     create = "CREATE_NEVER",
                     schema = NULL){
  bigQueryR::bqr_upload_data(
    projectId = projectID,
    datasetId = dataSet,
    tableId = tableId,
    upload_data = upload_data,
    writeDisposition = writeDisposition,
    schema = schema,
    create = create,
    autodetect = F
  )
  return(NULL)
}
