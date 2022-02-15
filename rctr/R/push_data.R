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

  # run this in a tryCatch, sometimes the job cannot be found by bqr_upload_data
  # after pushing data, resulting in an error even though data is successfully
  # written
  tryCatch({
    tmp = bigQueryR::bqr_upload_data(
      projectId = projectID,
      datasetId = dataSet,
      tableId = tableId,
      upload_data = upload_data,
      writeDisposition = writeDisposition,
      schema = schema,
      create = create,
      autodetect = F
    )
  },
  error = function(cond){
    message("API returned following error:")
    message(as.character(cond$message))
    if(stringr::str_detect(cond$message, "Not found")){
      message("\n Job not found... continuing anyway")
    } else {
      message("Unexpected error occurred pushing to DB")
    }
  },
  finally = {
    message("exiting tryCatch")
  })

  return(NULL)
}
