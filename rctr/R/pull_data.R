#' Pull data from database
#'
#' Evaluate sql statement and return result as a data.table. Ensure that you
#' have authorized with BigQuery e.g., with bigrquery::bq_auth(). If there is an
#' error will retry 3 times
#'
#' @param sql sql statement to run
#' @param projectID BigQuery project ID, by default will use Sys.getenv("bq_projectID")
#'
#' @return data.table populated with query results
#' @export
pull_data = function(sql, db_name = Sys.getenv("db_name")){
  
  # Establish connection to SQLite database
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  # Execute the SQL query and fetch the results into a data.table
  data <- data.table(DBI::dbGetQuery(conn, sql))
  
  # Close the connection
  DBI::dbDisconnect(conn)
  
  # Return the data.table
  return(data)
  
  # message(lubridate::now(), " running query:")
  # message(sql)
  # success = F
  # attempt <- 1
  # while( success == F && attempt <= 3 ) {
  #   message("attempt", attempt)
  #   attempt <- attempt + 1
  #   try({
  #     tb <- bigrquery::bq_project_query(projectID, sql)
  #     res = bigrquery::bq_table_download(tb)
  #     success = T
  #   })
  #   if(attempt > 3){
  #     stop("Query failed after 3 attempts")
  #   }
  # }
  # 
  # return(data.table::data.table(res))
}
