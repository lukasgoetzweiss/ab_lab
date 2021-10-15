#' Create records in audience table and audience_filter
#'
#' @param name Audience name
#' @param audience_filter Audience filters
#' @param dataset BigQuery dataset
#'
#' @return audience_id
#' @import data.table
#' @export
create_audience = function(name, audience_filter,
                           dataset = Sys.getenv("bq_dataSet")){
  audience_id = pull_data(glue::glue(
    "select max(audience_id)  from {dataset}.audience"
  ))$f0_[1] + 1
  if(is.na(audience_id)){ audience_id = 1 }

  message("creating audience ", name, ", audience_id = ", audience_id)

  push_data(
    "audience",
    data.table(
      audience_id = as.integer(audience_id),
      name = name,
      create_datetime = Sys.time()
    )
  )

  audience_filter_id = pull_data(glue::glue(
    "select max(audience_filter_id)  from {dataset}.audience_filter"
  ))$f0_[1]
  if(is.na(audience_filter_id)){ audience_filter_id = 0 }

  audience_filter_id = audience_filter_id + (1:audience_filter[, .N])

  push_data(
    "audience_filter",
    audience_filter[, .(audience_filter_id = as.integer(audience_filter_id),
                        audience_id = as.integer(audience_id),
                        filter_on,
                        comparator_sql,
                        comparator_params,
                        create_datetime = Sys.time())]
  )

  return(audience_id)
}
