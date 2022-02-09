#' Create user metadata
#'
#' @param user - user_data (from get_user)
#'
#' @return list of user metrics, variables, and variable values
#' @export
#'
#' @examples
create_user_metadata = function(user = NULL){

  if(is.null(user)){
    user = get_table("user", dataset = "example_data")
  }

  user_metric = NULL
  user_variable = NULL
  user_variable_value = NULL

  for(col_name in setdiff(names(user), Sys.getenv("unit_pk"))){
    if(user[, is.Date(get(col_name))]){
      message("parsing ", col_name, " as date")
    } else if (user[, mode(get(col_name))] == "character"){
      message("parsing ", col_name, " as character")
      user_variable = rbind(user_variable, data.table(name = col_name))
      user_variable_value = rbind(
        user_variable_value,
        data.table(
          variable = col_name,
          value = user[, unique(get(col_name))]
        )
      )
    } else if (user[, mode(get(col_name))] == "numeric") {
      message("parsing ", col_name, " as numeric")
      user_metric = rbind(
        user_metric,
        data.table(
          name = col_name,
          metric_min = user[, min(get(col_name))],
          metric_max = user[, max(get(col_name))]
        )
      )
    } else {
      message("failed to parse ", col_name)
    }
  }

  return(list(
    "user_metric" = user_metric,
    "user_variable" = user_variable,
    "user_variable_value" = user_variable_value
  ))

}
