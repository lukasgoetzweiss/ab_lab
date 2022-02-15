#' Apply filter logic to user data
#'
#' @param user user table from database
#' @param filter_params a string specifying the filter to apply
#'
#' @return user data appended with incl column.
#' @import data.table
#' @export

apply_filter = function(user, filter_params){

  message("applying filter ", filter_params)

  fp = strsplit(filter_params, ":")[[1]]
  filter_type = fp[1]

  if(filter_type == "m"){
    user[get(fp[2]) < as.numeric(fp[3]) | get(fp[2]) > as.numeric(fp[4]),
         incl := F]
    user[is.na(get(fp[2])), incl := F]

  } else if(filter_type == "v"){
    user[!(get(fp[2]) %in% fp[-c(1,2)]), incl := F]
    user[is.na(get(fp[2])), incl := F]
  } else {
    warning(paste("unable to parse filter_params:", filter_params))
  }

  message(user[, scales::percent(mean(incl))], " of users remain")

  return(user)
}
