#' Utility function for easy modal dialogs
#'
#' @param text Test to display in the modal
#'
#' @return simple modal with displaying text
#' @export
loadingModal = function(text = "Loading...") {
  modalDialog(p(text), footer = NULL)
}
