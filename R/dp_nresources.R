#' Return the number of resources in a Data Package
#'
#' @param dp A Data Package object.
#'
#' @return
#' Returns an integer with the number of resources in the Data Package.

#' @export
dp_nresources <- function(dp) {
  # If resources does not exist dp$resources will return NULL which has a 
  # length of 0
  length(dp_property(dp, "resources"))
}
