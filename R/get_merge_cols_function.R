#' Get Merge Columns Function
#'
#' This is a generic function that identifies the common columns between 2 datasets for merging.
#' @keywords cleaning merging
#' @export
#' @examples
#' get_merge_cols()

get_merge_cols <- function(data1, data2){
  names1 <- colnames(data1)
  names2 <- colnames(data2)
  remove <- setdiff(names1, names2)
  return(names1[! names1 %in% remove])
}
