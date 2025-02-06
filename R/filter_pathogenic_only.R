#' Filter for pathogenic results only
#'
#' This function filters a list of pathogenicity classifications, retaining only those marked as "Pathogenic".
#'
#' @param pathogenic_list List. A list of pathogenicity classifications.
#' @param related_list List. A list of corresponding data to filter alongside pathogenicity.
#' @return A list containing only the elements of the related list corresponding to "Pathogenic" classifications.
#' @export
filter_pathogenic_only <- function(pathogenic_list, related_list) {
  total_size <- length(pathogenic_list)
  positions <- which(unlist(pathogenic_list) == "Pathogenic")
  filtered_list <- rep(NA, length(unlist(pathogenic_list)))
  filtered_list[positions] <- unlist(related_list)[positions]
  grouped <- gl(total_size, ceiling(length(filtered_list) / total_size))
  return(split(filtered_list, grouped))
}
