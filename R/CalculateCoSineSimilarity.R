#' Calculate Cosine Similarity Between Two Vectors
#'
#' This function calculates the cosine similarity between two numeric vectors.
#' Cosine similarity measures the cosine of the angle between two vectors,
#' which is a value between -1 and 1. A cosine similarity of 1 indicates that
#' the vectors are identical, 0 indicates orthogonality, and -1 indicates that
#' the vectors point in completely opposite directions.
#'
#' @param targetMean A numeric vector representing the first set of values.
#' @param comparatorMean A numeric vector representing the second set of values.
#'
#' @return A numeric value representing the cosine similarity between the two vectors.
#'
#' @export
calculateCosineSimilarity <- function(targetMean, comparatorMean) {
  dot_product <- sum(targetMean * comparatorMean)
  magnitude_target <- sqrt(sum(targetMean^2))
  magnitude_comparator <- sqrt(sum(comparatorMean^2))
  output <- dot_product / (magnitude_target * magnitude_comparator)
  return(output)
}
