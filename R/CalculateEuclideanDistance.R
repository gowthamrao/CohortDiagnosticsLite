# Euclidean distance function
calculateEuclideanDistance <- function(targetMean, comparatorMean) {
  sqrt(sum((targetMean - comparatorMean)^2))
}
