#' Compute Standardized Difference Between Target and Comparator Proportions
#'
#' This function calculates the standardized difference between two data frames containing
#' target and comparator proportions. It assumes that the input data frames have columns
#' `targetMean` and `comparatorMean` representing the proportions of the target and comparator groups, respectively.
#' The standardized difference is computed using the pooled standard deviation of the proportions.
#'
#' @param targetProportion A data frame containing the target group's proportions with a column `targetMean`.
#' @param comparatorProportion A data frame containing the comparator group's proportions with a column `comparatorMean`.
#'
#' @return A data frame with the shared columns between the target and comparator data frames,
#' as well as the calculated standardized differences. The result includes columns for the
#' means (`targetMean`, `comparatorMean`), counts (if available), and the `stdDiff` column representing
#' the standardized difference.
#'
#' @details The function performs the following steps:
#' 1. Ensures that the required columns `targetMean` and `comparatorMean` exist in the provided data frames.
#' 2. Identifies shared columns between the two data frames and joins them.
#' 3. Computes the standard deviation for the target and comparator proportions using the formula:
#' \deqn{SD = \sqrt{p(1 - p)}}
#' where `p` is the proportion.
#' 4. Pooled standard deviation is calculated as:
#' \deqn{pooledSD = \sqrt{\frac{(SD_{Target}^2 + SD_{Comparator}^2)}{2}}}
#' 5. The standardized difference is then computed as:
#' \deqn{stdDiff = \frac{comparatorMean - targetMean}{pooledSD}}
#'
#' @export
computeStdDiff <- function(targetProportion, comparatorProportion) {
  # Ensure required columns exist in both data frames
  if (!"targetMean" %in% colnames(targetProportion)) {
    stop("targetMean not found in targetProportion")
  }
  
  if (!"comparatorMean" %in% colnames(comparatorProportion)) {
    stop("comparatorMean not found in comparatorProportion")
  }
  
  # Identify shared columns between the two data frames
  sharedColumns <- intersect(colnames(targetProportion),
                             colnames(comparatorProportion))
  
  # Combine the two data frames on shared columns
  combinedData <- targetProportion |>
    dplyr::full_join(comparatorProportion, by = sharedColumns)
  
  # Calculate standard deviations and standardized differences
  standardizedDifference <- combinedData |>
    dplyr::mutate(
      targetSd = sqrt(targetMean * (1 - targetMean)),
      comparatorSd = sqrt(comparatorMean * (1 - comparatorMean)),
      pooledSd = sqrt((targetSd ^ 2 + comparatorSd ^ 2) / 2),
      stdDiff = (comparatorMean - targetMean) / pooledSd
    ) |>
    dplyr::select(
      dplyr::all_of(sharedColumns),
      dplyr::all_of(dplyr::starts_with("target")),
      dplyr::all_of(dplyr::starts_with("comparator")),
      stdDiff
    )
  
  return(standardizedDifference)
}
