#' Compare Index Event Breakdown between Target and Comparator Cohorts
#'
#' This function compares the breakdown of index events between a target cohort and a comparator cohort.
#' It calculates the proportions of persons or records (depending on the `compareEntries` parameter) for each cohort
#' and combines the results into a single data frame. The function also computes standardized differences between
#' the target and comparator cohorts.
#'
#' @param indexEventBreakdown A data frame containing index event breakdown information. It should include fields such as
#'        `cohortDefinitionId`, `persons`, and `records`, along with `databaseId` and other event-related details.
#' @param cohortCount A data frame containing cohort counts, including `cohortSubjects` and `cohortEntries` for each `cohortDefinitionId`.
#' @param targetCohortId An integer representing the cohort definition ID of the target cohort to be compared.
#' @param targetDatabaseId (Optional) A string representing the ID of the target database. If NULL, it will be derived from the `indexEventBreakdown`.
#' @param comparatorCohortId An integer representing the cohort definition ID of the comparator cohort to be compared.
#' @param comparatorDatabaseId (Optional) A string representing the ID of the comparator database. If NULL, it will default to `targetDatabaseId`.
#' @param compareEntries A boolean indicating whether to compare records (if `TRUE`) or persons (if `FALSE`). Defaults to `FALSE`.
#'
#' @return A data frame with the combined breakdown of index events for the target and comparator cohorts. It includes
#'         fields such as cohort IDs, database IDs, event concepts, event counts, and computed means (proportions) for both
#'         target and comparator cohorts. The function also returns standardized differences between the target and comparator cohorts.
#'
#' @details
#' - The `compareEntries` parameter determines whether the comparison is based on the number of `records` or `persons`.
#'   If `compareEntries = TRUE`, the function compares records (from `cohortEntries` and `records`); otherwise, it compares persons
#'   (from `cohortSubjects` and `persons`).
#' - The `computeStandardizedDifference` function is applied to the proportions of both cohorts to calculate the standardized
#'   differences.
#'
#' @export
compareIndexEventBreakdown <- function(indexEventBreakdown,
                                       cohortCount,
                                       targetCohortId,
                                       targetDatabaseId = NULL,
                                       comparatorCohortId,
                                       comparatorDatabaseId = targetDatabaseId,
                                       compareEntries = FALSE) {
  # Check that required columns exist in indexEventBreakdown
  requiredColsIndexEvent <- c(
    "cohortDefinitionId",
    "databaseId",
    if (compareEntries) {
      "records"
    } else {
      "persons"
    }
  )
  missingColsIndexEvent <- setdiff(requiredColsIndexEvent, colnames(indexEventBreakdown))
  if (length(missingColsIndexEvent) > 0) {
    stop(paste(
      "Missing columns in 'indexEventBreakdown':",
      paste(missingColsIndexEvent, collapse = ", ")
    ))
  }

  # Check that required columns exist in cohortCount
  requiredColsCohortCount <- c("cohortId", if (compareEntries) {
    "cohortEntries"
  } else {
    "cohortSubjects"
  })

  missingColsCohortCount <- setdiff(requiredColsCohortCount, colnames(cohortCount))
  if (length(missingColsCohortCount) > 0) {
    stop(paste(
      "Missing columns in 'cohortCount':",
      paste(missingColsCohortCount, collapse = ", ")
    ))
  }

  # Check that targetCohortId and comparatorCohortId exist in indexEventBreakdown
  availableCohorts <- unique(indexEventBreakdown$cohortDefinitionId)
  if (!(targetCohortId %in% availableCohorts)) {
    stop(paste(
      "targetCohortId",
      targetCohortId,
      "not found in indexEventBreakdown"
    ))
  }
  if (!(comparatorCohortId %in% availableCohorts)) {
    stop(
      paste(
        "comparatorCohortId",
        comparatorCohortId,
        "not found in indexEventBreakdown"
      )
    )
  }

  # Check that database IDs exist in the data
  availableDatabases <- unique(indexEventBreakdown$databaseId)
  if (!(targetDatabaseId %in% availableDatabases)) {
    stop(paste(
      "targetDatabaseId",
      targetDatabaseId,
      "not found in indexEventBreakdown"
    ))
  }
  if (!(comparatorDatabaseId %in% availableDatabases)) {
    stop(
      paste(
        "comparatorDatabaseId",
        comparatorDatabaseId,
        "not found in indexEventBreakdown"
      )
    )
  }

  # Helper function to process data
  processCohortData <- function(indexEventBreakdown,
                                cohortCount,
                                cohortId,
                                databaseId,
                                compareEntries,
                                prefix = "target") {
    cohortCol <- if (compareEntries) {
      "cohortEntries"
    } else {
      "cohortSubjects"
    }

    countCol <- if (compareEntries) {
      "records"
    } else {
      "persons"
    }

    output <- indexEventBreakdown |>
      dplyr::filter(
        .data$cohortDefinitionId == cohortId,
        .data$databaseId %in% c(databaseId)
      ) |>
      dplyr::inner_join(
        cohortCount |>
          dplyr::select(
            cohortDefinitionId = cohortId,
            cohortValue = !!rlang::sym(cohortCol),
            "databaseId"
          ),
        by = c("cohortDefinitionId", "databaseId")
      ) |> # Use `:=` to dynamically assign column names
      dplyr::rename(
        !!paste0(prefix, "Count") := !!rlang::sym(countCol),
        !!paste0(prefix, "CohortId") := "cohortDefinitionId",
        !!paste0(prefix, "DatabaseId") := "databaseId"
      ) |>
      dplyr::mutate(!!paste0(prefix, "Mean") := !!rlang::sym(paste0(prefix, "Count")) / .data$cohortValue) |>
      dplyr::select(
        "conceptId",
        "sourceConcept",
        "domain",
        dplyr::all_of(dplyr::starts_with(prefix))
      ) |>
      dplyr::tibble()

    combis <- output |> dplyr::select("conceptId", "sourceConcept", "domain")

    if (combis |> nrow() > combis |>
      dplyr::distinct() |>
      nrow()) {
      stop(
        "After filtering by cohortId and databaseId, the combination of conceptId, sourceConcept, domain is not unique."
      )
    }

    return(output) # Return the output so that it's usable
  }

  # Process target and comparator data
  targetData <- processCohortData(
    indexEventBreakdown =
      indexEventBreakdown,
    cohortCount =
      cohortCount,
    cohortId =
      targetCohortId,
    databaseId =
      targetDatabaseId,
    compareEntries =
      compareEntries,
    prefix = "target"
  )

  comparatorData <- processCohortData(
    indexEventBreakdown =
      indexEventBreakdown,
    cohortCount =
      cohortCount,
    cohortId =
      comparatorCohortId,
    databaseId =
      comparatorDatabaseId,
    compareEntries =
      compareEntries,
    prefix = "comparator"
  )

  # Calculate standardized differences
  stdDiff <- calculateStandardizedDifference(
    targetProportion = targetData,
    comparatorProportion = comparatorData
  ) |>
    dplyr::arrange(dplyr::desc(stdDiff))

  # calculate CoSineSimilarity
  stdDiff$cosineSimilarity <- calculateCosineSimilarity(targetMean = stdDiff$targetMean, comparatorMean = stdDiff$comparatorMean)

  # calculate Euclidean Distance
  stdDiff$euclideanDistance <- calculateEuclideanDistance(targetMean = stdDiff$targetMean, comparatorMean = stdDiff$comparatorMean)

  return(stdDiff)
}
