#' Check Temporal Stability For Cohort Diagnostics Incidence Rate Data
#'
#' This function checks the temporal stability of the cohort diagnostics incidence rate data. It processes the data, calculates the predicted counts, and compares the likelihoods.
#'
#' @param annualizedCrudeIncidenceRateData A data frame that contains the columns cohortId, databaseId, gender, ageGroup, calendarYear, cohortCount, personYears, and incidenceRate.
#' @param cohort A data frame that contains the columns cohortId and cohortName.
#' @param maxNumberOfSplines The maximum number of splines to use in the model. If NULL, the default is 5.
#' @param splineTickInterval The interval at which to place the splines. The default is 3.
#' @param removeOutlierZeroCounts (Default TRUE) Do you want to remove continous zero counts in the beginning and end in the time series?
#' @param maxRatio The maximum ratio for the likelihood comparison. If NULL, the default is 1.25.
#' @param alpha The significance level for the likelihood ratio test. If NULL, the default is 0.05.
#' @return A data frame with the cohortId, databaseId, gender, ageGroup, stable flag, and isUnstable flag.
#' @export
checkTemporalStabilityForannualizedCrudeIncidenceRateData <- function(annualizedCrudeIncidenceRateData,
                                                                      cohort,
                                                                      maxNumberOfSplines = NULL,
                                                                      splineTickInterval = 3,
                                                                      maxRatio = 1.25,
                                                                      alpha = 0.05,
                                                                      removeOutlierZeroCounts = TRUE) {
  checkCrudeIncidenceRateDataStratifiedIsAnnualized(annualizedCrudeIncidenceRateData)

  observedCount <- processannualizedCrudeIncidenceRateData(
    annualizedCrudeIncidenceRateData = annualizedCrudeIncidenceRateData,
    removeOutlierZeroCounts = removeOutlierZeroCounts
  )

  combos <- observedCount |>
    dplyr::select("cohortId", "databaseId") |>
    dplyr::distinct()

  output <- c()

  # Create a progress bar
  pb <- utils::txtProgressBar(
    min = 0,
    max = nrow(combos),
    style = 3
  )

  for (i in (1:nrow(combos))) {
    combo <- combos[i, ]

    data <- observedCount |>
      dplyr::inner_join(combo, by = c("cohortId", "databaseId")) |>
      dplyr::arrange(.data$calendarYear) |>
      dplyr::mutate(
        firstYearToCensor = as.Date(NA),
        firstYearCensoredPersonYears = as.numeric(NA),
        lastYearToCensor = as.Date(NA),
        lastYearCensoredPersonYears = as.numeric(NA)
      )

    lastYear <- data |>
      dplyr::filter(.data$useYearData == 0) |>
      dplyr::filter(.data$calendarYear > min(data$calendarYear))

    if (nrow(lastYear) > 0) {
      data$lastYearToCensor <- lastYear$calendarYear
      data$lastYearCensoredPersonYears <- lastYear$personYears
    }

    firstYear <- data |>
      dplyr::filter(.data$useYearData == 0) |>
      dplyr::filter(.data$calendarYear == min(data$calendarYear))

    if (nrow(firstYear) > 0) {
      data$firstYearToCensor <- firstYear$calendarYear
      data$firstYearCensoredPersonYears <- firstYear$personYears
    }

    data <- data |>
      dplyr::filter(.data$useYearData == 1)

    if (nrow(data) > 1) {
      predicted <- getPredictedEventCounts(
        data = data,
        timeSequenceField = "calendarYear",
        countField = "cohortCount",
        personTimeField = "personYears",
        maxNumberOfSplines = maxNumberOfSplines,
        splineTickInterval = splineTickInterval,
        maxRatio = maxRatio,
        alpha = alpha
      )

      output[[i]] <- predicted |>
        dplyr::mutate(
          cyclopsExpectedIncidenceRate = round(
            x = (.data$cyclopsExpected / .data$personYears) * 1000,
            digits = 10
          ),
          cyclopsObservedExpectedCountRatio = round(
            x = .data$observed / .data$cyclopsExpected,
            digits = 4
          ),
          cyclopsObservedExpectedIncidenceRateRatio = round(
            x = .data$incidenceRate / .data$cyclopsExpectedIncidenceRate,
            digits = 4
          ),
          cyclopsPercentageDeviation = (.data$observed - .data$cyclopsExpected) / .data$cyclopsExpected * 100,
          cylopsExpected = round(x = .data$cyclopsExpected, digits = 4),

          ####
          glmExpectedIncidenceRate = round(
            x = (.data$glmExpected / .data$personYears) * 1000,
            digits = 10
          ),
          glmExpectedIncidenceRateUpperBound = round(
            x = (.data$glmExpectedUpperBound / .data$personYears) * 1000,
            digits = 10
          ),
          glmExpectedIncidenceRateLowerBound = round(
            x = (.data$glmExpectedLowerBound / .data$personYears) * 1000,
            digits = 10
          ),
          glmObservedExpectedCountRatio = round(
            x = .data$observed / .data$glmExpected,
            digits = 4
          ),
          glmObservedExpectedIncidenceRateRatio = round(
            x = .data$incidenceRate / .data$glmExpectedIncidenceRate,
            digits = 4
          ),
          glmPercentageDeviation = (.data$observed - .data$glmExpected) / .data$glmExpected * 100,
          glmExpected = round(x = .data$glmExpected, digits = 4)
        )
    }

    # Update the progress bar
    utils::setTxtProgressBar(pb, i)
  }

  # Close the progress bar
  close(pb)

  output <- dplyr::bind_rows(output)

  commonFields <- intersect(colnames(observedCount), colnames(output))

  output <- observedCount |>
    dplyr::left_join(output, by = commonFields) |>
    dplyr::inner_join(cohort |>
      dplyr::select("cohortId", "cohortName"), by = "cohortId") |>
    dplyr::relocate("cohortId", "cohortName")

  return(output)
}


summarizeTemporalStabilityForannualizedCrudeIncidenceRateData <- function(temporalStabilityOutput,
                                                                          cylopsStatisticallyUnstable) {
  # Calculate the counts for tested cohorts, databases, and combinations
  cohortsTested <- temporalStabilityOutput$cohortId |>
    unique() |>
    length()
  databasesTested <- temporalStabilityOutput$databaseId |>
    unique() |>
    length()
  combosTested <- temporalStabilityOutput |>
    dplyr::select("cohortId", "databaseId") |>
    dplyr::distinct() |>
    nrow()

  # Calculate the counts for failed cohorts, databases, and combinations
  cohortsFailed <- length(cylopsStatisticallyUnstable$cohortId |> unique())
  databasesFailed <- length(cylopsStatisticallyUnstable$databaseId |> unique())
  combosFailed <- cylopsStatisticallyUnstable |>
    dplyr::select("cohortId", "databaseId") |>
    dplyr::distinct() |>
    nrow()

  # Print the summary
  cat(
    "\n",
    "-----",
    "\n",
    "Number of cohorts tested:",
    cohortsTested,
    "\n",
    "Number of databases tested:",
    databasesTested,
    "\n",
    "Number of combos tested:",
    combosTested,
    "\n",
    "-----",
    "\n",
    "Number of cohorts that failed in atleast one datasource:",
    formatCountPercent(count = cohortsFailed, percent = cohortsFailed /
      cohortsTested),
    "\n",
    "Number of databases with atleast one cohort failed:",
    formatCountPercent(count = databasesFailed, percent = databasesFailed /
      databasesTested),
    "\n",
    "Number of combos failed:",
    formatCountPercent(count = combosFailed, percent = combosFailed /
      combosTested),
    "\n",
    "-----",
    "\n"
  )
}



#' Check If Data is Cohort Diagnostics Incidence Rate Data
#'
#' This function checks if the input data is a data frame and contains all the expected columns with the correct data types for cohort diagnostics incidence rate data.
#'
#' @param data A data frame that is expected to contain the columns cohortCount, personYears, gender, ageGroup, calendarYear, incidenceRate, cohortId, and databaseId.
#' @return This function does not return a value. It stops and throws an error if the input data does not meet the expected conditions.
checkCrudeIncidenceRateDataStratifiedIsAnnualized <- function(data) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }

  # Check if data has all the expected columns
  expected_columns <- c(
    "cohortCount",
    "personYears",
    "calendarYear",
    "incidenceRate",
    "cohortId",
    "databaseId"
  )
  if (!all(expected_columns %in% names(data))) {
    stop(paste(
      "Input data must contain the following columns:",
      paste(expected_columns, collapse = ", ")
    ))
  }

  # Check data types
  if (!is.numeric(data$cohortCount) |
    !is.numeric(data$personYears) |
    !is.numeric(data$incidenceRate) |
    !is.numeric(data$cohortId) |
    !is.character(data$calendarYear) |
    !is.character(data$databaseId)) {
    stop("Data types of some columns are not as expected.")
  }

  if (data |>
    dplyr::select("databaseId", "calendarYear", "cohortId") |>
    nrow() != data |>
    dplyr::select("databaseId", "calendarYear", "cohortId") |>
    dplyr::distinct() |>
    nrow()) {
    stop("data is not unique by databaseId, calendarYear, cohortId")
  }
}


#' Process Crude Incidence Rate Data from Cohort Diagnostics
#'
#' This function processes the cohort diagnostics incidence rate data. It filters the data based on certain conditions, calculates the incidence rate, and imputes missing years.
#'
#' @param annualizedCrudeIncidenceRateData A data frame that contains the columns cohortId, databaseId, calendarYear, cohortCount, personYears, and incidenceRate.
#' @param removeOutlierZeroCounts (Default TRUE) Do you want to remove continuous zero counts in the beginning and end in the time series?
#' @return A data frame with processed cohort diagnostics incidence rate data.
processannualizedCrudeIncidenceRateData <- function(annualizedCrudeIncidenceRateData,
                                                    removeOutlierZeroCounts = TRUE) {
  outputData <- annualizedCrudeIncidenceRateData |>
    dplyr::filter(.data$personYears > 0) |> # remove any records with 0 or lower (below min cell count for privacy protection) person years
    dplyr::mutate(
      # to adjust for min cell count privacy protection output in CohortDiagnostics
      cohortCount = ifelse(.data$cohortCount < 0, 1, .data$cohortCount),
      personYears = ifelse(.data$personYears < 0, 1, .data$personYears),
      calendarYear = as.integer(.data$calendarYear)
    ) |>
    dplyr::mutate(incidenceRate = (.data$cohortCount / .data$personYears) * 1000) |>
    dplyr::select(
      "cohortId",
      "databaseId",
      "calendarYear",
      "cohortCount",
      "personYears",
      "incidenceRate"
    ) |>
    dplyr::mutate(calendarYear = as.Date(paste0(.data$calendarYear, "-06-01"))) |> # mid year
    dplyr::arrange(.data$cohortId, .data$databaseId, .data$calendarYear) |>
    dplyr::mutate(
      zeroRecordLeadRemoved = 0,
      zeroRecordTrailRemoved = 0
    )

  if (removeOutlierZeroCounts) {
    outputDataFiltered <- outputData |>
      dplyr::group_by(.data$cohortId, .data$databaseId) |>
      dplyr::arrange(.data$calendarYear) |>
      dplyr::mutate(
        # Identify leading and trailing zeros
        isZero = .data$incidenceRate == 0,
        leadZeros = cumsum(!.data$isZero) == 0,
        # Cumulative sum for leading zeros
        trailZeros = rev(cumsum(rev(!.data$isZero))) == 0 # Reverse cumulative sum for trailing zeros
      ) |>
      dplyr::mutate(
        zeroRecordLeadRemoved = sum(.data$leadZeros),
        # Count lead zeros removed
        zeroRecordTrailRemoved = sum(.data$trailZeros) # Count trail zeros removed
      ) |>
      dplyr::filter(!(.data$leadZeros |
        .data$trailZeros)) |> # Remove rows that are leading or trailing zeros
      dplyr::select(-"isZero", -"leadZeros", -"trailZeros") |> # Clean up extra columns
      dplyr::ungroup()
  }

  yearsWithNoRecords <- outputData |>
    dplyr::select("cohortId", "databaseId", "calendarYear") |>
    dplyr::distinct() |>
    dplyr::left_join(
      outputDataFiltered |>
        dplyr::select("cohortId", "databaseId", "calendarYear") |>
        dplyr::mutate(keepYear = 1)
    ) |>
    dplyr::filter(is.na(.data$keepYear)) |>
    dplyr::mutate(year = as.integer(format(.data$calendarYear, "%Y"))) |>
    dplyr::select("cohortId", "databaseId", "year") |>
    dplyr::arrange(.data$cohortId, .data$databaseId, .data$year) |>
    dplyr::group_by(.data$cohortId, .data$databaseId) |>
    dplyr::summarise(
      yearsWithNoCohortRecords = as.character(paste0(.data$year, collapse = ", ")),
      .groups = "keep"
    ) |>
    dplyr::ungroup()

  outputData <- outputDataFiltered |>
    dplyr::left_join(yearsWithNoRecords, by = c("cohortId", "databaseId"))

  combos <- outputData |>
    dplyr::select("cohortId", "databaseId") |>
    dplyr::distinct()

  outputDataByCombo <- c()
  for (i in (1:nrow(combos))) {
    outputDataByCombo[[i]] <- outputData |>
      dplyr::inner_join(combos[i, ], by = c("cohortId", "databaseId"))

    if (all(
      outputDataByCombo[[i]]$zeroRecordLeadRemoved == 0,
      outputDataByCombo[[i]]$zeroRecordTrailRemoved == 0
    )) {
      outputDataByCombo[[i]] <- checkFirstLastYearPersonYearStability(data = outputDataByCombo[[i]])
    } else {
      outputDataByCombo[[i]] <- outputDataByCombo[[i]] |>
        dplyr::mutate(useYearData = as.integer(1))
    }
  }

  outputData <- dplyr::bind_rows(outputDataByCombo) |>
    dplyr::arrange(.data$cohortId, .data$databaseId, .data$calendarYear)


  yearsWithUnstablePersonYears <- outputData |>
    dplyr::filter(.data$useYearData == 0) |>
    dplyr::select("cohortId", "databaseId", "calendarYear") |>
    dplyr::distinct() |>
    dplyr::arrange(.data$cohortId, .data$databaseId, .data$calendarYear) |>
    dplyr::group_by(.data$cohortId, .data$databaseId) |>
    dplyr::mutate(year = as.integer(format(.data$calendarYear, "%Y"))) |>
    dplyr::summarise(
      yearsWithUnstablePersonYears = as.character(paste0(.data$year, collapse = ", ")),
      .groups = "keep"
    ) |>
    dplyr::ungroup()

  outputData <- outputData |>
    dplyr::left_join(yearsWithUnstablePersonYears,
      by = c("cohortId", "databaseId")
    )


  return(outputData)
}


# Define a helper function to check stability for each group
checkFirstLastYearPersonYearStability <- function(data) {
  # Exclude first and last calendarYear for model training
  trainData <- data |>
    dplyr::filter(
      .data$calendarYear != max(.data$calendarYear),
      .data$calendarYear != min(.data$calendarYear)
    )

  # Fit a linear model using trainData
  if (nrow(trainData) > 1) {
    # Ensure there are enough points to fit a model
    model <- stats::lm(.data$personYears ~ .data$calendarYear, data = trainData)

    # Calculate the residual standard error and significance threshold
    residualStandardError <- summary(model)$sigma

    # keep a very high significance threshold of 2 SE
    significanceThreshold <- 2 * residualStandardError

    # Predict the expected personYears for the last calendar year
    lastYear <- max(data$calendarYear)
    predictedPersonYearsLast <- stats::predict(model, newdata = data.frame(calendarYear = lastYear))
    actualPersonYearsLast <- data |>
      dplyr::filter(.data$calendarYear == lastYear) |>
      dplyr::pull(.data$personYears)
    differenceActualToLast <- actualPersonYearsLast - predictedPersonYearsLast
    stableLast <- abs(differenceActualToLast) > significanceThreshold

    # Predict the expected personYears for the first calendar year
    firstYear <- min(data$calendarYear)
    predictedPersonYearsFirst <- stats::predict(model, newdata = data.frame(calendarYear = firstYear))
    actualPersonYearsFirst <- data |>
      dplyr::filter(.data$calendarYear == firstYear) |>
      dplyr::pull(.data$personYears)
    differenceActualToFirst <- actualPersonYearsFirst - predictedPersonYearsFirst
    stableFirst <- abs(differenceActualToFirst) > significanceThreshold

    outputTestData <- dplyr::tibble(
      calendarYear = c(lastYear, firstYear),
      useYearData = c(stableLast |> as.integer(), stableFirst |> as.integer())
    )

    data <- data |>
      dplyr::left_join(outputTestData, by = c("calendarYear")) |>
      tidyr::replace_na(list(useYearData = 1))

    # Return the stability result and difference
    return(data)
  } else {
    return(data |>
      dplyr::mutate(useYearData = as.integer(NA)))
  }
}
