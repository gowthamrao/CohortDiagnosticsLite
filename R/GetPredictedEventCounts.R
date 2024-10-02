#' Get Predicted Count in a Data Frame
#'
#' This function predicts the count of events in a data frame using Poisson regression models with natural splines. It fits the model using both the `Cyclops` package and base `glm()`.
#' The function models event counts based on the field specified in the `timeSequenceField` parameter and includes an offset for the field specified in `personTimeField` to account for different exposure times. It evaluates model stability through likelihood ratio tests and other diagnostics.
#' The function adds flags to indicate model stability and significance, with separate indicators for the results of both `Cyclops` and `glm()` fits.
#'
#' **Input Data Structure:**
#' The input `data` should be a data frame with columns that are dynamically specified via function parameters.
#' - The `timeSequenceField` parameter represents a column containing the time sequence or index of the events (e.g., date or time intervals).
#' - The `countField` parameter refers to the column representing the observed count of events during each time interval.
#' - The `personTimeField` parameter points to the column representing the exposure time or population-at-risk during the corresponding time interval.
#' The `data` should not contain duplicate rows for the same value in the column specified by `timeSequenceField`, and both the columns for `countField` and `personTimeField` must not contain missing (`NA`) values.
#'
#' @param data A data frame that contains the columns specified in the function parameters.
#' @param timeSequenceField The field in the data frame that represents the time sequence.
#' @param countField The field in the data frame that represents the count of events.
#' @param personTimeField The field in the data frame that represents the person time.
#' @param maxNumberOfSplines The maximum number of splines to use in the model. If NULL, the default is 3.
#' @param splineTickInterval The interval at which to place the splines. The default is 3.
#' @param maxRatio The maximum ratio for the likelihood comparison. If NULL, the default is 1.25.
#' @param alpha The significance level for statistical tests and confidence intervals (default is 0.05).
#' @return A data frame with the observed and predicted counts from both `Cyclops` and `glm()`, along with diagnostics such as p-values, confidence intervals (for GLM), and stability indicators.
#' @export
getPredictedEventCounts <- function(data,
                                    timeSequenceField,
                                    countField,
                                    personTimeField,
                                    maxNumberOfSplines = NULL,
                                    splineTickInterval = 3,
                                    alpha = 0.05,
                                    maxRatio = 1.25) {
  if (nrow(data) == 0) {
    warning("No prediction. Input has 0 rows.")
    return(NULL)
  }

  # Check if there are duplicate records for the same timeSequenceField
  if (length(data[[timeSequenceField]]) != length(data[[timeSequenceField]] |> unique())) {
    stop("Cant have more than one record per ", timeSequenceField)
  }

  # Handle missing values
  if (any(is.na(data[[countField]]))) {
    stop(paste0("countField = ", countField, " has NA values."))
  }
  if (any(is.na(data[[personTimeField]]))) {
    stop(paste0("personTimeField = ", personTimeField, " has NA values."))
  }

  if (all(data[[countField]] == 0)) {
    warning("No prediction. Input has 0 rows.")
    return(NULL)
  }

  # Create a new column 'observed' based on the countField
  data$observed <- data[[countField]]

  # Arrange the data based on timeSequenceField and create a new column 'timeId'.
  # timeSequenceField is the natural interval order in the source data. timeId is the index of the record in the ordered data.
  data <- data |>
    dplyr::arrange(timeSequenceField) |>
    dplyr::mutate(timeId = dplyr::row_number())

  # Set the default value for maxNumberOfSplines if it's NULL
  if (is.null(maxNumberOfSplines)) {
    maxNumberOfSplines <- 3
  }

  numberOfXAxisTicks <- length(unique(data$timeId))

  # Determine the number of splines based on the number of unique timeId values
  numberOfSplines <-
    min(
      maxNumberOfSplines,
      ceiling(numberOfXAxisTicks / splineTickInterval)
    ) # we will put a spline for every 3 ticks

  data$numberOfSplinesUsed <- numberOfSplines

  # Create a Cyclops data object using the observed counts, timeId, and personTimeField
  # The formula used here is a Poisson regression model with natural splines
  offsetTerm <- paste0("offset(log(", personTimeField, "))")

  model <- Cyclops::createCyclopsData(
    formula =
      stats::as.formula(
        paste(
          "observed ~ splines::ns(x = timeId, df =",
          numberOfSplines,
          ") + ",
          offsetTerm
        )
      ),
    data = data,
    modelType = "pr"
  ) |>
    Cyclops::fitCyclopsModel() # Fit the Cyclops model

  # Predict the counts using the fitted model
  predictions <- stats::predict(model, newdata = data)

  # Add the predicted counts to the data
  data$cyclopsExpected <- as.double(predictions)

  data <- data |>
    likelihoodComparison(maxRatio = maxRatio, alpha = alpha) |>
    dplyr::rename(
      cyclopsRatio = "ratio",
      cyclopsPValue = "p",
      cyclopsStable = "stable"
    )

  # We cannot get confidence interval because Cyclops does not provide std errors.
  # to get confidence intervals we will have to use another package other than cyclops like Glm


  # ########
  # # Fit the Poisson regression using glm() with the correct reference to the personTimeField column
  # # Try fitting Poisson regression using glm() and handle errors
  tryCatch(
    {
      # assign default values
      data$glmExpected <- as.double(NA)
      data$glmExpectedLowerBound <- as.double(NA)
      data$glmExpectedUpperBound <- as.double(NA)
      data$glmDevianceValue <- as.double(NA)
      data$glmDegreesOfFreedom <- as.double(NA)
      data$glmPValueDeviance <- as.double(NA)
      data$glmPearsonChiSquare <- as.double(NA)
      data$glmPPValuePearson <- as.double(NA)

      modelGlm <- stats::glm(
        observed ~ splines::ns(timeId, df = numberOfSplines) + offset(log(data[[personTimeField]])),
        data = data,
        family = "poisson"
      )

      glmPredictions <- stats::predict(modelGlm,
        newdata = data,
        type = "link",
        se.fit = TRUE
      )

      glmPredictedLog <- glmPredictions$fit
      seLog <- glmPredictions$se.fit

      zValue <- stats::qnorm(1 - alpha / 2)

      glmLowerBoundLog <- glmPredictedLog - zValue * seLog
      glmUpperBoundLog <- glmPredictedLog + zValue * seLog

      # Exponentiation to get the predicted counts and CIs on the original scale
      data$glmExpected <- as.double(exp(glmPredictedLog))
      data$glmExpectedLowerBound <- as.double(exp(glmLowerBoundLog))
      data$glmExpectedUpperBound <- as.double(exp(glmUpperBoundLog))

      # using Martijn's likelikhood custom function to calculate p-value
      glmLikelihood <- data |>
        dplyr::select("glmExpected", "observed") |>
        dplyr::rename(cyclopsExpected = "glmExpected") |>
        likelihoodComparison(maxRatio = maxRatio, alpha = alpha) |>
        dplyr::rename(
          glmRatio = "ratio",
          glmPValue = "p",
          glmStable = "stable"
        ) |>
        dplyr::select("glmRatio", "glmPValue", "glmStable") |>
        dplyr::distinct()

      data <- data |>
        tidyr::crossing(glmLikelihood)

      # These tests, although valid, give different results from Martijn's likelihood function.


      ## add alpha

      # Deviance Test (G-test)
      # In Poisson regression, the deviance measures the difference between the observed and expected counts under the model.
      # Get the deviance from the fitted model
      data$glmDevianceValue <- modelGlm$deviance
      # Get the degrees of freedom (difference between the number of observations and the number of parameters)
      data$glmDegreesOfFreedom <- modelGlm$df.residual
      # Compute the p-value from the chi-square distribution
      data$glmPValueDeviance <- 1 - stats::pchisq(modelGlm$deviance, modelGlm$df.residual)

      # Pearson Chi-Squared Test - This test sums the squared differences between the observed and expected counts, scaled by the expected counts.
      # Compute Pearson's chi-squared test statistic
      data$glmPearsonChiSquare <- sum((data$observed - data$glmExpected)^
        2 / data$glmExpected)
      # Compute p-value for the Pearson chi-squared test
      data$glmPPValuePearson <- 1 - stats::pchisq(data$glmPearsonChiSquare, modelGlm$df.residual)

      data$glmPearsonStable <- min(data$glmPPValuePearson) > alpha
      data$glmDevianceStable <- min(data$glmPValueDeviance) > alpha
    },
    error = function(...) {
      # If there's an error, skip the glm part
      # message("\nError in glm fitting: ", e$message)
    }
  )

  # report <- data |> dplyr::select(
  #   observed,
  #   cyclopsExpected,
  #   glmExpected,
  #   cyclopsPValue,
  #   glmPValue,
  #   glmExpectedLowerBound,
  #   glmExpectedUpperBound,
  #   glmPPValuePearson,
  #   glmPValueDeviance,
  #   cyclopsStable,
  #   glmStable
  # )

  # Arrange the data based on timeId and select the 'observed' and 'expected' columns
  data <- data |>
    dplyr::arrange(.data$timeId) |>
    dplyr::select(-"timeId") |>
    dplyr::tibble()

  return(data)
}
