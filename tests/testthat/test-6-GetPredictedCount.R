test_that("getPredictedCount works as expected", {
  library(testthat)
  # Sample test data
  test_data <- data.frame(
    timeSequenceField = 1:10,
    countField = c(10, 12, 14, 13, 11, 15, 16, 14, 12, 11),
    personTimeField = rep(1000, 10) # Simulated person time
  )
  
  # Test case 1: Basic functionality
  result <- getPredictedCount(
    test_data,
    timeSequenceField = "timeSequenceField",
    countField = "countField",
    personTimeField = "personTimeField"
  )
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Test case 2: Verify columns in the result
  expected_columns <- c("observed",
                        "cyclopsExpected",
                        "cyclopsRatio",
                        "cyclopsPValue",
                        "cyclopsStable")
  expect_true(all(expected_columns %in% colnames(result)))
  
  # Test case 3: Test with different maxNumberOfSplines
  result_splines <- getPredictedCount(
    test_data,
    timeSequenceField = "timeSequenceField",
    countField = "countField",
    personTimeField = "personTimeField",
    maxNumberOfSplines = 5
  )
  expect_true(all(result_splines$numberOfSplinesUsed <= 5))
  
  # Test case 4: Test the imputation of missing counts
  test_data_missing <- test_data
  test_data_missing$countField[5] <- NA
  expect_error(
    getPredictedCount(
      test_data_missing,
      timeSequenceField = "timeSequenceField",
      countField = "countField",
      personTimeField = "personTimeField"
    )
  )
  
  # Test case 5: Test edge case with duplicate timeSequenceField (should throw an error)
  test_data_dup <- test_data
  test_data_dup$timeSequenceField[2] <- 1
  expect_error(
    getPredictedCount(
      test_data_dup,
      timeSequenceField = "timeSequenceField",
      countField = "countField",
      personTimeField = "personTimeField"
    ),
    "Cant have more than one record per timeSequenceField"
  )
  
  # Test case 6: Test case with all zero counts
  test_data_zero <- data.frame(
    timeSequenceField = 1:10,
    countField = rep(0, 10),
    personTimeField = rep(1000, 10)
  )
  expect_warning(
    getPredictedCount(
      test_data_zero,
      timeSequenceField = "timeSequenceField",
      countField = "countField",
      personTimeField = "personTimeField"
    )
  )
  
  # Test case 7: Check for predicted counts using GLM model
  result_glm <- getPredictedCount(
    test_data,
    timeSequenceField = "timeSequenceField",
    countField = "countField",
    personTimeField = "personTimeField"
  )
  
  expect_s3_class(result_glm, "data.frame")
  expect_true(all(
    c("timeSequenceField", "countField", "personTimeField") %in% colnames(result_glm)
  )) # GLM columns exist
  
  # Test case 8: Handling of empty data
  test_data_empty <- data.frame(
    timeSequenceField = integer(0),
    countField = integer(0),
    personTimeField = numeric(0)
  )
  expect_warning(
    getPredictedCount(
      test_data_empty,
      timeSequenceField = "timeSequenceField",
      countField = "countField",
      personTimeField = "personTimeField"
    )
  )
  
  # Test case 9: Test different maxRatio and alpha values
  result_custom <- getPredictedCount(
    test_data,
    timeSequenceField = "timeSequenceField",
    countField = "countField",
    personTimeField = "personTimeField",
    maxRatio = 2,
    alpha = 0.01
  )
  
  expect_true(all(result_custom$cyclopsRatio >= 0))
  expect_true(all(
    result_custom$cyclopsPValue >= 0 &
      result_custom$cyclopsPValue <= 1
  ))
  expect_equal(result_custom$cyclopsStable,
               result_custom$cyclopsPValue > 0.01)
  
  
  # Test case 10: personTimeField is NA
  # Sample test data
  test_data <- data.frame(
    timeSequenceField = 1:10,
    countField = c(10, 12, 14, 13, 11, 15, 16, 14, 12, 11),
    personTimeField = rep(1000, 10) # Simulated person time
  )
  test_data[5, ]$personTimeField <- NA
  expect_error(
    getPredictedCount(
      test_data,
      timeSequenceField = "timeSequenceField",
      countField = "countField",
      personTimeField = "personTimeField"
    )
  )
  
  # Test case 11: send 0 row data
  # Sample test data
  expect_warning(
    getPredictedCount(
      test_data[0, ],
      timeSequenceField = "timeSequenceField",
      countField = "countField",
      personTimeField = "personTimeField"
    )
  )
  
})




# Test cases for getPredictedCount function
test_that("Poisson GLM model is correctly fitted", {
  # Define a simple test dataset
  test_data <- tibble(
    timeId = 1:10,
    # Simulating 10 different time points
    observed = c(5, 8, 12, 15, 13, 7, 10, 9, 8, 6),
    # Observed counts at each time point
    personTime = c(10, 12, 15, 16, 17, 14, 13, 12, 10, 9) # Person-time (offset) for Poisson model
  )
  
  # Run the function
  result <- getPredictedCount(
    data = test_data,
    timeSequenceField = "timeId",
    countField = "observed",
    personTimeField = "personTime",
    maxNumberOfSplines = 3,
    splineTickInterval = 2,
    alpha = 0.05,
    maxRatio = 1.25
  )
  
  # Check if the result contains necessary fields
  expect_true("glmExpected" %in% names(result))
  expect_true("glmExpectedLowerBound" %in% names(result))
  expect_true("glmExpectedUpperBound" %in% names(result))
  
  # Check if glm model ran and filled values correctly
  expect_false(any(is.na(result$glmExpected))) # No NA values in predictions
  expect_false(any(is.na(result$glmExpectedLowerBound)))
  expect_false(any(is.na(result$glmExpectedUpperBound)))
  
  # Check if the glm deviance p-value is computed
  expect_true("glmPValueDeviance" %in% names(result))
  expect_false(any(is.na(result$glmPValueDeviance)))
  
  # Check the structure of output data
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10) # Should match the input data size
})

test_that("Poisson GLM model handles edge cases", {
  test_data <- tibble(
    timeId = 1:10,
    # Simulating 10 different time points
    observed = c(5, 8, 12, 15, 13, 7, 10, 9, 8, 6),
    # Observed counts at each time point
    personTime = c(10, 12, 15, 16, 17, 14, 13, 12, 10, 9) # Person-time (offset) for Poisson model
  )
  
  # Create data where observed counts are all zeros
  zero_data <- test_data |> dplyr::mutate(observed = 0)
  
  # Check if the function returns NULL for all zero counts
  expect_warning(
    getPredictedCount(
      data = zero_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    ),
    "No prediction. Input has 0 rows."
  )
  
  expect_null(suppressWarnings(
    getPredictedCount(
      data = zero_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    )
  )) # Result should be NULL when all observed counts are 0
  
  # Test with empty data
  empty_data <- test_data |> dplyr::slice(0)
  
  expect_warning(
    getPredictedCount(
      data = empty_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    ),
    "No prediction. Input has 0 rows."
  )
  
  expect_null(suppressWarnings(
    getPredictedCount(
      data = empty_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    )
  )) # Result should be NULL when input data is empty
})

test_that("getPredictedCount throws errors for invalid inputs", {
  # Test for duplicate records in timeSequenceField
  duplicate_data <- bind_rows(test_data, test_data[1, ]) # Duplicate the first row
  
  expect_error(
    getPredictedCount(
      data = duplicate_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    ),
    "Cant have more than one record per timeId"
  )
  
  # Test for NA values in observed count field
  na_data <- test_data |> mutate(observed = c(NA, observed[-1]))
  
  expect_error(
    getPredictedCount(
      data = na_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    ),
    "countField = observed has NA values."
  )
  
  # Test for NA values in personTime field
  na_person_time_data <- test_data |> mutate(personTime = c(NA, personTime[-1]))
  
  expect_error(
    getPredictedCount(
      data = na_person_time_data,
      timeSequenceField = "timeId",
      countField = "observed",
      personTimeField = "personTime"
    ),
    "personTimeField = personTime has NA values."
  )
})
