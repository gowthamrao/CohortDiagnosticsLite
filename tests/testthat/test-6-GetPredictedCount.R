test_that("getPredictedCount works as expected", {
  
  # Sample test data
  test_data <- data.frame(
    timeSequenceField = 1:10,
    countField = c(10, 12, 14, 13, 11, 15, 16, 14, 12, 11),
    personTimeField = rep(1000, 10) # Simulated person time
  )
  
  # Test case 1: Basic functionality
  result <- getPredictedCount(test_data,
                              timeSequenceField = "timeSequenceField",
                              countField = "countField",
                              personTimeField = "personTimeField")
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Test case 2: Verify columns in the result
  expected_columns <- c("observed", "cyclopsExpected", "cyclopsRatio", "cyclopsPValue", "cyclopsStable")
  expect_true(all(expected_columns %in% colnames(result)))
  
  # Test case 3: Test with different maxNumberOfSplines
  result_splines <- getPredictedCount(test_data,
                                      timeSequenceField = "timeSequenceField",
                                      countField = "countField",
                                      personTimeField = "personTimeField",
                                      maxNumberOfSplines = 5)
  expect_true(result_splines$numberOfSplinesUsed <= 5)
  
  # Test case 4: Test the imputation of missing counts
  test_data_missing <- test_data
  test_data_missing$countField[5] <- NA
  result_missing <- getPredictedCount(test_data_missing,
                                      timeSequenceField = "timeSequenceField",
                                      countField = "countField",
                                      personTimeField = "personTimeField")
  expect_true(sum(is.na(result_missing$observed)) > 0) # Check if missing values in observed
  expect_true(all(!is.na(result_missing$cyclopsExpected))) # Ensure expected values were imputed
  
  # Test case 5: Test edge case with duplicate timeSequenceField (should throw an error)
  test_data_dup <- test_data
  test_data_dup$timeSequenceField[2] <- 1
  expect_error(
    getPredictedCount(test_data_dup,
                      timeSequenceField = "timeSequenceField",
                      countField = "countField",
                      personTimeField = "personTimeField"),
    "Cant have more than one record per timeSequenceField"
  )
  
  # Test case 6: Test case with all zero counts
  test_data_zero <- data.frame(
    timeSequenceField = 1:10,
    countField = rep(0, 10),
    personTimeField = rep(1000, 10)
  )
  result_zero <- getPredictedCount(test_data_zero,
                                   timeSequenceField = "timeSequenceField",
                                   countField = "countField",
                                   personTimeField = "personTimeField")
  expect_s3_class(result_zero, "data.frame")
  expect_true(all(result_zero$observed == 0)) # Ensure observed counts are zero
  expect_true(all(result_zero$cyclopsExpected > 0)) # Check if expected counts are positive
  
  # Test case 7: Check for predicted counts using GLM model
  result_glm <- getPredictedCount(test_data,
                                  timeSequenceField = "timeSequenceField",
                                  countField = "countField",
                                  personTimeField = "personTimeField")
  
  expect_s3_class(result_glm, "data.frame")
  expect_true(all(c("glmExpected", "glmPValue", "glmStable") %in% colnames(result_glm))) # GLM columns exist
  
  # Test case 8: Handling of empty data
  test_data_empty <- data.frame(timeSequenceField = integer(0),
                                countField = integer(0),
                                personTimeField = numeric(0))
  
  result_empty <- getPredictedCount(test_data_empty,
                                    timeSequenceField = "timeSequenceField",
                                    countField = "countField",
                                    personTimeField = "personTimeField")
  expect_equal(nrow(result_empty), 0) # Check if the result is empty
  
  # Test case 9: Test different maxRatio and alpha values
  result_custom <- getPredictedCount(test_data,
                                     timeSequenceField = "timeSequenceField",
                                     countField = "countField",
                                     personTimeField = "personTimeField",
                                     maxRatio = 2,
                                     alpha = 0.01)
  
  expect_true(all(result_custom$cyclopsRatio >= 0))
  expect_true(result_custom$cyclopsPValue >= 0 & result_custom$cyclopsPValue <= 1)
  expect_equal(result_custom$cyclopsStable, result_custom$cyclopsPValue > 0.01)
  
})
