test_that("likelihoodComparison works as expected", {
  library(testthat)
  # Test case 1: Basic functionality
  test_data <- data.frame(
    observed = c(5, 10, 15),
    cyclopsExpected = c(6, 12, 18)
  )
  result <- likelihoodComparison(test_data)

  expect_s3_class(result, "data.frame") # Check if the result is a data frame
  expect_true(all(c("ratio", "p", "stable") %in% colnames(result))) # Check if the necessary columns exist

  # Test case 2: Check for expected default ratio and p-value calculation
  expect_true(all(result$ratio > 0)) # Ratio should be positive
  expect_true(all(result$p >= 0 &
    result$p <= 1)) # p-value should be between 0 and 1

  # Test case 3: Verify stability indicator (based on alpha = 0.05 default)
  expect_equal(result$stable, result$p > 0.05)

  # Test case 4: Test with different maxRatio and alpha values
  result_custom <- likelihoodComparison(test_data, maxRatio = 2, alpha = 0.01)
  expect_true(all(result_custom$p >= 0 & result_custom$p <= 1))
  expect_equal(result_custom$stable, result_custom$p > 0.01)

  # Test case 5: Edge case with zero observed values
  test_data_zero <- data.frame(
    observed = c(0, 0, 0),
    cyclopsExpected = c(1, 1, 1)
  )
  result_zero <- likelihoodComparison(test_data_zero)

  expect_s3_class(result_zero, "data.frame")
  expect_true(!is.na(result_zero$ratio)) # Ensure ratio is not NA


  # Test case 6: Edge case with all expected equal to observed
  test_data_equal <- data.frame(
    observed = c(10, 20, 30),
    cyclopsExpected = c(10, 20, 30)
  )
  result_equal <- likelihoodComparison(test_data_equal)
  expect_s3_class(result_equal, "data.frame")
  expect_true(all(result_equal$p >= 0 & result_equal$p <= 1))

  # Test case 7: Handling of empty data
  test_data_empty <- data.frame(observed = numeric(0), cyclopsExpected = numeric(0))
  result_empty <- likelihoodComparison(test_data_empty)
  expect_equal(nrow(result_empty), 0) # Expect an empty data frame
})
