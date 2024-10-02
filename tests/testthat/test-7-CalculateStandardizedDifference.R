library(testthat)

# Mock data for testing
targetProportion <- data.frame(id = 1:3, targetMean = c(0.2, 0.5, 0.8))

comparatorProportion <- data.frame(id = 1:3, comparatorMean = c(0.3, 0.6, 0.7))

# Test cases for calculateStandardizedDifference
test_that("Standardized difference calculation works", {
  result <- calculateStandardizedDifference(targetProportion, comparatorProportion)

  # Check result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that result has correct columns
  expect_true(all(
    c("id", "targetMean", "comparatorMean", "stdDiff") %in% colnames(result)
  ))

  # Recalculate the standardized differences manually
  # For row 1:
  targetMean_1 <- 0.2
  comparatorMean_1 <- 0.3
  pooledSd_1 <- sqrt((sqrt(targetMean_1 * (1 - targetMean_1))^2 + sqrt(
    comparatorMean_1 * (1 - comparatorMean_1)
  )^2) / 2)
  stdDiff_1 <- (comparatorMean_1 - targetMean_1) / pooledSd_1

  # For row 2:
  targetMean_2 <- 0.5
  comparatorMean_2 <- 0.6
  pooledSd_2 <- sqrt((sqrt(targetMean_2 * (1 - targetMean_2))^2 + sqrt(
    comparatorMean_2 * (1 - comparatorMean_2)
  )^2) / 2)
  stdDiff_2 <- (comparatorMean_2 - targetMean_2) / pooledSd_2

  # For row 3:
  targetMean_3 <- 0.8
  comparatorMean_3 <- 0.7
  pooledSd_3 <- sqrt((sqrt(targetMean_3 * (1 - targetMean_3))^2 + sqrt(
    comparatorMean_3 * (1 - comparatorMean_3)
  )^2) / 2)
  stdDiff_3 <- (comparatorMean_3 - targetMean_3) / pooledSd_3

  # Check that the standardized difference is calculated correctly
  expect_equal(round(result$stdDiff[1], 2), round(stdDiff_1, 2), tolerance = 0.01)
  expect_equal(round(result$stdDiff[2], 2), round(stdDiff_2, 2), tolerance = 0.01)
  expect_equal(round(result$stdDiff[3], 2), round(stdDiff_3, 2), tolerance = 0.01)
})



test_that("Function throws error when targetMean column is missing", {
  targetProportion_invalid <- data.frame(id = 1:3, someOtherColumn = c(0.2, 0.5, 0.8))

  expect_error(
    calculateStandardizedDifference(targetProportion_invalid, comparatorProportion),
    "targetMean not found in targetProportion"
  )
})

test_that("Function throws error when comparatorMean column is missing", {
  comparatorProportion_invalid <- data.frame(id = 1:3, someOtherColumn = c(0.3, 0.6, 0.7))

  expect_error(
    calculateStandardizedDifference(targetProportion, comparatorProportion_invalid),
    "comparatorMean not found in comparatorProportion"
  )
})

test_that("Standardized difference with no overlapping rows", {
  targetProportion_no_overlap <- data.frame(id = 1:3, targetMean = c(0.2, 0.5, 0.8))

  comparatorProportion_no_overlap <- data.frame(id = 4:6, comparatorMean = c(0.3, 0.6, 0.7))

  result <- calculateStandardizedDifference(
    targetProportion_no_overlap,
    comparatorProportion_no_overlap
  )

  # Check that there are no rows in the result (since no shared ids)
  expect_gte(nrow(result |> dplyr::filter(is.na(stdDiff))), sum(
    nrow(targetProportion_no_overlap),
    nrow(comparatorProportion_no_overlap)
  ))
})

test_that("Handles all shared columns", {
  targetProportion_extended <- data.frame(
    id = 1:3,
    targetMean = c(0.2, 0.5, 0.8),
    anotherSharedColumn = c(10, 20, 30)
  )

  comparatorProportion_extended <- data.frame(
    id = 1:3,
    comparatorMean = c(0.3, 0.6, 0.7),
    anotherSharedColumn = c(10, 20, 30)
  )

  result <- calculateStandardizedDifference(targetProportion_extended, comparatorProportion_extended)

  # Check that anotherSharedColumn is retained in the result
  expect_true("anotherSharedColumn" %in% colnames(result))
  expect_equal(result$anotherSharedColumn, c(10, 20, 30))
})
