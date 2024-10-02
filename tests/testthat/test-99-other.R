library(testthat)

# Test cases for the formatCountPercent function
test_that("formatCountPercent formats correctly with default percent digits", {
  expect_equal(formatCountPercent(1000, 0.1234), "1,000 (12.3%)")
  expect_equal(formatCountPercent(123456, 0.98765), "123,456 (98.8%)")
})

test_that("formatCountPercent formats correctly with custom percent digits", {
  expect_equal(
    formatCountPercent(1000, 0.1234, percentDigits = 2),
    "1,000 (12.34%)"
  )
  expect_equal(
    formatCountPercent(123456, 0.98765, percentDigits = 3),
    "123,456 (98.765%)"
  )
})

test_that("formatIntegerWithComma works correctly", {
  expect_equal(formatIntegerWithComma(1000), "1,000")
  expect_equal(formatIntegerWithComma(123456789), "123,456,789")
  expect_equal(formatIntegerWithComma(0), "0")
})

test_that("formatPercent works correctly with different digits", {
  expect_equal(formatPercent(0.1234), "12.34%")
  expect_equal(formatPercent(0.1234, digits = 1), "12.3%")
  expect_equal(formatPercent(0.98765, digits = 3), "98.765%")
})

test_that("formatPercent works correctly with edge cases", {
  expect_equal(formatPercent(0), "0.00%")
  expect_equal(formatPercent(1), "100.00%")
  expect_equal(formatPercent(0.9999, digits = 2), "99.99%")
})



# test_updateProgress.R

library(testthat)

# Function to capture printed output for testing
capture_output <- function(expr) {
  output <- capture.output(expr)
  paste(output, collapse = "\n")
}

# Test cases for updateProgress function
test_that("updateProgress prints progress message correctly", {
  # Test with a short message
  output <- capture_output(updateProgress("Progress at 50%", maxLength = 20))
  expect_equal(output, "\rProgress at 50%     ") # Padded to 20 characters

  # Test with a long message
  longMessage <- "This is a very long progress message that should be truncated or padded"
  output <- capture_output(updateProgress(longMessage, maxLength = 50))
  expectedOutput <- "\rThis is a very long progress message that should b"
  expect_equal(output, expectedOutput)

  # Test with a maxLength greater than the message length
  output <- capture_output(updateProgress("Loading...", maxLength = 30))
  expect_equal(output, "\rLoading...                    ") # Padded to 30 characters
})

# Edge cases
test_that("updateProgress handles edge cases correctly", {
  # Test with an empty message
  output <- capture_output(updateProgress("", maxLength = 10))
  expect_equal(output, "\r          ") # Padded with spaces

  # Test with maxLength shorter than message
  output <- capture_output(updateProgress("Short message", maxLength = 5))
  expect_equal(substr(output, 1, 5), "\rShor") # Message is truncated to 5 characters

  # Test with message exactly equal to maxLength
  output <- capture_output(updateProgress("Exact length", maxLength = 12))
  expect_equal(output, "\rExact length")
})
