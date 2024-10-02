library(testthat)
library(dplyr)

# Test if the function returns a tibble for each supported region
test_that("getReferencePopulation returns a tibble for valid regions", {
  # Define all valid regions
  valid_regions <- c("US", "UK", "Canada", "Europe", "Japan", "China", "Worldwide")

  for (region in valid_regions) {
    result <- getReferencePopulation(region)

    # Check if the result is a tibble
    expect_s3_class(result, "tbl_df")

    # Check if the tibble has the correct columns
    expect_named(result, c("ageGroup", "gender", "proportion"))

    # Check if the 'ageGroup' and 'gender' columns are character vectors
    expect_type(result$ageGroup, "character")
    expect_type(result$gender, "character")

    # Check if 'proportion' column is numeric
    expect_type(result$proportion, "double")

    # Check if the tibble has 22 rows (11 age groups per gender)
    expect_equal(nrow(result), 22)
  }
})

# Test if an invalid region throws an error
test_that("getReferencePopulation throws error for invalid regions", {
  # Invalid regions
  invalid_regions <- c("Mars", "Jupyter", "Venus")

  for (region in invalid_regions) {
    expect_error(getReferencePopulation(region), "Region not supported.")
  }
})
