library(testthat)
library(ggplot2)

# Test if the function produces a ggplot object
test_that("getPlotPopulationPyramid produces a ggplot object", {
  # Use reference population data for the US as the input data
  data <- getReferencePopulation("US")

  # Generate the plot
  p <- getPlotPopulationPyramid(data)

  # Check if the output is a ggplot object
  expect_s3_class(p, "ggplot")
})

# Test if the function works with different regions
test_that("getPlotPopulationPyramid works with different regions", {
  # Valid regions to test
  valid_regions <- c("US", "UK", "Canada", "Europe", "Japan", "China", "Worldwide")

  for (region in valid_regions) {
    data <- getReferencePopulation(region)

    # Generate the plot
    p <- getPlotPopulationPyramid(data)

    # Check if the output is a ggplot object
    expect_s3_class(p, "ggplot")
  }
})


# Test if the plot correctly flips the coordinates
test_that("getPlotPopulationPyramid flips coordinates", {
  # Use reference population data for the US
  data <- getReferencePopulation("US")

  # Generate the plot
  p <- getPlotPopulationPyramid(data)

  # Check that the coordinates are flipped by inspecting the ggplot object
  expect_true("CoordFlip" %in% class(p$coordinates))
})
