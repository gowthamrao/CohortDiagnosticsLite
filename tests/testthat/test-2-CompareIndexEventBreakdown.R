# test-compareIndexEventBreakdown.R

library(testthat)

# Mock data for testing
indexEventBreakdown <- data.frame(
  cohortDefinitionId = c(18478, 18478, 18490002, 18490002),
  persons = c(100, 200, 150, 250),
  records = c(200, 300, 250, 350),
  conceptId = c(1, 2, 1, 2),
  sourceConcept = c(0, 0, 0, 0),
  domain = c("DomainA", "DomainB", "DomainA", "DomainB"),
  databaseId = c("db1", "db1", "db1", "db1")
)

cohortCount <- data.frame(
  cohortId = c(18478, 18490002),
  cohortSubjects = c(500, 600),
  cohortEntries = c(700, 800),
  databaseId = c("db1", "db1")
)


test_that("Basic functionality works", {
  result <- compareIndexEventBreakdown(
    indexEventBreakdown = indexEventBreakdown,
    cohortCount = cohortCount,
    targetCohortId = 18478,
    comparatorCohortId = 18490002,
    targetDatabaseId = "db1",
    comparatorDatabaseId = "db1"
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("stdDiff") %in% colnames(result)))
  expect_equal(nrow(result), 2) # Two unique conceptId and sourceConcept combinations
})

test_that("Basic functionality works for entries", {
  result <- compareIndexEventBreakdown(
    indexEventBreakdown = indexEventBreakdown,
    cohortCount = cohortCount,
    targetCohortId = 18478,
    comparatorCohortId = 18490002,
    targetDatabaseId = "db1",
    comparatorDatabaseId = "db1",
    compareEntries = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("stdDiff") %in% colnames(result)))
  expect_equal(nrow(result), 2) # Two unique conceptId and sourceConcept combinations
})

test_that("Error handling no db", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown,
      cohortCount = cohortCount,
      targetCohortId = 18478,
      comparatorCohortId = 18490002,
      targetDatabaseId = "dbx",
      comparatorDatabaseId = "db1",
      compareEntries = TRUE
    )
  )
})

test_that("Error handling duplicated", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown |> dplyr::bind_rows(indexEventBreakdown),
      cohortCount = cohortCount,
      targetCohortId = 18478,
      comparatorCohortId = 18490002,
      targetDatabaseId = "db1",
      comparatorDatabaseId = "db1",
      compareEntries = TRUE
    )
  )
})

test_that("Missing columns index event breakdown", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown |> dplyr::select(-databaseId),
      cohortCount = cohortCount,
      targetCohortId = 18478,
      comparatorCohortId = 18490002,
      targetDatabaseId = "db1",
      comparatorDatabaseId = "db1",
      compareEntries = TRUE
    )
  )
})

test_that("Missing cohorts index event breakdown - target", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown,
      cohortCount = cohortCount,
      targetCohortId = 3423,
      comparatorCohortId = 18490002,
      targetDatabaseId = "db1",
      comparatorDatabaseId = "db1",
      compareEntries = TRUE
    )
  )
})

test_that("Missing cohorts index event breakdown - target", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown,
      cohortCount = cohortCount,
      targetCohortId = 18478,
      comparatorCohortId = 3423,
      targetDatabaseId = "db1",
      comparatorDatabaseId = "db1",
      compareEntries = TRUE
    )
  )
})

test_that("Missing databaseId index event breakdown - target", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown,
      cohortCount = cohortCount,
      targetCohortId = 18478,
      comparatorCohortId = 18490002,
      targetDatabaseId = "dbx",
      comparatorDatabaseId = "db1",
      compareEntries = TRUE
    )
  )
})


test_that("Missing databaseId index event breakdown - target", {
  expect_error(
    compareIndexEventBreakdown(
      indexEventBreakdown = indexEventBreakdown,
      cohortCount = cohortCount,
      targetCohortId = 18478,
      comparatorCohortId = 18490002,
      targetDatabaseId = "db1",
      comparatorDatabaseId = "dbx",
      compareEntries = TRUE
    )
  )
})
