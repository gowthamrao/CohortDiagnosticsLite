# test-getCalendarYearCohortFromObservationPeriod.R

library(testthat)

test_that("getCalendarYearCohortFromObservationPeriod works with valid inputs", {
  tempCohortTableName <- "#calendar_year_cohort"
  anchorDay <- 1
  anchorMonth <- 1

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  # Run the function
  expect_silent(
    CohortDiagnosticsLite:::getCalendarYearCohortFromObservationPeriod(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      tempCohortTableName = tempCohortTableName,
      anchorDay = 1,
      anchorMonth =
        1
    )
  )

  summary <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    tempEmulationSchema = tempEmulationSchema,
    sql = "SELECT cohort_definition_id,
                          count(*) records,
                          count(DISTINCT subject_id) persons
                        FROM #calendar_year_cohort
                      GROUP BY cohort_definition_id;",
    snakeCaseToCamelCase = TRUE
  )

  expect_true(is.data.frame(summary))

  if (nrow(summary) > 0) {
    expect_true(sum(summary$records) == sum(summary$persons))
  }

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    tempEmulationSchema = tempEmulationSchema,
    sql = "DROP TABLE IF EXISTS #calendar_year_cohort;"
  )
})

test_that("getCalendarYearCohortFromObservationPeriod throws error if connection is NULL", {
  # Test that the function stops when connection is NULL
  testthat::expect_error(
    getCalendarYearCohortFromObservationPeriod(
      connection = NULL,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      tempCohortTableName = tempCohortTableName,
      anchorDay = 1,
      anchorMonth =
        1
    ),
    "Please provide connection."
  )
})
