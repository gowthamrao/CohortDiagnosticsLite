test_that("Invoke cohort generation", {
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "cohort")

  # Next create the tables on the database
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortTableNames = cohortTableNames,
    cohortDatabaseSchema = cohortDatabaseSchema,
    incremental = FALSE
  )

  # Generate the cohort set
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortDefinitionSet,
    incremental = FALSE
  )

  ### index event breakdown
  indexEventBreakdown <- CohortDiagnosticsLite::getIndexEventBreakdown(
    cohortIds = cohortIds,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    tempEmulationSchema = tempEmulationSchema,
    databaseId = "eunomia"
  )

  testthat::expect_gte(
    object = nrow(indexEventBreakdown),
    expected = 0
  )

  ### incidence rate
  crudeIncidenceRateData <- getAnnualizedCrudeIncidenceRate(
    cohortDefinitionId = cohortIds[[1]],
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    tempEmulationSchema = tempEmulationSchema,
    databaseId = "db1"
  )

  testthat::expect_gt(
    object = nrow(crudeIncidenceRateData),
    expected = 0
  )
})
