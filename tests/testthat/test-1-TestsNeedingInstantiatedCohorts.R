test_that("Invoke cohort generation", {
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "cohort")
  
  # Next create the tables on the database
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortTableNames = cohortTableNames,
    cohortDatabaseSchema = cohortDatabaseSchema,
    incremental = TRUE
  )
  
  # Generate the cohort set
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortDefinitionSet,
    incremental = TRUE
  )
  
  
  ### index event breakdown
  indexEventBreakdown <- CohortDiagnosticsLite::getIndexEventBreakdown(
    cohortIds = cohortIds,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    tempEmulationSchema = tempEmulationSchema
  )
  
  testthat::expect_gt(object = nrow(indexEventBreakdown),
                      expected = 0)
  testthat::expect_equal(
    object = colnames(indexEventBreakdown) |> sort(),
    expected = c(
      'cohortDefinitionId',
      'conceptId',
      'persons',
      'records',
      'source'
    )
  )
  
  ### incidence rate
  crudeIncidenceRateData <- CohortDiagnosticsLite::getCrudeIncidenceRate(
    cohortDefinitionId = cohortIds[[1]],
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    tempEmulationSchema = tempEmulationSchema
  )
  
  testthat::expect_gt(object = nrow(crudeIncidenceRateData),
                      expected = 0)
  testthat::expect_equal(
    object = colnames(crudeIncidenceRateData) |> sort(),
    expected = c(
      'ageGroup',
      'calendarYear',
      'cohortCount',
      'cohortDefinitionId',
      'gender',
      'personYears'
    )
  )
})
