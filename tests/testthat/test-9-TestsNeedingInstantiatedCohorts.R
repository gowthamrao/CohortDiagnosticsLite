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

  ### cohort count
  cohortCount <- getCohortCounts(
    cohortIds = cohortIds,
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    tempEmulationSchema = tempEmulationSchema
  )

  testthat::expect_gt(object = nrow(cohortCount), expected = 0)

  cohortInclusionRule <- getCohortInclusionRules(
    cohortIds = cohortIds,
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    tempEmulationSchema = tempEmulationSchema
  )

  testthat::expect_gt(
    object = length(cohortInclusionRule),
    expected = 0
  )
})
