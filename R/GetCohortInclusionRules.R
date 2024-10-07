#' Retrieve cohort inclusion rule statistics and results.
#'
#' This function retrieves various inclusion rule statistics and results for the specified cohorts from the
#' cohort tables in a database schema.
#'
#' @param cohortIds A vector of cohort IDs to filter the cohort table for inclusion.
#' @template ConnectionDetails
#' @template Connection
#' @template CohortDatabaseSchema
#' @template CohortTableNames
#' @template TempEmulationSchema
#'
#' @return A list of tibbles containing:
#' \itemize{
#'   \item `cohortCount`: Cohort counts as retrieved by \code{\link{getCohortCounts}}.
#'   \item `inclusion`: Inclusion rules for the cohorts.
#'   \item `inclusionResult`: Results for the inclusion rules.
#'   \item `inclusionStats`: Statistics related to inclusion rule performance.
#'   \item `summaryStats`: Summary statistics for the cohorts.
#' }
#'
#' @details This function collects information about inclusion rules, cohort counts, and statistics
#' from various cohort tables. It automatically handles database connections and temporary table emulation.
#'
#' @examples
#' \dontrun{
#' connectionDetails <- DatabaseConnector::createConnectionDetails(
#'   dbms = "postgresql",
#'   server = "myserver", user = "myuser", password = "mypassword"
#' )
#' cohortTableNames <- list(
#'   cohortTable = "cohort",
#'   cohortInclusionTable = "cohort_inclusion",
#'   cohortInclusionResultTable = "cohort_inclusion_result",
#'   cohortInclusionStatsTable = "cohort_inclusion_stats",
#'   cohortSummaryStatsTable = "cohort_summary_stats"
#' )
#' inclusionRules <- getCohortInclusionRules(
#'   connectionDetails = connectionDetails,
#'   cohortDatabaseSchema = "my_schema",
#'   cohortTableNames = cohortTableNames,
#'   cohortIds = c(1, 2, 3)
#' )
#' }
#'
#' @export
getCohortInclusionRules <- function(connectionDetails = NULL,
                                    connection = NULL,
                                    cohortDatabaseSchema,
                                    cohortTableNames,
                                    cohortIds = NULL,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(
      expr = DatabaseConnector::dropEmulatedTempTables(connection = connection, tempEmulationSchema = tempEmulationSchema)
    )
    on.exit(expr = DatabaseConnector::disconnect(connection), add = TRUE)
  }

  results <- c()

  results$cohortCount <- getCohortCounts(
    connectionDetails = connectionDetails,
    connection = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    cohortIds = cohortIds,
    tempEmulationSchema = tempEmulationSchema
  )
  results$inclusion <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_database_schema.@stat_table WHERE cohort_definition_id IN (@cohort_ids);",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cohort_database_schema = cohortDatabaseSchema,
    stat_table = cohortTableNames$cohortInclusionTable,
    cohort_ids = cohortIds
  ) |> dplyr::tibble()

  results$inclusionResult <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_database_schema.@stat_table WHERE cohort_definition_id IN (@cohort_ids);",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cohort_database_schema = cohortDatabaseSchema,
    stat_table = cohortTableNames$cohortInclusionResultTable,
    cohort_ids = cohortIds
  ) |> dplyr::tibble()

  results$inclusionStats <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_database_schema.@stat_table WHERE cohort_definition_id IN (@cohort_ids);",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cohort_database_schema = cohortDatabaseSchema,
    stat_table = cohortTableNames$cohortInclusionStatsTable,
    cohort_ids = cohortIds
  ) |> dplyr::tibble()

  results$summaryStats <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_database_schema.@stat_table WHERE cohort_definition_id IN (@cohort_ids);",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cohort_database_schema = cohortDatabaseSchema,
    stat_table = cohortTableNames$cohortSummaryStatsTable,
    cohort_ids = cohortIds
  ) |> dplyr::tibble()

  return(results)
}
