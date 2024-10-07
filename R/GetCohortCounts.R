#' Retrieve cohort counts from a cohort table.
#'
#' This function retrieves the count of cohort entries and distinct subjects for the specified cohort IDs
#' from the given cohort table in a database schema.
#'
#' @param cohortIds A vector of cohort IDs to filter the cohort table for inclusion.
#' @template ConnectionDetails
#' @template Connection
#' @template CohortDatabaseSchema
#' @template CohortTable
#' @template TempEmulationSchema
#' @export
getCohortCounts <- function(connectionDetails = NULL,
                            connection = NULL,
                            cohortDatabaseSchema,
                            cohortTable,
                            cohortIds = NULL,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(
      expr = DatabaseConnector::dropEmulatedTempTables(connection = connection, tempEmulationSchema = tempEmulationSchema)
    )
    on.exit(expr = DatabaseConnector::disconnect(connection), add = TRUE)
  }

  sql <- "SELECT
            cohort_definition_id AS cohort_id,
            COUNT(*) AS cohort_entries,
            COUNT(DISTINCT subject_id) AS cohort_subjects
          FROM @cohort_database_schema.@cohort_table
          {@cohort_ids != ''} ? {WHERE cohort_definition_id IN (@cohort_ids)}
          GROUP BY cohort_definition_id;"

  cohortCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    tempEmulationSchema = tempEmulationSchema,
    cohort_database_schema = cohortDatabaseSchema,
    snakeCaseToCamelCase = TRUE,
    cohort_ids = cohortIds,
    cohort_table = cohortTable
  ) |>
    dplyr::tibble()

  return(cohortCount)
}
