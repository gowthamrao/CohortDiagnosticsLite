#' Creates a temporary table with calendar year cohorts from the observation period.
#'
#' This function generates a calendar year cohort based on the observation period
#' for each person in the database. The cohort start date is anchored on a specified day
#' and month, and the resulting cohort is stored in a temporary table.
#'
#' @param connection The connection object to the database.
#' @param cdmDatabaseSchema The schema name where the CDM (Common Data Model) tables are located.
#' @param tempEmulationSchema Schema to emulate temp tables in a non-temp schema. Defaults to the value of the `sqlRenderTempEmulationSchema` option.
#' @param tempCohortTableName The name of the temporary cohort table to be created. Default is `#calendar_year_cohort`.
#' @param anchorDay The day of every month to anchor the cohort start date. Default is 1.
#' @param anchorMonth The month of the year to anchor the cohort start date. Default is 1.
#'
#' @return NULL The function does not return a value but creates a temporary cohort table in the database.
#'
#' @details
#' This function constructs SQL to create a calendar year cohort table. The cohort table will
#' be based on the observation period and anchored to the specified `anchorDay` and `anchorMonth`.
#' It uses the CDM database schema and translates the SQL for the target database platform.
#'
getCalendarYearCohortFromObservationPeriod <- function(connection,
                                                       cdmDatabaseSchema,
                                                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                                       tempCohortTableName = "#calendar_year_cohort",
                                                       anchorDay = 1,
                                                       anchorMonth = 1) {
  if (is.null(connection)) {
    stop("Please provide connection.")
  }

  sql <-
    SqlRender::loadRenderTranslateSql(
      sqlFilename = "CalendarYearCohort.sql",
      packageName = utils::packageName(),
      dbms = connection@dbms,
      tempEmulationSchema = tempEmulationSchema,
      cohort_table_name = tempCohortTableName,
      cdm_database_schema = cdmDatabaseSchema,
      calendar_day = anchorDay,
      calendar_month = anchorMonth
    )

  DatabaseConnector::executeSql(connection,
    sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}
