#' Calculate Crude Annualized Incidence Rate
#'
#' This function calculates the crude annualized incidence rate for a given cohort in a CDM (Common Data Model) database.
#' It returns the incidence rates based on calendar year for subjects in the specified cohort.
#'
#' @template ConnectionDetails
#' @template Connection
#' @template CdmDatabaseSchema
#' @template CohortDatabaseSchema
#' @param cohortDefinitionId The cohort definition ID for which the incidence rate is to be calculated.
#' @param firstOccurrenceOnly Logical, if TRUE, only the first occurrence of the event per subject is considered.
#' @param washoutPeriod The washout period (in days) before a subject is eligible for their first event (default is 365 days).
#' @template TempEmulationSchema
#' @template CohortTable
#'
#' @return A tibble containing the incidence rates for each calendar year. The tibble consists of:
#' \item{calendarYear}{The calendar year for which the incidence rate is calculated.}
#' \item{incidenceRate}{The calculated crude incidence rate.}
#'
#' @details
#' This function calculates the crude annualized incidence rate using the first occurrence of the event (if specified) and
#' a specified washout period. The incidence rate is computed based on the number of first occurrences within a cohort
#' for each calendar year.
#'
#' SQL queries are executed to gather year range data, compute incidence rates, and return the summary.
#' Temporary tables are used during processing and are dropped after completion.
#'
#' @export
getCrudeIncidenceRate <-
  function(connectionDetails = NULL,
           connection = NULL,
           cohortDatabaseSchema,
           cohortDefinitionId,
           cdmDatabaseSchema,
           firstOccurrenceOnly = TRUE,
           washoutPeriod = 365,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           cohortTable) {
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    sqlCalendar <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetCalendarYearRange.sql",
        packageName = utils::packageName(),
        dbms = connection@dbms,
        cdm_database_schema = cdmDatabaseSchema
      )
    
    yearRange <-
      DatabaseConnector::querySql(connection, sqlCalendar, snakeCaseToCamelCase = TRUE)
    
    calendarYears <-
      dplyr::tibble(calendarYear = as.integer(seq(
        yearRange$startYear, yearRange$endYear, by = 1
      )))
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = "#calendar_years",
      data = calendarYears,
      dropTableIfExists = TRUE,
      createTable = TRUE,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      camelCaseToSnakeCase = TRUE
    )
    
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "ComputeIncidenceRates.sql",
        packageName = utils::packageName(),
        dbms = connection@dbms,
        tempEmulationSchema = tempEmulationSchema,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTable,
        cdm_database_schema = cdmDatabaseSchema,
        vocabulary_database_schema = cdmDatabaseSchema,
        first_occurrence_only = firstOccurrenceOnly,
        washout_period = washoutPeriod,
        cohort_id = cohortDefinitionId
      )
    
    DatabaseConnector::executeSql(connection, sql)
    
    sql <- "SELECT * FROM #rates_summary;"
    ratesSummary <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        tempEmulationSchema = tempEmulationSchema,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()
    
    sql <- "TRUNCATE TABLE #rates_summary; DROP TABLE #rates_summary;"
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
    return(ratesSummary)
  }
