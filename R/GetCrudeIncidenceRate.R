# Function to calculate crude annualized incidence rate that was originally developed for CohortDiagnostics
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
           cohortTableName) {
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
        yearRange$startYear, yearRange$endYear,
        by = 1
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
        cohort_table = cohortTableName,
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
