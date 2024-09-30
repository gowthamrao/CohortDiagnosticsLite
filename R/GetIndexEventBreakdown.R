#' Get Index Event Breakdown
#'
#' This function retrieves a breakdown of events (e.g., visits, procedures, drugs, observations, conditions, measurements) 
#' associated with the index event (on first cohort start date of a subject) for a given set of cohorts.
#'
#' For each event type (visit, procedure, drug exposure, observation, condition, and measurement), both standard
#' concept IDs and source concept IDs are retrieved, ensuring no duplication.
#'
#' @param cohortIds A vector of cohort IDs to filter the cohort table for inclusion.
#' @template ConnectionDetails
#' @template Connection
#' @template CdmDatabaseSchema
#' @template CohortDatabaseSchema
#' @template CohortTable
#' @template TempEmulationSchema
#'
#' @return A tibble containing the breakdown of counts for various clinical events. The tibble consists of:
#' \item{conceptId}{The concept ID of the event (e.g., visit, procedure, drug).}
#' \item{persons}{The number of distinct persons having the event.}
#' \item{records}{The total number of records of the event.}
#' \item{source}{The source of the event count, e.g., 'v1' for visit, 'v2' for visit source, 'p1' for procedure, etc.}
#'
#' @details
#' This function processes clinical event data for cohorts, such as visits, procedures, drugs, observations, conditions, and 
#' measurements, based on the index date (cohort start date). It counts the number of persons and records for each event and 
#' combines them into a single tibble, distinguishing between standard and source concepts.
#' 
#' SQL queries are translated and executed using the `DatabaseConnector` package. Temporary tables are dropped after processing.
#'
#' @export
getIndexEventBreakdown <- function(cohortIds,
                                   connectionDetails = NULL,
                                   connection = NULL,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " --HINT SORT_ON_KEY(INTERLEAVED:cohort_start_date)
            --HINT DISTRIBUTE_ON_KEY(subject_id)
            SELECT subject_id,
          		min(cohort_start_date) cohort_start_date
          	INTO #cohort_index
          	FROM @cohort_database_schema.@cohort_table
          	WHERE cohort_definition_id IN (@cohort_ids)
          	GROUP BY subject_id;",
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_ids = cohortIds,
    tempEmulationSchema = tempEmulationSchema
  )

  visitCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = " SELECT visit_concept_id concept_id,
            	COUNT(DISTINCT person_id) persons,
            	COUNT(*) records
            FROM @cdm_database_schema.visit_occurrence dt
            INNER JOIN #cohort_index ct
            ON dt.person_id = ct.subject_id
            	AND dt.visit_start_date = ct.cohort_start_date
            WHERE visit_concept_id > 0
            GROUP BY visit_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  visitSourceCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = " SELECT visit_source_concept_id concept_id,
            	COUNT(DISTINCT person_id) persons,
            	COUNT(*) records
            FROM @cdm_database_schema.visit_occurrence dt
            INNER JOIN #cohort_index ct
            ON dt.person_id = ct.subject_id
            	AND dt.visit_start_date = ct.cohort_start_date
            WHERE visit_concept_id != visit_source_concept_id
              AND visit_source_concept_id > 0
            GROUP BY visit_source_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  procedureCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = " SELECT procedure_concept_id concept_id,
            	COUNT(DISTINCT person_id) persons,
              COUNT(*) records
            FROM @cdm_database_schema.procedure_occurrence dt
            INNER JOIN #cohort_index ct
            ON dt.person_id = ct.subject_id
            	AND dt.procedure_date = ct.cohort_start_date
            WHERE procedure_concept_id > 0
            GROUP BY procedure_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  procedureSourceCount <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT procedure_source_concept_id concept_id,
              	COUNT(DISTINCT person_id) persons,
                COUNT(*) records
              FROM @cdm_database_schema.procedure_occurrence dt
              INNER JOIN #cohort_index ct
              ON dt.person_id = ct.subject_id
              	AND dt.procedure_date = ct.cohort_start_date
              WHERE procedure_source_concept_id != procedure_concept_id
                AND procedure_source_concept_id > 0
              GROUP BY procedure_source_concept_id;",
      cdm_database_schema = cdmDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  drugExposureCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT drug_concept_id concept_id,
            	COUNT(DISTINCT person_id) persons,
            	COUNT(*) records
            FROM @cdm_database_schema.drug_exposure dt
            INNER JOIN #cohort_index ct
           	ON dt.person_id = ct.subject_id
           		AND dt.drug_exposure_start_date = ct.cohort_start_date
            WHERE drug_concept_id > 0
            GROUP BY drug_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  drugExposureSourceCount <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT drug_source_concept_id source_concept_id,
              	COUNT(DISTINCT person_id) persons,
              	COUNT(*) records
              FROM @cdm_database_schema.drug_exposure dt
              INNER JOIN #cohort_index ct
             	ON dt.person_id = ct.subject_id
             		AND dt.drug_exposure_start_date = ct.cohort_start_date
              WHERE drug_source_concept_id != drug_concept_id
                AND drug_source_concept_id > 0
              GROUP BY drug_source_concept_id;",
      cdm_database_schema = cdmDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  observationCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT observation_concept_id concept_id,
            	COUNT(DISTINCT person_id) persons,
            	COUNT(*) records
            FROM @cdm_database_schema.observation dt
            INNER JOIN #cohort_index ct
          	ON dt.person_id = ct.subject_id
          		AND dt.observation_date = ct.cohort_start_date
            WHERE observation_concept_id > 0
            GROUP BY observation_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  observationSourceCount <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT observation_source_concept_id source_concept_id,
              	COUNT(DISTINCT person_id) persons,
              	COUNT(*) records
              FROM @cdm_database_schema.observation dt
              INNER JOIN #cohort_index ct
            	ON dt.person_id = ct.subject_id
            		AND dt.observation_date = ct.cohort_start_date
              WHERE observation_source_concept_id != observation_concept_id
                AND observation_source_concept_id > 0
              GROUP BY observation_source_concept_id;",
      cdm_database_schema = cdmDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  conditionCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT condition_concept_id concept_id,
          	COUNT(DISTINCT person_id) persons,
          	COUNT(*) records
          FROM @cdm_database_schema.condition_occurrence dt
          INNER JOIN #cohort_index ct
        	ON dt.person_id = ct.subject_id
        		AND dt.condition_start_date = ct.cohort_start_date
          WHERE condition_concept_id > 0
          GROUP BY condition_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  conditionSourceCount <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT condition_source_concept_id source_concept_id,
              	COUNT(DISTINCT person_id) persons,
              	COUNT(*) records
              FROM @cdm_database_schema.condition_occurrence dt
              INNER JOIN #cohort_index ct
             	ON dt.person_id = ct.subject_id
             		AND dt.condition_start_date = ct.cohort_start_date
              WHERE condition_source_concept_id != condition_concept_id
                AND condition_source_concept_id > 0
              GROUP BY condition_source_concept_id;",
      cdm_database_schema = cdmDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  measurementCount <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT measurement_concept_id concept_id,
            	COUNT(DISTINCT person_id) persons,
            	COUNT(*) records
            FROM @cdm_database_schema.measurement dt
            INNER JOIN #cohort_index ct
           	ON dt.person_id = ct.subject_id
           		AND dt.measurement_date = ct.cohort_start_date
            WHERE measurement_concept_id > 0
            GROUP BY measurement_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()

  measurementSourceCount <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT measurement_source_concept_id source_concept_id,
            	COUNT(DISTINCT person_id) persons,
            	COUNT(*) records
            FROM @cdm_database_schema.measurement dt
            INNER JOIN #cohort_index ct
          	ON dt.person_id = ct.subject_id
          		AND dt.measurement_date = ct.cohort_start_date
            WHERE measurement_source_concept_id != measurement_concept_id
              AND measurement_source_concept_id > 0
            GROUP BY measurement_source_concept_id;",
      cdm_database_schema = cdmDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  DatabaseConnector::renderTranslateExecuteSql(connection = connection, sql = " DROP TABLE IF EXISTS #cohort_index;")

  output <- dplyr::bind_rows(
    visitCount |> dplyr::mutate(source = "v1"),
    visitSourceCount |>
      dplyr::anti_join(
        visitCount |>
          dplyr::select("conceptId") |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(source = "v2"),
    procedureCount |> dplyr::mutate(source = "p1"),
    procedureSourceCount |>
      dplyr::anti_join(
        procedureCount |>
          dplyr::select("conceptId") |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(source = "p2"),
    drugExposureCount |> dplyr::mutate(source = "d1"),
    drugExposureSourceCount |>
      dplyr::anti_join(
        drugExposureCount |>
          dplyr::select("conceptId") |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(source = "d2"),
    observationCount |> dplyr::mutate(source = "o1"),
    observationSourceCount |>
      dplyr::anti_join(
        observationCount |>
          dplyr::select("conceptId") |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(source = "o2"),
    conditionCount |> dplyr::mutate(source = "c1"),
    conditionSourceCount |>
      dplyr::anti_join(
        conditionCount |>
          dplyr::select("conceptId") |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(source = "c2"),
    measurementCount |> dplyr::mutate(source = "m1"),
    measurementSourceCount |>
      dplyr::anti_join(
        measurementCount |>
          dplyr::select("conceptId") |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(source = "m2")
  )

  return(output)
}
