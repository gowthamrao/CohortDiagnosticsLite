
DROP TABLE IF EXISTS #rates_summary;
DROP TABLE IF EXISTS #calendar_years;
DROP TABLE IF EXISTS #numerator;
DROP TABLE IF EXISTS #denominator;

WITH unit1
AS (
	SELECT 0 AS unit1
	
	UNION ALL
	
	SELECT 1
	
	UNION ALL
	
	SELECT 2
	
	UNION ALL
	
	SELECT 3
	
	UNION ALL
	
	SELECT 4
	
	UNION ALL
	
	SELECT 5
	
	UNION ALL
	
	SELECT 6
	
	UNION ALL
	
	SELECT 7
	
	UNION ALL
	
	SELECT 8
	
	UNION ALL
	
	SELECT 9
	)
SELECT calendar_year
INTO #calendar_years
FROM (
	SELECT unit1000 + unit100 + unit10 + unit1 AS calendar_year
	FROM (
		SELECT unit1 * 1000 unit1000
		FROM unit1
		WHERE unit1 IN (
				1,
				2
				)
		) AS unit1000 -- 0 and 1 for years up to 2020
	CROSS JOIN (
		SELECT unit1 * 100 unit100
		FROM unit1
		) AS unit100 -- 0 to 2 for 100s place
	CROSS JOIN (
		SELECT unit1 * 10 unit10
		FROM unit1
		) AS unit10 -- 0 to 9 for decades
	CROSS JOIN unit1 unit1 -- 0 to 9 for single years
	) AS all_years
INNER JOIN (
	SELECT YEAR(MIN(observation_period_start_date)) AS start_year,
		YEAR(MAX(observation_period_end_date)) AS end_year
	FROM @cdm_database_schema.observation_period
	) rest ON calendar_year >= start_year
	AND calendar_year <= end_year;

SELECT YEAR(cohort_start_date) AS calendar_year,
	FLOOR((YEAR(cohort_start_date) - year_of_birth) / 10) AS age_group,
	gender_concept_id,
	COUNT(*) AS cohort_count
INTO #numerator
FROM (
	SELECT subject_id,
		MIN(cohort_start_date) AS cohort_start_date,
		MIN(cohort_end_date) AS cohort_end_date
	FROM @cohort_database_schema.@cohort_table
	WHERE cohort_definition_id = @cohort_id
	GROUP BY subject_id
	) cohort
INNER JOIN @cdm_database_schema.person ON subject_id = person.person_id
INNER JOIN @cdm_database_schema.observation_period ON observation_period.person_id = person.person_id
	AND DATEADD(DAY, @washout_period, observation_period_start_date) <= cohort_start_date
	AND observation_period_end_date >= cohort_start_date
WHERE gender_concept_id IN (
		8532,
		8507
		)
GROUP BY YEAR(cohort_start_date),
	FLOOR((YEAR(cohort_start_date) - year_of_birth) / 10),
	gender_concept_id;

SELECT calendar_year,
	age_group,
	gender_concept_id,
	SUM(CAST(DATEDIFF(DAY, start_date, end_date) AS BIGINT)) / 365.25 AS person_years
INTO #denominator
FROM (
	SELECT calendar_year,
		age_group,
		gender_concept_id,
		CASE 
			WHEN cohort_start_date IS NOT NULL
				THEN CASE 
						WHEN cohort_start_date < start_date
							THEN end_date
						ELSE cohort_start_date
						END
			ELSE start_date
			END AS start_date,
		end_date
	FROM (
		SELECT person.person_id,
			calendar_year,
			FLOOR((calendar_year - year_of_birth) / 10) AS age_group,
			gender_concept_id,
			CASE 
				WHEN observation_period_start_date > DATEFROMPARTS(calendar_year, 1, 1)
					THEN observation_period_start_date
				ELSE DATEFROMPARTS(calendar_year, 1, 1)
				END AS start_date,
			CASE 
				WHEN observation_period_end_date < DATEFROMPARTS(calendar_year + 1, 1, 1)
					THEN observation_period_end_date
				ELSE DATEFROMPARTS(calendar_year + 1, 1, 1)
				END AS end_date
		FROM (
			SELECT person_id,
				DATEADD(DAY, @washout_period, observation_period_start_date) AS observation_period_start_date,
				observation_period_end_date
			FROM @cdm_database_schema.observation_period
			WHERE DATEADD(DAY, @washout_period, observation_period_start_date) < observation_period_end_date
			) trunc_op
		INNER JOIN #calendar_years ON YEAR(observation_period_start_date) <= calendar_year
			AND YEAR(observation_period_end_date) >= calendar_year
		INNER JOIN @cdm_database_schema.person ON trunc_op.person_id = person.person_id
		WHERE gender_concept_id IN (
				8532,
				8507
				)
		) time_spans_1
	LEFT JOIN (
		SELECT subject_id,
			MIN(cohort_start_date) AS cohort_start_date
		FROM @cohort_database_schema.@cohort_table
		WHERE cohort_definition_id = @cohort_id
		GROUP BY subject_id
		) cohort ON subject_id = person_id
		AND cohort_start_date < end_date
	) time_spans_2
GROUP BY calendar_year,
	age_group,
	gender_concept_id;

SELECT denominator.calendar_year,
	denominator.age_group,
	concept_name gender,
	CASE 
		WHEN numerator.cohort_count IS NOT NULL
			THEN numerator.cohort_count
		ELSE CAST(0 AS INT)
		END AS cohort_count,
	person_years
INTO #rates_summary
FROM #denominator denominator
INNER JOIN @vocabulary_database_schema.concept ON denominator.gender_concept_id = concept_id
LEFT JOIN #numerator numerator ON denominator.calendar_year = numerator.calendar_year
	AND denominator.age_group = numerator.age_group;

DROP TABLE IF EXISTS #calendar_years;
DROP TABLE IF EXISTS #numerator;
DROP TABLE IF EXISTS #denominator;
