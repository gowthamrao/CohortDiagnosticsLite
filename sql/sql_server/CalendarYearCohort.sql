{DEFAULT @calendar_month = 1}
{DEFAULT @calendar_day = 1}


DROP TABLE IF EXISTS #calendar_years;

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
	),
calendar_years AS
(
SELECT DISTINCT calendar_year
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
	AND calendar_year <= end_year
)
SELECT cy.calendar_year AS cohort_definition_id,
       op.person_id AS subject_id,
       DATEFROMPARTS(cy.calendar_year, @calendar_month, @calendar_day) AS cohort_start_date,
       MIN(CASE 
             WHEN DATEADD(DAY, 365, DATEFROMPARTS(cy.calendar_year, @calendar_month, @calendar_day)) < op.observation_period_end_date 
             THEN DATEADD(DAY, 365, DATEFROMPARTS(cy.calendar_year, @calendar_month, @calendar_day)) 
             ELSE op.observation_period_end_date 
           END) AS cohort_end_date
INTO @cohort_table_name
FROM @cdm_database_schema.observation_period op
INNER JOIN calendar_years cy 
    ON DATEFROMPARTS(cy.calendar_year, @calendar_month, @calendar_day) >= op.observation_period_start_date
   AND DATEFROMPARTS(cy.calendar_year, @calendar_month, @calendar_day) <= op.observation_period_end_date
GROUP BY cy.calendar_year, op.person_id;
