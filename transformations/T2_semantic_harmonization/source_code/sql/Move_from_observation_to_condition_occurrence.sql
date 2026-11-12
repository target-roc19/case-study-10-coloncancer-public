BEGIN TRANSACTION;

-- We park the rows we want to move into a #temp table
CREATE OR REPLACE TEMP TABLE observations_to_move AS
SELECT
    observation_id,
    ori_table,
    unique_id,
    person_id,
    observation_concept_id,
    observation_date,
    observation_datetime,
    provider_id,
    visit_occurrence_id,
    visit_detail_id,
    observation_source_value,
    observation_source_concept_id
FROM d2.main.observation
WHERE observation_concept_id = 1617595;


-- Write the rows into the target tabel (condition_occurrence)
INSERT INTO d2.main.condition_occurrence (
    ori_table,
    unique_id,
    condition_occurrence_id,
    person_id,
    condition_concept_id,
    condition_start_date,
    condition_start_datetime,
    condition_end_date,
    condition_end_datetime,
    condition_type_concept_id,
    condition_status_concept_id,
    stop_reason,
    provider_id,
    visit_occurrence_id,
    visit_detail_id,
    condition_source_value,
    condition_source_concept_id,
    condition_status_source_value
)
SELECT
	-- Mapping of fields (as good as possible for columns that semi-match)
    ori_table,
    unique_id,
    -- I add 100000 to the observation_id to avoid id clashes (condition_occurrence_id goes up to 69882 in dummy data)
    observation_id + 100000 AS condition_occurrence_id,
    person_id,
    observation_concept_id AS condition_concept_id,
    observation_date AS condition_start_date,
    observation_datetime AS condition_start_datetime,
    NULL AS condition_end_date,
    NULL AS condition_end_datetime,
    0 AS condition_type_concept_id,
    0 AS condition_status_concept_id,
    NULL AS stop_reason,
    provider_id,
    visit_occurrence_id,
    visit_detail_id,
    observation_source_value AS condition_source_value,
    observation_source_concept_id AS condition_source_concept_id,
    NULL AS condition_status_source_value
FROM observations_to_move;


-- And remove those from the source (observation)
DELETE FROM d2.main.observation
WHERE observation_id IN (
    SELECT observation_id
    FROM observations_to_move
);

COMMIT;