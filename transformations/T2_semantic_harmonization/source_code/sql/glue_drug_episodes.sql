/* Here we dedup / glue certain drugs as per https://github.com/target-roc19/case-study-10-coloncancer/issues/112
 * I honestly think this is a bad idea and against all OMOP principles, but apparently it is necessary. 
 * Use with care and don't blame me. WL
 */

BEGIN TRANSACTION;

CREATE OR REPLACE TABLE d2.main.drug_exposure AS

-- Keep all but the rows we don't want to glue
SELECT *
FROM d2.main.drug_exposure
WHERE drug_concept_id NOT IN ('1337620', '1318011', '35807315', '1397141')

UNION ALL

-- Dedup / glue all the rows we want to
SELECT 
    MIN(ori_table) AS ori_table,
    MIN(unique_id) AS unique_id,
    MIN(drug_exposure_id) AS drug_exposure_id,
    person_id,
    drug_concept_id,
    MIN(drug_exposure_start_date) AS drug_exposure_start_date,
    MIN(drug_exposure_start_datetime) AS drug_exposure_start_datetime,
    MAX(drug_exposure_end_date) AS drug_exposure_end_date,
    MAX(drug_exposure_end_datetime) AS drug_exposure_end_datetime,
    MAX(verbatim_end_date) AS verbatim_end_date,
    drug_type_concept_id,
    stop_reason,
    refills,
    quantity,
    days_supply,
    sig,
    route_concept_id,
    lot_number,
    MIN(provider_id) AS provider_id,
    MIN(visit_occurrence_id) AS visit_occurrence_id,
    MIN(visit_detail_id) AS visit_detail_id,
    drug_source_value,
    drug_source_concept_id,
    route_source_value,
    dose_unit_source_value
FROM d2.main.drug_exposure
WHERE drug_concept_id IN ('1337620', '1318011', '35807315', '1397141')
GROUP BY
    person_id,
    drug_concept_id,
    drug_type_concept_id,
    stop_reason,
    refills,
    quantity,
    days_supply,
    sig,
    route_concept_id,
    lot_number,
    drug_source_value,
    drug_source_concept_id,
    route_source_value,
    dose_unit_source_value;

COMMIT;