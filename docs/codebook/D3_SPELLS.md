## Metadata

| Table name                | D3_SPELLS                                                                                                                             |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Observation periods that overlap or are separated by a gap of at most 180 days are merged into a single observation period (a spell). |
| Source                    | D2_CDM: observation_period; D3_PERSONS; configuration/config_values.yaml                                                              |
| Step                      | transformations/T2_semantic_harmonization/source_code/process_observation_periods.R                                                   |
| Content                   | Merged observation periods per person, with the person attributes attached                                                            |
| Population                | Persons in D3_PERSONS with an observation period where with_start_and_end and start_before_end are both TRUE                          |
| Unit of Observation (UoO) | One or more rows per person (key: person_id, start_spell)                                                                             |
| File format               | Parquet file (data/D3_study_variables/D3_SPELLS.parquet)                                                                              |

## Codebook

| variable_in_output       | variable(s) needed                                 | retrieved | computed | format    | rule_of_calculation                                                                                                                              | description                           | vocabulary |
| ------------------------ | -------------------------------------------------- | --------- | -------- | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------- | ---------- |
| person_id                | D3_PERSONS:person_id, observation_period:person_id | x         |          | Integer   |                                                                                                                                                  | key identifier                        |            |
| start_spell              | observation_period:observation_period_start_date   |           | x        | Date      | earliest start of the merged observation periods; set to birth_date when earlier than birth_date                                                 | start of the spell                    |            |
| end_spell                | observation_period:observation_period_end_date     |           | x        | Date      | latest end of the merged observation periods; imputed with config_values.yaml::end_study_date when missing and the spell starts before that date | end of the spell                      |            |
| year_of_birth            | D3_PERSONS:year_of_birth                           | x         |          | Integer   |                                                                                                                                                  | year of birth                         |            |
| sex_at_instance_creation | D3_PERSONS:sex_at_instance_creation                | x         |          | Character |                                                                                                                                                  | sex                                   | F, M       |
| birth_date               | D3_PERSONS:birth_date                              | x         |          | Date      |                                                                                                                                                  | date of birth                         |            |
| death_date               | D3_PERSONS:death_date                              | x         |          | Date      |                                                                                                                                                  | date of death, NA when alive          |            |
| with_start_and_end       | start_spell, end_spell                             |           | x        | Boolean   | TRUE when both dates are present                                                                                                                 | quality flag; only TRUE rows are kept | TRUE       |
| start_before_end         | start_spell, end_spell                             |           | x        | Boolean   | TRUE when start_spell < end_spell                                                                                                               | quality flag; only TRUE rows are kept | TRUE       |

## Processing Steps

1. Read `observation_period` from the D2 database and impute a missing end date with `config_values.yaml::end_study_date` when the period starts before that date.
2. Merge observation periods that overlap or are separated by a gap of at most 180 days into one spell. The gap is hard-coded in the script (`gap_allowed = 180`), not read from configuration.
3. Inner join with D3_PERSONS and correct a `start_spell` that precedes `birth_date`.
4. Set the quality flags and keep only spells where both are TRUE.
