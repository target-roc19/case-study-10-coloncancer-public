## Metadata

| Table name                | D3_UNIVARIATE_EPISODES                                                                                                           |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Per study variable, the periods during which that variable holds a given value for a person. One hive partition per variable_id. |
| Source                    | D3_CONCEPTS; D3_SPELLS; RWE-BRIDGE study_variables.csv; configuration/config_values.yaml                                         |
| Step                      | transformations/T3_study_design/source_code/create_univariate_episodes.R (episodeR::univariate_episodes_pipeline)                |
| Content                   | Value periods per person for each eligibility study variable, trimmed to the study period                                        |
| Population                | All persons in D3_SPELLS                                                                                                         |
| Unit of Observation (UoO) | One or more rows per person and variable (key: variable_id, person_id, start_episode)                                            |
| File format               | Hive-partitioned parquet (`data/D3_study_variables/D3_UNIVARIATE_EPISODES_parquet/variable_id=<variable_id>/`)                     |

## Codebook

| variable_in_output | variable(s) needed                                     | retrieved | computed | format    | rule_of_calculation                                                                            | description                           | vocabulary                                            |
| ------------------ | ------------------------------------------------------ | --------- | -------- | --------- | ---------------------------------------------------------------------------------------------- | ------------------------------------- | ----------------------------------------------------- |
| person_id          | D3_SPELLS:person_id                                    | x         |          | Integer   |                                                                                                | key identifier                        |                                                       |
| value              | D3_CONCEPTS:value                                      |           | x        | Character | value of the study variable during the episode, carried forward from the source concept record | value held during the episode         |                                                       |
| start_episode      | D3_CONCEPTS:date, config_values.yaml::start_study_date |           | x        | Date      | start of the period during which the value holds, trimmed to start_study_date                  | start date of the episode             |                                                       |
| end_episode        | D3_CONCEPTS:date, config_values.yaml::end_study_date   |           | x        | Date      | end of the period during which the value holds, trimmed to end_study_date                      | end date of the episode               |                                                       |
| variable_id        | study_variables.csv::variable_id                       |           | x        | Character | hive partition key                                                                             | study variable the episode belongs to | the study_variables.csv rows with eligibility == TRUE |

## Processing Steps

1. Select the rows of `study_variables.csv` with `eligibility == TRUE` and read the matching `concept_id` partitions from D3_CONCEPTS.
2. `episodeR::univariate_episodes_pipeline` turns the records into value periods per person: an initial spell per record, gap spells for the periods in between, and adjacent periods with the same value merged.
3. Episodes are trimmed to the study period (`start_study_date` - `end_study_date`) and persons without records still receive a row carrying the `missing_set_to` value.
