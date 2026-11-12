## Metadata

| Table name                | D3_PERSONS                                                                                                                       |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Processed person information: sex, imputed birth and death dates, and the data quality flags used to exclude incomplete records. |
| Source                    | D2_CDM: person, death                                                                                                            |
| Step                      | transformations/T2_semantic_harmonization/source_code/process_persons.R                                                          |
| Content                   | One processed record per person in the data instance                                                                             |
| Population                | Persons in the data instance that pass the quality checks in Processing Steps and are 18 or older at the end of the study period |
| Unit of Observation (UoO) | One row per person                                                                                                               |
| File format               | Parquet file (data/D3_study_variables/D3_PERSONS.parquet)                                                                        |

## Codebook

| variable_in_output       | variable(s) needed             | retrieved | computed | format    | rule_of_calculation                                                          | description                       | vocabulary  |
| ------------------------ | ------------------------------ | --------- | -------- | --------- | ---------------------------------------------------------------------------- | --------------------------------- | ----------- |
| person_id                | person:person_id               | x         |          | Integer   |                                                                              | key identifier                    |             |
| sex_at_instance_creation | person:gender_concept_id       |           | x        | Character | 8532 -> F; 8507 -> M; anything else -> NA                                    | sex                               | F, M        |
| imputed_birth_day        | person:day_of_birth            |           | x        | Boolean   | TRUE when year_of_birth is present and day_of_birth is missing               | day of birth was imputed (to 16)  | TRUE, FALSE |
| imputed_birth_month      | person:month_of_birth          |           | x        | Boolean   | TRUE when year_of_birth is present and month_of_birth is missing             | month of birth was imputed (to 6) | TRUE, FALSE |
| imputed_death_day        | death:death_date               |           | x        | Boolean   | TRUE when year_of_death is present and day_of_death is missing               | day of death was imputed (to 16)  | TRUE, FALSE |
| imputed_death_month      | death:death_date               |           | x        | Boolean   | TRUE when year_of_death is present and month_of_death is missing             | month of death was imputed (to 6) | TRUE, FALSE |
| birth_date               | person:day/month/year_of_birth |           | x        | Date      | combine day, month and year of birth                                         | date of birth                     |             |
| death_date               | death:death_date               |           | x        | Date      | combine day, month and year of death                                         | date of death, NA when alive      |             |
| missing_year_of_birth    | person:year_of_birth           |           | x        | Boolean   | TRUE when year_of_birth is missing                                           | quality flag                      | TRUE, FALSE |
| not_missing_year_death   | death:death_date               |           | x        | Boolean   | TRUE unless year_of_death is missing while day or month of death are present | quality flag                      | TRUE, FALSE |
| death_before_birth       | birth_date, death_date         |           | x        | Boolean   | TRUE when death_date < birth_date                                            | quality flag                      | TRUE, FALSE |
| missing_sex              | sex_at_instance_creation       |           | x        | Boolean   | TRUE when sex is missing or not F/M                                          | quality flag                      | TRUE, FALSE |
| year_of_birth            | person:year_of_birth           | x         |          | Integer   |                                                                              | year of birth                     |             |

## Processing Steps

1. Read `person` from the D2 database; merge the `death` table on person_id when present.
2. Derive `sex_at_instance_creation` from `gender_concept_id` and set the imputation and quality flags.
3. Impute a missing day of birth/death to 16 and a missing month to 6 (to 5 when the day is 31).
4. Build `birth_date` and `death_date` from the (imputed) day, month and year.
5. Remove records stepwise: missing year of birth, missing year of death with day/month present, death before birth, duplicated person_id, and missing sex.
