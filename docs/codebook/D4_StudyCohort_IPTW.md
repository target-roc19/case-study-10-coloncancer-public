# Metadata

| **Table name**                | D4_StudyCohort_IPTW                                                       |
| ----------------------------- | ------------------------------------------------------------------------- |
| **Description**               | Dataset consisting of IP weights for the matched cohort.                  |
| **Source**                    | D4_StudyCohortCovariates.parquet, psmodel_variables.csv                   |
| **Step**                      | transformations/T3_study_design/source_code/compute_d4_StudyCohort_IPTW.R |
| **Content**                   | Propensity scores and IP weights                                          |
| **Population**                | StudyCohort exposed and control units                                     |
| **Unit of Observation (UoO)** | One row per`person_id`                                                    |

---

# Codebook

| Variable name          | Description                                                                                                                                    | Format    | retrieved               | coputed | Variables from source used for the variable | Principles for coding                                                                                                              | vocabulary       |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------------------- | ------- | ------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- | ---------------- |
| **person_id**          | person's unique identifier                                                                                                                     | character | x                       |         | D4_StudyCohortCovariates::person_id         |                                                                                                                                    |                  |
| **group**              | Group identifier: control, exposed                                                                                                             | character | x                       |         | D4_StudyCohortCovariates::group             |                                                                                                                                    | exposed, control |
| group_binary           | Binary version of group (exposed = 1, control = 0)                                                                                             | integer   |                         | x       |                                             | if group == exposed then 1 else 0                                                                                                  | 0, 1             |
| **T0**                 | moment of treatment initiation with either CapOx-B (exposure) or CapOx (control)                                                               | Date      | x                       |         | D4_StudyCohortCovariates::T0                |                                                                                                                                    |                  |
| **p_score**            | Predicted probability of exposure from the PS model (`fitted.values`)                                                                          | Numeric   |                         | x       |                                             | Predicted probability from logistic regression: P(group_binary = 1 / covariates)                                                   | [0, 1]           |
| include_after_trimming | indicator variable; if trimming of the propensity scores applied, should this individual be selected into the final cohort or not              | Logical   | p_score, ps_trim_bounds | x       |                                             | IF ps_trim_bounds given: TRUE if p_score >= lower AND p_score <= upper, else FALSE. If no trimming, TRUE for all.                  | T/F              |
| raw_iptw               | Inverse probability of treatment weights (raw/untruncated).  (ATT weights)                                                                     | numeric   | p_score, group_binary   | x       |                                             | IF group_binary == 1 THEN 1 ELSE`PS / (1 - PS)`                                                                                    | >0               |
| stabilized_iptw        | Stabilized version of iptw (multiply by marginal probability of being assigned to the group you are assigned to - before trimming this is 0.5) | numeric   | p_score, group_binary   | x       |                                             |                                                                                                                                    | >0               |
| trunc_iptw             | Truncated stabilized IPTW (truncated at 1st and 99th percentile)                                                                               | numeric   | stabilised_iptw         | x       |                                             | Per group: values below 1st percentile set to P1; values above 99th percentile set to P99. Percentiles computed within each group. | >0               |

---

## Steps

#### `iptw` estimation

1. Extract predicted probabilities of exposed group membership from the model
2. If trimming of propensity score required, input lower and upper bound of propensity score values (provided by metadata/global parameter). If supplied, select only units who have `p_scores` within the trimming bounds, record in `include_after_trimming`who meets these criteria
3. Compute raw IPT weights from `p_score` for individuals with `include_after_trimming == TRUE`. Exact formula depends on ATT weights. For exposed is 1, for controls is PS/(1-PS)
4. Compute stabilized ipt weights using marginal probability of actual treatment assignment, by by `A*(sum(A == 1)/nrow(StudyCohort))*iptw_raw + (1-A)*(sum(A == 0)/nrow(StudyCohort)*iptw_raw`
5. Calculate truncated stabilized IPT weights; compute 1st and 99th percentile; all values lower than 1st percentile get set to 1st percentile, all values higher than 99th percentile get set to 99th percentile.
