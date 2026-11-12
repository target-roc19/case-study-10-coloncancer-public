## Metadata

| Table name                | D5_PS_ModelBuilding                                                                                                                                                                                                                                                    |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | File that describes the model building procedure for a propensity score model; starting from an initial list of variables to be included in the model, describing if each variable meets different data quality and/or data driven criteria to be included as relevant |
| Source                    | D4\_StudyCohort\_Covariates                                                                                                                                                                                                                                            |
| Step                      | fit_ps_model.R                                                                                                                                                                                                                                                         |
| Content                   | In/exclusion criteria assessment for every candidate variable for the final PS models.                                                                                                                                                                                 |
| Population                | Candidate variables to be included in a propensity score model for a particular matched cohort                                                                                                                                                                         |
| Unit of Observation (UoO) | NA                                                                                                                                                                                                                                                                     |

## Codebook

| Variable name    | Description                                                             | Source used for the variable          | Rule of calculation                                                                                   | Vocabulary   | Format    |
| ---------------- | ----------------------------------------------------------------------- | ------------------------------------- | ----------------------------------------------------------------------------------------------------- | ------------ | --------- |
| variable_id      | Candidate variables for the propensity score model.                     | `psmodel_variables.csv`               | The `var` column from the PS model metadata.                                                          |              | character |
| var_type         | Variable type.                                                          | `psmodel_variables.csv`               | The `vartype` column from the PS model metadata.                                                      | NUM; CAT; TF | character |
| force_in         | Whether the metadata marks the variable as forced into the model.       | `psmodel_variables.csv`               | Copied from `force_in`                                                                                |              | logical   |
| force_out        | Whether the metadata marks the variable as forced out of the model.     | `psmodel_variables.csv`               | Copied from `force_out`; defaults to `FALSE` when the column is absent.                               |              | logical   |
| missing_pct      | Percentage of missing values in the cohort.                             | `D4_StudyCohort_Covariates`           | `100 * number of missing values / number of cohort rows`, calculated for every candidate variable.    |              | numeric   |
| nonzero_variance | Whether at least two non-missing values are present in the cohort.      | `D4_StudyCohort_Covariates`           | `TRUE` when the number of unique non-missing values is at least two; otherwise `FALSE`.               |              | logical   |
| include_r1       | Whether the variable is included in the initial propensity score model. | Derived from metadata and cohort data | `TRUE` when `nonzero_variance == TRUE` and `force_out == FALSE`.                                      |              | logical   |
| included_r2      | Whether the variable is included in the final propensity score model.   | Derived from Round 1 diagnostics      | `TRUE` when retained after Round 1; otherwise `FALSE` when removed for a recorded fit issue.          |              | logical   |
| reason_excluded  | Reason a variable was excluded from a model round.                      | Derived from model diagnostics        | `zero_variance`, `force_out`, `non_convergence`, or `large_se`; `NA` when no exclusion reason exists. |              | character |

### Processing

1. Candidate variables are copied from `psmodel_variables.csv` and assessed for missingness and non-zero variance.
2. Round 1 includes variables with non-zero variance that are not marked `force_out`.
3. Categorical and true/false variables are converted to factors.
4. `SV_AGE` is modeled using `splines::ns(SV_AGE, df = 4)`.
5. If Round 1 does not converge or a variable has a coefficient standard error above the configured threshold, the affected variables are removed and the model is refit as Round 2.

### Exclusion reasons

`reason_excluded` is initialized to `NA`. It is set to `zero_variance` or `force_out` during candidate selection, and can subsequently be set to `non_convergence` or `large_se` based on Round 1 diagnostics. A variable with no exclusion reason has `NA` in this field.

## Output example

| variable_id          | var_type | force_in | force_out | missing_pct | nonzero_variance | include_r1 | included_r2 | reason_excluded |
| -------------------- | -------- | -------- | --------- | ----------- | ---------------- | ---------- | ----------- | --------------- |
| SV_AGE               | NUM      | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_SEX               | CAT      | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_ECOG_STATUS       | CAT      | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_METASTASIS_LIVER  | TF       | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_METASTASIS_LUNG   | TF       | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_SITE_SIDEDNESS    | CAT      | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_MUTATION_RAS      | CAT      | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_MUTATION_BRAF     | CAT      | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
| SV_DIAGNOSED_IN_2021 | TF       | TRUE     | FALSE     | 0           | TRUE             | TRUE       | TRUE        | NA              |
