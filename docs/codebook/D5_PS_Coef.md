# Metadata

| Field                         | Value                                                                                             |
| ----------------------------- | ------------------------------------------------------------------------------------------------- |
| **Table name**                | `D5_PS_COEF`                                                                                      |
| **Description**               | Coefficient estimates and model diagnostics from the final binomial propensity score model.       |
| **Source**                    | `D4_StudyCohort_Covariates`                                                                       |
| **Step**                      | `transformations/common_functionality/functions/compute_d4_StudyCohort_IPTW_function.R`           |
| **Content**                   | Coefficients from the final propensity score model after any Round 2 exclusions.                  |
| **Population**                | Rows defined by terms in the final propensity score model.                                        |
| **Unit of Observation (UoO)** | One row per model coefficient, including the intercept, factor contrasts, and spline basis terms. |

---

# CODEBOOK

| Variable name    | Description                                                                                          | Format    | Vocabulary | Variables from source used for the variable | Principles for coding |
| ---------------- | ---------------------------------------------------------------------------------------------------- | --------- | ---------- | ------------------------------------------- | --------------------- |
| **coefficient**  | Model coefficient name, such as `(Intercept)`, a factor contrast, or `splines::ns(SV_AGE, df = 4)1`. | character |            |                                             |                       |
| **estimate**     | Point estimate of the coefficient.                                                                   | numeric   |            |                                             |                       |
| **std_error**    | Standard error of the coefficient estimate.                                                          | numeric   |            |                                             |                       |
| **z_score**      | Wald z statistic for the coefficient.                                                                | numeric   |            |                                             |                       |
| **p_value**      | Two-sided Wald p-value for the coefficient.                                                          | numeric   | [0, 1]     |                                             |                       |
| **exp_estimate** | Exponential of `estimate`, representing the coefficient odds ratio.                                  | numeric   | > 0        |                                             |                       |

# Processing Steps

1. Fit the final binomial propensity score model after Round 1 diagnostics and any Round 2 exclusions.
2. Extract `summary(model_final)$coefficients`.
3. Store the coefficient estimates, standard errors, z statistics, p-values, and exponentiated estimates.

# Example

Example coefficients from a hypothetical final PS model fit.

| coefficient                                  | estimate | std_error | z_score | p_value | exp_estimate |
| -------------------------------------------- | -------- | --------- | ------- | ------- | ------------ |
| (Intercept)                                  |          |           |         |         |              |
| SV_SEXM                                      |          |           |         |         |              |
| SV_ECOG_STATUS1                              |          |           |         |         |              |
| SV_METASTASIS_LIVERTRUE                      |          |           |         |         |              |
| SV_METASTASIS_LUNGTRUE                       |          |           |         |         |              |
| SV_SITE_SIDEDNESSCOLON_RIGHT                 |          |           |         |         |              |
| SV_SITE_SIDEDNESSCOLON_UNKNOWN               |          |           |         |         |              |
| SV_SITE_SIDEDNESSRECTOSIGMOID                |          |           |         |         |              |
| SV_SITE_SIDEDNESSRECTUM                      |          |           |         |         |              |
| SV_MUTATION_RASpresent_KRASG12C_and_unk_type |          |           |         |         |              |
| SV_MUTATION_BRAFabsent                       |          |           |         |         |              |
| SV_MUTATION_BRAFpresent_V600E                |          |           |         |         |              |
| SV_DIAGNOSED_IN_2021TRUE                     |          |           |         |         |              |
| splines::ns(SV_AGE, df = 4)1                 |          |           |         |         |              |
| splines::ns(SV_AGE, df = 4)2                 |          |           |         |         |              |
| splines::ns(SV_AGE, df = 4)3                 |          |           |         |         |              |
| splines::ns(SV_AGE, df = 4)4                 |          |           |         |         |              |
