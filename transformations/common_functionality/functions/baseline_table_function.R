#' Baseline characteristics table for TARGET CS02
#'
#' Restricts cohort to `include_after_trimming == TRUE`, calls
#' `DescriptiveTable::DescriptivesTable()` for the unweighted layout
#' (Overall + EXPOSED + CONTROL N/perc/aux, NUM2 median/IQR,
#' expectedCat handling), computes the matching IPTW-weighted N/perc/aux
#' row-by-row (DescriptiveTable's count_* ignore weights), and attaches
#' unweighted + IPTW-weighted aSMD columns from
#' `DescriptiveTable::compute_asd()`.
#'
#' Output columns:
#'   label, var, type,
#'   N/perc/aux unwt × {overall, exposed, control},
#'   N/perc/aux wt   × {overall, exposed, control},
#'   asd_unwt, asd_wt.
#'
#' @param d4_covariates D4_StudyCohort_Covariates.
#' @param d4_iptw D4_StudyCohort_IPTW.
#' @param metadata DescriptiveTable-style metadata; columns
#'   `var; type; expectedCat; label; parent; parent_cat; header`.

GROUPS <- c("OVERALL", "exposed", "control")

# Weighted N/perc/aux for a single output row
weighted_row_stats <- function(x, w, type, label, lvl, tw) {
  if (all(is.na(x))) {
    return(rep(NA_real_, 3))
  }
  key <- if (type %in% c("TF", "CAT")) type else label
  switch(
    key,
    TF = {
      n <- sum(w[!is.na(x) & x == TRUE], na.rm = TRUE)
      c(n, 100 * n / tw, NA_real_)
    },
    CAT = {
      n <- sum(w[!is.na(x) & x == lvl], na.rm = TRUE)
      c(n, 100 * n / tw, NA_real_)
    },
    "Median (Q1, Q3)" = {
      q <- Hmisc::wtd.quantile(x, w, c(0.25, 0.5, 0.75), na.rm = TRUE)
      c(q[2], q[1], q[3])
    },
    "Min, Max" = c(
      suppressWarnings(min(x, na.rm = TRUE)),
      suppressWarnings(max(x, na.rm = TRUE)),
      NA_real_
    ),
    "Mean (SD)" = {
      mu <- stats::weighted.mean(x, w, na.rm = TRUE)
      sdw <- sqrt(sum(w * (x - mu)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
      c(mu, sdw, NA_real_)
    },
    rep(NA_real_, 3)
  )
}

# Standardized difference for a binary variable, calculated using non-missing values only. For categorical variables, a separate aSMD is calculated for each level (coded as 0/1). w = 1 gives the unweighted aSMD; IPT weights give the weighted aSMD.
level_asd <- function(ind, present, group, w) {
  prop <- function(g) {
    keep <- present & group == g
    denom <- sum(w[keep])
    if (denom == 0) NA_real_ else sum(w[keep & ind]) / denom
  }
  pe <- prop("exposed")
  pc <- prop("control")
  if (is.na(pe) || is.na(pc)) {
    return(NA_real_)
  }
  sd_avg <- sqrt((pe * (1 - pe) + pc * (1 - pc)) / 2)
  if (sd_avg == 0) 0 else abs(pe - pc) / sd_avg
}

build_baseline_table <- function(
  d4_covariates,
  d4_iptw,
  metadata,
  mask = FALSE
) {
  metadata <- data.table::as.data.table(metadata)
  cov_dt <- data.table::as.data.table(d4_covariates)
  iptw <- data.table::as.data.table(d4_iptw)
  cov_dt[, `:=`(person_id = as.character(person_id), T0 = as.Date(T0))]
  iptw[, `:=`(person_id = as.character(person_id), T0 = as.Date(T0))]

  # cohort and iptw
  cohort <- merge(
    cov_dt,
    iptw[
      include_after_trimming == TRUE,
      .(person_id, T0, w = as.numeric(stabilized_trunc_iptw))
    ],
    by = c("person_id", "T0")
  )
  cohort <- cohort[,
    intersect(c("group", "w", metadata$var), names(cohort)),
    with = FALSE
  ]

  # Normalize weights within arm so the weighted N sums to the arm headcount
  cohort[, w := w * (.N / sum(w)), by = "group"]

  # Coerce each metadata column to the type that DescriptivesTable expects
  for (i in seq_len(nrow(metadata))) {
    v <- metadata$var[i]
    if (!v %in% names(cohort)) {
      next
    }
    coerce <- switch(
      metadata$type[i],
      TF = as.logical,
      CAT = as.character,
      NUM1 = ,
      NUM2 = function(x) suppressWarnings(as.numeric(x)),
      identity
    )
    data.table::set(cohort, j = v, value = coerce(cohort[[v]]))
  }

  # Create group = "OVERALL" so DescriptivesTable creates
  # V*_OVERALL
  pop <- data.table::rbindlist(list(
    cohort,
    data.table::copy(cohort)[, group := "OVERALL"]
  ))
  out <- data.table::as.data.table(DescriptiveTable::DescriptivesTable(
    popdf = pop,
    table_metadata = metadata,
    groupcol = "group",
    output_format = "processed",
    calculate_asd = FALSE,
    keep_varinfo = TRUE,
    control_types = FALSE,
    round_decimals = 3,
    output_asd = FALSE
  ))

  # Weighted N/perc/aux per row
  pop_by_g <- list(
    OVERALL = cohort,
    exposed = cohort[group == "exposed"],
    control = cohort[group == "control"]
  )
  total_w <- sapply(pop_by_g, function(p) sum(p$w, na.rm = TRUE))
  for (g in GROUPS) {
    out[, paste0("W", 1:3, "_", g) := NA_real_]
  }

  for (r in seq_len(nrow(out))) {
    v <- out$var[r]
    if (is.na(v)) {
      next
    } # header row
    if (v == "Total") {
      for (g in GROUPS) {
        out[r, (paste0("W1_", g)) := round(total_w[[g]])]
      }
      next
    }
    if (!v %in% names(cohort)) {
      next
    }
    for (g in GROUPS) {
      p <- pop_by_g[[g]]
      s <- weighted_row_stats(
        p[[v]],
        p$w,
        out$type[r],
        out$label[r],
        out$cat[r],
        total_w[[g]]
      )
      # Weighted counts (TF/CAT) to whole numbers; summary stats keep 3 dp
      s <- if (out$type[r] %in% c("TF", "CAT")) {
        c(round(s[1]), round(s[2], 3), s[3])
      } else {
        round(s, 3)
      }
      out[r, (paste0("W", 1:3, "_", g)) := as.list(s)]
    }
  }

  # aSMDs. Categorical/binary variables report one standardized difference per
  # level (each level as a binary indicator); continuous variables keep the
  # mean-difference / pooled-SD aSMD from compute_asd on the variable's header.
  out[, `:=`(asd_unwt = NA_real_, asd_wt = NA_real_)]

  num_meta <- metadata[type %in% c("NUM1", "NUM2")]
  if (nrow(num_meta) > 0) {
    num_unwt <- DescriptiveTable::compute_asd(
      CovariatesInformation = cohort,
      metadata = num_meta[, .(var, type)],
      group_names = c("exposed", "control"),
      referenceExtension = "x",
      use_weights = FALSE
    )
    num_wt <- DescriptiveTable::compute_asd(
      CovariatesInformation = cohort,
      metadata = num_meta[, .(var, type)],
      group_names = c("exposed", "control"),
      referenceExtension = "x",
      use_weights = "w"
    )
    header_row <- match(num_meta$label, out$label)
    out[
      header_row,
      `:=`(
        asd_unwt = round(as.numeric(num_unwt$asd_x), 3),
        asd_wt = round(as.numeric(num_wt$asd_x), 3)
      )
    ]
  }

  unit_w <- rep(1, nrow(cohort))
  for (r in which(out$type %in% c("CAT", "TF"))) {
    v <- out$var[r]
    x <- cohort[[v]]
    present <- !is.na(x)
    ind <- present &
      (if (out$type[r] == "TF") as.logical(x) else x == out$cat[r])
    out[
      r,
      `:=`(
        asd_unwt = round(level_asd(ind, present, cohort$group, unit_w), 3),
        asd_wt = round(level_asd(ind, present, cohort$group, cohort$w), 3)
      )
    ]
  }

  # mask the unweighted EXPOSED/CONTROL cell counts
  # + percentages
  if (isTRUE(mask)) {
    out <- data.table::as.data.table(DescriptiveTable::mask_vector_wrapper(
      out,
      threshold = 5,
      count.names = c("V1_control", "V1_exposed"),
      pct.names = c("V2_control", "V2_exposed"),
      table_metadata = metadata
    ))
  }

  # Drop DescriptiveTable varinfo
  out[, c("id", "cat", "parentv", "parent_catv", "m_id") := NULL]
  for (g in GROUPS) {
    data.table::setnames(
      out,
      c(paste0("V", 1:3, "_", g), paste0("W", 1:3, "_", g)),
      c(
        paste0(c("N", "perc", "aux"), "_unwt_", tolower(g)),
        paste0(c("N", "perc", "aux"), "_wt_", tolower(g))
      )
    )
  }
  data.table::setcolorder(
    out,
    c(
      "label",
      "var",
      "type",
      "N_unwt_overall",
      "perc_unwt_overall",
      "aux_unwt_overall",
      "N_unwt_exposed",
      "perc_unwt_exposed",
      "aux_unwt_exposed",
      "N_unwt_control",
      "perc_unwt_control",
      "aux_unwt_control",
      "N_wt_overall",
      "perc_wt_overall",
      "aux_wt_overall",
      "N_wt_exposed",
      "perc_wt_exposed",
      "aux_wt_exposed",
      "N_wt_control",
      "perc_wt_control",
      "aux_wt_control",
      "asd_unwt",
      "asd_wt"
    )
  )
  out[]
}

#' Unweighted Overall/EXPOSED/CONTROL N/perc/aux + aSMD for one cohort
#'
#' Shared unweighted layout used by the trimming comparison. `suffix` is
#' appended to every statistic column so two blocks can sit side by side.
.descriptives_block <- function(cohort, metadata, suffix) {
  for (i in seq_len(nrow(metadata))) {
    v <- metadata$var[i]
    if (!v %in% names(cohort)) {
      next
    }
    coerce <- switch(
      metadata$type[i],
      TF = as.logical,
      CAT = as.character,
      NUM1 = ,
      NUM2 = function(x) suppressWarnings(as.numeric(x)),
      identity
    )
    data.table::set(cohort, j = v, value = coerce(cohort[[v]]))
  }
  pop <- data.table::rbindlist(list(
    cohort,
    data.table::copy(cohort)[, group := "OVERALL"]
  ))
  out <- data.table::as.data.table(DescriptiveTable::DescriptivesTable(
    popdf = pop,
    table_metadata = metadata,
    groupcol = "group",
    output_format = "processed",
    calculate_asd = FALSE,
    keep_varinfo = TRUE,
    control_types = FALSE,
    round_decimals = 3,
    output_asd = FALSE
  ))
  asd <- DescriptiveTable::compute_asd(
    CovariatesInformation = cohort,
    metadata = metadata[, .(var, type)],
    group_names = c("exposed", "control"),
    referenceExtension = "x",
    use_weights = FALSE
  )
  idx <- match(out$label, metadata$label)
  out[, asd := round(as.numeric(asd$asd_x)[idx], 3)]
  out[, c("id", "cat", "parentv", "parent_catv", "m_id") := NULL]
  for (g in GROUPS) {
    data.table::setnames(
      out,
      paste0("V", 1:3, "_", g),
      paste0(c("N", "perc", "aux"), "_", tolower(g))
    )
  }
  stat_cols <- setdiff(names(out), c("label", "var", "type"))
  data.table::setnames(out, stat_cols, paste0(stat_cols, "_", suffix))
  out
}

#' Baseline characteristics before vs after PS trimming, with aSMDs (#71 Table 3)
#'
#' Same unweighted layout as `build_baseline_table`, computed on the pre-trim
#' cohort (everyone with a propensity score) and the post-trim cohort
#' (`include_after_trimming == TRUE`) and placed side by side.
#'
#' @param d4_covariates D4_StudyCohort_Covariates.
#' @param d4_iptw D4_StudyCohort_IPTW.
#' @param metadata DescriptiveTable-style metadata (as for `build_baseline_table`).
build_baseline_trimming_table <- function(d4_covariates, d4_iptw, metadata) {
  metadata <- data.table::as.data.table(metadata)
  cov_dt <- data.table::as.data.table(d4_covariates)
  iptw <- data.table::as.data.table(d4_iptw)
  cov_dt[, `:=`(person_id = as.character(person_id), T0 = as.Date(T0))]
  iptw[, `:=`(person_id = as.character(person_id), T0 = as.Date(T0))]

  cohort_for <- function(keep) {
    cohort <- merge(cov_dt, keep, by = c("person_id", "T0"))
    cohort[, intersect(c("group", metadata$var), names(cohort)), with = FALSE]
  }
  before <- .descriptives_block(
    cohort_for(iptw[!is.na(p_score), .(person_id, T0)]),
    metadata,
    "before"
  )
  after <- .descriptives_block(
    cohort_for(iptw[include_after_trimming == TRUE, .(person_id, T0)]),
    metadata,
    "after"
  )
  after[, c("label", "var", "type") := NULL]
  out <- cbind(before, after)
  col_order <- c("label", "var", "type")
  for (suffix in c("before", "after")) {
    for (g in c("overall", "exposed", "control")) {
      col_order <- c(
        col_order,
        paste0(c("N", "perc", "aux"), "_", g, "_", suffix)
      )
    }
    col_order <- c(col_order, paste0("asd_", suffix))
  }
  data.table::setcolorder(out, col_order)
  out[]
}
