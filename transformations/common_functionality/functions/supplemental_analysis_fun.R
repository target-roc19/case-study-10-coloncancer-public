# Matching statistics among exposed individuals by calendar week
matching_bar_plot <- function(dt){

  dt <- data.table::as.data.table(dt)
  
  # Reshape to long format
  plot_data <- data.table::melt(
    dt,
    id.vars = "calendar_week",
    measure.vars = c(
      "matched_exposed",
      "unmatched_exposed",
      "matched_control",
      "unmatched_control"
    ),
    variable.name = "status",
    value.name = "n"
  )

  week_levels <- sort(unique(dt$calendar_week))

  plot_data[, calendar_week := factor(
    calendar_week,
    levels = week_levels
  )]

  # Plot
  ggplot2::ggplot(plot_data,
        ggplot2::aes(x = calendar_week,
            y = n,
            fill = status)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::scale_fill_manual(
      values = c(
        matched_exposed = "#1F77B4",
        unmatched_exposed = "#2CA02C",
        matched_control = "#FF7F0E",
        unmatched_control = "#D62728"
      ),
      labels = c(
        matched_exposed = "Matched exposed",
        unmatched_exposed = "Unmatched exposed",
        matched_control = "Matched control",
        unmatched_control = "Unmatched control"
      )
    ) +
    ggplot2::labs(
      x = "Calendar week",
      y = "Number of individuals",
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 7),
      legend.position = "top",
      panel.grid.major.x = ggplot2::element_blank()
    )


  }


DAYS_PER_MONTH <- if (exists("config_values")) {
  config_values$month_length
} else {
  30.44
}

.group_factor <- function(dt) {
  data.table::copy(dt)[,
    group := relevel(factor(group), ref = "control")
  ]
}

# survfit strata -> long data.table with a (0, 1) per group
.tidy_survfit <- function(fit) {
  strata <- rep(names(fit$strata), fit$strata)
  dt <- data.table::data.table(
    time = fit$time,
    surv = fit$surv,
    group = sub("^group=", "", strata)
  )
  anchor <- dt[, .(time = 0, surv = 1), by = group]
  data.table::rbindlist(list(anchor, dt), use.names = TRUE)[order(group, time)]
}

#' Scaled Schoenfeld residuals from the IPTW-adjusted Cox model
plot_schoenfeld <- function(dt, fup_days, event_count, weights) {
  dt <- .group_factor(data.table::as.data.table(dt)[
    include_after_trimming == TRUE
  ])

  fit <- survival::coxph(
    survival::Surv(dt[[fup_days]], dt[[event_count]]) ~ group,
    data = dt,
    cluster = person_id,
    robust = TRUE,
    weights = dt[[weights]]
  )
  zph <- survival::cox.zph(fit)

  resid <- data.table::data.table(
    time_months = zph$time / DAYS_PER_MONTH,
    residual = as.numeric(zph$y[, 1])
  )

  ggplot2::ggplot(resid, ggplot2::aes(x = time_months, y = residual)) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey40"
    ) +
    ggplot2::geom_point(size = 2, alpha = 0.6) +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = TRUE,
      color = "darkblue"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Follow-up time (months)",
      y = "Scaled Schoenfeld residual (group)"
    )
}

#' log(-log(S(t))) vs log(t) by group
plot_loglog <- function(dt, fup_days, event_count, weights) {
  dt <- .group_factor(data.table::as.data.table(dt)[
    include_after_trimming == TRUE
  ])
  fit <- survival::survfit(
    survival::Surv(dt[[fup_days]], dt[[event_count]]) ~ group,
    data = dt,
    weights = dt[[weights]]
  )
  ll <- .tidy_survfit(fit)
  ll <- ll[time > 0 & surv > 0 & surv < 1]
  ll[, `:=`(
    log_time = log(time),
    cloglog = log(-log(surv))
  )]
  ll[,
    group_label := factor(
      group,
      levels = c("control", "exposed"),
      labels = c("CapOx", "CapOx-B")
    )
  ]

  ggplot2::ggplot(
    ll,
    ggplot2::aes(x = log_time, y = cloglog, color = group_label)
  ) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("darkblue", "darkred")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "log(follow-up time in days)",
      y = "log(-log(survival probability))"
    )
}

# ' log(-log(S(t))) vs log(t) by group using Weibull AFT model
plot_loglog_weibull <- function(dt, fup_days, event_count, weights) {

  dt <- .group_factor(data.table::as.data.table(dt)[
    include_after_trimming == TRUE
  ])

  # Fit Weibull model
  fit <- flexsurv::flexsurvreg(
    stats::as.formula(
      paste0("survival::Surv(", fup_days, ", ", event_count, ") ~ group")
    ),
    data = dt,
    dist = "weibull",
    weights = dt[[weights]])

  # Prediction times
  times <- seq(
    1,
    max(dt[[fup_days]], na.rm = TRUE),
    length.out = 300
  )

  # Predictions for each group
  pred_list <- lapply(levels(factor(dt$group)), function(g) {

    s <- summary(
      fit,
      t = times,
      newdata = data.frame(group = g),
      ci = FALSE
    )[[1]]

    data.table::data.table(
      time = s$time,
      surv = s$est,
      group = g
    )
  })

  ll <- data.table::rbindlist(pred_list)

  ll <- ll[
    surv > 0 & surv < 1
  ][,
    `:=`(
      log_time = log(time),
      cloglog = log(-log(surv))
    )
  ]

  ll[, group_label := factor(
    group,
    levels = c("control", "exposed"),
    labels = c("CapOx", "CapOx-B")
  )]

  ggplot2::ggplot(
    ll,
    ggplot2::aes(
      x = log_time,
      y = cloglog,
      colour = group_label
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(
      values = c("darkblue", "darkred")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "log(follow-up time in days)",
      y = "log(-log(survival probability))"
    )
}

#' Plot absolute standardized differences (ASD) before and after matching
#' 
#' @param dt_unmatched Data.table containing covariate information for the unmatched dataset
#' @param dt_matched Data.table containing covariate information for the matched dataset
#' @param metadata Data.table containing metadata for the covariates
plot_asd <- function(dt_unmatched, dt_matched, metadata){

# compute ASD for unmatched and matched datasets
asd_unmatched <- DescriptiveTable::compute_asd(
  CovariatesInformation = dt_unmatched,
  metadata = metadata,
  referenceExtension = "unmatched",
  use_weights = FALSE,
  group_names = c("CONTROL", "EXPOSED")
)
asd_matched <- DescriptiveTable::compute_asd(
  CovariatesInformation = dt_matched,
  metadata = metadata,
  referenceExtension = "matched",
  use_weights = "ip_weight",
  group_names = c("CONTROL", "EXPOSED")
)

# merge matched and unmatched asds
asds_combined <-  merge(
    asd_unmatched,
    asd_matched[, .(var, asd_matched)],
    by = "var"
  )

# merge the labels from table_metadata
asds_combined <- merge(
  asds_combined,
  metadata[, .(var, label)],
  by = "var"
)

# prepare data for the plot in long format
asd_plot_data <- data.table::melt(
    asds_combined,
    id.vars = c("var", "type", "label"),
    measure.vars = c("asd_unmatched", "asd_matched"),
    variable.name = "diagnostic_stage",
    value.name = "asd"
  )
asd_plot_data[, asd := as.numeric(asd)]
asd_plot_data[, diagnostic_stage := factor(
  diagnostic_stage,
  levels = c("asd_unmatched", "asd_matched"),
  labels = c("Before matching", "After matching")
)
]
asd_plot_data[, label := data.table::fifelse(is.na(label), var, label)]
# For the metastasis rows, add "Metastasis: " to the label
asd_plot_data[grepl("SV_METASTASIS", var), label := paste0("Metastasis: ", label)]
# Also replace female label with "Sex"
asd_plot_data[grepl("Female", label), label := "Sex"]

# order the labels based on the unmatched ASD values
var_order <- asd_plot_data[
  diagnostic_stage == "Before matching"
][order(asd), label]
asd_plot_data[, label := factor(label, levels = var_order)]

fig_asd <- ggplot2::ggplot(
  asd_plot_data[!is.na(asd)],
  ggplot2::aes(
    x = label,
    y = asd,
    color = diagnostic_stage,
    shape = diagnostic_stage,
    group = diagnostic_stage
  )
) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::geom_hline(
    yintercept = 0.1,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_shape_manual(values = c(1, 1)) +
  ggplot2::scale_color_manual(values = c("darkred", "darkblue")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Absolute Standardised Difference"
  )
 
 # return a list with asds_combined and the figure
 return(list(asds_combined = asds_combined, figure_asd = fig_asd))  
}


# Unweighted versions of these functions

#' Scaled Schoenfeld residuals from the unweighted Cox model
plot_schoenfeld_unw <- function(dt, fup_days, event_count) {
  dt <- .group_factor(data.table::as.data.table(dt))

  fit <- survival::coxph(
    survival::Surv(dt[[fup_days]], dt[[event_count]]) ~ group,
    data = dt,
    cluster = person_id,
    robust = TRUE
  )
  zph <- survival::cox.zph(fit)

  resid <- data.table::data.table(
    time_months = zph$time / DAYS_PER_MONTH,
    residual = as.numeric(zph$y[, 1])
  )

  ggplot2::ggplot(resid, ggplot2::aes(x = time_months, y = residual)) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "grey40"
    ) +
    ggplot2::geom_point(size = 2, alpha = 0.6) +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = TRUE,
      color = "darkblue"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Follow-up time (months)",
      y = "Scaled Schoenfeld residual (group)"
    )
}

#' log(-log(S(t))) vs log(t) by group
plot_loglog_unw <- function(dt, fup_days, event_count) {
  dt <- .group_factor(data.table::as.data.table(dt))
  fit <- survival::survfit(
    survival::Surv(dt[[fup_days]], dt[[event_count]]) ~ group,
    data = dt
  )
  ll <- .tidy_survfit(fit)
  ll <- ll[time > 0 & surv > 0 & surv < 1]
  ll[, `:=`(
    log_time = log(time),
    cloglog = log(-log(surv))
  )]
  ll[,
    group_label := factor(
      group,
      levels = c("control", "exposed"),
      labels = c("CapOx", "CapOx-B")
    )
  ]

  ggplot2::ggplot(
    ll,
    ggplot2::aes(x = log_time, y = cloglog, color = group_label)
  ) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("darkblue", "darkred")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "log(follow-up time in days)",
      y = "log(-log(survival probability))"
    )
}

# ' log(-log(S(t))) vs log(t) by group using Weibull AFT model
plot_loglog_weibull_unw <- function(dt, fup_days, event_count) {

  dt <- .group_factor(data.table::as.data.table(dt))

  # Fit Weibull model
  fit <- flexsurv::flexsurvreg(
    stats::as.formula(
      paste0("survival::Surv(", fup_days, ", ", event_count, ") ~ group")
    ),
    data = dt,
    dist = "weibull")

  # Prediction times
  times <- seq(
    1,
    max(dt[[fup_days]], na.rm = TRUE),
    length.out = 300
  )

  # Predictions for each group
  pred_list <- lapply(levels(factor(dt$group)), function(g) {

    s <- summary(
      fit,
      t = times,
      newdata = data.frame(group = g),
      ci = FALSE
    )[[1]]

    data.table::data.table(
      time = s$time,
      surv = s$est,
      group = g
    )
  })

  ll <- data.table::rbindlist(pred_list)

  ll <- ll[
    surv > 0 & surv < 1
  ][,
    `:=`(
      log_time = log(time),
      cloglog = log(-log(surv))
    )
  ]

  ll[, group_label := factor(
    group,
    levels = c("control", "exposed"),
    labels = c("CapOx", "CapOx-B")
  )]

  ggplot2::ggplot(
    ll,
    ggplot2::aes(
      x = log_time,
      y = cloglog,
      colour = group_label
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(
      values = c("darkblue", "darkred")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "log(follow-up time in days)",
      y = "log(-log(survival probability))"
    )
}
