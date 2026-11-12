## A function that fits a KM model to create a data.table with the survival and cumulative event
## estimates along with their confidence intervals for each group in the dataset.
## These will be used as input data to create the cumulative event curves with confidence intervals.
## option to take the name of a weight column (e.g. "ip_weight") to fit a weighted KM
KM_fit_data <- function(dt, time_col, event_col, group_col, weights = NULL) {
  data.table::setDT(dt)

  if (!is.null(weights) && !weights %in% names(dt)) {
    logger::log_warn(
      "Weight column '{weights}' not found in dataset"
    )

    return(data.table::data.table())
  }

  fit_args <- list(
    formula = survival::Surv(get(time_col), get(event_col)) ~ get(group_col),
    data = dt
  )

  # Add weights only if supplied
  if (!is.null(weights)) {
    fit_args$weights <- dt[[weights]]
  }

  surv_fit <- do.call(survival::survfit, fit_args)

  df_surv_fit <- data.table::data.table(
    strata = rep(names(surv_fit$strata), surv_fit$strata),
    time = surv_fit$time,
    n_risk = surv_fit$n.risk,
    n_event = surv_fit$n.event,
    n_censor = surv_fit$n.censor,
    surv = surv_fit$surv,
    surv_ci_lwb = surv_fit$lower,
    surv_ci_upb = surv_fit$upper
  )

  df_surv_fit[,
    group := sub("^get\\(group_col\\)=", "", strata)
  ]

  df_surv_fit[,
    `:=`(
      cumevent = 1 - surv,
      cumevent_ci_lwb = 1 - surv_ci_upb,
      cumevent_ci_upb = 1 - surv_ci_lwb
    )
  ]

  df_surv_fit
}


#' @description
#' Convert grouped survival estimates into stepwise format suitable for
#' geom_step() and geom_ribbon().
#'
#' @details
#' Kaplan-Meier curves are step functions. This helper expands observations
#' within each group/stratum so that point estimates and confidence intervals
#' can be displayed correctly using ggplot2.
#'
#' For each group, consecutive observations are duplicated to create
#' horizontal segments between event times.
#'
#' @param data data.table or data.frame containing aggregated survival
#'   estimates.
#' @param group_col String. Column identifying groups/strata.
#' @param time_col String. Column containing follow-up time.
#' @param ycols Character vector containing columns that should be expanded
#'   into stepwise format (e.g. point estimates and confidence interval
#'   bounds).
#'
#' @return A data.table containing one row per step segment and group,
#'   suitable for use with geom_step() and geom_ribbon().
make_step_df <- function(data, group_col, time_col, ycols) {
  data.table::setDT(data)

  out <- data[
    order(get(time_col)),
    {
      times <- get(time_col)

      if (.N == 1) {
        tmp <- .SD[, ..ycols]
        tmp[[time_col]] <- times
      } else {
        idx <- seq_len(.N - 1)

        tmp <- data.table::data.table(
          time = c(
            rbind(times[idx], times[idx + 1]),
            times[.N]
          )
        )

        for (col in ycols) {
          y <- get(col)

          tmp[[col]] <- c(
            rbind(y[idx], y[idx]),
            y[.N]
          )
        }
      }

      tmp
    },
    by = group_col
  ]

  data.table::setnames(out, "time", time_col)

  out[]
}

#' @description
#' Create Kaplan-Meier survival curves or cumulative event curves with
#' confidence interval bands from aggregated survfit-style data.
#'
#' @details
#' The input data should contain one row per event time and group,
#' typically derived from a survfit object. Multiple groups are supported.
#'
#' The input data must contain:
#' 1) a grouping variable identifying the curve
#' 2) time points (x-axis)
#' 3) point estimates at each time point (y-axis)
#' 4) lower confidence interval bounds
#' 5) upper confidence interval bounds
#'
#' Internally, the function expands the data into stepwise format so that
#' geom_step() and geom_ribbon() correctly reproduce Kaplan-Meier-style
#' curves and confidence bands.
#'
#' @param data data.table or data.frame containing aggregated survival
#'   estimates.
#' @param group_col String. Column identifying groups/strata.
#' @param time_col String. Column containing follow-up time.
#' @param point_est_col String. Column containing point estimates
#'   (e.g. survival probability or cumulative incidence).
#' @param ci_lwb_col String. Column containing lower confidence limits.
#' @param ci_upb_col String. Column containing upper confidence limits.
#' @param plot_ci_opacity Numeric. Opacity of the confidence interval band.
#' @param plot_title String. Plot title. Use "" for no title.
#' @param plot_x_axis_title String. X-axis label.
#' @param plot_y_axis_title String. Y-axis label.
#' @param plot_x_lim_break Numeric vector of length 3:
#'   c(min, max, break_size) for the x-axis.
#' @param plot_y_lim_break Numeric vector of length 3:
#'   c(min, max, break_size) for the y-axis.
#' @param plot_line_thickness Numeric. Line width of the KM curve.
#' @param plot_font_title_size Numeric. Title font size.
#' @param plot_font_axis_size Numeric. Axis label and tick font size.
#'
#' @return A ggplot object containing one or more Kaplan-Meier or cumulative
#'   event curves with confidence interval bands.
create_km_with_input_data <- function(
  data,
  group_col,
  time_col,
  point_est_col,
  ci_lwb_col,
  ci_upb_col,
  plot_ci_opacity = 0.25,
  plot_title = "",
  plot_x_axis_title,
  plot_y_axis_title,
  plot_x_lim_break = c(0, 728, 30),
  plot_y_lim_break = c(0, 1, 0.25),
  plot_line_thickness = 0.5,
  plot_font_title_size = 13,
  plot_font_axis_size = 13
) {
  data.table::setDT(data)

  step_df <- make_step_df(
    data = data,
    group_col = group_col,
    time_col = time_col,
    ycols = c(
      point_est_col,
      ci_lwb_col,
      ci_upb_col
    )
  )
  ggplot2::ggplot(
    step_df,
    ggplot2::aes(
      x = .data[[time_col]],
      y = .data[[point_est_col]],
      colour = .data[[group_col]],
      fill = .data[[group_col]],
      group = .data[[group_col]]
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data[[ci_lwb_col]],
        ymax = .data[[ci_upb_col]]
      ),
      alpha = plot_ci_opacity,
      colour = NA
    ) +
    ggplot2::geom_step(
      direction = "hv",
      linewidth = plot_line_thickness
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(
        plot_x_lim_break[1],
        plot_x_lim_break[2],
        plot_x_lim_break[3]
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = plot_y_lim_break[1:2],
      breaks = seq(
        plot_y_lim_break[1],
        plot_y_lim_break[2],
        plot_y_lim_break[3]
      )
    ) +
    ggplot2::labs(
      title = plot_title,
      x = plot_x_axis_title,
      y = plot_y_axis_title,
      colour = NULL,
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = plot_font_title_size),
      text = ggplot2::element_text(size = plot_font_axis_size),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black")
    )
}
