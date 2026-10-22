################################################################################
## Estimation and Inference in Boundary Discontinuity Designs:
## Location-Based Methods
## Empirical Application: Figures
################################################################################
## Run CTY_2027_JASA--empapp.R first. This file reads its saved CSV results.
## Shared drawing functions appear first, followed by explicit calls in the
## order of the paper's Figures 1--2 and supplemental Figures SA-1--SA-12.
## Each call names its output PNG in figures/. Additional outputs appear last.
################################################################################

rm(list = ls(all = TRUE))

# Rscript uses the folder containing this file. For interactive execution,
# set the working directory to the replication folder first.
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) > 0) {
  setwd(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package ggplot2 is required to generate figures.", call. = FALSE)
}
output_dir <- "output"
figures_dir <- "figures"
dir.create(figures_dir, showWarnings = FALSE)

# Replace the generated figures when running this file in full.
old_files <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
if (length(old_files) > 0) unlink(old_files)


################################################################################
## Shared Input and Drawing Functions
################################################################################

read_output_csv <- function(file) {
  path <- file.path(output_dir, file)
  if (!file.exists(path)) {
    stop(sprintf("Missing output file: %s. Run CTY_2027_JASA--empapp.R first.", path), call. = FALSE)
  }
  read.csv(path, check.names = FALSE)
}

read_result_table <- function(file) {
  tab <- read_output_csv(file)
  required <- c(
    "row", "b1", "b2", "estimate.p", "estimate.q", "p.value",
    "ci.lower", "ci.upper", "cb.lower", "cb.upper"
  )
  missing <- setdiff(required, names(tab))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing columns: %s", file, paste(missing, collapse = ", ")), call. = FALSE)
  }

  tab$row <- as.character(tab$row)
  tab
}

read_percent_table <- function(file) {
  tab <- read_output_csv(file)
  required <- c("row", "itt.percent", "fuzzy.percent")
  missing <- setdiff(required, names(tab))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing columns: %s", file, paste(missing, collapse = ", ")), call. = FALSE)
  }

  tab$row <- as.character(tab$row)
  tab
}

point_rows <- function(tab) tab[!(tab$row %in% c("WBATE", "LBATE")), , drop = FALSE]

aggregate_value <- function(tab, name) {
  row <- tab[tab$row == name, , drop = FALSE]
  if (nrow(row) == 0 || !is.finite(row$estimate.p[1])) return(NA_real_)
  row$estimate.p[1]
}

################################ Formatting ####################################

eval_axis_breaks <- c(1, 5, 10, 15, 21, 25, 30, 35, 40)
eval_axis_labels <- paste0("b", eval_axis_breaks)

theme_rd2d <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 13),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = c(0.78, 0.9),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.width = grid::unit(0.55, "cm"),
      legend.key.height = grid::unit(0.45, "cm"),
      legend.margin = ggplot2::margin(3, 4, 3, 4),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_ylim <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[is.finite(vals)]
  lim <- range(vals)
  pad <- max(0.02, diff(lim) * 0.08)
  c(lim[1] - pad, lim[2] + pad)
}

b21_line <- function() {
  ggplot2::geom_vline(xintercept = 21, color = "grey90", linewidth = 0.45)
}

point_legend_theme <- function(id) {
  if (id %in% c("empapp_main_fuzzy", "empapp_main_itt")) {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0)
    ))
  }
  if (id == "empapp_main_fs") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.62),
      legend.justification = c(0, 0.5)
    ))
  }
  if (id == "empapp_main_itt0") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.52),
      legend.justification = c(0, 0.5)
    ))
  }
  if (id == "empapp_covbal_itt") {
    return(ggplot2::theme(
      legend.position = c(0.72, 0.08),
      legend.justification = c(0, 0)
    ))
  }
  ggplot2::theme(legend.position = "right", legend.justification = "center")
}

inference_legend_theme <- function(id) {
  if (id == "empapp_main_fuzzy") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0)
    ))
  }
  if (id == "empapp_main_itt") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0)
    ))
  }
  if (id == "empapp_main_itt0") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.55),
      legend.justification = c(0, 0.5)
    ))
  }
  if (id == "empapp_main_fs") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1)
    ))
  }
  if (id == "empapp_covbal_itt") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1)
    ))
  }
  ggplot2::theme()
}

heatmap_legend_theme <- function() {
  ggplot2::theme(
    legend.position = c(0.64, 0.86),
    legend.justification = c(0, 1)
  )
}

save_png <- function(plot, file, width = 6, height = 5) {
  device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggplot2::ggsave(
    filename = file.path(figures_dir, file),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    device = device
  )
  invisible(plot)
}

################################ Scatter Plot ##################################

save_scatter_plot <- function(dat, eval, file) {
  eval_labeled <- data.frame(
    eval,
    indx = seq_len(nrow(eval)),
    label = sprintf("b%d", seq_len(nrow(eval)))
  )
  eval_labeled <- eval_labeled[eval_labeled$indx %in% eval_axis_breaks, , drop = FALSE]
  eval_labeled$label_x <- ifelse(eval_labeled$x.1 == 0, -3.2, eval_labeled$x.1)
  eval_labeled$label_y <- ifelse(eval_labeled$x.1 == 0, eval_labeled$x.2, -2.4)
  eval_labeled$label_hjust <- ifelse(eval_labeled$x.1 == 0, 1, 0.5)
  eval_labeled$label_vjust <- ifelse(eval_labeled$x.1 == 0, 0.5, 1)
  boundary_segments <- data.frame(
    x = c(0, 0),
    xend = c(0, 80),
    y = c(0, 0),
    yend = c(55, 0),
    group = "Boundary",
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = dat,
      ggplot2::aes(x = x.1, y = x.2, color = factor(assignment), shape = factor(assignment)),
      alpha = 0.18,
      size = 0.35
    ) +
    ggplot2::geom_segment(
      data = boundary_segments,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = group),
      linewidth = 1.0,
      linetype = "solid",
      lineend = "butt"
    ) +
    ggplot2::geom_point(
      data = eval,
      ggplot2::aes(x = x.1, y = x.2),
      size = 0.65
    ) +
    ggplot2::geom_text(
      data = eval_labeled,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label,
        hjust = label_hjust,
        vjust = label_vjust
      ),
      color = "black",
      size = 2.7
    ) +
    ggplot2::scale_color_manual(
      values = c("0" = "indianred2", "1" = "dodgerblue4", "Boundary" = "grey45"),
      breaks = c("0", "1", "Boundary"),
      name = NULL,
      labels = c("Control", "Treatment", "Boundary")
    ) +
    ggplot2::scale_shape_manual(
      values = c("0" = 15, "1" = 16),
      name = NULL,
      labels = c("Control", "Treatment")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          alpha = c(1, 1, 1),
          size = c(2, 2, 0),
          shape = c(15, 16, NA),
          linetype = c("blank", "blank", "solid"),
          linewidth = c(0, 0, 1)
        )
      ),
      shape = "none"
    ) +
    ggplot2::coord_cartesian(xlim = c(-80, 100), ylim = c(-40, 60)) +
    ggplot2::labs(x = "Saber 11", y = "Sisben") +
    theme_rd2d() +
    ggplot2::theme(
      legend.position = c(0.23, 0.88),
      legend.justification = c(0.5, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  save_png(p, file)
}

############################### Result Figures ################################

save_point_plot <- function(tab, id, ylab, file) {
  pts <- point_rows(tab)
  pts$indx <- seq_len(nrow(pts))
  wbate <- aggregate_value(tab, "WBATE")
  lbate <- aggregate_value(tab, "LBATE")
  line_df <- data.frame(
    label = c("WBATE", "LBATE"),
    y = c(wbate, lbate),
    stringsAsFactors = FALSE
  )
  line_df <- line_df[is.finite(line_df$y), , drop = FALSE]

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = indx, y = estimate.p)) +
    b21_line() +
    ggplot2::geom_point(ggplot2::aes(color = "Estimate"), size = 1.35) +
    ggplot2::geom_hline(
      data = line_df,
      ggplot2::aes(yintercept = y, color = label, linetype = label),
      linewidth = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Estimate" = "black", "WBATE" = "dodgerblue4", "LBATE" = "firebrick"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Estimate" = "blank", "WBATE" = "dotted", "LBATE" = "longdash"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, NA, NA),
          linetype = c("blank", "dotted", "longdash"),
          linewidth = c(0, 0.6, 0.6)
        )
      ),
      linetype = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = eval_axis_breaks, labels = eval_axis_labels) +
    ggplot2::coord_cartesian(xlim = c(1, 40), ylim = plot_ylim(pts$estimate.p, wbate, lbate)) +
    ggplot2::labs(x = "Cutoffs on the boundary", y = ylab) +
    theme_rd2d() +
    point_legend_theme(id) +
    ggplot2::theme(
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  save_png(p, file)
}

save_percent_point_plot <- function(tab, value_col, ylab, file) {
  pts <- point_rows(tab)
  pts$indx <- seq_len(nrow(pts))
  pts$value <- pts[[value_col]]

  aggregates <- tab[tab$row %in% c("WBATE", "LBATE"), , drop = FALSE]
  line_df <- data.frame(
    label = aggregates$row,
    value = aggregates[[value_col]],
    stringsAsFactors = FALSE
  )
  line_df <- line_df[is.finite(line_df$value), , drop = FALSE]

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = indx, y = value)) +
    b21_line() +
    ggplot2::geom_point(ggplot2::aes(color = "Estimate"), size = 1.35) +
    ggplot2::geom_hline(
      data = line_df,
      ggplot2::aes(yintercept = value, color = label, linetype = label),
      linewidth = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Estimate" = "black", "WBATE" = "dodgerblue4", "LBATE" = "firebrick"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE"),
      labels = c("Estimate", "Average", "Maximum")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Estimate" = "blank", "WBATE" = "dotted", "LBATE" = "longdash"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE"),
      labels = c("Estimate", "Average", "Maximum")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, NA, NA),
          linetype = c("blank", "dotted", "longdash"),
          linewidth = c(0, 0.6, 0.6)
        )
      ),
      linetype = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = eval_axis_breaks, labels = eval_axis_labels) +
    ggplot2::coord_cartesian(
      xlim = c(1, 40),
      ylim = plot_ylim(pts$value, line_df$value)
    ) +
    ggplot2::labs(x = "Cutoffs on the boundary", y = ylab) +
    theme_rd2d() +
    ggplot2::theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  save_png(p, file)
}

save_inference_plot <- function(tab, id, ylab, file) {
  pts <- point_rows(tab)
  pts$indx <- seq_len(nrow(pts))
  pts$band.lower <- ifelse(is.finite(pts$cb.lower), pts$cb.lower, pts$ci.lower)
  pts$band.upper <- ifelse(is.finite(pts$cb.upper), pts$cb.upper, pts$ci.upper)

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = indx)) +
    b21_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = band.lower, ymax = band.upper, fill = "95% CB", color = "95% CB"),
      alpha = 0.14,
      linewidth = 0
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci.lower, ymax = ci.upper, color = "95% CI"),
      width = 0.12,
      linewidth = 0.35
    ) +
    ggplot2::geom_point(ggplot2::aes(y = estimate.p, color = "Estimate"), size = 1.2) +
    ggplot2::scale_color_manual(
      values = c("Estimate" = "black", "95% CI" = "black", "95% CB" = "dodgerblue4"),
      name = NULL,
      breaks = c("Estimate", "95% CI", "95% CB")
    ) +
    ggplot2::scale_fill_manual(values = c("95% CB" = "dodgerblue4"), name = NULL) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, NA, 22),
          linetype = c("blank", "solid", "blank"),
          fill = c(NA, NA, "dodgerblue4"),
          alpha = c(1, 1, 0.14),
          linewidth = c(0, 0.35, 0)
        )
      ),
      fill = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = eval_axis_breaks, labels = eval_axis_labels) +
    ggplot2::coord_cartesian(
      xlim = c(1, 40),
      ylim = plot_ylim(pts$estimate.p, pts$ci.lower, pts$ci.upper, pts$band.lower, pts$band.upper)
    ) +
    ggplot2::labs(x = "Cutoffs on the boundary", y = ylab) +
    theme_rd2d() +
    inference_legend_theme(id)

  save_png(p, file)
}

save_effect_heatmap <- function(tab, eval, label, file) {
  pts <- point_rows(tab)
  data_plot <- data.frame(eval, estimate.p = pts$estimate.p)
  data_plot$label <- sprintf("%02d", seq_len(nrow(data_plot)))
  mid <- stats::median(data_plot$estimate.p, na.rm = TRUE)

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x.1, y = x.2)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = estimate.p),
      color = "white",
      linewidth = 0.35,
      show.legend = TRUE
    ) +
    ggplot2::scale_fill_gradient2(
      low = "dodgerblue4",
      mid = "white",
      high = "firebrick",
      midpoint = mid,
      name = label
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      color = "black",
      size = 2.35
    ) +
    ggplot2::coord_fixed(ratio = 56 / 40, xlim = c(-10, 64), ylim = c(-10, 42)) +
    ggplot2::labs(x = "Saber 11", y = "Sisben") +
    theme_rd2d() +
    heatmap_legend_theme()

  save_png(p, file)
}

save_pvalue_heatmap <- function(tab, eval, label, file) {
  pts <- point_rows(tab)
  data_plot <- data.frame(eval, p.value = pts$p.value)
  data_plot$p.sig <- cut(
    data_plot$p.value,
    breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    labels = c("[0.000, 0.001]", "(0.001, 0.010]", "(0.010, 0.050]", "(0.050, 0.100]", "(0.100, 1.000]"),
    include.lowest = TRUE
  )
  sig_colors <- c(
    "[0.000, 0.001]" = "#b2182b",
    "(0.001, 0.010]" = "#ef8a62",
    "(0.010, 0.050]" = "#fddbc7",
    "(0.050, 0.100]" = "#d1e5f0",
    "(0.100, 1.000]" = "#67a9cf"
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x.1, y = x.2, fill = p.sig)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35, show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = sig_colors, name = label, drop = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%02d", seq_len(nrow(data_plot)))),
      color = "black",
      size = 2.35
    ) +
    ggplot2::coord_fixed(ratio = 56 / 40, xlim = c(-10, 64), ylim = c(-10, 42)) +
    ggplot2::labs(x = "Saber 11", y = "Sisben") +
    theme_rd2d() +
    heatmap_legend_theme()

  save_png(p, file)
}

################################################################################
## Read Saved Results
################################################################################

data <- read_output_csv("empapp_data.csv")
eval <- read_output_csv("empapp_eval.csv")

# ITT bandwidth selection.
fuzzy.bwitt <- read_result_table("empapp_main_fuzzy_bwitt.csv")
itt.bwitt <- read_result_table("empapp_main_itt_bwitt.csv")
fs.bwitt <- read_result_table("empapp_main_fs_bwitt.csv")
baseline.bwitt <- read_result_table("empapp_main_itt0_bwitt.csv")
balance.bwitt <- read_result_table("empapp_covbal_itt_bwitt.csv")
percent.bwitt <- read_percent_table("empapp_main_percent_bwitt.csv")

# Fuzzy bandwidth selection.
fuzzy.bwmain <- read_result_table("empapp_main_fuzzy_bwmain.csv")
itt.bwmain <- read_result_table("empapp_main_itt_bwmain.csv")
fs.bwmain <- read_result_table("empapp_main_fs_bwmain.csv")
baseline.bwmain <- read_result_table("empapp_main_itt0_bwmain.csv")
balance.bwmain <- read_result_table("empapp_covbal_itt_bwmain.csv")
percent.bwmain <- read_percent_table("empapp_main_percent_bwmain.csv")


################################################################################
## Figure 1: Assignment Boundary and Treatment Effects (ITT Bandwidths)
################################################################################

# Figure 1(a): Scatterplot and boundary.
# Input: output/empapp_data.csv, output/empapp_eval.csv.
save_scatter_plot(data, eval, file = "empapp_scatter_bwitt.png")

# Figure 1(b): Point estimates, WBATE, and LBATE.
# Input: output/empapp_main_itt_bwitt.csv.
save_point_plot(itt.bwitt, id = "empapp_main_itt",
               ylab = "Intention-to-treat effect",
               file = "empapp_main_itt_pointest_bwitt.png")


################################################################################
## Figure 2: Empirical Results (ITT Bandwidths)
################################################################################

# Figure 2(a): Intention-to-treat effects, intervals, and bands.
# Input: output/empapp_main_itt_bwitt.csv.
save_inference_plot(itt.bwitt, id = "empapp_main_itt",
                   ylab = "Intention-to-treat effect",
                   file = "empapp_main_itt_inference_bwitt.png")

# Figure 2(b): First stage effects, intervals, and bands.
# Input: output/empapp_main_fs_bwitt.csv.
save_inference_plot(fs.bwitt, id = "empapp_main_fs",
                   ylab = "First stage",
                   file = "empapp_main_fs_inference_bwitt.png")

# Figure 2(c): Fuzzy estimates, intervals, and bands.
# Input: output/empapp_main_fuzzy_bwitt.csv.
save_inference_plot(fuzzy.bwitt, id = "empapp_main_fuzzy",
                   ylab = "Fuzzy estimate",
                   file = "empapp_main_fuzzy_inference_bwitt.png")

# Figure 2(d): Covariate balance for mother's education, intervals, and bands.
# Input: output/empapp_covbal_itt_bwitt.csv.
save_inference_plot(balance.bwitt, id = "empapp_covbal_itt",
                   ylab = "Covariate balance ITT",
                   file = "empapp_covbal_itt_inference_bwitt.png")


################################################################################
## Figure SA-1: Intention-to-treat effects (ITT Bandwidths)
################################################################################

# Figure SA-1(a): Heatmap of point estimates.
# Input: output/empapp_main_itt_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(itt.bwitt, eval, label = "ITT",
                   file = "empapp_main_itt_heatmap_bwitt.png")

# Figure SA-1(b): Heatmap of pointwise p-values.
# Input: output/empapp_main_itt_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(itt.bwitt, eval, label = "ITT",
                   file = "empapp_main_itt_heatmap_pval_bwitt.png")


################################################################################
## Figure SA-2: First stage effects (ITT Bandwidths)
################################################################################

# Figure SA-2(a): Heatmap of point estimates.
# Input: output/empapp_main_fs_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(fs.bwitt, eval, label = "FS",
                   file = "empapp_main_fs_heatmap_bwitt.png")

# Figure SA-2(b): Heatmap of pointwise p-values.
# Input: output/empapp_main_fs_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fs.bwitt, eval, label = "FS",
                   file = "empapp_main_fs_heatmap_pval_bwitt.png")


################################################################################
## Figure SA-3: Fuzzy estimates (ITT Bandwidths)
################################################################################

# Figure SA-3(a): Heatmap of point estimates.
# Input: output/empapp_main_fuzzy_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(fuzzy.bwitt, eval, label = "Fuzzy",
                   file = "empapp_main_fuzzy_heatmap_bwitt.png")

# Figure SA-3(b): Heatmap of pointwise p-values.
# Input: output/empapp_main_fuzzy_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fuzzy.bwitt, eval, label = "Fuzzy",
                   file = "empapp_main_fuzzy_heatmap_pval_bwitt.png")


################################################################################
## Figure SA-4: Covariate balance for mother's education (ITT Bandwidths)
################################################################################

# Figure SA-4(a): Heatmap of point estimates.
# Input: output/empapp_covbal_itt_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(balance.bwitt, eval, label = "Cov. Bal.",
                   file = "empapp_covbal_itt_heatmap_bwitt.png")

# Figure SA-4(b): Heatmap of pointwise p-values.
# Input: output/empapp_covbal_itt_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(balance.bwitt, eval, label = "Cov. Bal.",
                   file = "empapp_covbal_itt_heatmap_pval_bwitt.png")


################################################################################
## Figure SA-5: Control-Side Baseline (ITT Bandwidths)
################################################################################

# Figure SA-5(a): Point estimates, intervals, and bands.
# Input: output/empapp_main_itt0_bwitt.csv.
save_inference_plot(baseline.bwitt, id = "empapp_main_itt0",
                   ylab = "Control-side baseline outcome",
                   file = "empapp_main_itt0_inference_bwitt.png")

# Figure SA-5(b): Heatmap of point estimates.
# Input: output/empapp_main_itt0_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(baseline.bwitt, eval, label = "Control baseline",
                   file = "empapp_main_itt0_heatmap_bwitt.png")


################################################################################
## Figure SA-6: Relative Effects (ITT Bandwidths)
################################################################################

# Figure SA-6(a): ITT percentages and their average and maximum.
# Input: output/empapp_main_percent_bwitt.csv.
save_percent_point_plot(percent.bwitt, value_col = "itt.percent",
                       ylab = "ITT / ITT.0 (%)",
                       file = "empapp_main_percent_itt_pointest_bwitt.png")

# Figure SA-6(b): Fuzzy percentages and their average and maximum.
# Input: output/empapp_main_percent_bwitt.csv.
save_percent_point_plot(percent.bwitt, value_col = "fuzzy.percent",
                       ylab = "Fuzzy / ITT.0 (%)",
                       file = "empapp_main_percent_fuzzy_pointest_bwitt.png")


################################################################################
## Figure SA-7: Intention-to-treat effects (Fuzzy Bandwidths)
################################################################################

# Figure SA-7(a): Heatmap of point estimates.
# Input: output/empapp_main_itt_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(itt.bwmain, eval, label = "ITT",
                   file = "empapp_main_itt_heatmap_bwmain.png")

# Figure SA-7(b): Heatmap of pointwise p-values.
# Input: output/empapp_main_itt_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(itt.bwmain, eval, label = "ITT",
                   file = "empapp_main_itt_heatmap_pval_bwmain.png")


################################################################################
## Figure SA-8: First stage effects (Fuzzy Bandwidths)
################################################################################

# Figure SA-8(a): Heatmap of point estimates.
# Input: output/empapp_main_fs_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(fs.bwmain, eval, label = "FS",
                   file = "empapp_main_fs_heatmap_bwmain.png")

# Figure SA-8(b): Heatmap of pointwise p-values.
# Input: output/empapp_main_fs_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fs.bwmain, eval, label = "FS",
                   file = "empapp_main_fs_heatmap_pval_bwmain.png")


################################################################################
## Figure SA-9: Fuzzy estimates (Fuzzy Bandwidths)
################################################################################

# Figure SA-9(a): Heatmap of point estimates.
# Input: output/empapp_main_fuzzy_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(fuzzy.bwmain, eval, label = "Fuzzy",
                   file = "empapp_main_fuzzy_heatmap_bwmain.png")

# Figure SA-9(b): Heatmap of pointwise p-values.
# Input: output/empapp_main_fuzzy_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fuzzy.bwmain, eval, label = "Fuzzy",
                   file = "empapp_main_fuzzy_heatmap_pval_bwmain.png")


################################################################################
## Figure SA-10: Covariate balance for mother's education (Fuzzy Bandwidths)
################################################################################

# Figure SA-10(a): Heatmap of point estimates.
# Input: output/empapp_covbal_itt_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(balance.bwmain, eval, label = "Cov. Bal.",
                   file = "empapp_covbal_itt_heatmap_bwmain.png")

# Figure SA-10(b): Heatmap of pointwise p-values.
# Input: output/empapp_covbal_itt_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(balance.bwmain, eval, label = "Cov. Bal.",
                   file = "empapp_covbal_itt_heatmap_pval_bwmain.png")


################################################################################
## Figure SA-11: Control-Side Baseline (Fuzzy Bandwidths)
################################################################################

# Figure SA-11(a): Point estimates, intervals, and bands.
# Input: output/empapp_main_itt0_bwmain.csv.
save_inference_plot(baseline.bwmain, id = "empapp_main_itt0",
                   ylab = "Control-side baseline outcome",
                   file = "empapp_main_itt0_inference_bwmain.png")

# Figure SA-11(b): Heatmap of point estimates.
# Input: output/empapp_main_itt0_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(baseline.bwmain, eval, label = "Control baseline",
                   file = "empapp_main_itt0_heatmap_bwmain.png")


################################################################################
## Figure SA-12: Relative Effects (Fuzzy Bandwidths)
################################################################################

# Figure SA-12(a): ITT percentages and their average and maximum.
# Input: output/empapp_main_percent_bwmain.csv.
save_percent_point_plot(percent.bwmain, value_col = "itt.percent",
                       ylab = "ITT / ITT.0 (%)",
                       file = "empapp_main_percent_itt_pointest_bwmain.png")

# Figure SA-12(b): Fuzzy percentages and their average and maximum.
# Input: output/empapp_main_percent_bwmain.csv.
save_percent_point_plot(percent.bwmain, value_col = "fuzzy.percent",
                       ylab = "Fuzzy / ITT.0 (%)",
                       file = "empapp_main_percent_fuzzy_pointest_bwmain.png")


################################################################################
## Additional Outputs
################################################################################

# These plots are retained in the replication files but do not appear in the
# current paper or supplement. The bandwidth choice is given in each filename.

# Additional output: Fuzzy estimates, point estimates.
# Input: output/empapp_main_fuzzy_bwitt.csv.
save_point_plot(fuzzy.bwitt, id = "empapp_main_fuzzy",
               ylab = "Fuzzy estimate",
               file = "empapp_main_fuzzy_pointest_bwitt.png")

# Additional output: First stage effects, point estimates.
# Input: output/empapp_main_fs_bwitt.csv.
save_point_plot(fs.bwitt, id = "empapp_main_fs",
               ylab = "First stage",
               file = "empapp_main_fs_pointest_bwitt.png")

# Additional output: Control-side baseline outcome, point estimates.
# Input: output/empapp_main_itt0_bwitt.csv.
save_point_plot(baseline.bwitt, id = "empapp_main_itt0",
               ylab = "Control-side baseline outcome",
               file = "empapp_main_itt0_pointest_bwitt.png")

# Additional output: Control-side baseline outcome, heatmap of p-values.
# Input: output/empapp_main_itt0_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(baseline.bwitt, eval, label = "Control baseline",
                   file = "empapp_main_itt0_heatmap_pval_bwitt.png")

# Additional output: Covariate balance for mother's education, point estimates.
# Input: output/empapp_covbal_itt_bwitt.csv.
save_point_plot(balance.bwitt, id = "empapp_covbal_itt",
               ylab = "Covariate balance ITT",
               file = "empapp_covbal_itt_pointest_bwitt.png")

# Additional output: Scatterplot and boundary (same data, alternative filename).
# Input: output/empapp_data.csv, output/empapp_eval.csv.
save_scatter_plot(data, eval, file = "empapp_scatter_bwmain.png")

# Additional output: Fuzzy estimates, point estimates.
# Input: output/empapp_main_fuzzy_bwmain.csv.
save_point_plot(fuzzy.bwmain, id = "empapp_main_fuzzy",
               ylab = "Fuzzy estimate",
               file = "empapp_main_fuzzy_pointest_bwmain.png")

# Additional output: Fuzzy estimates, intervals and bands.
# Input: output/empapp_main_fuzzy_bwmain.csv.
save_inference_plot(fuzzy.bwmain, id = "empapp_main_fuzzy",
                   ylab = "Fuzzy estimate",
                   file = "empapp_main_fuzzy_inference_bwmain.png")

# Additional output: Intention-to-treat effects, point estimates.
# Input: output/empapp_main_itt_bwmain.csv.
save_point_plot(itt.bwmain, id = "empapp_main_itt",
               ylab = "Intention-to-treat effect",
               file = "empapp_main_itt_pointest_bwmain.png")

# Additional output: Intention-to-treat effects, intervals and bands.
# Input: output/empapp_main_itt_bwmain.csv.
save_inference_plot(itt.bwmain, id = "empapp_main_itt",
                   ylab = "Intention-to-treat effect",
                   file = "empapp_main_itt_inference_bwmain.png")

# Additional output: First stage effects, point estimates.
# Input: output/empapp_main_fs_bwmain.csv.
save_point_plot(fs.bwmain, id = "empapp_main_fs",
               ylab = "First stage",
               file = "empapp_main_fs_pointest_bwmain.png")

# Additional output: First stage effects, intervals and bands.
# Input: output/empapp_main_fs_bwmain.csv.
save_inference_plot(fs.bwmain, id = "empapp_main_fs",
                   ylab = "First stage",
                   file = "empapp_main_fs_inference_bwmain.png")

# Additional output: Control-side baseline outcome, point estimates.
# Input: output/empapp_main_itt0_bwmain.csv.
save_point_plot(baseline.bwmain, id = "empapp_main_itt0",
               ylab = "Control-side baseline outcome",
               file = "empapp_main_itt0_pointest_bwmain.png")

# Additional output: Control-side baseline outcome, heatmap of p-values.
# Input: output/empapp_main_itt0_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(baseline.bwmain, eval, label = "Control baseline",
                   file = "empapp_main_itt0_heatmap_pval_bwmain.png")

# Additional output: Covariate balance for mother's education, point estimates.
# Input: output/empapp_covbal_itt_bwmain.csv.
save_point_plot(balance.bwmain, id = "empapp_covbal_itt",
               ylab = "Covariate balance ITT",
               file = "empapp_covbal_itt_pointest_bwmain.png")

# Additional output: Covariate balance for mother's education, intervals and bands.
# Input: output/empapp_covbal_itt_bwmain.csv.
save_inference_plot(balance.bwmain, id = "empapp_covbal_itt",
                   ylab = "Covariate balance ITT",
                   file = "empapp_covbal_itt_inference_bwmain.png")


################################################################################
## Check Saved Figures
################################################################################

expected_figures <- c(
  "empapp_scatter_bwitt.png",
  "empapp_main_itt_pointest_bwitt.png",
  "empapp_main_itt_inference_bwitt.png",
  "empapp_main_fs_inference_bwitt.png",
  "empapp_main_fuzzy_inference_bwitt.png",
  "empapp_covbal_itt_inference_bwitt.png",
  "empapp_main_itt_heatmap_bwitt.png",
  "empapp_main_itt_heatmap_pval_bwitt.png",
  "empapp_main_fs_heatmap_bwitt.png",
  "empapp_main_fs_heatmap_pval_bwitt.png",
  "empapp_main_fuzzy_heatmap_bwitt.png",
  "empapp_main_fuzzy_heatmap_pval_bwitt.png",
  "empapp_covbal_itt_heatmap_bwitt.png",
  "empapp_covbal_itt_heatmap_pval_bwitt.png",
  "empapp_main_itt0_inference_bwitt.png",
  "empapp_main_itt0_heatmap_bwitt.png",
  "empapp_main_percent_itt_pointest_bwitt.png",
  "empapp_main_percent_fuzzy_pointest_bwitt.png",
  "empapp_main_itt_heatmap_bwmain.png",
  "empapp_main_itt_heatmap_pval_bwmain.png",
  "empapp_main_fs_heatmap_bwmain.png",
  "empapp_main_fs_heatmap_pval_bwmain.png",
  "empapp_main_fuzzy_heatmap_bwmain.png",
  "empapp_main_fuzzy_heatmap_pval_bwmain.png",
  "empapp_covbal_itt_heatmap_bwmain.png",
  "empapp_covbal_itt_heatmap_pval_bwmain.png",
  "empapp_main_itt0_inference_bwmain.png",
  "empapp_main_itt0_heatmap_bwmain.png",
  "empapp_main_percent_itt_pointest_bwmain.png",
  "empapp_main_percent_fuzzy_pointest_bwmain.png",
  "empapp_main_fuzzy_pointest_bwitt.png",
  "empapp_main_fs_pointest_bwitt.png",
  "empapp_main_itt0_pointest_bwitt.png",
  "empapp_main_itt0_heatmap_pval_bwitt.png",
  "empapp_covbal_itt_pointest_bwitt.png",
  "empapp_scatter_bwmain.png",
  "empapp_main_fuzzy_pointest_bwmain.png",
  "empapp_main_fuzzy_inference_bwmain.png",
  "empapp_main_itt_pointest_bwmain.png",
  "empapp_main_itt_inference_bwmain.png",
  "empapp_main_fs_pointest_bwmain.png",
  "empapp_main_fs_inference_bwmain.png",
  "empapp_main_itt0_pointest_bwmain.png",
  "empapp_main_itt0_heatmap_pval_bwmain.png",
  "empapp_covbal_itt_pointest_bwmain.png",
  "empapp_covbal_itt_inference_bwmain.png"
)
missing_figures <- expected_figures[!file.exists(file.path(figures_dir, expected_figures))]
if (length(missing_figures) > 0) {
  stop(sprintf("Missing expected figures: %s", paste(missing_figures, collapse = ", ")), call. = FALSE)
}
too_small <- expected_figures[file.info(file.path(figures_dir, expected_figures))$size < 1000]
if (length(too_small) > 0) {
  stop(sprintf("Generated figure file(s) look empty: %s", paste(too_small, collapse = ", ")), call. = FALSE)
}

cat("Empirical application figures complete.\n")
cat(sprintf("Figures written to: %s\n", figures_dir))
