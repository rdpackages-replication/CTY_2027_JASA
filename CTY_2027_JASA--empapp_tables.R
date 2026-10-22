################################################################################
## Estimation and Inference in Boundary Discontinuity Designs:
## Location-Based Methods
## Empirical Application: Tables
################################################################################
## Run CTY_2027_JASA--empapp.R first. This file reads its saved CSV results.
## Shared formatting functions appear first, followed by explicit calls for
## Table 1 and supplemental Tables SA-1--SA-12. Additional fragments appear last.
## Each call names its output LaTeX fragment in tables/.
################################################################################

rm(list = ls(all = TRUE))

# Rscript uses the folder containing this file. For interactive execution,
# set the working directory to the replication folder first.
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) > 0) {
  setwd(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
}

output_dir <- "output"
tables_dir <- "tables"
dir.create(tables_dir, showWarnings = FALSE)

# Table 1 reports these boundary points and the WBATE/LBATE rows.
small_rows <- c(1, 5, 10, 15, 20, 25, 30, 35, 40)
aggregate_labels <- c("WBATE", "LBATE")

# Replace the generated empirical fragments when running this file in full.
old_files <- list.files(tables_dir, pattern = "^empapp_.*\\.tex$", full.names = TRUE)
if (length(old_files) > 0) unlink(old_files)


################################################################################
## Shared Input and Formatting Functions
################################################################################

read_result_table <- function(file, kind = "effect") {
  path <- file.path(output_dir, file)
  if (!file.exists(path)) {
    stop(sprintf("Missing output file: %s. Run CTY_2027_JASA--empapp.R first.", path), call. = FALSE)
  }

  tab <- read.csv(path, check.names = FALSE)
  required <- c("row", "h01", "h02", "h11", "h12", "N.Co", "N.Tr")
  if (kind == "effect") {
    required <- c(required, "estimate.p", "p.value", "ci.lower", "ci.upper")
  } else if (kind == "percent") {
    required <- c(required, "itt.percent", "fuzzy.percent")
  } else {
    stop(sprintf("Unknown table kind: %s", kind), call. = FALSE)
  }
  missing <- setdiff(required, names(tab))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing columns: %s", path, paste(missing, collapse = ", ")), call. = FALSE)
  }

  tab$row <- as.character(tab$row)
  tab
}

################################ Formatting ####################################

is_aggregate_row <- function(row_label) row_label %in% aggregate_labels

fmt_num <- function(x, digits = 3) {
  if (is.na(x) || !is.finite(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_pvalue <- function(x) {
  if (is.na(x) || !is.finite(x)) return("")
  if (x < 0.001) return("0.000")
  sprintf("%.3f", x)
}

fmt_interval <- function(lower, upper) {
  if (is.na(lower) || is.na(upper) || !is.finite(lower) || !is.finite(upper)) return("")
  sprintf("(%s, %s)", fmt_num(lower), fmt_num(upper))
}

fmt_row_label <- function(row_id) {
  if (row_id == "WBATE") return("$\\mathtt{WBATE}$")
  if (row_id == "LBATE") return("$\\mathtt{LBATE}$")
  sprintf("$\\bb_{%s}$", row_id)
}

bandwidth_values <- function(tab, i) {
  if (is_aggregate_row(tab$row[i])) return(c(h1 = NA_real_, h2 = NA_real_))

  h0 <- c(tab$h01[i], tab$h02[i])
  h1 <- c(tab$h11[i], tab$h12[i])
  h0_ok <- all(is.finite(h0))
  h1_ok <- all(is.finite(h1))

  if (h0_ok && h1_ok && isTRUE(all.equal(h0, h1))) {
    return(c(h1 = h0[1], h2 = h0[2]))
  }
  if (h0_ok) return(c(h1 = h0[1], h2 = h0[2]))
  if (h1_ok) return(c(h1 = h1[1], h2 = h1[2]))

  c(h1 = NA_real_, h2 = NA_real_)
}

effective_sample_values <- function(tab, i) {
  if (is_aggregate_row(tab$row[i])) return(c(N.Co = NA_real_, N.Tr = NA_real_))
  c(N.Co = tab$N.Co[i], N.Tr = tab$N.Tr[i])
}

interval_limits <- function(tab) {
  # The output CSVs contain pointwise CIs and, when requested, uniform CBs.
  # For display, use CBs wherever available; WBATE/LBATE keep their CI limits.
  has_cb <- all(c("cb.lower", "cb.upper") %in% names(tab))
  use_cb <- rep(FALSE, nrow(tab))
  if (has_cb) {
    use_cb <- is.finite(tab$cb.lower) & is.finite(tab$cb.upper)
  }

  data.frame(
    lower = ifelse(use_cb, tab$cb.lower, tab$ci.lower),
    upper = ifelse(use_cb, tab$cb.upper, tab$ci.upper)
  )
}

################################## Output ######################################

display_rows <- function(tab, keep_rows = NULL) {
  aggregate_rows <- tab[is_aggregate_row(tab$row), , drop = FALSE]
  point_rows <- tab[!is_aggregate_row(tab$row), , drop = FALSE]

  if (!is.null(keep_rows)) {
    point_rows <- point_rows[point_rows$row %in% as.character(keep_rows), , drop = FALSE]
  }

  out <- rbind(point_rows, aggregate_rows)
  ints <- interval_limits(out)
  bws <- t(vapply(seq_len(nrow(out)), function(i) bandwidth_values(out, i), numeric(2)))
  ns <- t(vapply(seq_len(nrow(out)), function(i) effective_sample_values(out, i), numeric(2)))

  data.frame(
    label = vapply(out$row, fmt_row_label, character(1)),
    h1 = bws[, "h1"],
    h2 = bws[, "h2"],
    N.Co = ns[, "N.Co"],
    N.Tr = ns[, "N.Tr"],
    estimate = out$estimate.p,
    pvalue = out$p.value,
    lower = ints$lower,
    upper = ints$upper,
    stringsAsFactors = FALSE
  )
}

display_percent_rows <- function(tab) {
  aggregate_rows <- tab[is_aggregate_row(tab$row), , drop = FALSE]
  point_rows <- tab[!is_aggregate_row(tab$row), , drop = FALSE]
  out <- rbind(point_rows, aggregate_rows)
  bws <- t(vapply(seq_len(nrow(out)), function(i) bandwidth_values(out, i), numeric(2)))
  ns <- t(vapply(seq_len(nrow(out)), function(i) effective_sample_values(out, i), numeric(2)))

  labels <- vapply(out$row, fmt_row_label, character(1))
  labels[out$row == "WBATE"] <- "Average"
  labels[out$row == "LBATE"] <- "Maximum"
  data.frame(
    label = labels,
    h1 = bws[, "h1"],
    h2 = bws[, "h2"],
    N.Co = ns[, "N.Co"],
    N.Tr = ns[, "N.Tr"],
    itt = out$itt.percent,
    fuzzy = out$fuzzy.percent,
    stringsAsFactors = FALSE
  )
}

latex_body <- function(rows) {
  lines <- character(0)
  for (i in seq_len(nrow(rows))) {
    if (grepl("WBATE|LBATE", rows$label[i])) {
      lines <- c(lines, "  \\midrule")
    }
    lines <- c(lines, paste0(
      "   ", rows$label[i], " & ",
      fmt_num(rows$h1[i], 1), " & ",
      fmt_num(rows$h2[i], 1), " & ",
      fmt_num(rows$N.Co[i], 0), " & ",
      fmt_num(rows$N.Tr[i], 0), " & ",
      fmt_num(rows$estimate[i]), " & ",
      fmt_pvalue(rows$pvalue[i]), " & ",
      fmt_interval(rows$lower[i], rows$upper[i]), "\\\\"
    ))
  }
  lines
}

latex_percent_body <- function(rows) {
  lines <- character(0)
  for (i in seq_len(nrow(rows))) {
    if (rows$label[i] %in% c("Average", "Maximum")) {
      lines <- c(lines, "  \\midrule")
    }
    lines <- c(lines, paste0(
      "   ", rows$label[i], " & ",
      fmt_num(rows$h1[i], 1), " & ",
      fmt_num(rows$h2[i], 1), " & ",
      fmt_num(rows$N.Co[i], 0), " & ",
      fmt_num(rows$N.Tr[i], 0), " & ",
      fmt_num(rows$itt[i]), " & ",
      fmt_num(rows$fuzzy[i]), "\\\\"
    ))
  }
  lines
}

write_tabular <- function(rows, file) {
  lines <- c(
    "\\begin{tabular}{lccccccc}",
    "  \\toprule\\toprule",
    "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & Estimate & $p$-value & 95\\% Interval \\\\",
    "  \\midrule",
    latex_body(rows),
    "  \\bottomrule\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(lines, file)
  invisible(lines)
}

write_percent_tabular <- function(rows, file) {
  lines <- c(
    "\\begin{tabular}{lcccccc}",
    "  \\toprule\\toprule",
    "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & ITT / ITT.0 (\\%) & Fuzzy / ITT.0 (\\%) \\\\",
    "  \\midrule",
    latex_percent_body(rows),
    "  \\bottomrule\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(lines, file)
  invisible(lines)
}

################################################################################
## Read Saved Results
################################################################################

# ITT bandwidth selection.
fuzzy.bwitt <- read_result_table("empapp_main_fuzzy_bwitt.csv")
itt.bwitt <- read_result_table("empapp_main_itt_bwitt.csv")
fs.bwitt <- read_result_table("empapp_main_fs_bwitt.csv")
baseline.bwitt <- read_result_table("empapp_main_itt0_bwitt.csv")
balance.bwitt <- read_result_table("empapp_covbal_itt_bwitt.csv")
percent.bwitt <- read_result_table("empapp_main_percent_bwitt.csv", kind = "percent")

# Fuzzy bandwidth selection.
fuzzy.bwmain <- read_result_table("empapp_main_fuzzy_bwmain.csv")
itt.bwmain <- read_result_table("empapp_main_itt_bwmain.csv")
fs.bwmain <- read_result_table("empapp_main_fs_bwmain.csv")
baseline.bwmain <- read_result_table("empapp_main_itt0_bwmain.csv")
balance.bwmain <- read_result_table("empapp_covbal_itt_bwmain.csv")
percent.bwmain <- read_result_table("empapp_main_percent_bwmain.csv", kind = "percent")


################################################################################
## Table 1: Selected Empirical Results (ITT Bandwidths)
################################################################################

# Table 1(a): Intention-to-treat effects.
# Input: output/empapp_main_itt_bwitt.csv.
write_tabular(display_rows(itt.bwitt, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_itt_small_bwitt.tex"))

# Table 1(b): First stage effects.
# Input: output/empapp_main_fs_bwitt.csv.
write_tabular(display_rows(fs.bwitt, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_fs_small_bwitt.tex"))

# Table 1(c): Fuzzy estimates.
# Input: output/empapp_main_fuzzy_bwitt.csv.
write_tabular(display_rows(fuzzy.bwitt, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_fuzzy_small_bwitt.tex"))

# Table 1(d): Covariate balance for mother's education.
# Input: output/empapp_covbal_itt_bwitt.csv.
write_tabular(display_rows(balance.bwitt, keep_rows = small_rows),
             file.path(tables_dir, "empapp_covbal_itt_small_bwitt.tex"))


################################################################################
## Tables SA-1--SA-6 (ITT Bandwidths)
################################################################################

# Table SA-1: Intention-to-treat effects.
# Input: output/empapp_main_itt_bwitt.csv.
write_tabular(display_rows(itt.bwitt),
             file.path(tables_dir, "empapp_main_itt_bwitt.tex"))

# Table SA-2: First stage effects.
# Input: output/empapp_main_fs_bwitt.csv.
write_tabular(display_rows(fs.bwitt),
             file.path(tables_dir, "empapp_main_fs_bwitt.tex"))

# Table SA-3: Fuzzy estimates.
# Input: output/empapp_main_fuzzy_bwitt.csv.
write_tabular(display_rows(fuzzy.bwitt),
             file.path(tables_dir, "empapp_main_fuzzy_bwitt.tex"))

# Table SA-4: Covariate balance for mother's education.
# Input: output/empapp_covbal_itt_bwitt.csv.
write_tabular(display_rows(balance.bwitt),
             file.path(tables_dir, "empapp_covbal_itt_bwitt.tex"))

# Table SA-5: Control-side baseline outcome.
# Input: output/empapp_main_itt0_bwitt.csv.
write_tabular(display_rows(baseline.bwitt),
             file.path(tables_dir, "empapp_main_itt0_bwitt.tex"))

# Table SA-6: Effects relative to baseline enrollment.
# Input: output/empapp_main_percent_bwitt.csv.
write_percent_tabular(display_percent_rows(percent.bwitt),
             file.path(tables_dir, "empapp_main_percent_bwitt.tex"))


################################################################################
## Tables SA-7--SA-12 (Fuzzy Bandwidths)
################################################################################

# Table SA-7: Intention-to-treat effects.
# Input: output/empapp_main_itt_bwmain.csv.
write_tabular(display_rows(itt.bwmain),
             file.path(tables_dir, "empapp_main_itt_bwmain.tex"))

# Table SA-8: First stage effects.
# Input: output/empapp_main_fs_bwmain.csv.
write_tabular(display_rows(fs.bwmain),
             file.path(tables_dir, "empapp_main_fs_bwmain.tex"))

# Table SA-9: Fuzzy estimates.
# Input: output/empapp_main_fuzzy_bwmain.csv.
write_tabular(display_rows(fuzzy.bwmain),
             file.path(tables_dir, "empapp_main_fuzzy_bwmain.tex"))

# Table SA-10: Covariate balance for mother's education.
# Input: output/empapp_covbal_itt_bwmain.csv.
write_tabular(display_rows(balance.bwmain),
             file.path(tables_dir, "empapp_covbal_itt_bwmain.tex"))

# Table SA-11: Control-side baseline outcome.
# Input: output/empapp_main_itt0_bwmain.csv.
write_tabular(display_rows(baseline.bwmain),
             file.path(tables_dir, "empapp_main_itt0_bwmain.tex"))

# Table SA-12: Effects relative to baseline enrollment.
# Input: output/empapp_main_percent_bwmain.csv.
write_percent_tabular(display_percent_rows(percent.bwmain),
             file.path(tables_dir, "empapp_main_percent_bwmain.tex"))


################################################################################
## Additional Outputs
################################################################################

# These selected-point fragments are retained for replication but do not
# appear in the current paper or supplement.

# Additional output: Control-side baseline outcome, selected points.
# Input: output/empapp_main_itt0_bwitt.csv.
write_tabular(display_rows(baseline.bwitt, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_itt0_small_bwitt.tex"))

# Additional output: Fuzzy estimates, selected points.
# Input: output/empapp_main_fuzzy_bwmain.csv.
write_tabular(display_rows(fuzzy.bwmain, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_fuzzy_small_bwmain.tex"))

# Additional output: Intention-to-treat effects, selected points.
# Input: output/empapp_main_itt_bwmain.csv.
write_tabular(display_rows(itt.bwmain, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_itt_small_bwmain.tex"))

# Additional output: First stage effects, selected points.
# Input: output/empapp_main_fs_bwmain.csv.
write_tabular(display_rows(fs.bwmain, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_fs_small_bwmain.tex"))

# Additional output: Control-side baseline outcome, selected points.
# Input: output/empapp_main_itt0_bwmain.csv.
write_tabular(display_rows(baseline.bwmain, keep_rows = small_rows),
             file.path(tables_dir, "empapp_main_itt0_small_bwmain.tex"))

# Additional output: Covariate balance for mother's education, selected points.
# Input: output/empapp_covbal_itt_bwmain.csv.
write_tabular(display_rows(balance.bwmain, keep_rows = small_rows),
             file.path(tables_dir, "empapp_covbal_itt_small_bwmain.tex"))


################################################################################
## Check Saved Tables
################################################################################

expected_tables <- c(
  "empapp_main_itt_small_bwitt.tex",
  "empapp_main_fs_small_bwitt.tex",
  "empapp_main_fuzzy_small_bwitt.tex",
  "empapp_covbal_itt_small_bwitt.tex",
  "empapp_main_itt_bwitt.tex",
  "empapp_main_fs_bwitt.tex",
  "empapp_main_fuzzy_bwitt.tex",
  "empapp_covbal_itt_bwitt.tex",
  "empapp_main_itt0_bwitt.tex",
  "empapp_main_percent_bwitt.tex",
  "empapp_main_itt_bwmain.tex",
  "empapp_main_fs_bwmain.tex",
  "empapp_main_fuzzy_bwmain.tex",
  "empapp_covbal_itt_bwmain.tex",
  "empapp_main_itt0_bwmain.tex",
  "empapp_main_percent_bwmain.tex",
  "empapp_main_itt0_small_bwitt.tex",
  "empapp_main_fuzzy_small_bwmain.tex",
  "empapp_main_itt_small_bwmain.tex",
  "empapp_main_fs_small_bwmain.tex",
  "empapp_main_itt0_small_bwmain.tex",
  "empapp_covbal_itt_small_bwmain.tex"
)
missing_tables <- expected_tables[!file.exists(file.path(tables_dir, expected_tables))]
if (length(missing_tables) > 0) {
  stop(sprintf("Missing expected tables: %s", paste(missing_tables, collapse = ", ")), call. = FALSE)
}

# Check the aggregate labels and sample-size headings in both fuzzy tables.
main_table <- paste(c(
  readLines(file.path(tables_dir, "empapp_main_fuzzy_bwmain.tex")),
  readLines(file.path(tables_dir, "empapp_main_fuzzy_bwitt.tex"))
), collapse = "\n")
forbidden <- c("Z value", "t-statistic", "P>|t|", "P>|z|", "CI/CB", "$<$0.001")
if (any(vapply(forbidden, grepl, logical(1), x = main_table, fixed = TRUE))) {
  stop("Generated empirical table contains an obsolete label.", call. = FALSE)
}
if (!grepl("WBATE", main_table) || !grepl("LBATE", main_table)) {
  stop("Generated empirical table is missing WBATE or LBATE.", call. = FALSE)
}
if (!grepl("$N_{\\mathrm{Co}}$", main_table, fixed = TRUE) || !grepl("$N_{\\mathrm{Tr}}$", main_table, fixed = TRUE)) {
  stop("Generated empirical table is missing N.Co or N.Tr headers.", call. = FALSE)
}

cat("Empirical application LaTeX tables complete.\n")
cat(sprintf("Tables written to: %s\n", tables_dir))
