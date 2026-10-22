################################################################################
## Estimation and Inference in Boundary Discontinuity Designs:
## Location-Based Methods
## Empirical Application: SPP Data
################################################################################
## Run this file first, followed by CTY_2027_JASA--empapp_tables.R and
## CTY_2027_JASA--empapp_figures.R. Results are saved as CSV files in output/.
## The two analyses below differ in the target used to select bandwidths:
##   bwparam = "main": fuzzy estimates (supplement, Section SA-10.2).
##   bwparam = "itt":  reduced-form estimates (paper and Section SA-10.1).
################################################################################

rm(list = ls(all = TRUE))


################################################################################
## Setup
################################################################################

# Rscript uses the folder containing this file. For interactive execution,
# set the working directory to the replication folder before running the code.
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) > 0) {
  setwd(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
}
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# The seed and order of the fits and summaries determine the Gaussian draws
# used for confidence bands and aggregate inference. Run the file in order
# to reproduce the saved results, including the ITT bandwidth analysis below.
set.seed(20260501)
repp <- 2000
repp_env <- Sys.getenv("RD2D_EMP_REPP", unset = "")
if (!identical(repp_env, "")) {
  repp <- suppressWarnings(as.integer(repp_env))
  if (is.na(repp) || repp <= 0) {
    stop("RD2D_EMP_REPP must be a positive integer.", call. = FALSE)
  }
}

library(rd2d)

# Check the installed interface and record the package version.
missing_args <- setdiff(c("params.other", "params.cov", "bwparam"),
                        names(formals(rd2d::rd2d)))
if (length(missing_args) > 0) {
  stop(paste("The installed rd2d package does not match the replication API.",
             "Missing rd2d() argument(s):", paste(missing_args, collapse = ", ")),
       call. = FALSE)
}
desc_path <- system.file("DESCRIPTION", package = "rd2d")
if (!nzchar(desc_path)) {
  stop("Could not find installed rd2d DESCRIPTION file.", call. = FALSE)
}
rd2d_version <- as.character(read.dcf(desc_path, fields = "Version")[1, 1])
cat(sprintf("Using rd2d version: %s\n", rd2d_version))

# Replace earlier empirical CSV outputs when running the full replication.
old_files <- list.files(output_dir, pattern = "^(rd2d_)?empapp_.*\\.csv$",
                       full.names = TRUE)
if (length(old_files) > 0) unlink(old_files)


################################################################################
## SPP Data
################################################################################

spp <- read.csv("spp.csv")
expected_columns <- c("running_saber11", "running_sisben", "eligible_spp",
                      "beneficiary_spp", "spadies_any", "icfes_educm1")
missing_columns <- setdiff(expected_columns, names(spp))
if (length(missing_columns) > 0) {
  stop(sprintf("spp.csv is missing columns: %s",
               paste(missing_columns, collapse = ", ")), call. = FALSE)
}
spp <- spp[, expected_columns]
names(spp) <- c("x.1", "x.2", "assignment", "fuzzy", "Y", "Z")

# The two scores are centered at the eligibility thresholds. Eligibility
# requires both scores to be nonnegative; fuzzy records actual program take-up.
expected_assignment <- as.integer(spp$x.1 >= 0 & spp$x.2 >= 0)
if (!all(spp$assignment == expected_assignment, na.rm = TRUE)) {
  stop("eligible_spp does not match 1{x.1 >= 0 & x.2 >= 0}.", call. = FALSE)
}

# Enrollment and mother's education use their own complete-case samples.
# Missing mother's education does not exclude a student from the enrollment fit.
keep_main <- complete.cases(spp[, c("x.1", "x.2", "assignment", "fuzzy", "Y")])
data.main <- spp[keep_main, c("x.1", "x.2", "assignment", "fuzzy", "Y"), drop = FALSE]
keep_balance <- complete.cases(spp[, c("x.1", "x.2", "assignment", "fuzzy", "Z")])
data.balance <- spp[keep_balance, c("x.1", "x.2", "assignment", "fuzzy", "Z"), drop = FALSE]


################################################################################
## Evaluation Points on the Boundary
################################################################################

# Points 1--20 follow the academic threshold; points 21--40 follow the wealth
# threshold. Point 21 is the corner where the two eligibility thresholds meet.
neval <- 40
nhalf <- ceiling(neval / 2)
eval <- rbind(
  data.frame(x.1 = rep(0, nhalf),
             x.2 = 40 - (seq_len(nhalf) - 1) * 40 / nhalf),
  data.frame(x.1 = (seq_len(neval - nhalf) - 1) * 56 / nhalf,
             x.2 = rep(0, neval - nhalf))
)

# summary() normalizes these weights. Each of the 40 points receives equal
# weight in the WBATE; the LBATE is the maximum over the same points.
wbate.weights <- rep(1, neval)

# Save the data and grid used by the figure scripts, together with run metadata.
write.csv(spp, file.path(output_dir, "empapp_data.csv"), row.names = FALSE, na = "")
write.csv(eval, file.path(output_dir, "empapp_eval.csv"), row.names = FALSE, na = "")
metadata <- data.frame(
  name = c("rd2d.version", "repp", "bwparams", "generated_at"),
  value = c(rd2d_version, as.character(repp), "main=bwmain; itt=bwitt",
            as.character(Sys.time())),
  stringsAsFactors = FALSE
)
write.csv(metadata, file.path(output_dir, "empapp_metadata.csv"),
          row.names = FALSE, na = "")


################################################################################
## Fuzzy Bandwidth Selection (Section SA-10.2)
################################################################################

cat("Running empirical application with bwparam='main'.\n")

# College enrollment. The default selector chooses pointwise MSE bandwidths;
# bwparam identifies the parameter targeted by that selector. The same local
# fit provides the fuzzy, reduced-form, first-stage, and control baseline results.
# params.cov requests the covariances needed for bands and aggregate inference.
result.main <- rd2d::rd2d(
  Y = data.main$Y,
  X = as.matrix(data.main[, c("x.1", "x.2")]),
  assignment = data.main$assignment,
  b = eval,
  fuzzy = data.main$fuzzy,
  bwparam = "main",
  params.other = "itt.0",
  params.cov = c("main", "itt", "fs", "itt.0")
)

# Fuzzy estimates of the local Wald ratio.
summary.fuzzy <- summary(result.main, output = "main", cbands = "main",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.fuzzy <- summary.fuzzy$tables[["main"]]
table.fuzzy <- data.frame(row = rownames(table.fuzzy), table.fuzzy, check.names = FALSE)
write.csv(table.fuzzy, file.path(output_dir, "empapp_main_fuzzy_bwmain.csv"),
          row.names = FALSE, na = "")

# Reduced-form effects of eligibility on college enrollment.
summary.itt <- summary(result.main, output = "itt", cbands = "itt",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.itt <- summary.itt$tables[["itt"]]
table.itt <- data.frame(row = rownames(table.itt), table.itt, check.names = FALSE)
write.csv(table.itt, file.path(output_dir, "empapp_main_itt_bwmain.csv"),
          row.names = FALSE, na = "")

# First-stage effects of eligibility on program take-up.
summary.fs <- summary(result.main, output = "fs", cbands = "fs",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.fs <- summary.fs$tables[["fs"]]
table.fs <- data.frame(row = rownames(table.fs), table.fs, check.names = FALSE)
write.csv(table.fs, file.path(output_dir, "empapp_main_fs_bwmain.csv"),
          row.names = FALSE, na = "")

# Control-side baseline enrollment probabilities.
summary.itt0 <- summary(result.main, output = "itt.0", cbands = "itt.0",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.itt0 <- summary.itt0$tables[["itt.0"]]
table.itt0 <- data.frame(row = rownames(table.itt0), table.itt0, check.names = FALSE)
write.csv(table.itt0, file.path(output_dir, "empapp_main_itt0_bwmain.csv"),
          row.names = FALSE, na = "")

# Covariate balance: use mother's education as the outcome and report the
# reduced-form estimates. The fit retains the same requested parameters and
# covariance matrices as the enrollment fit.
result.balance <- rd2d::rd2d(
  Y = data.balance$Z,
  X = as.matrix(data.balance[, c("x.1", "x.2")]),
  assignment = data.balance$assignment,
  b = eval,
  fuzzy = data.balance$fuzzy,
  bwparam = "main",
  params.other = "itt.0",
  params.cov = c("main", "itt", "fs", "itt.0")
)
summary.balance <- summary(result.balance, output = "itt", cbands = "itt",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.balance <- summary.balance$tables[["itt"]]
table.balance <- data.frame(row = rownames(table.balance), table.balance, check.names = FALSE)
write.csv(table.balance, file.path(output_dir, "empapp_covbal_itt_bwmain.csv"),
          row.names = FALSE, na = "")

# Effects relative to baseline enrollment: read the exported estimates used
# by the table and figure scripts, and divide at each boundary point first.
fuzzy <- read.csv(file.path(output_dir, "empapp_main_fuzzy_bwmain.csv"), check.names = FALSE)
itt <- read.csv(file.path(output_dir, "empapp_main_itt_bwmain.csv"), check.names = FALSE)
itt0 <- read.csv(file.path(output_dir, "empapp_main_itt0_bwmain.csv"), check.names = FALSE)
if (!identical(fuzzy$row, itt$row) || !identical(fuzzy$row, itt0$row)) {
  stop("Cannot construct percent table because summary rows do not align.", call. = FALSE)
}
points <- !(fuzzy$row %in% c("WBATE", "LBATE"))
denom <- itt0$estimate.p
if (!any(points) || any(!is.finite(denom[points]) | denom[points] == 0)) {
  stop("Percentage summaries require finite, nonzero pointwise baselines.", call. = FALSE)
}
percent <- data.frame(
  row = fuzzy$row,
  b1 = fuzzy$b1,
  b2 = fuzzy$b2,
  h01 = fuzzy$h01,
  h02 = fuzzy$h02,
  h11 = fuzzy$h11,
  h12 = fuzzy$h12,
  N.Co = fuzzy$N.Co,
  N.Tr = fuzzy$N.Tr,
  itt.percent = 100 * itt$estimate.p / denom,
  fuzzy.percent = 100 * fuzzy$estimate.p / denom,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Average and maximum of the 40 pointwise percentages. WBATE/LBATE are retained
# as CSV row keys; the displayed percentage summaries are Average/Maximum.
for (column in c("itt.percent", "fuzzy.percent")) {
  values <- percent[[column]][points]
  if (any(!is.finite(values))) {
    stop("Percentage summaries require finite pointwise estimates.", call. = FALSE)
  }
  percent[percent$row == "WBATE", column] <- mean(values)
  percent[percent$row == "LBATE", column] <- max(values)
}
write.csv(percent, file.path(output_dir, "empapp_main_percent_bwmain.csv"),
          row.names = FALSE, na = "")


################################################################################
## ITT Bandwidth Selection (Paper and Section SA-10.1)
################################################################################

cat("Running empirical application with bwparam='itt'.\n")

# College enrollment. The default selector chooses pointwise MSE bandwidths;
# bwparam identifies the parameter targeted by that selector. The same local
# fit provides the fuzzy, reduced-form, first-stage, and control baseline results.
# params.cov requests the covariances needed for bands and aggregate inference.
result.main <- rd2d::rd2d(
  Y = data.main$Y,
  X = as.matrix(data.main[, c("x.1", "x.2")]),
  assignment = data.main$assignment,
  b = eval,
  fuzzy = data.main$fuzzy,
  bwparam = "itt",
  params.other = "itt.0",
  params.cov = c("main", "itt", "fs", "itt.0")
)

# Fuzzy estimates of the local Wald ratio.
summary.fuzzy <- summary(result.main, output = "main", cbands = "main",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.fuzzy <- summary.fuzzy$tables[["main"]]
table.fuzzy <- data.frame(row = rownames(table.fuzzy), table.fuzzy, check.names = FALSE)
write.csv(table.fuzzy, file.path(output_dir, "empapp_main_fuzzy_bwitt.csv"),
          row.names = FALSE, na = "")

# Reduced-form effects of eligibility on college enrollment.
summary.itt <- summary(result.main, output = "itt", cbands = "itt",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.itt <- summary.itt$tables[["itt"]]
table.itt <- data.frame(row = rownames(table.itt), table.itt, check.names = FALSE)
write.csv(table.itt, file.path(output_dir, "empapp_main_itt_bwitt.csv"),
          row.names = FALSE, na = "")

# First-stage effects of eligibility on program take-up.
summary.fs <- summary(result.main, output = "fs", cbands = "fs",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.fs <- summary.fs$tables[["fs"]]
table.fs <- data.frame(row = rownames(table.fs), table.fs, check.names = FALSE)
write.csv(table.fs, file.path(output_dir, "empapp_main_fs_bwitt.csv"),
          row.names = FALSE, na = "")

# Control-side baseline enrollment probabilities.
summary.itt0 <- summary(result.main, output = "itt.0", cbands = "itt.0",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.itt0 <- summary.itt0$tables[["itt.0"]]
table.itt0 <- data.frame(row = rownames(table.itt0), table.itt0, check.names = FALSE)
write.csv(table.itt0, file.path(output_dir, "empapp_main_itt0_bwitt.csv"),
          row.names = FALSE, na = "")

# Covariate balance: use mother's education as the outcome and report the
# reduced-form estimates. The fit retains the same requested parameters and
# covariance matrices as the enrollment fit.
result.balance <- rd2d::rd2d(
  Y = data.balance$Z,
  X = as.matrix(data.balance[, c("x.1", "x.2")]),
  assignment = data.balance$assignment,
  b = eval,
  fuzzy = data.balance$fuzzy,
  bwparam = "itt",
  params.other = "itt.0",
  params.cov = c("main", "itt", "fs", "itt.0")
)
summary.balance <- summary(result.balance, output = "itt", cbands = "itt",
                           WBATE = wbate.weights, LBATE = TRUE, repp = repp)
table.balance <- summary.balance$tables[["itt"]]
table.balance <- data.frame(row = rownames(table.balance), table.balance, check.names = FALSE)
write.csv(table.balance, file.path(output_dir, "empapp_covbal_itt_bwitt.csv"),
          row.names = FALSE, na = "")

# Effects relative to baseline enrollment: read the exported estimates used
# by the table and figure scripts, and divide at each boundary point first.
fuzzy <- read.csv(file.path(output_dir, "empapp_main_fuzzy_bwitt.csv"), check.names = FALSE)
itt <- read.csv(file.path(output_dir, "empapp_main_itt_bwitt.csv"), check.names = FALSE)
itt0 <- read.csv(file.path(output_dir, "empapp_main_itt0_bwitt.csv"), check.names = FALSE)
if (!identical(fuzzy$row, itt$row) || !identical(fuzzy$row, itt0$row)) {
  stop("Cannot construct percent table because summary rows do not align.", call. = FALSE)
}
points <- !(fuzzy$row %in% c("WBATE", "LBATE"))
denom <- itt0$estimate.p
if (!any(points) || any(!is.finite(denom[points]) | denom[points] == 0)) {
  stop("Percentage summaries require finite, nonzero pointwise baselines.", call. = FALSE)
}
percent <- data.frame(
  row = fuzzy$row,
  b1 = fuzzy$b1,
  b2 = fuzzy$b2,
  h01 = fuzzy$h01,
  h02 = fuzzy$h02,
  h11 = fuzzy$h11,
  h12 = fuzzy$h12,
  N.Co = fuzzy$N.Co,
  N.Tr = fuzzy$N.Tr,
  itt.percent = 100 * itt$estimate.p / denom,
  fuzzy.percent = 100 * fuzzy$estimate.p / denom,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Average and maximum of the 40 pointwise percentages. WBATE/LBATE are retained
# as CSV row keys; the displayed percentage summaries are Average/Maximum.
for (column in c("itt.percent", "fuzzy.percent")) {
  values <- percent[[column]][points]
  if (any(!is.finite(values))) {
    stop("Percentage summaries require finite pointwise estimates.", call. = FALSE)
  }
  percent[percent$row == "WBATE", column] <- mean(values)
  percent[percent$row == "LBATE", column] <- max(values)
}
write.csv(percent, file.path(output_dir, "empapp_main_percent_bwitt.csv"),
          row.names = FALSE, na = "")


################################################################################
## Check Saved Outputs
################################################################################

# These filenames are the inputs to the empirical table and figure scripts.
expected_files <- c(
  "empapp_data.csv", "empapp_eval.csv", "empapp_metadata.csv",
  "empapp_main_fuzzy_bwmain.csv", "empapp_main_itt_bwmain.csv",
  "empapp_main_fs_bwmain.csv", "empapp_main_itt0_bwmain.csv",
  "empapp_covbal_itt_bwmain.csv", "empapp_main_percent_bwmain.csv",
  "empapp_main_fuzzy_bwitt.csv", "empapp_main_itt_bwitt.csv",
  "empapp_main_fs_bwitt.csv", "empapp_main_itt0_bwitt.csv",
  "empapp_covbal_itt_bwitt.csv", "empapp_main_percent_bwitt.csv"
)
missing_files <- expected_files[!file.exists(file.path(output_dir, expected_files))]
if (length(missing_files) > 0) {
  stop(sprintf("Missing expected output file(s): %s",
               paste(missing_files, collapse = ", ")), call. = FALSE)
}

cat("Empirical application output data complete.\n")
cat(sprintf("CSV outputs written to: %s\n", output_dir))
cat(sprintf("Generated %d CSV file(s).\n", length(expected_files)))
