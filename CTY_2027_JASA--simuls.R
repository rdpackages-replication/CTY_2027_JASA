################################################################################################
# Estimation and Inference in Boundary Discontinuity Designs: Location-Based Methods
# Simulation Study
################################################################################################

rm(list = ls(all = TRUE))

# Supplemental appendix, Section SA-11: Simulation Study.
# Run this file first, then CTY_2027_JASA--simuls_tables.R.
# The main workflow below loads the SPP data, calibrates the DGPs, computes
# their targets, and runs the replications. Functions collect the calculations
# repeated across DGPs and allow replications to run on parallel workers.
#
# The fuzzy DGP produces the ITT, first stage, and fuzzy results in Tables
# SA-13--SA-30. Both bandwidth selectors use the same simulated sample.
# Separate sharp DGPs produce six additional tables not printed in the SA.
#
# Defaults: 5,000 replications, 10,000 observations, and 2,000 Gaussian draws.
# RD2D_M, RD2D_N, RD2D_REPP, RD2D_SEED, and RD2D_WORKERS override the defaults.
# RD2D_OUTPUT_DIR selects the output folder (default: output/). Running this
# script replaces earlier simuls_*.csv files in that folder. Smaller runs are
# useful for checking execution, but do not reproduce the reported tables.

################################## Setup #######################################

# Rscript uses the folder containing this file. For interactive execution,
# set the working directory to the replication folder before running the code.

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

script_dir <- get_script_dir()
setwd(script_dir)

library(rd2d)

output_dir <- Sys.getenv("RD2D_OUTPUT_DIR", unset = "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# "main" selects bandwidths for the fuzzy curve; "itt" selects them for
# the reduced form. These suffixes identify the corresponding output files.
bw_specs <- data.frame(
  bwparam = c("main", "itt"),
  suffix = c("bwmain", "bwitt"),
  stringsAsFactors = FALSE
)

# Package output names and the names used in the saved CSV files. The ITT
# and first stage curves below come from the fuzzy DGP, not the sharp DGP.
estimand_specs <- data.frame(
  design = c("fuzzy", "fuzzy", "fuzzy", "sharp", "sharp"),
  output = c("main", "itt", "fs", "main", "main.0"),
  estimand = c("fuzzy", "itt", "fs", "sharp", "sharp0"),
  stringsAsFactors = FALSE
)
fuzzy_estimand_specs <- estimand_specs[estimand_specs$design == "fuzzy", ]
sharp_estimand_specs <- estimand_specs[estimand_specs$design == "sharp", ]

get_env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || identical(value, "")) return(default)

  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed <= 0) {
    stop(sprintf("%s must be a positive integer.", name), call. = FALSE)
  }
  parsed
}

rd2d_package_version <- function() {
  desc_path <- system.file("DESCRIPTION", package = "rd2d")
  if (!nzchar(desc_path)) {
    stop("Could not find installed rd2d DESCRIPTION file.", call. = FALSE)
  }
  as.character(read.dcf(desc_path, fields = "Version")[1, 1])
}

clean_output_dir <- function() {
  old_files <- list.files(output_dir, pattern = "^simuls_.*\\.csv$", full.names = TRUE)
  if (length(old_files) > 0) unlink(old_files)
}

write_output_csv <- function(x, file) {
  write.csv(x, file.path(output_dir, file), row.names = FALSE, na = "")
}

# The seed is fixed by the replication, design, and DGP. Worker scheduling
# and the number of workers therefore do not change the random draws.
simulation_seed <- function(seed_base, replication, design, dgp) {
  design_offset <- if (identical(design, "sharp")) 500000L else 0L
  seed_base + replication * 1000L + design_offset + dgp
}

################################## Data ########################################

# Calibrate on observations with complete scores, assignment, take-up, and
# enrollment. The control region must have zero treatment take-up.
load_spp_data <- function(path = "spp.csv") {
  raw <- read.csv(path)
  expected <- c(
    "running_saber11", "running_sisben", "eligible_spp",
    "beneficiary_spp", "spadies_any"
  )
  missing_cols <- setdiff(expected, names(raw))
  if (length(missing_cols) > 0) {
    stop(sprintf("spp.csv is missing columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  dat <- raw[, expected]
  names(dat) <- c("x.1", "x.2", "assignment", "fuzzy", "Y")
  dat <- dat[complete.cases(dat), ]

  expected_assignment <- as.integer(dat$x.1 >= 0 & dat$x.2 >= 0)
  if (!all(dat$assignment == expected_assignment)) {
    stop("eligible_spp does not match 1{x.1 >= 0 & x.2 >= 0}.", call. = FALSE)
  }

  dat$assignment <- as.numeric(dat$assignment)
  dat$fuzzy <- as.numeric(dat$fuzzy)
  dat$Y <- as.numeric(dat$Y)
  dat
}

make_eval_grid <- function() {
  # Start from the 40-point empirical grid and keep points 11--31. This gives
  # the 21-point simulation grid with the kink at the internal point b11.
  neval <- 40
  half <- ceiling(neval / 2)
  grid <- rbind(
    data.frame(
      x.1 = rep(0, half),
      x.2 = 40 - (seq_len(half) - 1) * 40 / half
    ),
    data.frame(
      x.1 = (seq_len(neval - half) - 1) * 56 / half,
      x.2 = rep(0, neval - half)
    )
  )
  grid[11:31, , drop = FALSE]
}

################################## DGP #########################################

# Complete polynomials of degree s = 1, 2, or 3 on the original score scales.
# The DGP degree s is separate from the local polynomial degree used by rd2d.
design_matrix <- function(dat, s) {
  x1 <- dat$x.1
  x2 <- dat$x.2

  if (s == 1) {
    return(cbind("(Intercept)" = 1, "x.1" = x1, "x.2" = x2))
  }
  if (s == 2) {
    return(cbind(
      "(Intercept)" = 1,
      "x.1" = x1,
      "x.2" = x2,
      "I(x.1^2)" = x1^2,
      "I(x.1 * x.2)" = x1 * x2,
      "I(x.2^2)" = x2^2
    ))
  }
  if (s == 3) {
    return(cbind(
      "(Intercept)" = 1,
      "x.1" = x1,
      "x.2" = x2,
      "I(x.1^2)" = x1^2,
      "I(x.1 * x.2)" = x1 * x2,
      "I(x.2^2)" = x2^2,
      "I(x.1^3)" = x1^3,
      "I(x.1^2 * x.2)" = x1^2 * x2,
      "I(x.1 * x.2^2)" = x1 * x2^2,
      "I(x.2^3)" = x2^3
    ))
  }

  stop("Only s = 1, s = 2, and s = 3 are supported.", call. = FALSE)
}

# Fit outcome means separately by assignment and a logistic take-up model
# in the treatment region. For the fuzzy DGP, lambda_1 links the outcome and
# take-up residuals. The additional sharp DGP sets this coefficient to zero.
calibrate_dgp <- function(dat, s, dgp, design = c("fuzzy", "sharp")) {
  design <- match.arg(design)
  Xs <- design_matrix(dat, s)
  out <- list(design = design, dgp = dgp, s = s, mu_fuzzy_0 = 0)

  for (side in 0:1) {
    ind <- dat$assignment == side
    X_side <- Xs[ind, , drop = FALSE]

    y_fit <- lm.fit(X_side, dat$Y[ind])
    beta_y <- as.numeric(y_fit$coefficients)
    names(beta_y) <- colnames(X_side)

    if (any(!is.finite(beta_y))) {
      stop(sprintf("Non-finite calibration coefficient for s=%d, assignment=%d.", s, side), call. = FALSE)
    }

    mu_y <- as.numeric(X_side %*% beta_y)
    eps_y <- dat$Y[ind] - mu_y

    if (side == 0) {
      if (any(dat$fuzzy[ind] != 0)) {
        stop("The one-sided take-up design requires W=0 for every control observation.", call. = FALSE)
      }
      lambda <- 0
      sigma2 <- mean(eps_y^2)
    } else {
      fuzzy_fit <- suppressWarnings(glm.fit(X_side, dat$fuzzy[ind], family = binomial()))
      beta_fuzzy <- as.numeric(fuzzy_fit$coefficients)
      names(beta_fuzzy) <- colnames(X_side)
      if (any(!is.finite(beta_fuzzy))) {
        stop(sprintf("Non-finite calibration coefficient for s=%d, assignment=%d.", s, side), call. = FALSE)
      }

      mu_fuzzy <- plogis(as.numeric(X_side %*% beta_fuzzy))
      eps_fuzzy <- dat$fuzzy[ind] - mu_fuzzy
      denom <- sum(eps_fuzzy^2)
      if (!is.finite(denom) || denom <= 0) {
        stop(sprintf("Degenerate first-stage residual variance for s=%d, assignment=%d.", s, side), call. = FALSE)
      }

      lambda <- if (identical(design, "sharp")) 0 else sum(eps_y * eps_fuzzy) / denom
      sigma2 <- mean((eps_y - lambda * eps_fuzzy)^2)
      out$beta_fuzzy_1 <- beta_fuzzy
    }
    if (!is.finite(sigma2) || sigma2 <= 0) {
      stop(sprintf("Degenerate outcome residual variance for s=%d, assignment=%d.", s, side), call. = FALSE)
    }

    out[[paste0("beta_y_", side)]] <- beta_y
    out[[paste0("lambda_", side)]] <- lambda
    out[[paste0("sigma_y_", side)]] <- sqrt(sigma2)
  }

  out
}

# Save the fitted coefficients and shock parameters for each design and DGP.
make_dgp_parameter_table <- function(calibrations) {
  rows <- lapply(calibrations, function(cal) {
    coef_rows <- do.call(rbind, lapply(0:1, function(side) {
      components <- c("beta_y", if (side == 0) "mu_fuzzy" else "beta_fuzzy")
      do.call(rbind, lapply(components, function(component) {
        values <- cal[[paste0(component, "_", side)]]
        data.frame(
          design = cal$design,
          dgp = cal$dgp,
          s = cal$s,
          side = side,
          component = component,
          term = if (is.null(names(values))) "" else names(values),
          value = as.numeric(values),
          stringsAsFactors = FALSE
        )
      }))
    }))

    shock_rows <- do.call(rbind, lapply(0:1, function(side) {
      data.frame(
        design = cal$design,
        dgp = cal$dgp,
        s = cal$s,
        side = side,
        component = c("lambda", "sigma_y"),
        term = "",
        value = c(cal[[paste0("lambda_", side)]], cal[[paste0("sigma_y_", side)]]),
        stringsAsFactors = FALSE
      )
    }))

    rbind(coef_rows, shock_rows)
  })

  do.call(rbind, rows)
}

# Evaluate the true curves analytically before drawing simulated samples.
# WBATE is their equal-weight average and LBATE their maximum on the 21-point
# grid. These are the finite-grid targets used to assess coverage and bias.
make_targets <- function(eval, calibrations) {
  out <- lapply(calibrations, function(cal) {
    R <- design_matrix(eval, cal$s)
    mu_y0 <- as.numeric(R %*% cal$beta_y_0)
    mu_y1 <- as.numeric(R %*% cal$beta_y_1)
    mu_fuzzy0 <- rep(cal$mu_fuzzy_0, nrow(eval))
    mu_fuzzy1 <- plogis(as.numeric(R %*% cal$beta_fuzzy_1))

    point_targets <- data.frame(
      dgp = cal$dgp,
      s = cal$s,
      row_type = "point",
      index = seq_len(nrow(eval)),
      aggregate = "",
      b1 = eval$x.1,
      b2 = eval$x.2,
      tau_itt = mu_y1 - mu_y0,
      tau_fs = mu_fuzzy1 - mu_fuzzy0,
      zeta = (mu_y1 - mu_y0) / (mu_fuzzy1 - mu_fuzzy0),
      sharp = mu_y1 - mu_y0,
      sharp0 = mu_y0,
      stringsAsFactors = FALSE
    )

    specs <- estimand_specs[estimand_specs$design == cal$design, ]
    target_map <- list(
      fuzzy = point_targets$zeta,
      itt = point_targets$tau_itt,
      fs = point_targets$tau_fs,
      sharp = point_targets$sharp,
      sharp0 = point_targets$sharp0
    )

    point_long <- do.call(rbind, lapply(specs$estimand, function(estimand) {
      data.frame(
        design = cal$design,
        point_targets[, c("dgp", "s", "row_type", "index", "aggregate", "b1", "b2")],
        estimand = estimand,
        target = target_map[[estimand]],
        stringsAsFactors = FALSE
      )
    }))

    aggregate_long <- do.call(rbind, lapply(split(point_long, point_long$estimand), function(z) {
      data.frame(
        dgp = z$dgp[1],
        design = z$design[1],
        s = z$s[1],
        row_type = "aggregate",
        index = NA_integer_,
        aggregate = c("WBATE", "LBATE"),
        b1 = NA_real_,
        b2 = NA_real_,
        estimand = z$estimand[1],
        target = c(mean(z$target), max(z$target)),
        stringsAsFactors = FALSE
      )
    }))

    rbind(point_long, aggregate_long)
  })

  do.call(rbind, out)
}

################################ Simulation ####################################

# Compute pointwise intervals, uniform bands, and aggregate inference using
# the covariance matrices saved by rd2d. Gaussian draws enter here via repp.
summary_table <- function(fit, output) {
  point_table <- fit[[output]]
  invisible(utils::capture.output(
    summ <- summary(
      fit,
      output = output,
      cbands = output,
      WBATE = rep(1, nrow(point_table)),
      LBATE = TRUE,
      repp = repp
    )
  ))
  summ$tables[[output]]
}

table_col <- function(table, name) {
  if (name %in% names(table)) table[[name]] else rep(NA_real_, nrow(table))
}

estimand_name <- function(output, design) {
  if (identical(design, "fuzzy") && identical(output, "main")) return("fuzzy")
  if (identical(design, "sharp") && identical(output, "main")) return("sharp")
  if (identical(design, "sharp") && identical(output, "main.0")) return("sharp0")
  output
}

# Each estimand contributes 21 pointwise rows and two aggregate rows. Keep
# point estimates, inference, bandwidths, and effective sample sizes together.
make_output_rows <- function(fit, output, design, rep_id, dgp_id, dgp_s,
                             bwparam) {
  table <- summary_table(fit, output)
  point_rows <- !(rownames(table) %in% c("WBATE", "LBATE"))
  points <- table[point_rows, , drop = FALSE]
  aggregates <- table[!point_rows, , drop = FALSE]
  estimand <- estimand_name(output, design)

  point_out <- data.frame(
    replication = rep_id,
    design = design,
    dgp = dgp_id,
    s = dgp_s,
    bwparam = bwparam,
    estimand = estimand,
    row_type = "point",
    index = seq_len(nrow(points)),
    aggregate = "",
    b1 = table_col(points, "b1"),
    b2 = table_col(points, "b2"),
    h01 = table_col(points, "h01"),
    h02 = table_col(points, "h02"),
    h11 = table_col(points, "h11"),
    h12 = table_col(points, "h12"),
    N.Co = table_col(points, "N.Co"),
    N.Tr = table_col(points, "N.Tr"),
    estimate.p = table_col(points, "estimate.p"),
    std.err.p = table_col(points, "std.err.p"),
    estimate.q = table_col(points, "estimate.q"),
    std.err.q = table_col(points, "std.err.q"),
    t.value = table_col(points, "t.value"),
    p.value = table_col(points, "p.value"),
    ci.lower = table_col(points, "ci.lower"),
    ci.upper = table_col(points, "ci.upper"),
    cb.lower = table_col(points, "cb.lower"),
    cb.upper = table_col(points, "cb.upper"),
    stringsAsFactors = FALSE
  )

  aggregate_out <- data.frame(
    replication = rep_id,
    design = design,
    dgp = dgp_id,
    s = dgp_s,
    bwparam = bwparam,
    estimand = estimand,
    row_type = "aggregate",
    index = NA_integer_,
    aggregate = rownames(aggregates),
    b1 = NA_real_,
    b2 = NA_real_,
    h01 = NA_real_,
    h02 = NA_real_,
    h11 = NA_real_,
    h12 = NA_real_,
    N.Co = NA_real_,
    N.Tr = NA_real_,
    estimate.p = table_col(aggregates, "estimate.p"),
    std.err.p = NA_real_,
    estimate.q = table_col(aggregates, "estimate.q"),
    std.err.q = table_col(aggregates, "std.err.q"),
    t.value = table_col(aggregates, "t.value"),
    p.value = table_col(aggregates, "p.value"),
    ci.lower = table_col(aggregates, "ci.lower"),
    ci.upper = table_col(aggregates, "ci.upper"),
    cb.lower = NA_real_,
    cb.upper = NA_real_,
    stringsAsFactors = FALSE
  )

  rbind(point_out, aggregate_out)
}

# Independent score components, each distributed as 100 * Beta(3, 4) - 25.
draw_scores <- function(n) {
  data.frame(
    x.1 = 100 * rbeta(n, 3, 4) - 25,
    x.2 = 100 * rbeta(n, 3, 4) - 25
  )
}

assignment_rule <- function(X) {
  as.numeric(X$x.1 >= 0 & X$x.2 >= 0)
}

# Generate scores, assignment, treatment take-up (fuzzy DGP), and outcomes.
# This sample is reused for both fuzzy bandwidth selectors.
simulate_sample <- function(cal, n) {
  X <- draw_scores(n)
  assignment <- assignment_rule(X)
  R <- design_matrix(X, cal$s)

  mu_y0 <- as.numeric(R %*% cal$beta_y_0)
  mu_y1 <- as.numeric(R %*% cal$beta_y_1)

  if (identical(cal$design, "fuzzy")) {
    mu_fuzzy1 <- plogis(as.numeric(R %*% cal$beta_fuzzy_1))
    fuzzy0 <- rep(cal$mu_fuzzy_0, n)
    invisible(runif(n)) # Preserve the random-number sequence used by earlier simulations.
    fuzzy1 <- as.numeric(runif(n) <= mu_fuzzy1)
    Y0 <- mu_y0 + rnorm(n, sd = cal$sigma_y_0)
    Y1 <- mu_y1 + cal$lambda_1 * (fuzzy1 - mu_fuzzy1) + rnorm(n, sd = cal$sigma_y_1)
    fuzzy <- ifelse(assignment == 1, fuzzy1, fuzzy0)
  } else {
    Y0 <- mu_y0 + rnorm(n, sd = cal$sigma_y_0)
    Y1 <- mu_y1 + rnorm(n, sd = cal$sigma_y_1)
    fuzzy <- NULL
  }

  list(
    X = X,
    assignment = assignment,
    Y = ifelse(assignment == 1, Y1, Y0),
    fuzzy = fuzzy
  )
}

# Each file identifies the replication, DGP, design, polynomial degree, and
# bandwidth selector. Fuzzy files have 69 rows; sharp files have 46 rows.
raw_result_file <- function(replication, dgp, design, s, suffix) {
  file.path(
    output_dir,
    sprintf("simuls_raw_rep%04d_dgp%02d_%s_s%d_%s.csv", replication, dgp, design, s, suffix)
  )
}

write_raw_result <- function(rows, replication, cal, suffix) {
  write.csv(
    rows,
    file = raw_result_file(replication, cal$dgp, cal$design, cal$s, suffix),
    row.names = FALSE,
    na = ""
  )
}

# On each fuzzy sample, fit with both bandwidth selectors and summarize
# ITT, first stage, and fuzzy estimates in that order. This order also fixes
# the sequence of Gaussian draws used for inference.
# With the validated rd2d version, omitted options use joint fits, p = 1,
# q = 2, a product triangular kernel, and one bandwidth for both scores/sides.
run_fuzzy_calibration <- function(j, cal, sample, eval, repp, bw_specs) {
  for (i in seq_len(nrow(bw_specs))) {
    bwparam <- bw_specs$bwparam[i]
    suffix <- bw_specs$suffix[i]

    fit <- rd2d::rd2d(
      Y = sample$Y,
      X = sample$X,
      assignment = sample$assignment,
      b = eval,
      fuzzy = sample$fuzzy,
      bwparam = bwparam,
      stdvars = FALSE,
      masspoints = "off",
      vce = "hc1",
      params.cov = c("main", "itt", "fs")
    )

    rows <- rbind(
      make_output_rows(fit, "itt", cal$design, j, cal$dgp, cal$s, bwparam),
      make_output_rows(fit, "fs", cal$design, j, cal$dgp, cal$s, bwparam),
      make_output_rows(fit, "main", cal$design, j, cal$dgp, cal$s, bwparam)
    )
    write_raw_result(rows, j, cal, suffix)
  }
}

# Additional sharp DGP results: the outcome discontinuity and control-side
# mean, each using the sharp bandwidth selector. These are not printed in the SA.
run_sharp_calibration <- function(j, cal, sample, eval, repp) {
  bwparam <- "main"
  suffix <- "bwmain"

  fit <- rd2d::rd2d(
    Y = sample$Y,
    X = sample$X,
    assignment = sample$assignment,
    b = eval,
    stdvars = FALSE,
    masspoints = "off",
    vce = "hc1",
    params.other = "main.0",
    params.cov = c("main", "main.0")
  )

  rows <- rbind(
    make_output_rows(fit, "main", cal$design, j, cal$dgp, cal$s, bwparam),
    make_output_rows(fit, "main.0", cal$design, j, cal$dgp, cal$s, bwparam)
  )
  write_raw_result(rows, j, cal, suffix)
}

# One worker handles all six DGPs for a given replication. Reseeding before
# each DGP makes its sample and inference draws independent of worker order.
simulate_replication <- function(j, fuzzy_calibrations, sharp_calibrations,
                                 eval, n, repp, seed_base, bw_specs) {
  for (cal in fuzzy_calibrations) {
    set.seed(simulation_seed(seed_base, j, cal$design, cal$dgp))
    sample <- simulate_sample(cal, n)
    run_fuzzy_calibration(j, cal, sample, eval, repp, bw_specs)
  }

  for (cal in sharp_calibrations) {
    set.seed(simulation_seed(seed_base, j, cal$design, cal$dgp))
    sample <- simulate_sample(cal, n)
    run_sharp_calibration(j, cal, sample, eval, repp)
  }

  TRUE
}

# Use the same replication function in sequential and parallel execution.
# Parallel workers load rd2d and receive the fixed calibrations and settings.
run_simulations <- function(m, num_workers) {
  if (num_workers <= 1) {
    message("Running sequentially.")
    invisible(lapply(seq_len(m), function(j) {
      simulate_replication(
        j = j,
        fuzzy_calibrations = fuzzy_calibrations,
        sharp_calibrations = sharp_calibrations,
        eval = eval,
        n = n,
        repp = repp,
        seed_base = seed_base,
        bw_specs = bw_specs
      )
    }))
    return(invisible(TRUE))
  }

  cl <- parallel::makeCluster(num_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(cl, "script_dir", envir = globalenv())
  parallel::clusterEvalQ(cl, {
    setwd(script_dir)
    library(rd2d)
    NULL
  })

  message(sprintf("Running in parallel with %d worker(s).", num_workers))

  parallel::clusterExport(
    cl,
    c(
      "fuzzy_calibrations", "sharp_calibrations", "eval", "n", "repp",
      "seed_base", "output_dir", "bw_specs",
      "simulation_seed", "design_matrix", "draw_scores", "assignment_rule",
      "simulate_sample", "summary_table", "table_col", "estimand_name",
      "make_output_rows", "raw_result_file", "write_raw_result",
      "run_fuzzy_calibration", "run_sharp_calibration", "simulate_replication"
    ),
    envir = environment()
  )

  parallel::parLapplyLB(cl, seq_len(m), function(j) {
    simulate_replication(
      j = j,
      fuzzy_calibrations = fuzzy_calibrations,
      sharp_calibrations = sharp_calibrations,
      eval = eval,
      n = n,
      repp = repp,
      seed_base = seed_base,
      bw_specs = bw_specs
    )
  })

  invisible(TRUE)
}

############################## Main Workflow ###################################

# 1. Start the run and replace earlier simulation CSV outputs.
set.seed(3)
clean_output_dir()

# 2. Load the calibration sample and construct the 21 evaluation points.
spp <- load_spp_data("spp.csv")
eval <- make_eval_grid()

# 3. Calibrate degrees s = 1, 2, and 3 once, holding the parameters fixed
# across replications. Fuzzy calibrations generate Tables SA-13--SA-30.
fuzzy_calibrations <- list(
  calibrate_dgp(spp, s = 1, dgp = 1, design = "fuzzy"),
  calibrate_dgp(spp, s = 2, dgp = 2, design = "fuzzy"),
  calibrate_dgp(spp, s = 3, dgp = 3, design = "fuzzy")
)
# The sharp calibrations generate the six additional sharp/control tables.
sharp_calibrations <- list(
  calibrate_dgp(spp, s = 1, dgp = 1, design = "sharp"),
  calibrate_dgp(spp, s = 2, dgp = 2, design = "sharp"),
  calibrate_dgp(spp, s = 3, dgp = 3, design = "sharp")
)
calibrations <- c(fuzzy_calibrations, sharp_calibrations)

# 4. Set the replication count, sample size, Gaussian draws, and base seed.
m <- get_env_int("RD2D_M", 5000)
n <- get_env_int("RD2D_N", 10000)
repp <- get_env_int("RD2D_REPP", 2000)
seed_base <- get_env_int("RD2D_SEED", 20260510)

# Set RD2D_WORKERS to the CPUs allocated to this job on a shared server.
# Each worker runs one replication at a time.
num_cores <- parallel::detectCores()
if (is.na(num_cores)) num_cores <- 2
default_workers <- max(1, num_cores - 4)
num_workers <- min(m, get_env_int("RD2D_WORKERS", default_workers))

# 5. Save calibration parameters, analytic targets, estimand names, and
# package/run metadata before starting the simulations.
write_output_csv(make_dgp_parameter_table(calibrations), "simuls_dgp_parameters.csv")
write_output_csv(make_targets(eval, calibrations), "simuls_dgp_targets.csv")
write_output_csv(estimand_specs, "simuls_estimands.csv")
write_output_csv(
  data.frame(
    name = c(
      "rd2d.version", "m", "n", "repp", "seed_base", "workers",
      "bwparams", "generated_at"
    ),
    value = c(
      rd2d_package_version(), m, n, repp, seed_base, num_workers,
      paste(paste(bw_specs$bwparam, bw_specs$suffix, sep = "="), collapse = "; "),
      as.character(Sys.time())
    ),
    stringsAsFactors = FALSE
  ),
  "simuls_metadata.csv"
)

# 6. Run the replications and record elapsed and CPU time. At the defaults,
# the six fuzzy and three sharp specifications produce 45,000 raw CSV files.
timing <- system.time(run_simulations(m, num_workers))
write_output_csv(
  data.frame(
    metric = names(timing),
    seconds = as.numeric(timing),
    stringsAsFactors = FALSE
  ),
  "simuls_runtime.csv"
)

# 7. Check that every expected raw file exists. The table script reads these
# files and the saved targets; it does not rerun estimation or simulation.
expected_raw <- c(unlist(lapply(fuzzy_calibrations, function(cal) {
  unlist(lapply(bw_specs$suffix, function(suffix) {
    basename(raw_result_file(seq_len(m), cal$dgp, cal$design, cal$s, suffix))
  }), use.names = FALSE)
}), use.names = FALSE), unlist(lapply(sharp_calibrations, function(cal) {
  basename(raw_result_file(seq_len(m), cal$dgp, cal$design, cal$s, "bwmain"))
}), use.names = FALSE))
missing_raw <- expected_raw[!file.exists(file.path(output_dir, expected_raw))]
if (length(missing_raw) > 0) {
  stop(sprintf("Missing expected raw simulation file: %s", missing_raw[1]), call. = FALSE)
}

cat("Simulation output complete.\n")
cat(sprintf("Raw simulation files written to: %s\n", output_dir))
cat(sprintf("Generated %d raw CSV file(s).\n", length(expected_raw)))
