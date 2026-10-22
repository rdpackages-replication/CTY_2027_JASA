################################################################################
# Estimation and Inference in Boundary Discontinuity Designs:
# Location-Based Methods
# Empirical Application: SPP Data
################################################################################
# Run this file first, followed by CTY_2027_JASA--empapp_tables.py and
# CTY_2027_JASA--empapp_figures.py. Results are saved as CSV files in output/.
# bwparam="main" selects fuzzy bandwidths (Section SA-10.2); bwparam="itt"
# selects ITT bandwidths (the paper and Section SA-10.1).

from __future__ import annotations

import argparse
import os
from datetime import datetime
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path

import numpy as np
import pandas as pd
from rd2d import rd2d


################################################################################
# Setup
################################################################################

# For interactive execution, start in the replication folder.
script_dir = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
os.chdir(script_dir)
parser = argparse.ArgumentParser()
parser.add_argument("--data", type=Path, default=Path("spp.csv"))
parser.add_argument("--output", type=Path, default=Path("output"))
parser.add_argument("--repp", type=int, default=int(os.getenv("RD2D_EMP_REPP", "2000")))
if "__file__" in globals() and __name__ == "__main__":
    args = parser.parse_args()
else:
    args = parser.parse_args([])
if args.repp <= 0:
    raise ValueError("--repp must be a positive integer.")
output_dir = args.output
output_dir.mkdir(parents=True, exist_ok=True)
repp = args.repp

# Gaussian draws are requested in summary(), after fitting. The Python package
# uses its own fixed seed for these draws; the R and Python draws differ.
try:
    rd2d_version = version("rd2d")
except PackageNotFoundError:
    rd2d_version = "unknown"

# Replace earlier empirical CSV outputs when running the full replication.
for old in output_dir.glob("empapp_*.csv"):
    old.unlink()


################################################################################
# SPP Data
################################################################################

spp = pd.read_csv(args.data)
expected_columns = [
    "running_saber11", "running_sisben", "eligible_spp",
    "beneficiary_spp", "spadies_any", "icfes_educm1",
]
missing_columns = [name for name in expected_columns if name not in spp.columns]
if missing_columns:
    raise ValueError(f"spp.csv is missing columns: {', '.join(missing_columns)}")
spp = spp.loc[:, expected_columns].copy()
spp.columns = ["x.1", "x.2", "assignment", "fuzzy", "Y", "Z"]

# Eligibility requires both centered scores to be nonnegative. The variable
# fuzzy records actual program take-up, Y enrollment, and Z mother's education.
expected_assignment = ((spp["x.1"] >= 0) & (spp["x.2"] >= 0)).astype(int)
valid = spp["assignment"].notna()
if not np.all(spp.loc[valid, "assignment"].to_numpy() == expected_assignment.loc[valid].to_numpy()):
    raise ValueError("eligible_spp does not match 1{x.1 >= 0 & x.2 >= 0}.")

# Enrollment and mother's education use separate complete-case samples.
data_main = spp.loc[:, ["x.1", "x.2", "assignment", "fuzzy", "Y"]].dropna().copy()
data_balance = spp.loc[:, ["x.1", "x.2", "assignment", "fuzzy", "Z"]].dropna().copy()


################################################################################
# Evaluation Points on the Boundary
################################################################################

# Points 1--20 follow the academic threshold; points 21--40 follow the wealth
# threshold. Point 21 is the corner where the eligibility thresholds meet.
neval = 40
nhalf = int(np.ceil(neval / 2))
eval_grid = pd.concat([
    pd.DataFrame({"x.1": np.zeros(nhalf),
                  "x.2": 40 - np.arange(nhalf) * 40 / nhalf}),
    pd.DataFrame({"x.1": np.arange(neval - nhalf) * 56 / nhalf,
                  "x.2": np.zeros(neval - nhalf)}),
], ignore_index=True)

# summary() normalizes the weights, giving each boundary point equal weight.
wbate_weights = np.ones(neval)
spp.to_csv(output_dir / "empapp_data.csv", index=False, na_rep="")
eval_grid.to_csv(output_dir / "empapp_eval.csv", index=False, na_rep="")
metadata = pd.DataFrame({
    "name": ["rd2d.version", "repp", "bwparams", "generated_at"],
    "value": [rd2d_version, str(repp), "main=bwmain; itt=bwitt",
              datetime.now().strftime("%Y-%m-%d %H:%M:%S")],
})
metadata.to_csv(output_dir / "empapp_metadata.csv", index=False, na_rep="")


################################################################################
# Fuzzy Bandwidth Selection (Section SA-10.2)
################################################################################

print("Running empirical application with bwparam='main'.")

# College enrollment. The same fit provides the fuzzy, ITT, first stage,
# and control baseline results. Covariances support bands and aggregation.
result_main = rd2d(
    data_main["Y"].to_numpy(),
    data_main[["x.1", "x.2"]].to_numpy(),
    data_main["assignment"].to_numpy(),
    eval_grid.to_numpy(),
    fuzzy=data_main["fuzzy"].to_numpy(),
    bwparam="main",
    params_other="itt.0",
    params_cov=["main", "itt", "fs", "itt.0"],
)

# Fuzzy estimates of the local Wald ratio.
summary_fuzzy = result_main.summary(
    output="main", cbands="main", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_fuzzy = summary_fuzzy.tables["main"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_fuzzy.index[neval:])
table_fuzzy.insert(0, "row", row_labels)
table_fuzzy.to_csv(output_dir / "empapp_main_fuzzy_bwmain.csv", index=False, na_rep="")

# Reduced-form effects on college enrollment.
summary_itt = result_main.summary(
    output="itt", cbands="itt", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_itt = summary_itt.tables["itt"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_itt.index[neval:])
table_itt.insert(0, "row", row_labels)
table_itt.to_csv(output_dir / "empapp_main_itt_bwmain.csv", index=False, na_rep="")

# First stage effects on program take-up.
summary_fs = result_main.summary(
    output="fs", cbands="fs", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_fs = summary_fs.tables["fs"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_fs.index[neval:])
table_fs.insert(0, "row", row_labels)
table_fs.to_csv(output_dir / "empapp_main_fs_bwmain.csv", index=False, na_rep="")

# Control-side baseline enrollment probabilities.
summary_itt0 = result_main.summary(
    output="itt.0", cbands="itt.0", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_itt0 = summary_itt0.tables["itt.0"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_itt0.index[neval:])
table_itt0.insert(0, "row", row_labels)
table_itt0.to_csv(output_dir / "empapp_main_itt0_bwmain.csv", index=False, na_rep="")

# Covariate balance uses mother's education and its own complete-case sample.
result_balance = rd2d(
    data_balance["Z"].to_numpy(),
    data_balance[["x.1", "x.2"]].to_numpy(),
    data_balance["assignment"].to_numpy(),
    eval_grid.to_numpy(),
    fuzzy=data_balance["fuzzy"].to_numpy(),
    bwparam="main",
    params_other="itt.0",
    params_cov=["main", "itt", "fs", "itt.0"],
)
summary_balance = result_balance.summary(
    output="itt", cbands="itt", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_balance = summary_balance.tables["itt"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_balance.index[neval:])
table_balance.insert(0, "row", row_labels)
table_balance.to_csv(output_dir / "empapp_covbal_itt_bwmain.csv", index=False, na_rep="")

# Divide each point estimate by its control baseline, then take the average
# and maximum of the 40 percentages. These are not ratios of aggregate rows.
# Read the saved CSVs to retain the original replication's numerical precision.
fuzzy = pd.read_csv(output_dir / "empapp_main_fuzzy_bwmain.csv")
itt = pd.read_csv(output_dir / "empapp_main_itt_bwmain.csv")
itt0 = pd.read_csv(output_dir / "empapp_main_itt0_bwmain.csv")
if not (fuzzy["row"].equals(itt["row"]) and fuzzy["row"].equals(itt0["row"])):
    raise ValueError("Cannot construct percent table because summary rows do not align.")

# Keep the shared CSV row keys; displays use Average/Maximum for percentages.
points = ~fuzzy["row"].isin(["WBATE", "LBATE"])
denom = itt0["estimate.p"]
if not points.any() or not np.isfinite(denom[points]).all() or (denom[points] == 0).any():
    raise ValueError("Percentage summaries require finite, nonzero pointwise baselines.")
percent = pd.DataFrame(
    {
        "row": fuzzy["row"],
        "b1": fuzzy["b1"],
        "b2": fuzzy["b2"],
        "h01": fuzzy["h01"],
        "h02": fuzzy["h02"],
        "h11": fuzzy["h11"],
        "h12": fuzzy["h12"],
        "N.Co": fuzzy["N.Co"],
        "N.Tr": fuzzy["N.Tr"],
        "itt.percent": 100 * itt["estimate.p"] / denom,
        "fuzzy.percent": 100 * fuzzy["estimate.p"] / denom,
    }
)
for column in ["itt.percent", "fuzzy.percent"]:
    values = percent.loc[points, column]
    if not np.isfinite(values).all():
        raise ValueError("Percentage summaries require finite pointwise estimates.")
    percent.loc[percent["row"] == "WBATE", column] = values.mean()
    percent.loc[percent["row"] == "LBATE", column] = values.max()
percent.to_csv(output_dir / "empapp_main_percent_bwmain.csv", index=False, na_rep="")


################################################################################
# ITT Bandwidth Selection (Paper and Section SA-10.1)
################################################################################

print("Running empirical application with bwparam='itt'.")

# College enrollment. The same fit provides the fuzzy, ITT, first stage,
# and control baseline results. Covariances support bands and aggregation.
result_main = rd2d(
    data_main["Y"].to_numpy(),
    data_main[["x.1", "x.2"]].to_numpy(),
    data_main["assignment"].to_numpy(),
    eval_grid.to_numpy(),
    fuzzy=data_main["fuzzy"].to_numpy(),
    bwparam="itt",
    params_other="itt.0",
    params_cov=["main", "itt", "fs", "itt.0"],
)

# Fuzzy estimates of the local Wald ratio.
summary_fuzzy = result_main.summary(
    output="main", cbands="main", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_fuzzy = summary_fuzzy.tables["main"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_fuzzy.index[neval:])
table_fuzzy.insert(0, "row", row_labels)
table_fuzzy.to_csv(output_dir / "empapp_main_fuzzy_bwitt.csv", index=False, na_rep="")

# Reduced-form effects on college enrollment.
summary_itt = result_main.summary(
    output="itt", cbands="itt", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_itt = summary_itt.tables["itt"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_itt.index[neval:])
table_itt.insert(0, "row", row_labels)
table_itt.to_csv(output_dir / "empapp_main_itt_bwitt.csv", index=False, na_rep="")

# First stage effects on program take-up.
summary_fs = result_main.summary(
    output="fs", cbands="fs", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_fs = summary_fs.tables["fs"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_fs.index[neval:])
table_fs.insert(0, "row", row_labels)
table_fs.to_csv(output_dir / "empapp_main_fs_bwitt.csv", index=False, na_rep="")

# Control-side baseline enrollment probabilities.
summary_itt0 = result_main.summary(
    output="itt.0", cbands="itt.0", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_itt0 = summary_itt0.tables["itt.0"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_itt0.index[neval:])
table_itt0.insert(0, "row", row_labels)
table_itt0.to_csv(output_dir / "empapp_main_itt0_bwitt.csv", index=False, na_rep="")

# Covariate balance uses mother's education and its own complete-case sample.
result_balance = rd2d(
    data_balance["Z"].to_numpy(),
    data_balance[["x.1", "x.2"]].to_numpy(),
    data_balance["assignment"].to_numpy(),
    eval_grid.to_numpy(),
    fuzzy=data_balance["fuzzy"].to_numpy(),
    bwparam="itt",
    params_other="itt.0",
    params_cov=["main", "itt", "fs", "itt.0"],
)
summary_balance = result_balance.summary(
    output="itt", cbands="itt", repp=repp,
    WBATE=wbate_weights, LBATE=True,
)
table_balance = summary_balance.tables["itt"].copy()
row_labels = [str(i) for i in range(1, neval + 1)]
row_labels.extend(str(i) for i in table_balance.index[neval:])
table_balance.insert(0, "row", row_labels)
table_balance.to_csv(output_dir / "empapp_covbal_itt_bwitt.csv", index=False, na_rep="")

# Divide each point estimate by its control baseline, then take the average
# and maximum of the 40 percentages. These are not ratios of aggregate rows.
# Read the saved CSVs to retain the original replication's numerical precision.
fuzzy = pd.read_csv(output_dir / "empapp_main_fuzzy_bwitt.csv")
itt = pd.read_csv(output_dir / "empapp_main_itt_bwitt.csv")
itt0 = pd.read_csv(output_dir / "empapp_main_itt0_bwitt.csv")
if not (fuzzy["row"].equals(itt["row"]) and fuzzy["row"].equals(itt0["row"])):
    raise ValueError("Cannot construct percent table because summary rows do not align.")

# Keep the shared CSV row keys; displays use Average/Maximum for percentages.
points = ~fuzzy["row"].isin(["WBATE", "LBATE"])
denom = itt0["estimate.p"]
if not points.any() or not np.isfinite(denom[points]).all() or (denom[points] == 0).any():
    raise ValueError("Percentage summaries require finite, nonzero pointwise baselines.")
percent = pd.DataFrame(
    {
        "row": fuzzy["row"],
        "b1": fuzzy["b1"],
        "b2": fuzzy["b2"],
        "h01": fuzzy["h01"],
        "h02": fuzzy["h02"],
        "h11": fuzzy["h11"],
        "h12": fuzzy["h12"],
        "N.Co": fuzzy["N.Co"],
        "N.Tr": fuzzy["N.Tr"],
        "itt.percent": 100 * itt["estimate.p"] / denom,
        "fuzzy.percent": 100 * fuzzy["estimate.p"] / denom,
    }
)
for column in ["itt.percent", "fuzzy.percent"]:
    values = percent.loc[points, column]
    if not np.isfinite(values).all():
        raise ValueError("Percentage summaries require finite pointwise estimates.")
    percent.loc[percent["row"] == "WBATE", column] = values.mean()
    percent.loc[percent["row"] == "LBATE", column] = values.max()
percent.to_csv(output_dir / "empapp_main_percent_bwitt.csv", index=False, na_rep="")

print(f"CSV outputs written to: {output_dir}")
print("Generated 15 CSV file(s).")
