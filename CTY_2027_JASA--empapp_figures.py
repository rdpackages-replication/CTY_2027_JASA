################################################################################
# Estimation and Inference in Boundary Discontinuity Designs:
# Location-Based Methods
# Empirical Application: Figures
################################################################################
# Read the CSV results from CTY_2027_JASA--empapp.py. Shared formatting
# functions precede the explicit manuscript sections; additional outputs come last.

from __future__ import annotations

import os
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
os.environ.setdefault("MPLCONFIGDIR", str(SCRIPT_DIR / ".matplotlib-cache"))

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.colors import BoundaryNorm, ListedColormap, TwoSlopeNorm

OUTPUT_DIR = SCRIPT_DIR / "output"
FIGURES_DIR = SCRIPT_DIR / "figures"
AGGREGATE_LABELS = {"WBATE", "LBATE"}
EVAL_AXIS_BREAKS = [1, 5, 10, 15, 21, 25, 30, 35, 40]
EVAL_AXIS_LABELS = [f"b{i}" for i in EVAL_AXIS_BREAKS]

FIGURES_DIR.mkdir(parents=True, exist_ok=True)
for old in FIGURES_DIR.glob("*.png"):
    old.unlink()


################################################################################
# Shared Input and Drawing Functions
################################################################################

def read_output_csv(file_name: str) -> pd.DataFrame:
    path = OUTPUT_DIR / file_name
    if not path.exists():
        raise FileNotFoundError(f"Missing output file: {path}. Run CTY_2027_JASA--empapp.py first.")
    return pd.read_csv(path)


def read_result_table(file_name: str) -> pd.DataFrame:
    tab = read_output_csv(file_name)
    required = ["row", "b1", "b2", "estimate.p", "estimate.q", "p.value", "ci.lower", "ci.upper", "cb.lower", "cb.upper"]
    missing = [name for name in required if name not in tab.columns]
    if missing:
        raise ValueError(f"{file_name} is missing columns: {', '.join(missing)}")
    tab["row"] = tab["row"].astype(str)
    return tab


def read_percent_table(file_name: str) -> pd.DataFrame:
    tab = read_output_csv(file_name)
    required = ["row", "itt.percent", "fuzzy.percent"]
    missing = [name for name in required if name not in tab.columns]
    if missing:
        raise ValueError(f"{file_name} is missing columns: {', '.join(missing)}")
    tab["row"] = tab["row"].astype(str)
    return tab


def point_rows(tab: pd.DataFrame) -> pd.DataFrame:
    return tab.loc[~tab["row"].isin(AGGREGATE_LABELS)].copy()


def aggregate_value(tab: pd.DataFrame, name: str, column: str = "estimate.p") -> float:
    row = tab.loc[tab["row"] == name]
    if row.empty:
        return np.nan
    return float(row.iloc[0][column])


def save_png(fig: plt.Figure, file_name: str, width: float = 6, height: float = 5) -> None:
    fig.set_size_inches(width, height)
    fig.tight_layout()
    fig.savefig(FIGURES_DIR / file_name, dpi=300)
    plt.close(fig)


def plot_ylim(*arrays: object) -> tuple[float, float]:
    vals = np.concatenate([np.asarray(x, dtype=float).ravel() for x in arrays if x is not None])
    vals = vals[np.isfinite(vals)]
    if vals.size == 0:
        return -1.0, 1.0
    lo, hi = float(np.min(vals)), float(np.max(vals))
    if np.isclose(lo, hi):
        pad = max(0.02, abs(lo) * 0.08, 0.02)
    else:
        pad = max(0.02, (hi - lo) * 0.08)
    return lo - pad, hi + pad


def style_axes(ax: plt.Axes) -> None:
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def save_scatter_plot(dat: pd.DataFrame, eval_grid: pd.DataFrame, file_name: str) -> None:
    fig, ax = plt.subplots()
    control = dat["assignment"] == 0
    treated = dat["assignment"] == 1
    ax.scatter(dat.loc[control, "x.1"], dat.loc[control, "x.2"], s=3, c="indianred", alpha=0.18, marker="s", label="Control")
    ax.scatter(dat.loc[treated, "x.1"], dat.loc[treated, "x.2"], s=3, c="#104e8b", alpha=0.18, marker="o", label="Treatment")
    ax.plot([0, 0], [0, 55], color="0.35", lw=1.0, label="Boundary")
    ax.plot([0, 80], [0, 0], color="0.35", lw=1.0)
    ax.scatter(eval_grid["x.1"], eval_grid["x.2"], s=8, c="black", zorder=4)
    for idx in EVAL_AXIS_BREAKS:
        row = eval_grid.iloc[idx - 1]
        x = -3.2 if row["x.1"] == 0 else row["x.1"]
        y = row["x.2"] if row["x.1"] == 0 else -2.4
        ha = "right" if row["x.1"] == 0 else "center"
        va = "center" if row["x.1"] == 0 else "top"
        ax.text(x, y, f"b{idx}", ha=ha, va=va, fontsize=8)
    ax.set_xlim(-80, 100)
    ax.set_ylim(-40, 60)
    ax.set_xlabel("Saber 11")
    ax.set_ylabel("Sisben")
    ax.legend(loc="upper left", bbox_to_anchor=(0.10, 0.98), frameon=True)
    style_axes(ax)
    save_png(fig, file_name)


def save_point_plot(tab: pd.DataFrame, ylab: str, file_name: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    x = np.arange(1, len(pts) + 1)
    wbate = aggregate_value(tab, "WBATE")
    lbate = aggregate_value(tab, "LBATE")
    fig, ax = plt.subplots()
    ax.axvline(21, color="0.9", lw=0.45)
    ax.scatter(x, pts["estimate.p"], s=12, color="black", label="Estimate", zorder=3)
    if np.isfinite(wbate):
        ax.axhline(wbate, color="#104e8b", lw=0.7, ls=":", label="WBATE")
    if np.isfinite(lbate):
        ax.axhline(lbate, color="firebrick", lw=0.7, ls="--", label="LBATE")
    ax.set_xticks(EVAL_AXIS_BREAKS, EVAL_AXIS_LABELS)
    ax.set_xlim(1, 40)
    ax.set_ylim(*plot_ylim(pts["estimate.p"], [wbate, lbate]))
    ax.set_xlabel("Cutoffs on the boundary")
    ax.set_ylabel(ylab)
    ax.legend(frameon=True)
    style_axes(ax)
    save_png(fig, file_name)


def save_percent_point_plot(tab: pd.DataFrame, value_col: str, ylab: str, file_name: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    x = np.arange(1, len(pts) + 1)
    aggregates = tab.loc[tab["row"].isin(AGGREGATE_LABELS)].copy()
    fig, ax = plt.subplots()
    ax.axvline(21, color="0.9", lw=0.45)
    ax.scatter(x, pts[value_col], s=12, color="black", label="Estimate", zorder=3)
    for label, color, ls in [("WBATE", "#104e8b", ":"), ("LBATE", "firebrick", "--")]:
        row = aggregates.loc[aggregates["row"] == label]
        if not row.empty and np.isfinite(float(row.iloc[0][value_col])):
            ax.axhline(float(row.iloc[0][value_col]), color=color, lw=0.7, ls=ls, label={"WBATE": "Average", "LBATE": "Maximum"}[label])
    ax.set_xticks(EVAL_AXIS_BREAKS, EVAL_AXIS_LABELS)
    ax.set_xlim(1, 40)
    ax.set_ylim(*plot_ylim(pts[value_col], aggregates[value_col]))
    ax.set_xlabel("Cutoffs on the boundary")
    ax.set_ylabel(ylab)
    ax.legend(frameon=True)
    style_axes(ax)
    save_png(fig, file_name)


def save_inference_plot(tab: pd.DataFrame, ylab: str, file_name: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    x = np.arange(1, len(pts) + 1)
    band_lower = pts["cb.lower"].where(np.isfinite(pts["cb.lower"]), pts["ci.lower"])
    band_upper = pts["cb.upper"].where(np.isfinite(pts["cb.upper"]), pts["ci.upper"])
    fig, ax = plt.subplots()
    ax.axvline(21, color="0.9", lw=0.45)
    ax.fill_between(x, band_lower, band_upper, color="#104e8b", alpha=0.14, label="95% CB")
    ax.errorbar(x, pts["estimate.p"], yerr=[pts["estimate.p"] - pts["ci.lower"], pts["ci.upper"] - pts["estimate.p"]], fmt="none", ecolor="black", elinewidth=0.35, capsize=1.5, label="95% CI")
    ax.scatter(x, pts["estimate.p"], s=10, color="black", label="Estimate", zorder=3)
    ax.set_xticks(EVAL_AXIS_BREAKS, EVAL_AXIS_LABELS)
    ax.set_xlim(1, 40)
    ax.set_ylim(*plot_ylim(pts["estimate.p"], pts["ci.lower"], pts["ci.upper"], band_lower, band_upper))
    ax.set_xlabel("Cutoffs on the boundary")
    ax.set_ylabel(ylab)
    ax.legend(frameon=True)
    style_axes(ax)
    save_png(fig, file_name)


def save_effect_heatmap(tab: pd.DataFrame, eval_grid: pd.DataFrame, label: str, file_name: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    values = pts["estimate.p"].to_numpy(float)
    fig, ax = plt.subplots()
    mid = float(np.nanmedian(values)) if np.isfinite(values).any() else 0.0
    norm = TwoSlopeNorm(vmin=float(np.nanmin(values)), vcenter=mid, vmax=float(np.nanmax(values))) if not np.isclose(np.nanmin(values), np.nanmax(values)) else None
    sc = ax.scatter(eval_grid["x.1"], eval_grid["x.2"], c=values, s=240, marker="s", cmap="RdBu_r", norm=norm, edgecolor="white", linewidth=0.4)
    for i, row in eval_grid.iterrows():
        ax.text(row["x.1"], row["x.2"], f"{i + 1:02d}", ha="center", va="center", fontsize=7)
    fig.colorbar(sc, ax=ax, label=label)
    ax.set_xlim(-10, 64)
    ax.set_ylim(-10, 42)
    ax.set_aspect(56 / 40)
    ax.set_xlabel("Saber 11")
    ax.set_ylabel("Sisben")
    style_axes(ax)
    save_png(fig, file_name)


def save_pvalue_heatmap(tab: pd.DataFrame, eval_grid: pd.DataFrame, label: str, file_name: str) -> None:
    pts = point_rows(tab).reset_index(drop=True)
    pvals = pts["p.value"].to_numpy(float)
    bins = [0, 0.001, 0.01, 0.05, 0.1, 1.0]
    colors = ["#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf"]
    cmap = ListedColormap(colors)
    norm = BoundaryNorm(bins, cmap.N, clip=True)
    fig, ax = plt.subplots()
    sc = ax.scatter(eval_grid["x.1"], eval_grid["x.2"], c=np.clip(pvals, 0, 1), s=240, marker="s", cmap=cmap, norm=norm, edgecolor="white", linewidth=0.4)
    for i, row in eval_grid.iterrows():
        ax.text(row["x.1"], row["x.2"], f"{i + 1:02d}", ha="center", va="center", fontsize=7)
    cbar = fig.colorbar(sc, ax=ax, label=label, boundaries=bins)
    cbar.set_ticks([0.0005, 0.0055, 0.03, 0.075, 0.55])
    cbar.set_ticklabels(["0.000", "[0.001, 0.010)", "[0.010, 0.050)", "[0.050, 0.100)", ">= 0.100"])
    ax.set_xlim(-10, 64)
    ax.set_ylim(-10, 42)
    ax.set_aspect(56 / 40)
    ax.set_xlabel("Saber 11")
    ax.set_ylabel("Sisben")
    style_axes(ax)
    save_png(fig, file_name)


################################################################################
# Read Saved Results
################################################################################

data = read_output_csv("empapp_data.csv")
eval_grid = read_output_csv("empapp_eval.csv")

# ITT bandwidth selection.
fuzzy_bwitt = read_result_table("empapp_main_fuzzy_bwitt.csv")
itt_bwitt = read_result_table("empapp_main_itt_bwitt.csv")
fs_bwitt = read_result_table("empapp_main_fs_bwitt.csv")
baseline_bwitt = read_result_table("empapp_main_itt0_bwitt.csv")
balance_bwitt = read_result_table("empapp_covbal_itt_bwitt.csv")
percent_bwitt = read_percent_table("empapp_main_percent_bwitt.csv")

# Fuzzy bandwidth selection.
fuzzy_bwmain = read_result_table("empapp_main_fuzzy_bwmain.csv")
itt_bwmain = read_result_table("empapp_main_itt_bwmain.csv")
fs_bwmain = read_result_table("empapp_main_fs_bwmain.csv")
baseline_bwmain = read_result_table("empapp_main_itt0_bwmain.csv")
balance_bwmain = read_result_table("empapp_covbal_itt_bwmain.csv")
percent_bwmain = read_percent_table("empapp_main_percent_bwmain.csv")


################################################################################
# Figure 1
################################################################################

# Figure 1(a)
# Input: output/empapp_data.csv, output/empapp_eval.csv.
save_scatter_plot(data, eval_grid, file_name="empapp_scatter_bwitt.png")

# Figure 1(b)
# Input: output/empapp_main_itt_bwitt.csv.
save_point_plot(itt_bwitt, ylab="Intention-to-treat effect",
    file_name="empapp_main_itt_pointest_bwitt.png")


################################################################################
# Figure 2
################################################################################

# Figure 2(a)
# Input: output/empapp_main_itt_bwitt.csv.
save_inference_plot(itt_bwitt, ylab="Intention-to-treat effect",
    file_name="empapp_main_itt_inference_bwitt.png")

# Figure 2(b)
# Input: output/empapp_main_fs_bwitt.csv.
save_inference_plot(fs_bwitt, ylab="First stage",
    file_name="empapp_main_fs_inference_bwitt.png")

# Figure 2(c)
# Input: output/empapp_main_fuzzy_bwitt.csv.
save_inference_plot(fuzzy_bwitt, ylab="Fuzzy estimate",
    file_name="empapp_main_fuzzy_inference_bwitt.png")

# Figure 2(d)
# Input: output/empapp_covbal_itt_bwitt.csv.
save_inference_plot(balance_bwitt, ylab="Covariate balance ITT",
    file_name="empapp_covbal_itt_inference_bwitt.png")


################################################################################
# Figure SA-1
################################################################################

# Figure SA-1(a)
# Input: output/empapp_main_itt_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(itt_bwitt, eval_grid, label="ITT",
    file_name="empapp_main_itt_heatmap_bwitt.png")

# Figure SA-1(b)
# Input: output/empapp_main_itt_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(itt_bwitt, eval_grid, label="ITT",
    file_name="empapp_main_itt_heatmap_pval_bwitt.png")


################################################################################
# Figure SA-2
################################################################################

# Figure SA-2(a)
# Input: output/empapp_main_fs_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(fs_bwitt, eval_grid, label="FS",
    file_name="empapp_main_fs_heatmap_bwitt.png")

# Figure SA-2(b)
# Input: output/empapp_main_fs_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fs_bwitt, eval_grid, label="FS",
    file_name="empapp_main_fs_heatmap_pval_bwitt.png")


################################################################################
# Figure SA-3
################################################################################

# Figure SA-3(a)
# Input: output/empapp_main_fuzzy_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(fuzzy_bwitt, eval_grid, label="Fuzzy",
    file_name="empapp_main_fuzzy_heatmap_bwitt.png")

# Figure SA-3(b)
# Input: output/empapp_main_fuzzy_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fuzzy_bwitt, eval_grid, label="Fuzzy",
    file_name="empapp_main_fuzzy_heatmap_pval_bwitt.png")


################################################################################
# Figure SA-4
################################################################################

# Figure SA-4(a)
# Input: output/empapp_covbal_itt_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(balance_bwitt, eval_grid, label="Cov. Bal.",
    file_name="empapp_covbal_itt_heatmap_bwitt.png")

# Figure SA-4(b)
# Input: output/empapp_covbal_itt_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(balance_bwitt, eval_grid, label="Cov. Bal.",
    file_name="empapp_covbal_itt_heatmap_pval_bwitt.png")


################################################################################
# Figure SA-5
################################################################################

# Figure SA-5(a)
# Input: output/empapp_main_itt0_bwitt.csv.
save_inference_plot(baseline_bwitt, ylab="Control-side baseline outcome",
    file_name="empapp_main_itt0_inference_bwitt.png")

# Figure SA-5(b)
# Input: output/empapp_main_itt0_bwitt.csv, output/empapp_eval.csv.
save_effect_heatmap(baseline_bwitt, eval_grid, label="Control baseline",
    file_name="empapp_main_itt0_heatmap_bwitt.png")


################################################################################
# Figure SA-6
################################################################################

# Figure SA-6(a)
# Input: output/empapp_main_percent_bwitt.csv.
save_percent_point_plot(percent_bwitt, value_col="itt.percent", ylab="ITT / ITT.0 (%)",
    file_name="empapp_main_percent_itt_pointest_bwitt.png")

# Figure SA-6(b)
# Input: output/empapp_main_percent_bwitt.csv.
save_percent_point_plot(percent_bwitt, value_col="fuzzy.percent", ylab="Fuzzy / ITT.0 (%)",
    file_name="empapp_main_percent_fuzzy_pointest_bwitt.png")


################################################################################
# Figure SA-7
################################################################################

# Figure SA-7(a)
# Input: output/empapp_main_itt_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(itt_bwmain, eval_grid, label="ITT",
    file_name="empapp_main_itt_heatmap_bwmain.png")

# Figure SA-7(b)
# Input: output/empapp_main_itt_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(itt_bwmain, eval_grid, label="ITT",
    file_name="empapp_main_itt_heatmap_pval_bwmain.png")


################################################################################
# Figure SA-8
################################################################################

# Figure SA-8(a)
# Input: output/empapp_main_fs_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(fs_bwmain, eval_grid, label="FS",
    file_name="empapp_main_fs_heatmap_bwmain.png")

# Figure SA-8(b)
# Input: output/empapp_main_fs_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fs_bwmain, eval_grid, label="FS",
    file_name="empapp_main_fs_heatmap_pval_bwmain.png")


################################################################################
# Figure SA-9
################################################################################

# Figure SA-9(a)
# Input: output/empapp_main_fuzzy_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(fuzzy_bwmain, eval_grid, label="Fuzzy",
    file_name="empapp_main_fuzzy_heatmap_bwmain.png")

# Figure SA-9(b)
# Input: output/empapp_main_fuzzy_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(fuzzy_bwmain, eval_grid, label="Fuzzy",
    file_name="empapp_main_fuzzy_heatmap_pval_bwmain.png")


################################################################################
# Figure SA-10
################################################################################

# Figure SA-10(a)
# Input: output/empapp_covbal_itt_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(balance_bwmain, eval_grid, label="Cov. Bal.",
    file_name="empapp_covbal_itt_heatmap_bwmain.png")

# Figure SA-10(b)
# Input: output/empapp_covbal_itt_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(balance_bwmain, eval_grid, label="Cov. Bal.",
    file_name="empapp_covbal_itt_heatmap_pval_bwmain.png")


################################################################################
# Figure SA-11
################################################################################

# Figure SA-11(a)
# Input: output/empapp_main_itt0_bwmain.csv.
save_inference_plot(baseline_bwmain, ylab="Control-side baseline outcome",
    file_name="empapp_main_itt0_inference_bwmain.png")

# Figure SA-11(b)
# Input: output/empapp_main_itt0_bwmain.csv, output/empapp_eval.csv.
save_effect_heatmap(baseline_bwmain, eval_grid, label="Control baseline",
    file_name="empapp_main_itt0_heatmap_bwmain.png")


################################################################################
# Figure SA-12
################################################################################

# Figure SA-12(a)
# Input: output/empapp_main_percent_bwmain.csv.
save_percent_point_plot(percent_bwmain, value_col="itt.percent", ylab="ITT / ITT.0 (%)",
    file_name="empapp_main_percent_itt_pointest_bwmain.png")

# Figure SA-12(b)
# Input: output/empapp_main_percent_bwmain.csv.
save_percent_point_plot(percent_bwmain, value_col="fuzzy.percent", ylab="Fuzzy / ITT.0 (%)",
    file_name="empapp_main_percent_fuzzy_pointest_bwmain.png")


################################################################################
# Additional Outputs
################################################################################

# Additional output
# Input: output/empapp_main_fuzzy_bwitt.csv.
save_point_plot(fuzzy_bwitt, ylab="Fuzzy estimate",
    file_name="empapp_main_fuzzy_pointest_bwitt.png")

# Additional output
# Input: output/empapp_main_fs_bwitt.csv.
save_point_plot(fs_bwitt, ylab="First stage",
    file_name="empapp_main_fs_pointest_bwitt.png")

# Additional output
# Input: output/empapp_main_itt0_bwitt.csv.
save_point_plot(baseline_bwitt, ylab="Control-side baseline outcome",
    file_name="empapp_main_itt0_pointest_bwitt.png")

# Additional output
# Input: output/empapp_main_itt0_bwitt.csv, output/empapp_eval.csv.
save_pvalue_heatmap(baseline_bwitt, eval_grid, label="Control baseline",
    file_name="empapp_main_itt0_heatmap_pval_bwitt.png")

# Additional output
# Input: output/empapp_covbal_itt_bwitt.csv.
save_point_plot(balance_bwitt, ylab="Covariate balance ITT",
    file_name="empapp_covbal_itt_pointest_bwitt.png")

# Additional output
# Input: output/empapp_data.csv, output/empapp_eval.csv.
save_scatter_plot(data, eval_grid, file_name="empapp_scatter_bwmain.png")

# Additional output
# Input: output/empapp_main_fuzzy_bwmain.csv.
save_point_plot(fuzzy_bwmain, ylab="Fuzzy estimate",
    file_name="empapp_main_fuzzy_pointest_bwmain.png")

# Additional output
# Input: output/empapp_main_fuzzy_bwmain.csv.
save_inference_plot(fuzzy_bwmain, ylab="Fuzzy estimate",
    file_name="empapp_main_fuzzy_inference_bwmain.png")

# Additional output
# Input: output/empapp_main_itt_bwmain.csv.
save_point_plot(itt_bwmain, ylab="Intention-to-treat effect",
    file_name="empapp_main_itt_pointest_bwmain.png")

# Additional output
# Input: output/empapp_main_itt_bwmain.csv.
save_inference_plot(itt_bwmain, ylab="Intention-to-treat effect",
    file_name="empapp_main_itt_inference_bwmain.png")

# Additional output
# Input: output/empapp_main_fs_bwmain.csv.
save_point_plot(fs_bwmain, ylab="First stage",
    file_name="empapp_main_fs_pointest_bwmain.png")

# Additional output
# Input: output/empapp_main_fs_bwmain.csv.
save_inference_plot(fs_bwmain, ylab="First stage",
    file_name="empapp_main_fs_inference_bwmain.png")

# Additional output
# Input: output/empapp_main_itt0_bwmain.csv.
save_point_plot(baseline_bwmain, ylab="Control-side baseline outcome",
    file_name="empapp_main_itt0_pointest_bwmain.png")

# Additional output
# Input: output/empapp_main_itt0_bwmain.csv, output/empapp_eval.csv.
save_pvalue_heatmap(baseline_bwmain, eval_grid, label="Control baseline",
    file_name="empapp_main_itt0_heatmap_pval_bwmain.png")

# Additional output
# Input: output/empapp_covbal_itt_bwmain.csv.
save_point_plot(balance_bwmain, ylab="Covariate balance ITT",
    file_name="empapp_covbal_itt_pointest_bwmain.png")

# Additional output
# Input: output/empapp_covbal_itt_bwmain.csv.
save_inference_plot(balance_bwmain, ylab="Covariate balance ITT",
    file_name="empapp_covbal_itt_inference_bwmain.png")


################################################################################
# Check Saved Figures
################################################################################

expected = [
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
    "empapp_covbal_itt_inference_bwmain.png",
]
missing = [name for name in expected if not (FIGURES_DIR / name).exists()]
if missing:
    raise FileNotFoundError(f"Missing expected figures: {', '.join(missing)}")
too_small = [name for name in expected if (FIGURES_DIR / name).stat().st_size < 1000]
if too_small:
    raise ValueError(f"Generated figure file(s) look empty: {', '.join(too_small)}")
print("Empirical application figures complete.")
print(f"Figures written to: {FIGURES_DIR.relative_to(SCRIPT_DIR)}")
