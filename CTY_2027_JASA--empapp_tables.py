################################################################################
# Estimation and Inference in Boundary Discontinuity Designs:
# Location-Based Methods
# Empirical Application: Tables
################################################################################
# Read the CSV results from CTY_2027_JASA--empapp.py. Shared formatting
# functions precede the explicit manuscript sections; additional outputs come last.

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
OUTPUT_DIR = SCRIPT_DIR / "output"
TABLES_DIR = SCRIPT_DIR / "tables"
SMALL_ROWS = {"1", "5", "10", "15", "20", "25", "30", "35", "40"}
AGGREGATE_LABELS = {"WBATE", "LBATE"}

# Table 1 reports these selected points and the WBATE/LBATE rows.

TABLES_DIR.mkdir(parents=True, exist_ok=True)
for old in TABLES_DIR.glob("empapp_*.tex"):
    old.unlink()


################################################################################
# Shared Input and Formatting Functions
################################################################################

def read_result_table(file_name: str, kind: str = "effect") -> pd.DataFrame:
    path = OUTPUT_DIR / file_name
    if not path.exists():
        raise FileNotFoundError(f"Missing output file: {path}. Run CTY_2027_JASA--empapp.py first.")
    tab = pd.read_csv(path)
    required = ["row", "h01", "h02", "h11", "h12", "N.Co", "N.Tr"]
    if kind == "effect":
        required += ["estimate.p", "p.value", "ci.lower", "ci.upper"]
    elif kind == "percent":
        required += ["itt.percent", "fuzzy.percent"]
    else:
        raise ValueError(f"Unknown table kind: {kind}")
    missing = [name for name in required if name not in tab.columns]
    if missing:
        raise ValueError(f"{path} is missing columns: {', '.join(missing)}")
    tab["row"] = tab["row"].astype(str)
    return tab


def is_aggregate(row_label: str) -> bool:
    return row_label in AGGREGATE_LABELS


def scalar(value: object) -> float:
    try:
        out = float(value)
    except (TypeError, ValueError):
        return np.nan
    return out


def fmt_num(value: object, digits: int = 3) -> str:
    x = scalar(value)
    if not np.isfinite(x):
        return ""
    return f"{x:.{digits}f}"


def fmt_pvalue(value: object) -> str:
    x = scalar(value)
    if not np.isfinite(x):
        return ""
    if x < 0.001:
        return "0.000"
    return f"{x:.3f}"


def fmt_interval(lower: object, upper: object) -> str:
    lo = scalar(lower)
    hi = scalar(upper)
    if not np.isfinite(lo) or not np.isfinite(hi):
        return ""
    return f"({fmt_num(lo)}, {fmt_num(hi)})"


def fmt_row_label(row_id: str) -> str:
    if row_id == "WBATE":
        return "$\\mathtt{WBATE}$"
    if row_id == "LBATE":
        return "$\\mathtt{LBATE}$"
    return f"$\\bb_{{{row_id}}}$"


def bandwidth_values(row: pd.Series) -> tuple[float, float]:
    if is_aggregate(str(row["row"])):
        return np.nan, np.nan
    h0 = np.array([scalar(row["h01"]), scalar(row["h02"])])
    h1 = np.array([scalar(row["h11"]), scalar(row["h12"])])
    if np.all(np.isfinite(h0)) and np.all(np.isfinite(h1)) and np.allclose(h0, h1):
        return float(h0[0]), float(h0[1])
    if np.all(np.isfinite(h0)):
        return float(h0[0]), float(h0[1])
    if np.all(np.isfinite(h1)):
        return float(h1[0]), float(h1[1])
    return np.nan, np.nan


def effective_sample_values(row: pd.Series) -> tuple[float, float]:
    if is_aggregate(str(row["row"])):
        return np.nan, np.nan
    return scalar(row["N.Co"]), scalar(row["N.Tr"])


def interval_limits(tab: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    if {"cb.lower", "cb.upper"}.issubset(tab.columns):
        use_cb = np.isfinite(pd.to_numeric(tab["cb.lower"], errors="coerce")) & np.isfinite(
            pd.to_numeric(tab["cb.upper"], errors="coerce")
        )
        lower = tab["ci.lower"].copy()
        upper = tab["ci.upper"].copy()
        lower.loc[use_cb] = tab.loc[use_cb, "cb.lower"]
        upper.loc[use_cb] = tab.loc[use_cb, "cb.upper"]
        return lower, upper
    return tab["ci.lower"], tab["ci.upper"]


def order_rows(tab: pd.DataFrame, keep_rows: set[str] | None = None) -> pd.DataFrame:
    aggregates = tab.loc[tab["row"].isin(AGGREGATE_LABELS)].copy()
    points = tab.loc[~tab["row"].isin(AGGREGATE_LABELS)].copy()
    if keep_rows is not None:
        points = points.loc[points["row"].isin(keep_rows)].copy()
    return pd.concat([points, aggregates], ignore_index=True)


def display_rows(tab: pd.DataFrame, keep_rows: set[str] | None = None) -> pd.DataFrame:
    out = order_rows(tab, keep_rows=keep_rows)
    lower, upper = interval_limits(out)
    rows = []
    for i, row in out.iterrows():
        h1, h2 = bandwidth_values(row)
        nco, ntr = effective_sample_values(row)
        rows.append(
            {
                "label": fmt_row_label(str(row["row"])),
                "h1": h1,
                "h2": h2,
                "N.Co": nco,
                "N.Tr": ntr,
                "estimate": scalar(row["estimate.p"]),
                "pvalue": scalar(row["p.value"]),
                "lower": scalar(lower.iloc[i]),
                "upper": scalar(upper.iloc[i]),
            }
        )
    return pd.DataFrame(rows)


def display_percent_rows(tab: pd.DataFrame) -> pd.DataFrame:
    out = order_rows(tab)
    rows = []
    for _, row in out.iterrows():
        h1, h2 = bandwidth_values(row)
        nco, ntr = effective_sample_values(row)
        rows.append(
            {
                "label": {"WBATE": "Average", "LBATE": "Maximum"}.get(str(row["row"]), fmt_row_label(str(row["row"]))),
                "h1": h1,
                "h2": h2,
                "N.Co": nco,
                "N.Tr": ntr,
                "itt": scalar(row["itt.percent"]),
                "fuzzy": scalar(row["fuzzy.percent"]),
            }
        )
    return pd.DataFrame(rows)


def latex_body(rows: pd.DataFrame) -> list[str]:
    lines: list[str] = []
    for _, row in rows.iterrows():
        if "WBATE" in row["label"] or "LBATE" in row["label"]:
            lines.append("  \\midrule")
        lines.append(
            "   "
            + row["label"]
            + " & "
            + fmt_num(row["h1"], 1)
            + " & "
            + fmt_num(row["h2"], 1)
            + " & "
            + fmt_num(row["N.Co"], 0)
            + " & "
            + fmt_num(row["N.Tr"], 0)
            + " & "
            + fmt_num(row["estimate"])
            + " & "
            + fmt_pvalue(row["pvalue"])
            + " & "
            + fmt_interval(row["lower"], row["upper"])
            + "\\\\"
        )
    return lines


def latex_percent_body(rows: pd.DataFrame) -> list[str]:
    lines: list[str] = []
    for _, row in rows.iterrows():
        if row["label"] in {"Average", "Maximum"}:
            lines.append("  \\midrule")
        lines.append(
            "   "
            + row["label"]
            + " & "
            + fmt_num(row["h1"], 1)
            + " & "
            + fmt_num(row["h2"], 1)
            + " & "
            + fmt_num(row["N.Co"], 0)
            + " & "
            + fmt_num(row["N.Tr"], 0)
            + " & "
            + fmt_num(row["itt"])
            + " & "
            + fmt_num(row["fuzzy"])
            + "\\\\"
        )
    return lines


def write_lines(path: Path, lines: list[str]) -> None:
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_tabular(rows: pd.DataFrame, path: Path) -> None:
    write_lines(
        path,
        [
            "\\begin{tabular}{lccccccc}",
            "  \\toprule\\toprule",
            "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & Estimate & $p$-value & 95\\% CI \\\\",
            "  \\midrule",
            *latex_body(rows),
            "  \\bottomrule\\bottomrule",
            "\\end{tabular}",
        ],
    )


def write_percent_tabular(rows: pd.DataFrame, path: Path) -> None:
    write_lines(
        path,
        [
            "\\begin{tabular}{lcccccc}",
            "  \\toprule\\toprule",
            "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & ITT / ITT.0 (\\%) & Fuzzy / ITT.0 (\\%) \\\\",
            "  \\midrule",
            *latex_percent_body(rows),
            "  \\bottomrule\\bottomrule",
            "\\end{tabular}",
        ],
    )


################################################################################
# Read Saved Results
################################################################################

# ITT bandwidth selection.
fuzzy_bwitt = read_result_table("empapp_main_fuzzy_bwitt.csv")
itt_bwitt = read_result_table("empapp_main_itt_bwitt.csv")
fs_bwitt = read_result_table("empapp_main_fs_bwitt.csv")
baseline_bwitt = read_result_table("empapp_main_itt0_bwitt.csv")
balance_bwitt = read_result_table("empapp_covbal_itt_bwitt.csv")
percent_bwitt = read_result_table("empapp_main_percent_bwitt.csv", kind="percent")

# Fuzzy bandwidth selection.
fuzzy_bwmain = read_result_table("empapp_main_fuzzy_bwmain.csv")
itt_bwmain = read_result_table("empapp_main_itt_bwmain.csv")
fs_bwmain = read_result_table("empapp_main_fs_bwmain.csv")
baseline_bwmain = read_result_table("empapp_main_itt0_bwmain.csv")
balance_bwmain = read_result_table("empapp_covbal_itt_bwmain.csv")
percent_bwmain = read_result_table("empapp_main_percent_bwmain.csv", kind="percent")


################################################################################
# Table 1
################################################################################

# Table 1(a)
# Input: output/empapp_main_itt_bwitt.csv.
write_tabular(display_rows(itt_bwitt, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_itt_small_bwitt.tex")

# Table 1(b)
# Input: output/empapp_main_fs_bwitt.csv.
write_tabular(display_rows(fs_bwitt, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_fs_small_bwitt.tex")

# Table 1(c)
# Input: output/empapp_main_fuzzy_bwitt.csv.
write_tabular(display_rows(fuzzy_bwitt, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_fuzzy_small_bwitt.tex")

# Table 1(d)
# Input: output/empapp_covbal_itt_bwitt.csv.
write_tabular(display_rows(balance_bwitt, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_covbal_itt_small_bwitt.tex")


################################################################################
# Table SA-1
################################################################################

# Table SA-1
# Input: output/empapp_main_itt_bwitt.csv.
write_tabular(display_rows(itt_bwitt),
    TABLES_DIR / "empapp_main_itt_bwitt.tex")


################################################################################
# Table SA-2
################################################################################

# Table SA-2
# Input: output/empapp_main_fs_bwitt.csv.
write_tabular(display_rows(fs_bwitt),
    TABLES_DIR / "empapp_main_fs_bwitt.tex")


################################################################################
# Table SA-3
################################################################################

# Table SA-3
# Input: output/empapp_main_fuzzy_bwitt.csv.
write_tabular(display_rows(fuzzy_bwitt),
    TABLES_DIR / "empapp_main_fuzzy_bwitt.tex")


################################################################################
# Table SA-4
################################################################################

# Table SA-4
# Input: output/empapp_covbal_itt_bwitt.csv.
write_tabular(display_rows(balance_bwitt),
    TABLES_DIR / "empapp_covbal_itt_bwitt.tex")


################################################################################
# Table SA-5
################################################################################

# Table SA-5
# Input: output/empapp_main_itt0_bwitt.csv.
write_tabular(display_rows(baseline_bwitt),
    TABLES_DIR / "empapp_main_itt0_bwitt.tex")


################################################################################
# Table SA-6
################################################################################

# Table SA-6
# Input: output/empapp_main_percent_bwitt.csv.
write_percent_tabular(display_percent_rows(percent_bwitt),
    TABLES_DIR / "empapp_main_percent_bwitt.tex")


################################################################################
# Table SA-7
################################################################################

# Table SA-7
# Input: output/empapp_main_itt_bwmain.csv.
write_tabular(display_rows(itt_bwmain),
    TABLES_DIR / "empapp_main_itt_bwmain.tex")


################################################################################
# Table SA-8
################################################################################

# Table SA-8
# Input: output/empapp_main_fs_bwmain.csv.
write_tabular(display_rows(fs_bwmain),
    TABLES_DIR / "empapp_main_fs_bwmain.tex")


################################################################################
# Table SA-9
################################################################################

# Table SA-9
# Input: output/empapp_main_fuzzy_bwmain.csv.
write_tabular(display_rows(fuzzy_bwmain),
    TABLES_DIR / "empapp_main_fuzzy_bwmain.tex")


################################################################################
# Table SA-10
################################################################################

# Table SA-10
# Input: output/empapp_covbal_itt_bwmain.csv.
write_tabular(display_rows(balance_bwmain),
    TABLES_DIR / "empapp_covbal_itt_bwmain.tex")


################################################################################
# Table SA-11
################################################################################

# Table SA-11
# Input: output/empapp_main_itt0_bwmain.csv.
write_tabular(display_rows(baseline_bwmain),
    TABLES_DIR / "empapp_main_itt0_bwmain.tex")


################################################################################
# Table SA-12
################################################################################

# Table SA-12
# Input: output/empapp_main_percent_bwmain.csv.
write_percent_tabular(display_percent_rows(percent_bwmain),
    TABLES_DIR / "empapp_main_percent_bwmain.tex")


################################################################################
# Additional Outputs
################################################################################

# Additional output
# Input: output/empapp_main_itt0_bwitt.csv.
write_tabular(display_rows(baseline_bwitt, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_itt0_small_bwitt.tex")

# Additional output
# Input: output/empapp_main_fuzzy_bwmain.csv.
write_tabular(display_rows(fuzzy_bwmain, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_fuzzy_small_bwmain.tex")

# Additional output
# Input: output/empapp_main_itt_bwmain.csv.
write_tabular(display_rows(itt_bwmain, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_itt_small_bwmain.tex")

# Additional output
# Input: output/empapp_main_fs_bwmain.csv.
write_tabular(display_rows(fs_bwmain, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_fs_small_bwmain.tex")

# Additional output
# Input: output/empapp_main_itt0_bwmain.csv.
write_tabular(display_rows(baseline_bwmain, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_main_itt0_small_bwmain.tex")

# Additional output
# Input: output/empapp_covbal_itt_bwmain.csv.
write_tabular(display_rows(balance_bwmain, keep_rows=SMALL_ROWS),
    TABLES_DIR / "empapp_covbal_itt_small_bwmain.tex")


################################################################################
# Check Saved Tables
################################################################################

expected = [
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
    "empapp_covbal_itt_small_bwmain.tex",
]
missing = [name for name in expected if not (TABLES_DIR / name).exists()]
if missing:
    raise FileNotFoundError(f"Missing expected tables: {', '.join(missing)}")
main_text = "\n".join((TABLES_DIR / f"empapp_main_fuzzy_{suffix}.tex").read_text(encoding="utf-8") for suffix in ("bwmain", "bwitt"))
forbidden = ["Z value", "t-statistic", "P>|t|", "P>|z|", "CI/CB", "$<$0.001"]
if any(label in main_text for label in forbidden):
    raise ValueError("Generated empirical table contains an obsolete label.")
if "WBATE" not in main_text or "LBATE" not in main_text:
    raise ValueError("Generated empirical table is missing WBATE or LBATE.")
if "$N_{\\mathrm{Co}}$" not in main_text or "$N_{\\mathrm{Tr}}$" not in main_text:
    raise ValueError("Generated empirical table is missing N.Co or N.Tr headers.")
print("Empirical application LaTeX tables complete.")
print(f"Tables written to: {TABLES_DIR.relative_to(SCRIPT_DIR)}")
