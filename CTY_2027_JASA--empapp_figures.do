/**************************************************************************************************
* Location-Based Methods: empirical application figures from Stata CSV output
**************************************************************************************************/
* Stata plots the corresponding estimates with pointwise intervals.
* These displays omit the manuscript uniform bands, aggregate reference lines,
* and heatmaps. See the numbered R/Python sections for those displays.
clear all
set more off

* Exporting the scatterplot with all students requires additional Java memory.
set java_heapmax 4096m

capture confirm file "spp.csv"
if _rc {
    local dofile = subinstr("`c(filename)'", "\\", "/", .)
    local slash = strrpos("`dofile'", "/")
    if `slash' > 0 cd "`=substr("`dofile'", 1, `slash' - 1)'"
}

local outdir : environment RD2D_OUTPUT_DIR
if "`outdir'" == "" local outdir "output"
local figuresdir : environment RD2D_FIGURES_DIR
if "`figuresdir'" == "" local figuresdir "figures"
cap mkdir "`figuresdir'"


********************************************************************************
* Shared Drawing Programs
********************************************************************************

program define point_plot
    syntax using/, Saving(string) Yvar(name) Ytitle(string)
    preserve
    import delimited using "`using'", clear varnames(1) asdouble
    gen double idx = real(row)
    keep if idx < .
    twoway (rcap cilower ciupper idx, lcolor(gs10)) ///
           (scatter `yvar' idx, mcolor(black) msize(vsmall)), ///
           legend(off) xtitle("Boundary point") ytitle("`ytitle'") graphregion(color(white))
    graph export "`saving'", replace width(1800)
    restore
end

program define scatter_plot
    syntax using/, Saving(string) Yvar(name) Ytitle(string)
    preserve
    import delimited using "`using'", clear varnames(1) asdouble
    gen double idx = real(row)
    keep if idx < .
    twoway (scatter `yvar' idx, mcolor(black) msize(vsmall)), ///
           legend(off) xtitle("Boundary point") ytitle("`ytitle'") graphregion(color(white))
    graph export "`saving'", replace width(1800)
    restore
end


********************************************************************************
* Figure 1(a): Assignment Boundary (Stata Display)
********************************************************************************

* The treatment boundary consists of the two nonnegative coordinate axes.
import delimited using "`outdir'/empapp_data.csv", clear varnames(1) asdouble
twoway (scatter x2 x1 if assignment==0, mcolor(red%20) msymbol(square) msize(tiny)) ///
       (scatter x2 x1 if assignment==1, mcolor(navy%20) msymbol(circle) msize(tiny)) ///
       (function y=0, range(0 80) lcolor(gs8)) ///
       (function y=0, range(0 55) horizontal lcolor(gs8)), ///
       legend(order(1 "Control" 2 "Treatment")) xtitle("Saber 11") ytitle("Sisben") graphregion(color(white))
graph export "`figuresdir'/empapp_scatter.png", replace width(1800)


********************************************************************************
* ITT Bandwidths
********************************************************************************

* Figure 1(b) and Figure 2(a): ITT estimates with pointwise intervals.
point_plot using "`outdir'/empapp_main_itt_bwitt.csv", ///
    saving("`figuresdir'/empapp_main_itt_pointest_bwitt.png") ///
    yvar(estimatep) ytitle("ITT")

* Figure 2(b): first stage estimates with pointwise intervals.
point_plot using "`outdir'/empapp_main_fs_bwitt.csv", ///
    saving("`figuresdir'/empapp_main_fs_pointest_bwitt.png") ///
    yvar(estimatep) ytitle("First stage")

* Figure 2(c): fuzzy estimates with pointwise intervals.
point_plot using "`outdir'/empapp_main_fuzzy_bwitt.csv", ///
    saving("`figuresdir'/empapp_main_fuzzy_pointest_bwitt.png") ///
    yvar(estimatep) ytitle("Fuzzy estimate")

* Figure 2(d): covariate balance with pointwise intervals.
point_plot using "`outdir'/empapp_covbal_itt_bwitt.csv", ///
    saving("`figuresdir'/empapp_covbal_itt_pointest_bwitt.png") ///
    yvar(estimatep) ytitle("Covariate balance ITT")

* Figure SA-6(a): pointwise percentages (aggregate reference lines omitted).
scatter_plot using "`outdir'/empapp_main_percent_bwitt.csv", ///
    saving("`figuresdir'/empapp_main_percent_itt_pointest_bwitt.png") ///
    yvar(ittpercent) ytitle("ITT / ITT.0 (%)")

* Figure SA-6(b): pointwise percentages (aggregate reference lines omitted).
scatter_plot using "`outdir'/empapp_main_percent_bwitt.csv", ///
    saving("`figuresdir'/empapp_main_percent_fuzzy_pointest_bwitt.png") ///
    yvar(fuzzypercent) ytitle("Fuzzy / ITT.0 (%)")


********************************************************************************
* Fuzzy Bandwidths
********************************************************************************

* Additional output: ITT estimates with pointwise intervals. Fuzzy bandwidths.
point_plot using "`outdir'/empapp_main_itt_bwmain.csv", ///
    saving("`figuresdir'/empapp_main_itt_pointest_bwmain.png") ///
    yvar(estimatep) ytitle("ITT")

* Additional output: first stage estimates with pointwise intervals. Fuzzy bandwidths.
point_plot using "`outdir'/empapp_main_fs_bwmain.csv", ///
    saving("`figuresdir'/empapp_main_fs_pointest_bwmain.png") ///
    yvar(estimatep) ytitle("First stage")

* Additional output: fuzzy estimates with pointwise intervals. Fuzzy bandwidths.
point_plot using "`outdir'/empapp_main_fuzzy_bwmain.csv", ///
    saving("`figuresdir'/empapp_main_fuzzy_pointest_bwmain.png") ///
    yvar(estimatep) ytitle("Fuzzy estimate")

* Additional output: covariate balance with pointwise intervals. Fuzzy bandwidths.
point_plot using "`outdir'/empapp_covbal_itt_bwmain.csv", ///
    saving("`figuresdir'/empapp_covbal_itt_pointest_bwmain.png") ///
    yvar(estimatep) ytitle("Covariate balance ITT")

* Figure SA-12(a): pointwise percentages (aggregate reference lines omitted).
scatter_plot using "`outdir'/empapp_main_percent_bwmain.csv", ///
    saving("`figuresdir'/empapp_main_percent_itt_pointest_bwmain.png") ///
    yvar(ittpercent) ytitle("ITT / ITT.0 (%)")

* Figure SA-12(b): pointwise percentages (aggregate reference lines omitted).
scatter_plot using "`outdir'/empapp_main_percent_bwmain.csv", ///
    saving("`figuresdir'/empapp_main_percent_fuzzy_pointest_bwmain.png") ///
    yvar(fuzzypercent) ytitle("Fuzzy / ITT.0 (%)")

di as text "Stata empirical application figures written to: `figuresdir'"
