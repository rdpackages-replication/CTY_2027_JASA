/**************************************************************************************************
* Estimation and Inference in Boundary Discontinuity Designs: Location-Based Methods
* Empirical Application: SPP Data
* Stata replication output
**************************************************************************************************/

clear all
set more off
set seed 20260501

capture confirm file "spp.csv"
if _rc {
    local dofile = subinstr("`c(filename)'", "\\", "/", .)
    local slash = strrpos("`dofile'", "/")
    if `slash' > 0 cd "`=substr("`dofile'", 1, `slash' - 1)'"
}

local rd2d_stata_path : environment RD2D_STATA_PATH
if "`rd2d_stata_path'" != "" adopath ++ "`rd2d_stata_path'"
capture which rd2d
if _rc {
    di as error "Stata rd2d package not found. Install rd2d or set RD2D_STATA_PATH to the rd2d/stata folder."
    exit 111
}


* Record the version declared by the Stata package used for this run.
local rd2d_version "unknown"
quietly findfile rd2d.ado
local rd2d_ado "`r(fn)'"
tempname ado_header
file open `ado_header' using "`rd2d_ado'", read text
file read `ado_header' ado_line
while r(eof) == 0 {
    if regexm("`ado_line'", "^[*]!version[ ]+([^ ]+)") {
        local rd2d_version = regexs(1)
        continue, break
    }
    file read `ado_header' ado_line
}
file close `ado_header'

local outdir : environment RD2D_OUTPUT_DIR
if "`outdir'" == "" local outdir "output"
cap mkdir "`outdir'"
local oldfiles : dir "`outdir'" files "empapp_*.csv"
foreach f of local oldfiles {
    erase "`outdir'/`f'"
}

* Optional comparison: use saved bandwidths from another language.
local refdir : environment RD2D_REFERENCE_OUTPUT
* This Stata interface reports pointwise inference. It does not construct
* uniform bands or LBATE intervals; repp is retained as a package option.
local repp : environment RD2D_EMP_REPP
if "`repp'" == "" local repp 2000


********************************************************************************
* CSV Formatting Functions
********************************************************************************

mata:
string scalar rd2d_emp_fmt(real scalar x)
{
    if (x >= .) return("")
    return(strofreal(x, "%21.15g"))
}

real scalar rd2d_emp_mean(real colvector x)
{
    x = select(x, x :< .)
    if (rows(x) == 0) return(.)
    return(mean(x))
}

real scalar rd2d_emp_max(real colvector x)
{
    x = select(x, x :< .)
    if (rows(x) == 0) return(.)
    return(max(x))
}

string scalar rd2d_emp_row(string scalar label, real rowvector values)
{
    real scalar j
    string scalar line
    line = label
    for (j=1; j<=cols(values); j++) line = line + "," + rd2d_emp_fmt(values[j])
    return(line)
}

void rd2d_emp_add_wbate(real matrix V, real scalar level, real rowvector values)
{
    real scalar n, se, se2, center, tval, cval
    real rowvector w
    n = rows(V)
    if (n > 0 & rows(V) == cols(V)) {
        w = J(1, n, 1/n)
        se2 = (w * V * w')[1,1]
        if (se2 < 0) se2 = 0
        se = sqrt(se2)
        center = values[5]
        if (se > 0 & center < .) {
            tval = center / se
            cval = invnormal((level + 100) / 200)
            values[6] = se
            values[7] = tval
            values[8] = 2 * (1 - normal(abs(tval)))
            values[9] = center - cval * se
            values[10] = center + cval * se
        }
    }
}

void rd2d_emp_write_frame_csv(string scalar file, string scalar vars, string scalar header)
{
    real matrix X
    real scalar fh, i, j
    string scalar line
    X = st_data(., tokens(vars))
    fh = fopen(file, "w")
    fput(fh, header)
    for (i=1; i<=rows(X); i++) {
        line = rd2d_emp_fmt(X[i,1])
        for (j=2; j<=cols(X); j++) line = line + "," + rd2d_emp_fmt(X[i,j])
        fput(fh, line)
    }
    fclose(fh)
}

void rd2d_emp_write_metadata(string scalar file, string scalar repp, string scalar package_version)
{
    real scalar fh
    fh = fopen(file, "w")
    fput(fh, "name,value")
    fput(fh, "rd2d.stata," + package_version)
    fput(fh, "repp," + repp)
    fput(fh, "bwparams,main=bwmain; itt=bwitt")
    fclose(fh)
}

void rd2d_emp_write_location_csv(string scalar file, string scalar mname, string scalar vname)
{
    real matrix M, V
    real scalar fh, i
    real rowvector wb, lb
    M = st_matrix(mname)
    V = (vname == "" ? J(0,0,.) : st_matrix(vname))
    fh = fopen(file, "w")
    fput(fh, "row,b1,b2,estimate.p,std.err.p,estimate.q,std.err.q,t.value,p.value,ci.lower,ci.upper,h01,h02,h11,h12,N.Co,N.Tr,cb.lower,cb.upper")
    for (i=1; i<=rows(M); i++) fput(fh, rd2d_emp_row(strofreal(i), (M[i,.], ., .)))
    wb = J(1, 18, .)
    wb[3] = rd2d_emp_mean(M[,3])
    wb[5] = rd2d_emp_mean(M[,5])
    rd2d_emp_add_wbate(V, 95, wb)
    fput(fh, rd2d_emp_row("WBATE", wb))
    lb = J(1, 18, .)
    lb[3] = rd2d_emp_max(M[,3])
    lb[5] = rd2d_emp_max(M[,5])
    fput(fh, rd2d_emp_row("LBATE", lb))
    fclose(fh)
}

void rd2d_emp_write_percent_csv(string scalar file, string scalar main_name, string scalar itt_name, string scalar itt0_name)
{
    real matrix main, itt, itt0
    real scalar fh, i
    real rowvector vals
    main = st_matrix(main_name)
    itt = st_matrix(itt_name)
    itt0 = st_matrix(itt0_name)
    // Percentage summaries use all pointwise ratios, with no omitted locations.
    if (rows(main) == 0 | any(itt0[,3] :== 0) | any(itt0[,3] :>= .) |
        any(itt[,3] :>= .) | any(main[,3] :>= .)) {
        errprintf("Percentage summaries require finite estimates and nonzero baselines.\n")
        _error(3498)
    }
    fh = fopen(file, "w")
    fput(fh, "row,b1,b2,h01,h02,h11,h12,N.Co,N.Tr,itt.percent,fuzzy.percent")
    for (i=1; i<=rows(main); i++) {
        vals = (main[i,1], main[i,2], main[i,11], main[i,12], main[i,13], main[i,14], main[i,15], main[i,16], 100*itt[i,3]/itt0[i,3], 100*main[i,3]/itt0[i,3])
        fput(fh, rd2d_emp_row(strofreal(i), vals))
    }
    vals = J(1,10,.)
    vals[9] = rd2d_emp_mean(100 :* itt[,3] :/ itt0[,3])
    vals[10] = rd2d_emp_mean(100 :* main[,3] :/ itt0[,3])
    fput(fh, rd2d_emp_row("WBATE", vals))
    vals = J(1,10,.)
    vals[9] = rd2d_emp_max(100 :* itt[,3] :/ itt0[,3])
    vals[10] = rd2d_emp_max(100 :* main[,3] :/ itt0[,3])
    fput(fh, rd2d_emp_row("LBATE", vals))
    fclose(fh)
}
end

program define rd2d_emp_numlist_from_csv, rclass
    syntax using/, Columns(string)
    capture confirm file "`using'"
    if _rc {
        return local list ""
        exit
    }
    preserve
    import delimited using "`using'", clear varnames(1) asdouble
    capture confirm variable row
    if !_rc {
        gen double __rownum = real(row)
        keep if __rownum < .
    }
    local values ""
    forvalues i = 1/`=_N' {
        foreach col of local columns {
            local values `"`values' `=string(`col'[`i'], "%21.15g")'"'
        }
    }
    restore
    return local list `"`values'"'
end


********************************************************************************
* SPP Data
********************************************************************************

* Enrollment and covariate balance use separate samples below.
* Preserve the scores as doubles; float rounding changes selected bandwidths.
import delimited using "spp.csv", clear varnames(1) asdouble
keep running_saber11 running_sisben eligible_spp beneficiary_spp spadies_any icfes_educm1
rename running_saber11 x1
rename running_sisben x2
rename eligible_spp assignment
rename beneficiary_spp fuzzy
rename spadies_any Y
rename icfes_educm1 Z
gen byte __expected_assignment = x1 >= 0 & x2 >= 0 if !missing(x1, x2)
assert assignment == __expected_assignment if !missing(assignment, x1, x2)
drop __expected_assignment
frame rename default spp


********************************************************************************
* Evaluation Points on the Boundary
********************************************************************************

* Points 1--20 follow the academic threshold; points 21--40 follow the
* wealth threshold. Point 21 is the corner.
frame create eval
frame eval {
    set obs 40
    gen double x1 = cond(_n <= 20, 0, (_n - 21) * 56 / 20)
    gen double x2 = cond(_n <= 20, 40 - (_n - 1) * 40 / 20, 0)
    local blist ""
    forvalues i = 1/`=_N' {
        local blist `"`blist' `=string(x1[`i'], "%21.15g")' `=string(x2[`i'], "%21.15g")'"'
    }
    mata: rd2d_emp_write_frame_csv("`outdir'/empapp_eval.csv", "x1 x2", "x.1,x.2")
}

frame spp {
    mata: rd2d_emp_write_frame_csv("`outdir'/empapp_data.csv", "x1 x2 assignment fuzzy Y Z", "x.1,x.2,assignment,fuzzy,Y,Z")
}
mata: rd2d_emp_write_metadata("`outdir'/empapp_metadata.csv", "`repp'", "`rd2d_version'")


********************************************************************************
* Fuzzy Bandwidth Selection (Section SA-10.2)
********************************************************************************

frame change spp
* College enrollment: retain its complete-case sample.
preserve
gen double Y_now = Y
drop if missing(x1, x2, assignment, fuzzy, Y_now)
local hopt ""
if "`refdir'" != "" {
    rd2d_emp_numlist_from_csv using "`refdir'/empapp_main_fuzzy_bwmain.csv", columns(h01 h02 h11 h12)
    if "`r(list)'" != "" local hopt "h(`r(list)')"
}
quietly rd2d Y_now x1 x2 assignment, b(`blist') fuzzy(fuzzy) ///
    bwparam(main) paramsother(itt.0) paramscov(main itt fs itt.0) repp(`repp') `hopt'
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_fuzzy_bwmain.csv", "e(main)", "e(V_main)")
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_itt_bwmain.csv", "e(itt)", "e(V_itt)")
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_fs_bwmain.csv", "e(fs)", "e(V_fs)")
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_itt0_bwmain.csv", "e(itt_0)", "e(V_itt_0)")
* Compute pointwise percentages before taking their average and maximum.
mata: rd2d_emp_write_percent_csv("`outdir'/empapp_main_percent_bwmain.csv", "e(main)", "e(itt)", "e(itt_0)")
restore

* Covariate balance: use mother's education and its own complete-case sample.
preserve
gen double Y_now = Z
drop if missing(x1, x2, assignment, fuzzy, Y_now)
local hopt ""
if "`refdir'" != "" {
    rd2d_emp_numlist_from_csv using "`refdir'/empapp_covbal_itt_bwmain.csv", columns(h01 h02 h11 h12)
    if "`r(list)'" != "" local hopt "h(`r(list)')"
}
quietly rd2d Y_now x1 x2 assignment, b(`blist') fuzzy(fuzzy) ///
    bwparam(main) paramscov(itt) repp(`repp') `hopt'
mata: rd2d_emp_write_location_csv("`outdir'/empapp_covbal_itt_bwmain.csv", "e(itt)", "e(V_itt)")
restore


********************************************************************************
* ITT Bandwidth Selection (Paper and Section SA-10.1)
********************************************************************************

frame change spp
* College enrollment: retain its complete-case sample.
preserve
gen double Y_now = Y
drop if missing(x1, x2, assignment, fuzzy, Y_now)
local hopt ""
if "`refdir'" != "" {
    rd2d_emp_numlist_from_csv using "`refdir'/empapp_main_fuzzy_bwitt.csv", columns(h01 h02 h11 h12)
    if "`r(list)'" != "" local hopt "h(`r(list)')"
}
quietly rd2d Y_now x1 x2 assignment, b(`blist') fuzzy(fuzzy) ///
    bwparam(itt) paramsother(itt.0) paramscov(main itt fs itt.0) repp(`repp') `hopt'
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_fuzzy_bwitt.csv", "e(main)", "e(V_main)")
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_itt_bwitt.csv", "e(itt)", "e(V_itt)")
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_fs_bwitt.csv", "e(fs)", "e(V_fs)")
mata: rd2d_emp_write_location_csv("`outdir'/empapp_main_itt0_bwitt.csv", "e(itt_0)", "e(V_itt_0)")
* Compute pointwise percentages before taking their average and maximum.
mata: rd2d_emp_write_percent_csv("`outdir'/empapp_main_percent_bwitt.csv", "e(main)", "e(itt)", "e(itt_0)")
restore

* Covariate balance: use mother's education and its own complete-case sample.
preserve
gen double Y_now = Z
drop if missing(x1, x2, assignment, fuzzy, Y_now)
local hopt ""
if "`refdir'" != "" {
    rd2d_emp_numlist_from_csv using "`refdir'/empapp_covbal_itt_bwitt.csv", columns(h01 h02 h11 h12)
    if "`r(list)'" != "" local hopt "h(`r(list)')"
}
quietly rd2d Y_now x1 x2 assignment, b(`blist') fuzzy(fuzzy) ///
    bwparam(itt) paramscov(itt) repp(`repp') `hopt'
mata: rd2d_emp_write_location_csv("`outdir'/empapp_covbal_itt_bwitt.csv", "e(itt)", "e(V_itt)")
restore

di as text "Stata empirical application output data complete."
di as text "CSV outputs written to: `outdir'"
