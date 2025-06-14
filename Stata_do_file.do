clear
cd "D:\desk"
import excel "City_Panel.xlsx", firstrow clear



***Figure 2: Sigma convergence-pgdp
**PGDP indicates the GDP per capita 

gen lnpgdp=ln(PGDP)

gen region = ""
replace region = "east" if east == 1
replace region = "west" if west == 1
replace region = "middle" if middle == 1
replace region = "north" if north == 1


preserve
drop if region == ""
collapse (sd) sd_gdp = lnpgdp, by(year region)
tempfile regional_sd
save `regional_sd'
restore

collapse (sd) sd_gdp = lnpgdp, by(year)
gen region = "national"
tempfile national_sd
save `national_sd'


use `regional_sd', clear
append using `national_sd'

* （σ-convergence）
twoway ///
    (line sd_gdp year if region == "national", lpattern(solid) lcolor(red) lwidth(thick)) ///
    (line sd_gdp year if region == "east", lpattern(dash) lcolor(blue)) ///
    (line sd_gdp year if region == "west", lpattern(dash) lcolor(brown)) ///
    (line sd_gdp year if region == "middle", lpattern(dash) lcolor(orange)) ///
    (line sd_gdp year if region == "north", lpattern(dash) lcolor(green)), ///
    legend(order(1 "National" 2 "East" 3 "West" 4 "Middle" 5 "North_East")) ///
    ytitle("Sigma") xtitle("Year")

	
	
****Figure 3
keep city year pgdp lnpgdp east west middle north

keep if inlist(year, 1980, 1990, 2000, 2010, 2020)


reshape wide pgdp lnpgdp, i(city) j(year)

* Total growth
gen g_80_20 = lnpgdp2020 - lnpgdp1980
gen g_80_90 = lnpgdp1990 - lnpgdp1980
gen g_90_00 = lnpgdp2000 - lnpgdp1990
gen g_00_10 = lnpgdp2010 - lnpgdp2000
gen g_10_20 = lnpgdp2020 - lnpgdp2010

**Figure 3- Panel A: National_80_20
twoway  (lfit g_80_20 lnpgdp1980, ///
        lcolor(blue) lwidth(medium) lpattern(dash)) ///
		(scatter g_80_20 lnpgdp1980, ///
        msize(tiny) mcolor(%50) jitter(10)), ///
        legend(off) ///
		xtitle("Log GDP per capita in 1980") ///
        ytitle("Growth rate 1980-2020") ///
		title("Panel A - Covergence Trends 1980-2020") 

**Figure 3- Panel B: National_80_90
twoway  (lfit g_80_90 lnpgdp1980, ///
        lcolor(blue) lwidth(medium) lpattern(dash)) ///
		(scatter g_80_90 lnpgdp1980, ///
        msize(tiny) mcolor(%50) jitter(10)), ///
        legend(off) ///
	    xtitle("Log GDP per capita in 1980") ///
        ytitle("Growth rate 1980-1990") ///
		title("Panel B - Covergence Trends 1980-1990") 
 
**Figure 3- Panel C: National_90_00
twoway  (lfit g_90_00 lnpgdp1990, ///
        lcolor(blue) lwidth(medium) lpattern(dash)) ///
		(scatter g_90_00 lnpgdp1990, ///
        msize(tiny) mcolor(%50) jitter(10)), ///
        legend(off) ///
	    xtitle("Log GDP per capita in 1990") ///
        ytitle("Growth rate 1990-2000") ///
		title("Panel C - Covergence Trends 1990-2000") 
		
**Figure 3- Panel D:National_00_10
twoway  (lfit g_00_10 lnpgdp2000, ///
        lcolor(blue) lwidth(medium) lpattern(dash)) ///
		(scatter g_00_10 lnpgdp2000, ///
        msize(tiny) mcolor(%50) jitter(10)), ///
        legend(off) ///
	    xtitle("Log GDP per capita in 2000") ///
        ytitle("Growth rate 2000-2010") ///	
		title("Panel D - Covergence Trends 2000-2010") 


**Figure 3- Panel E:National_10_20
twoway  (lfit g_10_20 lnpgdp2010, ///
        lcolor(blue) lwidth(medium) lpattern(dash)) ///
		(scatter g_10_20 lnpgdp2010, ///
        msize(tiny) mcolor(%50) jitter(10)), ///
        legend(off) ///
	    xtitle("Log GDP per capita in 2010") ///
        ytitle("Growth rate 2010-2020") 	///
		title("Panel E - Covergence Trends 2010-2020")
		
	
***Beta convergence -- unconditional pgdp
preserve
gen year0 = year - 10
rename lnpgdp lnpgdp_t10
keep city year0 lnpgdp_t10
tempfile future
save `future'
restore

gen year0 = year
merge 1:1 city year0 using `future', keep(match) nogen

*gen growth10 = (lnpgdp_t10 - lnpgdp) / 10
gen growth10_pgdp = exp((lnpgdp_t10 - lnpgdp) / 10) - 1


***unconditional beta convergence
***national
tempname beta_nat
postfile `beta_nat' year str10 region beta se tstat rsq N using beta_nat.dta, replace

forvalues t = 1980/2010 {
    preserve
    keep if year == `t'
    keep if !missing(growth10_pgdp, lnpgdp)

    if _N >= 10 {
        regress growth10_pgdp lnpgdp
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_nat' (`t') ("national") (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1980 {
            outreg2 using beta_nat.xls, replace ctitle(`t') dec(4) keep(lnpgdp)
        }
        else {
            outreg2 using beta_nat.xls, append ctitle(`t') dec(4) keep(lnpgdp)
        }
    }

    restore
}

postclose `beta_nat'


****East 
tempname beta_east
postfile `beta_east' year str10 region beta se tstat rsq N using beta_east.dta, replace

forvalues t = 1980/2010 {
    preserve
    keep if year == `t' & east==1
    keep if !missing(growth10_pgdp, lnpgdp)

    if _N >= 10 {
        regress growth10_pgdp lnpgdp
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_east' (`t') ("east") (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1980 {
            outreg2 using beta_east.xls, replace ctitle(`t') dec(4) keep(lnpgdp)
        }
        else {
            outreg2 using beta_east.xls, append ctitle(`t') dec(4) keep(lnpgdp)
        }
    }

    restore
}

postclose `beta_east'


****West
tempname beta_west
postfile `beta_west' year str10 region beta se tstat rsq N using beta_west.dta, replace

forvalues t = 1980/2010 {
    preserve
    keep if year == `t' & west==1
    keep if !missing(growth10_pgdp, lnpgdp)

    if _N >= 10 {
        regress growth10_pgdp lnpgdp
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_west' (`t') ("west") (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1980 {
            outreg2 using beta_west.xls, replace ctitle(`t') dec(4) keep(lnpgdp)
        }
        else {
            outreg2 using beta_west.xls, append ctitle(`t') dec(4) keep(lnpgdp)
        }
    }

    restore
}

postclose `beta_west'


*****middle
tempname beta_middle
postfile `beta_middle' year str10 region beta se tstat rsq N using beta_middle.dta, replace

forvalues t = 1980/2010 {
    preserve
    keep if year == `t' & middle==1
    keep if !missing(growth10_pgdp, lnpgdp)

    if _N >= 10 {
        regress growth10_pgdp lnpgdp
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_middle' (`t') ("middle") (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1980 {
            outreg2 using beta_middle.xls, replace ctitle(`t') dec(4) keep(lnpgdp)
        }
        else {
            outreg2 using beta_middle.xls, append ctitle(`t') dec(4) keep(lnpgdp)
        }
    }

    restore
}

postclose `beta_middle'

*****North
tempname beta_north
postfile `beta_north' year str10 region beta se tstat rsq N using beta_north.dta, replace

forvalues t = 1980/2010 {
    preserve
    keep if year == `t' & north==1
    keep if !missing(growth10_pgdp, lnpgdp)

    if _N >= 10 {
        regress growth10_pgdp lnpgdp
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_north' (`t') ("north") (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1980 {
            outreg2 using beta_north.xls, replace ctitle(`t') dec(4) keep(lnpgdp)
        }
        else {
            outreg2 using beta_north.xls, append ctitle(`t') dec(4) keep(lnpgdp)
        }
    }

    restore
}

postclose `beta_north'

****graph
use beta_nat.dta, clear
append using beta_east.dta
append using beta_west.dta
append using beta_middle.dta
append using beta_north.dta

* confident interval
gen ub = beta + 1.96 * se
gen lb = beta - 1.96 * se

* Figure 4-Panel A
twoway ///
    (rarea ub lb year if region == "national", color(gs12)) ///
    (line beta year if region == "national", lcolor(red) lwidth(thick)) ///
    (line beta year if region == "east", lcolor(blue) lpattern(dash)) ///
    (line beta year if region == "west", lcolor(brown) lpattern(dash)) ///
    (line beta year if region == "middle", lcolor(orange) lpattern(dash)) ///
    (line beta year if region == "north", lcolor(green) lpattern(dash)), ///
    ytitle("β-convergence rate") xtitle("Start Year") ///
	title("Panel A - Unconditional Convergence 1980-2010") ///
    legend(order(2 "National" 3 "East" 4 "West" 5 "Middle" 6 "North_East"))


	
	   
**********Conditional convernge
gen lnpop=ln(pop)
gen lnfixed=ln(fixed)
gen lnstudent=ln(uni_student)
gen lnpass=ln(passenger)


********National + Regional
tempname beta_nat_c
postfile `beta_nat_c' year beta se tstat rsq N using beta_nat_c.dta, replace

forvalues t = 1990/2010 {
    preserve
    keep if year == `t'
    keep if !missing(growth10_pgdp, lnpgdp, lnpop, lnstudent)

    if _N >= 10 {
        
		reghdfe growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass, absorb(year)
	
		
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_nat_c' (`t') (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1990 {
            outreg2 using beta_nat_c.xls, replace ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
        else {
            outreg2 using beta_nat_c.xls, append ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
    }

    restore
}

postclose `beta_nat_c'


******East
tempname beta_east_c
postfile `beta_east_c' year beta se tstat rsq N using beta_east_c.dta, replace

forvalues t = 1990/2010 {
    preserve
    keep if year == `t' & east == 1
    keep if !missing(growth10_pgdp, lnpgdp, lnpop, lnstudent)

    if _N >= 10 {
        
		reghdfe growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass, absorb(year)
	
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_east_c' (`t') (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1990 {
            outreg2 using beta_east_c.xls, replace ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
        else {
            outreg2 using beta_east_c.xls, append ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
    }

    restore
}

postclose `beta_east_c'

***middle
tempname beta_middle_c
postfile `beta_middle_c' year beta se tstat rsq N using beta_middle_c.dta, replace

forvalues t = 1990/2010 {
    preserve
    keep if year == `t' & middle == 1
    keep if !missing(growth10_pgdp, lnpgdp, lnpop, lnstudent)

    if _N >= 10 {
        
		reghdfe growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass, absorb(year)
	
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_middle_c' (`t') (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1990 {
            outreg2 using beta_middle_c.xls, replace ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
        else {
            outreg2 using beta_middle_c.xls, append ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
    }

    restore
}

postclose `beta_middle_c'

********west
tempname beta_west_c
postfile `beta_west_c' year beta se tstat rsq N using beta_west_c.dta, replace

forvalues t = 1990/2010 {
    preserve
    keep if year == `t' & west == 1
    keep if !missing(growth10_pgdp, lnpgdp, lnpop, lnstudent)

    if _N >= 10 {
        
		reghdfe growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass, absorb(year)
	
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_west_c' (`t') (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1990 {
            outreg2 using beta_west_c.xls, replace ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
        else {
            outreg2 using beta_west_c.xls, append ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
    }

    restore
}

postclose `beta_west_c'

****north
tempname beta_north_c
postfile `beta_north_c' year beta se tstat rsq N using beta_north_c.dta, replace

forvalues t = 1990/2010 {
    preserve
    keep if year == `t' & north == 1
    keep if !missing(growth10_pgdp, lnpgdp, lnpop, lnstudent)

    if _N >= 10 {
        
		reghdfe growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass, absorb(year)
	
		local b    = _b[lnpgdp]
        local se   = _se[lnpgdp]
        local tval = `b' / `se'
        local rsq  = e(r2)
        local N    = e(N)
		
        post `beta_north_c' (`t') (`b') (`se') (`tval') (`rsq') (`N')
		
		if `t' == 1990 {
            outreg2 using beta_north_c.xls, replace ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
        else {
            outreg2 using beta_north_c.xls, append ctitle(`t') dec(4) keep(lnpgdp lnpop lnfixed lnstudent lnpass)
        }
    }

    restore
}

postclose `beta_north_c'

******graph
use beta_nat_c.dta, clear
gen region = "national"
append using beta_east_c.dta
replace region = "east" if missing(region)

append using beta_west_c.dta
replace region = "west" if missing(region)

append using beta_middle_c.dta
replace region = "middle" if missing(region)

append using beta_north_c.dta
replace region = "north" if missing(region)

gen ub = beta + 1.96 * se
gen lb = beta - 1.96 * se
save beta_allregions.dta, replace

use beta_allregions.dta, clear

* Figure 4- Panel B
twoway ///
    (rarea ub lb year if region == "national", color(gs12)) ///
    (line beta year if region == "national", lcolor(red) lwidth(thick)) ///
    (line beta year if region == "east", lcolor(blue) lpattern(dash)) ///
    (line beta year if region == "west", lcolor(brown) lpattern(dash)) ///
    (line beta year if region == "middle", lcolor(orange) lpattern(dash)) ///
    (line beta year if region == "north", lcolor(green) lpattern(dash)), ///
    ytitle("β-convergence rate") xtitle("Start Year") ///
	title("Panel B - Conditional Convergence 1990-2010") ///
    legend(order(2 "National" 3 "East" 4 "West" 5 "Middle" 6 "North_East"))


	
******Table 1
reg growth10_pgdp lnpgdp i.year if year>1989, r
outreg2 using beta_c.xls, keep(lnpgdp)
reg growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass i.year if year>1989, r
outreg2 using beta_c.xls, keep(lnpgdp lnpop lnfixed lnstudent lnpass)

reg growth10_pgdp lnpgdp i.year if year>1989 & east==1, r
outreg2 using beta_c.xls, keep(lnpgdp)
reg growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass i.year if year>1989 & east==1, r
outreg2 using beta_c.xls, keep(lnpgdp lnpop lnfixed lnstudent lnpass)

reg growth10_pgdp lnpgdp i.year if year>1989 & middle==1, r
outreg2 using beta_c.xls, keep(lnpgdp)
reg growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass i.year if year>1989 & middle==1, r
outreg2 using beta_c.xls, keep(lnpgdp lnpop lnfixed lnstudent lnpass)

reg growth10_pgdp lnpgdp i.year if year>1989 & west==1, r
outreg2 using beta_c.xls, keep(lnpgdp)
reg growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass i.year if year>1989 & west==1, r
outreg2 using beta_c.xls, keep(lnpgdp lnpop lnfixed lnstudent lnpass)

reg growth10_pgdp lnpgdp i.year if year>1989 & north==1, r
outreg2 using beta_c.xls, keep(lnpgdp)
reg growth10_pgdp lnpgdp lnpop lnfixed lnstudent lnpass i.year if year>1989 & north==1, r
outreg2 using beta_c.xls, keep(lnpgdp lnpop lnfixed lnstudent lnpass)