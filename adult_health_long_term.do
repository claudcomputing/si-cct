#delimit;
clear;
cap clear matrix;
cap clear mata;
cap log close;
set more off;
set mem 400m;
set matsize 800;

*sysdir set PLUS 		"c:/Program Files (x86)/Stata8/ado/plus";

*=========================*
Description: Estimate Impact of Oportunidades on Long Term Health (Table 7)
*=========================*;


/*gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/";

Claudia's directories*/
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta";
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct";
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output";


log using 	"$code/adult_health_long_term.log", replace;

use "$data/investments_data.dta";

*==============================
 Estimation Sample  -- households with information on consumption in Nov 2003 
					-- drop consumption outliers for sample consistency with estimation sample in Table 5
*==============================;
keep if wave == 7;
tab wave;
drop if consumo_pp_ae ==0 | consumo_pp_ae ==.;

foreach x in consumo_pp_ae2 {;
	xtile `x'_tl_t2 = `x' if t2_c1_op ~=., nq(200);
	xtile `x'_tl_tr = `x' if treatcom ~=. & ineligible ==1, nq(200);
};

sort state muni local folio wave;
tempfile investments;
save `investments';

*==============================
 Merge Adult Morbidity Data
==============================;
u "$data/adults_morbidity_03.dta", clear;
sort state muni local folio wave;
merge state muni local folio wave using `investments';
keep if _merge ==3;
sort state muni local folio numero wave;
dups state muni local folio numero wave;
sort state muni local folio numero wave;

*=================*
 Globals Variables
*=================*;
gl adl_d sick inactivity;
gl adl_c days_sick days_inactivity nb_km moderate_adl vigorous_adl;

foreach y in moderate vigorous {; 
	replace `y'_adl =. if `y'_adl ==0;
};
			
g age_sq =age*age;

gl basic		nbani197 nbani297 ha97;
gl controls_all	no497 big497
				age age_sq female educ1_hh ethnicity_hh educ1_sp 			
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p 
				homeown97 dirtfloor97 electricity97  
				org_faenas min_dist lnup_cwagepm up2_mcwagepm
				dummy_age_hh dummy_educ_hh dummy_ethnicity_hh dummy_age_sp dummy_educ_sp
				dummy_dage0_7_97 dummy_dirtfloor97 dummy_electricity97;

*======================================================
 Regressions Table 7 (ITT Eligibles)
========================================================;
foreach x of global adl_d {;
	reg `x' t2_c1_op $basic hhsize_97 $controls_all if age<=64 & consumo_pp_ae2_tl_t2 >1 & consumo_pp_ae2_tl_t2 <200, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x', title (`x');
};

foreach x of global adl_c {;
	reg `x' t2_c1_op $basic hhsize_97 $controls_all if age <=64 & consumo_pp_ae2_tl_t2 >1 & consumo_pp_ae2_tl_t2 <200, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x', title (`x');
};

***Output Table 7;
estout sick days_sick inactivity days_inactivity nb_km moderate_adl vigorous_adl
	using "$out_p/adl_w7.text", stats(N mean, fmt(%9.0fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var (Control Households)")) keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - Health HH) replace;

log close;
