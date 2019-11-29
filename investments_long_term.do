#delimit;
clear;
cap log close;
cap clear matrix;
cap clear mata;
set more off;
set mem 400m;
set matsize 500;
version 9.0;
*sysdir set PLUS "c:/Program Files/Stata8/ado/plus";

*===========================================================================
Description & Notes: 
	- Long Term Impact of Oportunidades on Agricultural Assets (T/C variation)
	- micro-enterprise data in 2003 is not comparable 
	- work with value of animals rather than with nb of animals 
	- animals (and hence all animals dummies, values, etc.) and land ha have been trimmed at the top 1% of the distribution per wave
*==========================================================================================================================;

/*
gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/";


Claudia's directories*/
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta";
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct";
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output";
log using "$code/investments_long_term.log", replace;

use "$data/investments_data.dta";

*==============================
 Estimation Sample: keep hh with non-missing info on any of the dependent vars (draft animals, prod animals, land & micro-enterprise)
*==============================;
keep if wave ==2 | wave ==3 | wave ==4 | wave ==7;
replace land =. if ha ==.;
g byte all =(ani1 ~=. & ani2 ~=. & land ~=. & me~=.);
sort state muni local folio wave;
replace all =all[_n-1] if wave ==7;
keep if all ==1;

*===============================
 Wave Dummies & Treatment*Wave Interactions
*===============================;
foreach x in 2 3 4 7 {;
	g byte wave`x' =(wave ==`x');
};

foreach x in t2_c1_op treatcom {;
	foreach y in wave2 wave3 wave4 wave7 {;
		g `x'`y' =`y' * `x';
	};
};

*drop production animal outliers in wave 7;
ta wave if vani2 >=50000 & vani2 ~=.;
sum vani2 if vani2 >=50000;
keep if vani2 <50000;

*==============================
  Define Globals 
*==============================;
gl dep_all ani1 ani2 land vani1 vani2 ha;
gl dep_cont vani1 vani2 ha;
gl dep_dum ani1 ani2 land;

gl treatcom1 t2_c1_op;
gl treatcom2 treatcom;

gl basic		nbani197 nbani297 ha97;
gl controls		no497 big497
				age_hh age_hh2 female_hh educ1_hh ethnicity_hh 
				age_sp age_sp2 educ1_sp 			
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p hhsize97
				homeown97 dirtfloor97  electricity97 
				org_faenas min_dist lnup_cwagepm up2_mcwagepm
				dummy_age_hh dummy_educ_hh dummy_ethnicity_hh dummy_age_sp dummy_educ_sp
				dummy_dage0_7_97 dummy_dirtfloor97 dummy_electricity97;

*===========================================================================
Regressions: Agricultural Assets (Table 2, Panels B & C)
			 Waves 2, 3, 4 (pooled) & wave 7 & ITT & controls (Unconditional and Conditional)
			 Test that the effect is sustained through wave 7
*===========================================================================;
***Agricultural Assets: Eligibles, Unconditional -- Table 2, Panels B1 & C1;
foreach x of global dep_all {;
foreach y of global treatcom1 {;
	reg `x' `y' `y'wave7 wave3 wave4 wave7 $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	test `y' + `y'wave7 =0;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_w2347_`y', title (`x');

	reg `x' `y' $basic $controls if wave ==7, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_w7_`y', title (`x');
};
};

***Output for Table 2 -- Panel B1 and C1;
estout ani1_w2347_t2_c1_op ani2_w2347_t2_c1_op land_w2347_t2_c1_op vani1_w2347_t2_c1_op vani2_w2347_t2_c1_op ha_w2347_t2_c1_op  
	 ani1_w7_t2_c1_op ani2_w7_t2_c1_op land_w7_t2_c1_op vani1_w7_t2_c1_op vani2_w7_t2_c1_op ha_w7_t2_c1_op 
	using "$out_p/wave7.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op t2_c1_opwave7) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles Unconditional) replace;
estimates clear;

***Agricultural Assets: Eligibles, Conditional -- Table 2, Panels B2 & C2;
foreach x of global dep_dum {;
foreach y of global treatcom1 {;
	reg `x' `y' `y'wave7 wave3 wave4 wave7 $basic $controls if `x'97 ==0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	test `y' + `y'wave7 =0;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_w2347_`y', title (`x');

	reg `x' `y' $basic $controls if wave ==7 & `x'97 ==0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_w7_`y', title (`x');
};
};

***Eligibles - Conditional - continuous;
foreach x of global dep_cont {;
foreach y of global treatcom1 {;
	reg `x' `y' `y'wave7 wave3 wave4 wave7 $basic $controls if `x'97 >0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	test `y' + `y'wave7 =0;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_w2347_`y', title (`x');

	reg `x' `y' $basic $controls if wave ==7 & `x'97 >0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_w7_`y', title (`x');
};
};

***Output for Table 2 -- Panel B2 and C2;
estout ani1_w2347_t2_c1_op ani2_w2347_t2_c1_op land_w2347_t2_c1_op vani1_w2347_t2_c1_op vani2_w2347_t2_c1_op ha_w2347_t2_c1_op  
	   ani1_w7_t2_c1_op ani2_w7_t2_c1_op land_w7_t2_c1_op vani1_w7_t2_c1_op vani2_w7_t2_c1_op ha_w7_t2_c1_op 
	using "$out_p/wave7.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op t2_c1_opwave7) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles Conditional) append;

estimates clear;
log close;

