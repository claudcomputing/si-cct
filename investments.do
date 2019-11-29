#delimit;
clear;
cap log close;
cap clear matrix;
cap clear mata;
set more off;
set mem 400m;
set matsize 500;
version 9.0;
*sysdir set PLUS "c:/Program Files (x86)/Stata8/ado/plus";
*sysdir set PLUS "C:\Program Files (x86)\Stata15\ado\plus";
*===========================================================================
Description & Notes: 
	- Impact of Oportunidades on Agricultural Assets and Micro-Enterprise Activities over Experimental Period: 
	  ITT, conditional vs. unconditional models
	- work with value of animals rather than with nb of animals 
	- animals (and hence all animals dummies, values, etc.) and land ha have been trimmed at the top 1% of the distribution per wave
*==========================================================================================================================;
/*
gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/";
*/

/*
Claudia's directories*/
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta";
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct";
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output";

cap log using "$code/investments.log", replace;

use "$data/investments_data.dta";

*==============================
 Estimation Sample: keep hh with non-missing info on any of the dependent vars (draft animals, prod animals, land & micro-enterprise)
*==============================;
keep if wave ==2 | wave ==3 | wave ==4;
replace land =. if ha ==.;
g byte all =(ani1 ~=. & ani2 ~=. & land ~=. & me~=.);
keep if all ==1;

*===============================
 Wave Dummies & Treatment*Wave Interactions
*===============================;
foreach x in 2 3 4 {;
	g byte wave`x' =(wave ==`x');
};

foreach x in t2_c1_op treatcom {;
	foreach y in wave2 wave3 wave4 {;
		g `x'`y' =`y' * `x';
	};
};

*==============================
  Define Globals 
*==============================;
gl dep_all 		ani1 ani2 land vani1 vani2 ha;
gl dep_dum 		ani1 ani2 land;
gl dep_cont 	vani1 vani2 ha;
gl dep_me 		me2 crafts const sewing food repair other;
gl dep_all_in	ani1 ani2 land vani1 vani2 ha me2 crafts;

gl treatcom1 	t2_c1_op;
gl treatcom2 	treatcom;

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
Regressions: Agricultural Assets & Micro-Enterprise Activity
			 Waves 2, 3, 4 pooled (add test that we can pool) & ITT & controls (Unconditional and Conditional)
*===========================================================================;
***Agricultural Assets: Eligibles, Unconditional -- Table 2, Panel A1;
foreach x of global dep_all {;
foreach y of global treatcom1 {;
	reg `x' `y' wave3 wave4 $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};
};

***Output for Table 2 -- Panel A1;
estout ani1_pc ani2_pc land_pc vani1_pc vani2_pc ha_pc 
	using "$out_p/main.text", stats(N mean, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - Unconditional) replace;

estout ani1_pc_pvalue ani2_pc_pvalue land_pc_pvalue vani1_pc_pvalue vani2_pc_pvalue ha_pc_pvalue 
	using "$out_p/time.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_opwave2 t2_c1_opwave3 t2_c1_opwave4) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - Unconditional) replace;
estimates clear;

foreach y of global treatcom1 {;
***Agricultural Assets: Eligibles, Conditional (dummy vars) -- Table 2, Panel A2;
foreach x of global dep_dum {;
	reg `x' `y' wave3 wave4 $basic $controls if `x'97 ==0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls if `x'97 ==0, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};

***Agricultural Assets: Eligibles, Conditional (continuous vars) -- Table 2, Panel A2;
foreach x of global dep_cont {;
	reg `x' `y' wave3 wave4 $basic $controls if `x'97 >0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls if `x'97 >0, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};
};

***Output for Table 2 -- Panel A2;
estout ani1_pc ani2_pc land_pc vani1_pc vani2_pc ha_pc 
	 using "$out_p/main.text", stats(N mean, fmt(%9.0fc %9.3fc %9.3fc) 
	 labels("Number Observations" "Mean Dep Var Conditional Sample (Control Households)")) 
	 keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	 starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - Conditional) append;

estout ani1_pc_pvalue ani2_pc_pvalue land_pc_pvalue vani1_pc_pvalue vani2_pc_pvalue ha_pc_pvalue 
	using "$out_p/time.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Conditional Sample (Control Households)")) 
	keep(t2_c1_opwave2 t2_c1_opwave3 t2_c1_opwave4) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - Conditional) append;
estimates clear;

***Micro-Enterprise: Eligibles, Unconditional -- Table 3;
foreach x of global dep_me {;
foreach y of global treatcom1 {;
	reg `x' `y' wave3 wave4 $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};
};

***Output for Table 3;
estout me2_pc crafts_pc const_pc sewing_pc food_pc repair_pc other_pc
	using "$out_p/main.text", stats(N mean, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ME by Type -- ITT Eligibles) append;

estout me2_pc_pvalue crafts_pc_pvalue const_pc_pvalue sewing_pc_pvalue food_pc_pvalue repair_pc_pvalue other_pc_pvalue 
	using "$out_p/time.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_opwave2 t2_c1_opwave3 t2_c1_opwave4) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(Me by Type -- ITT Eligibles) append;
estimates clear;


*===========================================================================
Regressions: Agricultural Assets & Micro-Enterprise Activity -- Table A6
			 Waves 2, 3, 4 pooled (add test that we can pool) & ITT & controls (Unconditional and Conditional)
*===========================================================================;
preserve;
keep if ineligible ==1;

***Agricultural Assets & Micro-Enterprises: Ineligibles, Unconditional -- Table A6, Panel 1;
foreach x of global dep_all_in {;
foreach y of global treatcom2 {;
	reg `x' `y' wave3 wave4 $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};
};

***Output for Table 6 -- Panel 1;
estout ani1_pc ani2_pc land_pc vani1_pc vani2_pc ha_pc me2_pc crafts_pc 
 	 using "$out_p/main.text", stats(N mean, fmt(%9.0fc %9.3fc  %9.3fc) 
	 labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	 keep(treatcom) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	 starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(Ineligibles - Unconditional) append;

estout ani1_pc_pvalue ani2_pc_pvalue land_pc_pvalue vani1_pc_pvalue vani2_pc_pvalue ha_pc_pvalue me2_pc_pvalue crafts_pc_pvalue
	 using "$out_p/time.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc  %9.3fc) 
	 labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)")) 
	 keep(treatcomwave2 treatcomwave3 treatcomwave4) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	 starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(Ineligibles - Unconditional) append;
estimates clear;

foreach y of global treatcom2 {;
***Agricultural Assets: Ineligibles, Conditional (Dummy Vars) -- Table A6, Panel 2;
foreach x of global dep_dum {;
	reg `x' `y' wave3 wave4 $basic $controls if `x'97 ==0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls if `x'97 ==0, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};

***Agricultural Assets: Ineligibles, Conditional (Continuous Vars) -- Table A6, Panel 2;
foreach x of global dep_cont {;
	reg `x' `y' wave3 wave4 $basic $controls if `x'97 >0, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_pc, title (`x');

	reg `x' `y'wave2 `y'wave3 `y'wave4 wave3 wave4 $basic $controls if `x'97 >0, cl(comuid);
	test `y'wave2 = `y'wave3 = `y'wave4;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar pvalue =r(p);
	estadd scalar pvalue;
	est store `x'_pc_pvalue, title (`x');
};
};

***Output for Table 6 -- Panel 2;
estout ani1_pc ani2_pc land_pc vani1_pc vani2_pc ha_pc
 	 using "$out_p/main.text", stats(N mean, fmt(%9.0fc  %9.3fc %9.3fc) 
	 labels("Number Observations" "Mean Dep Var Conditional Sample (Control Households)")) 
	 keep(treatcom) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	 starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(Ineligibles - Conditional) append;

estout ani1_pc_pvalue ani2_pc_pvalue land_pc_pvalue vani1_pc_pvalue vani2_pc_pvalue ha_pc_pvalue 
	 using "$out_p/time.text", stats(N mean pvalue, fmt(%9.0fc %9.3fc  %9.3fc) 
	 labels("Number Observations" "Mean Dep Var Conditional Sample (Control Households)")) 
	 keep(treatcomwave2 treatcomwave3 treatcomwave4) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	 starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(Ineligibles - Conditional) append;

restore;
log close;

