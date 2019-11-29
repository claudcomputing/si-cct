#delimit;
clear;
cap log close;
cap clear matrix;
cap clear mata;
set more off;
set mem 400m;
set matsize 500;
version 9.0;
sysdir set PLUS "c:/Program Files (x86)/Stata8/ado/plus";

*===========================================================================
Description & Notes: 
	 - Evidence Supporting Productive Investments: crop sales, animal production sales, agricultural income, productive loans
	 - Experimental Period: Most of the data is available for waves Oct 98 and Oct 99 
*==========================================================================================================================;

gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/";

log using "$code/production_income_credit.log", replace;

use $data/investments_data.dta;

*==============================
 Estimation Sample: keep hh with non-missing info on any of the dependent vars (draft animals, prod animals, land & micro-enterprise)
*==============================;
keep if wave ==2 | wave ==3 | wave ==4;
replace land =. if ha ==.;
g byte all =(ani1 ~=. & ani2 ~=. & land ~=. & me~=.);
keep if all ==1;

*=================================
 Wave Dummies & Other Variables
*=================================;
***Wave Dummies;
foreach x in 2 3 4 {;
	g byte wave`x' =(wave ==`x');
};

foreach x in farm cons emerg {;
	replace credit_`x' =0 if credit ==0 & credit_`x' ==.;
};

***Total Value Home Production -- i.e. Total Agricultural Income (monthly) =
	= value of home produced consumption (monthly) + value of crop sales (crop_sales/12) 
	+ value of animal sales (animal_sales_a/6) +
	+ value of animal derivatives sold (animal_prod_sales_amount -- monthly & only available for 98o -- so narrow def);

g crop_sales_m   = crop_sales/12;
g ani_sales_a_m  = ani_sales_a/6;

egen home_prod_tot_w3 		=rsum(crop_sales_m ani_sales_a_m homeprod ani_prod_sales_a);
replace home_prod_tot_w3	=. if (crop_sales_m ==. & ani_sales_a_m ==. & homeprod ==. & ani_prod_sales_a ==.);

egen home_prod_tot 		=rsum(crop_sales_m ani_sales_a_m homeprod);
replace home_prod_tot 	=. if (crop_sales_m ==. & ani_sales_a_m ==. & homeprod ==.);

foreach x in crop_sales_m ani_sales_a_m ani_prod_sales_a home_prod_tot home_prod_tot_w3 ag_exp {;
	g `x'_pp_ae = `x'/hhsize_ae;
	g `x'_pp_ae2 = `x'/hhsize_ae2;
};

lab var home_prod_tot			"Total Farm Income - home prod + crop sales + ani sales (monthly)";
lab var home_prod_tot_pp_ae		"Total Farm Income - home prod + crop sales + ani sales (monthly pp ae - original)";
lab var home_prod_tot_pp_ae2	"Total Farm Income - home prod + crop sales + ani sales (monthly pp ae - Di Maro)";
lab var home_prod_tot_w3		"Total Farm Income - home prod + crop sales + ani sales + ani prod sales (monthly)
								only for wave 3";
lab var home_prod_tot_w3_pp_ae	"Total Farm Income - home prod + crop sales + ani sales + ani prod sales (monthly pp ae 
								- original) only for wave 3";
lab var home_prod_tot_w3_pp_ae2	"Total Farm Income - home prod + crop sales + ani sales + ani prod sales (monthly pp ae
								- Di Maro) only for wave 3";

**Trim Outliers;
foreach x in homeprod home_prod_tot home_prod_tot_w3
homeprod_pp_ae home_prod_tot_pp_ae home_prod_tot_w3_pp_ae
homeprod_pp_ae2 home_prod_tot_pp_ae2 home_prod_tot_w3_pp_ae2 {;
	xtile `x'_tile =`x', nq(100); 
	sum `x' if `x'_tile <=95;
};

*==============================
  Define Globals 
===============================;
gl dep_d 	maiz frijol ani_prod credit_farm;
gl credit 	credit credit_cons credit_farm trans_t trans_out;
gl home  	home_prod_tot home_prod_tot_pp_ae home_prod_tot_pp_ae2;
gl home3  	home_prod_tot_w3 home_prod_tot_w3_pp_ae home_prod_tot_w3_pp_ae2;

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

gl controls_ae	no497 big497
				age_hh age_hh2 female_hh educ1_hh ethnicity_hh 
				age_sp age_sp2 educ1_sp 			
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p hhsize_ae
				homeown97 dirtfloor97  electricity97 
				org_faenas min_dist lnup_cwagepm up2_mcwagepm
				dummy_age_hh dummy_educ_hh dummy_ethnicity_hh dummy_age_sp dummy_educ_sp
				dummy_dage0_7_97 dummy_dirtfloor97 dummy_electricity97;

gl controls_ae2	no497 big497
				age_hh age_hh2 female_hh educ1_hh ethnicity_hh 
				age_sp age_sp2 educ1_sp 			
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p hhsize_ae2
				homeown97 dirtfloor97  electricity97 
				org_faenas min_dist lnup_cwagepm up2_mcwagepm
				dummy_age_hh dummy_educ_hh dummy_ethnicity_hh dummy_age_sp dummy_educ_sp
				dummy_dage0_7_97 dummy_dirtfloor97 dummy_electricity97;
				
*=====================================================
Supporting Evidence - Total Agricultural Income B (wave 3) -- Table 4 
*=====================================================;
foreach x of global home3 {;
foreach y of global treatcom1 {;
	reg `x' `y' $basic $controls if home_prod_tot_w3_tile <=95 & wave ==3, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};
};

foreach x in home_prod_tot_w3 {;
foreach y of global treatcom1 {;
reg  `x' `y' $basic $controls_ae if home_prod_tot_w3_tile <=95 & wave ==3, cl(comuid);
sum `x' if e(sample) & `y' ==0;
scalar mean =r(mean);
estadd scalar mean;
scalar std =r(sd);
estadd scalar std;
est store `x'_ae, title (`x');

reg `x' `y' $basic $controls_ae2 if home_prod_tot_w3_tile <=95 & wave ==3, cl(comuid);
sum `x' if e(sample) & `y' ==0;
scalar mean =r(mean);
estadd scalar mean;
scalar std =r(sd);
estadd scalar std;
est store `x'_ae2, title (`x');
};
};

*=====================================================
Supporting Evidence - Crops Production, Animal Production Sales, Productive Loans and Agricultural Income A (waves 2 and 3)  -- Table 4 
*=====================================================;
foreach x of global dep_d {;
foreach y of global treatcom1 {;
	reg `x' `y' wave3 $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};
};

foreach x of global home {;
foreach y of global treatcom1 {;
	reg `x' `y' wave3 if home_prod_tot_tile <=95, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_nc, title (`x');

	reg `x' `y' wave3 $basic $controls if home_prod_tot_tile <=95, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};
};

foreach x in home_prod_tot {;
foreach y of global treatcom1 {;
	reg `x' `y' wave3 $basic $controls_ae if home_prod_tot_tile <=95, cl(comuid);	
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_ae, title (`x');
	
	reg `x' `y' wave3 $basic $controls_ae2 if home_prod_tot_tile <=95, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_ae2, title (`x');
};
};
	
***Output for Table 4;
estout home_prod_tot_pp_ae2_c home_prod_tot_w3_pp_ae2_c maiz_c frijol_c ani_prod_c credit_farm_c
	using "$out_p/supporting.text", stats(N mean std, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)" 
	"SD Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Income - Eligibles) replace;

***Appendix -- Output for Table A3;
estout home_prod_tot_pp_ae2_c home_prod_tot_pp_ae_c home_prod_tot_c home_prod_tot_ae2 home_prod_tot_ae 
	 home_prod_tot_w3_pp_ae2_c home_prod_tot_w3_pp_ae_c home_prod_tot_w3_c home_prod_tot_w3_ae2 home_prod_tot_w3_ae 
	using "$out_p/supporting.text", stats(N mean std, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)" 
	"SD Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Income pp ae Appendix) append;

*=====================================================
Supporting Evidence - Credit (Waves 2 & 3), Transfers from Friends, Transfers to Friends (waves 2 and 4) -- Eligibles -- Table 6
*=====================================================;
foreach x of global credit {;
foreach y of global treatcom1 {;
	reg `x' `y' $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};
};

***Output for Table 6;
estout credit_c credit_cons_c credit_farm_c trans_t_c trans_out_c
	using "$out_p/supporting.text", stats(N mean std, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)" 
	"SD Dep Var Unconditional Sample (Control Households)")) 
	keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Credit - Eligibles) append;

*=====================================================
Supporting Evidence - Crops Production, Animal Production Sales, Productive Loans and Agricultural Income A & B)  -- Table A7 INELIGIBLES
*=====================================================;
estimates clear;
preserve;
keep if ineligible ==1;

foreach y of global treatcom2 {;
foreach x of global home3 {;
	reg `x' `y' $basic $controls if home_prod_tot_w3_tile <=95 & wave ==3, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};

foreach x of global dep_d {;
	reg `x' `y' wave3 $basic $controls, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};

foreach x of global home {;
	reg `x' `y' wave3 $basic $controls if home_prod_tot_tile <=95, cl(comuid);
	sum `x' if e(sample) & `y' ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	scalar std =r(sd);
	estadd scalar std;
	est store `x'_c, title (`x');
};
};

tab ineligible;

***Appendix -- Output for Table A7;
estout home_prod_tot_pp_ae2_c home_prod_tot_w3_pp_ae2_c maiz_c frijol_c ani_prod_c credit_farm_c
	using "$out_p/supporting.text", stats(N mean std, fmt(%9.0fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var Unconditional Sample (Control Households)" 
	"SD Dep Var Unconditional Sample (Control Households)")) 
	keep(treatcom) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Income - Ineligibles) append;

restore;
log close;




