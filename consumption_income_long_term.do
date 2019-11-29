#delimit;
clear;
cap clear matrix;
cap clear mata;
cap log close;
set more off;
set mem 400m;
set matsize 800;

sysdir set PLUS 		"c:/Program Files (x86)/Stata8/ado/plus";
sysdir set PERSONAL 	"c:/Program Files (x86)/Stata8/ado/personal";

*=========================*
Description: Estimate impact of progresa on consumption and other sources of income in Nov 2003 
				Eligible and Ineligible Households (Tables 5, A8)
				Sensitivity to Trimming Consumption Outliers & Separate Food vs. Non-Food Expenditure (Table A4)
*=========================*;

/*
gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/";

Claudia's directories*/
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta";
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct";
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output";
log using 	"$code/consumption_income_long_term.log", replace;

use "$data/investments_data.dta";

*==============================
 Estimation Sample -- households with information on consumption in Nov 2003
*==============================;
keep if wave == 7;
drop if consumo_pp_ae ==0 | consumo_pp_ae ==.;

*================================================================*
 Private & Public Transfers & Out of the Hh Earnings - PP AE & Compare Values/Trimming
*=================================================================*;
gl public 	solidaridad INI probecat empleotemp procampo liconsa DIF tortilla progbecas progpapilla progmonetario
			amount_solidaridad amount_INI amount_probecat amount_empleotemp amount_procampo;
sum $public;
des $public;

**Procampo and progmonetario (progresa) are the most common public subsidies hh receives -- consider amount_procampo;
ren amount_procampo 	amount_p; 			*public transfers;
ren amount_t 			amount_t_pr;		*private transfers;

ren nonfood nonfoodexp;
foreach x in foodexp nonfoodexp {;
	drop `x'_pp `x'_pp_ae;
};

foreach x in amount_p amount_t_pr amount_borrowed_m outwage_lw_hh foodexp nonfoodexp {;
	g `x'_pp_ae = `x'/hhsize_ae;
	g `x'_pp_ae2 = `x'/hhsize_ae2;
};

egen amount_t_pb	 		=rsum(amount_p witransfer_act_m);
replace amount_t_pb		 	=. if amount_p ==. & witransfer_act_m ==.;
egen amount_t_pb_pp_ae 		=rsum(amount_p_pp_ae witransfer_act_m_pp_ae);
replace amount_t_pb_pp_ae 	=. if amount_p_pp_ae ==. & witransfer_act_m_pp_ae ==.;
egen amount_t_pb_pp_ae2 	=rsum(amount_p_pp_ae2 witransfer_act_m_pp_ae2);
replace amount_t_pb_pp_ae2	=. if amount_p_pp_ae2 ==. & witransfer_act_m_pp_ae2 ==.; 

** Dummies for Proportions;
foreach x in consumo homeprod foodexp nonfoodexp witransfer_act_m amount_p amount_t_pb amount_t_pr amount_borrowed_m outwage_lw_hh
consumo_pp_ae homeprod_pp_ae foodexp_pp_ae nonfoodexp_pp_ae witransfer_act_m_pp_ae amount_p_pp_ae amount_t_pb_pp_ae amount_t_pr_pp_ae 
amount_borrowed_m_pp_ae outwage_lw_hh_pp_ae
consumo_pp_ae2 homeprod_pp_ae2 foodexp_pp_ae2 nonfoodexp_pp_ae2 witransfer_act_m_pp_ae2 amount_p_pp_ae2 amount_t_pb_pp_ae2 amount_t_pr_pp_ae2 
amount_borrowed_m_pp_ae2 outwage_lw_hh_pp_ae2 {;
	g d`x' =(`x'>0);
};

*============================*
 Trim Outliers
*============================*;
sum amount_p amount_t_pr amount_borrowed_m outwage_lw_hh amount_p_pp_ae2 witransfer_act_m_pp_ae2 amount_p_pp_ae2 amount_t_pb_pp_ae2 
amount_t_pr_pp_ae2 outwage_lw_hh_pp_ae2 amount_borrowed_m_pp_ae2 consumo_pp_ae2 foodexp nonfoodexp foodexp_pp_ae2 nonfoodexp_pp_ae2 
if t2_c1_op~=., d;

foreach x in consumo consumo_pp_ae consumo_pp_ae2 
homeprod homeprod_pp_ae homeprod_pp_ae2 foodexp foodexp_pp_ae foodexp_pp_ae2 nonfoodexp nonfoodexp_pp_ae nonfoodexp_pp_ae2  
witransfer_act_m witransfer_act_m_pp_ae witransfer_act_m_pp_ae2 amount_p amount_p_pp_ae amount_p_pp_ae2 
amount_t_pb amount_t_pb_pp_ae amount_t_pb_pp_ae2 amount_t_pr amount_t_pr_pp_ae amount_t_pr_pp_ae2 
amount_borrowed_m amount_borrowed_m_pp_ae amount_borrowed_m_pp_ae2 outwage_lw_hh outwage_lw_hh_pp_ae outwage_lw_hh_pp_ae2 {;
	xtile `x'_tl_t2 = `x' if t2_c1_op ~=., nq(200);
	xtile `x'_tl_tr = `x' if treatcom ~=. & ineligible ==1, nq(200);
};

*=================*
 Final Globals - final set of controls
*=================*;
gl basic		nbani197 nbani297 ha97;
gl controls		no497 big497
				age_hh age_hh2 female_hh educ1_hh ethnicity_hh 
				age_sp age_sp2 educ1_sp 			
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p 
				homeown97 dirtfloor97 electricity97  
				org_faenas min_dist lnup_cwagepm up2_mcwagepm
				dummy_age_hh dummy_educ_hh dummy_ethnicity_hh dummy_age_sp dummy_educ_sp
				dummy_dage0_7_97 dummy_dirtfloor97 dummy_electricity97;
			
*======================================================
 ITT results: wave 7 - Eligibles -- Consumption & Other Income Sources (Table 5)
		Trim top & bottom 1% consumption distribution, top 2% of distrib of other vars (5% for wages).
*=======================================================;
foreach x in consumo_pp_ae2 foodexp_pp_ae2 nonfoodexp_pp_ae2 {;
	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x', title (`x');
};

foreach x in homeprod_pp_ae2 witransfer_act_m_pp_ae2 amount_p_pp_ae2 amount_t_pb_pp_ae2 amount_t_pr_pp_ae2 amount_borrowed_m_pp_ae2 {;
	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199 & `x'_tl_t2 <=196, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x', title (`x');
};

foreach x in outwage_lw_hh_pp_ae2 {;
	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199 & `x'_tl_t2 <=190, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x', title (`x');
};

***Output Table 5;
estout consumo_pp_ae2 homeprod_pp_ae2 witransfer_act_m_pp_ae2 amount_p_pp_ae2 amount_t_pr_pp_ae2 amount_borrowed_m_pp_ae2 outwage_lw_hh_pp_ae2 
	using "$out_p/consumo_w7.text", stats(N mean mean_p sd prop, fmt(%9.0fc %9.3fc %9.3fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var (Control Households)" "Mean Dep Var | y >0(Control Households)" "SD Dep Var (Control Households)"
	 "Prop Dep Var >0")) keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - AE2) replace;

*======================================================
 ITT results: wave 7 - Eligibles -- Consumption AE2 
			  Sensitivity to Triming Outliers & Food vs. Non-Food Expenditures (Table A4)
*=======================================================;
foreach x in consumo_pp_ae2 {;
	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x'_notrim, title (notrim);

	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 <200, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x'_top5, title (top0.5);

	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 >1 & consumo_pp_ae2_tl_t2 <200, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x'_tb5, title (t&b0.5);

	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 <199, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x'_top1, title (top1);

	reg `x' t2_c1_op $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199, cl(comuid);
	sum `x' if e(sample) & t2_c1_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & t2_c1_op ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x'_tb1, title (t&b1);
};

***Output for Appendix Table A4;
estout consumo_pp_ae2_notrim consumo_pp_ae2_top5 consumo_pp_ae2_tb5 consumo_pp_ae2_top1 consumo_pp_ae2_tb1 foodexp_pp_ae2 nonfoodexp_pp_ae2 
	using "$out_p/consumo_w7.text", stats(N mean mean_p sd prop, fmt(%9.0fc %9.3fc %9.3fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var (Control Households)" "Mean Dep Var | y >0(Control Households)" "SD Dep Var (Control Households)"
	 "Prop Dep Var >0")) keep(t2_c1_op) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Eligibles - Consumption by Type & Outliers) append;
estimates clear;

*==================================================================================
ITT results: wave 7 - Ineligibles -- Consumption & Other Income Sources (Table A8)
		Trim top & bottom 1% consumption distribution, top 2% of distrib of other vars (5% for wages).
*===================================================================================;
preserve;
keep if ineligible ==1;
count if witransfer_act_m >0 & witransfer_act_m ~=.;
replace witransfer_act_m =0 if witransfer_act_m >0 &  witransfer_act_m ~=.; *ineligible are not receiving transfers;

foreach x in consumo_pp_ae2 foodexp_pp_ae2 nonfoodexp_pp_ae2 {;
	reg `x' treatcom $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_tr >2 & consumo_pp_ae2_tl_tr <199, cl(comuid);
	sum `x' if e(sample) & treatcom ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & treatcom ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x', title (`x');
};

foreach x in homeprod_pp_ae2 amount_p_pp_ae2 amount_t_pr_pp_ae2 amount_borrowed_m_pp_ae2 {;
	reg `x' treatcom $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_tr >2 & consumo_pp_ae2_tl_tr <199 & `x'_tl_tr <=196, cl(comuid);
	sum `x' if e(sample) & treatcom ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & treatcom ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x', title (`x');
};

foreach x in outwage_lw_hh_pp_ae2 {;
	reg `x' treatcom $basic hhsize_ae2_97 $controls if consumo_pp_ae2_tl_tr >2 & consumo_pp_ae2_tl_tr <199 & `x'_tl_tr <=190, cl(comuid);
	sum `x' if e(sample) & treatcom ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	sum `x' if e(sample) & treatcom ==0 & `x'>0;
	scalar mean_p =r(mean);
	estadd scalar mean_p;
	scalar sd =r(sd);
	estadd scalar sd;
	sum d`x' if e(sample);
	scalar prop =r(mean);
	estadd scalar prop;
	est store `x', title (`x');
};

***Output for Appendix Table A8;
estout consumo_pp_ae2 homeprod_pp_ae2 amount_p_pp_ae2 amount_t_pr_pp_ae2 amount_borrowed_m_pp_ae2 outwage_lw_hh_pp_ae2 
	using "$out_p/consumo_w7.text", stats(N mean mean_p sd prop, fmt(%9.0fc %9.3fc %9.3fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Dep Var (Control Households)" "Mean Dep Var | y >0(Control Households)" "SD Dep Var (Control Households)"
	 "Prop Dep Var >0")) keep(treatcom) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(ITT Ineligibles - AE2) append;

estimates clear;
restore;
log close;

