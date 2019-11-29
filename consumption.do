#delimit;
clear;
cap log close;
cap clear matrix;
cap clear mata;
set more off;
set mem 400m;
set matsize 1500;

*sysdir set PLUS 		"c:/Program Files (x86)/Stata8/ado/plus";
*sysdir set PERSONAL 	"c:/Program Files (x86)/Stata8/ado/personal";

*============================================================*
Description: MPC, MIE on Consumption (Table 10) & First Stages (Table A10)
			 Oportunidades Impact on Consumption and MPC during Experimental Period & Test if MPC is Constant (Table 8)
			 Sensitiviy of MPC and MIE to Trimming Outliers (Table A9)
*=============================================================*;

/*
gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/";

Claudia's directories*/
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta";
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct";
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output";
log using "$code/consumption.log", replace;

*===========================================
 Estimation Sample & Create New Vars
===========================================;
use "$data/investments_data.dta";
drop if consumo_pp_ae ==0; 

***Wave Dummies;
foreach x in 2 3 4 6 7 {;
	g byte wave`x' =(wave ==`x');
};

foreach x in t2_c1_op  {;
foreach y of varlist wave2 wave3 wave4{;
	g `x'_`y' = `x' * `y';
};
};

foreach x in 02 314 1526 27p 15p {;
	foreach y in act pot {;
		g cum`x'_`y'_pp_ae2 =cum`x'_`y'/hhsize_ae2;
	};
};

*============================*
Final Cleaning: identify potential consumption and transfers outliers
*============================*;
foreach x in consumo_pp_ae2 consumo1_pp_ae2 {;
	xtile `x'_tl_t2 = `x' if t2_c1_op ~=., nq(200);
};

foreach x in witransfer_act_m_pp_ae2 {;
	xtile `x'_tl_t2 = `x' if t2_c1_op ~=. & `x' >0, nq(200);
};

*=================*
 Final Globals - final set of controls
==================*;
gl wave  		wave3 wave6 wave7;
gl basic		nbani197 nbani297 ha97;
gl controls_ae2	no497 big497
				age_hh age_hh2 female_hh educ1_hh ethnicity_hh 
				age_sp age_sp2 educ1_sp 			
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p hhsize_ae2_97
				homeown97 dirtfloor97  electricity97  
				org_faenas min_dist 
				dummy_age_hh dummy_educ_hh dummy_ethnicity_hh dummy_age_sp dummy_educ_sp
				dummy_dage0_7_97 dummy_dirtfloor97 dummy_electricity97;

*============================*
1. MPC Over Time during Experimental Period  (Table 8)
*============================*;
	reg consumo1_pp_ae2 t2_c1_op_wave2 t2_c1_op_wave3 t2_c1_op_wave4 $controls_ae2 if consumo1_pp_ae2_tl_t2 >2 & consumo1_pp_ae2_tl_t2 <199  
	& witransfer_act_m_pp_ae2_tl_t2 ~=1 &  witransfer_act_m_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=199 & witransfer_act_m_pp_ae2_tl_t2 ~=200 & wave <=4, cl(comuid);
	sum consumo1_pp_ae2 if e(sample) & t2_c1_op ==0;
	scalar avg_c =r(mean);
	estadd scalar avg_c;
	sum witransfer_act_m_pp_ae2 if e(sample);
	scalar avg_t =r(mean);
	estadd scalar avg_t;
	test _b[t2_c1_op_wave2] = _b[t2_c1_op_wave3];
	scalar pvalue2 =r(p);
	estadd scalar pvalue2;
	test _b[t2_c1_op_wave2] =_b[t2_c1_op_wave3] =_b[t2_c1_op_wave4];
	scalar pvalue3 =r(p);
	estadd scalar pvalue3;
	est store consumo1_wave3, title(consumo1_pp_ae2);

	bys wave: sum witransfer_act_m_pp_ae2 if e(sample) & t2_c1_op ==1;
		
	reg consumo_pp_ae2 t2_c1_op_wave2 t2_c1_op_wave3 $controls_ae2 if consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199  
	& witransfer_act_m_pp_ae2_tl_t2 ~=1 &  witransfer_act_m_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=199 & witransfer_act_m_pp_ae2_tl_t2 ~=200 & wave <=4, cl(comuid);
	sum consumo_pp_ae2 if e(sample) & t2_c1_op ==0;
	scalar avg_c =r(mean);
	estadd scalar avg_c;
	sum witransfer_act_m_pp_ae2 if e(sample);
	scalar avg_t =r(mean);
	estadd scalar avg_t;
	test _b[t2_c1_op_wave2] = _b[t2_c1_op_wave3];
	scalar pvalue2 =r(p);
	estadd scalar pvalue2;
	est store consumo_wave2, title(consumo_pp_ae2);
	
estout consumo_wave2 consumo1_wave3
	using "$out_p/consumo_app.text", stats(N avg_c avg_t pvalue2 pvalue3, fmt(%9.0fc %9.3fc %9.3fc %9.3fc %9.3fc) 
	labels("Number Observations" "Mean Consumption pc ae (Control Households)" "Mean Transfer pc ae" "Prob(w2=w3)" "Prob(w2=w3=w4)")) 
	keep(t2_c1_op_wave2 t2_c1_op_wave3 t2_c1_op_wave4) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(MPC per Wave -- Eligibles) replace;
estimates clear;

*============================*
2. Trimming Outliers -- Appendix Table (Table A9)
*============================*;
ren t2_c1_op t2_op;
drop if t2_op ==.;

foreach x in consumo_pp_ae2 {;
xi: ivreg `x'
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum1526_pot_pp_ae2 cum27p_pot_pp_ae2) $controls_ae2 i.muni*i.wave, cl(comuid) first;
   	sum `x' if e(sample) & t2_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_notrim, title (notrim);
	
xi: ivreg `x' 
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum1526_pot_pp_ae2 cum27p_pot_pp_ae2) $controls_ae2 i.muni*i.wave 
	if consumo_pp_ae2_tl_t2 ~=200 & witransfer_act_m_pp_ae2_tl_t2 ~=200, cl(comuid) first;
    sum `x' if e(sample) & t2_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_top5, title (top0.5);

xi: ivreg `x' 
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum1526_pot_pp_ae2 cum27p_pot_pp_ae2) $controls_ae2 i.muni*i.wave 
	if consumo_pp_ae2_tl_t2 ~=1 & consumo_pp_ae2_tl_t2 ~=200 
	& witransfer_act_m_pp_ae2_tl_t2 ~=1 & witransfer_act_m_pp_ae2_tl_t2 ~=200, cl(comuid) first;
    sum `x' if e(sample) & t2_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_tb5, title (t&b0.5);

xi: ivreg `x'
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum1526_pot_pp_ae2 cum27p_pot_pp_ae2) $controls_ae2 i.muni*i.wave 
	if consumo_pp_ae2_tl_t2 ~=200 & consumo_pp_ae2_tl_t2 ~=199 
	& witransfer_act_m_pp_ae2_tl_t2 ~=200 & witransfer_act_m_pp_ae2_tl_t2 ~=199, cl(comuid) first;
    sum `x' if e(sample) & t2_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_top1, title (top1);
	
xi: ivreg `x'
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum1526_pot_pp_ae2 cum27p_pot_pp_ae2) $controls_ae2 i.muni*i.wave 
	if consumo_pp_ae2_tl_t2 ~=200 & consumo_pp_ae2_tl_t2 ~=199 & consumo_pp_ae2_tl_t2 ~=1 & consumo_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=200 & witransfer_act_m_pp_ae2_tl_t2 ~=199 
	& witransfer_act_m_pp_ae2_tl_t2 ~=1 & witransfer_act_m_pp_ae2_tl_t2 ~=2, cl(comuid) first;
    sum `x' if e(sample) & t2_op ==0;
	scalar mean =r(mean);
	estadd scalar mean;
	est store `x'_tb1, title (t&b1);
};

	estout consumo_pp_ae2_notrim consumo_pp_ae2_top5 consumo_pp_ae2_tb5 consumo_pp_ae2_top1 consumo_pp_ae2_tb1 
	using "$out_p/consumo_app.text", stats(N mean, fmt(%9.0fc %9.3fc) 
	labels("Number Observations" "Mean Consumption pc ae (Control Households)")) 
	keep(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2) cells(b(fmt(%9.3f) star) se(fmt(%9.3f) par(( ))))
	starl(+ 0.1 * 0.05 ** 0.01) legend label collabels(none) title(Consumption Trimming -- Eligibles) append;
estimates clear;
		
*============================*
3. Regressions: Effect of Oportunidades on Consumption using predicted transfers as instruments for actual transfers
				note t2_c1_op = intention to treat   -- t2
				Trim top & bottom 1% of consumption and monthly transfer distribution (Tables 10, A10)
*============================*;
*************************2SLS *****************************;
#delimit;
xi:  ivreg consumo_pp_ae2 
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum1526_pot_pp_ae2 cum27p_pot_pp_ae2)
	$controls_ae2 i.muni*i.wave
 	if  consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199 & witransfer_act_m_pp_ae2_tl_t2 ~=1 & witransfer_act_m_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=199 & witransfer_act_m_pp_ae2_tl_t2 ~=200, cl(comuid) first;
    sum consumo_pp_ae2 if e(sample) & t2_op ==0;
    scalar avg =r(mean);
	sum witransfer_act_m_pp_ae2 if e(sample);
	scalar avgt =r(mean);
    test _b[witransfer_act_m_pp_ae2]==1;
    scalar mpc1 =r(F);
    scalar pmpc1 =r(p);
    test _b[cum1526_act_pp_ae2] = _b[cum27p_act_pp_ae2];
	#delimit;
    outreg using "$out_p/consumo_pp_ae2.txt", 
    se bdec(3) ctitle(LS) /*coefastr 10pct
    addstat( "Mean Dep Var:", avg , "Mean Transfer:", avgt , "MPC =1 F(1):", mpc1, "Prob MPC =1:", pmpc1 , 
		   "Cum 1526 =27p", r(F), "Prob Cum 1526 =27p", r(p)) adec(3)*/  replace;

xi:  ivreg consumo_pp_ae2 
	(witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum15p_act_pp_ae2 = 
	transfer_hh3_m_pp_ae2 cum314_pot_pp_ae2 cum15p_pot_pp_ae2)
	$controls_ae2 i.muni*i.wave
  	if  consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199 & witransfer_act_m_pp_ae2_tl_t2 ~=1 & witransfer_act_m_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=199 & witransfer_act_m_pp_ae2_tl_t2 ~=200, cl(comuid) first;
    sum consumo_pp_ae2 if e(sample) & t2_op ==0;
    scalar avg =r(mean);
	sum witransfer_act_m_pp_ae2 if e(sample);
	scalar avgt =r(mean);
    test _b[witransfer_act_m_pp_ae2]==1;
    scalar mpc1 =r(F);
    scalar pmpc1 =r(p);
    outreg using "$out_p/consumo_pp_ae2.txt", 
    se bdec(3) ctitle(LS) /*coefastr 10pct
    addstat( "Mean Dep Var:", avg , "Mean Transfer:", avgt , "MPC =1 F(1):", mpc1, "Prob MPC =1:", pmpc1) adec(3)*/ append;

*************************OLS *****************************;
xi: reg consumo_pp_ae2 
	witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum1526_act_pp_ae2 cum27p_act_pp_ae2 
	$controls_ae2 i.muni*i.wave
  	if  consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199 & witransfer_act_m_pp_ae2_tl_t2 ~=1 & witransfer_act_m_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=199 & witransfer_act_m_pp_ae2_tl_t2 ~=200, cl(comuid);
	sum consumo_pp_ae2 if e(sample) & t2_op ==0;
	scalar avg =r(mean);
	sum witransfer_act_m_pp_ae2 if e(sample);
	scalar avgt =r(mean);
    test _b[witransfer_act_m_pp_ae2]==1;
    scalar mpc1 =r(F);
    scalar pmpc1 =r(p);
	test _b[cum1526_act_pp_ae2] = _b[cum27p_act_pp_ae2];
	outreg using "$out_p/consumo_pp_ae2.txt", 
	se bdec(3) ctitle(OLS) /*coefastr 10pct
	addstat( "Mean Dep Var:", avg , "Mean Transfer:", avgt , "MPC =1 F(1):", mpc1, "Prob MPC =1:", pmpc1 , 
	"Cum 1526 =27p", r(F), "Prob Cum 1526 =27p", r(p)) adec(3)*/ append;

xi: reg consumo_pp_ae2 
	witransfer_act_m_pp_ae2 cum314_act_pp_ae2 cum15p_act_pp_ae2 
	$controls_ae2 i.muni*i.wave
 	if  consumo_pp_ae2_tl_t2 >2 & consumo_pp_ae2_tl_t2 <199 & witransfer_act_m_pp_ae2_tl_t2 ~=1 & witransfer_act_m_pp_ae2_tl_t2 ~=2 
	& witransfer_act_m_pp_ae2_tl_t2 ~=199 & witransfer_act_m_pp_ae2_tl_t2 ~=200, cl(comuid);
    sum consumo_pp_ae2 if e(sample) & t2_op ==0;
    scalar avg =r(mean);
	sum witransfer_act_m_pp_ae2 if e(sample);
	scalar avgt =r(mean);
    test _b[witransfer_act_m_pp_ae2]==1;
    scalar mpc1 =r(F);
    scalar pmpc1 =r(p);
    outreg using "$out_p/consumo_pp_ae2.txt", 
    se bdec(3) ctitle(LS) /*coefastr 10pct
    addstat( "Mean Dep Var:", avg , "Mean Transfer:", avgt , "MPC =1 F(1):", mpc1, "Prob MPC =1:", pmpc1) adec(3)*/ append;

log close;


