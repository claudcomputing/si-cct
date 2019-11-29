#delimit;
clear;
cap log close;
cap clear matrix;
set more off;
set mem 400m;
program drop _all;
*sysdir set PLUS "c:/Program Files (x86)/Stata8/ado/plus";

*=======================
Description: Summary Stats for Eligible Households (Table 1) and Ineligible Households (Table A5) 
			 Macro Income and Price Effects -- Animal Prices and Community Wages (Table A2)
			 Sample Sizes & Analysis of Attrition (Section 3 "Experimental Design and Data")
===========================================================================================================================;
/*
Original directories
gl data		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl code		"C:/THESIS/OPORTUNIDADES/INVESTMENTS/CODE/vAEJApp/";
gl out_p	"C:/THESIS/OPORTUNIDADES/INVESTMENTS/T/vAEJApp/des/";*/

/*
Claudia's directories*/
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta";
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct";
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output";


log using "$code/investments_des.log", replace;

u "$data/investments_data.dta";

*======================================
 Create Wave & Edu Dummies
 Recover Missing Values for Summary Stats (values had been replaced)
=======================================;
***Wave Dummies;
foreach x in 0 2 3 4 5 6 7 {;
	gen byte wave`x' = (wave ==`x');
	label var wave`x' "Wave `x' =1";
};

***Recover missing values;
foreach x in no497 small497 big497 {;
	replace `x' =. if deffarm97 ==.;
};

foreach x in age_hh female_hh ethnicity_hh educ_hh age_sp educ_sp hhsize97 homeown97 dirtfloor97 electricity97 {;
	replace `x' =. if dummy_`x' ==1;
};

foreach x in dage0_7 dage8_17_97 dage18_54_97 dage55p {;
	replace `x' =. if dummy_dage0_7 ==1;
};

replace up_cwagepm =. if up2_mcwagepm ==1;  
replace up_cwagepw =. if up2_mcwagepm ==1;  
replace up_cwagepc =. if up2_mcwagepm ==1;  

***Education Dummies;
foreach x in hh sp {;
	drop educ1_`x';
	g educ0_`x' =(educ_`x' <=0.5);
	g educ1_`x' =(educ_`x' >=1 & educ_`x' <=5);
	g educ2_`x' =(educ_`x' ==6);
	g educ3_`x' =(educ_`x' >=7 & educ_`x'<=18);
};

foreach x in hh sp {;
	foreach y in 0 1 2 3 {;
		replace educ`y'_`x' =. if educ_`x' ==.;
	};
};

*===========================================
  Define Globals for Summary Statistics
============================================;
gl dep97_d 		ani197 ani297 land97;
gl dep97_c 		vani197 vani297 ha97;
gl cont_d 		female_hh ethnicity_hh educ0_hh educ1_hh educ2_hh educ3_hh educ0_sp educ1_sp educ2_sp educ3_sp 
				dage0_7_97 dage8_17_97 dage18_54_97 dage55p_97 
				homeown97 dirtfloor97 electricity97 no497 small497 big497 org_faenas;
gl cont_c 		age_hh age_sp hhsize97 min_dist up_cwagepm;
gl cont_ent 	age_ent1 educ_ent1 female_ent1 benef_ent1;

*Treatment/control groups;
gl treatcom1 t2_c1_op;
gl treatcom3 treatcom;

version 7.0; *change version to run old code for sum stats;
svyset psu comuid;

*=============================
  Sample Sizes 
==============================;
tab wave0;
tab t2_c1_op if wave ==0;
tab t1_c2_op if wave ==0;

*================================================================================
 Covariates & Dependent Vars at Baseline (wave 0): Eligible Households (Table 1)
*================================================================================;
**Covariates;
postfile eligibles str20 name obstret treat tstd obscont control cstd tstat using "$out_p/eligibles.dta", replace;
	foreach var of global cont_d {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post eligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0)  (difttest);
 		drop difttest name;
		scalar drop _all;
	};

	foreach var of global cont_c {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if `var' >0 & wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if `var' >0 & wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post eligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

***Characteristics entrepreneur -- use info in wave ==2 (Oct 98) because there is no infomration availabel at baseline;
	foreach var of global cont_ent {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==2 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean `var' if wave ==2, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post eligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

***Dependent Variables;
	foreach var of global dep97_d {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post eligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

	foreach var of global dep97_c {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if `var' >0 & wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if  `var' >0 & wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post eligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};
postclose eligibles;

*====================================================================================
 Covariates & Dependent Vars at Baseline (wave 0): Ineligible Households (Table A5)
*====================================================================================;
preserve;
keep if ineligible ==1;

**Covariates;
postfile ineligibles str20 name obstret treat tstd obscont control cstd tstat using "$out_p/ineligibles.dta", replace;
	foreach var of global cont_d {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==0 & treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==0, obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post ineligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0)  (difttest);
 		drop difttest name;
		scalar drop _all;
	};


	foreach var of global cont_c {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if `var' >0 & wave ==0 & treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if `var' >0 & wave ==0, obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post ineligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

***Characteristics entrepreneur -- use info in wave ==2 (Oct 98) because there is no infomration available at baseline;
	foreach var of global cont_ent {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==2 & treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean `var' if wave ==2, obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post ineligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

***Dependent Variables;
	foreach var of global dep97_d {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==0 & treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==0, obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post ineligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

	foreach var of global dep97_c {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if `var' >0 & wave ==0 & treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if  `var' >0 & wave ==0, obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post ineligibles  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};
postclose ineligibles;
restore;

*=========================================================
  Animal Prices (Community Selling Prices)  -- Table A2, Panel B
	- Use information from wave 2 (Oct 98) as there is no information on animal prices at baseline. 
	- The animal prices (community averaged) are constructed using household reports on value of animals sold in October 1998 and May 1999
*=========================================================;
gl prices csprice1_av csprice2_av csprice3_av csprice4_av csprice5_av csprice6_av csprice7_av csprice8_av;
preserve;
keep if wave ==2; 
bys comuid wave: g counter_comu =_n;
keep if counter_comu ==1; *one observation per community;

postfile aprices str20 name obstret treat tstd obscont control cstd  tstat using "$out_p/aprices.dta", replace;
foreach var of global prices {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var', obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post aprices (name) (N1) (coef1) (std1) (N0) (coef0) (std0)  (difttest);
 		drop difttest name;
		scalar drop _all;
	};
postclose aprices;
restore;

*========================================================
   Community Wages -- Table A2, Panel A
*========================================================;
gl cwages up_cwagepm up_cwagepw up_cwagepc;
preserve;
keep if wave >=2 & wave <=4;
bys comuid wave: g counter_comu =_n;
keep if counter_comu ==1; *one observation per community;

postfile cwages str20 name obstret treat tstd obscont control cstd tstat using "$out_p/cwages.dta", replace;
foreach var of global cwages {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if treatcom ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var', obs  by(treatcom);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post cwages (name) (N1) (coef1) (std1) (N0) (coef0) (std0)  (difttest);
 		drop difttest name;
		scalar drop _all;
	};
postclose cwages;
restore;

*========================================
  Analysis of Attrition
*========================================;
***Flag attriters -- per wave;
sort state muni local folio wave;
by state muni local folio: g out0_2 =(wave[_n] ==0 & (wave[_n+1] ==3 | wave[_n+1] ==4 | wave[_n+1] ==5 | wave[_n+1] ==6 | wave[_n+1] ==7));
sort state muni local folio wave;
by state muni local folio: g out2_3 =(wave[_n] ==2 & (wave[_n+1] ==4 | wave[_n+1] ==5 | wave[_n+1] ==6 | wave[_n+1] ==7));
sort state muni local folio wave;
by state muni local folio: g out3_4 =(wave[_n] ==3 & (wave[_n+1] ==5 | wave[_n+1] ==6 | wave[_n+1] ==7));
sort state muni local folio wave;
by state muni local folio: g out4_5 =(wave[_n] ==4 & (wave[_n+1] ==6 | wave[_n+1] ==7));
sort state muni local folio wave;
by state muni local folio: g out5_6 =(wave[_n] ==5 & (wave[_n+1] ==7));

g byte out_in_temp =(out0_2 ==1 | out2_3 ==1 | out3_4 ==1 | out4_5 ==1 | out5_6 ==1);
bys state muni local folio: egen out_in =max(out_in_temp);
label var out_in "(hh) =1 if household leaves the sample and comes back in";

tab out_in panel_balanced_2;
g byte pureat =(panel_balanced_2 ==0 & out_in ==0);
label var pureat "(hh) =1 if household is a pure attriter -never comes back in the sample";

ta pureat if t2_c1_op ~=. & wave ==0;
ta pureat wave if t2_c1_op ~=. & wave ==0, col;
bys t2_c1_op: ta pureat if wave ==0;

reg pureat t2_c1_op if wave ==0, cl(comuid);
reg pureat t1_c2_op if wave ==0, cl(comuid);

*====================================================================================
 Covariates & Dependent Vars at Baseline (wave 0): Attriter Households
*====================================================================================;
keep if pureat ==0;

**Covariates;
postfile nonattriters str20 name obstret treat tstd obscont control cstd tstat using "$out_p/nonattriters.dta", replace;
	foreach var of global cont_d {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post nonattriters  (name) (N1) (coef1) (std1) (N0) (coef0) (std0)  (difttest);
 		drop difttest name;
		scalar drop _all;
	};

	foreach var of global cont_c {;
	gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if `var' >0 & wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if `var' >0 & wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post nonattriters  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

***Characteristics entrepreneur -- use info in wave ==2 (Oct 98) because there is no infomration availabel at baseline;
	foreach var of global cont_ent {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==2 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==2, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post nonattriters  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

***Dependent Variables;
	foreach var of global dep97_d {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post nonattriters  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};

	foreach var of global dep97_c {;
		gen str20 name="`var'" ;
		foreach y in 0 1 {;
			sum `var' if `var' >0 & wave ==0 & t2_c1_op ==`y';
			scalar coef`y' =r(mean);
			scalar std`y' =r(sd);
			scalar N`y' =r(N);
		};
		svymean  `var' if  `var' >0 & wave ==0, obs  by(t2_c1_op);
		svylc [`var']1 - [`var']0;
		gen difttest=r(est)/r(se);
		post nonattriters  (name) (N1) (coef1) (std1) (N0) (coef0) (std0) (difttest);
 		drop difttest name;
		scalar drop _all;
	};
postclose nonattriters;
log close;

