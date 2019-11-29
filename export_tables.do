*export to excel;

cd "$out_p"
dir


/*program 1
investments_des.do
Creates: 
   5.9k  11/29/19 13:15  aprices.dta       
   5.7k  11/29/19 13:15  cwages.dta        
   7.2k  11/29/19 13:15  eligibles.dta     
   7.2k  11/29/19 13:15  ineligibles.dta   
   7.2k  11/29/19 13:16  nonattriters.dta  

Table 1 
Table A2 
Table A5

Summary Statistics for Eligible and Ineligible Households
Macro Income and Price Effects (Animal Prices and Community Wages)
Sample Sizes and Analysis of Attrition*/

use eligibles, clear
       export excel using si-cct-MainTables, sheet("Table1-Baseline") sheetreplace
use aprices, clear
       export excel using si-cct-Appendices, sheet("TableA2-IncomePriceAnimal") sheetreplace
use cwages, clear
       export excel using si-cct-Appendices, sheet("TableA2-IncomePriceWages") sheetreplace
use nonattriters, clear
       export excel using si-cct-Appendices, sheet("nonattriters") sheetreplace
use ineligibles, clear
       export excel using si-cct-Appendices, sheet("ineligibles") sheetreplace
  
	   
/* program 2
investments.do
Creates:

   2.0k  11/29/19 14:22  main.text
   3.1k  11/29/19 14:22  time.text  

Table 2, Panel A
Table 3 
Table A6 

Impact of OPORTUNIDADES on Agricultural Assets and Micro-Enterprise Activities over Experimental Period
Eligible and Ineligible Households*/

*I'm copying these into the spreadsheet by hand;

/* program 3
investments_long_term.do
Creates: 
	wave7.text

Table 2,
Panels B and C
Long-Term Impact of OPORTUNIDADES on Agricultural Assets (Nov 2003 data)
*/
*I'm copying into the spreadsheet by hand;

/* program 4
production_income_credit.do
Creates:
	supporting.text
	
Table 4
Table 6
Table A3
Table A7
OPORTUNIDADES Impact on Agricultural Production, Agricultural Income and Credit for Eligible and Ineligible Households
*/

/* program 5
consumption_income_long_term.do
Creates:
	consumo_w7.text
Table 5
Table A4
Table A8
OPORTUNIDADES Impact on Long Term Consumption and Income Sources, including Sensitivity to Trimming Outliers (Nov 2003 data)
*/
*I'm copying into the spreadsheet by hand;

/* program 6
adult_health_long_term.do
Creates:
	adl_w7.text
Table 7
OPORTUNIDADES Impact on Long Term Health of Prime Age Adults (Nov 2003 data)
*/
*I'm copying into the spreadsheet by hand;

/* program 7
consumption.do
Table 8
Table 10
Table A9
Table A10
OPORTUNIDADES Impact on Consumption and MPC Over Experimental Period
MPC and MIE Estimates, including Sensitivity to Trimming Outliers
*/


/*
INTERVENTION TIMING AND DATA WAVES
Wave 0
October 1997
Baseline
Wave 2
October 1998
Experimental Variation
Treatment households start receiving benefits in March/April 1998
Wave 3
May 1999
Experimental Variation
Wave 4
November 1999
Experimental Variation
Control households start receiving benefits in November/December 1999
Wave 5
May 2000
Wave 6
November 2000
Wave 7
November 2003
Long Term Effects*/
