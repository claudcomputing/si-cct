*export to excel;


dir
gl data		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\dta"
gl code		"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\code\si-cct"
gl out_p	"C:\Users\Claud\Box\A3SR Social Impact\si_cct\2010-0343_data_modified4class\output"

cd "$out_p"
/*program 1
investments_des.do
Creates: 
	aprices.dta       
	cwages.dta        
	eligibles.dta     
	ineligibles.dta   
	nonattriters.dta  

Table 1 
Table A2 
Table A5

Summary Statistics for Eligible and Ineligible Households
Macro Income and Price Effects (Animal Prices and Community Wages)
Sample Sizes and Analysis of Attrition*/

use eligibles, clear
       export excel using si-cct-MainTables, sheet("Table1-Baseline") firstrow(variables) sheetreplace 
use aprices, clear
       export excel using si-cct-MainTables, sheet("TableA2-IncomePriceAnimal") firstrow(variables) sheetreplace 
use cwages, clear
       export excel using si-cct-MainTables, sheet("TableA2-IncomePriceWages") firstrow(variables) sheetreplace 
use nonattriters, clear
       export excel using si-cct-MainTables, sheet("nonattriters") firstrow(variables) sheetreplace 
use ineligibles, clear
       export excel using si-cct-MainTables, sheet("ineligibles") firstrow(variables) sheetreplace 
  
	   
/* program 2
investments.do
Creates:
	main.text
	time.text  

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
Creates:
	consumo_app.txt
	consumo_pp_ae2.txt
Table 8
Table 10
Table A9
Table A10
OPORTUNIDADES Impact on Consumption and MPC Over Experimental Period
MPC and MIE Estimates, including Sensitivity to Trimming Outliers
*/
*I'm copying into the spreadsheet by hand;
