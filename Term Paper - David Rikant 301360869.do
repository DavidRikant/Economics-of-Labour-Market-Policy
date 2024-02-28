******************************************************************************
* Term Paper
* Author: David Rikant
* Class: ECON 480 D100
* Date: Fall 2023
******************************************************************************

********
* TOPIC
********

* Do married people in Canada earn more than those who are not married? What factors account for this difference?

********
* SETUP
********

* Making sure no log files are already open
cap log close

* Making sure no other data/macro variables are in memory
clear all

* Creating macro-variables for directory and code
global data "/Users/davidrikant/Downloads/School/ECON 480 - Fall 2023"
global code "/Users/davidrikant/Downloads/School/ECON 480 - Fall 2023"

* Creating a new log called "Term Paper.log"
log using "$code/Term Paper - David Rikant 301360869.log", replace

* Reading in the 2021 Census Individual PUMF data
use "$data/ipumf_2021_final_en.dta", clear

****************************************
* CREATING SAMPLE OF INTEREST VARIABLES
****************************************

* Creating restrictions for the sample:
keep if agegrp >= 8 & agegrp <= 16 & ///
	hdgree < 88 & ///
	cow < 9 & ///
	fptwk < 9 & ///
	marsth < 8 & ///
	empin > 0 & empin < 88888888

* Transforming employment income variable into a logarithm:
gen logincome = ln(empin)

* Defining the agegrp term, each value is associated with a 5 year period beginning at 20 and ending at 64 years of age. All NA values have been removed prior in the restrictions command.
tab agegrp

* Creating region variables using the province PR variable, seperating them by their respective zones
gen West = (pr == 59 | pr == 48 | pr == 47)
gen North = pr == 70
gen Center = (pr == 46 | pr == 35 | pr == 24)
gen East = (pr == 10 | pr == 11 | pr == 12 | pr == 13)
global region "West North East"

* In addition we are going to include the gender variable as a new variable "Gender":
gen Gender = (gender == 1)
label variable Gender "dummy gender = 1 if Woman+"
label define GEN_value 0 "Man+" 1 "Woman+"
label values Gender GEN_value

* Next, we will be including educational attainment. There are N/A values present within the "hdgree" variable labelled "88 and 99", which will be removed. Additionally, only a few educational attainments will be used in this sample.

gen NoCDoD = (hdgree == 1) // no certificate, diploma, or degree
label variable NoCDoD "dummy no education = 1 if received no certificate, diploma, or degree"
label define nocdod_value 0 "Other" 1 "no certificate, diploma, or degree"
label values NoCDoD nocdod_value

gen highschool = (hdgree == 2) // highschool diploma
label variable highschool "dummy highschool education = 1 if completed highschool"
label define highschool_value 0 "Other" 1 "High (secondary) school diploma or equivalency certificate"
label values highschool highschool_value

gen somecollege = (hdgree == 5 | hdgree == 6 | hdgree == 7) // attained some college
label variable somecollege "dummy college education = 1 if recieved some college education"
label define somecollege_value 0 "Other" 1 "Program of minimum 3 months to more than 2 years, from College, CEGEP, or other non-university certificates or diplomas"
label values somecollege somecollege_value

gen bachelors = (hdgree == 9) // Bachelor's degree
label variable bachelors "dummy bachelor education = 1 if completed an bachelor's degree"
label define bachelors_value 0 "Other" 1 "Bachelor's degree"
label values bachelors bachelors_value

gen masters = (hdgree == 12) // Master's degree
label variable masters "dummy masters education = 1 if completed a Master's degree"
label define masters_value 0 "Other" 1 "Master's degree"
label values masters masters_value

* Grouping together the education dummy variables
global education "NoCDoD highschool somecollege bachelors masters"

* Creating a dummy variable for class of worker "COW" to specify that I am only interested in the Employee variable:
gen cow_emp = (cow == 1) // Idetifies as an Employee
label variable cow_emp "dummy employee class of worker = 1 if identifies as an employee"
label define cow_emp_value 0 "Other" 1 "Employee"
label values cow_emp cow_emp_value

* The primary variable of interest is marital status, specifically whether an individual is married or not married which will be a new variable named "Marstat":
cap gen Married = 0 // Not married
replace Married = 1 if (marsth == 2) // Married

**********************************************************************
* ESTIMATED AVERAGE EARNINGS IN EACH EDUCATION TYPE BY MARITAL STATUS
**********************************************************************

bys Married: summ  logincome if (NoCDoD & agegrp & Gender & cow_emp) [aw = weight]

bys Married: summ  logincome if (highschool & agegrp & Gender & cow_emp) [aw = weight]

bys Married: summ  logincome if (somecollege & agegrp & Gender & cow_emp) [aw = weight]

bys Married: summ  logincome if (bachelors & agegrp & Gender & cow_emp) [aw = weight]

bys Married: summ  logincome if (masters & agegrp & Gender & cow_emp) [aw = weight]

*************************
* DESCRIPTIVE STATISTICS
*************************

* Using and installing asdoc to create formatted Word document tables for the descriptive statistics
cap ssc install asdoc

* NON-weighted descriptive statistics estimated on the sample 
asdoc summ logincome Married Gender cow_emp $education $region i.agegrp

* Weighted descriptive statistics estimated on the sample 
asdoc summ logincome Married Gender cow_emp $education $region i.agegrp [aw = weight]

****************************************
* ESTIMATING MULTIPLE LINEAR REGRESSION
****************************************

* Regression of the specified model
xi: reg logincome Married Gender cow_emp $education $region i.agegrp [aw = weight], r
outreg2 using reg_table_1.doc, replace ctitle(General Model) label(insert) sdec (4)addnote (Notes: This regression table is adjusted via heteroskedasticity-robust and has been weighted by an average weight. Also note that the variables _Iagegrp_8-16 are the same dummy variables as the ones present in the descriptive statistics tables in part a.)

* Creating global macro-variables for the independent variable (Y), and the dependent variables (X), dependent variables without married (mar_X), and dependent variables without Gender (gen_X)
global Y "logincome"
global X "Married Gender cow_emp $education $region _I*"
global mar_X "Gender cow_emp $education $region _I*" // without Married variable
global gen_X "Married cow_emp $education $region _I*" // without Gender variable

* Regression of logincome by marital status
bys Married: outreg2 using reg_table_2.doc, replace: reg $Y $mar_X, robust

* Regression of logincome by gender
bys Gender: outreg2 using reg_table_3.doc, replace: reg $Y $gen_X, robust

* Regression of logincome using mybswreg using bootstrap weights
mybswreg $Y $X [aw = weight], cmd(regress) bsweights(wt*) cmeanbs(50) level(95)
cap outreg2 using reg_table_4.doc, replace ctitle(Mybswreg Model) label

* Note, that due to mybswreg is an imported code function, it does not format properly into a Word document, therefore, the table was copied and pasted from the results command window of STATA directly into the Word document. Minor manual adjustments were made as a result.

*******************************
* OAXACA-BLINDER DECOMPOSITION
*******************************

* installing the oaxaca package
cap ssc install oaxaca 

* Running the more generalized and simpler decomposition
outreg2 using obd_table_1.doc, replace: oaxaca $Y $mar_X, by(Married)

* Running a more broken down version of the decomposition
outreg2 using obd_table_2.doc, replace: oaxaca logincome Gender cow_emp (region: $region) (age: _I*) (education: $education), by(Married)

******************
* SAVING THE DATA
******************

global out "$data"
cap save "$out/Term Paper - David Rikant 301360869.dta", replace
cap log close
