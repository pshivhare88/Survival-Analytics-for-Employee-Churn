/*==================================================================================================*/
/* This is the SAS code file for Project 1 under the survival analysis class for Summer-2017 Semester * 
/* Author of this code : Team 9 * / 
/* Team members are as follows */ 
/* 1. Xueling CHEN, 2. Crystal(Yunong) Liu, 3. Vinit Gupta */ 
/* 4. Rahul Manchanda, 5. Pawan Shivhare 6. Rajarshi Das  */ 
/* We have used the analysis done by this code and put all the details under the project reaport 
document named : Team9-Final-Report-Project1.docx */ 
/* This code is written by the members of team 9 only, we have used class material and 
internet as resourecs for reference only */  
/****************************************************************************************************/
/* With the following statement, we have created our project library */ 

Libname Project1 'P:\FINAL-Project1'; 

/* Data preparation step, creating the sas file from the .CSV file*/
PROC IMPORT OUT=project1.Team9_csv FILE='P:\FINAL-Project1\FermaLogis.csv' DBMS=CSV 
		REPLACE;
RUN;
PROC PRINT data= project1.Team9_csv; 
Title' SAS data file to be used by team 9 for Project 1 Analysis'; 
RUN;  	
/*Adding few more variables based on certain given conditions*/ 
/*These variables will help, our further analysis of the data */ 
DATA project1.Team9_csv;
	SET project1.Team9_csv;

	IF NumCompaniesWorked=0 or NumCompaniesWorked=1 or NumCompaniesWorked=2 or 
		NumCompaniesWorked=3 or NumCompaniesWorked=4 THEN
			NumCompaniesWorked1='Less than or equal to 4 Companies';

	IF NumCompaniesWorked=5 or NumCompaniesWorked=6 or NumCompaniesWorked=7 or 
		NumCompaniesWorked=8 or NumCompaniesWorked=9 THEN
			NumCompaniesWorked1='Greater than 4 Companies';
	NumBonusAwarded=SUM(bonus_1, bonus_2, bonus_3, bonus_4, bonus_5, bonus_6, 
		bonus_7, bonus_8, bonus_9, bonus_10, bonus_11, bonus_12, bonus_13, bonus_14, 
		bonus_15, bonus_16, bonus_17, bonus_18, bonus_19, bonus_20, bonus_21, 
		bonus_22, bonus_23, bonus_24, bonus_25, bonus_26, bonus_27, bonus_28, 
		bonus_29, bonus_30, bonus_31, bonus_32, bonus_33, bonus_34, bonus_35, 
		bonus_36, bonus_37, bonus_38, bonus_39, bonus_40);
	BonusReceivedRatio=NumBonusAwarded/YearsAtCompany;

	IF YearsAtCompany <=3 Then
		Datasplit='YOUNG_EMP';

	IF YearsAtCompany > 3 Then
		Datasplit='Exp_Emp';

	IF Attrition="No" THEN
		IAttrition=0;
	ELSE
		IAttrition=1;

	IF BonusReceivedRatio=. THEN
		BonusReceivedRatio=0;

	IF overtime="No" THEN
		Iovertime=0;
	ELSE
		Iovertime=1;

	IF BusinessTravel="Travel_Frequently" then
		IBusinessTravel=2;
	ELSE IF BusinessTravel="Travel_Rarely" then
		IBusinessTravel=1;
	ELSE IF BusinessTravel="Non-Travel" then
		IBusinessTravel=0;

	IF MaritalStatus='Married' then
		IMaritalStatus=1;
	ELSE IF MaritalStatus='Divorced' then
		IMaritalStatus=0;
	ELSE IF MaritalStatus='Single' then
		IMaritalStatus=2;
RUN;
/* Now, we are creating two separate data files, one for Young Employee and one for 
experienced employee */ 
DATA project1.Young_emp project1.Exp_emp;
	SET project1.Team9_csv;

	If UPCASE(Datasplit)='YOUNG_EMP' THEN
		OUTPUT project1.Young_emp;
	Else if UPCASE(Datasplit)='EXP_EMP' THEN
		output project1.Exp_emp;
RUN;
/* Now we will try to explore the data, to analyze the contribution of different parameters 
towards employee attrition for the Fermalogis company */ 
/* Analyzing contribution of Marital status towards attrition */ 
PROC SGPLOT data=project1.Young_emp;
	VBAR attrition / GROUP=MaritalStatus groupdisplay=cluster;
	TITLE 'Attrition by Marital Status for Young Employees';
RUN;
/* Analyzing contribution of Department towards attrition */ 
PROC SGPLOT data=project1.Young_emp;
	VBAR attrition / GROUP=department groupdisplay=cluster;
	TITLE 'Attrition by department for Young Employees';
RUN;
/*Analyzing Contribution of Jobrole towards Attrition*/
PROC SGPLOT data=project1.Young_emp;
	VBAR attrition / GROUP=jobrole groupdisplay=cluster;
	TITLE 'Attrition by jobrole for Young Employees';
RUN;

/* Analyzing Contribution of Age towards Attrition*/
proc sql ;
	create table rec_tablevisual as select count(employeeNumber) as empCount, Age, 
		Attrition from project1.Young_emp group by Age, Attrition;

PROC SGPLOT DATA=rec_tablevisual;
	SERIES X=Age Y=empCount / group=attrition;
	XAXIS TYPE=DISCRETE;
	XAXIS LABEL='Age of Employee';
	YAXIS LABEL='Count of Employees' VALUES=(0 TO 30 BY 2);
	TITLE 'Attrition rate by Age for Young Employees';
RUN;

PROC SGPLOT DATA=project1.team9_csv;
	VBAR Attrition/ RESPONSE=MonthlyIncome STAT=MEAN;
	Title 'Bar plot for Attrition vs Monthly Income of the employee';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR Attrition/ RESPONSE=MonthlyIncome STAT=MEAN;
	Title 'Bar plot for Attrition vs Monthly Income for Young employees';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR Attrition/ RESPONSE=MonthlyIncome STAT=MEAN;
	Title 'Bar plot for Attrition vs Monthly Income for Experienced employees';
run;

PROC SGPLOT DATA=project1.team9_csv;
	VBAR Attrition/ GROUP=OverTime;
	Title 'Bar plot for Attrition vs Overtime ';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR Attrition/ GROUP=OverTime;
	Title 'Bar plot for Attrition vs Over Time for Young employees';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR Attrition/ GROUP=OverTime;
	Title 'Bar plot for Attrition vs Over Time for Experienced employees';
run;

PROC FREQ DATA=project1.team9_csv;
	TABLES Attrition * OverTime / CHISQ;
	TITLE 'Association of Attrition and OverTime ';
RUN;

PROC FREQ DATA=project1.Young_emp;
	TABLES Attrition * OverTime / CHISQ;
	TITLE 'Association of Attrition and OverTime for Young employees ';
RUN;

PROC FREQ DATA=project1.Exp_emp;
	TABLES Attrition * OverTime / CHISQ;
	TITLE 'Association of Attrition and OverTime for Experienced employee';
RUN;

PROC SGPLOT DATA=project1.team9_csv;
	VBAR Attrition/ GROUP=StockOptionLevel;
	Title 'Bar plot for Attrition vs StockOptionLevel ';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR Attrition/ GROUP=StockOptionLevel;
	Title 'Bar plot for Attrition vs StockOptionLevel for Young employee ';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR Attrition/ GROUP=StockOptionLevel;
	Title 'Bar plot for Attrition vs StockOptionLevel for Experienced employee ';
run;

PROC FREQ DATA=project1.team9_csv;
	TABLES Attrition * StockOptionLevel / CHISQ;
	TITLE 'Association of Attrition and Stock Option Level ';
RUN;

PROC FREQ DATA=project1.Young_emp;
	TABLES Attrition * StockOptionLevel / CHISQ;
	TITLE 'Association of Attrition and StockOptionLevel for Young employees ';
RUN;

PROC FREQ DATA=project1.Exp_emp;
	TABLES Attrition * StockOptionLevel / CHISQ;
	TITLE 
		'Association of Attrition and StockOptionLevel for Experienced employees';
RUN;

PROC SGPLOT DATA=project1.team9_csv;
	VBAR Attrition/ GROUP=BusinessTravel;
	Title 'Bar plot for Attrition vs Business Travel ';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR Attrition/ GROUP=BusinessTravel;
	Title 'Bar plot for Attrition vs BusinessTravel for Young employees ';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR Attrition/ GROUP=BusinessTravel;
	Title 'Bar plot for Attrition vs BusinessTravel for Experienced employees ';
run;

PROC FREQ DATA=project1.team9_csv;
	TABLES Attrition * BusinessTravel / CHISQ;
	TITLE 'Association of Attrition and BusinessTravel ';
RUN;

PROC FREQ DATA=project1.Young_emp;
	TABLES Attrition * BusinessTravel / CHISQ;
	TITLE 'Association of Attrition and BusinessTravel for Young employees ';
RUN;

PROC FREQ DATA=project1.Exp_emp;
	TABLES Attrition * BusinessTravel / CHISQ;
	TITLE 'Association of Attrition and BusinessTravel for Experienced employees';
RUN;

PROC FREQ DATA=project1.team9_csv;
	TABLES Attrition * EnvironmentSatisfaction / CHISQ;
	TITLE 'Association of Attrition and EnvironmentSatisfaction ';
RUN;

PROC FREQ DATA=project1.Young_emp;
	TABLES Attrition * EnvironmentSatisfaction / CHISQ;
	TITLE 
		'Association of Attrition and EnvironmentSatisfaction for Young employees ';
RUN;

PROC FREQ DATA=project1.Exp_emp;
	TABLES Attrition * EnvironmentSatisfaction / CHISQ;
	TITLE 'Association of Attrition and EnvironmentSatisfaction for Experienced emmployees ';
RUN;

PROC FREQ DATA=project1.team9_csv;
	TABLES Attrition * JobSatisfaction / CHISQ;
	TITLE 'Association of Attrition and JobSatisfaction ';
RUN;

PROC FREQ DATA=project1.Young_emp;
	TABLES Attrition * JobSatisfaction / CHISQ;
	TITLE 'Association of Attrition and JobSatisfaction for Young emplloyees';
RUN;

PROC FREQ DATA=project1.Exp_emp;
	TABLES Attrition * JobSatisfaction / CHISQ;
	TITLE 'Association of Attrition and JobSatisfaction for Experienced employees';
RUN;

PROC CORR DATA=project1.team9_csv PLOTS=scatter;
	VAR YearsAtCompany;
	WITH MonthlyIncome;
	TITLE 'Correlations for MonthlyIncome with YearsAtCompany';
RUN;

PROC CORR DATA=project1.Young_emp PLOTS=scatter;
	VAR YearsAtCompany;
	WITH MonthlyIncome;
	TITLE 'Correlations for MonthlyIncome with YearsAtCompany for Young employees';
RUN;

PROC CORR DATA=project1.Exp_emp PLOTS=scatter;
	VAR YearsAtCompany;
	WITH MonthlyIncome;
	TITLE 
		'Correlations for MonthlyIncome with YearsAtCompany for Experienced employees';
RUN;

PROC anova data=project1.team9_csv;
	CLASS Attrition;
	MODEL MonthlyIncome=Attrition;
	MEANS Attrition/SCHEFFE;
	TITLE 'MonthlyIncome based on Attrition';

PROC anova data=project1.Young_emp;
	CLASS Attrition;
	MODEL MonthlyIncome=Attrition;
	MEANS Attrition/SCHEFFE;
	TITLE 'MonthlyIncome based on Attrition for Young employees';

PROC anova data=project1.Exp_emp;
	CLASS Attrition;
	MODEL MonthlyIncome=Attrition;
	MEANS Attrition/SCHEFFE;
	TITLE 'MonthlyIncome based on Attrition for Experienced employees';

PROC SGPLOT DATA=project1.team9_csv;
	VBAR NumCompaniesWorked1/ GROUP=Attrition GROUPDISPLAY=cluster STAT=percent;
	Title 'Bar plot for Attrition vs Num of CompaniesWorked ';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR NumCompaniesWorked1/ GROUP=Attrition GROUPDISPLAY=cluster STAT=percent;
	Title 'Bar plot for Attrition vs Num of Companies Worked for Young Employees';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR NumCompaniesWorked1/ GROUP=Attrition GROUPDISPLAY=cluster STAT=percent;
	Title 
		'Bar plot for Attrition vs Num of Companies Worked for Experienced employees ';
run;

PROC FREQ DATA=project1.team9_csv;
	TABLES Attrition * NumCompaniesWorked1 / CHISQ;
	TITLE 'Association of Attrition and Num of CompaniesWorked ';
RUN;

PROC FREQ DATA=project1.Young_emp;
	TABLES Attrition * NumCompaniesWorked1 / CHISQ;
	TITLE 'Association of Attrition and Num of CompaniesWorked for Young Employees ';
RUN;

PROC FREQ DATA=project1.Exp_emp;
	TABLES Attrition * NumCompaniesWorked1 / CHISQ;
	TITLE 
		'Association of Attrition and Num of CompaniesWorked for Experienced employees';
RUN;

PROC SGPLOT DATA=project1.team9_csv;
	VBAR Attrition/ Response=NumBonusAwarded STAT=Mean;
	Title 'Bar plot for Attrition vs Num of BonusAwarded ';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR Attrition/ Response=NumBonusAwarded STAT=Mean;
	Title 'Bar plot for Attrition vs Num of BonusAwarded for Youung Employees ';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR Attrition/ Response=NumBonusAwarded STAT=Mean;
	Title 'Bar plot for Attrition vs Num of BonusAwarded for Experienced Employees ';
run;

PROC SGPLOT DATA=project1.team9_csv;
	VBAR Attrition/ Response=DistanceFromHome STAT=Mean;
	Title 'Bar plot for Attrition vs DistanceFromHome ';
run;

PROC SGPLOT DATA=project1.Young_emp;
	VBAR Attrition/ Response=DistanceFromHome STAT=Mean;
	Title 'Bar plot for Attrition vs DistanceFromHome for Young Employees ';
run;

PROC SGPLOT DATA=project1.Exp_emp;
	VBAR Attrition/ Response=DistanceFromHome STAT=Mean;
	Title 'Bar plot for Attrition vs DistanceFromHome for Experienced Employees ';
run;
/* In the following sections, we are working different Survival Plots for 
further analysis of the data from survival perspective */ 
/* In the survival plots as well, we have categorized the data at different level 
to closely analyze our data */ 
/*Surival plots for employees till 3years of Tenure */
DATA project1.project1.rec_upto3 ;
	set project1.Young_emp;
	IF Iattrition=0 then
		YearsAtCompany=4;
RUN; 

proc lifetest data=project1.rec_upto3  method=life intervals=0 1 2 3 4 plots=(s, h);
	time YearsAtCompany*IAttrition(0);
	strata overtime;
	test StockOptionLevel monthlyrate TotalWorkingYears EnvironmentSatisfaction 
		age DistanceFromHome IBusinessTravel IMaritalStatus NumCompaniesWorked 
		JobSatisfaction HourlyRate;
	title 'Survival plot for employees with 3 years of Tenure';
run;
/**************************************************************************************************/
/*Surival plots for employees with greater than 3years of Tenure */
proc lifetest data=project1.Exp_emp method=life intervals=4 10 20 30 40 
		plots=(s, h);
	time YearsAtCompany*IAttrition(0);
	strata overtime;
	test StockOptionLevel monthlyrate TotalWorkingYears EnvironmentSatisfaction 
		age DistanceFromHome IBusinessTravel IMaritalStatus NumCompaniesWorked 
		JobSatisfaction HourlyRate;
run;

/*Hazard plots for both the categories (YearsAtcompany <= 3 and > 3) */
proc lifetest data=project1.rec_upto3  method=life intervals=0 1 2 3 4 plots=(h);
	time YearsAtCompany*IAttrition(0);
run;

proc lifetest data=project1.Exp_emp method=life intervals=4 10 20 30 40 
		plots=(h);
	time YearsAtCompany*IAttrition(0);
run;

/* Strata plot for gender based analysis */
proc lifetest data=project1.rec_upto3  method=life intervals=0 1 2 3 4 plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata gender;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

proc lifetest data=project1.Exp_emp method=life intervals=4 10 20 30 40 
		plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata gender;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

/* Strata plot for Overtime*/
proc lifetest data=project1.rec_upto3  method=life intervals=0 1 2 3 4 plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata Overtime;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

proc lifetest data=project1.Exp_emp method=life intervals=4 10 20 30 40 
		plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata Overtime;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

/* Strata plot for Marital Status*/
proc lifetest data=project1.rec_upto3  method=life intervals=0 1 2 3 4 plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata MaritalStatus;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

proc lifetest data=project1.Exp_emp method=life intervals=4 10 20 30 40 
		plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata MaritalStatus;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

/* Strata plot for Business Travel*/
proc lifetest data=project1.rec_upto3  method=life intervals=0 1 2 3 4 plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata BusinessTravel;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

proc lifetest data=project1.Exp_emp method=life intervals=4 10 20 30 40 
		plots=(s);
	time YearsAtCompany*IAttrition(0);
	strata BusinessTravel;
	test StockOptionLevel TotalWorkingYears EnvironmentSatisfaction age 
		DistanceFromHome IBusinessTravel IMaritalStatus JobSatisfaction;
run;

/* Now we are working on analyzing the data running various statistical models 
available for each section and categories of employees (like for example, Young vs. experienced
and also if young, we are running models for employees having less than 3 years of tenure */  

***Analysis of Young Employees;
**Start of LogNormal Model;
*LogNormal for less than 3 years for null;

PROC LIFEREG DATA=project1.Young_emp;
	MODEL YearsAtCompany*IAttrition(0)= /DISTRIBUTION=lnormal;
RUN;

*LogNormal for less than 3 years;

PROC LIFEREG DATA=project1.Young_emp;
	CLASS overtime StockOptionLevel BusinessTravel EnvironmentSatisfaction 
		Department JobSatisfaction WorkLifeBalance;
	MODEL YearsAtCompany*IAttrition(0)=Age distancefromhome overtime 
		StockOptionLevel BusinessTravel EnvironmentSatisfaction Department 
		JobSatisfaction MonthlyIncome NumCompaniesWorked TotalWorkingYears 
		WorkLifeBalance YearsInCurrentRole YearsWithCurrManager BonusReceivedRatio 
		/DISTRIBUTION=lnormal;
RUN;

*Find Loglikelihood statistic for LogNormal;

DATA project1.calculateLogRatioForLogNormal;
	L_null=-246.68;
	L_full=-42.28;
	L=2 * ABS(L_full - L_null);
	p_value=1 - probchi(L, 14);
RUN;

PROC PRINT DATA=project1.calculateLogRatioForLogNormal;
RUN;

**End of LogNormal Model;
**Start of Exponential Model;
*Exponential Model for less than 3 years;

PROC LIFEREG DATA=project1.Young_emp;
	CLASS overtime StockOptionLevel BusinessTravel EnvironmentSatisfaction 
		Department JobSatisfaction WorkLifeBalance;
	MODEL YearsAtCompany*IAttrition(0)=Age distancefromhome overtime 
		StockOptionLevel BusinessTravel EnvironmentSatisfaction Department 
		JobSatisfaction MonthlyIncome NumCompaniesWorked WorkLifeBalance 
		YearsInCurrentRole YearsWithCurrManager /DISTRIBUTION=exponential;
RUN;

*Exponential for less than 3 years for null;

PROC LIFEREG DATA=project1.Young_emp;
	MODEL YearsAtCompany*IAttrition(0)= /DISTRIBUTION=exponential;
RUN;

*Find Loglikelihood statistic for Exponential;

DATA project1.calculateLogRatioForExponential;
	L_null=-280.742;
	L_full=-188.267;
	L=2 * ABS(L_full - L_null);
	p_value=1 - probchi(L, 12);
RUN;

PROC PRINT DATA=project1.calculateLogRatioForExponential;
RUN;

**End of Exponential model;
**Start of Weibull Model;
*Weibull for less than 3 years;

PROC LIFEREG DATA=project1.Young_emp;
	CLASS overtime StockOptionLevel BusinessTravel EnvironmentSatisfaction 
		Department JobSatisfaction WorkLifeBalance;
	MODEL YearsAtCompany*IAttrition(0)=distancefromhome monthlyrate overtime 
		StockOptionLevel BusinessTravel EnvironmentSatisfaction Department 
		JobSatisfaction MonthlyIncome NumCompaniesWorked TotalWorkingYears 
		WorkLifeBalance YearsInCurrentRole YearsWithCurrManager BonusReceivedRatio 
		/DISTRIBUTION=weibull;
RUN;

*Weibull for less than 3 years for null;

PROC LIFEREG DATA=project1.Young_emp;
	MODEL YearsAtCompany*IAttrition(0)=/DISTRIBUTION=weibull;
RUN;

*Find Loglikelihood statistic for Weibull;

DATA project1.calculateLogRatioForWeibull;
	L_null=-253.164;
	L_full=-44.656;
	L=2 * ABS(L_full - L_null);
	p_value=1 - probchi(L, 14);
RUN;

PROC PRINT DATA=project1.calculateLogRatioForWeibull;
RUN;

**End of Weibull Model

**Comparing models that are created;

DATA project1.CompareModels_young;
	L_exponential=-188.26;
	L_weibull=-44.65;
	L_lognormal=-42.28;
	LRTEW=-2*(L_exponential - L_weibull);
	LRTLW=-2*(L_lognormal - L_weibull);
	LRTLE=-2*(L_lognormal - L_exponential);
	p_valueEW=1 - probchi(LRTEW, 1);
	p_valueLW=1 - probchi(LRTLW, 1);
	p_valueLE=1 - probchi(LRTLE, 2);
RUN;

PROC PRINT DATA=project1.CompareModels_young;
RUN;

***Analysis of Experienced Employees

**Start of LogNormal Model;
* Null Model for lnormal for Experienced Employees;

PROC LIFEREG DATA=project1.Exp_emp;
	*null model to test null hypothesis;
	MODEL YearsAtCompany*IAttrition(0)= / D=lnormal;

	/*Loglikelihood of null hypothesis*/
RUN;

*lnormal for Experienced Employees;

PROC LIFEREG DATA=project1.Exp_emp;
	CLASS overtime StockOptionLevel BusinessTravel EnvironmentSatisfaction 
		Department Education EducationField JobInvolvement JobLevel JobSatisfaction 
		RelationshipSatisfaction;
	MODEL YearsAtCompany*IAttrition(0)=distancefromhome overtime StockOptionLevel 
		BusinessTravel EnvironmentSatisfaction Department EducationField 
		JobInvolvement JobLevel JobSatisfaction NumCompaniesWorked 
		RelationshipSatisfaction TrainingTimesLastYear YearsInCurrentRole 
		YearsWithCurrManager /DISTRIBUTION=lnormal;
RUN;

*Log ratio for lnormal for Experienced Employees;

DATA project1.calculateLogRatio_exp;
	L_null=-355.59;
	L_full=-156.37;
	L=2 * ABS(L_full - L_null);
	p_value=1 - probchi(L, 14);
RUN;

PROC PRINT data=project1.calculateLogRatio_exp;
RUN;

**End of LogNormal Model;
**Start of Weibull Model;
* Null Model for weibull for Experienced Employees;

PROC LIFEREG DATA=project1.Exp_emp;
	*null model to test null hypothesis;
	MODEL YearsAtCompany*IAttrition(0)= / D=weibull;

	/*Loglikelihood of null hypothesis*/
RUN;

*weibull for Experienced Employees;

PROC LIFEREG DATA=project1.Exp_emp;
	CLASS overtime BusinessTravel Department EducationField JobInvolvement 
		JobLevel JobRole MaritalStatus RelationshipSatisfaction 
		EnvironmentSatisfaction;
	MODEL YearsAtCompany*IAttrition(0)=distancefromhome overtime BusinessTravel 
		Department EducationField JobInvolvement JobLevel MaritalStatus 
		NumCompaniesWorked TotalWorkingYears TrainingTimesLastYear YearsInCurrentRole 
		YearsWithCurrManager BonusReceivedRatio EnvironmentSatisfaction 
		/DISTRIBUTION=weibull;
RUN;

*Log ratio for weibull for Experienced Employees;

DATA project1.calculateLogRatio_weibull_exp;
	L_null=-366.97;
	L_full=-145.9409387;
	L=2 * ABS(L_full - L_null);
	p_value=1 - probchi(L, 14);
RUN;

PROC PRINT data=project1.calculateLogRatio_weibull_exp;
RUN;

**End of Weibull Model;
**Start of Exponential Model;
* Null Model for exponential for Experienced Employees;

PROC LIFEREG DATA=project1.Exp_emp;
	*null model to test null hypothesis;
	MODEL YearsAtCompany*IAttrition(0)= / D=exponential;

	/*Loglikelihood of null hypothesis*/
RUN;

*exponential for Experienced Employees;

PROC LIFEREG DATA=project1.Exp_emp;
	CLASS overtime StockOptionLevel BusinessTravel EnvironmentSatisfaction 
		Department Education EducationField JobInvolvement JobLevel JobSatisfaction 
		WorkLifeBalance;
	MODEL YearsAtCompany*IAttrition(0)=distancefromhome overtime StockOptionLevel 
		BusinessTravel EnvironmentSatisfaction Department EducationField 
		JobInvolvement JobLevel JobSatisfaction NumCompaniesWorked 
		RelationshipSatisfaction TrainingTimesLastYear WorkLifeBalance 
		YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager 
		/DISTRIBUTION=exponential;
RUN;

*Log ratio for exponential for Experienced Employees;

DATA project1.calculateLogRatio_exponen_exp;
	L_null=-388.25;
	L_full=-280.83;
	L=2 * ABS(L_full - L_null);
	p_value=1 - probchi(L, 16);
RUN;

PROC PRINT data=project1.calculateLogRatio_exponen_exp;
RUN;

**End of Exponential Model;
**Comparing models that are created;

DATA Project1.CompareModels_exp;
	L_exponential=-280.83;
	L_weibull=-145.9409387;
	L_lognormal=-156.37;
	LRTEW=-2*(L_exponential - L_weibull);
	LRTLW=-2*(L_lognormal - L_weibull);
	LRTLE=-2*(L_lognormal - L_exponential);
	p_valueEW=1 - probchi(LRTEW, 1);
	p_valueLW=1 - probchi(LRTLW, 1);
	p_valueLE=1 - probchi(LRTLE, 2);
RUN;

PROC PRINT DATA=Project1.CompareModels_exp;
RUN;

