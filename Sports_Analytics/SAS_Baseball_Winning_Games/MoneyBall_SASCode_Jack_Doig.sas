DATA moneyballdata;
 SET '/sscc/home/j/jsd417/411Project1/moneyball.sas7bdat';
RUN;
DATA moneyballdata2;
 SET '/sscc/home/j/jsd417/411Project1/moneyball.sas7bdat';
RUN;
DATA moneyballdata_FINALTEST;
 SET '/sscc/home/j/jsd417/411Project1/moneyball_test.sas7bdat';
RUN;
data moneyballdata2;
  	modify moneyballdata2;

*this data set was used in EDA, seaparate from thoe other one from fear of corrupting it

*if TEAM_BATTING_HR= 0 then remove;
*if TEAM_BASERUN_SB= 0 then remove;
*if TEAM_BASERUN_SB = 0 OR TEAM_BATTING_SB='.' then remove;
*if team_pitching_H > 20000 then remove;
*if team_pitching_HR = 0 OR team_pitching_HR='.' then remove;
*if TEAM_PITCHING_BB >2000  then remove;
*if TEAM_PITCHING_SO = 0 OR TEAM_PITCHING_SO='.' or TEAM_PITCHING_SO > 3000 then remove;

run;
/*
*EXPLORATION OF DATA
**************************************

PROC CORR NOPROB NOSIMPLE data=moneyballdata;
var TARGET_WINS 	TEAM_BATTING_H 	TEAM_BATTING_2B 	TEAM_BATTING_3B 	TEAM_BATTING_HR 	TEAM_BATTING_BB 	TEAM_BATTING_SO 	TEAM_BASERUN_SB 	TEAM_BASERUN_CS 	TEAM_BATTING_HBP 	TEAM_PITCHING_H 	TEAM_PITCHING_HR 	TEAM_PITCHING_BB 	TEAM_PITCHING_SO 	TEAM_FIELDING_E 	TEAM_FIELDING_DP ;
run;
  
ods graphics on;
PROC SGSCATTER DATA=moneyballdata;
compare  X=TEAM_BATTING_H Y=TEAM_BATTING_2B; 
run;
ods graphics off;

ods graphics on;
PROC SGSCATTER DATA=moneyballdata;
compare  X=TEAM_BATTING_H Y=TEAM_BATTING_3B; 
run;
ods graphics off;

ods graphics on;
PROC SGSCATTER DATA=moneyballdata;
compare  X=TEAM_BASERUN_SB Y=TEAM_BASERUN_CS; 
run;

ods graphics on;
PROC SGSCATTER DATA=moneyballdata;
compare  X=TEAM_PITCHING_H Y=TEAM_PITCHING_HR; 
run;

*/

/*VARIABLE CORRECTION	
***********************************
*delete three rows from the dataset
*/

data moneyballdata;
  	modify moneyballdata;
   if index=1347 then remove;
   if index=2486 then remove;
   if index=1494 then remove;
run;

/*doing a regression to find realistic values for TEAM_BATTING_3b

PROC REG data=moneyballdata2;
model TEAM_BATTING_3B = TEAM_BATTING_SO;
run;
*/

data moneyballdata;
modify moneyballdata;
if TEAM_BATTING_3B= 0 OR TEAM_BATTING_3B>200  then TEAM_BATTING_3B = 110.70624 -0.07636*TEAM_BATTING_SO;
run;

/*doing a regression to find realistic values for TEAM_BATTING_SO
PROC REG data=moneyballdata2;
model TEAM_BATTING_SO = TEAM_BATTING_HR;
run;
*/

data moneyballdata;
modify moneyballdata;
if TEAM_BATTING_SO= 0 OR TEAM_BATTING_SO='.' then TEAM_BATTING_SO = 427.67277 + 2.99557*TEAM_BATTING_HR;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_BASERUN_SB = TEAM_BATTING_3B;
run;
*/

data moneyballdata;
modify moneyballdata;
if TEAM_BASERUN_SB = 0 OR TEAM_BASERUN_SB ='.' then TEAM_BASERUN_SB = 31.77479  + 1.73187*TEAM_BATTING_3B;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_BASERUN_CS = TEAM_BASERUN_SB;
run;
*/

data moneyballdata;
modify moneyballdata;
if TEAM_BASERUN_CS = 0 OR TEAM_BASERUN_CS ='.' then TEAM_BASERUN_CS = 20.50346 	+  0.33767*TEAM_BASERUN_SB;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_PITCHING_H = TEAM_FIELDING_E;
run;
*/

data moneyballdata;
modify moneyballdata;
if  TEAM_PITCHING_H > 25000 then  TEAM_PITCHING_H =  	890.03009 +  3.50844 *TEAM_FIELDING_E;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_PITCHING_HR = TEAM_BATTING_HR;
run;
*/

data moneyballdata;
modify moneyballdata;
if team_pitching_HR = 0  then   TEAM_PITCHING_HR =  8.13728 + 0.97995*TEAM_BATTING_HR;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_PITCHING_BB = TEAM_BATTING_BB;
run;
*/

data moneyballdata;
modify moneyballdata;
if TEAM_PITCHING_BB >2000   then   TEAM_PITCHING_BB =  186.42785 	 + 0.72062*TEAM_BATTING_BB;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_PITCHING_SO = TEAM_PITCHING_BB;
run;
*/

data moneyballdata;
modify moneyballdata;
if TEAM_PITCHING_SO = 0 OR TEAM_PITCHING_SO='.' or TEAM_PITCHING_SO > 3000   then   TEAM_PITCHING_SO =  724.36459 + 0.14713*TEAM_PITCHING_BB;
run;

/*
PROC REG data=moneyballdata2;
model TEAM_FIELDING_DP = TEAM_FIELDING_E;
run;

proc univariate data=moneyballdata;
      var TEAM_BATTING_HBP	;
      title "";
      histogram;
run;
*/



data moneyballdata;
modify moneyballdata;
if TEAM_FIELDING_DP = 0 OR TEAM_FIELDING_DP='.'  then  TEAM_FIELDING_DP =  max(165.81621 -0.10542*TEAM_FIELDING_E,0);

if  TEAM_BATTING_HBP = '.' then  TEAM_BATTING_HBP = 29+ (95-29)*RAND('NORMAL',0.5,0.16);
if  TEAM_BATTING_HBP < 29 then  TEAM_BATTING_HBP =29;
if  TEAM_BATTING_HBP > 95 then  TEAM_BATTING_HBP =95;
/*no need for the index going fwd*/
DROP index;
run;

/*More EDA

proc univariate data=moneyballdata;
      var TEAM_BATTING_HBP	;
      title "";
      histogram;
run;


*/

data moneyballdata;
set moneyballdata;
BATTING_COMPETENCE = TEAM_BATTING_H 
					+ 2*TEAM_BATTING_2B 
					+ 3*TEAM_BATTING_3B
					+ 4*TEAM_BATTING_HR 
					+ TEAM_BATTING_BB
					- TEAM_BATTING_SO;

BASESTEALING_COMPETENCE = TEAM_BASERUN_SB 
					- TEAM_BASERUN_CS;

FIELDING_COMPETENCE = TEAM_FIELDING_E
					- TEAM_FIELDING_DP;

PITCHING_COMPETENCE = TEAM_PITCHING_H + 
						4*TEAM_PITCHING_HR +
						TEAM_PITCHING_BB -
						TEAM_PITCHING_SO ;

DEFENSIVE_COMPETENCE = FIELDING_COMPETENCE+PITCHING_COMPETENCE;
OFFENSIVE_COMPETENCE = BATTING_COMPETENCE+BASESTEALING_COMPETENCE;

TOTAL_COMPETENCE = OFFENSIVE_COMPETENCE - DEFENSIVE_COMPETENCE;


TEAM_BATTING_H_log = log(TEAM_BATTING_H);
TEAM_BATTING_2B_log = log(TEAM_BATTING_2B);
TEAM_BATTING_SO_log = log(TEAM_BATTING_SO	);
TEAM_BATTING_HBP_log = log(TEAM_BATTING_HBP	);
TEAM_PITCHING_HR_log = log(TEAM_PITCHING_HR	);
TEAM_PITCHING_BB_log = log(TEAM_PITCHING_BB	);
TEAM_PITCHING_SO_log = log(TEAM_PITCHING_SO	);
TEAM_FIELDING_E_log = log(TEAM_FIELDING_E	);

run;
/*
Further EDA
*/
PROC CORR NOPROB NOSIMPLE data=moneyballdata;
var TARGET_WINS 	PITCHING_COMPETENCE BASESTEALING_COMPETENCE  BATTING_COMPETENCE  FIELDING_COMPETENCE;
run;


PROC CORR NOPROB NOSIMPLE data=moneyballdata;
var  TARGET_WINS
 TARGET_WINS_log
TEAM_BATTING_H_log
TEAM_BATTING_2B_log
TEAM_BATTING_3B_log
TEAM_BATTING_HR_log
TEAM_BATTING_BB_log
TEAM_BATTING_SO_log
TEAM_BASERUN_SB_log
TEAM_BASERUN_CS_log
TEAM_BATTING_HBP_log
TEAM_PITCHING_H_log
TEAM_PITCHING_HR_log
TEAM_PITCHING_BB_log
TEAM_PITCHING_SO_log
TEAM_FIELDING_E_log
TEAM_FIELDING_DP_log

TEAM_BATTING_H_sqrt
TEAM_BATTING_2B_sqrt
TEAM_BATTING_3B_sqrt
TEAM_BATTING_HR_sqrt
TEAM_BATTING_BB_sqrt
TEAM_BATTING_SO_sqrt
TEAM_BASERUN_SB_sqrt
TEAM_BASERUN_CS_sqrt
TEAM_BATTING_HBP_sqrt
TEAM_PITCHING_H_sqrt
TEAM_PITCHING_HR_sqrt
TEAM_PITCHING_BB_sqrt
TEAM_PITCHING_SO_sqrt
TEAM_FIELDING_E_sqrt
TEAM_FIELDING_DP_sqrt

;
run;

/*now the EDA and Data prep is done, split into train and testing sets*/

proc surveyselect data=moneyballdata samprate=0.10 SEED=39647 out=Sample outall 
           method=srs noprint;
run;

Data moneyballdataTEST; set Sample ; run; 
data moneyballdataTEST; modify moneyballdataTEST; if selected=0 then remove; run;
Data moneyballdataTRAIN;set Sample ;run; 
data moneyballdataTRAIN; modify moneyballdataTRAIN;	if selected=1 then remove;run;

/*doing the model exporation*/

   proc print data=moneyballdataTRAIN(obs=10);
      proc print data=moneyballdataTEST(obs=10);
      proc means data=moneyballdataTRAIN;
      proc means data=moneyballdataTEST;
      
       proc means data=moneyballdataTRAIN;

/*Exploring the no-transform models*/

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

/*Forward/backward/stepwise*/

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP / selection=forward ;
run;
/*Scoring the out of sample test*/
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
/*calculating RMSE*/
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP / selection=backward;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP / selection=stepwise;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_PITCHING_SO TEAM_FIELDING_E TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

/*Exploring the derived variable models*/

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE; 
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE; 
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

/*Forward/backward/stepwise*/

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE / selection=forward; 
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE; 
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE / selection=backward; 
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE; 
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE / selection=stepwise; 
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  BATTING_COMPETENCE BASESTEALING_COMPETENCE FIELDING_COMPETENCE PITCHING_COMPETENCE; 
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

/*Exploring the log transformed variable models*/

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

/*Forward/backward/stepwise*/

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP / selection=forward;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP / selection=backward;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;

proc reg data=moneyballdataTRAIN outest=RegOut aic bic;
	model TARGET_WINS  = TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP / selection=stepwise;
run;
proc score data=moneyballdataTEST score=RegOut type=parms predict out=Pred;
   var  TEAM_BATTING_H_log TEAM_BATTING_2B_log TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BATTING_SO_log TEAM_BASERUN_SB TEAM_BASERUN_CS TEAM_BATTING_HBP_log TEAM_PITCHING_H TEAM_PITCHING_HR_log TEAM_PITCHING_BB_log TEAM_PITCHING_SO_log TEAM_FIELDING_E_log TEAM_FIELDING_DP;
run;
proc print data=RegOut ; run;
proc SQL ;
select sqrt(sum((TARGET_WINS - model1)*(TARGET_WINS - model1))/228) from pred;
run;


