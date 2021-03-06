/*
load the new data 
*/

DATA moneyballdata_newdata;
 SET '/sscc/home/j/jsd417/411Project1/moneyball_test.sas7bdat';
RUN;


/*
a macro for scoaring the data using the model
*/


%macro SCORE( INFILE, OUTFILE );

data &OUTFILE.;
set &INFILE.;
run;

data &INFILE.;
modify &INFILE.;

if TEAM_BATTING_3B= 0 OR TEAM_BATTING_3B>200  then TEAM_BATTING_3B =  max(110.70624 -0.07636*TEAM_BATTING_SO,0);
if TEAM_BATTING_SO= 0 OR TEAM_BATTING_SO='.' then TEAM_BATTING_SO = 427.67277 + 2.99557*TEAM_BATTING_HR;
if TEAM_BASERUN_SB = 0 OR TEAM_BASERUN_SB ='.' then TEAM_BASERUN_SB = 31.77479  + 1.73187*TEAM_BATTING_3B;
if TEAM_BASERUN_CS = 0 OR TEAM_BASERUN_CS ='.' then TEAM_BASERUN_CS = 20.50346 	+  0.33767*TEAM_BASERUN_SB;
if  TEAM_PITCHING_H > 25000 then  TEAM_PITCHING_H =  	890.03009 +  3.50844 *TEAM_FIELDING_E;
if team_pitching_HR = 0  then   TEAM_PITCHING_HR =  8.13728 + 0.97995*TEAM_BATTING_HR;
if TEAM_PITCHING_BB >2000   then   TEAM_PITCHING_BB =  186.42785 	 + 0.72062*TEAM_BATTING_BB;
if TEAM_PITCHING_SO = 0 OR TEAM_PITCHING_SO='.' or TEAM_PITCHING_SO > 3000   then   TEAM_PITCHING_SO =  724.36459 + 0.14713*TEAM_PITCHING_BB;
if TEAM_FIELDING_DP = 0 OR TEAM_FIELDING_DP='.'  then  TEAM_FIELDING_DP =  max(165.81621 -0.10542*TEAM_FIELDING_E,0);
if  TEAM_BATTING_HBP = '.' then  TEAM_BATTING_HBP = 29+ (95-29)*RAND('NORMAL',0.5,0.16);
if  TEAM_BATTING_HBP < 29 then  TEAM_BATTING_HBP =29;
if  TEAM_BATTING_HBP > 95 then  TEAM_BATTING_HBP =95;
run;

/*DERIVED VARIABLES*/
data &OUTFILE.;
set &OUTFILE.;

TEAM_BATTING_H_log = log(TEAM_BATTING_H);
TEAM_BATTING_2B_log = log(TEAM_BATTING_2B);
TEAM_BATTING_SO_log = log(TEAM_BATTING_SO	);
TEAM_BATTING_HBP_log = log(TEAM_BATTING_HBP	);
TEAM_PITCHING_HR_log = log(TEAM_PITCHING_HR	);
TEAM_PITCHING_BB_log = log(TEAM_PITCHING_BB	);
TEAM_PITCHING_SO_log = log(TEAM_PITCHING_SO	);
TEAM_FIELDING_E_log = log(TEAM_FIELDING_E	);

run;

data &OUTFILE.;
set &OUTFILE.;
/*RUN FINAL MODEL*/
P_TARGET_WINS = round(-304.30066
+ TEAM_BATTING_H_log*75.07852
+ TEAM_BATTING_2B_log*-5.88131
+ TEAM_BATTING_3B*0.11961
+ TEAM_BATTING_HR*0.02753
+ TEAM_BATTING_BB*0.04044
+ TEAM_BATTING_SO_log*-11.24385
+ TEAM_BASERUN_SB*0.03965
+ TEAM_PITCHING_H*-0.00103
+ TEAM_PITCHING_BB_log*-13.57495
+ TEAM_PITCHING_SO_log*11.44466
+ TEAM_FIELDING_E_log*-12.92534
+ TEAM_FIELDING_DP*-0.08670);
if P_TARGET_WINS < 0 then P_TARGET_WINS = 0;
run;

%mend;

/*
CALL the macro and print the results
*/

%SCORE(moneyballdata_newdata, moneyballdata_scored);

proc print data=moneyballdata_scored;
run;
