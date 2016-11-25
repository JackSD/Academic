DATA InsData;
 SET '/sscc/home/j/jsd417/411project2/logit_insurance_test.sas7bdat';
RUN;



/*
a macro for scoaring the data using the model
*/


%macro SCORE( INFILE, OUTFILE );

data &OUTFILE.;
set &INFILE.;
run;

data &OUTFILE.;
set &OUTFILE.;
/*creating the new category where the job cat not given*/
if  JOB = '' then  JOB = 'UNIDENTIFIED';
run;

/*DATA IMPUTATION*/
data  &OUTFILE.;
set &OUTFILE.;
/*imputation of variables*/
/*dummy variables*/
PARENT1_yes = (PARENT1='Yes');
PARENT1_no = (PARENT1='No');

MSTATUS_yes = (MSTATUS='Yes');
MSTATUS_no = (MSTATUS='z_No');

SEX_f = (SEX='z_F');
SEX_m = (SEX='M');

EDUCATION_bach  = (EDUCATION='Bachelors');
EDUCATION_lessHS  = (EDUCATION='<High School');
EDUCATION_HS  = (EDUCATION='z_High School');
EDUCATION_masters  = (EDUCATION='Masters');
EDUCATION_PhD  = (EDUCATION='PhD');

JOB_clerical = (JOB='Clerical');
JOB_HomeMaker = (JOB='Home Maker');
JOB_Student = (JOB='Student');
JOB_Professional = (JOB='Professional');
JOB_Lawyer = (JOB='Lawyer');
JOB_z_BlueCollar = (JOB='z_Blue Collar');
JOB_Manager = (JOB='Manager');
JOB_Doctor = (JOB='Doctor');
JOB_Blank = (JOB= 'UNIDENTIFIED');

CAR_USE_private = (CAR_USE='Private');
CAR_USE_Commercial = (CAR_USE='Commercial');

CAR_TYPE_suv = (CAR_TYPE='z_SUV');
CAR_TYPE_Minivan = (CAR_TYPE='Minivan');
CAR_TYPE_SportsCar = (CAR_TYPE='Sports Car');
CAR_TYPE_Pickup  = (CAR_TYPE='Pickup');
CAR_TYPE_Van = (CAR_TYPE='Van');
CAR_TYPE_PanelTruck = (CAR_TYPE='Panel Truck');

RED_CAR_yes = (RED_CAR='yes');
RED_CAR_no = (RED_CAR='no');

URBANICITY_urban = (URBANICITY='Highly Urban/ Urban');
URBANICITY_rural = (URBANICITY='z_Highly Rural/ Rural');

REVOKED_yes = (REVOKED='Yes');
REVOKED_no = (REVOKED='No');
run;

data  &OUTFILE.;
modify  &OUTFILE.;


if  INCOME='.' and  HOME_VAL >0 then  INCOME=  round(0.45803 *HOME_VAL -35872,1);
if HOME_VAL ='.' and INCOME = '.' then  INCOME  = round(2.23186*BLUEBOOK +30913,1) ;
if HOME_VAL =0 and INCOME = '.' then  INCOME  = round(2.23186*BLUEBOOK +30913,1) ;
if INCOME < 0 then INCOME = 0;

if  INCOME='0' and HOME_VAL ='.'  then  HOME_VAL=0;
if  HOME_VAL='.' and INCOME > 0  then  HOME_VAL= round(2.183263 *INCOME +  78318,1);
if HOME_VAL ='.' and INCOME = '.' then  HOME_VALUE = round(3.61598*BLUEBOOK +109272,1) ;

if  CAR_AGE='.'  then CAR_AGE= round(0.00005342 * INCOME + 4.90679,1) ;
if  CAR_AGE < 1  then CAR_AGE= round(0.00005342 * INCOME + 4.90679,1)  ;
if  YOJ='.'  then YOJ= round(0.00000203 * INCOME + 11.22846,1)  ;

if Age ='.' AND JOB = 'Student' then Age = 41;
if Age ='.' AND JOB = 'Clerical' then Age = 42;
if Age ='.' AND JOB = 'Home Maker' then Age =45 ;
if Age ='.' AND JOB = 'Lawyer' then Age = 48;
if Age ='.' AND JOB = 'z_Blue Collar' then Age = 44;
if Age ='.' AND JOB = 'Professional' then Age =46 ;
if Age ='.' AND JOB = 'Manager' then Age =47 ;
if Age ='.' AND JOB = 'UNIDENTIFIED' then Age =47 ;
run;

/*Scoring the data */

data &OUTFILE.;
set &OUTFILE.;

TEMP = -1.7127+ 0.4858*KIDSDRIV+ -0.2529*INCOME+ -0.1712*HOME_VAL
+ 0.0159*TRAVTIME+ -0.00003*BLUEBOOK+ -0.0566*TIF
+ -0.1151*OLDCLAIM+ 0.202*CLM_FREQ+ 0.2474*MVR_PTS+ 0.432*PARENT1_yes
+ -0.4919*MSTATUS_yes+ -0.4222*EDUCATION_bach
+ -0.4208*EDUCATION_masters+ -0.727*JOB_Manager+ -0.79*JOB_Doctor+ -0.7734*CAR_USE_private
+ -0.5958*CAR_TYPE_Minivan+ 0.3298*CAR_TYPE_SportsCar+ 2.353*URBANICITY_urban
+ 0.8939*REVOKED_yes;

TEMP=exp(TEMP);

P_TARGET_FLAG	= TEMP / (1+TEMP);
/*estimating the cost*/
P_TARGET_AMT = 0;
if P_TARGET_FLAG	>0.5 then P_TARGET_AMT = 1469;

run;





run;


%mend;


%SCORE(InsData, InsData_scored);

/*
proc print data=InsData_scored(obs=1000);
run;
*/


proc print data= InsData_scored(obs=10);  run;

/*
libname diss '/sscc/home/j/jsd417/411Project1/';
data diss.INSURANCE_TEST_SCORED_DOIG;
  set   InsData_scored;
  keep index P_TARGET_FLAG P_TARGET_AMT;
run;
*/

