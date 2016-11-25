

DATA InsData;
 SET '/sscc/home/j/jsd417/411project2/logit_insurance.sas7bdat';

/*imputation of variables*/
/*creating the new category where the job cat not given*/
if  JOB = '' then  JOB = 'UNIDENTIFIED';

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


RUN;

data InsData;
modify InsData;


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

*if  INCOME<0  then INCOME=  0;
*if   HOME_VAL<0  then  HOME_VAL=  0;
*if   CAR_AGE<0  then  CAR_AGE=  0;
run;

/*derived variables*/
data InsData;
set InsData;
/*these variables get replaced later by standarization, here I preserve them*/
INCOME1 = INCOME;
HOME_VAL1 = HOME_VAL; 
YOJ1 = YOJ;
CLM_FREQ1 =CLM_FREQ ;
MVR_PTS1 = MVR_PTS;
OLDCLAIM1 = OLDCLAIM;
run;

PROC STANDARD DATA=InsData MEAN=5 STD=1 OUT=zInsData;
  VAR INCOME HOME_VAL YOJ CLM_FREQ MVR_PTS OLDCLAIM;
RUN;

/*derived variables*/
data zInsData;
set zInsData;
WEALTH = INCOME + HOME_VAL + YOJ;
CLAIMBEHAVIOR = MVR_PTS + OLDCLAIM + CLM_FREQ;

TARGET_AMT_log = log(TARGET_AMT);
KIDSDRIV_log = log(KIDSDRIV);
AGE_log = log(AGE);
HOMEKIDS_log = log(HOMEKIDS);
YOJ_log = log(YOJ);
INCOME_log = log(INCOME);
HOME_VAL_log = log(HOME_VAL);
TRAVTIME_log = log(TRAVTIME);
BLUEBOOK_log = log(BLUEBOOK);
TIF_log = log(TIF);
OLDCLAIM_log = log(OLDCLAIM);
CLM_FREQ_log = log(CLM_FREQ);
MVR_PTS_log = log(MVR_PTS);
CAR_AGE_log = log(CAR_AGE);

run;



/*now the EDA and Data prep is done, split into train and testing sets*/

proc surveyselect data=zInsData samprate=0.20 SEED=39647 out=Sample outall 
           method=srs noprint;
run;

Data InsDataTEST; set Sample ; run; 
data InsDataTEST; modify InsDataTEST; if selected=0 then remove; run;
Data InsDataTRAIN;set Sample ;run; 
data InsDataTRAIN; modify InsDataTRAIN;	if selected=1 then remove;run;

/*model building*/

   proc logistic data=InsDataTRAIN outest=betas covout  plots(only)=(roc(id=obs) effect);
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no

/ selection=backward; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

Proc npar1way data=Score2 ks;
Class TARGET_FLAG;
Var p_1;
Run;


/*doing a regression to estimate the cost*/


DATA InsData_accidents;
 SET InsData;
RUN;
DATA InsData_accidents;
 modify InsData_accidents;
 if TARGET_Flag=0 then remove;
RUN;
PROC CORR data=InsData_accidents; run;
PROC Reg;
model target_amt = bluebook;
run;
proc univariate data=InsData_accidents;
      var TARGET_AMT;
      histogram;
   run;
proc means data=InsData_accidents;run;

/*
Model Building section ************

proc print data=Score2 (obs=10); run;


   proc logistic data=InsDataTRAIN outest=betas covout plots(only)=(roc(id=obs) effect);
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no

/ selection=stepwise; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

Proc npar1way data=Score2 ks;
Class TARGET_FLAG;
Var p_1;
Run;

  proc logistic data=InsDataTRAIN outest=betas covout plots(only)=(roc(id=obs) effect);
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS WEALTH TRAVTIME BLUEBOOK TIF CLAIMBEHAVIOR
CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS 
EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional 
JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial 
CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck 
RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
/  link=probit; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;
   
   
   Proc npar1way data=Score2 ks;
Class TARGET_FLAG;
Var p_1;
Run;


---- probit regression

   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no

/ selection=stepwise link=probit; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;


   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no

/  link=probit; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

--- probit, dervied variables

   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS WEALTH TRAVTIME BLUEBOOK TIF CLAIMBEHAVIOR
CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS 
EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional 
JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial 
CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck 
RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
/  link=probit; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;


   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS WEALTH TRAVTIME BLUEBOOK TIF CLAIMBEHAVIOR
CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS 
EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional 
JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial 
CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck 
RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
/ selection=backward link=probit; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

---- DERIVED VARIABLES:

   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS WEALTH TRAVTIME BLUEBOOK TIF CLAIMBEHAVIOR
CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS 
EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional 
JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial 
CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck 
RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
/ selection=stepwise; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS WEALTH TRAVTIME BLUEBOOK TIF CLAIMBEHAVIOR
CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS 
EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional 
JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial 
CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck 
RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
 ; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
 /selection=stepwise; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;


   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
 /selection=backward; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

proc print data=Score2 (obs=10); run;

title 'fwd method Regression';
   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
 /selection=forward; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

proc print data=Score2 (obs=10); run;

title 'No selection method Regression';
   proc logistic data=InsDataTRAIN outest=betas covout;
      model TARGET_FLAG(event='1')=KIDSDRIV AGE HOMEKIDS YOJ INCOME HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ MVR_PTS CAR_AGE PARENT1_yes PARENT1_no MSTATUS_yes MSTATUS_no SEX_f SEX_m EDUCATION_bach EDUCATION_lessHS EDUCATION_HS EDUCATION_masters EDUCATION_PhD JOB_clerical JOB_HomeMaker JOB_Student JOB_Professional JOB_Lawyer JOB_z_BlueCollar JOB_Manager JOB_Doctor JOB_Blank CAR_USE_private CAR_USE_Commercial CAR_TYPE_suv CAR_TYPE_Minivan CAR_TYPE_SportsCar CAR_TYPE_Pickup CAR_TYPE_Van CAR_TYPE_PanelTruck RED_CAR_yes RED_CAR_no URBANICITY_urban URBANICITY_rural REVOKED_yes REVOKED_no
 ; 
                   score data=InsDataTEST out=Score2 fitstat;
   run;

proc print data=Score2 (obs=10); run;


*/






/*
************************EDA: studing the unusal car_age distribution
*/
/*
proc univariate data=InsData;
      var CAR_AGE;
      histogram;
   run;
*/
/*

PROC CORR NOPROB NOSIMPLE data=zInsData;

 ods graphics on;   
  DATA InsData_nonzeroincome;
 SET InsData;
RUN;

PROC PRINT data=InsData_nonzeroincome (obs=10);
run;

DATA InsData_nonzeroincome;
 modify InsData_nonzeroincome;
 if INCOME=0 then remove;
 * if CAR_AGE=0 then remove;
 *if HOME_VAL =0 then remove;
 *if CAR_AGE='.' then remove;
RUN;



*/



/*
****************************
doing regressions to allow regression imputation of variables:*/

/*
 ods graphics on;   
  DATA InsData_nonzeroincome;
 SET InsData;
RUN;

PROC PRINT data=InsData_nonzeroincome (obs=10);
run;

DATA InsData_nonzeroincome;
 modify InsData_nonzeroincome;
 if INCOME=0 then remove;
 * if CAR_AGE=0 then remove;
 *if HOME_VAL =0 then remove;
 *if CAR_AGE='.' then remove;
RUN;


PROC REG data=InsData_nonzeroincome PLOTS(MAXPOINTS=NONE);
model YOJ = INCOME;
run;

PROC REG data=InsData_nonzeroincome PLOTS(MAXPOINTS=NONE);
model INCOME = HOME_VAL;
run;

PROC REG data=InsData_nonzeroincome  PLOTS(MAXPOINTS=NONE);
model CAR_AGE = INCOME;
run;

PROC REG data=InsData_nonzeroincome PLOTS(MAXPOINTS=NONE);
model HOME_VAL=  BLUEBOOK;
run;
PROC REG data=InsData_nonzeroincome PLOTS(MAXPOINTS=NONE);
model INCOME=  BLUEBOOK;
run;

   ods graphics off;


DATA InsData_accidents;
 SET InsData;
RUN;
DATA InsData_accidents;
 modify InsData_accidents;
 if TARGET_Flag=0 then remove;
RUN;

PROC CONTENTS data=InsData;

PROC SORT data=InsData; BY TARGET_Flag; run;

PROC MEANS DATA=InsData;
RUN;



PROC MEANS DATA=zInsData;
RUN;


PROC SORT data=InsData; BY DESCENDING JOB ; 
run;

/*
ods graphics on;
   proc boxplot  data=InsData;
      plot AGE*JOB /
         boxstyle = schematic;
 insetgroup min max mean /
         header = 'data';   
run;
ods graphics on;
*/

/*

EDA: Target Variable analysis for categorical variables

PROC FREQ DATA=InsData;
tables CAR_TYPE
CAR_USE
EDUCATION
JOB
MSTATUS
PARENT1
SEX
URBANICITY
RED_CAR
REVOKED
;
Run;

/*Target Variable analysis 
PROC FREQ DATA=InsData_accidents;
tables CAR_TYPE
CAR_USE
EDUCATION
JOB
MSTATUS
PARENT1
SEX
URBANICITY
RED_CAR
REVOKED
;
Run;


proc univariate data=InsData_accidents;
      var TARGET_amt;
      histogram;
   run;
   
   ods graphics on;
PROC SGSCATTER DATA=InsData;
compare X=TARGET_amt Y=Income;
title " ";
run;
ods graphics off;

PROC CORR NOPROB NOSIMPLE data=InsData0;

PROC reg data=InsData;
 model TARGET_Flag = JOB;
 run;


proc print data=InsData0 (obs=10); 
run;

PROC SORT data=InsData0; BY DESCENDING JOB ; 
run;

ods graphics on;
   proc boxplot  data=InsData0;
      plot INCOME*JOB /
         boxstyle = schematic
         nohlabel
         horizontal;
   run;
ods graphics on;

PROC SORT data=InsData0; BY DESCENDING EDUCATION ; 
run;

ods graphics on;
   proc boxplot  data=InsData0;
      plot INCOME*EDUCATION /
         boxstyle = schematic
         nohlabel
         horizontal;
   run;
ods graphics on;
*/