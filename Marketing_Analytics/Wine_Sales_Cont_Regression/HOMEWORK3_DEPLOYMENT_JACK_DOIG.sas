DATA WineData;
 SET '/sscc/home/j/jsd417/411p3/wine_test.sas7bdat';
RUN;


/*
a macro for scoring the data using the model
*/


%macro SCORE( INFILE, OUTFILE );

data &OUTFILE.;
set &INFILE.;
run;

data &OUTFILE.;
set &OUTFILE.;
/*setting the average values for the missing values*/
 if STARS = "." then STARS = 0;
 if residualsugar = "." then residualsugar = 23.36;
 if chlorides = "." then chlorides = 0.222;
 if freesulfurdioxide = "." then freesulfurdioxide = 106.6;
 if totalsulfurdioxide = "." then totalsulfurdioxide = 204.3;
 if ph = "."  then ph = 3.20;
 if sulphates = "." then sulphates = 0.846;
 if alcohol = "." then alcohol = 10.52;
 
run;

/*DATA IMPUTATION*/
data  &OUTFILE.;
set &OUTFILE.;
/*dummy variables*/
STARS0 = (STARS = '0');
STARS1 = (STARS = '1');
STARS2 = (STARS = '2');
STARS3 = (STARS = '3');
STARS4 = (STARS = '4');

/*setting the absolute values/modifications*/ 
FixedAcidity1=ABS(FixedAcidity);
VolatileAcidity1=ABS(VolatileAcidity);
CitricAcid1=ABS(CitricAcid);
ResidualSugar1=ABS(ResidualSugar);
Chlorides1=ABS(Chlorides);
FreeSulfurDioxide1=ABS(FreeSulfurDioxide);
TotalSulfurDioxide1=ABS(TotalSulfurDioxide);
Sulphates1=ABS(Sulphates);
Alcohol1=ABS(Alcohol);
LabelAppeal1=LabelAppeal+3;

/*dummy variables*/
AcidIndex4=(AcidIndex=4);
AcidIndex5=(AcidIndex=5);
AcidIndex6=(AcidIndex=6);
AcidIndex7=(AcidIndex=7);
AcidIndex8=(AcidIndex=8);
AcidIndex9=(AcidIndex=9);
AcidIndex10=(AcidIndex=10);
AcidIndex11=(AcidIndex=11);
AcidIndex12=(AcidIndex=12);
AcidIndex13=(AcidIndex=13);
AcidIndex14=(AcidIndex=14);
AcidIndex15=(AcidIndex=15);
AcidIndex16=(AcidIndex=16);
AcidIndex17=(AcidIndex=17);

labelappeal11=(labelappeal1=1);
labelappeal12=(labelappeal1=2);
labelappeal13=(labelappeal1=3);
labelappeal14=(labelappeal1=4);
labelappeal15=(labelappeal1=5);

run;

/*Scoring the data */

data &OUTFILE.;
set &OUTFILE.;

/*dealing with the zero inflation part*/
TEMP = -16.2029  +16.5102*STARS0 +14.5434*STARS1 + 10.2747*STARS2 + 0*STARS3 + 0*STARS4;
TEMP=exp(TEMP);
P_ZERO_ZIP	= TEMP / (1+TEMP);
if P_ZERO_ZIP>=0.5 then P_TARGET=0;

/*now scoring the NB part*/
if P_ZERO_ZIP < 0.5 then TEMP2 =  1.5828-0.0154*VolatileAcidity1+-0.9758*LabelAppeal11+-0.6085*LabelAppeal12+-0.3398*LabelAppeal13+-0.157*LabelAppeal14+0*LabelAppeal15+0.1866*AcidIndex4+0.3462*AcidIndex5+
0.3594*AcidIndex6+0.3324*AcidIndex7+0.3178*AcidIndex8+0.2715*AcidIndex9+0.1916*AcidIndex10+0.0081*AcidIndex11+0.097*AcidIndex12+0.2235*AcidIndex13+0.2392*AcidIndex14+0.3677*AcidIndex15+
-19.4828*AcidIndex16+0*AcidIndex17+-0.3843*STARS0+-0.3294*STARS1+-0.2055*STARS2+-0.1021*STARS3+0*STARS4;

/*converting to a countable (P_TARGET) using the log link function*/
if P_ZERO_ZIP < 0.5 then P_TARGET= exp(TEMP2);
run;

%mend;


%SCORE(WineData, WineData_scored);

/*
proc print data= WineData_scored(obs=100);  run;
*/
/*
libname diss '/sscc/home/j/jsd417/411p3/';
data diss.WineData_Test_scored_SCORED_DOIG;
  set   WineData_scored;
  keep index P_TARGET;
run;
*/

