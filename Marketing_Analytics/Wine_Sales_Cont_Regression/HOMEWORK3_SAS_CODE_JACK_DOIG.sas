DATA WineData;
 SET '/sscc/home/j/jsd417/411p3/wine.sas7bdat';
RUN;

DATA WineDataActualTest;
 SET '/sscc/home/j/jsd417/411p3/wine_test.sas7bdat';
RUN;

DATA WineData2;
 SET WineData;
 
 /*since some of the functions handle classification*/
 if STARS = "." then STARS = 0;
 if residualsugar = "." then residualsugar = 23.36;
 if chlorides = "." then chlorides = 0.222;
 if freesulfurdioxide = "." then freesulfurdioxide = 106.6;
 if totalsulfurdioxide = "." then totalsulfurdioxide = 204.3;
 if ph = "."  then ph = 3.20;
 if sulphates = "." then sulphates = 0.846;
 if alcohol = "." then alcohol = 10.52;
 
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

STARS0 = (STARS = '0');
STARS1 = (STARS = '1');
STARS2 = (STARS = '2');
STARS3 = (STARS = '3');
STARS4 = (STARS = '4');

/*
Exploring if there i a correlation with the Target variable for the ratio of free to bounded SD:

BoundedSulfurDioxide = TotalSulfurDioxide-FreeSulfurDioxide;
RatioFreeSDToBoundedSD  = FreeSulfurDioxide/BoundedSulfurDioxide ;
*/
VolatileAcidity_log = log(VolatileAcidity);
FreeSulfurDioxide_log = log(FreeSulfurDioxide);

/*
TARGET_log = log(TARGET);
FixedAcidity_log = log(FixedAcidity);
VolatileAcidity_log = log(VolatileAcidity);
CitricAcid_log = log(CitricAcid);
ResidualSugar_log = log(ResidualSugar);
Chlorides_log = log(Chlorides);
FreeSulfurDioxide_log = log(FreeSulfurDioxide);
TotalSulfurDioxide_log = log(TotalSulfurDioxide);
Density_log = log(Density);
pH_log = log(pH);
Sulphates_log = log(Sulphates);
Alcohol_log = log(Alcohol);
LabelAppeal_log = log(LabelAppeal);
AcidIndex_log = log(AcidIndex);
STARS_log = log(STARS);
*/
/*
TARGET_square =TARGET*TARGET;
FixedAcidity_square =FixedAcidity*FixedAcidity;
VolatileAcidity_square =VolatileAcidity*VolatileAcidity;
CitricAcid_square =CitricAcid*CitricAcid;
ResidualSugar_square =ResidualSugar*ResidualSugar;
Chlorides_square =Chlorides*Chlorides;
FreeSulfurDioxide_square =FreeSulfurDioxide*FreeSulfurDioxide;
TotalSulfurDioxide_square =TotalSulfurDioxide*TotalSulfurDioxide;
Density_square =Density*Density;
pH_square =pH*pH;
Sulphates_square =Sulphates*Sulphates;
Alcohol_square =Alcohol*Alcohol;
LabelAppeal_square =LabelAppeal*LabelAppeal;
AcidIndex_square =AcidIndex*AcidIndex;
STARS_square =STARS*STARS;

TARGET_squarert = sqrt(TARGET);
FixedAcidity_squarert = sqrt(FixedAcidity);
VolatileAcidity_squarert = sqrt(VolatileAcidity);
CitricAcid_squarert = sqrt(CitricAcid);
ResidualSugar_squarert = sqrt(ResidualSugar);
Chlorides_squarert = sqrt(Chlorides);
FreeSulfurDioxide_squarert = sqrt(FreeSulfurDioxide);
TotalSulfurDioxide_squarert = sqrt(TotalSulfurDioxide);
Density_squarert = sqrt(Density);
pH_squarert = sqrt(pH);
Sulphates_squarert = sqrt(Sulphates);
Alcohol_squarert = sqrt(Alcohol);
LabelAppeal_squarert = sqrt(LabelAppeal);
AcidIndex_squarert = sqrt(AcidIndex);
STARS_squarert = sqrt(STARS);
*/

RUN;

/*EDA: exploring log, square and sqrt transforms to see if corealtions improved

proc CORR NOSIMPLE noprob  data=wineData2;
VAR  TARGET 
  TARGET_log  
FixedAcidity_log  
VolatileAcidity_log  
CitricAcid_log  
ResidualSugar_log  
Chlorides_log  
FreeSulfurDioxide_log  
TotalSulfurDioxide_log  
Density_log  
pH_log  
Sulphates_log  
Alcohol_log  
LabelAppeal_log  
AcidIndex_log  
STARS_log  
;
run;

proc CORR NOSIMPLE noprob  data=wineData2;
VAR  TARGET 
TARGET_square  
FixedAcidity_square  
VolatileAcidity_square  
CitricAcid_square  
ResidualSugar_square  
Chlorides_square  
FreeSulfurDioxide_square  
TotalSulfurDioxide_square  
Density_square  
pH_square  
Sulphates_square  
Alcohol_square  
LabelAppeal_square  
AcidIndex_square  
STARS_square  
;
run;

proc CORR NOSIMPLE noprob  data=wineData2;
VAR  TARGET 
TARGET_squarert 
FixedAcidity_squarert 
VolatileAcidity_squarert 
CitricAcid_squarert 
ResidualSugar_squarert 
Chlorides_squarert 
FreeSulfurDioxide_squarert 
TotalSulfurDioxide_squarert 
Density_squarert 
pH_squarert 
Sulphates_squarert 
Alcohol_squarert 
LabelAppeal_squarert 
AcidIndex_squarert 
STARS_squarert 
 
;
run;
*/

/*EDA : exploring the missing STARS values in the context of label appeal


data winedata3;
set winedata;
run;
data winedata3;
modify winedata3;
if STARS ~= '.' then remove;
run;
proc univariate data=WineData3;
      var labelappeal;
      histogram;
run;
*/

/*
EDA : exploring the negatvie values for residual sugar

data winedata3;
set winedata;
run;
data winedata3;
modify winedata3;
if ResidualSugar< 0 then remove;
run;


data winedata4;
set winedata;
run;
data winedata4;
modify winedata4;
if ResidualSugar>= 0 then remove;
run;
proc univariate data=WineData3;
      var ResidualSugar;
      histogram;
run;

proc univariate data=WineData4;
      var ResidualSugar;
      histogram;
run;
*/

/*
EDA: studing the target distribution
*/
/*

PROC PRINT WineData;

proc univariate data=WineData;
      var TARGET;
      histogram;
run;

proc CORR NOSIMPLE noprob  data=wineData;
run;

proc MEANS data=wineData;

PROC SGSCATTER data=wineData;
compare x=RatioFreeSDToBoundedSD  y=target;
run;

PROC SGSCATTER data=wineData; compare x=ResidualSugar y=Alcohol ; run;

proc univariate data=WineData2; var FixedAcidity;  histogram; run;
proc univariate data=WineData2; var FixedAcidity1;  histogram; run;
proc univariate data=WineData2; var VolatileAcidity;  histogram; run;
proc univariate data=WineData2; var VolatileAcidity1;  histogram; run;
proc univariate data=WineData2; var CitricAcid;  histogram; run;
proc univariate data=WineData2; var CitricAcid1;  histogram; run;
proc univariate data=WineData2; var ResidualSugar;  histogram; run;
proc univariate data=WineData2; var ResidualSugar1;  histogram; run;
proc univariate data=WineData2; var Chlorides;  histogram; run;
proc univariate data=WineData2; var Chlorides1;  histogram; run;
proc univariate data=WineData2; var FreeSulfurDioxide;  histogram; run;
proc univariate data=WineData2; var FreeSulfurDioxide1;  histogram; run;
proc univariate data=WineData2; var TotalSulfurDioxide;  histogram; run;
proc univariate data=WineData2; var TotalSulfurDioxide1;  histogram; run;
proc univariate data=WineData2; var Sulphates;  histogram; run;
proc univariate data=WineData2; var Sulphates1;  histogram; run;
proc univariate data=WineData2; var Alcohol;  histogram; run;
proc univariate data=WineData2; var Alcohol1;  histogram; run;

proc univariate data=WineData;
      var labelappeal;
      histogram;
run;



proc CORR NOSIMPLE noprob  data=wineData2;
VAR target 
FixedAcidity1 VolatileAcidity1 CitricAcid1 ResidualSugar1 Chlorides1 FreeSulfurDioxide1 TotalSulfurDioxide1 density  ph 
Sulphates1 Alcohol1 LabelAppeal1 acidindex stars  ;
run;

*/


/*setting up the training and test sets*/

/*PROC pRINT data=winedata2(obs=1000); run;*/


proc surveyselect data=WineData2 samprate=0.20 SEED=39647 out=Sample outall 
           method=srs noprint;
run;
Data WineDataTEST; set Sample ; run; 
data WineDataTEST; modify WineDataTEST; if selected=0 then remove; run;
Data WineDataTRAIN;set Sample ;run; 
data WineDataTRAIN; modify WineDataTRAIN;	if selected=1 then remove;run;

/*trying a number of variables combinations with the poisson regression*/

/*
proc genmod data=WineDataTRAIN;
   class labelappeal1 acidindex stars ;
   model target=  residualsugar1 chlorides1
     freesulfurdioxide1 totalsulfurdioxide1 density ph sulphates1   labelappeal1
     acidindex stars fixedacidity1 volatileacidity1 citricacid1 alcohol1 
       / dist=poisson link=log  ;
   store out=poimodel    ;
run;
proc plm source=poimodel;
        score data=WineDataTEST out=demo_wine pred=p_target_poi/ ilink;
run;
proc SQL ;
select sqrt(sum((TARGET - p_target_poi)*(TARGET - p_target_poi))/2559) from demo_wine;
run;
*/
/*proc print data=demo_wine;*/
/*
proc genmod data=WineDataTRAIN;
   class labelappeal1 acidindex stars ;
   model target=  
			FreeSulfurDioxide1
			VolatileAcidity1
			LabelAppeal1
			AcidIndex
			STARS
       / dist=poisson link=log  ;
   store out=poimodel    ;
run;
proc plm source=poimodel;
        score data=WineDataTEST out=demo_wine pred=p_target_poi/ ilink;
run;
proc SQL ;
select sqrt(sum((TARGET - p_target_poi)*(TARGET - p_target_poi))/2559) from demo_wine;
run;


proc genmod data=WineDataTRAIN;
   class labelappeal1 stars ;
   model target=  
			LabelAppeal1
			STARS
       / dist=poisson link=log  ;
   store out=poimodel    ;
run;
proc plm source=poimodel;
        score data=WineDataTEST out=demo_wine pred=p_target_poi/ ilink;
run;
proc SQL ;
select sqrt(sum((TARGET - p_target_poi)*(TARGET - p_target_poi))/2559) from demo_wine;
run;

*/

*///next for negative binomial model///;

/*
proc genmod data=WineDataTRAIN;
   class labelappeal1 acidindex stars ;
   model target=  residualsugar1 chlorides1
     freesulfurdioxide1 totalsulfurdioxide1 density ph sulphates1   labelappeal1
     acidindex stars fixedacidity1 volatileacidity1 citricacid1 alcohol1 
       / dist=nb link=log  ;
   store out=xmodel    ;
run;
proc plm source=xmodel    ;
        score data=WineDataTEST out=demo_wine pred=p_target/ ilink;
run;
proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;

proc genmod data=WineDataTRAIN;
   class labelappeal1 acidindex stars ;
   model target=  
			FreeSulfurDioxide1
			VolatileAcidity1
			LabelAppeal1
			AcidIndex
			STARS
       / dist=nb link=log  ;
   store out=xmodel    ;
run;
proc plm source=xmodel    ;
        score data=WineDataTEST out=demo_wine pred=p_target/ ilink;
run;
proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;


proc genmod data=WineDataTRAIN;
   class labelappeal1 stars ;
   model target=  
			LabelAppeal1
			STARS
       / dist=nb link=log  ;
   store out=xmodel    ;
run;
proc plm source=xmodel    ;
        score data=WineDataTEST out=demo_wine pred=p_target/ ilink;
run;
proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;
*/

*///next for zip model///;

/*
proc genmod data=WineDataTRAIN ;
	class labelappeal1 acidindex stars ;
   model target=  residualsugar1 chlorides1
     freesulfurdioxide1 totalsulfurdioxide1 density ph sulphates1   labelappeal1
     acidindex stars fixedacidity1 volatileacidity1 citricacid1 alcohol1 
       /  dist=zip link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;

proc genmod data=WineDataTRAIN ;
	   class labelappeal1 acidindex stars ;
   model target=  
			FreeSulfurDioxide1
			VolatileAcidity1
			LabelAppeal1
			AcidIndex
			STARS
       /  dist=zip link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;

proc genmod data=WineDataTRAIN ;
	  class labelappeal1 stars ;
   model target=  
			LabelAppeal1
			STARS
       /  dist=zip link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;


proc genmod data=WineDataTRAIN ;
	class labelappeal1 acidindex stars ;
   model target=  residualsugar1 chlorides1
     freesulfurdioxide1 totalsulfurdioxide1 density ph sulphates1   labelappeal1
     acidindex stars fixedacidity1 volatileacidity1 citricacid1 alcohol1 
       /  dist=zinb link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;

proc genmod data=WineDataTRAIN ;
	   class labelappeal1 acidindex stars ;
   model target=  
			FreeSulfurDioxide1
			VolatileAcidity1
			LabelAppeal1
			AcidIndex
			STARS
       /  dist=zinb link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;

proc genmod data=WineDataTRAIN ;
	  class labelappeal1 stars ;
   model target=  
			LabelAppeal1
			STARS
       /  dist=zinb link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;

*/



/*Exploring the no-transform models*/
/*
proc reg data=WineDataTRAIN outest=RegOut aic bic;
model target = FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4;
run;


proc score data=winedataTEST score=RegOut type=parms predict out=Pred;
   var  FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4;
run;
proc print data=RegOut ; run;

/*Forward/backward/stepwise*/

/*
proc reg data=WineDataTRAIN outest=RegOut aic bic;
model target = FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4/ selection=forward ;
run;

*/

/*Scoring the out of sample test


proc score data=winedataTEST score=RegOut type=parms predict out=Pred;
   var  FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4;
run;
proc print data=RegOut ; run;

proc reg data=WineDataTRAIN outest=RegOut aic bic;
model target = FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4 / selection=backward;
run;
proc score data=winedataTEST score=RegOut type=parms predict out=Pred;
   var  FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4;
run;
proc print data=RegOut ; run;

proc reg data=WineDataTRAIN outest=RegOut aic bic;
model target = FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4 / selection=stepwise;
run;
proc score data=winedataTEST score=RegOut type=parms predict out=Pred;
   var  FixedAcidity1
VolatileAcidity1
CitricAcid1
ResidualSugar1
Chlorides1
FreeSulfurDioxide1
TotalSulfurDioxide1
Density
pH
Sulphates1
Alcohol1
LabelAppeal1
AcidIndex
STARSother
STARS1
STARS2
STARS3
STARS4;
run;
proc print data=RegOut ; run;

*/

/*analysis of the FINAL model*/

proc genmod data=WineDataTRAIN ;
	   class labelappeal1 acidindex stars ;
   model target=  
			FreeSulfurDioxide1
			VolatileAcidity1
			LabelAppeal1
			AcidIndex
			STARS
       /  dist=zinb link=log  ;
       zeromodel STARS /link=logit;   
   store out=xmodel    ;
   
run;

proc plm source=xmodel    ;
        score data=WineDataTEST  out=demo_wine pred=p_target pzero=P_ZERO_ZIP/ ilink;
run;

proc print data=demo_wine2(obs=100);run;

proc SQL ;
select sqrt(sum((TARGET - p_target)*(TARGET - p_target))/2559) from demo_wine;
run;
