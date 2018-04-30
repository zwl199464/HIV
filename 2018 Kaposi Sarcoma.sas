
libname HIV 'D:\SAS\Project\HIV cancer\Kaposi Sarcoma';
libname HI 'D:\SAS\Project\HIV cancer\FCDS_Non-Hodgkin''s Lymphoma (NHL)';
options source2;
%include "D:\SAS\Project\HIV cancer\FCDS_Non-Hodgkin's Lymphoma (NHL)\_format.sas"/lrecl=1000;
%include "D:\SAS\Project\HIV cancer\FCDS_Non-Hodgkin's Lymphoma (NHL)\_makeform.sas"/lrecl=1000;
%include "D:\SAS\Project\HIV cancer\FCDS_Non-Hodgkin's Lymphoma (NHL)\_format_label2013.sas"/lrecl=1000;
%m_format; 
%makeform(name=age50_3f, endpoint=50 70 high);
%makeform(name=age50f, endpoint=17 54 69 high);
%makeform(name=age55f, endpoint=17 54 high);
%makeform(name=age60_1f, endpoint=17 59 high);
%makeform(name=age65f, endpoint=17 64 high);
%makeform(name=agef, endpoint=17 29 39 49 55 69 high);
%makeform(name=ageNHL, endpoint=17 49 59 69 79 high);
run;
proc format;
VALUE BB
     9680 =' Malig Lymph Lg B cell diffuse'
     9687 =' Burkitt Lymphoma NOS'
     9678 =' Primary effusion Lymphoma';
	 VALUE NHL
     9670-9699='nhl - mature b-cell lymphomas '
	9700-9719='nhl - mature t and nk-cell lymphomas '
		9720-9729='nhl - precursor cell lymphoblastic lymphoma ';
value hisG 
           9140 ='Kaposi Sarcoma'
           9670-9729='Non-Hodgkin Lymphoma';
value yearofdx
           1='1981-1996'
           0= '1997-2013';
value stage 
          0,1='Early (Localized)'
		2,3,7='Advanced  (Regional and Distant)';
value ins
			0='Uninsured'
			1='Private Insurance'
			2,3,4,5='Government/other'
			.='Unknown';

run;
data ALL;
set hiv.Kaposi_sarcoma hi.Nhl2013;
where put (HISTO3V, hisG.) in ('Kaposi Sarcoma','Non-Hodgkin Lymphoma');
format HISTO3V hisG. ins6c ins. ; *ks=6682 NHL=82653;
run;

data analysis ;
set ALL;
if DATE_yr<1997 then before_HARRT=1;else before_HARRT=0;
format HISTO3V HISTO3VGF. before_HARRT yearofdx. seer_stage stage.;
run;
*proc contents data= ALL;

*run;



data Almost; set analysis; *no duplicates, single record for each patients;
	where age_dx>17 and state_dx='FL' and age_dx ne 999 and county_dx not in (. 998 999) and sex in (1:2); 
    if vvalue(kriegerSF3U) ne 'Unknown'& vvalue(race3_eth) ne 'Unknown' &
     vvalue(MAR_STAT4cU) ne 'Unknown'& vvalue(NHIARECU) ne 'Unknown' & vvalue(ins7cU) ne 'Unknown' &
      MAR_STAT4cU ne . & race3_eth ne . & surv_mon ne . & seer_stage in (0,1,2,3,7);
      Age=age_dx;
	if death=0 then status=1; 
	if death=1 then status=0;
	%m_label2013;
	format age age50f. HISTO3V hisG. ;
run; *n=47297;
data KS; set Almost; where put(HISTO3V,hisG.) ='Kaposi Sarcoma' and age < 56; format ins6c ins. age agef.;*n=1205;
data NHL;set Almost; where put(HISTO3V,hisG.) ='Non-Hodgkin Lymphoma' ; format ins6c ins. HISTO3V BB.;*n=45705;
proc sort data=ks NODUPKEY ; by casenum;*n=1205 ;
proc sort data=NHL NODUPKEY ; by casenum HISTO3V;*n=45705 with no dup casenum 
means there are near 2000 have two types of NHL;
proc sql; create table work.overlap as select * from work.NHL n, work.KS s where n.casenum=s.casenum;quit;*n=21;
proc sql; create table work.cross as select * /*n=36 dul ks and NHL*/
From (select * from work.NHL where casenum in
(select n.casenum From work.NHL n, work.KS s where n.casenum=s.casenum))
outer union corr select * from (select * from work.KS where casenum in 
(select n.casenum From work.NHL n, work.KS s where n.casenum=s.casenum)) ;quit;*n=42;
proc sql; create table work.all as select * from work.NHL outer union corr select * from work.KS ;quit; *n=46910;
data all; set all;agg=put(age, age55f.);format ins6c ins. HISTO3V hisG.; run;

*******************************************************


Output to Excel files

*******************************************************;
ods html;
proc freq data=all; table seer_stage;run;

  ods tagsets.excelxp file='D:\SAS\Project\HIV cancer\Burden of AIDS defined cancer in Florida\sy.xml' style=statistical
      options(auto_subtotals='yes' default_column_width='7, 10, 10, 7, 7' frozen_rowheaders='yes' sheet_interval='none' sheet_name='Table 1 Demographic' 
              autofilter='all' autofilter_table='2' embedded_titles='yes' embedded_footnotes='no');
proc tabulate missing data=NHL; where sex in (1,2);
FORMAT HISTO3V BB.; where HISTO3V in(9680 ,9687,9678);
class status race3c NHIAREC kriegerSF3   MAR_STAT4c ins6c   fcds_site age  sex HISTO3V before_HARRT
	/s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue];
table all status*[style=[background=lightgrey]] sex*[style=[background=lightgrey]] race3c NHIAREC*[style=[background=lightgrey]] kriegerSF3   MAR_STAT4c*[style=[background=lightgrey]]
	ins6c before_HARRT age , 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1 rowpctn='row%'*F=6.1) ( HISTO3V*[style=[background=lightgreen]]  )*(n='N'*F=comma8.0 colpctn='col%'*F=6.1 rowpctn='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table nhL1: Demographic characteristics";
 run;
 proc tabulate missing data=NHL; where sex in (1,2);
 FORMAT HISTO3V NHL.;
class status race3c NHIAREC kriegerSF3  MAR_STAT4c ins6c   fcds_site age  sex HISTO3V before_HARRT 
	/s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue];
table all status*[style=[background=lightgrey]] sex*[style=[background=lightgrey]] race3c NHIAREC*[style=[background=lightgrey]] kriegerSF3   MAR_STAT4c*[style=[background=lightgrey]]
	ins6c before_HARRT age, 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1 rowpctn='row%'*F=6.1) ( HISTO3V*[style=[background=lightgreen]]  )*(n='N'*F=comma8.0 colpctn='col%'*F=6.1 rowpctn='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table NHL2: Demographic characteristics";
 run;
 ods tagsets.excelxp close;
   ods tagsets.excelxp file='D:\SAS\Project\HIV cancer\Burden of AIDS defined cancer in Florida\wl.xml' style=statistical
      options(auto_subtotals='yes' default_column_width='7, 10, 10, 7, 7' frozen_rowheaders='yes' sheet_interval='none' sheet_name='Table 1 Demographic' 
              autofilter='all' autofilter_table='2' embedded_titles='yes' embedded_footnotes='no');
proc tabulate missing data=all;
class  status sex race3c NHIARECU kriegerSF3U  MAR_STAT4cU ins6c before_HARRT HISTO3V
	/s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue];
table all status*[style=[background=lightgrey]] sex*[style=[background=lightgrey]] race3c NHIARECU*[style=[background=lightgrey]] kriegerSF3U   MAR_STAT4cU*[style=[background=lightgrey]]
	ins6c /*[style=[background=lightgrey]]*/ before_HARRT*[style=[background=lightgrey]] age , 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1) ( HISTO3V*[style=[background=lightgreen]])*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table KS NHL: Demographic characteristics";
 run;

proc tabulate missing data=all;
format seer_stage stage.;
class  status  race3c NHIAREC kriegerSF3   MAR_STAT4c ins6c   fcds_site age sex HISTO3V seer_stage before_HARRT
	/s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue];
table all status*[style=[background=lightgrey]] sex*[style=[background=lightgrey]] race3c NHIAREC*[style=[background=lightgrey]] kriegerSF3  MAR_STAT4c*[style=[background=lightgrey]]
	ins6c before_HARRT age, 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1) ( HISTO3V*seer_stage*[style=[background=lightgreen]])*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table KS NHL: seer";
run;
ods tagsets.excelxp close;
ods tagsets.excelxp file='D:\SAS\Project\HIV cancer\Burden of AIDS defined cancer in Florida\syy.xml' style=statistical
      options(auto_subtotals='yes' default_column_width='7, 10, 10, 7, 7' frozen_rowheaders='yes' sheet_interval='none' sheet_name='Table 1 Demographic' 
              autofilter='all' autofilter_table='2' embedded_titles='yes' embedded_footnotes='no');
proc tabulate missing data=nhl;
format seer_stage stage. HISTO3V nhl.;
class  status  race3c NHIAREC kriegerSF3   MAR_STAT4c ins6c   fcds_site age sex HISTO3V seer_stage before_HARRT
	/s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue];
table all status*[style=[background=lightgrey]] sex*[style=[background=lightgrey]] race3c NHIAREC*[style=[background=lightgrey]] kriegerSF3  MAR_STAT4c*[style=[background=lightgrey]]
	ins6c before_HARRT age , 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1) ( HISTO3V*seer_stage*[style=[background=lightgreen]])*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table  NHL: seer";
run;


ods tagsets.excelxp close;


options mlogic mprint SYMBOLGEN;
%macro multivar(da,class,ref,type=n,sc=NONE,ci=both);
%let ref1=%scan(&ref,1,'*')  ;%let ref2=%scan(&ref,2,'*') ;
%let ref3=%scan(&ref,3,'*');%let ref4=%scan(&ref,4,'*') ;%let ref5=%scan(&ref,5,'*'); %let ref6=%scan(&ref,6,'*') ;
proc logistic data= &da. OUTDESIGN= Design_&da. OUTEST= Test_&da. OUTMODEL= Model_&da.;
%if &da.=ks %then %do; %let race=race1; format ins6c ins7cf.; %end;%else %let race=race3c;
%if &type.=n %then %do; title; title "Multiple Response &da."; %end;
   %else %if &type.=b %then %do; where HISTO3V between 9670 and 9699; title; title "Multiple Response &da.For B cell"; %end;
   %else %if &type.=t %then %do; where HISTO3V between 9700 and 9719; title; title "Multiple Response &da. For T and NK cell"; %end;
    %else %if &type.=o %then %do; where HISTO3V between 9720 and 9729; title; title "Multiple Response &da. For P"; %end;
   format seer_stage stage.;
  %if &class ne ""%then %do; class %scan(&class,1)(ref="&ref1.") %scan(&class,2)(ref="&ref2.")
%scan(&class,3)(ref="&ref3.") %scan(&class,4)(ref="&ref4.") %scan(&class,5)(ref="&ref5.")
%scan(&class,6)(ref=&ref6.)/param=ref;%end;
 model seer_stage= sex &race. NHIAREC kriegerSF3  ins6c before_HARRT age / SCALE= &sc. AGGREGATE CLODDS= &ci. ;
   
run;
%mend;
%macro univar(da,class,ref,type=n,sc=NONE,ci=both);
%do i=1 %to 6;
%let refa=%scan(&ref,&i.,'*')  ;
proc logistic data= &da. OUTDESIGN= Design_&da. OUTEST= Test_&da. OUTMODEL= Model_&da.;
%if &da.=ks %then %do; %let race=race1; format ins6c ins7cf.; %end;%else %let race=race3c;
%if &type.=n %then %do; title; title "Univariate Response &da."; %end;
   %else %if &type.=b %then %do; where HISTO3V between 9670 and 9699; title; title "Univariate Response &da.For B cell"; %end;
   %else %if &type.=t %then %do; where HISTO3V between 9700 and 9719; title; title "Univariate Response &da. For T and NK cell"; %end;
    %else %if &type.=o %then %do; where HISTO3V between 9720 and 9729;  title; title "Univariate Response &da. For P"; %end;
  format seer_stage stage.;
 class %scan(&class,&i.)(ref="&refa.") / param=ref;
 model seer_stage= %scan(&class,&i.)/ SCALE= &sc. AGGREGATE CLODDS= &ci. ;
   
run;
%end;
%mend;
*1997-2013;
ods html file='D:\SAS\Project\HIV cancer\Burden of AIDS defined cancer in Florida\result.html';
%multivar(ks,sex race1 NHIAREC kriegerSF3  ins6c before_HARRT
,Female*White* Non-Hispanic *Lowest*Uninsured*Last );
%multivar(nhl,sex race3c NHIAREC kriegerSF3 ins6c before_HARRT
,Female*White* Non-Hispanic *Highest*Private Insurance*First );


%multivar(nhl,sex race3c NHIAREC kriegerSF3  ins6c before_HARRT
,Female*White* Non-Hispanic *Highest*Private Insurance*First,type=b );
%multivar(nhl,sex race3c NHIAREC kriegerSF3  ins6c before_HARRT
,Female*White* Non-Hispanic *Highest*Private Insurance*Last,type=t );
%multivar(nhl,sex race3c NHIAREC kriegerSF3  ins6c before_HARRT
,Male*White* Non-Hispanic *Lowest*Private Insurance*First,type=o);

%univar(ks,sex race1 NHIAREC kriegerSF3  ins6c before_HARRT
,Female*White* Non-Hispanic *Lowest*Uninsured*1997-2013);
%univar(nhl,sex race3c NHIAREC kriegerSF3 ins6c before_HARRT
,Female*White* Non-Hispanic *Highest*Private Insurance*1981-1996);
%univar(nhl,sex race3c NHIAREC kriegerSF3  ins6c before_HARRT
,Female*White* Non-Hispanic *Highest*Private Insurance*1981-1996,type=b );
%univar(nhl,sex race3c NHIAREC kriegerSF3  ins6c before_HARRT
,Female*White* Non-Hispanic *Highest*Private Insurance*1997-2013,type=t );
%univar(nhl,sex race3c NHIAREC kriegerSF3  ins6c before_HARRT
,Male*White* Non-Hispanic *Lowest*Private Insurance*1997-2013,type=o);
proc freq data = nhl;
exact or; format seer_stage stage.;
	tables seer_stage*sex / chisq exact cmh  ;
	tables seer_stage*race1 / chisq exact cmh  ;
	tables seer_stage*NHIAREC / chisq exact cmh  ;
	tables seer_stage*kriegerSF3 / chisq exact cmh  ;
	tables seer_stage*ins6c / chisq exact cmh  ;
    tables seer_stage*before_HARRT / chisq exact cmh  ;
	 where HISTO3V between 9720 and 9729;
run;
/*proc logistic data=nhl ;
    format seer_stage stage.;
   model seer_stage=sex race3c NHIAREC kriegerSF3 age ins7cU;
   title 'Multiple Response nhl';
run;
*/

/*proc logistic data=nhl ;
    format seer_stage stage.;
   class sex race3c NHIAREC kriegerSF3 ins7cU age;
   model seer_stage=sex race3c NHIAREC kriegerSF3 age ins7cU/ ;
   oddsratio seer_stage;
       where HISTO3V between 9670 and 9699;
   title 'Multiple Response nhl B';
run;

proc logistic data=nhl ;
    format seer_stage stage.;
   class sex race3c NHIAREC kriegerSF3 ins7cU age;
   model seer_stage=sex race3c NHIAREC kriegerSF3 age ins7cU/ ;
   oddsratio seer_stage;
    where HISTO3V between 9700 and 9719;
   title 'Multiple Response nhl t and nk';
run;
proc logistic data=nhl ;
    format seer_stage stage.;
   class sex race3c NHIAREC kriegerSF3 ins7cU age;
   model seer_stage=sex race3c NHIAREC kriegerSF3 age ins7cU/ ;
   oddsratio seer_stage;
   	 where HISTO3V between 9720 and 9729;
   title 'Multiple Response nhl precursor ';
run;
ods html close;
/*data NHL_2;
set nhl;
PYear=;*/

/*proc stdrate data=all
refdata=ks
method=direct
stat=rate(mult=1000)
effect=ratio
plots(only)=effect
;
population group= before_HARRT event=Death total=surv_mon;
reference total=surv_mon;
strata sex race3c NHIAREC kriegerSF3 age ins7cU / effect;
run;
*/
/*ods tagsets.excelxp options( default_column_width='20, 10, 10, 7, 7' sheet_interval='none' sheet_name='Table 2 Clinic');
proc tabulate missing data=all; 
class status    race3c NHIAREC kriegerSF3 fcds_site D_AJCC_S5  stage_T stage_N  stage_M seer_stage Grade ER PR ERPR  HISTO3V EOD10_NE3 BEHO3V sex 
	EOD10_PN2 event_chm event_hm Event_rx Event_sx   /s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue]; 
table all  stage_T stage_N*[style=[background=lightgrey]]  stage_M D_AJCC_S5*[style=[background=lightgrey]] seer_stage Grade*[style=[background=lightgrey]]
	ER PR*[style=[background=lightgrey]] ERPR EOD10_PN2*[style=[background=lightgrey]]  HISTO3V*[style=[background=lightgrey]] BEHO3V event_chm*[style=[background=lightgrey]] event_hm Event_rx*[style=[background=lightgrey]] Event_sx, 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1) (  race3c NHIAREC *[style=[background=lightgreen]] kriegerSF3  *[style=[background=lightgreen]] sex )*(n='N'*F=comma8.0 colpctn='col%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table 2.1: Pathological and Clinical characteristics";
run;

proc tabulate missing data=all; 
class status    race3c NHIAREC kriegerSF3 fcds_site D_AJCC_S5  stage_T stage_N  stage_M seer_stage Grade ER PR ERPR  HISTO3V EOD10_NE3 BEHO3V sex 
	EOD10_PN2 event_chm event_hm Event_rx Event_sx   /s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue]; 
table all  stage_T stage_N*[style=[background=lightgrey]]  stage_M D_AJCC_S5*[style=[background=lightgrey]] seer_stage Grade*[style=[background=lightgrey]]
	ER PR*[style=[background=lightgrey]] ERPR EOD10_PN2*[style=[background=lightgrey]]   HISTO3V*[style=[background=lightgrey]] BEHO3V event_chm*[style=[background=lightgrey]] event_hm Event_rx*[style=[background=lightgrey]] Event_sx, 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1) ( race3c NHIAREC *[style=[background=lightgreen]] kriegerSF3  *[style=[background=lightgreen]] sex )*(n='N'*F=comma8.0 rowpctn='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table 2.2: Pathological and Clinical characteristics";
run;

ods tagsets.excelxp close;*/

/*proc freq data=all;table seer_stage D_AJCC_S2; run;
proc lifetest data=all plots=survival(atrisk=0 to 396 by 12);
      format seer_stage stage.;
      time surv_mon * Status(0);
	  strata kriegerSF3 / test=logrank adjust=sidak;
      where put(HISTO3V, hisG.) ='Kaposi Sarcoma';
run;
proc lifetest data=all plots=survival(atrisk=0 to 396 by 12);
      format seer_stage stage.;
      time surv_mon * Status(0);
      where put (HISTO3V, hisG.)='Non-Hodgkin Lymphoma';
run;
   proc lifetest data=all plots=survival(atrisk=0 to 396 by 12);
      format seer_stage stage.;
      time surv_mon * Status(0);
      strata seer_stage HISTO3V/ test=logrank adjust=sidak;
      run;
PROC PHREG data=analydat;
model inhosp*censor(0)=expose1-expose6
pwhsp status1 sex1 age1-age3 ms1
paygr1-paygr2 oc_cat1-oc_cat9 ccep
 / rl ties=efron ;
title1 'Cox regression with exposure
 status in the model';
run; 
%macro cox(data,var);
proc phreg data=&data;
   class &var ;
   model surv_mon*Status(0) = &var  ;
   hazardratio "Hazard Ratio &var." &var;
run;
%mend;
%let datal= ks nhl all;
%let varl= sex race3c NHIAREC kriegerSF3 age ins7cU;
%macro uni;
data _null_;
do i 1to 2;
 call symputx(scan( &datal.,i),data2);
   do t 1 to 6;
     call symputx(scan( &varl.,t),var2);
	 if _n_ >1 then
      %cox(&data2.,&var2.);
	  end;
end;
%mend;
%cox(ks,sex);
%cox(nhl,sex);
%cox(ks,race3c);
%cox(nhl,race3c);
%cox(ks,NHIAREC);
%cox(nhl,NHIAREC);
%cox(ks,kriegerSF3);
%cox(nhl,kriegerSF3);
%cox(ks,ins7cU);
%cox(nhl,ins7cU);
proc phreg data=all;

   class sex race3c NHIAREC kriegerSF3 age ins7cU;
   model surv_mon*Status(0) = sex race3c NHIAREC kriegerSF3 age ins7cU;
   *bayes seed=1  statistics=(summary interval);
   hazardratio 'Hazard Ratio Statement 1' sex;
   hazardratio 'Hazard Ratio Statement 2' age / unit=10;
   hazardratio 'Hazard Ratio Statement 3' race3c;
   hazardratio 'Hazard Ratio Statement 4' NHIAREC;
   hazardratio 'Hazard Ratio Statement 5' kriegerSF3;
hazardratio 'Hazard Ratio Statement 6' ins7cU;
run;*/
/*ods html;
proc freq data=nhl;
 format seer_stage stage.;
table age*seer_stage  /nocol nopct chisq;
*sex race3c NHIAREC kriegerSF3 ins7cU age;
run;*/
proc tabulate missing data=all;
class  status sex race3c NHIARECU kriegerSF3U  MAR_STAT4cU ins6c before_HARRT HISTO3V age
	/s=[cellwidth=2in font_weight=medium indent=0 just=c] style=[foreground=blue];
table all status*[style=[background=lightgrey]] sex*[style=[background=lightgrey]] race3c NHIARECU*[style=[background=lightgrey]] kriegerSF3U   MAR_STAT4cU*[style=[background=lightgrey]]
	ins6c /*[style=[background=lightgrey]]*/ before_HARRT*[style=[background=lightgrey]] age , 
		(all='All Patients n(%)')*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1) ( HISTO3V*[style=[background=lightgreen]])*(n='N'*F=comma8.0 colpctn='col%'*F=6.1  ROWPCTN ='row%'*F=6.1 )/s=[indent=0] nocellmerge;
	title "Table KS NHL: Demographic characteristics";
 run;
