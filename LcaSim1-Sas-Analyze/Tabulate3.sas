LIBNAME cond3 "C:\Documents and Settings\jjd264\My Documents\Paper-DziakCoffmanLanzaLi2011\LcaSim1-Sas3\Data";
%LET truesize = 3;

DATA temp1;  SET cond3.out1; WHERE log_likelihood>-99999999 & total_n < 3200; n = total_n; rep = rep; ll1 = log_likelihood;  gsq1 = g_squared;  aic1 = aic; abic1 = abic; caic1 = caic; bic1 = bic;  KEEP n rep ll1 gsq1 aic1 abic1 caic1 bic1;  RUN;
DATA temp2;  SET cond3.out2; WHERE log_likelihood>-99999999& total_n < 3200; n = total_n; rep = rep; ll2 = log_likelihood;  gsq2 = g_squared;  aic2 = aic; abic2 = abic; caic2 = caic; bic2 = bic;  KEEP n rep ll2 gsq2 aic2 abic2 caic2 bic2;  RUN;
DATA temp3;  SET cond3.out3; WHERE log_likelihood>-99999999& total_n < 3200; n = total_n; rep = rep; ll3 = log_likelihood;  gsq3 = g_squared;  aic3 = aic; abic3 = abic; caic3 = caic; bic3 = bic;  KEEP n rep ll3 gsq3 aic3 abic3 caic3 bic3;  RUN;
DATA temp4;  SET cond3.out4; WHERE log_likelihood>-99999999& total_n < 3200; n = total_n; rep = rep; ll4 = log_likelihood;  gsq4 = g_squared;  aic4 = aic; abic4 = abic; caic4 = caic; bic4 = bic;  KEEP n rep ll4 gsq4 aic4 abic4 caic4 bic4;  RUN;
DATA temp5;  SET cond3.out5; WHERE log_likelihood>-99999999& total_n < 3200; n = total_n; rep = rep; ll5 = log_likelihood;  gsq5 = g_squared;  aic5 = aic; abic5 = abic; caic5 = caic; bic5 = bic;  KEEP n rep ll5 gsq5 aic5 abic5 caic5 bic5;  RUN;
DATA temp6;  SET cond3.out6; WHERE log_likelihood>-99999999& total_n < 3200; n = total_n; rep = rep; ll6 = log_likelihood;  gsq6 = g_squared;  aic6 = aic; abic6 = abic; caic6 = caic; bic6 = bic;  KEEP n rep ll6 gsq6 aic6 abic6 caic6 bic6;  RUN;
DATA tempSSE; SET cond3.sse; WHERE rep > 0 & n < 3200; n = n; rep = rep; sse1=col1; sse2=col2; sse3=col3; sse4=col4; sse5=col5; sse6=col6; keep n rep sse1-sse6; run;

PROC SORT DATA=temp1; BY rep n; RUN;
PROC SORT DATA=temp2; BY rep n; RUN;
PROC SORT DATA=temp3; BY rep n; RUN;
PROC SORT DATA=temp4; BY rep n; RUN;
PROC SORT DATA=temp5; BY rep n; RUN;
PROC SORT DATA=temp6; BY rep n; RUN;
PROC SORT DATA=tempSSE; BY rep n; RUN;

DATA results;
	MERGE temp1 temp2 temp3 temp4 temp5 temp6;
	BY rep n ;
RUN;

DATA results; MERGE results tempSSE; BY rep n;  RUN;

PROC IML;
   USE results;
        READ ALL VAR {  rep n 
						 ll1 gsq1 aic1 abic1 caic1 bic1 
 						 ll2 gsq2 aic2 abic2 caic2 bic2 
						 ll3 gsq3 aic3 abic3 caic3 bic3 
						 ll4 gsq4 aic4 abic4 caic4 bic4 
						 ll5 gsq5 aic5 abic5 caic5 bic5 
						 ll6 gsq6 aic6 abic6 caic6 bic6 
                         sse1 sse2 sse3 sse4 sse5 sse6} ;
   CLOSE results;
   p1 = ((aic1-gsq1)/2)[1];
   p2 = ((aic2-gsq2)/2)[1];
   p3 = ((aic3-gsq3)/2)[1];
   p4 = ((aic4-gsq4)/2)[1];
   p5 = ((aic5-gsq5)/2)[1];
   p6 = ((aic6-gsq6)/2)[1]; 

   p1other = 6*1 + (1-1);  * rhos plus free gammas;
   p2other = 6*2 + (2-1);  * rhos plus free gammas;
   p3other = 6*3 + (3-1);  * rhos plus free gammas;
   p4other = 6*4 + (4-1);  * rhos plus free gammas;
   p5other = 6*5 + (5-1);  * rhos plus free gammas;
   p6other = 6*6 + (6-1);  * rhos plus free gammas;
   PRINT p1 p2 p3 p4 p5 p6 p1other p2other p3other p4other p5other p6other; 


   aic1a = aic1; aic2a = aic2; aic3a=aic3; aic4a=aic4; aic5a=aic5; aic6a=aic6;
   abic1a = abic1; abic2a = abic2; abic3a=abic3; abic4a=abic4; abic5a=abic5; abic6a=abic6;
   caic1a = caic1; caic2a = caic2; caic3a=caic3; caic4a=caic4; caic5a=caic5; caic6a=caic6;
   bic1a = bic1; bic2a = bic2; bic3a=bic3; bic4a=bic4; bic5a=bic5; bic6a=bic6; 

   aic1b = -2*ll1 + 2*p1; aic2b = -2*ll2 + 2*p2; aic3b = -2*ll3 + 2*p3; aic4b = -2*ll4 + 2*p4; aic5b = -2*ll5 + 2*p5; aic6b = -2*ll6 + 2*p6; 
   bic1b = -2*ll1 + log(n)*p1; bic2b = -2*ll2 + log(n)*p2; bic3b = -2*ll3 + log(n)*p3; bic4b = -2*ll4 + log(n)*p4; bic5b = -2*ll5 + log(n)*p5; bic6b = -2*ll6 + log(n)*p6; 
   caic1b = -2*ll1 + (log(n)+1)*p1; caic2b = -2*ll2 + (log(n)+1)*p2; caic3b = -2*ll3 + (log(n)+1)*p3; caic4b = -2*ll4 + (log(n)+1)*p4; caic5b = -2*ll5 + (log(n)+1)*p5; caic6b = -2*ll6 + (log(n)+1)*p6; 
   abic1b = -2*ll1 + ((n+2)/24)*p1; abic2b = -2*ll2 + ((n+2)/24)*p2; abic3b = -2*ll3 + ((n+2)/24)*p3; abic4b = -2*ll4 + ((n+2)/24)*p4; abic5b = -2*ll5 + ((n+2)/24)*p5; abic6b = -2*ll6 + ((n+2)/24)*p6; 
   check1 = aic1a-aic1b; 
   check2 = aic1a-aic1b; 
   check3 = aic1a-aic1b; 
   check4 = aic1a-aic1b; 
   check5 = aic1a-aic1b; 
   check6 = aic1a-aic1b; 	
   PRINT check1 check2 check3 check4 check5 check6 ;

   aicthree1b = -2*ll1 + 3*p1; aicthree2b = -2*ll2 + 3*p2; aicthree3b = -2*ll3 + 3*p3; aicthree4b = -2*ll4 + 3*p4; aicthree5b = -2*ll5 + 3*p5; aicthree6b = -2*ll6 + 3*p6;   

   tempmat = aic1a || aic2a || aic3a || aic4a || aic5a || aic6a;  tempmat2 = tempmat[,>:<];
   aicpicks = tempmat2;
   tempmat = aicthree1b || aicthree2b || aicthree3b || aicthree4b || aicthree5b || aicthree6b;  tempmat2 = tempmat[,>:<];
   aicthreepicks = tempmat2;
   tempmat = bic1a || bic2a || bic3a || bic4a || bic5a || bic6a;  tempmat2 = tempmat[,>:<];
   bicpicks = tempmat2;
   tempmat = abic1a || abic2a || abic3a || abic4a || abic5a || abic6a;  tempmat2 = tempmat[,>:<];
   abicpicks = tempmat2;
   tempmat = caic1a || caic2a || caic3a || caic4a || caic5a || caic6a;  tempmat2 = tempmat[,>:<];
   caicpicks = tempmat2;

   aicover  = aicpicks  > &truesize;
   aicthreeover  = aicthreepicks  > &truesize;
   bicover  = bicpicks  > &truesize;
   abicover = abicpicks > &truesize;
   caicover = caicpicks > &truesize;

   aicunder  = aicpicks  < &truesize;
   aicthreeunder  = aicthreepicks  < &truesize;
   bicunder  = bicpicks  < &truesize;
   abicunder = abicpicks < &truesize;
   caicunder = caicpicks < &truesize;

   aicunder  = aicpicks  < &truesize;
   aicthreeunder  = aicthreepicks  < &truesize;
   bicunder  = bicpicks  < &truesize;
   abicunder = abicpicks < &truesize;
   caicunder = caicpicks < &truesize;
   
   aicat  = aicpicks  = &truesize;
   aicthreeat  = aicthreepicks  = &truesize;
   bicat  = bicpicks  = &truesize;
   abicat = abicpicks = &truesize;
   caicat = caicpicks = &truesize;
 
   mse1 = (sse1/64);
   mse2 = (sse2/64);
   mse3 = (sse3/64);
   mse4 = (sse4/64);
   mse5 = (sse5/64);
   mse6 = (sse6/64);

  aicmse =  mse1 # (aicpicks=1)+
			mse2 # (aicpicks=2)+
			mse3 # (aicpicks=3)+
			mse4 # (aicpicks=4)+
			mse5 # (aicpicks=5)+
			mse6 # (aicpicks=6);
  aicthreemse =  mse1 # (aicthreepicks=1)+
			mse2 # (aicthreepicks=2)+
			mse3 # (aicthreepicks=3)+
			mse4 # (aicthreepicks=4)+
			mse5 # (aicthreepicks=5)+
			mse6 # (aicthreepicks=6);
  bicmse =  mse1 # (bicpicks=1)+
			mse2 # (bicpicks=2)+
			mse3 # (bicpicks=3)+
			mse4 # (bicpicks=4)+
			mse5 # (bicpicks=5)+
			mse6 # (bicpicks=6);
  abicmse = mse1 # (abicpicks=1)+
			mse2 # (abicpicks=2)+
			mse3 # (abicpicks=3)+
			mse4 # (abicpicks=4)+
			mse5 # (abicpicks=5)+
			mse6 # (abicpicks=6);
  caicmse = mse1 # (caicpicks=1)+
			mse2 # (caicpicks=2)+
			mse3 # (caicpicks=3)+
			mse4 # (caicpicks=4)+
			mse5 # (caicpicks=5)+
			mse6 # (caicpicks=6);
  print aicmse bicmse abicmse caicmse;

   CREATE picks VAR {   n rep 
						aicover  aicthreeover  bicover  abicover  caicover
						aicunder aicthreeunder bicunder abicunder caicunder 
						aicat    aicthreeat    bicat    abicat    caicat
                        aicmse  aicthreemse  bicmse  abicmse  caicmse };
	   APPEND;
   CLOSE picks;
QUIT;

PROC MEANS DATA=picks;
	CLASS n;
	VAR aicover  aicthreeover  bicover  abicover  caicover
		aicunder aicthreeunder bicunder abicunder caicunder 
		aicat    aicthreeat    bicat    abicat    caicat
        aicmse  aicthreemse  bicmse  abicmse  caicmse; 
	OUTPUT OUT=meanpicks;
RUN;
DATA meanpicks;
	SET meanpicks;
	WHERE _TYPE_=1 & _STAT_="MEAN";
RUN;
DATA temp_aic;	SET meanpicks; over=aicover; under=aicunder; at=aicat; mse=aicmse; stat="aic"; RUN;
DATA temp_aicthree;	SET meanpicks; over=aicthreeover; under=aicthreeunder; at=aicthreeat; mse=aicthreemse; stat="3ic"; RUN;
DATA temp_bic;	SET meanpicks; over=bicover; under=bicunder; at=bicat; mse=bicmse; stat="bic"; RUN;
DATA temp_abic;	SET meanpicks; over=abicover; under=abicunder; at=abicat; mse=abicmse; stat="abic"; RUN;
DATA temp_caic;	SET meanpicks; over=caicover; under=caicunder; at=caicat; mse=caicmse; stat="caic"; RUN;
DATA toplot; SET temp_aic temp_aicthree temp_bic temp_abic temp_caic; RUN;	
PROC GPLOT DATA=toplot;
	PLOT over*n=stat;
	PLOT under*n=stat;
	PLOT at*n=stat;	
	PLOT mse*n=stat;
    symbol interpol=join;
RUN;QUIT;
 
DATA meanpicks_out;
	SET meanpicks;
	KEEP N	AICOVER AICTHREEOVER	BICOVER	ABICOVER	CAICOVER	AICUNDER	AICTHREEUNDER	BICUNDER	ABICUNDER	CAICUNDER	AICAT	AICTHREEAT	BICAT	ABICAT	CAICAT	AICmse	AICTHREEmse	BICmse	ABICmse	CAICmse;
RUN;
PROC EXPORT DATA= meanpicks_out
            OUTFILE= "C:\Documents and Settings\jjd264\My Documents\Paper-DziakCoffmanLanzaLi2011\LcaSim1-Sas-Analyze\results3.txt"  
            DBMS=TAB REPLACE;
RUN;
