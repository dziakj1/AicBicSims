LIBNAME cond3 "C:\Documents and Settings\jjd264\My Documents\Paper-DziakCoffmanLanzaLi2011\LcaSim1-Sas4\Data";
%LET truesize = 4;

DATA tempSSE; SET cond3.sse; WHERE rep > 0 & n < 3200; n = n; rep = rep; sse1=col1; sse2=col2; sse3=col3; sse4=col4; sse5=col5; sse6=col6; keep sse1-sse6 n rep; run;

PROC REG DATA=cond3.sse;
	MODEL col4 = n;
RUN;
PROC REG DATA=tempSSE;
	MODEL sse4=n;
RUN;

DATA tempmse; 
	SET tempSSE;
	mse1 = (sse1/64);
	mse2 = (sse2/64);
	mse3 = (sse3/64);
	mse4 = (sse4/64);
	mse5 = (sse5/64);
	mse6 = (sse6/64);
	KEEP n sse1-sse6 mse1-mse6;
RUN;

PROC MEANS DATA=tempmse;
	CLASS n;
	VAR mse1 mse2 mse3 mse4 mse5 mse6; 
	OUTPUT OUT=meanmse;
RUN;
DATA meanmse;
	SET meanmse;
	WHERE _TYPE_=1 & _STAT_="MEAN";
RUN;


PROC EXPORT DATA= meanmse
            OUTFILE= "C:\Documents and Settings\jjd264\My Documents\Paper-DziakCoffmanLanzaLi2011\LcaSim1-Sas-Analyze\meanmse4.txt"  
            DBMS=TAB REPLACE;
RUN;
 
