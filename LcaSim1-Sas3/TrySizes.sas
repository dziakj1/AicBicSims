%let path=   C:\Documents and Settings\jjd264\My Documents\Paper-DziakCoffmanLanzaLi2010\LcaSim1-Sas3\;
LIBNAME lca "C:\Documents and Settings\jjd264\My Documents\Paper-DziakCoffmanLanzaLi2010\LcaSim1-Sas3\Data";
%INCLUDE "&path\SimulateLcaDataset.sas";
%LET nrep =1000; 
%LET ncond=20; 
DATA lca.true_gamma;
    INPUT true_gamma;
    DATALINES;
    0.4925 
    0.2625 
    0.2450
    ;
RUN;
DATA lca.true_rho_yes;
    INPUT lc1 lc2 lc3;
    DATALINES;
       0.3300 0.8100 0.8081
       0.2000 0.8200 0.7169
       0.0100 0.2500 0.4133
       0.0300 0.0200 0.9098
       0.0000 0.0300 0.7683
       0.0400 0.3100 0.3420
    ;
RUN;

DATA lca.Out1; Log_Likelihood = -99999999; RUN;
DATA lca.Out2; Log_Likelihood = -99999999; RUN;
DATA lca.Out3; Log_Likelihood = -99999999; RUN;
DATA lca.Out4; Log_Likelihood = -99999999; RUN;
DATA lca.Out5; Log_Likelihood = -99999999; RUN;
DATA lca.Out6; Log_Likelihood = -99999999; RUN;
DATA lca.sse;  Log_Likelihood = -99999999; RUN;

%MACRO SimulateLca; 
    %DO condition = 1 %TO &ncond;
        DATA _NULL_;
            condition = &condition;
            n_total = (condition-1) * 100;
            IF condition = 1 THEN n_total = 50;
            CALL SYMPUT('n_total',n_total);
        RUN;
        %DO rep = 1 %TO %EVAL(&nrep);
            %SimulateLcaDataset(  true_gamma_dataset = lca.true_gamma,
                                  true_rho_dataset = lca.true_rho_yes,    
                                  output_dataset_name=lca.sim_data,
                                  total_n=&n_total );
            %DO size_index = 1 %TO 6;
                PROC LCA DATA=lca.sim_data OUTEST=lca.outest&size_index OUTPARAM=lca.outpar&size_index NOPRINT;
                    NCLASS &size_index;
                    ITEMS Item001 Item002 Item003 Item004 Item005 Item006;
                    FREQ Count;
                    CATEGORIES 2 2 2 2 2 2; 
                    SEED 1000;
                    NSTARTS 20;
                RUN;
                DATA lca.temp;
                    SET lca.outest&size_index;
                    rep = &rep;
                    total_n = &n_total;
                RUN;
                DATA lca.Out&size_index;
                    SET lca.Out&size_index lca.temp; 
                RUN;
            %END;
        PROC IML; 
           /* Get the true rhos and gammas */ 
            USE lca.true_gamma;
                READ ALL INTO true_gamma; 
            CLOSE lca.true_gamma;
            USE lca.true_rho_yes;
                READ ALL INTO true_rho_yes;
            CLOSE lca.true_rho_yes; 
           /* Get the estimated rhos and gammas */
           USE lca.outpar1;
                READ ALL VAR { estlc1 } INTO est1;
           CLOSE lca.outpar1; 
           USE lca.outpar2;
                READ ALL VAR { estlc1 estlc2 } INTO est2;
           CLOSE lca.outpar2; 
           USE lca.outpar3;
                READ ALL VAR { estlc1 estlc2 estlc3  } INTO est3;
           CLOSE lca.outpar3; 
           USE lca.outpar4;
                READ ALL VAR { estlc1 estlc2 estlc3 estlc4 } INTO est4;
           CLOSE lca.outpar4; 
           USE lca.outpar5;
                READ ALL VAR { estlc1 estlc2 estlc3 estlc4 estlc5  } INTO est5;
           CLOSE lca.outpar5; 
           USE lca.outpar6;
                READ ALL VAR { estlc1 estlc2 estlc3 estlc4 estlc5 estlc6 } INTO est6;
           CLOSE lca.outpar6; 
           /* Get the true cell probabilities */
           n_items = 6;
           all_cells = COLVEC(REPEAT({1 2}`,1,(2**(n_items-1))));
           DO i = 1 TO (n_items - 1);
              temp = COLVEC(REPEAT({1 2}`,2**(i),(2**(n_items-i-1))));
              all_cells = all_cells || temp;
           END;
           n_true_classes = NCOL(true_rho_yes);
           true_probs_given_class = J(2**n_items,n_true_classes,0);
           DO cell_index = 1 TO (2**n_items);
              DO class_index = 1 TO n_true_classes;
                m1 = (true_rho_yes[,class_index]) || (1-true_rho_yes[,class_index]);
                m2 = (all_cells[cell_index,]=1)` || (all_cells[cell_index,]=2)`;
                m3 = m1#m2;
                m4 = m3[,+]+1E-20;
                true_probs_given_class[cell_index,class_index] = EXP(SUM(LOG(m4)));
              END;
            END;
            true_probs = J(2**n_items,1,0);
            DO class_index = 1 TO n_true_classes;
                true_probs = true_probs + true_gamma[class_index]*true_probs_given_class[,class_index];
             END;  
            /* Get the true cell probabilities for each class*/
            %DO    size_index = 1 %TO 6;  
               estimated_gamma = est&size_index[1,];
               estimated_rho_yes = est&size_index[2:(n_items+1),];
               estimated_probs_given_class = J(2**n_items,&size_index,0);
               DO cell_index = 1 TO (2**n_items);
                  DO class_index = 1 TO &size_index;
                    m1 = (estimated_rho_yes[,class_index]) || (1-estimated_rho_yes[,class_index]);
                    m2 = (all_cells[cell_index,]=1)` || (all_cells[cell_index,]=2)`;
                    m3 = m1#m2;
                    m4 = m3[,+]+1E-20;
                    estimated_probs_given_class[cell_index,class_index] = EXP(SUM(LOG(m4)));
                  END;
                END;
                estimated_probs&size_index = J(2**n_items,1,0);
                DO class_index = 1 TO &size_index;
                    estimated_probs&size_index = estimated_probs&size_index + estimated_gamma[class_index]*estimated_probs_given_class[,class_index];
                END;   
                sqd_errors&size_index = (estimated_probs&size_index - true_probs)##2;
                sum_sqd_errors&size_index = sqd_errors&size_index[+];
            %END;
            sum_sqd_errors_by_size = sum_sqd_errors1  // sum_sqd_errors2  //sum_sqd_errors3  //sum_sqd_errors4  //sum_sqd_errors5 // sum_sqd_errors6 ;
            sum_sqd_errors_by_size = sum_sqd_errors_by_size `;
            CREATE lca.sum_sqd_errors_by_size FROM sum_sqd_errors_by_size;
                APPEND FROM sum_sqd_errors_by_size;
            CLOSE lca.sum_sqd_errors_by_size;
        QUIT; 
		DATA temp;
			SET lca.sum_sqd_errors_by_size;
            rep = &rep;
            n = &n_total;
		RUN;			
        DATA lca.sse;
            SET lca.sse temp;
        RUN;
        %END;
    %END;
%MEND;
%SimulateLca;


