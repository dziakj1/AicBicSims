%MACRO SimulateLcaDataset(  true_gamma_dataset=,
                            true_rho_dataset=, 
                            output_dataset_name=, 
                            total_n= ); 
    PROC IML;   
        USE &true_gamma_dataset;
            READ ALL INTO true_gamma;
        CLOSE &true_gamma_dataset;
        USE &true_rho_dataset;
            READ ALL INTO true_rho_yes;
        CLOSE &true_rho_dataset;
        total_n = &total_n;
        true_nclass = NCOL(true_rho_yes); 
        n_items = NROW(true_rho_yes); 
        ***************************************************** ;
        items = ('Item' + CHAR((1:n_items),3)); 
                                        * assumes < 1000 items;
        CALL CHANGE(items," ","0",0);
        true_class = J(&total_n,1,0);  
        DO this_person = 1 TO &total_n ;
            temp = 0;
            CALL RANDGEN(temp, 'TABLE', true_gamma);
            true_class[this_person] = temp;
        END;
        responses = J(&total_n, n_items, 0);
        DO this_person = 1 TO &total_n ;
            DO this_item = 1 TO n_items;
                temp = 0;
                this_persons_class = true_class[this_person];
                CALL RANDGEN(temp, 'BERNOULLI', 
                        true_rho_yes[this_item,this_persons_class]);  
                     responses[this_person,this_item] = 2 - temp;     
            END;
        END;
        ResponsePatterns = COLVEC(REPEAT({1 2}`,1,(2**(n_items-1))));
        DO i = 1 TO (n_items - 1);
            temp = COLVEC(REPEAT({1 2}`,2**(i),(2**(n_items-i-1))));
            ResponsePatterns = ResponsePatterns || temp;
        END;
        Count = J((2**n_items),1,0);
        DO pattern_index = 1 TO (2**n_items);
            Matches = J(&total_n,1,1);
            DO i = 1 TO n_items;
                temp = (responses[,i]=ResponsePatterns[pattern_index,i]);
                Matches = Matches # temp;
            END;
            Count[pattern_index] = Matches[+];
        END;  
         responses_matrix =  ResponsePatterns || Count; 
        cols = items || 'Count'; 
        CREATE &output_dataset_name FROM responses_matrix [ COLNAME = cols ];
            APPEND  FROM responses_matrix; 
        CLOSE &output_dataset_name;
    QUIT;
    DATA &output_dataset_name;
        SET &output_dataset_name;
        WHERE Count > 0; * omit non-occurring patterns;
    RUN; 
    QUIT;
%MEND;

