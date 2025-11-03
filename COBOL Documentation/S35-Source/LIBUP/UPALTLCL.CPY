       CLEAR-ALTL-FILE SECTION.
           MOVE ALL X"09" TO ALTL-REC.

           MOVE 0 TO ALTL-LTOUCH-DATE.

           PERFORM VARYING LTLSUB FROM 1 BY 1 UNTIL LTLSUB > 3
              MOVE 0 TO ALTL-LIEN-DATE(LTLSUB)
                        ALTL-LIEN-TYPE(LTLSUB)
           END-PERFORM.

           MOVE 0 TO ALTL-TITLE-SEND-FMNO
                     ALTL-TITLE-DATE
                     ALTL-TITLE-LIEN-DATE ALTL-TITLE-RCV-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE SPACES TO ALTL-LIEN-NAME(SUB)
                             ALTL-LIEN-ADR(SUB)
                             ALTL-LIEN-CITY(SUB)
                             ALTL-LIEN-STATE(SUB)
                             ALTL-LIEN-ZIP(SUB)
           END-PERFORM.

           MOVE SPACES TO ALTL-TITLE-NO ALTL-LIEN-LETTER-SENT   
                          ALTL-LIEN-COMMENT1 ALTL-LIEN-COMMENT2      
                          ALTL-LIEN-TITLE-STATE ALTL-TITLE-STATUS-CODE  
                          ALTL-TITLE-SEND-FLAG ALTL-TITLE-NAME         
                          ALTL-TITLE-ADR ALTL-TITLE-CITY
                          ALTL-TITLE-STATE 
                          ALTL-TITLE-ZIP ALTL-TITLE-SENT-BY      
                          ALTL-SEND-ASAP ALTL-TITLE-ADR2
                          ALTL-TITLE-COMMENT1
                          ALTL-TITLE-COMMENT2 ALTL-RELEASE-TYPE       
                          ALTL-CARRIER-TYPE ALTL-CARRIER-ACCTNO.

