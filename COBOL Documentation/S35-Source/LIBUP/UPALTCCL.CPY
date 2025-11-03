       CLEAR-ALTC-FILE SECTION.
           MOVE ALL X"09" TO ALTC-REC.

           MOVE 0 TO ALTC-LTOUCH-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE 0 TO ALTC-MILEAGE-1  ALTC-MILEAGE-2  ALTC-MILEAGE-3
                        ALTC-VALUE-1    ALTC-VALUE-2    ALTC-VALUE-3
                        ALTC-SELLPR-1   ALTC-SELLPR-2   ALTC-SELLPR-3
                        ALTC-MISC-AMT-1 ALTC-MISC-AMT-2 ALTC-MISC-AMT-3
                        ALTC-VIN-CHG-DATE-1 ALTC-VIN-CHG-DATE-2
                        ALTC-VIN-CHG-DATE-3
              MOVE SPACES TO ALTC-HHGS(SUB)
           END-PERFORM.
         
           MOVE SPACES TO ALTC-DESC-1 ALTC-MAKE-1 ALTC-MODEL-1          
                          ALTC-YEAR-1 ALTC-STYLE-1 ALTC-CLASS-1          
                          ALTC-VIN-1 ALTC-TITLE-NO-1 ALTC-SENT-FLAG-1.
           MOVE SPACES TO ALTC-DESC-2 ALTC-MAKE-2 ALTC-MODEL-2          
                          ALTC-YEAR-2 ALTC-STYLE-2 ALTC-CLASS-2          
                          ALTC-VIN-2 ALTC-TITLE-NO-2 ALTC-SENT-FLAG-2.
           MOVE SPACES TO ALTC-DESC-3 ALTC-MAKE-3 ALTC-MODEL-3          
                          ALTC-YEAR-3 ALTC-STYLE-3 ALTC-CLASS-3          
                          ALTC-VIN-3 ALTC-TITLE-NO-3 ALTC-SENT-FLAG-3.

