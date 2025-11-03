      * COPYMEMBER: LIBSP/SP01PT
      ******************************************************************
      *                                                 
      *        WORLD PROFICIENCY POINTS/COMMISSION TABLES FILE                  
      *           DATA/R?/SP/PTFILE
      *                                                 
      *         NOTES: 
      *         
      *         RATING SHEET PROFILE COMMISSION:         
      *         ALLOWING UP TO 12 BREAKS FOR COMMISSIONS; HIGHEST
      *         LEVEL WOULD BE INDICATED BY UPPER-LIMIT OF 9999.99
      *         THE FIELD PT-UPPER-LIMIT IS THE UPPER BREAK FOR EACH
      *         LEVEL OF PERCENT OF STANDAND.
      *         EACH COMMISSION LEVEL HAS 3 COMMISSION PERCENTS
      *         (PT-COMM-PERCENT) WHICH CORRESPOND TO A LIMIT OF
      *         3 DELINQUENCY PERCENTS (PT-DEL-PERCENT).       
      *          
      * REV:                                            
      *  CS  030813 CHANGED PT-PERCENT-SLOW-TABLE AND PT-PERCENT-BAD-DEBT
      *             TABLE TO OCCURS 10
      ******************************************************************
       78  PT-SIZE                           VALUE 537.
       01  PT-REC.
           03  PT1-KEY.
               05  PT-STATE                  PIC X(4).
               05  PT-SEQ                    PIC 99.
           03  PT-EFFDATE                    PIC 9(6)     COMP-6.
           03  PT-EXPDATE                    PIC 9(6)     COMP-6.

      *        RATING SHEET PROFIT COMMISSIONS:         
           03  PT-COMMISSION-TABLE           OCCURS 12 TIMES.
               05  PT-COMM-PCT-PROFIT-LIMIT  PIC 9(4)V99  COMP-6.
               05  PT-COMM-PERCENT           OCCURS 3 TIMES
                                             PIC 99V99    COMP-6.
           03  PT-DEL-LEVEL                  OCCURS 3 TIMES
                                             PIC 99V99    COMP-6.

      *        PROFICIENCY RATING POINTS:
           03  PT-PERCENT-PROFIT-TABLE       OCCURS 14 TIMES.
               05  PT-PCT-PROFIT-LIMIT       PIC 9(4)V99  COMP-6.
               05  PT-PROFIT-POINTS          PIC 9(4)     COMP-6.
           03  PT-PERCENT-PROFIT-CAP         PIC 99       COMP-6.

           03  PT-PERCENT-DELINQ-TABLE       OCCURS 10 TIMES.
               05  PT-PCT-DELINQ-LIMIT       PIC 9(4)V99  COMP-6.
               05  PT-DELINQ-POINTS          PIC 9(4)     COMP-6.
           03  PT-PERCENT-DELINQ-CAP         PIC 99       COMP-6.

           03  PT-PERCENT-VOLUME-TABLE       OCCURS 14 TIMES.
               05  PT-PCT-VOLUME-LIMIT       PIC 9(4)V99  COMP-6.
               05  PT-VOLUME-POINTS          PIC 9(4)     COMP-6.
           03  PT-PERCENT-VOLUME-CAP         PIC 99       COMP-6.

           03  PT-PERCENT-GAIN-TABLE         OCCURS 7 TIMES.
               05  PT-PCT-GAIN-LIMIT         PIC 9(4)V99  COMP-6.
               05  PT-GAIN-POINTS            PIC 9(4)     COMP-6.
           03  PT-PERCENT-GAIN-CAP           PIC 99       COMP-6.
 
           03  PT-PERCENT-SLOW-TABLE         OCCURS 10 TIMES.
               05  PT-PCT-SLOW-LIMIT         PIC 9(4)V99  COMP-6.
               05  PT-SLOW-POINTS            PIC 9(4)     COMP-6.
           03  PT-PERCENT-SLOW-CAP           PIC 99       COMP-6.

           03  PT-PERCENT-BAD-DEBT-TABLE     OCCURS 10 TIMES.
               05  PT-PCT-BAD-DEBT-LIMIT     PIC 9(4)V99  COMP-6.
               05  PT-BAD-DEBT-POINTS        PIC 9(4)     COMP-6.
           03  PT-PERCENT-BAD-DEBT-CAP       PIC 99       COMP-6.

           03  FILLER                        PIC X(80).
