      * COPYMEMBER: LIBSP/SP01SD
      ******************************************************************
      *                                                 
      *        WORLD STANDARDS FILE                  
      *           DATA/R?/SP/SDFILE
      *                                                 
      * 7/16/03 NOTES: 
      *         ORIGINALLY WE THOUGHT THIS MIGHT BE BY STATE, OR AT
      *         LEAST HAVE EXCEPTIONS (FOR EX. READ FIRST BY BR #, IF
      *         NO COMMISSION RECORD DEFAULT TO THE STATE).  NOW SANDY
      *         SAYS WILL ALWAYS BE BY STATE, WITH NO EXCEPTIONS EVEN
      *         FOR DE NOVO BRANCHES (BUT I'M LEAVING STATE DEFINED AS
      *         X(4) IN CASE WE EVER NEED TO HAVE BY BR # OR GROUP).
      *         
      *         RATING SHEET PROFILE COMMISSION:         
      *         ALLOWING UP TO 12 BREAKS FOR COMMISSIONS; HIGHEST
      *         LEVEL WOULD BE INDICATED BY UPPER-LIMIT OF 9999.99
      *         THE FIELD SD-UPPER-LIMIT IS THE UPPER BREAK FOR EACH
      *         LEVEL OF PERCENT OF STANDAND.
      *         EACH COMMISSION LEVEL HAS 3 COMMISSION PERCENTS
      *         (SD-COMM-PERCENT) WHICH CORRESPOND TO A LIMIT OF
      *         3 DELINQUENCY PERCENTS (SD-DEL-PERCENT).       
      *          
      * REV:                                            
      *  JTG 030821 CHANGED SD-STD-PCT-ACCT-RETENTION FROM  PIC 99V99    COMP-6
      *                                               TO    PIC 999V9    COMP-6
      *  CS  030822 ADDED SD-STD-SALES-PER-MONTH-WCBC
      *  JTG 040227 ADDED SD-STD-GROWTH                            PER JR/BARB
      *  BLV 040430 INCREASE SD-STD-GROWTH FROM 99V99 TO 9999V99
      *  BLM 160408 ADD SD-LOANTYP-TOTAL, VALUE OF "T", FOR BONUS1 GAIN AND
      *             TAX RETURN STANDARDS, WORLD PR# 1149
      *  BLM 160413 REDEFINES LOAN VOLUME TABLE WITH STD-ACCT-GAIN TABLE,
      *             WORLD PR# 1149
      *************************************************************************
       78  SD-SIZE                           VALUE 143.
       01  SD-REC.
           03  SD1-KEY.
               05  SD-STATE                  PIC X(4).
               05  SD-LOANTYPE               PIC X.
                   88  SD-LOANTYPE-VALID     VALUES "S" "L" "W" "T".
                   88  SD-LOANTYPE-SMALL     VALUE  "S".
                   88  SD-LOANTYPE-LARGE     VALUE  "L".
                   88  SD-LOANTYPE-WCBC      VALUE  "W".
                   88  SD-LOANTYPE-TOTAL     VALUE  "T".
               05  SD-SEQ                    PIC 99.
           03  SD-EFFDATE                    PIC 9(6)     COMP-6.
           03  SD-EXPDATE                    PIC 9(6)     COMP-6.

      *        STANDARDS
           03  SD-STANDARDS.
               05  SD-STD-POSTAGE-PER-ACCT       PIC 99V99    COMP-6.
               05  SD-STD-OFFICE-PER-ACCT        PIC 99V99    COMP-6.
               05  SD-STD-TOT-DEL                PIC 99V99    COMP-6.
               05  SD-STD-PCT-OPENING-30-DAY     PIC 99V99    COMP-6.
               05  SD-STD-PCT-OPENING-INCREASE   PIC 99V99    COMP-6.
               05  SD-STD-PCT-SLOW               PIC 99V99    COMP-6.
               05  SD-STD-GROSS-BAD-DEBT         PIC 99V99    COMP-6.
               05  SD-STD-MONTHLY-BAD-DEBT       PIC 99V99    COMP-6.
               05  SD-STD-ANNUAL-BAD-DEBT        PIC 99V99    COMP-6.
               05  SD-STD-MONTHLY-PROFIT-PCT     PIC 99V99    COMP-6.
               05  SD-STD-ANNUAL-PROFIT-PCT      PIC 99V99    COMP-6.
               05  SD-STD-PCT-REFER-PER-ACCT     PIC 99V99    COMP-6.
               05  SD-STD-PCT-ACCT-RETENTION     PIC 999V9    COMP-6.
               05  SD-STD-MONTHLY-LOAN-VOLUME    OCCURS 12.
                   07  SD-STD-PCT-LOAN-VOLUME    PIC 99V99    COMP-6.
                   07  SD-STD-ACCT-GAIN REDEFINES
                       SD-STD-PCT-LOAN-VOLUME    PIC 9(4)     COMP-6.
               05  SD-STD-SALES-PER-MONTH-WCBC   PIC 99       COMP-6.
               05  SD-STD-GROWTH                 PIC 9999V99  COMP-6.
           03  FILLER                        PIC X(76).
