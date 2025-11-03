      ***********************************************
      *    GET EXTENSION DAYS
      ***********************************************
       REBATE-GET-EXTDAYS SECTION.
           MOVE SP-PYEXYEARTP TO ELAPSED-YRTYPE.
           MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE.
           IF SP-PYEXFMDATE-LOAN-DT
              MOVE REB-LN-LOANDATE TO NUM-DATE
           ELSE
              MOVE REB-LN-INTDATE TO NUM-DATE.
           PERFORM TIMALL.
           SUBTRACT 30 FROM ELAPSED-DAYS GIVING REB-TOTAL-EXTDAYS.

      ***********************************************************
      *    FIND INSURANCE RATE FROM SPR RATE TABLE
      *              USING INTERPOLATION / OR NEXT HIGH
      *                       (A&H)
      *
      *    IN  - REB-INSSUB       = SPR INSURANCE LEVEL
      *    OUT - REB-HUNDREDS
      *                AND        = RATE FOR REMAINING INS. TERM
      *          REB-MANYDECIMALS
      * REV:
      *  JTG 113094 CHANGED TO PASS RATE BACK WITH 4 DECIMALS
      *             SP-RBFRMLA = 'R' FOR TIME LOAN, CA AH REFUND
      *  JTG 070595 CHANGED TO USE INTERPOLATION OR NEXT HIGH
      *             RE: SPR
      *  JTG 071295 CHANGED TO HANDLE 7 OTHER INSURANCES
      ***********************************************************
