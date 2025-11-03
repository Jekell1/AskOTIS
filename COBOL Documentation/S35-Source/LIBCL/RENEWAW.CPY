      * COPYMEMBER: LIBCL/RENEWAW
      ****************************************************
      *    RENEWAW
      *    WORKERS FOR FINDING OLD LOANS, TESTING IF PAIDOUT SAME
      *     DAY AS TRYING TO MAKE NEW LOAN
      * REV:
      *  110118 BAH NEW FOR WORLD, IL PR# 734
      *  170404 BAH CHANGED RENEWA-LOANDATE FROM 6 TO 8
      ****************************************************
       01  RENEWA-WORKERS.
      * IN
           03  RENEWA-LOANDATE          PIC 9(8).
           03  RENEWA-SSNO OCCURS 4 TIMES PIC 9(9).
      * OUT
           03  RENEWA-STOP-LOAN         PIC X.
