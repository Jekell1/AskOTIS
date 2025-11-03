      * COPYMEMBER: LIBLP/LPLCGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LCFILE
      *
      *=================================================================
      * REV:
      * JKC 2021-0505 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LC-REC FROM QLC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LC-FIELDS.
           MOVE QLC-CODE TO LC-CODE.

           IF ( QLC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LC-LTOUCH-DATE
           ELSE
              MOVE QLC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LC-LTOUCH-DATE.

           MOVE QLC-DESC TO LC-DESC.
           MOVE QLC-SMALLOAN TO LC-SMALLOAN.
           MOVE QLC-SALEFIN TO LC-SALEFIN.
           MOVE QLC-SERVACCT TO LC-SERVACCT.
           MOVE QLC-LOANTYPE TO LC-LOANTYPE.
           MOVE QLC-PROF1 TO LC-PROF1.
           MOVE QLC-PROF2 TO LC-PROF2.
           MOVE QLC-SKIP-TRW TO LC-SKIP-TRW.
           MOVE QLC-DISPLAY-ERN TO LC-DISPLAY-ERN.
           MOVE QLC-LCARDFG TO LC-LCARDFG.
           MOVE QLC-CALC-HI-CREDIT TO LC-CALC-HI-CREDIT.
           MOVE QLC-STOP-ON-DLNO TO LC-STOP-ON-DLNO.
           MOVE QLC-SALES-FORCED-ENTRY TO LC-SALES-FORCED-ENTRY.
           MOVE QLC-AFP-CLASS TO LC-AFP-CLASS.
           MOVE QLC-CON-PDUE-DAYS TO LC-CON-PDUE-DAYS.
           MOVE QLC-REC-PDUE-DAYS TO LC-REC-PDUE-DAYS.
           MOVE QLC-GIFTNET TO LC-GIFTNET.
           MOVE QLC-APP-REQ-FOR-LOAN TO LC-APP-REQ-FOR-LOAN.
           MOVE QLC-POOLID TO LC-POOLID.
           MOVE QLC-USE-ALT-GL-FEE TO LC-USE-ALT-GL-FEE.
           MOVE QLC-PROGRP-CLASS TO LC-PROGRP-CLASS.
           MOVE QLC-ALLOW-CASH-PROCEEDS TO LC-ALLOW-CASH-PROCEEDS.
 
      *-----------------------------------------------------------------
      *    LOAD QLC-REC FROM LC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LC-FIELDS.
           MOVE LC-CODE TO QLC-CODE.

           IF ( LC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLC-LTOUCH-DATE
           ELSE
              MOVE LC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLC-LTOUCH-DATE.

           MOVE LC-DESC TO QLC-DESC.
           MOVE LC-SMALLOAN TO QLC-SMALLOAN.
           MOVE LC-SALEFIN TO QLC-SALEFIN.
           MOVE LC-SERVACCT TO QLC-SERVACCT.
           MOVE LC-LOANTYPE TO QLC-LOANTYPE.
           MOVE LC-PROF1 TO QLC-PROF1.
           MOVE LC-PROF2 TO QLC-PROF2.
           MOVE LC-SKIP-TRW TO QLC-SKIP-TRW.
           MOVE LC-DISPLAY-ERN TO QLC-DISPLAY-ERN.
           MOVE LC-LCARDFG TO QLC-LCARDFG.
           MOVE LC-CALC-HI-CREDIT TO QLC-CALC-HI-CREDIT.
           MOVE LC-STOP-ON-DLNO TO QLC-STOP-ON-DLNO.
           MOVE LC-SALES-FORCED-ENTRY TO QLC-SALES-FORCED-ENTRY.
           MOVE LC-AFP-CLASS TO QLC-AFP-CLASS.
           MOVE LC-CON-PDUE-DAYS TO QLC-CON-PDUE-DAYS.
           MOVE LC-REC-PDUE-DAYS TO QLC-REC-PDUE-DAYS.
           MOVE LC-GIFTNET TO QLC-GIFTNET.
           MOVE LC-APP-REQ-FOR-LOAN TO QLC-APP-REQ-FOR-LOAN.
           MOVE LC-POOLID TO QLC-POOLID.
           MOVE LC-USE-ALT-GL-FEE TO QLC-USE-ALT-GL-FEE.
           MOVE LC-PROGRP-CLASS TO QLC-PROGRP-CLASS.
           MOVE LC-ALLOW-CASH-PROCEEDS TO QLC-ALLOW-CASH-PROCEEDS.
 
