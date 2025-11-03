      * COPYMEMBER: LIBLP/LP01LC_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN CLASS FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QLC1-WHERE-BEG-KEY.
           03  QLC1-WBEG-CODE          PIC 9(03).

       01  QLC1-WHERE-END-KEY.
           03  QLC1-WEND-CODE          PIC 9(03).

       01  QLC-REC.
           03  QLC-CODE                    PIC 9(3).
           03  QLC-LTOUCH-DATE             PIC X(10).
           03  QLC-DESC                    PIC X(30).
           03  QLC-SMALLOAN                PIC X.
           03  QLC-SALEFIN                 PIC X.
           03  QLC-SERVACCT                PIC X.
           03  QLC-LOANTYPE                PIC X.
           03  QLC-PROF1                   PIC X.
           03  QLC-PROF2                   PIC X.
           03  QLC-SKIP-TRW                PIC X.
           03  QLC-DISPLAY-ERN             PIC X.
           03  QLC-LCARDFG                 PIC X.
           03  QLC-CALC-HI-CREDIT          PIC X.
           03  QLC-STOP-ON-DLNO            PIC X.
           03  QLC-SALES-FORCED-ENTRY      PIC X.
           03  QLC-AFP-CLASS               PIC X.
           03  QLC-CON-PDUE-DAYS           PIC X.
           03  QLC-REC-PDUE-DAYS           PIC X.
           03  QLC-GIFTNET                 PIC X.
           03  QLC-APP-REQ-FOR-LOAN        PIC X.
           03  QLC-POOLID                  PIC 9(5).
           03  QLC-USE-ALT-GL-FEE          PIC X.
           03  QLC-PROGRP-CLASS            PIC 9(3).
           03  QLC-ALLOW-CASH-PROCEEDS     PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
