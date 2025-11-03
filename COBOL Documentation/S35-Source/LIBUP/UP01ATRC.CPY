      * COPYMEMBER: LIBUP/UP01ATRC.CPY
      ******************************************************************
      *
      * DESCRIPTION:  ASCII TRANSACTION FILE [CHECKS/CASH]
      *
      * FILE INFO  :  CHECKS/CASH
      *               LP/TRCFILE
      *
      * 03/05/2021 BAH ADDED TC-EXTRACT-FG, TC-DEBIT-REFCD, TC-FUTURE-ALPHA5
      *                                                     #1490
      ******************************************************************
       78  ATRC-SIZE                    VALUE 469.
       01  ATRC-REC.
           03  ATC-BRNO             PIC 9(4).
           03  FILLER               PIC X.
           03  ATC-KEY-DATE         PIC 9(8).
           03  FILLER               PIC X.
           03  ATC-CKNO             PIC 9(6).
           03  FILLER               PIC X.
           03  ATC-CLASS            PIC 9(3). 
           03  FILLER               PIC X.
           03  ATC-SEQ              PIC 9(5).
           03  FILLER               PIC X.
           03  ATC-LTOUCH-DATE      PIC 9(8).
           03  FILLER               PIC X.
           03  ATC-TYPE             PIC X(2).
           03  FILLER               PIC X.
           03  ATC-DATE             PIC 9(8).
           03  FILLER               PIC X.
           03  ATC-AMT              PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATC-PAYEE            PIC X(60).
           03  FILLER               PIC X.
           03  ATC-REF              PIC 9(6).
           03  FILLER               PIC X.
           03  ATC-REASON           PIC X(30).
           03  FILLER               PIC X.
           03  ATC-GLDIST           OCCURS 5.
               05  ATC-GLNO         PIC 9(11).
               05  FILLER           PIC X.
               05  ATC-GLAMT        PIC +9(7).99.
               05  FILLER           PIC X.
           03  ATC-VOID             PIC X.
           03  FILLER               PIC X.
           03  ATC-ACCTNO           PIC 9(8).
           03  FILLER               PIC X.
           03  ATC-PAID             PIC X.
           03  FILLER               PIC X.
           03  ATC-PGM              PIC XX.
           03  FILLER               PIC X.
           03  ATC-DLRVNO           PIC 9.
           03  FILLER               PIC X.
           03  ATC-SPRCLASS         PIC 999.
           03  FILLER               PIC X.
           03  ATC-DLNO             PIC 9(4).99.
           03  FILLER               PIC X.
           03  ATC-USERID           PIC X(10).
           03  FILLER               PIC X.
           03  ATC-POSTDATE         PIC 9(8).
           03  FILLER               PIC X.
           03  ATC-POSTTIME         PIC 9(6).
           03  FILLER               PIC X.
           03  ATC-TILLNO           PIC 9(2).
           03  FILLER               PIC X.
           03  ATC-ADR1             PIC X(30).
           03  FILLER               PIC X.
           03  ATC-ADR2             PIC X(30).
           03  FILLER               PIC X.
           03  ATC-CITY             PIC X(30).
           03  FILLER               PIC X.
           03  ATC-STATE            PIC X(2).
           03  FILLER               PIC X.
           03  ATC-ZIP              PIC X(10).
           03  FILLER               PIC X.
           03  ATC-PAYEE-NO         PIC 9(6).
           03  FILLER               PIC X.
           03  ATC-ACH-FG           PIC X.
           03  FILLER               PIC X.
           03  ATC-EXTRACT-FG       PIC X.
           03  FILLER               PIC X.
           03  ATC-DEBIT-REFCD      PIC X(5).
           03  FILLER               PIC X.
           03  ATC-CR              PIC X.
           03  ATC-LF              PIC X.
