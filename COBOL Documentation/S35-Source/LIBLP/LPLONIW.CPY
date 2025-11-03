      * COPYMEMBER: LIBLP/LPLONIW
      * 131014 ADDED SAVEKEY HERE TO PASS, WAS XFER/FLDVAL IN A15
      * BAH 170405 CHANGE TRANS-DATE TO BE CCYYMMDD, PD0006
      ************************************
      *     C O M M O N   M E M O R Y    *
      ************************************
       01  PROG-BUF.
           03  ERRCD                      PIC X(1).
           03  NEW-PROG-FORM.
               05  NEW-PROG.
                   07  FILLER             PIC X.
                   07  NEW-PROG-SUB       PIC X.
               05  NEW-FORM               PIC X(1).
           03  OLD-PROG-FORM.
               05  OLD-PROG               PIC X(2).
               05  OLD-FORM               PIC X(1).
           03  ROUTE-CD                   PIC X(1).
           03  TRANS-DATE                 PIC 9(8).
           03  TRANS-D                    REDEFINES TRANS-DATE.
               05  TRANS-CC               PIC 99.
               05  TRANS-YEAR             PIC 99.
               05  TRANS-MONTH            PIC 99.
               05  TRANS-DAY              PIC 99.
           03  BRANCH                     PIC 9(4).
           03  LOAN-KEY                   PIC 9(8).
           03  BORROWER-KEY               PIC 9(9).
           03  LPLONI-CONAME              PIC X(20).
           03  LPLONI-SYSDATE             PIC X(8).
           03  LPLONI-USERID.
               05  FILLER                 PIC X.
               05  LPLONI-USER-BRANCH     PIC 9(4).
               05  LPLONI-USER-NAME       PIC X(5).
           03  USER-PID                   PIC X(7).
      * YYYYMMDD
           03  MONTH-DATE                 PIC 9(8).

           03  BULK-CONVERSION-FG     PIC X.
           03  SAVEKEY                PIC X(9).
           03  BRAVAIL-BUF            PIC X.

           03  FILLER                 PIC X(50).

