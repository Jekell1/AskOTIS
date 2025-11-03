      * COPYMEMBER: LIBEX/EX01XLSR
      ******************************************************************
      *    LOAN STATUS RESCHEDULE/SCRA TRAILER
      *
      * REV:
      *  110329 BAH NEW REGACC PR# 289
      ******************************************************************
       78  XLSR-SIZE                     VALUE 192.
       01  XLSR-REC.
           03  XLS-R-BRACCTNO             PIC 9(10).
           03  XLS-R-ACCTNO               PIC 9(8).
           03  XLS-R-CODE                 PIC X.
           03  XLS-R-SEQNO                PIC 9.

           03  XLS-R-SCRA-ACTIVE           PIC X.
           03  XLS-R-SCRA-START-DATE       PIC 9(8).
           03  XLS-R-SCRA-END-DATE         PIC 9(8).
           03  XLS-R-SCRA-BORROWER         PIC X.
           03  XLS-R-CURRENT-RESCHED-DATE  PIC 9(8).
           03  XLS-R-CURRENT-REASON-CODE   PIC X(5).
           03  XLS-R-INITIAL-RESCHED-DATE  PIC 9(8).
           03  XLS-R-INITIAL-REASON-CODE   PIC X(5).
           03  XLS-R-INITIAL-TDR-DATE      PIC 9(8).
           03  XLS-R-INITIAL-TDR-REASON    PIC X(5).
           03  XLS-R-CURRENT-TDR-DATE      PIC 9(8).
           03  XLS-R-CURRENT-TDR-REASON    PIC X(5).
           03  FILLER                      PIC X(100).
           03  XLS-R-CR                    PIC X.
           03  XLS-R-LF                    PIC X.
