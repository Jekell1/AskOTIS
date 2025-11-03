      * COPYMEMBER: LIBEX/EX01XGQ
      ******************************************************************
      *    GLOBAL COLLECTOR QUEUE EXTRACTION FILE
      *    REGACC PR# 254
      ******************************************************************
       78  XGQ-SIZE                                  VALUE 53.
       01  XGQ-REC.
           03  XGQ-BRACCTNO          PIC 9(10).
           03  XGQ-BRNO              PIC 9(4).
           03  XGQ-NUMBER            PIC 9(6).
           03  XGQ-SEQNO             PIC 9(2).
           03  XGQ-COLLID            PIC XXX.
           03  XGQ-UNUSED            PIC X.
           03  XGQ-SEQ-FG            PIC X.
           03  XGQ-DATE              PIC 9(8).
           03  XGQ-START-TIME        PIC +9(4).
           03  XGQ-REV-DEL           PIC 9(5).
           03  XGQ-REV-PDUE-DAYS     PIC 9(5).
           03  XGQ-REV-BAL           PIC 9(6).
           03  XGQ-WORK-COLLID       PIC XXX.
           03  XGQ-WORKED-FG         PIC X.
           03  XGQ-IN-PROCESS        PIC X.
           03  XGQ-DATE-QUEUED       PIC 9(8).
           03  XGQ-TIME-ZONE-ERROR   PIC X.
           03  XGQ-FILLER            PIC X(100).
           03  XGQ-CR                PIC X.
           03  XGQ-LF                PIC X.
