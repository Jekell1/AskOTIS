      * COPYMEMBER: LIBSP/SP01CQS
      ******************************************************************
      *               (CQ)
      *    DAILY COLLECTOR QUEUE SUMMARY FILE
      *
      *
      * CQS1-KEY: CQS-BRNO, CQS-COLLID
      *
      *=================================================================
      *
      *  NOTE!!! THE FIELDS IN CQ2-KEY MUST MATCH THE FIELDS IN
      *          CQS-CQ2-KEY-FIELDS AREA BOTH IN SIZE & POSITION.
      *
      *=================================================================
      * REV:
      *  CS 190213 HEADER RECORD NEEDS TO BE A SEPARATE FILE
      *            SP01CQS(SUMMARY)
      *  CS 190603 REMOVE ENDING FILLER
      *  CS 20210506 REMOVE CQ-FUTURE-KEY1, AND CQ-FUTURE-KEY2
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  CQS-SIZE                         VALUE  125.
       01  CQS-REC.
           03  CQS1-KEY.
               05  CQS-BRNO                 PIC 9(4).
               05  CQS-COLLID               PIC X(3). 

      $XFD DATE=YYYYMMDD
           03  CQS-LTOUCH-DATE              PIC 9(08)   COMP-3.
           03  CQS-TOT-REC                  PIC 9(5)    COMP-3.
           03  CQS-WORKED-REC               PIC 9(5)    COMP-3.
           03  CQS-TIMED-REC                PIC 9(5)    COMP-3.

           03  CQS-CQ2-KEY-FIELDS.
               05  CQS-CQ-BRNO              PIC 9(4).
               05  CQS-CQ-COLLID            PIC XXX.
               05  CQS-CQ-REV-BEHAVIOR-RISK PIC 9.
               05  CQS-CQ-SEQ-FG            PIC X.
      $XFD DATE=YYYYMMDD
               05  CQS-CQ-DATE              PIC 9(8).
               05  CQS-CQ-START-TIME        PIC S9(4)   COMP-3.
               05  CQS-CQ-REV-DEL           PIC 9(5).
               05  CQS-CQ-MISC-KEY-FLD      PIC X(3).
               05  CQS-CQ-REV-PDUE-DAYS     PIC 9(5).
               05  CQS-CQ-REV-BAL           PIC 9(7).
               05  CQS-CQ-SEQ               PIC 9(3).

           03  FILLER                       PIC X(61).
