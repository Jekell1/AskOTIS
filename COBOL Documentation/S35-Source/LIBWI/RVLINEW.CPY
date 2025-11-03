      * COPYMEMBER: LIBWI/RVLINEW

      ****************************************************************
      * CTRL B  CALLED FROM WITHIN SCREEN
      *                   (RVLINEW)    RVLINE-BUF = 25
      * BLM 160616 REMOVE RVLINE-CYCLE-DAY, NOT USED
      * BAH 160824 CHANGED RVLINE-SPRCLASS FROM 9(2) TO 9(3)
      * BAH 160928 ADDED RVLINE-BRNO
      * KEC 2016.1116 ADDED RVLINE-SEQNO WHICH IS FOR LTR-SEQNO.
      *               ADDED RVLINE-LTR1-KEY (GROUP) AND MOVED RVLINE-BRNO,
      *               RVLINE-ACCTNO AND RVLINE-SEQNO UNDER IT.
      * KEC 2017.0306 ADDED 'SP' FILE PREFIX FOR WORKERS.
      * BAH 2019.0625 REMOVED RVLINE-REVOLVING, PT0153
      ****************************************************************
       01  RVLINE-BUF.
           03  RVLINE-BRNO         PIC 9(4).
           03  RVLINE-ACCTNO       PIC 9(8).
           03  RVLINE-UNITPER-CD   PIC X.
           03  RVLINE-ARCHIVE-FG   PIC X.
