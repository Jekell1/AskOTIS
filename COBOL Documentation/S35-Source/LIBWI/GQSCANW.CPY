      * COPYMEMBER: LIBWI/GQSCANW
      ******************************************************************
      *
      * IDENTIFICATION: COLLECTOR (GLOBAL) QUEUE WINDOW SCAN BUFFER
      *
      *=================================================================
      * REV:
      * KEC 2017.0109 REMOVED THE FOLLOWING SINCE NOT USED IN A30:
      *             GQSCAN-KEY
      *             GQSCAN-DATE
      *             GQSCAN-START-TIME
      *             GQSCAN-HH
      *             GQSCAN-MM
      *             GQSCAN-SEQ-FIELDS
      *             GQSCAN-REV-DEL
      *             GQSCAN-REV-BAL
      *             GQSCAN-NUMBER
      *             GQSCAN-SEQ
      * SMR 2024-0219 ADDED GQSCAN-VGQ-PATH TO RESOLVE DIFFICULTY WITH
      *               THE GQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *               START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *               GQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      * BAH 2024.0502 MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  GQSCAN-BUF.
           03  GQSCAN-SYSDATE         PIC X(08).
           03  GQSCAN-SP-EXPD-FRMLA   PIC X(01).
           03  GQSCAN-BR-LOAN-NUM-FG  PIC X(01).
           03  GQSCAN-BRANCH          PIC 9(04).
           03  GQSCAN-COLLID          PIC X(03).
           03  GQSCAN-BR-LOW-TO-HI    PIC X(01).
           03  GQSCAN-ACCTNO          PIC 9(08).
           03  GQSCAN-VGQ-PATH        PIC X(35).
           03  GQSCAN-STATUS          PIC XX.
