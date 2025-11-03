      * COPYMEMBER: LIBWI/CQSCANW
      ******************************************************************
      *
      * IDENTIFICATION: COLLECTOR (LOCAL) QUEUE WINDOW SCAN BUFFER
      *
      *=================================================================
      * REV:
      * KEC 2017.0109 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED THE FOLLOWING SINCE NOT USED IN A30:
      *             CQSCAN-ID
      *             CQSCAN-KEY
      *             CQSCAN-DATE
      *             CQSCAN-START-TIME
      *             CQSCAN-HH
      *             CQSCAN-MM
      *             CQSCAN-SEQ-FIELDS
      *             CQSCAN-REV-DEL
      *             CQSCAN-REV-BAL
      *             CQSCAN-NUMBER
      *             CQSCAN-SEQ
      *
      * SMR 2023-0925 ADDED VCQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE CQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          CQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      *          ADDED SPECIFICALLY CQSCAN-VCQ-PATH.
      * BAH 2024.0416 REMOVED CQSCAN-SYSDATE
      *  2024.0506 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  CQSCAN-BUF.
           03  CQSCAN-SP-EXPD-FRMLA   PIC X(01).
           03  CQSCAN-BR-LOAN-NUM-FG  PIC X(01).
           03  CQSCAN-BRANCH          PIC 9(04).
           03  CQSCAN-COLLID          PIC X(03).
           03  CQSCAN-BR-LOW-TO-HI    PIC X(01).
           03  CQSCAN-ACCTNO          PIC 9(08).
           03  CQSCAN-VCQ-PATH        PIC X(35).
           03  CQSCAN-STATUS          PIC XX.
