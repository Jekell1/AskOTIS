      * COPYMEMBER: LIBGB/GB01RC_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - GLOBAL RECAP DAILY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  RC1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  RC1-CURSOR-STAT-GOOD               VALUE "00".
           88  RC1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  RC1-CURSOR-NG-STAT           PIC X(2)  VALUE SPACES.
           88  RC1-CURSOR-NG-STAT-GOOD            VALUE "00".
           88  RC1-CURSOR-NG-STAT-BAD             VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  RC2-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  RC2-CURSOR-STAT-GOOD               VALUE "00".
           88  RC2-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  RC1-RETURN-CODE              PIC S9(4)   COMP-5.
       01  RC2-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QRC1-WHERE-BEG-KEY.
           03  QRC1-WBEG-BRNO           PIC 9(04).
           03  QRC1-WBEG-TRANS-DATE     PIC X(10).

       01  QRC1-WHERE-END-KEY.
           03  QRC1-WEND-BRNO           PIC 9(04).
           03  QRC1-WEND-TRANS-DATE     PIC X(10).

       01  QRC2-WHERE-BEG-KEY.
           03  QRC2-WBEG-STATUS         PIC X(02).
           03  QRC2-WBEG-BRNO           PIC 9(04).
           03  QRC2-WBEG-TRANS-DATE     PIC X(10).

       01  QRC2-WHERE-END-KEY.
           03  QRC2-WEND-STATUS         PIC X(02).
           03  QRC2-WEND-BRNO           PIC 9(04).
           03  QRC2-WEND-TRANS-DATE     PIC X(10).

       01  QRC-REC.
           03  QRC-STATUS              PIC XX.
           03  QRC-BRNO                PIC 9(4).
           03  QRC-TRANS-DATE          PIC X(10).
           03  QRC-LTOUCH-DATE         PIC X(10).
           03  QRC-RECAP-FG             PIC X.
           03  QRC-GL-FG                PIC X.
           03  QRC-EOD-BATCH-FG         PIC X.
           03  QRC-NIGHTLY-FUNDS-FG     PIC X.
           03  QRC-EOM1-BATCH-FG        PIC X.
           03  QRC-EOM-UPDATE-FG        PIC X.
           03  QRC-EOM2-BATCH-FG        PIC X.
           03  QRC-BS-P2P3-INBAL-FG     PIC X.
           03  QRC-DEPOSIT              PIC S9(7)V99.
           03  QRC-OVER-SHORT           PIC S9(7)V99.
           03  QRC-CHECKS               PIC S9(7)V99.
           03  QRC-OPENED-DATE          PIC X(10).
           03  QRC-OPENED-TIME          PIC 9(6).
           03  QRC-OP-USERID            PIC X(10).
           03  QRC-CLOSED-DATE          PIC X(10).
           03  QRC-CLOSED-TIME          PIC 9(6).
           03  QRC-CL-USERID            PIC X(10).
           03  QRC-UPDATED-DATE         PIC X(10).
           03  QRC-UPDATED-TIME         PIC 9(6).
           03  QRC-UPDATED-USERID       PIC X(10).
           03  QRC-EOD-BATCH-DATE       PIC X(10).
           03  QRC-EOD-BATCH-TIME       PIC 9(6).
           03  QRC-FUNDS-DATE           PIC X(10).
           03  QRC-FUNDS-TIME           PIC 9(6).
           03  QRC-EARNINGS-DATE        PIC X(10).
           03  QRC-EARNINGS-TIME        PIC 9(6).
           03  QRC-EOM-UPDATED-DATE     PIC X(10).
           03  QRC-EOM-UPDATED-TIME     PIC 9(6).
           03  QRC-EOM2-BATCH-DATE      PIC X(10).
           03  QRC-EOM2-BATCH-TIME      PIC 9(6).
           03  QRC-CHECK-DATE           PIC X(10).
           03  QRC-CHECK-TIME           PIC 9(6).
           03  QRC-LAST-ACCRUE          PIC 9(4).
           03  QRC-ERROR-MSG            PIC X(15).
           03  QRC-BANK-BAL             PIC S9(9)V99.
           03  QRC-CHECK-DEPOSIT        PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
