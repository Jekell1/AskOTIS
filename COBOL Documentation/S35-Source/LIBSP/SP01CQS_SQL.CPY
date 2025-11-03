      * COPYMEMBER: LIBSP/SP01CQS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY COLLECTOR QUEUE SUMMARY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CQS1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  CQS1-CURSOR-STAT-GOOD             VALUE "00".
           88  CQS1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CQS1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QCQS1-WHERE-BEG-KEY.
           03  QCQS1-WBEG-BRNO             PIC 9(04).
           03  QCQS1-WBEG-COLLID           PIC X(03).
 
       01  QCQS1-WHERE-END-KEY.
           03  QCQS1-WEND-BRNO             PIC 9(04).
           03  QCQS1-WEND-COLLID           PIC X(03).
 
       01  QCQS-REC.
           03  QCQS-BRNO                     PIC 9(4).
           03  QCQS-COLLID                   PIC X(3).
           03  QCQS-LTOUCH-DATE              PIC X(10).
           03  QCQS-TOT-REC                  PIC 9(5).
           03  QCQS-WORKED-REC               PIC 9(5).
           03  QCQS-TIMED-REC                PIC 9(5).
           03  QCQS-CQ-BRNO                  PIC 9(4).
           03  QCQS-CQ-COLLID                PIC XXX.
           03  QCQS-CQ-REV-BEHAVIOR-RISK     PIC 9.
           03  QCQS-CQ-SEQ-FG                PIC X.
           03  QCQS-CQ-DATE                  PIC X(10).
           03  QCQS-CQ-START-TIME            PIC S9(4).
           03  QCQS-CQ-REV-DEL               PIC 9(5).
           03  QCQS-CQ-MISC-KEY-FLD          PIC X(3).
           03  QCQS-CQ-REV-PDUE-DAYS         PIC 9(5).
           03  QCQS-CQ-REV-BAL               PIC 9(7).
           03  QCQS-CQ-SEQ                   PIC 9(3).

       EXEC SQL END DECLARE SECTION END-EXEC.
