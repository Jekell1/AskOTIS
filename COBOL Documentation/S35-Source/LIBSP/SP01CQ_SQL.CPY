      * COPYMEMBER: LIBSP/SP01CQ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY COLLECTOR QUEUE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CQ1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CQ1-CURSOR-STAT-GOOD              VALUE "00".
           88  CQ1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  CQ2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CQ2-CURSOR-STAT-GOOD              VALUE "00".
           88  CQ2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  CQ2-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  CQ2-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  CQ2-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CQ1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  CQ2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QCQ1-WHERE-BEG-KEY.
           03  QCQ1-WBEG-BRNO             PIC 9(04).
           03  QCQ1-WBEG-ACCTNO           PIC 9(08).
 
       01  QCQ1-WHERE-END-KEY.
           03  QCQ1-WEND-BRNO             PIC 9(04).
           03  QCQ1-WEND-ACCTNO           PIC 9(08).

       01  QCQ2-WHERE-BEG-KEY.
           03  QCQ2-WBEG-BRNO              PIC 9(04).
           03  QCQ2-WBEG-COLLID            PIC X(03).
           03  QCQ2-WBEG-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ2-WBEG-SEQ-FG            PIC X.
           03  QCQ2-WBEG-DATE              PIC X(10).
           03  QCQ2-WBEG-START-TIME        PIC S9(04).
           03  QCQ2-WBEG-REV-DEL           PIC 9(05).
           03  QCQ2-WBEG-MISC-KEY-FLD      PIC X(03).
           03  QCQ2-WBEG-REV-PDUE-DAYS     PIC 9(05).
           03  QCQ2-WBEG-REV-BAL           PIC 9(07).
           03  QCQ2-WBEG-SEQ               PIC 9(03).
 
       01  QCQ2-WHERE-END-KEY.
           03  QCQ2-WEND-BRNO              PIC 9(04).
           03  QCQ2-WEND-COLLID            PIC X(03).
           03  QCQ2-WEND-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ2-WEND-SEQ-FG            PIC X.
           03  QCQ2-WEND-DATE              PIC X(10).
           03  QCQ2-WEND-START-TIME        PIC S9(04).
           03  QCQ2-WEND-REV-DEL           PIC 9(05).
           03  QCQ2-WEND-MISC-KEY-FLD      PIC X(03).
           03  QCQ2-WEND-REV-PDUE-DAYS     PIC 9(05).
           03  QCQ2-WEND-REV-BAL           PIC 9(07).
           03  QCQ2-WEND-SEQ               PIC 9(03).

       01  QCQ2-WHERE-SCAN-BEG-KEY.
           03  QCQ2-WSBEG-COLLID            PIC X(03).
           03  QCQ2-WSBEG-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ2-WSBEG-SEQ-FG            PIC X.
           03  QCQ2-WSBEG-DATE              PIC X(10).
           03  QCQ2-WSBEG-START-TIME        PIC S9(04).
           03  QCQ2-WSBEG-REV-DEL           PIC 9(05).
           03  QCQ2-WSBEG-MISC-KEY-FLD      PIC X(03).
           03  QCQ2-WSBEG-REV-PDUE-DAYS     PIC 9(05).
           03  QCQ2-WSBEG-REV-BAL           PIC 9(07).
           03  QCQ2-WSBEG-SEQ               PIC 9(03).
 
       01  QCQ-REC.
           03  QCQ-BRNO              PIC 9(4).
           03  QCQ-ACCTNO            PIC 9(8).
           03  QCQ-COLLID            PIC XXX.
           03  QCQ-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ-SEQ-FG            PIC X.
           03  QCQ-DATE              PIC X(10).
           03  QCQ-START-TIME        PIC S9(4).
           03  QCQ-REV-DEL           PIC 9(5).
           03  QCQ-MISC-KEY-FLD      PIC X(3).
           03  QCQ-REV-PDUE-DAYS     PIC 9(5).
           03  QCQ-REV-BAL           PIC 9(7).
           03  QCQ-SEQ               PIC 9(3).
           03  QCQ-LTOUCH-DATE       PIC X(10).
           03  QCQ-WORK-COLLID       PIC XXX.
           03  QCQ-WORKED-FG         PIC X.
           03  QCQ-IN-PROCESS        PIC X.
           03  QCQ-DATE-QUEUED       PIC X(10).
           03  QCQ-TIME-ZONE-ERROR   PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
