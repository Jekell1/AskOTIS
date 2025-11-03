      * COPYMEMBER: LIBSP/SP01GQ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - GLOBAL COLLECTOR QUEUE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  GQ1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GQ1-CURSOR-STAT-GOOD              VALUE "00".
           88  GQ1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GQ1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  GQ1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  GQ1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  GQ2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GQ2-CURSOR-STAT-GOOD              VALUE "00".
           88  GQ2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GQ2-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  GQ2-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  GQ2-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GQ1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  GQ2-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QGQ1-WHERE-BEG-KEY.
           03  QGQ1-WBEG-BRNO        PIC 9(04).
           03  QGQ1-WBEG-ACCTNO      PIC 9(08).
 
       01  QGQ1-WHERE-END-KEY.
           03  QGQ1-WEND-BRNO        PIC 9(04).
           03  QGQ1-WEND-ACCTNO      PIC 9(08).
 
       01  QGQ2-WHERE-BEG-KEY.
           03  QGQ2-WBEG-COLLID         PIC X(03).
           03  QGQ2-WBEG-SEQ-FG         PIC X(01).
           03  QGQ2-WBEG-DATE           PIC X(10).
           03  QGQ2-WBEG-START-TIME     PIC S9(4).
           03  QGQ2-WBEG-REV-DEL        PIC 9(5).
           03  QGQ2-WBEG-MISC-KEY-FLD   PIC X(3).
           03  QGQ2-WBEG-REV-PDUE-DAYS  PIC 9(5).
           03  QGQ2-WBEG-REV-BAL        PIC 9(7).
           03  QGQ2-WBEG-SEQ            PIC 9(3).

       01  QGQ2-WHERE-END-KEY.
           03  QGQ2-WEND-COLLID         PIC X(03).
           03  QGQ2-WEND-SEQ-FG         PIC X(01).
           03  QGQ2-WEND-DATE           PIC X(10).
           03  QGQ2-WEND-START-TIME     PIC S9(4).
           03  QGQ2-WEND-REV-DEL        PIC 9(5).
           03  QGQ2-WEND-MISC-KEY-FLD   PIC X(3).
           03  QGQ2-WEND-REV-PDUE-DAYS  PIC 9(5).
           03  QGQ2-WEND-REV-BAL        PIC 9(7).
           03  QGQ2-WEND-SEQ            PIC 9(3).

       01  QGQ-REC.
           03  QGQ-BRNO              PIC 9(4).
           03  QGQ-ACCTNO            PIC 9(8).
           03  QGQ-COLLID            PIC XXX.
           03  QGQ-SEQ-FG            PIC X.
           03  QGQ-DATE              PIC X(10).
           03  QGQ-START-TIME        PIC S9(4).
           03  QGQ-REV-DEL           PIC 9(5).
           03  QGQ-MISC-KEY-FLD      PIC X(3).
           03  QGQ-REV-PDUE-DAYS     PIC 9(5).
           03  QGQ-REV-BAL           PIC 9(7).
           03  QGQ-SEQ               PIC 9(3).
           03  QGQ-LTOUCH-DATE       PIC X(10).
           03  QGQ-WORK-COLLID       PIC XXX.
           03  QGQ-WORKED-FG         PIC X.
           03  QGQ-IN-PROCESS        PIC X.
           03  QGQ-DATE-QUEUED       PIC X(10).
           03  QGQ-TIME-ZONE-ERROR   PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
