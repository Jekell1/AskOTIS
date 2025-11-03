      * COPYMEMBER: LIBGB/GB01EO_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BATCH REPORT PROCESSING FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      * JKC 2023-0929 ADDED EO1-ROW-COUNT.
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  EO1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  EO1-CURSOR-STAT-GOOD               VALUE "00".
           88  EO1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  EO1-CURSOR-GR-STAT           PIC X(2)  VALUE SPACES.
           88  EO1-CURSOR-GR-STAT-GOOD            VALUE "00".
           88  EO1-CURSOR-GR-STAT-BAD             VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  EO1-RETURN-CODE             PIC S9(4)     COMP-5.
       01  EO1-ROW-COUNT               PIC 9(4).

       01  QEO1-WHERE-BEG-KEY.
           03  QEO1-WBEG-PCODE         PIC X(3).
           03  QEO1-WBEG-STEP          PIC 9(4).
 
       01  QEO1-WHERE-END-KEY.
           03  QEO1-WEND-PCODE         PIC X(3).
           03  QEO1-WEND-STEP          PIC 9(4).
 
       01  QEO-REC.
           03  QEO-PCODE               PIC X(3).
           03  QEO-STEP                PIC 9999.
           03  QEO-LTOUCH-DATE         PIC X(10).
           03  QEO-DIR                 PIC XX.
           03  QEO-PROGRAM             PIC X(6).
           03  QEO-COPIES              PIC 9.
           03  QEO-COMMENT             PIC X(30).
           03  QEO-MESS                PIC X(60).
           03  QEO-START-DATE          PIC X(10).
           03  QEO-START-TIME          PIC 9(6).
           03  QEO-COMPLETE-DATE       PIC X(10).
           03  QEO-COMPLETE-TIME       PIC 9(6).
           03  QEO-COMPLETE-STAT       PIC X(6).
           03  QEO-PARAMETERS          PIC X(600).
           03  QEO-ERROR-STEP-X        PIC XXXX.
           03  QEO-TRANSFER            PIC X.
           03  QEO-LEVEL               PIC 9.

       EXEC SQL END DECLARE SECTION END-EXEC.
