      * COPYMEMBER: LIBSP/SP01GQS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - GLOBAL COLLECTOR QUEUE SUMMARY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  GQS1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  GQS1-CURSOR-STAT-GOOD             VALUE "00".
           88  GQS1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GQS1-CURSOR-GR-STAT          PIC X(2) VALUE SPACES.
           88  GQS1-CURSOR-GR-STAT-GOOD          VALUE "00".
           88  GQS1-CURSOR-GR-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  GQS2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  GQS2-CURSOR-STAT-GOOD             VALUE "00".
           88  GQS2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GQS2-CURSOR-GR-STAT          PIC X(2) VALUE SPACES.
           88  GQS2-CURSOR-GR-STAT-GOOD          VALUE "00".
           88  GQS2-CURSOR-GR-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GQS1-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QGQS1-WHERE-BEG-KEY.
           03  QGQS1-WBEG-COLLID      PIC X(03).
 
       01  QGQS1-WHERE-END-KEY.
           03  QGQS1-WEND-COLLID      PIC X(03).
 
       01  QGQS-REC.
           03  QGQS-COLLID                   PIC X(3).
           03  QGQS-LTOUCH-DATE              PIC X(10).
           03  QGQS-TOT-REC                  PIC 9(5).
           03  QGQS-WORKED-REC               PIC 9(5).
           03  QGQS-TIMED-REC                PIC 9(5).
           03  QGQS-GROUP-ID                 PIC X(6).
           03  QGQS-GQ-COLLID                PIC XXX.
           03  QGQS-GQ-SEQ-FG                PIC X.
           03  QGQS-GQ-DATE                  PIC X(10).
           03  QGQS-GQ-START-TIME            PIC S9(4).
           03  QGQS-GQ-REV-DEL               PIC 9(5).
           03  QGQS-GQ-MISC-KEY-FLD          PIC X(3).
           03  QGQS-GQ-REV-PDUE-DAYS         PIC 9(5).
           03  QGQS-GQ-REV-BAL               PIC 9(7).
           03  QGQS-GQ-SEQ                   PIC 9(3).

       EXEC SQL END DECLARE SECTION END-EXEC.
