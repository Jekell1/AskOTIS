      * COPYMEMBER: LIBLP/LP01SS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CREDIT SCORE STATISTICS FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  SS1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  SS1-CURSOR-STAT-GOOD              VALUE "00".
           88  SS1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  SS1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  SS1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  SS1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SS1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QSS1-WHERE-BEG-KEY.
           03  QSS1-WBEG-DATE           PIC 9(06).
           03  QSS1-WBEG-BRNO           PIC 9(04).
           03  QSS1-WBEG-CLASS          PIC 9(03).

       01  QSS1-WHERE-END-KEY.
           03  QSS1-WEND-DATE           PIC 9(06).
           03  QSS1-WEND-BRNO           PIC 9(04).
           03  QSS1-WEND-CLASS          PIC 9(03).

       01  QSS-REC.
           03  QSS-DATE-YYYYMM         PIC 9(6).
           03  QSS-BRNO                PIC 9(4).
           03  QSS-CLASS               PIC 9(3).
           03  QSS-LTOUCH-DATE         PIC X(10).
           03  QSS-UNITS-1-1-1    PIC S9(7).
           03  QSS-OUTBAL-1-1-1   PIC S9(9)V99.
           03  QSS-UNITS-1-1-2    PIC S9(7).
           03  QSS-OUTBAL-1-1-2   PIC S9(9)V99.
           03  QSS-UNITS-1-1-3    PIC S9(7).
           03  QSS-OUTBAL-1-1-3   PIC S9(9)V99.
           03  QSS-UNITS-1-1-4    PIC S9(7).
           03  QSS-OUTBAL-1-1-4   PIC S9(9)V99.
           03  QSS-UNITS-1-1-5    PIC S9(7).
           03  QSS-OUTBAL-1-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-1-1-1    PIC S9(7).
           03  QSS-AMT-PL-1-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-1-1-2    PIC S9(7).
           03  QSS-AMT-PL-1-1-2   PIC S9(9)V99.
           03  QSS-UNITS-1-2-1    PIC S9(7).
           03  QSS-OUTBAL-1-2-1   PIC S9(9)V99.
           03  QSS-UNITS-1-2-2    PIC S9(7).
           03  QSS-OUTBAL-1-2-2   PIC S9(9)V99.
           03  QSS-UNITS-1-2-3    PIC S9(7).
           03  QSS-OUTBAL-1-2-3   PIC S9(9)V99.
           03  QSS-UNITS-1-2-4    PIC S9(7).
           03  QSS-OUTBAL-1-2-4   PIC S9(9)V99.
           03  QSS-UNITS-1-2-5    PIC S9(7).
           03  QSS-OUTBAL-1-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-1-2-1    PIC S9(7).
           03  QSS-AMT-PL-1-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-1-2-2    PIC S9(7).
           03  QSS-AMT-PL-1-2-2   PIC S9(9)V99.
           03  QSS-UNITS-2-1-1    PIC S9(7).
           03  QSS-OUTBAL-2-1-1   PIC S9(9)V99.
           03  QSS-UNITS-2-1-2    PIC S9(7).
           03  QSS-OUTBAL-2-1-2   PIC S9(9)V99.
           03  QSS-UNITS-2-1-3    PIC S9(7).
           03  QSS-OUTBAL-2-1-3   PIC S9(9)V99.
           03  QSS-UNITS-2-1-4    PIC S9(7).
           03  QSS-OUTBAL-2-1-4   PIC S9(9)V99.
           03  QSS-UNITS-2-1-5    PIC S9(7).
           03  QSS-OUTBAL-2-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-2-1-1    PIC S9(7).
           03  QSS-AMT-PL-2-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-2-1-2    PIC S9(7).
           03  QSS-AMT-PL-2-1-2   PIC S9(9)V99.
           03  QSS-UNITS-2-2-1    PIC S9(7).
           03  QSS-OUTBAL-2-2-1   PIC S9(9)V99.
           03  QSS-UNITS-2-2-2    PIC S9(7).
           03  QSS-OUTBAL-2-2-2   PIC S9(9)V99.
           03  QSS-UNITS-2-2-3    PIC S9(7).
           03  QSS-OUTBAL-2-2-3   PIC S9(9)V99.
           03  QSS-UNITS-2-2-4    PIC S9(7).
           03  QSS-OUTBAL-2-2-4   PIC S9(9)V99.
           03  QSS-UNITS-2-2-5    PIC S9(7).
           03  QSS-OUTBAL-2-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-2-2-1    PIC S9(7).
           03  QSS-AMT-PL-2-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-2-2-2    PIC S9(7).
           03  QSS-AMT-PL-2-2-2   PIC S9(9)V99.
           03  QSS-UNITS-3-1-1    PIC S9(7).
           03  QSS-OUTBAL-3-1-1   PIC S9(9)V99.
           03  QSS-UNITS-3-1-2    PIC S9(7).
           03  QSS-OUTBAL-3-1-2   PIC S9(9)V99.
           03  QSS-UNITS-3-1-3    PIC S9(7).
           03  QSS-OUTBAL-3-1-3   PIC S9(9)V99.
           03  QSS-UNITS-3-1-4    PIC S9(7).
           03  QSS-OUTBAL-3-1-4   PIC S9(9)V99.
           03  QSS-UNITS-3-1-5    PIC S9(7).
           03  QSS-OUTBAL-3-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-3-1-1    PIC S9(7).
           03  QSS-AMT-PL-3-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-3-1-2    PIC S9(7).
           03  QSS-AMT-PL-3-1-2   PIC S9(9)V99.
           03  QSS-UNITS-3-2-1    PIC S9(7).
           03  QSS-OUTBAL-3-2-1   PIC S9(9)V99.
           03  QSS-UNITS-3-2-2    PIC S9(7).
           03  QSS-OUTBAL-3-2-2   PIC S9(9)V99.
           03  QSS-UNITS-3-2-3    PIC S9(7).
           03  QSS-OUTBAL-3-2-3   PIC S9(9)V99.
           03  QSS-UNITS-3-2-4    PIC S9(7).
           03  QSS-OUTBAL-3-2-4   PIC S9(9)V99.
           03  QSS-UNITS-3-2-5    PIC S9(7).
           03  QSS-OUTBAL-3-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-3-2-1    PIC S9(7).
           03  QSS-AMT-PL-3-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-3-2-2    PIC S9(7).
           03  QSS-AMT-PL-3-2-2   PIC S9(9)V99.
           03  QSS-UNITS-4-1-1    PIC S9(7).
           03  QSS-OUTBAL-4-1-1   PIC S9(9)V99.
           03  QSS-UNITS-4-1-2    PIC S9(7).
           03  QSS-OUTBAL-4-1-2   PIC S9(9)V99.
           03  QSS-UNITS-4-1-3    PIC S9(7).
           03  QSS-OUTBAL-4-1-3   PIC S9(9)V99.
           03  QSS-UNITS-4-1-4    PIC S9(7).
           03  QSS-OUTBAL-4-1-4   PIC S9(9)V99.
           03  QSS-UNITS-4-1-5    PIC S9(7).
           03  QSS-OUTBAL-4-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-4-1-1    PIC S9(7).
           03  QSS-AMT-PL-4-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-4-1-2    PIC S9(7).
           03  QSS-AMT-PL-4-1-2   PIC S9(9)V99.
           03  QSS-UNITS-4-2-1    PIC S9(7).
           03  QSS-OUTBAL-4-2-1   PIC S9(9)V99.
           03  QSS-UNITS-4-2-2    PIC S9(7).
           03  QSS-OUTBAL-4-2-2   PIC S9(9)V99.
           03  QSS-UNITS-4-2-3    PIC S9(7).
           03  QSS-OUTBAL-4-2-3   PIC S9(9)V99.
           03  QSS-UNITS-4-2-4    PIC S9(7).
           03  QSS-OUTBAL-4-2-4   PIC S9(9)V99.
           03  QSS-UNITS-4-2-5    PIC S9(7).
           03  QSS-OUTBAL-4-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-4-2-1    PIC S9(7).
           03  QSS-AMT-PL-4-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-4-2-2    PIC S9(7).
           03  QSS-AMT-PL-4-2-2   PIC S9(9)V99.
           03  QSS-UNITS-5-1-1    PIC S9(7).
           03  QSS-OUTBAL-5-1-1   PIC S9(9)V99.
           03  QSS-UNITS-5-1-2    PIC S9(7).
           03  QSS-OUTBAL-5-1-2   PIC S9(9)V99.
           03  QSS-UNITS-5-1-3    PIC S9(7).
           03  QSS-OUTBAL-5-1-3   PIC S9(9)V99.
           03  QSS-UNITS-5-1-4    PIC S9(7).
           03  QSS-OUTBAL-5-1-4   PIC S9(9)V99.
           03  QSS-UNITS-5-1-5    PIC S9(7).
           03  QSS-OUTBAL-5-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-5-1-1    PIC S9(7).
           03  QSS-AMT-PL-5-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-5-1-2    PIC S9(7).
           03  QSS-AMT-PL-5-1-2   PIC S9(9)V99.
           03  QSS-UNITS-5-2-1    PIC S9(7).
           03  QSS-OUTBAL-5-2-1   PIC S9(9)V99.
           03  QSS-UNITS-5-2-2    PIC S9(7).
           03  QSS-OUTBAL-5-2-2   PIC S9(9)V99.
           03  QSS-UNITS-5-2-3    PIC S9(7).
           03  QSS-OUTBAL-5-2-3   PIC S9(9)V99.
           03  QSS-UNITS-5-2-4    PIC S9(7).
           03  QSS-OUTBAL-5-2-4   PIC S9(9)V99.
           03  QSS-UNITS-5-2-5    PIC S9(7).
           03  QSS-OUTBAL-5-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-5-2-1    PIC S9(7).
           03  QSS-AMT-PL-5-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-5-2-2    PIC S9(7).
           03  QSS-AMT-PL-5-2-2   PIC S9(9)V99.
           03  QSS-UNITS-6-1-1    PIC S9(7).
           03  QSS-OUTBAL-6-1-1   PIC S9(9)V99.
           03  QSS-UNITS-6-1-2    PIC S9(7).
           03  QSS-OUTBAL-6-1-2   PIC S9(9)V99.
           03  QSS-UNITS-6-1-3    PIC S9(7).
           03  QSS-OUTBAL-6-1-3   PIC S9(9)V99.
           03  QSS-UNITS-6-1-4    PIC S9(7).
           03  QSS-OUTBAL-6-1-4   PIC S9(9)V99.
           03  QSS-UNITS-6-1-5    PIC S9(7).
           03  QSS-OUTBAL-6-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-6-1-1    PIC S9(7).
           03  QSS-AMT-PL-6-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-6-1-2    PIC S9(7).
           03  QSS-AMT-PL-6-1-2   PIC S9(9)V99.
           03  QSS-UNITS-6-2-1    PIC S9(7).
           03  QSS-OUTBAL-6-2-1   PIC S9(9)V99.
           03  QSS-UNITS-6-2-2    PIC S9(7).
           03  QSS-OUTBAL-6-2-2   PIC S9(9)V99.
           03  QSS-UNITS-6-2-3    PIC S9(7).
           03  QSS-OUTBAL-6-2-3   PIC S9(9)V99.
           03  QSS-UNITS-6-2-4    PIC S9(7).
           03  QSS-OUTBAL-6-2-4   PIC S9(9)V99.
           03  QSS-UNITS-6-2-5    PIC S9(7).
           03  QSS-OUTBAL-6-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-6-2-1    PIC S9(7).
           03  QSS-AMT-PL-6-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-6-2-2    PIC S9(7).
           03  QSS-AMT-PL-6-2-2   PIC S9(9)V99.
           03  QSS-UNITS-7-1-1    PIC S9(7).
           03  QSS-OUTBAL-7-1-1   PIC S9(9)V99.
           03  QSS-UNITS-7-1-2    PIC S9(7).
           03  QSS-OUTBAL-7-1-2   PIC S9(9)V99.
           03  QSS-UNITS-7-1-3    PIC S9(7).
           03  QSS-OUTBAL-7-1-3   PIC S9(9)V99.
           03  QSS-UNITS-7-1-4    PIC S9(7).
           03  QSS-OUTBAL-7-1-4   PIC S9(9)V99.
           03  QSS-UNITS-7-1-5    PIC S9(7).
           03  QSS-OUTBAL-7-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-7-1-1    PIC S9(7).
           03  QSS-AMT-PL-7-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-7-1-2    PIC S9(7).
           03  QSS-AMT-PL-7-1-2   PIC S9(9)V99.
           03  QSS-UNITS-7-2-1    PIC S9(7).
           03  QSS-OUTBAL-7-2-1   PIC S9(9)V99.
           03  QSS-UNITS-7-2-2    PIC S9(7).
           03  QSS-OUTBAL-7-2-2   PIC S9(9)V99.
           03  QSS-UNITS-7-2-3    PIC S9(7).
           03  QSS-OUTBAL-7-2-3   PIC S9(9)V99.
           03  QSS-UNITS-7-2-4    PIC S9(7).
           03  QSS-OUTBAL-7-2-4   PIC S9(9)V99.
           03  QSS-UNITS-7-2-5    PIC S9(7).
           03  QSS-OUTBAL-7-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-7-2-1    PIC S9(7).
           03  QSS-AMT-PL-7-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-7-2-2    PIC S9(7).
           03  QSS-AMT-PL-7-2-2   PIC S9(9)V99.
           03  QSS-UNITS-8-1-1    PIC S9(7).
           03  QSS-OUTBAL-8-1-1   PIC S9(9)V99.
           03  QSS-UNITS-8-1-2    PIC S9(7).
           03  QSS-OUTBAL-8-1-2   PIC S9(9)V99.
           03  QSS-UNITS-8-1-3    PIC S9(7).
           03  QSS-OUTBAL-8-1-3   PIC S9(9)V99.
           03  QSS-UNITS-8-1-4    PIC S9(7).
           03  QSS-OUTBAL-8-1-4   PIC S9(9)V99.
           03  QSS-UNITS-8-1-5    PIC S9(7).
           03  QSS-OUTBAL-8-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-8-1-1    PIC S9(7).
           03  QSS-AMT-PL-8-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-8-1-2    PIC S9(7).
           03  QSS-AMT-PL-8-1-2   PIC S9(9)V99.
           03  QSS-UNITS-8-2-1    PIC S9(7).
           03  QSS-OUTBAL-8-2-1   PIC S9(9)V99.
           03  QSS-UNITS-8-2-2    PIC S9(7).
           03  QSS-OUTBAL-8-2-2   PIC S9(9)V99.
           03  QSS-UNITS-8-2-3    PIC S9(7).
           03  QSS-OUTBAL-8-2-3   PIC S9(9)V99.
           03  QSS-UNITS-8-2-4    PIC S9(7).
           03  QSS-OUTBAL-8-2-4   PIC S9(9)V99.
           03  QSS-UNITS-8-2-5    PIC S9(7).
           03  QSS-OUTBAL-8-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-8-2-1    PIC S9(7).
           03  QSS-AMT-PL-8-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-8-2-2    PIC S9(7).
           03  QSS-AMT-PL-8-2-2   PIC S9(9)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
