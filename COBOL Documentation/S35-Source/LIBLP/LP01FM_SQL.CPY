      * COPYMEMBER: LIBLP/LP01FM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - FORM REQUEST FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  FM1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  FM1-CURSOR-STAT-GOOD              VALUE "00".
           88  FM1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  FM2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  FM2-CURSOR-STAT-GOOD              VALUE "00".
           88  FM2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  FM1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  FM2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QFM1-WHERE-BEG-KEY.
           03  QFM1-WBEG-BRNO             PIC 9(04).
           03  QFM1-WBEG-INITKEY          PIC X(20).
           03  QFM1-WBEG-FMNO             PIC 9(04).

       01  QFM1-WHERE-END-KEY.
           03  QFM1-WEND-BRNO             PIC 9(04).
           03  QFM1-WEND-INITKEY          PIC X(20).
           03  QFM1-WEND-FMNO             PIC 9(04).

       01  QFM2-WHERE-BEG-KEY.
           03  QFM2-WBEG-BRNO             PIC 9(04).
           03  QFM2-WBEG-FMNO             PIC 9(04).
           03  QFM2-WBEG-ZIP              PIC X(15).

       01  QFM2-WHERE-END-KEY.
           03  QFM2-WEND-BRNO             PIC 9(04).
           03  QFM2-WEND-FMNO             PIC 9(04).
           03  QFM2-WEND-ZIP              PIC X(15).
 
       01  QFM-REC.
           03  QFM-BRNO                 PIC 9(4).
           03  QFM-INITKEY              PIC X(20).
           03  QFM-FMNO                 PIC 9(4).
           03  QFM-ZIP                      PIC X(15).
           03  QFM-PRINT                    PIC X.
           03  QFM-STMT-COUNTER             PIC 9(5).
           03  QFM-TRDATE                   PIC X(10).
           03  QFM-USERID                   PIC X(10).
           03  QFM-LTOUCH-DATE              PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
