      * COPYMEMBER: LIBGL/GL01GLLOGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - G/L LOG - EOM GT PURGE
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GLLOGS1-CURSOR-STAT          PIC X(2) VALUE SPACES.
           88  GLLOGS1-CURSOR-STAT-GOOD          VALUE "00".
           88  GLLOGS1-CURSOR-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  GLLOGS2-CURSOR-STAT          PIC X(2) VALUE SPACES.
           88  GLLOGS2-CURSOR-STAT-GOOD          VALUE "00".
           88  GLLOGS2-CURSOR-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GLLOGS1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  GLLOGS2-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QGLLOGS1-WHERE-BEG-KEY.
           03  QGLLOGS1-WBEG-ID                PIC X(06).
           03  QGLLOGS1-WBEG-SYSTEM-DATE       PIC X(10).
           03  QGLLOGS1-WBEG-SEQNO             PIC 9(02).
 
       01  QGLLOGS1-WHERE-END-KEY.
           03  QGLLOGS1-WEND-ID                PIC X(06).
           03  QGLLOGS1-WEND-SYSTEM-DATE       PIC X(10).
           03  QGLLOGS1-WEND-SEQNO             PIC 9(02).
 
       01  QGLLOGS2-WHERE-BEG-KEY.
           03  QGLLOGS2-WBEG-ID                PIC X(06).
           03  QGLLOGS2-WBEG-BEG-DATE          PIC X(10).
           03  QGLLOGS2-WBEG-END-DATE          PIC X(10).
 
       01  QGLLOGS2-WHERE-END-KEY.
           03  QGLLOGS2-WEND-ID                PIC X(06).
           03  QGLLOGS2-WEND-BEG-DATE          PIC X(10).
           03  QGLLOGS2-WEND-END-DATE          PIC X(10).

       01  QGLLOGS-REC.
           03  QGLLOGS-ID           PIC X(6).
           03  QGLLOGS-SYSTEM-DATE  PIC X(10).
           03  QGLLOGS-SEQNO        PIC 9(2).
           03  QGLLOGS-BEG-DATE     PIC X(10).
           03  QGLLOGS-END-DATE         PIC X(10).
           03  QGLLOGS-BRNO-RANGE       PIC 9(4).
           03  QGLLOGS-TIME             PIC 9(6).
           03  QGLLOGS-RECS-PURGED      PIC 9(11).
           03  QGLLOGS-USERID           PIC X(10).
           03  QGLLOGS-LTOUCH-DATE      PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
