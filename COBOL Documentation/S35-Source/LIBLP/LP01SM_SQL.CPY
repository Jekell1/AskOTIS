      * COPYMEMBER: LIBLP/LP01SM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - STATE PARAMETER MEMO FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      * JKC 2022-0913 ADDED QSM1-WHERE-SCAN-BEG-KEY.
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  SM1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  SM1-CURSOR-STAT-GOOD              VALUE "00".
           88  SM1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  SM1-CURSOR-LT-STAT           PIC X(2) VALUE SPACES.
           88  SM1-CURSOR-LT-STAT-GOOD           VALUE "00".
           88  SM1-CURSOR-LT-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  SM1-CURSOR-SC-STAT           PIC X(2) VALUE SPACES.
           88  SM1-CURSOR-SC-STAT-GOOD           VALUE "00".
           88  SM1-CURSOR-SC-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SM1-RETURN-CODE               PIC S9(4)   COMP-5.

       01  QSM1-WHERE-BEG-KEY.
           03  QSM1-WBEG-ORGST           PIC X(02).
           03  QSM1-WBEG-SPRCLASS        PIC 9(03).
           03  QSM1-WBEG-SUBCLASS        PIC 9(02).
           03  QSM1-WBEG-LAWCODE         PIC 9(02).
           03  QSM1-WBEG-DATE-REVERSED   PIC 9(8).
           03  QSM1-WBEG-SEQ             PIC 9(03).
 
       01  QSM1-WHERE-END-KEY.
           03  QSM1-WEND-ORGST           PIC X(02).
           03  QSM1-WEND-SPRCLASS        PIC 9(03).
           03  QSM1-WEND-SUBCLASS        PIC 9(02).
           03  QSM1-WEND-LAWCODE         PIC 9(02).
           03  QSM1-WEND-DATE-REVERSED   PIC 9(8).
           03  QSM1-WEND-SEQ             PIC 9(03).
 
       01  QSM1-WHERE-SCAN-BEG-KEY.
      *--> 03  QSM1-WSBEG-ORGST           PIC X(02).   ***NOT NEEDED***
      *--> 03  QSM1-WSBEG-SPRCLASS        PIC 9(03).   ***NOT NEEDED***
      *--> 03  QSM1-WSBEG-SUBCLASS        PIC 9(02).   ***NOT NEEDED***
      *--> 03  QSM1-WSBEG-LAWCODE         PIC 9(02).   ***NOT NEEDED***
           03  QSM1-WSBEG-DATE-REVERSED   PIC 9(8).
           03  QSM1-WSBEG-SEQ             PIC 9(03).                    
 
       01  QSM-REC.
           03  QSM-SP-ORGST        PIC X(02).
           03  QSM-SP-SPRCLASS     PIC 9(03).
           03  QSM-SP-SUBCLASS     PIC 9(02).
           03  QSM-SP-LAWCODE      PIC 9(02).
           03  QSM-DATE-REVERSED   PIC 9(08).
           03  QSM-SEQ             PIC 9(03).
           03  QSM-LTOUCH-DATE     PIC X(10).
           03  QSM-LTOUCH-TIME-HHMM       PIC 9(4).
           03  QSM-LTOUCH-USERID          PIC X(10).
           03  QSM-MEMO1                  PIC X(30).
           03  QSM-MEMO2                  PIC X(30).
           03  QSM-MEMO-TYPE              PIC XX.

       EXEC SQL END DECLARE SECTION END-EXEC.
