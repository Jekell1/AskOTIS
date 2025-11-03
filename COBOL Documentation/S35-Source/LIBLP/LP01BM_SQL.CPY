      * COPYMEMBER: LIBLP/LP01BM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BORROWER MEMO FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BM1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BM1-CURSOR-STAT-GOOD              VALUE "00".
           88  BM1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  BM1-CURSOR-LT-STAT           PIC X(2) VALUE SPACES.
           88  BM1-CURSOR-LT-STAT-GOOD           VALUE "00".
           88  BM1-CURSOR-LT-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  BM1-CURSOR-SC-STAT           PIC X(2) VALUE SPACES.
           88  BM1-CURSOR-SC-STAT-GOOD           VALUE "00".
           88  BM1-CURSOR-SC-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  BM1-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QBM1-WHERE-BEG-KEY.
           03  QBM1-WBEG-BRNO             PIC 9(04).
           03  QBM1-WBEG-SSNO             PIC 9(09).
           03  QBM1-WBEG-DATE-REV         PIC 9(08).
           03  QBM1-WBEG-SEQ              PIC 9(03).
 
       01  QBM1-WHERE-END-KEY.
           03  QBM1-WEND-BRNO             PIC 9(04).
           03  QBM1-WEND-SSNO             PIC 9(09).
           03  QBM1-WEND-DATE-REV         PIC 9(08).
           03  QBM1-WEND-SEQ              PIC 9(03).
 
       01  QBM1-WHERE-SCAN-BEG-KEY.
      *--> 03  QBM1-WSBEG-BRNO            PIC 9(04).  ***NOT NEEDED***
      *--> 03  QBM1-WSBEG-SSNO            PIC 9(09).  ***NOT NEEDED***
           03  QBM1-WSBEG-DATE-REV        PIC 9(08).
           03  QBM1-WSBEG-SEQ             PIC 9(03).
 
       01  QBM-REC.
           03  QBM-BRNO                 PIC 9(4).
           03  QBM-SSNO                 PIC 9(9).
           03  QBM-DATE-REVERSED        PIC 9(8).
           03  QBM-SEQ                  PIC 9(3).
           03  QBM-LTOUCH-DATE          PIC X(10).
           03  QBM-LTOUCH-TIME-HHMM     PIC 9(4).
           03  QBM-LTOUCH-USERID        PIC X(10).
           03  QBM-MEMO                 PIC X(60).
           03  QBM-MEMO-TYPE            PIC XX.

       EXEC SQL END DECLARE SECTION END-EXEC.
