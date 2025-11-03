      * COPYMEMBER: LIBLP/LP01DM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DEALER MEMO FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  DM1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  DM1-CURSOR-STAT-GOOD              VALUE "00".
           88  DM1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  DM1-CURSOR-LT-STAT           PIC X(2) VALUE SPACES.
           88  DM1-CURSOR-LT-STAT-GOOD           VALUE "00".
           88  DM1-CURSOR-LT-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  DM1-CURSOR-SC-STAT           PIC X(2) VALUE SPACES.
           88  DM1-CURSOR-SC-STAT-GOOD           VALUE "00".
           88  DM1-CURSOR-SC-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DM1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QDM1-WHERE-BEG-KEY.
           03  QDM1-WBEG-BRNO             PIC 9(04).
           03  QDM1-WBEG-DLNO             PIC 9(04)V99.
           03  QDM1-WBEG-DATE-REV         PIC 9(08).
           03  QDM1-WBEG-SEQ              PIC 9(03).
 
       01  QDM1-WHERE-END-KEY.
           03  QDM1-WEND-BRNO             PIC 9(04).
           03  QDM1-WEND-DLNO             PIC 9(04)V99.
           03  QDM1-WEND-DATE-REV         PIC 9(08).
           03  QDM1-WEND-SEQ              PIC 9(03).
 
       01  QDM1-WHERE-SCAN-BEG-KEY.
      *--> 03  QDM1-WSBEG-BRNO            PIC 9(04).   ***NOT NEEDED***
           03  QDM1-WSBEG-DLNO            PIC 9(04)V99.
           03  QDM1-WSBEG-DATE-REV        PIC 9(08).
           03  QDM1-WSBEG-SEQ             PIC 9(03).                     
       
       01  QDM-REC.
           03  QDM-BRNO                 PIC 9(4).
           03  QDM-DL-DLNO              PIC 9(4)V99.
           03  QDM-DATE-REVERSED        PIC 9(08).
           03  QDM-SEQ                  PIC 9(3).
           03  QDM-LTOUCH-DATE          PIC X(10).
           03  QDM-LTOUCH-TIME-HHMM     PIC 9(4).
           03  QDM-LTOUCH-USERID        PIC X(10).
           03  QDM-MEMO                 PIC X(60).
           03  QDM-MEMO-TYPE            PIC XX.

       EXEC SQL END DECLARE SECTION END-EXEC.
