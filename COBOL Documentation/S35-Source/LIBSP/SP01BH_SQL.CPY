      * COPYMEMBER: LIBSP/SP01BH_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BULLETIN BOARD HISTORY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BH1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BH1-CURSOR-STAT-GOOD              VALUE "00".
           88  BH1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BH1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  BH1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  BH1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  BH1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QBH1-WHERE-BEG-KEY.
           03  QBH1-WBEG-USERID         PIC X(10).
           03  QBH1-WBEG-DATE           PIC 9(08).
           03  QBH1-WBEG-TIME           PIC 9(04).

       01  QBH1-WHERE-END-KEY.
           03  QBH1-WEND-USERID         PIC X(10).
           03  QBH1-WEND-DATE           PIC 9(08).
           03  QBH1-WEND-TIME           PIC 9(04).

       01  QBH-REC.
           03  QBH-USERID            PIC X(10).
           03  QBH-DATE-REVERSED     PIC 9(08).
           03  QBH-TIME              PIC 9(4).
           03  QBH-GROUP             PIC X(4).
           03  QBH-ACTION            PIC X(40).
           03  QBH-LTOUCH-DATE       PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
