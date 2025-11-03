      * COPYMEMBER: LIBSP/SP01BB_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BULLETIN BOARD FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BB1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BB1-CURSOR-STAT-GOOD              VALUE "00".
           88  BB1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BB1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  BB1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  BB1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BB2-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  BB2-CURSOR-STAT-GOOD               VALUE "00".
           88  BB2-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BB2-CURSOR-GR-STAT           PIC X(2)  VALUE SPACES.
           88  BB2-CURSOR-GR-STAT-GOOD            VALUE "00".
           88  BB2-CURSOR-GR-STAT-BAD             VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  BB1-RETURN-CODE              PIC S9(4)   COMP-5.
       01  BB2-RETURN-CODE              PIC S9(4)   COMP-5. 

       01  QBB1-WHERE-BEG-KEY.
           03  QBB1-WBEG-CATAGORY       PIC X(04).
           03  QBB1-WBEG-SUBCAT         PIC X(04).
           03  QBB1-WBEG-USERID         PIC X(10).
           03  QBB1-WBEG-SEQNO          PIC 9(06).

       01  QBB1-WHERE-END-KEY.
           03  QBB1-WEND-CATAGORY       PIC X(04).
           03  QBB1-WEND-SUBCAT         PIC X(04).
           03  QBB1-WEND-USERID         PIC X(10).
           03  QBB1-WEND-SEQNO          PIC 9(06).

       01  QBB2-WHERE-BEG-KEY.
           03  QBB2-WBEG-CATAGORY       PIC X(04).
           03  QBB2-WBEG-SUBCAT         PIC X(04).
           03  QBB2-WBEG-USERID         PIC X(10).
           03  QBB2-WBEG-NAME           PIC X(20).

       01  QBB2-WHERE-END-KEY.
           03  QBB2-WEND-CATAGORY       PIC X(04).
           03  QBB2-WEND-SUBCAT         PIC X(04).
           03  QBB2-WEND-USERID         PIC X(10).
           03  QBB2-WEND-NAME           PIC X(20).

       01  QBB-REC.
           03  QBB-CATAGORY        PIC X(04).
           03  QBB-SUBCATAGORY     PIC X(04).
           03  QBB-USERID          PIC X(10).
           03  QBB-SEQNO           PIC 9(06).
           03  QBB-NAME            PIC X(20).
           03  QBB-LOGNAME         PIC X(10).
           03  QBB-POSTNAME        PIC X(10).
           03  QBB-OTHERBR         PIC X(10).
           03  QBB-POSTDATE        PIC X(10).
           03  QBB-POSTTIME        PIC 9(06).
           03  QBB-PASSWORD        PIC X(08).
           03  QBB-GROUP           PIC X(04).
           03  QBB-BRNO            PIC 9(4).
           03  QBB-ACTION          PIC X(40).
           03  QBB-MEMO-LINE-1     PIC X(60).
           03  QBB-MEMO-LINE-2     PIC X(60).
           03  QBB-MEMO-LINE-3     PIC X(60).
           03  QBB-MEMO-LINE-4     PIC X(60).
           03  QBB-MEMO-LINE-5     PIC X(60).
           03  QBB-MEMO-LINE-6     PIC X(60).
           03  QBB-MEMO-LINE-7     PIC X(60).
           03  QBB-MEMO-LINE-8     PIC X(60).
           03  QBB-MEMO-LINE-9     PIC X(60).
           03  QBB-MEMO-LINE-10    PIC X(60).
           03  QBB-LTOUCH-DATE     PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
