      * COPYMEMBER: LIBLP/LP01TRD_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - DEPOSITS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRD1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRD1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRD1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRD2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRD2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRD2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRD1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  TRD2-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QTRD1-WHERE-BEG-KEY.
           03  QTRD1-WBEG-BRNO             PIC 9(4).
           03  QTRD1-WBEG-DATE             PIC X(10).
 
       01  QTRD1-WHERE-END-KEY.
           03  QTRD1-WEND-BRNO             PIC 9(4).
           03  QTRD1-WEND-DATE             PIC X(10).

       01  QTRD2-WHERE-BEG-KEY.
           03  QTRD2-WBEG-DATE             PIC X(10).
           03  QTRD2-WBEG-BRNO             PIC 9(4).
 
       01  QTRD2-WHERE-END-KEY.
           03  QTRD2-WEND-DATE             PIC X(10).
           03  QTRD2-WEND-BRNO             PIC 9(4).
 
       01  QTRD-REC.
           03  QTD-BRNO             PIC 9(4).
           03  QTD-DATE             PIC X(10).
           03  QTD-LTOUCH-DATE      PIC X(10).
           03  QTD-DEPOSIT          PIC S9(7)V99.
           03  QTD-CHECK-DEPOSIT    PIC S9(7)V99.
           03  QTD-BANK             PIC 9(11).
           03  QTD-CASH             PIC 9(11).
           03  QTD-GLFG             PIC X.
           03  QTD-GLOVERSHORT      PIC 9(11).
           03  QTD-OVERSHORT        PIC S9(5)V99.
           03  QTD-USERID           PIC X(10).
           03  QTD-POSTDATE         PIC X(10).
           03  QTD-POSTTIME         PIC 9(6).

       EXEC SQL END DECLARE SECTION END-EXEC.
