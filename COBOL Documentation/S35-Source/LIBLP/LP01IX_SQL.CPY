      * COPYMEMBER: LIBLP/LP01IX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - ALTERNATE INDEX FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  IX1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  IX1-CURSOR-STAT-GOOD              VALUE "00".
           88  IX1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                        "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  IX2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  IX2-CURSOR-STAT-GOOD              VALUE "00".
           88  IX2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  IX3-CURSOR-STAT              PIC X(3) VALUE SPACES.
           88  IX3-CURSOR-STAT-GOOD              VALUE "00".
           88  IX3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  IX1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  IX2-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  IX3-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QIX1-WHERE-BEG-KEY.
           03  QIX1-WBEG-BRNO              PIC 9(04).
           03  QIX1-WBEG-CODE              PIC X(01).
           03  QIX1-WBEG-ACCTNO            PIC 9(08).
           03  QIX1-WBEG-SEQNO             PIC 9(03).
 
       01  QIX1-WHERE-END-KEY.
           03  QIX1-WEND-BRNO              PIC 9(04).
           03  QIX1-WEND-CODE              PIC X(01).
           03  QIX1-WEND-ACCTNO            PIC 9(08).
           03  QIX1-WEND-SEQNO             PIC 9(03).
 
       01  QIX2-WHERE-BEG-KEY.
           03  QIX2-WBEG-BRNO              PIC 9(04).
           03  QIX2-WBEG-VIN               PIC X(30).

       01  QIX2-WHERE-END-KEY.
           03  QIX2-WEND-BRNO              PIC 9(04).
           03  QIX2-WEND-VIN               PIC X(30).

       01  QIX3-WHERE-BEG-KEY.
           03  QIX3-WBEG-VIN               PIC X(30).

       01  QIX3-WHERE-END-KEY.
           03  QIX3-WEND-VIN               PIC X(30).

       01  QIX-REC.
           03  QIX-BRNO               PIC 9(04).
           03  QIX-CODE               PIC X(01).
           03  QIX-ACCTNO             PIC 9(08).
           03  QIX-SEQNO              PIC 9(03).
           03  QIX-VIN                PIC X(30).
           03  QIX-LTOUCH-DATE        PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
