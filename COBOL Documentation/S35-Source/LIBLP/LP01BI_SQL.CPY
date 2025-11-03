      * COPYMEMBER: LIBLP/LP01BI_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BORROWER INDEX FILE
      *
      *=================================================================
      * REV:
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BI1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BI1-CURSOR-STAT-GOOD              VALUE "00".
           88  BI1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BI1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  BI1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  BI1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BI2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BI2-CURSOR-STAT-GOOD              VALUE "00".
           88  BI2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BI2-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  BI2-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  BI2-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BI3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BI3-CURSOR-STAT-GOOD              VALUE "00".
           88  BI3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BI3-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  BI3-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  BI3-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  BI1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  BI2-RETURN-CODE           PIC S9(4)   COMP-5.
       01  BI3-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QBI1-WHERE-BEG-KEY.
           03  QBI1-WBEG-SSNO       PIC 9(09).
           03  QBI1-WBEG-ACCTNO     PIC 9(08).
           03  QBI1-WBEG-OWNBR      PIC 9(04).

       01  QBI1-WHERE-END-KEY.
           03  QBI1-WEND-SSNO       PIC 9(09).
           03  QBI1-WEND-ACCTNO     PIC 9(08).
           03  QBI1-WEND-OWNBR      PIC 9(04).

       01  QBI2-WHERE-BEG-KEY.
           03  QBI2-WBEG-LNAME      PIC X(35).
           03  QBI2-WBEG-FNAME      PIC X(35).

       01  QBI2-WHERE-END-KEY.
           03  QBI2-WEND-LNAME      PIC X(35).
           03  QBI2-WEND-FNAME      PIC X(35).

       01  QBI3-WHERE-BEG-KEY.
           03  QBI3-WBEG-OWNBR      PIC 9(04).
           03  QBI3-WBEG-SSNO       PIC 9(09).
           03  QBI3-WBEG-ACCTNO     PIC 9(08).

       01  QBI3-WHERE-END-KEY.
           03  QBI3-WEND-OWNBR      PIC 9(04).
           03  QBI3-WEND-SSNO       PIC 9(09).
           03  QBI3-WEND-ACCTNO     PIC 9(08).

       01  QBI-REC.
           03  QBI-SSNO             PIC 9(9).
           03  QBI-ACCTNO           PIC 9(8).
           03  QBI-OWNBR            PIC 9(4).
           03  QBI-LNAME            PIC X(35).
           03  QBI-FNAME            PIC X(35).
           03  QBI-LTOUCH-DATE      PIC X(10).
           03  QBI-MAKERCD          PIC X.
           03  QBI-ORGST            PIC XX.
           03  QBI-GIFTNET          PIC X.
           03  QBI-POFFDATE         PIC X(10).
           03  QBI-ENTDATE          PIC X(10).
           03  QBI-SALEFIN          PIC X.
           03  QBI-CONVERCD         PIC X.
           03  QBI-PLCD             PIC X.
           03  QBI-SMALLOAN         PIC X.
           03  QBI-PLDATE           PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
