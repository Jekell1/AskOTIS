      * COPYMEMBER: LIBLP/LP01LH_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN HISTORY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LH1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LH1-CURSOR-STAT-GOOD              VALUE "00".
           88  LH1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{NOT GREATER CURSOR FOR START-NOT-GREATER & READ-PREVIOUS}
       01  LH1-CURSOR-NG-STAT           PIC X(2) VALUE SPACES.
           88  LH1-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LH1-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LH1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLH1-WHERE-BEG-KEY.
           03  QLH1-WBEG-BRNO             PIC 9(04).
           03  QLH1-WBEG-SSNO             PIC 9(09).
           03  QLH1-WBEG-ACCTNO           PIC 9(08).
       01  QLH1-WHERE-END-KEY.
           03  QLH1-WEND-BRNO             PIC 9(04).
           03  QLH1-WEND-SSNO             PIC 9(09).
           03  QLH1-WEND-ACCTNO           PIC 9(08).
 
 
       01  QLH-REC.
           03  QLH-BRNO         PIC 9(4).
           03  QLH-SSNO         PIC 9(10).
           03  QLH-ACCTNO       PIC 9(8).
           03  QLH-LTOUCH-DATE      PIC X(10).
           03  QLH-CLASS            PIC 9(3).
           03  QLH-LAWCD            PIC 99.
           03  QLH-COLLCD           PIC XX.
           03  QLH-MAKERCD          PIC X.
           03  QLH-POCD             PIC X(3).
           03  QLH-LOANDATE         PIC X(10).
           03  QLH-PLDATE           PIC X(10).
           03  QLH-JDDATE           PIC X(10).
           03  QLH-POFFDATE         PIC X(10).
           03  QLH-LNAMT            PIC S9(7)V99.
           03  QLH-ORGTERM          PIC 9(3).
           03  QLH-POTERMS          PIC 9(3).
           03  QLH-PYPROFILE        PIC 9(12).
           03  QLH-DLNO             PIC 9(4)V99.
           03  QLH-STATUS           PIC X(10).
           03  QLH-ARCHIVE-FG       PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
