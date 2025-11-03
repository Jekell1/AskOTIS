      * COPYMEMBER: LIBLP/LP01LE_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN ENDORSER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LE1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LE1-CURSOR-STAT-GOOD              VALUE "00".
           88  LE1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LE1-RETURN-CODE               PIC S9(4)   COMP-5.

       01  QLE1-WHERE-BEG-KEY.
           03  QLE1-WBEG-BRNO            PIC 9(04).
           03  QLE1-WBEG-ACCTNO          PIC 9(08).
           03  QLE1-WBEG-TYPE            PIC X.
           03  QLE1-WBEG-FLAG            PIC X.
           03  QLE1-WBEG-SSNO            PIC 9(09).
           03  QLE1-WBEG-OSACCTNO        PIC 9(08).
       01  QLE1-WHERE-END-KEY.
           03  QLE1-WEND-BRNO            PIC 9(04).
           03  QLE1-WEND-ACCTNO          PIC 9(08).
           03  QLE1-WEND-TYPE            PIC X.
           03  QLE1-WEND-FLAG            PIC X.
           03  QLE1-WEND-SSNO            PIC 9(09).
           03  QLE1-WEND-OSACCTNO        PIC 9(08).
 
 
       01  QLE-REC.
           03  QLE-BRNO             PIC 9(4).
           03  QLE-ACCTNO           PIC 9(8).
           03  QLE-TYPE             PIC X.
           03  QLE-FLAG             PIC X.
           03  QLE-SSNO             PIC 9(9).
           03  QLE-OSACCTNO         PIC 9(8).
           03  QLE-LTOUCH-DATE      PIC X(10).
           03  QLE-E-OSRATE             PIC 999V999.
           03  QLE-E-OSAMT              PIC 9(7)V99.
           03  QLE-E-OSDATE             PIC X(10).
           03  QLE-E-AMT                PIC 9(7)V99.
           03  QLE-E-DATE               PIC X(10).
           03  QLE-E-OSMAKERCD          PIC X.
           03  QLE-R-LOANDATE           PIC X(10).
           03  QLE-R-LNAMT              PIC 9(7)V99.
           03  QLE-R-POFFDATE           PIC X(10).
           03  QLE-R-RESTRICTED-MONEY   PIC 9(7)V99.
           03  QLE-R-BALANCE            PIC 9(7)V99.
           03  QLE-R-SALEFIN            PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
