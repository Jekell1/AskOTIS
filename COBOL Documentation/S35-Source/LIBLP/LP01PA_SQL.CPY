      * COPYMEMBER: LIBLP/LP01PA_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - PAYEE FILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  PA1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  PA1-CURSOR-STAT-GOOD              VALUE "00".
           88  PA1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  PA2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  PA2-CURSOR-STAT-GOOD              VALUE "00".
           88  PA2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  PA1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  PA2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QPA1-WHERE-BEG-KEY.
           03  QPA1-WBEG-BRNO             PIC 9(04).
           03  QPA1-WBEG-NUMBER           PIC 9(06).

       01  QPA1-WHERE-END-KEY.
           03  QPA1-WEND-BRNO             PIC 9(04).
           03  QPA1-WEND-NUMBER           PIC 9(06).

       01  QPA2-WHERE-BEG-KEY.
           03  QPA2-WBEG-BRNO             PIC 9(04).
           03  QPA2-WBEG-SORTKEY          PIC X(10).

       01  QPA2-WHERE-END-KEY.
           03  QPA2-WEND-BRNO             PIC 9(04).
           03  QPA2-WEND-SORTKEY          PIC X(10).
 
       01  QPA-REC.
           03  QPA-BRNO            PIC 9(4).
           03  QPA-NUMBER          PIC 9(6).
           03  QPA-NAME            PIC X(30).
           03  QPA-ADR1            PIC X(30).
           03  QPA-ADR2            PIC X(30).
           03  QPA-CITY            PIC X(20).
           03  QPA-STATE           PIC X(2).
           03  QPA-ZIP             PIC X(10).
           03  QPA-LCHECK          PIC 9(6).
           03  QPA-LCDATE          PIC X(10).
           03  QPA-LAMT            PIC S9(7)V99.
           03  QPA-YTD-PAID        PIC S9(7)V99.
           03  QPA-LYTD-PAID       PIC S9(7)V99.
           03  QPA-SORTKEY         PIC X(10).
           03  QPA-CUSNO           PIC X(12).
           03  QPA-CONTACT         PIC X(30).
           03  QPA-PHONE           PIC 9(10).
           03  QPA-FAX             PIC 9(10).
           03  QPA-1099-FLAG       PIC X.
           03  QPA-1099-ID         PIC X(12).
           03  QPA-LEGAL-NAME1     PIC X(50).
           03  QPA-LEGAL-NAME2     PIC X(50).
           03  QPA-VAT-PERC        PIC 99V99.
           03  QPA-RFC             PIC X(13).
           03  QPA-CURP            PIC X(18).
           03  QPA-LTOUCH-DATE     PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
