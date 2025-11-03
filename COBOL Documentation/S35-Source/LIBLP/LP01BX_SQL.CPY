      * COPYMEMBER: LIBLP/LP01BX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BORROWER TRAILER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  BX1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  BX1-CURSOR-STAT-GOOD              VALUE "00".
           88  BX1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~". 

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  BX1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QBX1-WHERE-BEG-KEY.
           03  QBX1-WBEG-BRNO             PIC 9(4).
           03  QBX1-WBEG-SSNO             PIC 9(9).
       01  QBX1-WHERE-END-KEY.
           03  QBX1-WEND-BRNO             PIC 9(4).
           03  QBX1-WEND-SSNO              PIC 9(9).
 
       01  QBX-REC.
           03  QBX-BRNO                 PIC 9(4).
           03  QBX-SSNO                 PIC 9(9).
           03  QBX-LTOUCH-DATE          PIC X(10).
           03  QBX-ALT-ADR1             PIC X(30).
           03  QBX-ALT-ADR2             PIC X(30).
           03  QBX-ALT-CITY             PIC X(30).
           03  QBX-ALT-STATE            PIC X(2).
           03  QBX-ALT-ZIP              PIC X(10).
           03  QBX-PREV-ADR1            PIC X(30).
           03  QBX-PREV-ADR2            PIC X(30).
           03  QBX-PREV-CITY            PIC X(30).
           03  QBX-PREV-STATE           PIC X(2).
           03  QBX-PREV-ZIP             PIC X(10).
           03  QBX-KYC-ADR1             PIC X(24).
           03  QBX-KYC-ADR2             PIC X(24).
           03  QBX-KYC-CITY             PIC X(15).
           03  QBX-KYC-STATE            PIC X(2).
           03  QBX-KYC-ZIP              PIC X(10).
           03  QBX-KYC-IDTYPE           PIC X(2).
           03  QBX-KYC-ISSAUTH          PIC X(2).
           03  QBX-KYC-NUMBER           PIC X(25).
           03  QBX-KYC-ISSDATE          PIC X(10).
           03  QBX-KYC-EXPDATE          PIC X(10).
           03  QBX-KYC-LAST-VERDATE     PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
