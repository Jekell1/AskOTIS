      * COPYMEMBER: LIBLP/LP01CI_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CUSTOMER INFORMATION FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CI1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CI1-CURSOR-STAT-GOOD              VALUE "00".
           88  CI1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CI2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CI2-CURSOR-STAT-GOOD              VALUE "00".
           88  CI2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CI3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CI3-CURSOR-STAT-GOOD              VALUE "00".
           88  CI3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CI1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  CI2-RETURN-CODE           PIC S9(4)   COMP-5.
       01  CI3-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QCI1-WHERE-BEG-KEY.
           03  QCI1-WBEG-BRNO              PIC 9(04).
           03  QCI1-WBEG-ACCTNO            PIC 9(08).
 
       01  QCI1-WHERE-END-KEY.
           03  QCI1-WEND-BRNO              PIC 9(04).
           03  QCI1-WEND-ACCTNO            PIC 9(08).

       01  QCI2-WHERE-BEG-KEY.
           03  QCI2-WBEG-BRNO              PIC 9(04).
           03  QCI2-WBEG-SSNO1             PIC 9(09).
 
       01  QCI2-WHERE-END-KEY.
           03  QCI2-WEND-BRNO              PIC 9(04).
           03  QCI2-WEND-SSNO1             PIC 9(09).
 
       01  QCI3-WHERE-BEG-KEY.
           03  QCI3-WBEG-BRNO              PIC 9(04).
           03  QCI3-WBEG-SSNO2             PIC 9(09).
 
       01  QCI3-WHERE-END-KEY.
           03  QCI3-WEND-BRNO              PIC 9(04).
           03  QCI3-WEND-SSNO2             PIC 9(09).
 
       01  QCI-REC.
           03  QCI-BRNO                    PIC 9(4).
           03  QCI-ACCTNO                  PIC 9(8).
           03  QCI-SSNO                    PIC 9(9).
           03  QCI-SSNO-2                  PIC 9(9).
           03  QCI-LTOUCH-DATE             PIC X(10).
           03  QCI-DATE-UPDATED            PIC X(10).
           03  QCI-AMT-REQUESTED           PIC S9(7).
           03  QCI-MARITAL-STAT            PIC X.
           03  QCI-YEARS-IN-AREA           PIC 99.
           03  QCI-DATE-AT-PRESENT-ADR     PIC X(10).
           03  QCI-MONTHLY-RENT-MTG        PIC S9(7).
           03  QCI-FAIR-MARKET-VALUE       PIC S9(7).
           03  QCI-MTG-BALANCE             PIC S9(7).
           03  QCI-CHECKING-ACCOUNT        PIC X.
           03  QCI-SAVINGS-ACCOUNT         PIC X.
           03  QCI-TOTAL-MO-INCOME         PIC S9(7).
           03  QCI-LESS-MO-PAY             PIC S9(7).
           03  QCI-DATE-HIRE-1             PIC X(10).
           03  QCI-POSITION-CODE-1         PIC X(5).
           03  QCI-GROSS-MO-INCOME-1       PIC S9(7).
           03  QCI-DATE-HIRE-2             PIC X(10).
           03  QCI-POSITION-CODE-2         PIC X(5).
           03  QCI-GROSS-MO-INCOME-2       PIC S9(7).

       EXEC SQL END DECLARE SECTION END-EXEC.
