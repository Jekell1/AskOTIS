      * COPYMEMBER: LIBLP/LP01ST_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - STATE CODE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  ST1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  ST1-CURSOR-STAT-GOOD              VALUE "00".
           88  ST1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  ST1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  ST1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  ST1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  ST1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QST1-WHERE-BEG-KEY.
           03  QST1-WBEG-CODE           PIC X(02).

       01  QST1-WHERE-END-KEY.
           03  QST1-WEND-CODE           PIC X(02).

       01  QST-REC.
           03  QST-CODE                      PIC X(02).
           03  QST-LTOUCH-DATE               PIC X(10).
           03  QST-DESC                      PIC X(16).
           03  QST-REINSTATE-WINDOW          PIC X(01).
           03  QST-REPO-RENOTICE             PIC X(01).
           03  QST-TITLE-RELEASE-TYPE        PIC X(01).
           03  QST-AUCTION-DATE-REQ          PIC X(01).
           03  QST-GEOGRAPHIC-CODE           PIC 9(04).
           03  QST-MAX-LOAN-AVAIL            PIC 9(6)V99.
           03  QST-MAX-TAX-LOAN-AMT          PIC S9(7)V99.
           03  QST-NSF-FEE-AMT               PIC S9(5)V99.

       EXEC SQL END DECLARE SECTION END-EXEC. 
