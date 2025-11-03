      * COPYMEMBER: LIBUT/UT01ZM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - ZM STATE CODE FILE
      *
      *=================================================================
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  ZMB1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  ZMB1-CURSOR-STAT-GOOD              VALUE "00".
           88  ZMB1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  ZMB1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  ZMB1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  ZMB1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QZMB1-WHERE-BEG-KEY.
           03  QZMB1-WBEG-CODE           PIC X(02).

       01  QZMB1-WHERE-END-KEY.
           03  QZMB1-WEND-CODE           PIC X(02).

       01  QZMB-REC.
           03  QZMB-CODE                      PIC X(02).
           03  QZMB-LTOUCH-DATE               PIC X(10).
           03  QZMB-DESC                      PIC X(16).
           03  QZMB-REINSTATE-WINDOW          PIC X(01).
           03  QZMB-REPO-RENOTICE             PIC X(01).
           03  QZMB-TITLE-RELEASE-TYPE        PIC X(01).
           03  QZMB-AUCTION-DATE-REQ          PIC X(01).
           03  QZMB-GEOGRAPHIC-CODE           PIC 9(04).
           03  QZMB-MAX-LOAN-AVAIL            PIC 9(6)V99.
           03  QZMB-MAX-TAX-LOAN-AMT          PIC S9(7)V99.
           03  QZMB-NSF-FEE-AMT               PIC S9(5)V99.
           03  QZMB-TEST-AMOUNT               PIC S9(7)V99.
           03  QZMB-TEST-CODE                 PIC X(2).
           03  QZMB-TEST-DATE                 PIC 9(6).
           03  QZMB-TEST-NUM                  PIC 9(4).
           03  QZMB-TEST-RATE                 PIC 9(3)V999.
           03  QZMB-TEST-SSNO                 PIC 9(9).

       EXEC SQL END DECLARE SECTION END-EXEC. 
