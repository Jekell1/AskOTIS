      * COPYMEMBER: LIBLP/LP01BX
      **************************************************************************
      *            ( BORROWER TRAILER FILE )
      *                     LP01BX
      *
      *  BX1-KEY = BX-BRNO, BX-SSNO
      * 
      * REV:
      *  160510 BAH NEW BORROWER TRAILER FOR MEXICO INFORMATION
      *  170222 BAH CHANGED RFC FROM 12 TO 15, OFFICIAL FROM 19 TO 20 AND
      *             CUPR FROM 18 TO 20
      *  190415  CS REMOVE MEXICO FIELDS, ADD FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  BX-SIZE                     VALUE  490.
       01  BX-REC.
           03  BX1-KEY.
               05  BX-BRNO             PIC 9(4).
               05  BX-SSNO             PIC 9(9).

      $XFD DATE=YYYYMMDD
           03  BX-LTOUCH-DATE          PIC 9(8) COMP-3.
           03  BX-ALT-ADDRESS.
               05  BX-ALT-ADR1         PIC X(30).
               05  BX-ALT-ADR2         PIC X(30).
               05  BX-ALT-CITY         PIC X(30).
               05  BX-ALT-STATE        PIC X(2).
               05  BX-ALT-ZIP          PIC X(10).

           03  BX-PREV-ADDRESS.
               05  BX-PREV-ADR1        PIC X(30).
               05  BX-PREV-ADR2        PIC X(30).
               05  BX-PREV-CITY        PIC X(30).
               05  BX-PREV-STATE       PIC X(2).
               05  BX-PREV-ZIP         PIC X(10).

           03  BX-KNOW-YOUR-CUSTOMER.
               05  BX-KYC-ADR1         PIC X(24).
               05  BX-KYC-ADR2         PIC X(24).
               05  BX-KYC-CITY         PIC X(15).
               05  BX-KYC-STATE        PIC X(2).
               05  BX-KYC-ZIP          PIC X(10).
               05  BX-KYC-IDTYPE       PIC X(2).
                   88  BX-KYC-IDTYPE-VALID VALUE "A" "C" "D" "O"
                                                 "P" "R" "T".
               05  BX-KYC-ISSAUTH      PIC X(2).
               05  BX-KYC-NUMBER       PIC X(25).
      $XFD DATE=YYYYMMDD
               05  BX-KYC-ISSDATE      PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
               05  BX-KYC-EXPDATE      PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
               05  BX-KYC-LAST-VERDATE PIC 9(8) COMP-3.
               05  FILLER              PIC X(149).
