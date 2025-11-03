      * COPYMEMBER: LIBLP/LP01BX
      **************************************************************************
      *            ( BORROWER TRAILER FILE )
      *                     LP01BX
      *
      * REV:
      *  160510 BAH NEW BORROWER TRAILER FOR MEXICO INFORMATION
      *  170222 BAH CHANGED RFC FROM 12 TO 15, OFFICIAL FROM 19 TO 20 AND
      *             CUPR FROM 18 TO 20
      **************************************************************************
       78  BX-SIZE                     VALUE 1050.
       01  BX-REC.
           03  BX-BRNO                 PIC 9(4).
           03  BX1-KEY.
               05  BX-SSNO             PIC 9(9).

      *>>XDD DATE=YYYYMMDD
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
      *>>XDD DATE=YYYYMMDD
               05  BX-KYC-ISSDATE      PIC 9(8) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  BX-KYC-EXPDATE      PIC 9(8) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  BX-KYC-LAST-VERDATE PIC 9(8) COMP-3.

      * 99999999, SEQ 5
           03  BX-SECOND-LNAME         PIC X(35).
      * OBX-ALT-ADR1
           03  BX-PART-OF-TOWN         PIC X(30).
      * OBX-ALT-ADR2
           03  BX-CROSS-STREET         PIC X(30).
           03  BX-MUNICIPAL            PIC X(30).
           03  BX-EMP-PART-OF-TOWN     PIC X(30).
           03  BX-EMP-MUNICIPAL        PIC X(30).
           03  BX-RFC-TAX-ID           PIC X(15).
      * OFFICIAL ID (VOTER REG #, PASSPORT #, PROFESSIONAL #, ETC)
           03  BX-OFFICIAL-ID          PIC X(20).
      * OBX-EMAIL
           03  BX-SOURCE-OF-INCOME     PIC X(35).
      * 99999999, SEQ 4
           03  BX-PURPOSE-OF-LOAN      PIC X(35).
      * 99999999, SEQ 6
           03  BX-UNION-ACCTNO         PIC X(20).
      * NATIONAL ID# - REQUIRED
           03  BX-CURP                 PIC X(20).

           03  FILLER                  PIC X(379).
