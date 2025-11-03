      * COPYMEMBER: LIBLP/LP01LTB
      ******************************************************************
      *    LOAN BANKRUPTCY TRAILER FILE : (800) BYTES
      *
      * 160502 BAH ADDED REAFFIRMATION/DISCHARGE FIELDS WORLD 1150
      **************************************************************************
       78  LTB-SIZE                      VALUE 800.
       01  LTB-REC.
           03  LTB1-KEY.
               05  LTB-BRNO              PIC 9(4).
               05  LTB-ACCTNO            PIC 9(8).
               05  LTB-SEQNO             PIC 999.

      *>>XDD DATE=YYYYMMDD
           03  LTB-LTOUCH-DATE          PIC 9(8)  COMP-3.

      *---------------------------------------------------------------
      *    BANKRUPT RECORD "B":
      *---------------------------------------------------------------
           03  LTB-BANKRUPT-AREA.
      *>>XDD DATE=YYYYMMDD
               05  LTB-FILEDATE         PIC 9(8)        COMP-3.
               05  LTB-CASE-NO          PIC X(9).
               05  LTB-CHAPTER          PIC 99.
               05  LTB-CHAPTER-X REDEFINES LTB-CHAPTER
                                        PIC XX.
               05  LTB-MISC1            PIC X(30).
               05  LTB-MISC2            PIC X(30).
               05  LTB-MISC3            PIC X(30).
      *>>XDD DATE=YYYYMMDD
               05  LTB-BDDATE           PIC 9(8)        COMP-3.
               05  LTB-WHO-FILED        PIC X.
               05  LTB-ENDORSER-FILED   PIC X.
               05  LTB-CLAIM-AMT        PIC S9(7)V99    COMP-3.
               05  LTB-ACTIVE-INACTIVE  PIC X.
               05  LTB-INT-LC-DUE       PIC S9(7)V99    COMP-3.
               05  LTB-OTH-DUE          PIC S9(7)V99    COMP-3.
               05  LTB-TDUEAMT          PIC S9(7)V99    COMP-3.

          
      * (1) DEBTOR'S ATTORNEY
      * (2) CLERK OF BANKRUPTCY COURT
      * (3) TRUSTEE
           03  LTB-BANKRUPT-TABLE OCCURS 3.
               05  LTB-NAME             PIC X(24).
               05  LTB-ADR1             PIC X(24).
               05  LTB-ADR2             PIC X(24).
               05  LTB-CITY             PIC X(15).
               05  LTB-STATE            PIC XX.
               05  LTB-ZIP              PIC X(10).
               05  LTB-PHONE            PIC 9(10)    COMP-3.
               05  FILLER               PIC X(20).

      * REAFFIRMATION/DISCHARGE
      *>>XDD DATE=YYYYMMDD
           03  LTB-REAFF-DATE           PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LTB-REAFF-SYSDATE        PIC 9(8)     COMP-3.
           03  LTB-REAFF-BALANCE        PIC 9(6)V99  COMP-3.
           03  LTB-REAFF-PAYAMT         PIC 9(6)V99  COMP-3.       
           03  LTB-REAFF-NO-PAYS        PIC 9(3).
           03  LTB-INTRATE              PIC 9(3)V9(3).
      *>>XDD DATE=YYYYMMDD
           03  LTB-RESCIND-DATE         PIC 9(8)     COMP-3.
           03  LTB-PARTIAL-FULL         PIC X.
      *>>XDD DATE=YYYYMMDD
           03  LTB-DISCHARGE-DATE       PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LTB-DISCHARGE-SYSDATE    PIC 9(8)     COMP-3.

           03  FILLER                   PIC X(226).
