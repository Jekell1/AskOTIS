      * COPYMEMBER: LIBLP/LP01PC
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01PC
      *
      *  PC1-KEY = PC-ID, PC-BRANCH, PC-SOC-SEC
      *  PC2-KEY = PC2-ID, PC2-BRANCH, PC2-BWNAME
      *
      * DESCRIPTION   :  LOAN SCRATCH PAD FILE (PC-FILE)
      *                  --- FOR F3 EXPANDED COLLATERAL
      *
      *  PC-LTC-RECORD MUST ALWAYS MATCH LTC-RECORD (1000) WAS (650)
      *  PC-LTA-RECORD MUST ALWAYS MATCH LTA-RECORD (500) WAS (322)
      *
      *  PC-ID = "A" FOR SAVED AUTO INSURANCE INFO
      *  PC-ID = "C" FOR SAVED AUTO 1, AUTO 2, HHGS 1, HHGS 2, HHGS 3
      *
      *=================================================================
      * REV:
      * MJD 1999.0127 ----- UPDATED IN FLEXGEN -----
      *
      * KEC 2006.0621 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ADDED PC-SIZE.
      * BAH 100128 ADDED PC-ID = "5" FOR UNITED ACCEPTANCE TO STORE ANOTHER
      *            LINE OF HHGS, PR# 3524
      * BAH 160114 MADE SURE PC-LT-RECORD WAS CORRECT SIZE TO HOLD
      *            WK-LT-RECORD AND/OR WK-LT-A-RECORD PD0003
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED COMP-3 FROM PC1-KEY.
      * BAH 2020.0610 REMOVED FILLER AT END ADDED PC-LTOUCH-DATE
      * BAH 2020.0617 ADDED PC-LTC-RECORD LAYOUT AND PC-LTA-RECORD LAYOUT
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  PC-SIZE                                   VALUE 1800.
       01  PC-REC.
           03  PC1-KEY.
               05  PC-ID               PIC X.
               05  PC-BRANCH           PIC 9(4).
               05  PC-SOC-SEC          PIC 9(10).
      * PC-LTC-RECORD HOLDS WK-LT-RECORD 
      * PC-LTC-RECORD = 1000
           03  PC-LTC-RECORD.
               05  PC-LTC1-KEY.
                   07  PC-LTC-BRNO              PIC 9(4).
                   07  PC-LTC-ACCTNO            PIC 9(8).
                   07  PC-LTC-SEQNO             PIC 999.
      $XFD DATE=YYYYMMDD
               05  PC-LTC-LTOUCH-DATE           PIC 9(8)     COMP-3.
               05  PC-LTC-COLLATERAL-INFO.
                   07  PC-LTC-VIN-KEYS.
                       09  PC-LTC-DESC-1        PIC X(15).
                       09  PC-LTC-MAKE-1        PIC X(15).
                       09  PC-LTC-MODEL-1       PIC X(15).
                       09  PC-LTC-YEAR-1        PIC X(4).
                       09  PC-LTC-STYLE-1       PIC X(8).
                       09  PC-LTC-CLASS-1       PIC X(8).
                       09  PC-LTC-VIN-1         PIC X(20).
                       09  PC-LTC-TITLE-NO-1    PIC X(20).
                       09  PC-LTC-MILEAGE-1     PIC S9(7)    COMP-3.
                       09  PC-LTC-VALUE-1       PIC S9(7)V99 COMP-3.
                       09  PC-LTC-SELLPR-1      PIC S9(7)V99 COMP-3.
                       09  PC-LTC-MISC-AMT-1    PIC S9(7)V99 COMP-3.
                       09  PC-LTC-SENT-FLAG-1   PIC X.
      $XFD DATE=YYYYMMDD
                       09  PC-LTC-VIN-CHG-DATE-1 PIC 9(8)     COMP-3.
                       09  FILLER                PIC X(30).
                       09  PC-LTC-DESC-2         PIC X(15).
                       09  PC-LTC-MAKE-2         PIC X(15).
                       09  PC-LTC-MODEL-2        PIC X(15).
                       09  PC-LTC-YEAR-2         PIC X(4).
                       09  PC-LTC-STYLE-2        PIC X(8).
                       09  PC-LTC-CLASS-2        PIC X(8).
                       09  PC-LTC-VIN-2          PIC X(20).
                       09  PC-LTC-TITLE-NO-2     PIC X(20).
                       09  PC-LTC-MILEAGE-2      PIC S9(7)    COMP-3.
                       09  PC-LTC-VALUE-2        PIC S9(7)V99 COMP-3.
                       09  PC-LTC-SELLPR-2       PIC S9(7)V99 COMP-3.
                       09  PC-LTC-MISC-AMT-2     PIC S9(7)V99 COMP-3.
                       09  PC-LTC-SENT-FLAG-2    PIC X.
      $XFD DATE=YYYYMMDD
                       09  PC-LTC-VIN-CHG-DATE-2 PIC 9(8)     COMP-3.
                       09  FILLER                PIC X(30).
                       09  PC-LTC-DESC-3         PIC X(15).
                       09  PC-LTC-MAKE-3         PIC X(15).
                       09  PC-LTC-MODEL-3        PIC X(15).
                       09  PC-LTC-YEAR-3         PIC X(4).
                       09  PC-LTC-STYLE-3        PIC X(8).
                       09  PC-LTC-CLASS-3        PIC X(8).
                       09  PC-LTC-VIN-3          PIC X(20).
                       09  PC-LTC-TITLE-NO-3     PIC X(20).
                       09  PC-LTC-MILEAGE-3      PIC S9(7)    COMP-3.
                       09  PC-LTC-VALUE-3        PIC S9(7)V99 COMP-3.
                       09  PC-LTC-SELLPR-3       PIC S9(7)V99 COMP-3.
                       09  PC-LTC-MISC-AMT-3     PIC S9(7)V99 COMP-3.
                       09  PC-LTC-SENT-FLAG-3    PIC X.
      $XFD DATE=YYYYMMDD
                       09  PC-LTC-VIN-CHG-DATE-3 PIC 9(8)     COMP-3.
                       09  FILLER                PIC X(30).

                   07  PC-LTC-AUTO-GROUP REDEFINES PC-LTC-VIN-KEYS.
                       09  PC-LTC-AUTO             OCCURS 3.
                           11  PC-LTC-DESC       PIC X(15).
                           11  PC-LTC-MAKE       PIC X(15).
                           11  PC-LTC-MODEL      PIC X(15).
                           11  PC-LTC-YEAR       PIC X(4).
                           11  PC-LTC-STYLE      PIC X(8).
                           11  PC-LTC-CLASS      PIC X(8).
                           11  PC-LTC-VIN        PIC X(20).
                           11  PC-LTC-TITLE-NO   PIC X(20).
                           11  PC-LTC-MILEAGE    PIC S9(7)    COMP-3.
                           11  PC-LTC-VALUE      PIC S9(7)V99 COMP-3.
                           11  PC-LTC-SELLPR     PIC S9(7)V99 COMP-3.
                           11  PC-LTC-MISC-AMT   PIC S9(7)V99 COMP-3.
                           11  PC-LTC-SENT-FLAG  PIC X.
      $XFD DATE=YYYYMMDD
                           11  PC-LTC-VIN-CHG-DATE PIC 9(8)   COMP-3.
                           11  FILLER              PIC X(30).

                   07  PC-LTC-HHGS-GROUP.
                       09  PC-LTC-HHGS           PIC X(80) OCCURS 3.
                   07  PC-LTC-HHGS-INFO REDEFINES PC-LTC-HHGS-GROUP.
                       09  PC-LTC-HHGS-TABLE         OCCURS 3.
                           11  PC-LTC-COLL-INFO1    PIC X(20).
                           11  PC-LTC-COLL-INFO2    PIC X(20).
                           11  PC-LTC-COLL-INFO3    PIC X(20).
                           11  PC-LTC-COLL-INFO4    PIC X(20).
               05  FILLER                           PIC X(260).
      * PC-LTA-RECORD HOLDS WK-LT-A-RECORD SIZE 500 WAS 322
           03  PC-LTA-RECORD.
               05  PC-LTA1-KEY.
                   07  PC-LTA-BRNO        PIC 9(4).
                   07  PC-LTA-ACCTNO      PIC 9(8).
                   07  PC-LTA-SEQNO       PIC 999.
      $XFD DATE=YYYYMMDD
               05  PC-LTA-LTOUCH-DATE     PIC 9(8) COMP-3.
      * PC-LTA-AUTO-RECORD SIZE 480 WAS 302
               05  PC-LTA-AUTO-RECORD.
                   07  PC-LTA-DLR-REQ     PIC X.
                   07  PC-LTA-INS-TYPE    PIC X.
                   07  PC-LTA-INS-CO      PIC X(25).
                   07  PC-LTA-POLICY-NO   PIC X(25).
                   07  PC-LTA-AGENCY      PIC X(25).
                   07  PC-LTA-AGENT       PIC X(25).
                   07  PC-LTA-ADR1        PIC X(25).
                   07  PC-LTA-ADR2        PIC X(25).
                   07  PC-LTA-CITY        PIC X(15).
                   07  PC-LTA-STATE       PIC XX.
                   07  PC-LTA-ZIP         PIC X(10).
                   07  PC-LTA-PHONE       PIC 9(10)    COMP-3.
      $XFD DATE=YYYYMMDD
                   07  PC-LTA-EFF-DATE     PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
                   07  PC-LTA-EXP-DATE     PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
                   07  PC-LTA-CAN-DATE     PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
                   07  PC-LTA-CONF-DATE    PIC 9(8)     COMP-3.
                   07  PC-LTA-LAST-FORM    PIC 9(4)     COMP-3.
                   07  PC-LTA-COV-TYPE     PIC X.
                   07  PC-LTA-BIL-COV      PIC S9(7)V99 COMP-3.
                   07  PC-LTA-PDL-COV      PIC S9(7)V99 COMP-3.
                   07  PC-LTA-PIP-COV      PIC S9(7)V99 COMP-3.
                   07  PC-LTA-UMB-COV      PIC S9(7)V99 COMP-3.
                   07  PC-LTA-COL-COV      PIC S9(7)V99 COMP-3.
                   07  PC-LTA-LOSS-PAYEE   PIC X(25).
                   07  PC-LTA-COM-COV      PIC S9(7)V99 COMP-3.
                   07  PC-LTA-TRACK-ONLY   PIC X.
                   07  PC-LTA-INSURANCE    PIC X.
                   07  PC-LTA-TRACK-MONTHS PIC X(12).
                   07  PC-LTA-INS-MONTHS   PIC X(12).
                   07  PC-LTA-CPP-MONTHS   PIC X(12).
                   07  FILLER              PIC X(178).

      $XFD DATE=YYYYMMDD
           03  PC-LOANDATE             PIC 9(8)    COMP-3.
           03  PC-ORGTERM              PIC 9(3).
           03  PC-REGPYAMT             PIC 9(6)V99 COMP-3.
           03  PC2-KEY.
               05  PC2-ID              PIC X.
               05  PC2-BRANCH          PIC 9(4).
               05  PC2-BWNAME          PIC X(33).

      $XFD DATE=YYYYMMDD
           03  PC-LTOUCH-DATE          PIC 9(8)     COMP-3.

           03  FILLER                  PIC X(229).
