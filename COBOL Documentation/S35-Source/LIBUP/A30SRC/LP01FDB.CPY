      * COPYMEMBER: LIBLP/LP01FDB
      ******************************************************************
      *
      * IDENTIFICATION:  FORM DATA BASE FILE
      *
      * DESCRIPTION   :  FORM DATA BASE FILE
      *                  CONTAINS DATA BASE FOR 2 FILES      
      *                  100 FIELDS PER RECORD                       
      *
      *=================================================================
      * VALID DATA BASE RECORDS (FDB-DBNO):
      *
      *   1 - (LN) LOAN FILE
      *   2 - (BW) BORROWER FILE
      *   3 - (BR) BRANCH FILE
      *   4 - (NO) NOTICE FILE
      *   5 - (LP) LOAN PAYMENT FILE
      *   6 - (DL) DEALER FILE
      *   7 - (CL) COLLATERAL TRAILER FILE (LT)
      *   8 - (LT) INSURANCE TRAILER FILE (LT)
      *   9 - (GB) GLOBAL FILE
      *  10 - (LS) REPO TRAILER FILE
      *  11 - (LA) LOAN APP FILE
      *  12 - (BA) BORROWER APP FILE *> BA NO LONGER EXIST KEC 2016.04
      *  13 - (MA) MINI APP FILE
      *  14 - (ML) MAIL LIST FILE
      *  15 - (LT) BANKRUPT TRAILER FILE
      *  16 - (LN) LOAN FILE ADDENDUM
      *  17 - (VN) VENDOR A/P FILE
      *  18 - (CD) POOL ID CODE FILE
      *  19 - (LI) LIEN/TITLE TRAILER FILE
      *  21 - (NT) NOTE MASTER FILE
      *  22 - (IV) INVESTOR FILE
      *  23 - (NC) NOTE CLASS FILE
      *  30 - (PA) MISC CHECK PAYEE FILE
      *  31 - (ES) ESCROW FILE
      *
      *=================================================================
      * REV:                                            
      * KEC 2016.0114 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ***NEW*** CREATED FROM REDEF WITHIN FD-FILE (LP01FD).
      *         FDB-SIZE = 5000.
      *
      ******************************************************************
       78  FDB-SIZE                            VALUE 5000.
       01  FDB-REC.
           03  FDB1-KEY.
               05  FDB-DBNO                    PIC 9(02).

      *=================================================================
      *>>XDD DATE=YYYYMMDD
           03  FDB-LTOUCH-DATE                 PIC 9(08)      COMP-3.

           03  FDB-DATABASEX.
               05  FDB-DATABASE                OCCURS 200.
                   07  FDB-DNAME               PIC X(08).
                   07  FDB-DOFFSET             PIC 9(04)      COMP-3.
                   07  FDB-DDIGITS             PIC 9(02)      COMP-3.
                   07  FDB-DTYPE               PIC 9(02)      COMP-3.
                   07  FDB-DDECIMAL            PIC 9(02)      COMP-3.

           03  FDB-FIELD-DESCS       REDEFINES FDB-DATABASEX.
               05  FDB-DATA-RECS               OCCURS 2
                                               PIC X(1700).

      *=================================================================
           03  FDB-CALLKEY                     OCCURS 2.
               05  FDB-CALLID                  PIC X(02).
               05  FDB-KEYLOC                  PIC 9(04)       COMP-3.

           03  FILLER                          PIC X(1583).
