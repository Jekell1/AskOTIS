      * COPYMEMBER: LIBLP/LP01CDB
      ******************************************************************
      *
      *   DESCRIPTION:  CODE DESCRIPTION BRANCH FILE
      *
      *   CDB1-KEY = CDB-TYPE, CDB-BRANCH, CDB-CODE
      *
      *=================================================================
      *  TYPE BRNO CODE* DESCRIPTION
      *  ---- ---- ----- ----------------------------------------------
      *   CN  ####   3   COLLECTOR ID
      *   GC  ####   2   FEE
      *
      *   * = NUMBER FOR THE CODE REPRESENTS THE MAX LENGTH/BYTES
      *       OF THE CODE FOR GIVEN TYPE.
      *
      *=================================================================
      * REV:
      * KEC 2018-1120 ***NEW*** A32 & A34 DEVELOPMENT.
      *         THE CODE TYPES OF "CN" AND "GC" WERE MOVED FROM THE
      *         CODE DESCRIPTION FILE (CDFILE) TO THE 
      *         CODE DESCRIPTION BRANCH FILE (CDBFILE) SO TO MAKE THE 
      *         CDFILE MORE EASIER USE (STANDARD KEY FORMAT).
      *  CS 190513 ADD FUTURE FIELDS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  CDB-SIZE                            VALUE 200.
       01  CDB-REC.
           03  CDB1-KEY.
               05  CDB-TYPE                PIC X(02).
               05  CDB-BRANCH              PIC 9(04).
               05  CDB-CODE                PIC X(08).

      $XFD DATE=YYYYMMDD
           03  CDB-LTOUCH-DATE             PIC 9(08)      COMP-3.
           03  CDB-DESC                    PIC X(35).

      *-------------------------------------------------{ COLLECTOR ID }
           03  CDB-CN-GROUP-ID             PIC X(6).
           03  CDB-CN-PHONE                PIC 9(10)      COMP-3.
           03  CDB-CN-PHONE-EXT            PIC 9(5)       COMP-3.

      *----------------------------------------------------------{ FEE }
           03  CDB-GC-GLFEENO              PIC 9(7)       COMP-3.
           03  CDB-GC-GLFEEFINCHG          PIC X(1).
           03  CDB-GC-GLFEE-INT-CHARGABLE  PIC X(1).
           03  CDB-GC-FEE-GIFTNET          PIC X(1).
           03  CDB-GC-FEE-PRODNO           PIC 9(5)       COMP-3.
           03  CDB-GC-FEE-PRICE            PIC 9(4)V99    COMP-3.
           03  CDB-GC-NEW-PRODNO           PIC 9(5)       COMP-3.
           03  CDB-GC-FEE-MAXAMT           PIC 9(5)V99    COMP-3.
           03  CDB-GC-ALT-GLFEENO          PIC 9(7)       COMP-3.
           03  CDB-GC-COMM-GLFEENO         PIC 9(11)      COMP-3.
           03  CDB-GC-COMM-PERCENT         PIC 999V99     COMP-3.
           03  CDB-GC-FEE-EARNINGS-FG      PIC X.
           03  CDB-GC-BRANCH-COMM-AMOUNT   PIC 9(6)       COMP-3.
           03  FILLER                      PIC X(92).
