      * COPYMEMBER: LIBEX/EX01XTRC
      **************************************************************************
      * CASH/CHECKS TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *  CS  040907 ADD XTRC-REGION-NUMBER   [PR#76 REGACC]
      *  BAH 2021/01/13 ADDED XTRC-EXTRACTED-FG, WORLD #1486
      *  BAH 2021/03/08 ADDED XTRC-DEBIT-REFCD, WORLD #1490
      **************************************************************************
       01  XTRC-REC.
           03  XTRC-BRNO                 PIC 9(04).
           03  XTRC-CKNO                 PIC 9(06).
           03  XTRC-TRANS-DATE           PIC 9(08).
           03  XTRC-CODE                 PIC X(01).
           03  XTRC-CLASS                PIC 9(02).
           03  XTRC-SEQ                  PIC 9(04).
           03  XTRC-TYPE                 PIC X(02).
           03  XTRC-DATE                 PIC 9(08).
           03  XTRC-AMT                  PIC +9(07).99.
           03  XTRC-PAYEE                PIC X(30).
           03  XTRC-REF                  PIC 9(06).
           03  XTRC-REASON               PIC X(30).
           03  XTRC-GLDIST-TAB.
               05  XTRC-GLNO1            PIC 9(11).
               05  XTRC-GLAMT1           PIC +9(7).99.
               05  XTRC-GLNO2            PIC 9(11).
               05  XTRC-GLAMT2           PIC +9(7).99.
               05  XTRC-GLNO3            PIC 9(11).
               05  XTRC-GLAMT3           PIC +9(7).99.
               05  XTRC-GLNO4            PIC 9(11).
               05  XTRC-GLAMT4           PIC +9(7).99.
               05  XTRC-GLNO5            PIC 9(11).
               05  XTRC-GLAMT5           PIC +9(7).99.
           03  XTRC-GLDIST REDEFINES XTRC-GLDIST-TAB OCCURS 5.
               05  XTRC-GLNO             PIC 9(11).
               05  XTRC-GLAMT            PIC +9(7).99.
           03  XTRC-VOID                 PIC X(01).
           03  XTRC-ACCTNO               PIC 9(08).
           03  XTRC-PAID                 PIC X(01).
           03  XTRC-PGM                  PIC X(02).
           03  XTRC-DLRVNO               PIC 9(01).
           03  XTRC-SPRCLASS             PIC 9(02).
           03  XTRC-DLNO                 PIC 9(04).99.
           03  XTRC-TILLNO               PIC X(01).
           03  XTRC-USERID               PIC X(10).
           03  XTRC-POSTDATE             PIC 9(08).
           03  XTRC-POSTTIME             PIC 9(06).
           03  XTRC-REGION-NUMBER        PIC 9(2).
           03  XTRC-EXTRACT-FG           PIC X.
           03  XTRC-DEBIT-REFCD          PIC X(5).
           03  FILLER                    PIC X(92).
           03  XTRC-CR                   PIC X(01).
           03  XTRC-LF                   PIC X(01).
