      * COPYMEMBER: LIBEX/EX01XTRD
      **************************************************************************
      * DEPOSIT TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *      040907 ADD XTRD-REGION-NUMBER
      **************************************************************************
       01  XTRD-REC.
           03  XTRD-BRNO                 PIC 9(04).
           03  XTRD-TRANS-DATE           PIC 9(08).
           03  XTRD-CODE                 PIC X(01).
           03  XTRD-DEPOSIT              PIC +9(07).99.
           03  XTRD-BANK                 PIC 9(11).
           03  XTRD-CASH                 PIC 9(11).
           03  XTRD-GLFG                 PIC X(01).
           03  XTRD-GLOVERSHORT          PIC 9(11).
           03  XTRD-OVERSHORT            PIC +9(05).99.
           03  XTRD-USERID               PIC X(10).
           03  XTRD-POSTDATE             PIC 9(08).
           03  XTRD-POSTTIME             PIC 9(06).
           03  XTRD-REGION-NUMBER        PIC 9(2).
           03  FILLER                    PIC X(98).
           03  XTRD-CR                   PIC X(01).
           03  XTRD-LF                   PIC X(01).
