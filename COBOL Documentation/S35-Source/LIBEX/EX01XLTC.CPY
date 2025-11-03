      * COPYMEMBER: LIBEX/EX01XLTC
      *************************************************************************
      * COLLATERAL TRAILER EXTRACTION FILE        
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *   CS  040907 ADD XLTC-REGION-NUMBER PR#76 REGACC
      *   CS 180509 PUT BACK XLTC-UNUSED, NEEDS TO MATCH A15
      **************************************************************************
       78  XLTC-SIZE                    VALUE 532.
       01  XLTC-REC.
           03  XLTC-BRACCTNO            PIC 9(10).
           03  XLTC-ACCTNO              PIC 9(08).
           03  XLTC-CODE                PIC X.
           03  XLTC-SEQNO               PIC 9(03).
           03  XLTC-TYPE                PIC 9(02).
           03  XLTC-DESCRIPTION1        PIC X(10).
           03  XLTC-MAKE1               PIC X(15).
           03  XLTC-MODEL1              PIC X(15).
           03  XLTC-YEAR1               PIC X(04).
           03  XLTC-STYLE1              PIC X(08).
           03  XLTC-CLASS1              PIC X(08).
           03  XLTC-ID1                 PIC X(20).
           03  XLTC-TITLE-NO1           PIC X(08).
           03  XLTC-MILEAGE1            PIC +9(07).
           03  XLTC-VALUE1              PIC +9(05).99.
           03  XLTC-SELLPR1             PIC +9(05).99.
           03  XLTC-MISC-AMT1           PIC +9(05).99.
           03  XLTC-DESCRIPTION2        PIC X(10).
           03  XLTC-MAKE2               PIC X(15).
           03  XLTC-MODEL2              PIC X(15).
           03  XLTC-YEAR2               PIC X(04).
           03  XLTC-STYLE2              PIC X(08).
           03  XLTC-CLASS2              PIC X(08).
           03  XLTC-ID2                 PIC X(20).
           03  XLTC-TITLE-NO2           PIC X(08).
           03  XLTC-MILEAGE2            PIC +9(07).
           03  XLTC-VALUE2              PIC +9(05).99.
           03  XLTC-SELLPR2             PIC +9(05).99.
           03  XLTC-MISC-AMT2           PIC +9(05).99.
           03  XLTC-GOODS1              PIC X(80).
           03  XLTC-GOODS2              PIC X(80).
           03  XLTC-REGION-NUMBER       PIC 99.
           03  XLTC-UNUSED              PIC X(98).
           03  XLTC-CR                  PIC X(01).
           03  XLTC-LF                  PIC X(01).
