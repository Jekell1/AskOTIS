      * COPYMEMBER: LIBGL/GL01FS
      ********************************************************
      *    FSFILE: FINANCIAL STATEMENT FILE
      *                  
      * REV:
      *  JTG 010694 EXPANDED FS-BRANCH TO 4 DIGITS
      *   CS 03011  FS-REC AREA TO BE MOVED TO NEW WORK FILE
      *  BLC 030217 ADD LEVEL 78 FOR FS-SIZE
      ********************************************************
       78  FS-SIZE                  VALUE 122.
       01  FS-REC.
           03  FS1-KEY.
               05  FS-REPORT        PIC 99.
               05  FS-LINENO        PIC 9(4).
           03  FS-REC-AREA.

           05  FS-TYPE              PIC X.
           05  FS-LEVEL             PIC 9.
           05  FS-CONTROL           PIC 9.
           05  FS-GLNUM.
               07  FS-BRANCH        PIC 9(4).
               07  FS-ACCTNO        PIC 9(7).
           05  FS-ACCOUNT           REDEFINES FS-GLNUM
                                    PIC 9(11).
           05  FS-DESCR             PIC X(36).
           05  FS-CMONTH            PIC S9(9)V99 COMP-3.
           05  FS-LYMONTH           PIC S9(9)V99 COMP-3.
           05  FS-CYTD              PIC S9(9)V99 COMP-3.
           05  FS-LYTD              PIC S9(9)V99 COMP-3.
           05  FS-CMBUD             PIC S9(9)    COMP-3.
           05  FS-YTDBUD            PIC S9(9)    COMP-3.
           05  FS-ANBUD             PIC S9(9)    COMP-3.
           05  FS-REFNO             PIC 9(4).
           05  FS-PERIOD            PIC 99.
           05  FS-PRNCD             PIC X.
           05  FS-SIGN              PIC X.
           05  FS-DRCR              PIC X.
           05  FS-BR                PIC 9(4).
           05  FS-BR2               PIC 9(4).
           05  FILLER               PIC X(10).
