      * COPYMEMBER: LIBGL/GL01AGL
      **************************************************
      *          ASCII GENERAL LEDGER MASTER  
      *
      **************************************************
       78  AGL-SIZE                 VALUE 898.
       01  AGL-REC.
           03  AGL-FLAG             PIC X.
           03  FILLER               PIC X.
           03  AGL-BR-ACCOUNT       PIC 9(11).
           03  FILLER               PIC X.
           03  AGL-LTOUCH-DATE      PIC 9(8).
           03  FILLER               PIC X.
           03  AGL-DESCRIPTION      PIC X(36).
           03  FILLER               PIC X.
           03  AGL-CONSOL           PIC 9(4).
           03  FILLER               PIC X.
           03  AGL-OPENBAL          PIC +9(9).99.
           03  FILLER               PIC X.
           03  AGL-CYBAL-TABLE OCCURS 12.
               05  AGL-CYBAL        PIC +9(9).99.
               05  FILLER           PIC X.
           03  AGL-LYBAL-TABLE OCCURS 12.
               05  AGL-LYBAL        PIC +9(9).99.
               05  FILLER           PIC X.
           03  AGL-CYBUD-TABLE OCCURS 12.
               05  AGL-CYBUD        PIC +9(9).
               05  FILLER           PIC X.
           03  AGL-LYBUD-TABLE OCCURS 12.
               05  AGL-LYBUD        PIC +9(9).
               05  FILLER           PIC X.
           03  AGL-NYBUD-TABLE OCCURS 12.
               05  AGL-NYBUD        PIC +9(9).
               05  FILLER           PIC X.
           03  AGL-TYPE             PIC X.
           03  FILLER               PIC X.
           03  AGL-CLEAR            PIC X.
           03  FILLER               PIC X.
           03  AGL-NUMBER1          PIC 9(7).
           03  FILLER               PIC X.
           03  AGL-NUMBER2          PIC 9(7).
           03  FILLER               PIC X.
           03  AGL-FS-FLAG          PIC X.
           03  FILLER               PIC X.
           03  AGL-REPORTS-TABLE   OCCURS 5.
               05  AGL-REPORTS      PIC X.
               05  FILLER           PIC X.
           03  AGL-LYOPENBAL        PIC +9(9).99.
           03  FILLER               PIC X.
           03  AGL-DLALLOC-GL       PIC X.
           03  FILLER               PIC X.
           03  AGL-SPANISH-DESC     PIC X(36).
           03  FILLER               PIC X.
           03  AGL-CR               PIC X.
           03  AGL-LF               PIC X.
