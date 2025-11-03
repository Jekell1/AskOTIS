      * COPYMEMBER: LIBLP/LONPGW

      *----------------------------------------------------------------
      *              MISC SCREEN WORKERS FOR LP/LONPG*.C
      *
      * REV:
      *  JTG 030698 ADDED D-OVR-FG FIELD 070
      *  MJD 000209 ADDED D-2TERM TO DISPLAY TERM WITHOUT LEADING ZEROS
      *  BAH 021016 FEE-LIST FIELDS NAMES WERE WRONG !! FEE CODES WERE NOT
      *             DISPLAYING ON A CANCEL
      *  BAH 190215 CHANGED D-COLID01,02,03 AND 04 TO BE PIC X(20).
      *----------------------------------------------------------------
      * LONPGW CONTAINS  DISPLAY-ORIG-BUF
      *                  DISPLAY-ORIG-LIST
      *                  1STPY-BUF
      *                  1STPY-LIST
      *                  DISPLAY-RESET-LIST
      *                  DISPLAY-BUF1
      *                  DEF-BUF
      *                  DEF-LIST
      *                  DF2-BUF
      *                  DF2-LIST
      *                  LEGEND
      *                  LONPC2-BUF
      *                  LONPC2-LIST
      *                  MESSAGE-BUF
      *                  MESSAGE-LIST
      *                  MESS-LIST
      *                  MAX
      *                  SPEC-TABLE
      *                  SPEC-TABLE2 (SPEC, CHFG)
      *----------------------------------------------------------------

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-ORIG-BUF.
           03  D-LOAN-NUM      PIC X(16).
           03  D-BORR-INFO     OCCURS 2.
               05  D-SSNO      PIC X(11).
               05  D-BORR      PIC X(33).
               05  D-MAKERCD   PIC X(1).
           03  D-ORGST         PIC XX.
           03  D-ORGBR         PIC 9999      BLANK ZERO.
           03  D-SUBCLASS      PIC Z9.
           03  D-SUBDESC       PIC X(15).
           03  D-LOANDATE      PIC X(8).
           03  D-MDTE          PIC 99/99/99  BLANK ZERO.
           03  D-OTRM          PIC ZZ9       BLANK ZERO.
           03  D-UTRM          PIC ZZ9       BLANK ZERO.
           03  1STPY-BUF.
               05  D-1STPYDATE PIC 99/99/99  BLANK ZERO.
               05  D-ORGTERM   PIC ZZ9       BLANK ZERO.
               05  D-ARROW1    PIC XXXX.
           03  D-COLCD         PIC X(2).
           03  D-COLDESC       PIC X(15).
           03  D-INSCODE       PIC X(3).
           03  D-DLNO          PIC ZZ99.99   BLANK ZERO.
           03  D-DLDESC        PIC X(15).
           03  D-COLLATERAL-01 PIC X(80).
           03  D-COLLATERAL-X1 REDEFINES D-COLLATERAL-01.
               05  D-COLID01   PIC X(20).
               05  D-COLID02   PIC X(20).
               05  D-COLID03   PIC X(20).
               05  D-COLID04   PIC X(20).
           03  D-OVR           PIC ZZ9.999.
           03  D-OVR-FG        PIC X.
           03  D-EFF           PIC ZZ9.999.
           03  D-SMP           PIC ZZ9.999.
           03  D-APR           PIC ZZ9.999.
           03  D-CL-AH-PP-O1   OCCURS 4.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1OUR       PIC X.
               05  D-1TERM      PIC ZZ9       BLANK ZERO.
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-O2-O3      OCCURS 2.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2OUR       PIC X.
               05  D-2EXP       PIC 9(6)      BLANK ZERO.
               05  D-2TERM REDEFINES D-2EXP
                                PIC Z(6)      BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.

       01  DISPLAY-ORIG-LIST.
           03  FILLER          PIC X(48)  VALUE
           "LOANKEY SS01 BORR01 MAKER01 SS02 BORR02 MAKER02 ".
           03  FILLER          PIC X(56)  VALUE
           "ORGST ORGBR SUBCL SUBDESC LNDATE MDTE OTRM UTRM PMTDATE ".
           03  FILLER          PIC X(26)  VALUE
           "TERM ARROW1 COLCD COLDESC ".
           03  FILLER          PIC X(52)  VALUE
           "INSCODE DLNO DLDESC COLID01 COLID02 COLID03 COLID04 ".
      *    03  FILLER          PIC X(32)  VALUE
      *    "COLID05 COLID06 COLID07 COLID08 ".
           03  FILLER          PIC X(56)  VALUE
           "070 071 EFF SMP APR 080 081 082 083 084 090 091 092 093 ".
           03  FILLER          PIC X(44)  VALUE
           "094 100 101 102 103 104 110 111 112 113 114 ".
           03  FILLER          PIC X(40)  VALUE
           "120 121 122 123 124 130 131 132 133 134.".

       01  DISPLAY-RESET-LIST.
           03  FILLER          PIC X(43)  VALUE
           "REGPAY LASTPAY 1STPAY TOTREP AMTFIN INTCHG ".
           03  FILLER          PIC X(40)  VALUE
           "134 133 132 131 130 124 123 122 121 120 ".
           03  FILLER          PIC X(44)  VALUE
           "114 113 112 111 110 104 103 102 101 100 094 ".
           03  FILLER          PIC X(44)  VALUE
           "093 092 091 090 084 083 082 081 080 071 070 ".
           03  FILLER          PIC X(24)  VALUE
           "063 062 061 060 050 040".
           03  FILLER          PIC X(37)  VALUE
           "030 022 021 020 INSCODE TERM PMTDATE.".

       01  1STPY-LIST.
           03  FILLER          PIC X(20) VALUE
           "PMTDATE TERM ARROW1.".

      *01  FEETBL-CLEAR         PIC X(72) VALUE SPACES.
      *01  FEETBL-LIST.
      *    03  FILLER          PIC X(36) VALUE
      *    "060 061 062 063 064.".

       01  DISPLAY-BUF1        PIC X(501) VALUE " ".

       01  DEF-BUF             REDEFINES DISPLAY-BUF1.
           03  DEF-ORGST       PIC XX.
           03  DEF-ORGBR       PIC 9(4).
           03  DEF-LOANDATE    PIC 99/99/99.

       01  DF2-BUF.
           03  DF2-PROCEEDS          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-PROCEEDS-FG       PIC X.
           03  DF2-OVRDRATE          PIC ZZ9.999   BLANK ZERO.
           03  DF2-OVRDRATE-FG       PIC X.
           03  DF2-SMPRATE           PIC ZZ9.999   BLANK ZERO.
           03  DF2-LNAMT             PIC Z(5)9.99  BLANK ZERO.
           03  DF2-INTCHG            PIC Z(5)9.99  BLANK ZERO.
           03  DF2-FINAMT            PIC Z(5)9.99  BLANK ZERO.
           03  DF2-TREPAY            PIC Z(5)9.99  BLANK ZERO.
           03  DF2-ANTICIPATED       PIC Z(5)9.99  BLANK ZERO.
           03  DF2-1STPYAMT          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-LASTPYAMT         PIC Z(5)9.99  BLANK ZERO.
           03  DF2-REGPYAMT          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-RETURN-CODE       PIC X.
           03  DF2-LOOP-COUNT        PIC Z(3).

       01  DF2-LIST.
           03  FILLER          PIC X(28)  VALUE
           "040 041 070 071 EFF LOANAMT ".
           03  FILLER          PIC X(49)  VALUE
           "INTCHG AMTFIN TOTREP ANTIC 1STPAY LASTPAY REGPAY ".
           03  FILLER          PIC X(8)  VALUE
           "3XX 4XX.".


       01  DEF-LIST            PIC X(16) VALUE
           "ORGST ORGBR 010.".

       01  LEGEND                PIC X(70).

      *01  LONPC2-LIST           PIC X(7) VALUE "PC2BUF.".
      *01  LONPC2-BUF            VALUE SPACES.
      *    03  PRLOAN-RESTRICTED PIC X.

       01  FEE-BUF.
           03  D-FEECD1              PIC XX.
           03  D-FEEAMT1             PIC Z(4).ZZ.
           03  D-FEECD2              PIC XX.
           03  D-FEEAMT2             PIC Z(4).ZZ.

       01 FEE-LIST                  PIC X(16) VALUE
           "060 061 062 063.".

       01  MESSAGE-BUF               PIC X(37) VALUE SPACES.
       01  MESSAGE-LIST              PIC X(10) VALUE "M1:R M2:R.".
       01  MESS-LIST                 PIC X(5)  VALUE "MSEL.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   COMP VALUE 13.
       01  SPEC-TABLE.
      *  1 ADDON DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      *  2 ADDON TYPE (RT/ET/NP/LP)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *  3 TYPE OF ADDON (FEE/AH/CL/PP/O1/O2/O3)
           03  FILLER      PIC X(8) VALUE "ENNS010 ".
      *  4 ADJUSTMENT
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      *  5  INSURANCE CODES
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *  6  PD-FEECODE(1), (2)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *  7 PD-OVRDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033N".
      *  8 PD-INSCOMP(1)-CL
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      *  9 PD-INSCOMP(2)-AH
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 10 PD-INSCOMP(3)-PP
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 11 PD-INSCOMP(4)-O1
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 12 PD-INSCOMP(5)-O2
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 13 PD-INSCOMP(6)-O3
           03  FILLER      PIC X(8) VALUE "ENNN020N".
       01 SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 13 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
