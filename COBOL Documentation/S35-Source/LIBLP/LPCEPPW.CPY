      * COPYMEMBER: LIBLP/LPCEPPW
      *---------------------------------------------------
      *    CONTRACTUAL ELPASED PAY PERIODS CALCULATION
      *
      *       NOTE: DON'T REMOVE INITIAL VALUE FROM
      *             CEPP-NOCAP-FG & CEPP-INCDEF-FG
      *             AND CEPP-INCALDEL-FG WITHOUT CHANGING
      *             LPCEPP.IN.C LOGIC.
      * REV:
      *  JTG 051089 CHANGED WORKER TO COMP
      *  JTG 040990 ADDED CEPP-INCALDEL-FG FOR PACESETTER
      *  JTG 071696 ADDED CEPP-UNITPER-REM (CAC, FL)
      *  BLM 170309 CHANGE CEPP1-DATE & CEPP2-DATE TO BE
      *             CCYYMMDD, PR # PD0006
      *---------------------------------------------------
       01  CEPP-DATE-WORKERS.
           03  CEPP1-DATE         PIC 9(8).
           03  FILLER            REDEFINES CEPP1-DATE.
               05  CEPP1-CC       PIC 99.
               05  CEPP1-YR       PIC 99.
               05  CEPP1-MO       PIC 99.
               05  CEPP1-DA       PIC 99.
           03  CEPP2-DATE         PIC 9(8).
           03  FILLER            REDEFINES CEPP2-DATE.
               05  CEPP2-CC       PIC 99.
               05  CEPP2-YR       PIC 99.
               05  CEPP2-MO       PIC 99.
               05  CEPP2-DA       PIC 99.
           03  CEPP-PERIODS       PIC S9(5)   COMP.
           03  CEPP-UNITPER-REM   PIC S9(3)   COMP.
           03  CEPP-MAKERCD       PIC X.
           03  CEPP-NOCAP-FG      PIC X       VALUE " ".
           03  CEPP-INCDEF-FG     PIC X       VALUE " ".
           03  CEPP-INCALDEL-FG   PIC X       VALUE " ".
