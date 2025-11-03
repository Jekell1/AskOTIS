      * COPYMEMBER: LIBLP/RESCINDW
      ******************************************************************
      *
      *         R E S C I S S I O N
      *
      ******************************************************************
      *   NAME: RESCINDW
      *   DESC: WORKERS TO CALCULATE THE RESCISSION
      *-----------------------------------------------------------------
      *   REV :
      *   KEC 021199 CHANGED SP-RESCIND-BUS-DAYS-VALID FROM A MAX OF
      *               5 DAYS TO A MAX OF 8 DAYS.       REGENCY PR# 00015
      *   BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      *               CHANGES NAMES TO BE "DATE" NOT "DAY"
      *
      ******************************************************************
       01  RESCISSION.
           03  RESCISSION-CALCS.
               05  FILLER               PIC X(1)           VALUE SPACES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    RESCIND-BUSINESS --- THE NUMBER OF BUSINESS DAYS;
      *    RESCIND-DATE(1)                --- FIRST FULL DAY IN RESCIND,
      *                                      [ RESCIND-1STDATE ];
      *               (RESCIND-BUSINESS) --- LAST  FULL DAY IN RESCIND,
      *                                      [ RESCIND-LASTDATE ];
      *               (RESCIND-BUS. + 1) --- FIRST  DAY OUT OF RESCIND,
      *                                      [ RESCIND-OUTDATE ].
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  RESCISSION-DATES.
               05  RESCIND-DATE       PIC 9(8) COMP  OCCURS 9.
               05  RESCIND-1STDATE    PIC 9(8) COMP  VALUE ZEROES.
               05  RESCIND-LASTDATE   PIC 9(8) COMP  VALUE ZEROES.
               05  RESCIND-OUTDATE    PIC 9(8) COMP  VALUE ZEROES.

               05  RESCIND-BUSINESS   PIC 9(1)       VALUE ZEROES.
               05  RESCIND-BUSINESS-X REDEFINES RESCIND-BUSINESS
                                      PIC X(1).
               05  RESCIND-DAY-SUB    PIC 9(1)       VALUE ZEROES.

               05  RESCIND-EDITDATE     PIC 9(8)     VALUE ZEROES.
               05  RESCIND-EDITDATE1    REDEFINES RESCIND-EDITDATE.
                   07  RESCIND-EDITCC   PIC 9(2).
                   07  RESCIND-EDITYY   PIC 9(2).
                   07  RESCIND-EDITMMDD PIC 9(4).
               05  RESCIND-EDITDATE2    REDEFINES RESCIND-EDITDATE.
                   07  RESCIND-EDIT-CC  PIC 9(2).
                   07  RESCIND-EDIT-YY  PIC 9(2).
                   07  RESCIND-EDIT-MM  PIC 9(2).
                   07  RESCIND-EDIT-DD  PIC 9(2).

               05  RESCIND-HOLIDAY-FLAG PIC X(1)        VALUE SPACES.
               05  RESCIND-HOLIDAY-SUB  PIC 9(2)        VALUE ZEROES.
               05  RESCIND-LOANDATE     PIC 9(8)        VALUE ZEROES.
               05  RESCIND-WORKDATE     PIC 9(8)        VALUE ZEROES.
