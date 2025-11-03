      * COPYMEMBER: LIBLP/LPAPRSW
      **************************************************************************
      *                     (LPAPRSW)
      *    CALCULATION WORKERS FOR
      *    EQUIVELANT SIMPLE AND EFFECTIVE INTEREST RATE
      *
      *REV:
      * JTG 062288 ADDED INTCHGABLE AND REGPYAMT
      * JTG 051089 CHANGED WORKERS TO COMP
      * JTG 080889 ADDED APRS-CTBL
      * JTG 043093 REMOVED APRS-DISFRMLA
      * JTG 060493 ADDED APRS-ODDPYAMT-ADJ, APRS-LASTPYAMT-ADJ
      * JTG 100998 REMOVED APRS-ODDPYAMT-ADJ AND CHANGED  APRS-LASTPYAMT-ADJ
      *            TO APRS-PYAMT-ADJ
      * JTG 030430 CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *            OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *             ADDED APRS-SPREAD APRS-REM                   CITIZENS #2006
      **************************************************************************
      * INPUT AREA:

      * OUTPUT AREA:
       01  APRS-SMPRATE           PIC S999V999    COMP.
       01  APRS-EFFRATE           PIC S999V999    COMP.
       01  APRS-CTBL              PIC 9.
      * THESE FIELDS USED ON ACTUARIAL REBATE:
       01  APRS-INTCHGABLE        PIC S9(7)V99    COMP.
       01  APRS-REGPYAMT          PIC S9(6)V9(9)  COMP.

      * WORK AREA:
       01  APRS-PYAMT-ADJ         PIC S9(6)V99    COMP.
       01  APRS-SPREAD            PIC 9(6)V99     COMP.
       01  APRS-REM               PIC 9(6)V99     COMP.

