      * COPYMEMBER: LIBWI/DEBREFW
      **************************************************************************
      * REV:
      *  BAH 2021.03.05 NEW TO SCAN DEBIT REFCDS ONLY #1490
      *  BAH 2024.0416 REMOVED DEBREF-FILE-TYPE
      *  BAH 2024.0424 MADE LEVEL 01, CALL NOT WINDOW
      **************************************************************************
       01  DEBREF-BUF.
           07  DEBREF-CD1-KEY              PIC X(10).
           07  FILLER            REDEFINES DEBREF-CD1-KEY.
               09  DEBREF-TYPE             PIC X(2).
               09  DEBREF-CODE             PIC X(8).
