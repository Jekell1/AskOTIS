      * COPYMEMBER: LIBWI/TCSCANW
      ******************************************************************
      * REV:
      * KEC 2016.0328 CHANGED TCSCAN WORKERS TO MATCH CD1-KEY FOR TC LAYOUT.
      *               COMMENTED OUT TCSCAN-DESC & TCSCAN-REFNO SINCE NOT
      *               BEING USED ANYWHERE.
      * BAH 2024.0425 MADE 01 LEVEL AND CALL, NOT WINDOW
      ******************************************************************
       01  TCSCAN-BUF.
           03  TCSCAN-TYPE       PIC X(2).
           03  TCSCAN-CODE       PIC X(2).
