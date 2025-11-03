      * COPYMEMBER: LIBWI/TC2SCNW
      * TRANSACTION CODE REFNO'S
      ******************************************************************
      * REV:
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED TC2SCN WORKERS TO MATCH CD1-KEY FOR TC LAYOUT.
      *         COMMENTED OUT TC2SCN-DESC SINCE NOT BEING USED ANYWHERE.
      * BAH 2024.0425 MAKE LEVEL 01, CALL NOT WINDOW
      *
      ******************************************************************
       01  TC2SCN-BUF.
           03  TC2SCN-TYPE       PIC X(2).
           03  TC2SCN-CODE       PIC X(8).
           03  TC2SCN-REFNO      PIC X(5).
