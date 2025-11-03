      * COPYMEMBER: LIBWI/PWSCANW
      ******************************************************************
      *
      * IDENTIFICATION: LIBWI/PWSCANW.CPY
      *
      * DESCRIPTION   : LINK WORKERS FOR WI/PWSCAN
      *
      *=================================================================
      * REV:
      * KEC 2015.1201 FIXED TO NOT USE PWSCAN-DIR ANYMORE;
      *               INCREASED PWSCAN-SEQ TO BE 3  BYTES.
      * BAH 2024.0501 MADE 01 LEVEL, CALL NOT WINDOW
      *
      ******************************************************************
       01  PWSCAN-BUF.
           03  PWSCAN-PW1-KEY.
               05  PWSCANW-PGM       PIC X(10).
               05  PWSCANW-SEQ       PIC 9(03).
           03  PWSCAN-STATUS         PIC XX.
