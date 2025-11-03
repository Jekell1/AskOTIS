      * COPYMEMBER: LIBLP/BULKGRW
      *****************************************************
      *                   (LIBLP/BULKGRW)
      *    BULK TRANSFER WORKERS
      *****************************************************
       01  BRN-LIST               PIC X(15)   VALUE
           "FRGRNAME TOBRN.".
       01  TRANSFER-LIST          PIC X(31)   VALUE
           "FRGR TOBR TOMACH TODATA TODLNO.".
       01  FR-STATE               PIC XX.

      * BUFFER TO REDISPLAY SCREEN INPUTS AFTER CALLING PERUSE

       01  SCREEN-BUF.
           03  D-FR-GR            PIC X(4).
           03  D-FR-GR-NAME       PIC X(20).
           03  D-TO-BR            PIC Z(4).
           03  D-TO-BR-NAME       PIC X(20).
       01  SCREEN-LIST            PIC X(25)    VALUE
           "FRGR FRGRNAME TOBR TOBRN.".


