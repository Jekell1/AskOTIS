      * COPYMEMBER: LIBLP/CLASTRW
      *****************************************************
      *                   (LIBLP/CLASTRW)
      *    CLASS TRANSFER WORKERS
      *****************************************************
       01  LCN-LIST               PIC X(06)   VALUE
           "TOLCN.".
       01  TRANSFER-LIST          PIC X(23)   VALUE
           "FRLC FRLC2 TOLC TODLNO.".

      *-----------------------------------------------------------------
      * BUFFER TO REDISPLAY SCREEN INPUTS AFTER CALLING PERUSE
      *-----------------------------------------------------------------
       01  SCREEN-BUF.
           03  D-FR-LC            PIC Z(2).
           03  D-FR2-LC           PIC Z(2).
           03  D-TO-LC            PIC Z(2).
           03  D-TO-LC-DESC       PIC X(20).
       01  SCREEN-LIST            PIC X(22)    VALUE
           "FRLC FRLC2 TOLC TOLCN.".
