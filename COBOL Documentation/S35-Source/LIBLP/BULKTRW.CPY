      * COPYMEMBER: LIBLP/BULKTRW
      *******************************************************************
      *                   (LIBLP/BULKTRW)
      *    BULK TRANSFER WORKERS
      *
      *    5/20/03 - NOTE, I DON'T KNOW WHY FR-DATA-PATH & TO-DATA-PATH
      *    ARE 50 LONG...I IN ALL 3 PROGRAMS & THEY JUST HOLD THE VALUE
      *    OF THE X(6) FIELD BR-DATA-PATH...THIS CONFUSED ME BUT I DIDN'T
      *    WANT TO RISK CHANGING ALL THE BULK FORMS & PROGRAMS.   BLV
      *  REV:
      *  BLM 130507 ADD  BLKONE-LIST, BLKONE-BUF, TRANSFER-ACCT,
      *             WORLD PR# 865
      *******************************************************************
       01  BRN-LIST               PIC X(12)   VALUE
           "FRBRN TOBRN.".
       01  TRANSFER-LIST          PIC X(47)   VALUE
           "FRBR FRMACH FRDATA TOBR TOMACH TODATA TODLNO.".

       01  FR-STATE               PIC XX.

      * BUFFER TO REDISPLAY SCREEN INPUTS AFTER CALLING PERUSE

       01  SCREEN-BUF.
           03  D-FR-BR            PIC Z(4).
           03  D-FR-BR-NAME       PIC X(20).
           03  D-TO-BR            PIC Z(4).
           03  D-TO-BR-NAME       PIC X(20).
       01  SCREEN-LIST            PIC X(22)    VALUE
           "FRBR FRBRN TOBR TOBRN.".


