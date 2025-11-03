      ******************************************************************
      *    ASCII LOAN PAYMENT GENERAL LEDGER DISTRIBUTION FILE: 
      ******************************************************************
      $XFD  FILE=ALXGFILE
       FD  ALXG-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALXG.CPY".

       FD  ALXG-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALXG-DONE-REC   PIC X.
