      ******************************************************************
      *    ASCII LOAN PAYMENT TRAILER EARNINGS FILE: 
      ******************************************************************
      $XFD  FILE=ALXEFILE
       FD  ALXE-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALXE.CPY".

       FD  ALXE-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALXE-DONE-REC   PIC X.
