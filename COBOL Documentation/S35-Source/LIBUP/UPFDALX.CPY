      * COPYMEMBER: LIBUP/UPFDALX
      ******************************************************************
      *    ASCII LOAN PAYMENT TRAILER FILE
      ******************************************************************
      $XFD  FILE=ALXFILE
       FD  ALX-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALX.CPY".

       FD  ALX-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALX-DONE-REC   PIC X.

