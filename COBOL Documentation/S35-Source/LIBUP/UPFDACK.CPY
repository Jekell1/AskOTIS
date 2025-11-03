      * COPYMEMBER: LIBUP/UPFDACK
      ********************************************************************
      *   CASH DRAWER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ACKFILE
       FD  ACK-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ACK.CPY".

       FD  ACK-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACK-DONE-REC   PIC X.

