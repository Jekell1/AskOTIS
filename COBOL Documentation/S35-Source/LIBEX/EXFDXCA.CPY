      * COPYMEMBER: LIBEX/EXFDXCA
      ********************************************************************
      *   CASH DRAWER DATA WAREHOUSE EXTRACTION
      ********************************************************************
      $XFD  FILE=XCAFILE
       FD  XCA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBEX/EX01XCA.CPY".
