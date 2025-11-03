      * COPYMEMBER: LIBLP/LPFDEA
      ************************************************************
      *  119       ( EXTRACTION/ACTION FILE )
      *
      *  REV:
      *   062895 BAH ADDED DLNO1 AND DLNO2           # E021
      *                     CONTRACTUAL-DAY1 AND CONTRACTUAL-DAY2
      ************************************************************
       FD  OEA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01EA.CPY" REPLACING LEADING "EA" BY "OEA".
