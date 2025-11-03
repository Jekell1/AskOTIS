      * COPYMEMBER: LIBGL/GLFDFS
      ************************************************
      *    FSFILE: FINANCIAL STATEMENT FILE
      *   
      * REV:
      *  JTG 010694 EXPANDED FS-BRANCH TO 4 DIGITS
      ************************************************
       FD  OFS-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/GL01FS.CPY" REPLACING LEADING "FS" BY "OFS".
