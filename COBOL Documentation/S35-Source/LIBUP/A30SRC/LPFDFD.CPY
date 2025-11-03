      * COPYMEMBER: LIBLP/LPFDFD
      **********************************************************
      *     FORM DESCRIPTOR FILE
      *     KEY 1 - FMNO
      *     KEY 2 - DCODE, DBNO
      *     FD-REC CONTAINS 1 FORM DESCRIPTOR;
      *     UP TO 999 RECORDS (FMNO 1-999)
      *     FD-REC1 CONTAINS DATA BASE FOR 2 FILES
      *     100 FIELDS PER RECORD
      *     FD-DCODE IS ALWAYS 99, FD-DBNO 1-99
      *
      *     1STFILE CODES: SEE LIST IN FDMAIN.C
      * REV:
      **********************************************************
       FD  OFD-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01FD.CPY" REPLACING LEADING "FD" BY "OFD".
