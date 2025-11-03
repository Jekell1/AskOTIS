      * COPYMEMBER: LIBLP/LPFDLE
      ******************************************************************
      *   LOAN ENDORSER FILE        (LE)
      *       KEY - ACCTNO, SSNO, OSACCTNO
      *       LS-OSRATE -  00.0000 (PRINCIPAL ONLY)
      *                    ??.???? (EXACT RATE)
      *                    99.9999 (NORMAL LOAN RATE)
      *       LE-TYPE:
      *               'E' - LOAN ENDORSER RECORDS
      *               'R' - RESTRICTED RECORDS
      * REV:
      ******************************************************************
       FD  OLE-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LE.CPY" REPLACING LEADING "LE" BY "OLE".
