      * COPYMEMBER: LIBLP/LPFDPD
      ********************************************************************
      *             LOAN SCRATCH PAD FILE              RSZ 2289 TOTAL
      * ID = Q (QUOTES) OR (CAN LOANS)              PD-RECORD = LN-REC
      *      R (RENEWALS)                              RSZ 1800
      *      L (SAVED LOANS)
      *      B (SAVED BORROWERS)
      ********************************************************************
       FD  OPD-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01PD.CPY"
                          REPLACING LEADING "PD" BY "OPD".
