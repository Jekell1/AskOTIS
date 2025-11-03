      * COPYMEMBER: LIBLP/LPEACL
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPEACL
      *
      * DESCRIPTION   :  EXTRACTION/ACTION FILE CLEAR ROUTINE
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0427 {PR#01138} REGIONAL ACCEPTANCE; KEVIN MULLEN
      *                ADDED EA-PL-BEG-DATE AND EA-PL-END-DATE
      * BLV 010507 CLEAR EA-PROM-BREAK1 EA-PROM-BREAK2
      * BLV 021029 CLEAR EA-DAY-OF-MONTH-DUE1, EA-DAY-OF-MONTH-DUE2,
      *            REGACC PR# 0009
      * BLV 021029 THIS ROUTINE WAS NOT USED ANYWHERE.  FIXED IT,
      *            ADDED ALL NUMERIC FIELDS & PUT IT IN EAMAIN,EQMAIN
      *  CS 190425 CLEAR FUTURE NUMERIC FIELDS
      *  20210426 AMM REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-EA-FILE SECTION.
           MOVE SPACES TO EA-REC.
           MOVE 0 TO EA-PASTDUE-DAY1     EA-PASTDUE-DAY2
                     EA-PASTDUE-AMT1     EA-PASTDUE-AMT2
                     EA-ACCT-BAL1        EA-ACCT-BAL2
                     EA-LSTFM-DATE1      EA-LSTFM-DATE2
                     EA-LSTFM-NO1        EA-LSTFM-NO2
                     EA-PASTPROM-DAY1    EA-PASTPROM-DAY2
                     EA-RECENCY-DAY1     EA-RECENCY-DAY2
                     EA-DLNO1            EA-DLNO2
                     EA-CONTRACTUAL-DAY1 EA-CONTRACTUAL-DAY2
                     EA-SETFM-NO
                     EA-INC-PROMISED
                     EA-SETFM(1)
                     EA-SETFM(2)
                     EA-SETFM(3)
                     EA-PROM-BREAK1      EA-PROM-BREAK2
                     EA-PL-BEG-DATE      EA-PL-END-DATE
                     EA-LNDATE-DAYS1     EA-LNDATE-DAYS2
                     EA-MATURE-DAYS1     EA-MATURE-DAYS2
                     EA-DAY-OF-MO-DUE1   EA-DAY-OF-MO-DUE2.

