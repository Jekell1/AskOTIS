      * COPYMEMBER: LIBNP/NP01NM
      ***********************************************************
      *        NOTES MEMO FILE          BYTES = 89
      *        KEY 1 - ACCTNO, DATE, SEQ
      * REV:
      ***********************************************************
       01  NM-REC.
           03  NM1-KEY.
               05  NM-ACCTNO             PIC 9(6).
               05  NM-DATE               PIC 9(8).
               05  FILLER REDEFINES NM-DATE.
                   07  NM-CC             PIC 99.
                   07  NM-YY             PIC 99.
                   07  NM-MM             PIC 99.
                   07  NM-DD             PIC 99.
               05  NM-SEQ                PIC 99.
           03  NM-MEMO.
               05  NM-MEMO1              PIC X(30).
               05  NM-MEMO2              PIC X(30).
           03  NM-MEMO-TYPE              PIC X.
           03  FILLER                    PIC X(10).
