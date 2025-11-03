      * COPYMEMBER: LIBNP/NP01IM
      **************************************************
      *          ( INVESTOR MEMO INDEX # 1 )
      *        ( KEY: INVESTOR SSNO/DATE/SEQ  )
      * REV:
      **************************************************
       01  IM-REC.
           03  IM1-KEY.
               05  IM-SSNO             PIC 9(9).
               05  IM-DATE             PIC 9(8).
               05  FILLER REDEFINES IM-DATE.
                   07  IM-CC             PIC 99.
                   07  IM-YY             PIC 99.
                   07  IM-MM             PIC 99.
                   07  IM-DD             PIC 99.
               05  IM-SEQ              PIC 9(2).
           03  IM-MEMO                 PIC X(60).
           03  IM-MEMO-TYPE            PIC X.
           03  FILLER                  PIC X(10).
