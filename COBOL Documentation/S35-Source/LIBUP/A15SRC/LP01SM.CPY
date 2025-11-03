      * COPYMEMBER: LIBLP/LP01SM
      **************************************************************************
      *        STATE PARAMETER MEMO FILE          BYTES = 83
      *        KEY 1 - STATE, SPRCLASS, SUBCLASS, LAWCODE, DATE, SEQ
      * REV:
      **************************************************************************
       01  SM-REC.
           03  SM1-KEY.
               05  SM-SP-KEY.
                   07 SM-ORGST           PIC X(02).
                   07 SM-SPRCLASS        PIC 9(02).
                   07 SM-SUBCLASS        PIC 9(02).
                   07 SM-LAWCODE         PIC 9(02).
               05  SM-DATE               PIC 9(8).
               05  FILLER REDEFINES SM-DATE.
                   07  SM-CC             PIC 99. 
                   07  SM-YY             PIC 99. 
                   07  SM-MM             PIC 99. 
                   07  SM-DD             PIC 99. 
               05  SM-SEQ                PIC 99.
           03  SM-MEMO.
               05  SM-MEMO1              PIC X(30).
               05  SM-MEMO2              PIC X(30).
           03  SM-MEMO-TYPE              PIC X.
           03  FILLER                    PIC X(1).
