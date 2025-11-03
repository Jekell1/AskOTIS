      * COPYMEMBER: LIBLP/LPPDUEW
      *--------------------------------------------------------
      *    PAST DUE DAYS WORKERS
      * REV:
      *  JTG 051089 CHANGED FIELDS TO WORKERS
      *  JTG 040495 ADDED PDUE-1STPYDA, RE: PROBLEMS WITH FEB
      *  BAH 170313 FIX REDEFINES OF PDUE-1STPYDATE
      *--------------------------------------------------------
       01  PDUE-WORKERS.
      * CCYYMMDD
           03  PDUE-1STPYDATE     PIC 9(8).
           03  FILLER REDEFINES PDUE-1STPYDATE.
               05  FILLER         PIC 99.
               05  FILLER         PIC 99.
               05  FILLER         PIC 99.
               05  PDUE-1STPYDA   PIC 99.
           03  PDUE-TODAY-DATE    PIC 9(8)    COMP.
           03  PDUE-DAYS          PIC S9(5)   COMP.
