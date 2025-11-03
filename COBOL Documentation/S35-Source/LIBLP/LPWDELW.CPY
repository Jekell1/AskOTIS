      * COPYMEMBER: LIBLP/LPWDELW
      *---------------------------------------------------
      *                LIBLP/LPWDELW
      *    WAGE ASSIGNMENT DELINQ/PASTDUE WORKERS
      *                (ARREARAGE)
      *
      * REV:
      *    CS 20220110 CHANGED WDEL-MAKERCD FROM PIC S999 TO PIC X TO MATCH
      *                WHAT ??-MAKERCDE IS EVERYWHERE ELSE.  
      *          
      *---------------------------------------------------
       01  WDEL-WORKERS.
           03  WDEL-1STPYDATE     PIC 9(8)     COMP.
           03  WDEL-PAYDATE       PIC 9(8)     COMP.
           03  WDEL-MAKERCD       PIC X.
           03  WDEL-DELINQUENT    PIC S9(7)V99 COMP.
           03  WDEL-ARREARAGE     PIC S9(7)V99 COMP.

