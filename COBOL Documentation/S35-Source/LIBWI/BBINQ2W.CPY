      * COPYMEMBER: LIBWI/BBINQ2W
      ******************************************************************
      *                     LIBWI/BBINQ2W
      *
      * DESCRIPTION: MORE INFO WORKER BETWEEN WI/BBINQ AND WI/BBINQ2
      *
      *    BBINQ2-ACTION:   "A" = EDIT OLD RECORD (ADMIN MODE)
      *                     "N" = NEW RECORD      (ADMIN MODE)
      *                     "O" = EDIT RECORD     (EDIT MODE)
      *                     "V" = VIEW RECORD     (VIEW MODE)
      ******************************************************************
       01  BBINQ2-BUF.
           03  BBINQ2-LOGIN           PIC X(10).
           03  BBINQ2-ACTION          PIC X(01).
           03  BBINQ2-BB-KEY.
               05  BBINQ2-CATAGORY    PIC X(04).
               05  BBINQ2-SUBCATAGORY PIC X(04).
               05  BBINQ2-USERID      PIC X(10).
               05  BBINQ2-SEQNO       PIC 9(06).
