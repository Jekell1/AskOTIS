      * COPYMEMBER: LIBCL/FINDLNW
      ****************************************************
      *    FINDLNW
      *    WORKERS FOR FINDING PRIOR LOANS
      * REV:
      *  JTG 081497 ADDED FIND-GIFTNET WORLD A90
      *  BAH 170323 CHANGED FIND-TRANS-DATE FROM 9(6) TO 9(8).
      ****************************************************
       01  FINDLN-WORKERS.
      * IN
           03  FIND-TRANS-DATE          PIC 9(8).
           03  FIND-SSNO                PIC 9(9).
           03  FIND-GIFTNET             PIC X.
      * OUT
           03  FOUND-NUMBER             OCCURS 5 TIMES
                                        PIC 9(8).
