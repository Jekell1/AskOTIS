      * COPYMEMBER: LIBSP/SPCPCL

      ******************************************************************
      *    SPCPCL    COLLECTOR POOL  FILE CLEAR ROUTINE
      *
      * REV:
      *  BAH 160406 NEW
      ******************************************************************
       CLEAR-CP-FILE SECTION.
           MOVE SPACES TO CP-REC.

           MOVE 0 TO CP-BRNO CP-TOTAL-ACCOUNTS CP-COLLECTOR-COUNT
                     CP-LAST-SUBSCRIPT CP-LTOUCH-DATE.
