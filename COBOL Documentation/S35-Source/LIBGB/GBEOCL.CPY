      * COPYMEMBER: LIBGB/GBEOCL

      ******************************************************************
      *    GBEOCL    EOFILE CLEAR ROUTINE
      *
      *=================================================================
      * REV:
      * JKC 2019-0412 CLEAR FUTURE FIELDS
      * JKC 2022-0803 ADDED MOVING ZEROES TO EO-LEVEL DUE TO GETTING
      *          SQL ERROR "NUMERIC VALUE OUT OF RANGE".
      ******************************************************************
       CLEAR-EO-FILE SECTION.
           MOVE SPACES TO EO-REC.

           MOVE 0      TO EO-STEP EO-COPIES EO-START-DATE EO-START-TIME
                          EO-COMPLETE-DATE EO-COMPLETE-TIME
                          EO-ERROR-STEP EO-LEVEL.
