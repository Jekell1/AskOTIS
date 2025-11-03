      *********************************
      *   EARLY PROVISION ROUTINES
      *********************************
       REBATE-EP1 SECTION.
           IF LN-EXTCHG = 0
              MOVE "B" TO REB-HOLD-RBDTE
           ELSE
              IF REB-HOLD-RBDTE = "D"
                 MOVE "D" TO REB-HOLD-RBDTE
              ELSE
                 MOVE "C" TO REB-HOLD-RBDTE.
      * ^^^^^^^^^^
      * THE ABOVE LOGIS IS CONFUSING TO ME, PRIOR TO ADDING "D" THE ELSE
      * JUST SET THE VALUE TO "C" TO USE A LATER DATE DUE TO EXTENSION.
      * FROM THIS POINT FORWARD 'C' AND 'D' ARE THE SAME SO THE ONLY ONE
      * AFFECTED IS 'A' WHICH WILL CONTINUE TO BE FORCED TO A 'C AS IT
      * DID BEFORE.

