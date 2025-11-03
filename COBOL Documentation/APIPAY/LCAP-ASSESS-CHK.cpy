      ****************************************************
      * DOES LCAP-LCPDTH-DATE INDICATE A LATE CHARGE?
      ****************************************************
       LCAP-ASSESS-CHK SECTION.
           MOVE LCAP-LCPDTH-DATE TO LCAS-LCPDTH-DATE.
           MOVE LCAP-1STPYDATE TO LCAS-1STPYDATE.
           MOVE LCAP-PAYDATE TO LCAS-PAYDATE.
           PERFORM LATE-ASSESS-ROUTINE.
      * ADDED SET OF LCHG-DAYS-AFTER-DUE PER MULLEN #804 LCTYPE = 'Q'
           MOVE LCAS-ELAPSED-DAYS TO LCHG-DAYS-AFTER-DUE.

      ****************************************************
      * TEST ELAPSED AGAINST LCAP-LCPDTH-DATE:
      ****************************************************
