       EXIT-CALIFORNIA-SPECIAL-OPTION.
           EXIT.

      *********************************************
      *   ROUTINE TO CALL SUBPROGRAM:
      *
      *   MOVE FIELDS TO PASS TO POFF-COMMON
      *   MOVE PROGRAM NAME TO POFF-FORM-PROG
      *********************************************
      *POFF-CALL-SUBPROG SECTION.
      *    MOVE POFF-FORM-PATH TO FORM-NAM.

      *POFF-CALL-SUBPROG-RETRY.
      *    MOVE POFF-PAYDATE TO POFF-COMMON-PAYDATE.
      *    CALL FORM-PROGX USING FORM-PATH POFF-COMMON LN-REC SP-REC
      *         ON OVERFLOW
      *         GO TO POFF-CALL-SUBPROG-RETRY.

      *    CANCEL FORM-PROGX.

       COPY "LIBLP/MIPPO.CPY".

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
