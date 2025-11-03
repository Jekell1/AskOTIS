      * COPYMEMBER: LIBGB/CHKSEC *** PREVIOUSLY GETFM.CPY
      *
      ***********************************************
      *   S E C U R I T Y   C H E C K:
      *
      *   IF STAT = "3U" UPDATE FLAG WAS OFF
      *             "3O" UPDATE FLAG WAS ON
      *                  OWNERSHIP FLAG WAS OFF
      *             "3S" UPDATE FLAG WAS ON
      *                  OWNERSHIP FLAG WAS ON
      *                  USER PROFILE FLAG WAS OFF
      ***********************************************
       CHECK-SECURITY.
           MOVE FORM-PATHNAME TO FORM-PATH CHKSEC-FORM-PATH.
           MOVE "CO" TO FORM-OBJ.
      *    MOVE "CL/CHKSEC" TO FORM-NAM.
           CALL "CHKSEC" USING CHKSEC-FIELDS
             ON OVERFLOW
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO EXIT-PROG
           END-CALL.
           CANCEL "CHKSEC".
           MOVE CHKSEC-FORM-PATH TO FORM-PATH.
           MOVE CHKSEC-STAT      TO STAT.
           IF STAT NOT = "00"
              MOVE STAT TO FORM-OBJ
              GO TO EXIT-PROG.
