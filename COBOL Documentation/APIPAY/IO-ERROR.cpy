      *-----------------------------------------------------------------
      *================================================================*
      * END COPYBOOK: LIBLP\LPOPIN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\FERRORS.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/FERRORS
      *    ****************************
      *    *    FILE ERRORS ROUTINES
      *    ****************************
       FILE-IO-ERROR SECTION.
       IO-ERROR.
           PERFORM FILE-ERRORS.
           PERFORM SEND-MESS.
      *================================================================*
      * END COPYBOOK: LIBGB\FERRORS.CPY                                *
      *================================================================*
           MOVE "E" TO ERRCD.

       END-PROGRAM.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.

           MOVE RETURN-STATUS TO RETURN-CODE.

           EXIT PROGRAM RETURNING RETURN-CODE.
