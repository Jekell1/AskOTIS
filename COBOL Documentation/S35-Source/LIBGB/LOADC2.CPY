
      ****************************************************************
      * 7/26/12  CONVERSION TO A30 COMPLETED
      *          DIDN'T BRING OVER ALT-PATH STUFF FROM A15
      * 11/20/12 ADDED EXIT-PATHNAME TO THE CALL FORM-PROGX
      * BLM 130213 CHANGED LOGUID TO NOT CALL C PROGRAM
      *
      *         STANDARD PROGRAM LOADER
      *
      *   NAME: LOADCO
      *   DESC: THIS ROUTINE ALLOWS FOR A COBOL SUB-PROGRAM
      *         TO BE LOADED AND EXECUTED, VIA CALL.
      *         1. THE ROUTINE TESTS FOR MISSING PROGRAM.
      *            THE CALLING PROGRAM MUST PROVIDE FOR
      *            A MISSING PROGRAM RECOVERY POINT,
      *            LABELED "LOAD-RESTART".
      *         2. THE ROUTINE ALLOWS FOR PROGRAM RELOAD.
      *            THE CALLING PROGRAM MUST PROVIDE FOR
      *            A RELOAD POINT, LABELED "LOAD-POST".
      *   IN  : PLACE PATH NAME OF PROGRAM TO BE EXECUTED
      *         INTO FORM-PATH.
      *         PROVIDE RETURN POINTS IN THE PROGRAM:
      *         1. LOAD-RESTART (MISSING PROGRAM RECOVERY)
      *         2. LOAD-POST    (RETRY PROGRAM LOAD)
      *   OUT : PROGRAM IS CANCELED FROM MEMORY.
      *         FORM-NAM = " "  (EXIT TCLP)
      *         FORM-OBJ = "  " (FORM MISSING)
      *                  = "3Z" (            )
      *                  = "3O" (OPTION NOT PURCHASED)
      *                  = "3S" (SECURITY VIOLATION)
      *   COPY: LOADCOW, ACCESS, ACCESSW
      *   REV :
      *    BLV 061098 ADD NEW FIELDS FOR CHANGES TO H_LOGUID.C
      *               LOGUID NO LONGER DOES ANOTHER ACCESS-CALL &
      *               WILL NOW WRITE TO /USR/TMP/ULFILE, NOT RTS_UTMP,
      *               REMOVED STATUS CHECK ON LOGUID
      *    BLV 061198 ADD INTERBR TO LOG BR# FOR INTER BRANCH PMTS
      *               AND BULK TRANSFER UPDATE FOR H_LOGUID.C
      * BLM 130213 FIX LOGUID TO NOT CALL C PROGRAM
      * MJD 131203 REPLACED "CANCEL FORM-PATHNAME" WITH
      *             "CANCEL SAVE-FORM-PATH" ON THE MAIN CALL.    
      *            WHEN RETURNING FROM THE CALL
      *            FORM-PATH HAS THE RETURN PROGRAM NAME NOT THE NAME OF
      *            THE PROGRAM YOU JUST CALLED AND IS THEREFORE NOT
      *            CANCELLING THE PROGRAM WE JUST CALLED.
      * MJD 140612 ADDED EXIT-PATHNAME TO CALL OF LOGUID.
      * BLM 150616 FIX CALL OF LOGUID FOR CHANGED ARGUMENTS, FIX
      *            CANCEL OF FORM-PROGX, GET RID OF SECURITY 
      *            ERROR CHECKS NOW IN CHKSEC, GET RID OF LOADCO-OP1
      *  BAH 20220816 COMMENTED OUT LOGUID
      ****************************************************************
      *    CALL "LOGUID" USING FORM-PATHNAME 
      *                           FORM-PROGX
      *                           EXIT-PATHNAME.
      *    CANCEL "LOGUID".

           CALL FORM-PROGX USING FORM-PATH FORM-LINK-AREA EXIT-PATHNAME
                ON EXCEPTION GO TO LOAD-OVERFLOW.
           CANCEL FORM-PROGX.

      **************************************
      *    CHECK FOR SECURITY ERRORS     
      **************************************
           IF FORM-OBJ = "3Z"
              GO TO LOAD-OVERFLOW.
      *  END SECURITY ERROR CHECK
           IF FORM-NAM = " "
              GO TO STOP-RUN.
           GO TO LOAD-POST.
       LOAD-OVERFLOW.
           MOVE "CALL ERROR" TO ERRMSG-HEAD.
           MOVE "CA," TO FILE-STAT-DISP.
           CALL "C$CALLERR" USING FILE-STATD2.
           MOVE FILE-STAT-DISP TO ERRMSG-MSG1.
           MOVE FORM-PATH     TO ERRMSG-MSG2.
           MOVE SPACES         TO ERRMSG-MSG3.
           PERFORM CALL-ERRMSG.
           GO TO STOP-RUN.
