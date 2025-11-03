      * COPYMEMBER: LIBGB/LOADCO

      ****************************************************************
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOGIC TO SUBSTITUTE FORM-PATH CALL VIA. ENVIRONMENT
      *
      *    EXAMPLE OF USE OF THIS LOGIC:
      *      YOU NEED TO RUN A DEBUG VERSION OF LONPF0.C ON THE LIVE
      *      CUSTOMERS MACHINE.
      *
      *      SET ENVIRONMENT:
      *          1. ALT       = /USR/A15/CO/LP/LONPF0
      *          2. ALT_PATH  = /USR/A15/CO/DB/LONPF0
      *
      *          3. ALT2      = /USR/A15/CO/LP/LONPF0
      *          4. ALT2_PATH = /USR/A15/CO/LP/DBUGF0
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      * REV :
      *  BLV 061098 ADD NEW FIELDS FOR CHANGES TO H_LOGUID.C
      *             LOGUID NO LONGER DOES ANOTHER ACCESS-CALL &
      *             WILL NOW WRITE TO /USR/TMP/ULFILE, NOT RTS_UTMP,
      *             REMOVED STATUS CHECK ON LOGUID
      *  BLV 061198 ADD INTERBR TO LOG BR# FOR INTER BRANCH PMTS
      *             AND BULK TRANSFER UPDATE FOR H_LOGUID.C
      *  JTG 990708 ADDED LOGIC TO SUBSTITUTE FORM-PATH CALL VIA. ENVIRONMENT
      *  JTG 991201 ADDED LOGIC TO SUBSTITUTE FORM-PATH CALL VIA. ENVIRONMENT
      *             EITHER LOADCO_ALT & LOADCO_ALT_PATH  (OR)  ALT & ALT-PATH
      *  JTG 030107 ADDED LOGIC TO SUBSTITUTE FORM-PATH CALL VIA. ENVIRONMENT
      *              REMOVED LOADCO_ALT
      *             CHANGED TO ALT2 & ALT2_PATH  (OR)  ALT & ALT-PATH
      *  JTG 031014 ADDED MOVE " " TO MESS FOR ALT2 TEST
      *  BAH 121107 CHANGED USING FORM-PATHNAME TO FORM-PATH-LINK
      *  BLM 130108 CHECK LOADCO-LOAN-PROC-FG TO DETERMINE CALLING ARGUMENTS
      *  BLM 130213 FIX LOGUID TO NOT CALL C PROGRAM
      *  BLM 130221 CHANGED "CALL ERRMSG" TO PERFORM CALL-ERRMSG
      *  MJD 140612 ADDED EXIT-PATHNAME TO CALL OF LOGUID.
      *  BLM 150616 FIX CALL OF LOGUID FOR CHANGED ARGUMENTS, FIX
      *             CANCEL OF FORM-PROGX, GET RID OF SECURITY
      *             ERROR CHECKS NOW IN CHKSEC, GET RID OF LOADCO-OP1,
      *             ALT-PATH LOGIC
      *  BAH 20220816 COMMENTED OUT LOGUID
      **************************************************************************
       LOAD-IT.
      * LOGUID:
      *    CALL "LOGUID" USING FORM-PATHNAME
      *                           FORM-PROGX
      *                           EXIT-PATHNAME.
      *    CANCEL "LOGUID".

           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF
                ON OVERFLOW GO TO LOAD-OVERFLOW.
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

       LOAD-MISSING.
           PERFORM SEND-MESS.
           MOVE ACCESS-BUF TO MESS FORM-PATH.
           PERFORM SEND-MESS.
           GO TO LOAD-RESTART.

       LOAD-OVERFLOW.
           MOVE "CALL ERROR" TO ERRMSG-HEAD.
           MOVE "CA," TO FILE-STAT-DISP.
           CALL "C$CALLERR" USING FILE-STATD2.
           MOVE FILE-STAT-DISP TO ERRMSG-MSG1.
           MOVE FORM-PATH TO ERRMSG-MSG2.
           MOVE SPACES TO ERRMSG-MSG3.
           PERFORM CALL-ERRMSG.
           GO TO STOP-RUN.
