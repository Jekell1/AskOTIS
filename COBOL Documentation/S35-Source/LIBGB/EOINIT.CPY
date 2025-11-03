      * COPYMEMBER: LIBGB/EOINIT
      ******************************************************************
      *                 (EOINIT)
      *    BATCH PROCESSING DRIVER
      *
      *    EXT-ROUTE-BUF:
      *         "00" -  MENU DRIVEN
      *         "01" -  MENU CHAINED?
      *         "40" -  INITIAL PARAMETER ENTRY
      *         "50" -  MODIFY PARAMETERS
      *         "60" -  AUDIT OF PROCEDURE
      *         "70" -  EXECUTION OF PROCEDURE
      *
      *=================================================================
      * REV:
      * BLM 101124 ADD ACCEPT OF GP-REC ENVIRONMENT TO CHECK NEW
      *            SECURITY FLAG, IF NEW - CALL CHKSEC INSTEAD OF
      *            COCHK, PR# NEWSECURITY
      * BLM 101202 REMOVE PARAGRAPH NAME "CHECK-STATUS" SINCE GETFM
      *            COULD BE PERFORMED AS A PARAGRAPH, PR# NEWSECURITY
      * BLM 111130 MOVE VERSION WITH NEW SECURITY TO A15, PR# NEWSECURITY
      * MJD 130108 ADDED DISPLAY WINDOW UPON EXT-EOBUF-WINDOW-HANDLE
      *            IN PLACE OF GETFM.
      *            NOTE THAT EOMAIN WILL HAVE TO OPEN IT'S OWN WINDOW
      *            WHEN FOR ROUTE-BUF = "40" OR "50".
      * BAH 130412 CHANGED ERRCD TO EXT-EOBUF-ERRCD EXTERNAL
      * 10/16/2013 BAH CHANGED FROM STANDARD GRAPHICAL TO FLOATING BECAUSE
      *            EMTRRG CALLED FROM WI/BALAID (FLOATING) WOULD NOT DISPLAY
      *            CLEANLY, LEFT OPEN/CLOSE CASH DRAWER (LONPL0) SCREEN MIXED
      *            IN.
      * MJD 140508 ADDED EXT_WIN_W_MOD AND EXT_WIN_H_MOD TO WINDOW
      * MJD 140509 REMOVED EXT-WINDOW-CELL-HEIGHT/WIDTH AND
      *            REPLACED THEM WITH CELL SIZE IS LABEL FONT.  
      *            THIS WILL DEFAULT THE CELL SIZE TO BE THAT OF THE
      *            FONT AND SCALE APPROPRIATELY IN A30/V30.
      * MJD 2014.0604 ADDED ADDITINOAL EXT- WORKERS FOR WINDOW CONTROL
      * JKC 2020.1026 ADDED TIMER FUNCTION (GB/TIMER.CBL) IF MENU
      *               DRIVEN EXECUTION AFTER THE USER ENTERS "Y" TO ACCEPT.
      * BAH 2024.0308 COMMENTED OUT MOVE "Y" TO QUEUE-BUF
      * JKC 2024-0716 ADDED SQL-ERROR-DISPLAY; NEEDED FOR SUPPRESSING
      *          ERROR MESSAGE WINDOW DISPLAYING WHEN EXECUTING VIA
      *          BATCH (EOEXEC/EOCRON); CANNOT USE EXT-ROUTE-BUF SINCE
      *          NOT ALL PROGRAMS THAT USE CONNECT_SQL DOES NOT HAVE
      *          CONAME_EXT WHICH IS NEEDED; S35Q-88
      *          MISSING STORED PROCEDURE STOPPED ENTIRE BATCH AND MADE
      *          USER PRESS RETURN. SHOULD CONTINUE WITH NEXT STEP
      *
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.
      
      * NEED TO SUPPRESS ERROR MESSAGE WINDOW FROM DISPLAYING WHEN
      * EXECUTING VIA BATCH (EOEXEC/EOCRON)
      * MISSING STORED PROCEDURE STOPPED ENTIRE BATCH AND MADE
      * USER PRESS RETURN. SHOULD CONTINUE WITH NEXT STEP
           IF EXT-ROUTE-BUF = "60" OR "70"
              MOVE "N" TO SQL-ERROR-DISPLAY.

      * UTILITY OPTION; NO DISPLAY
           MOVE "U"    TO P-TIMER-TYPE.

           IF EXT-ROUTE-BUF = "00"
              GO TO EOINIT-MENU-DRIVEN.

           IF EXT-ROUTE-BUF = "01"
              GO TO EOINIT-MENU-CHAINED.

           MOVE "E" TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EOINIT-EDIT.
           IF EXT-ROUTE-BUF = "60"
              GO TO EOINIT-AUDIT.
           IF EXT-ROUTE-BUF = "70"
              GO TO EOINIT-EXEC.

           MOVE "BAD ROUTE-CODE" TO MESS.
           PERFORM SEND-MESS.

           MOVE EXT-ROUTE-BUF TO MESS.
           PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      **************************************************
      *    ENTRY OF PARAMETERS FORM EOMAIN.C
      *    EXT-ROUTE-BUF
      *         "40" - INITIAL PARAMETER ENTRY
      *         "50" - MODIFY PARAMETERS
      **************************************************
       EOINIT-EDIT.
           MOVE VERSION-CONTROL TO EXT-EOBUF-VERSION.
           MOVE " " TO EXT-EOBUF-ERRCD.

       EOINIT-EDIT-ENTRY.
           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF = "50"
              PERFORM DISPLAY-PARAMETERS
              MOVE "(PAGE2)" TO WR-BUF
              MOVE "SYSDATE." TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "  BATCH PROCEDURE MAINTENANCE" TO WR-BUF
              MOVE "CONAME." TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE "00" TO VDUSTATUS.
           PERFORM ENTER-PARAMETERS.
           IF VDUSTATUS = "10" GO TO EOINIT-EDIT-ENTRY.
           IF VDUSTATUS = "1>"
              MOVE "C" TO EXT-EOBUF-ERRCD.
           GO TO END-PROGRAM.


      **************************************************
      *    AUDIT OF PARAMETERS FROM EOEXEC.C
      *    EXT-ROUTE-BUF  = "60"
      **************************************************
       EOINIT-AUDIT.
           MOVE " " TO EXT-EOBUF-ERRCD.
           IF EXT-EOBUF-VERSION NOT = VERSION-CONTROL
              MOVE "V" TO EXT-EOBUF-ERRCD.
           PERFORM EO-EXEC-AUDIT.
           GO TO END-PROGRAM.

      **************************************************
      *    EXECUTION OF BATCH REPORT FROM EOEXEC.C
      *    EXT-ROUTE-BUF  = "70"
      **************************************************
       EOINIT-EXEC.

           PERFORM EO-EXEC-AUDIT.
           MOVE EXT-EOBUF-PRPATH TO PRU-FILE.
           MOVE EXT-EOBUF-COPIES TO PRU-COPIES.

           MOVE " "              TO EXT-EOBUF-ERRCD.

           GO TO EOINIT-CONTINUE.

      **************************************************
      *    MENU DRIVEN REPORT
      *    EXT-ROUTE-BUF = "00"
      **************************************************
       EOINIT-MENU-DRIVEN.

           MOVE FORM-PATHNAME TO FORM-PATH.
           IF FORM-PROG = " "
              MOVE FORM-PROGX TO FORM-PROG.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-EOINIT
                   CONTROL VALUE IS VDU-WCTRL-EOINIT
                   MODAL
                   BLANK SCREEN
                   HANDLE IS EXT-EOBUF-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      ***********************************************
      *   S E C U R I T Y   C H E C K               *
      ***********************************************
           MOVE FORM-PATH   TO CHKSEC-FORM-PATH.
           MOVE "CO"        TO FORM-OBJ.
           MOVE "CL/CHKSEC" TO FORM-NAM.

           CALL "CHKSEC" USING CHKSEC-FIELDS ON OVERFLOW
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO EXIT-PROG
           END-CALL.

           CANCEL "CHKSEC".
           MOVE CHKSEC-FORM-PATH TO FORM-PATH.
           MOVE CHKSEC-STAT      TO STAT.
           IF STAT = "3U"
              CANCEL FORM-PROGX
              MOVE CHKSEC-FORM-PATH TO FORM-PATH
              MOVE CHKSEC-STAT TO STAT.

           IF STAT NOT = "00"
              MOVE STAT TO FORM-OBJ
              GO TO EXIT-PROG.

      * END OF SECURITY CHECK:

       EOINIT-SETPR.
           PERFORM PRU-IN
           IF STAT NOT = "00"
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO END-PROGRAM.
       EOINIT-ENTER-MENU.
           PERFORM ENTER-PARAMETERS.
           IF VDUSTATUS = "10" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1U" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS = "99" GO TO EOINIT-CONTINUE.
       EOINIT-ACCEPT-DATA.
      * PROGRAM MUST SET THIS TO "Y" IF IT DOES NOT WANT TO STOP ON
      * THE ACCEPT FIELD.... USED TO JUST CHECK IF THE FLAG EXISTED ON THE
      * FORM AS A NONDISPLAY FIELD
           IF NOACCEPT-BUF = "Y"
              GO TO EOINIT-CONTINUE.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS = "10" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1U" GO TO EOINIT-ENTER-MENU.
           IF VDUSTATUS NOT = "00" GO TO EOINIT-ACCEPT-DATA.
           IF VDUBUF = "N" GO TO EOINIT-ENTER-MENU.
           IF VDUBUF NOT = "Y"
              GO TO EOINIT-ACCEPT-DATA.
     
      * PERFORM TIMER-START FUNCTION; SET BEG TIME FIELD.
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.

           IF ( P-TIMER-TYPE-SET-VALID )
               MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
           ELSE
      * REPORT OPTION
               MOVE "R" TO P-TIMER-TYPE. 

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

           GO TO EOINIT-CONTINUE.

       EOINIT-MENU-CHAINED.
           MOVE FORM-PATHNAME TO FORM-PATH.

       EOINIT-CONTINUE.
