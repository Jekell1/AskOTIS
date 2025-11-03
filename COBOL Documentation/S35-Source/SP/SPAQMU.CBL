       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPAQMU.
      ******************************************************************
      *
      *  DIR : SP
      *  DESC: STATE AUDITOR MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  JTG 012198 ADDED 'CALL DLFORM' BEFORE STOP RUN STATEMENT
      *             ACU 311 NEEDS TO CLOSE OUT OUR SCREEN ENVIRONMENT??
      *  BLV 073198 ADDED LOGIC TO LOG OFF USER WHEN EXITING WITH F7
      *             LIKE TCLP CALLING LOGUID WITH "LOGGEDOFF" WHEN
      *             F2 HIT
      * JKC 210702 ADDED SQL-DISCONNECT BEFORE 'STOP RUN'; WE FOUND THAT
      *            THE USE OF SQL-DISCONNECT WHEN EXITING THE
      *            APPLICATION IS ONLY NEEDED; DOING A SQL-DISCONNECT
      *            WHEN EXITING A PROGRAM WILL CAUSE ALL CURSORS TO BE
      *            CLOSED FROM THE CALLING PROGRAM WHICH EFFECTS IT 
      *            WHEN CONTINUING WITH A READ-NEXT.
      *
      * JKC 220623 HAD TO ADD `EXEC SQL INCLUDE` (SQLCA & SQLDA) FOR
      *            THE USE OF LIBGB/DISCONNECT_SQL.CPY TO COMPILE
      *            WITHOUT WARNINGS WHICH DOES NOT SHOW ANY WARNING
      *            MESSAGE BUT SHOWS 1,000+ DUPLICATION OF THIS LINE:
      *               EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
      *
      * BAH 20220816 COMMENTED OUT LOGUID
      *
      * JKC 2024-0124 ADDED OTIS TRACE LOGGING LOGIC
      *
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/SPAQMU_DEF.CPY".
       COPY "LIBSP/SPAQMU_WKS.CPY".
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/SPAQMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPAQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "SPAQMU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

      * SECURITY CHECK
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL "CHKSEC" USING CHKSEC-FIELDS.
           CANCEL "CHKSEC".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF CHKSEC-STAT NOT = "00"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO MENU-EXIT.

       GET-MENU-SELECTION-CLR.
           MOVE 0 TO VDU-PROGRAMMING.

      ************************************************
      *    GET SELECTION
      ************************************************
       GET-MENU-SELECTION.
           MOVE FORM-LIST TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  N020SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO FORM-BUF.

           IF VDUSTATUS = "10"
              MOVE 1 TO RETURN-CODE
              GO TO EXIT-PROG.

       MENU-EXIT.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PROG.

           IF VDUSTATUS = "10"
              GO TO EXIT-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02
              DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.

       M01. MOVE "SP/AQEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/AUDIQ" TO FORM-NAM.
            GO TO RUN-PROG.
            
       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPAQMU-SCREEN.
       RUN-IT.
           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF X-FORM-NAMX = "AUDIQ"
              MOVE "LP/AUDIQ" TO FORM-NAM
              GO TO RUN-IT.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPAQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO DISPLAY-MENU.
       EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPAQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DESTROY SPAQMU-SCREEN.

      *    MOVE "LOGOFF" TO FORM-PROGX.
      *    CALL "LOGUID" USING FORM-PATHNAME
      *                        FORM-PROGX
      *                        EXIT-PATHNAME.
      *    CANCEL "LOGUID".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OTIS SESSION COMPLETED" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME            TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPAQMU"                 TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SQL-DISCONNECT.  *> NEED TO BE LAST BEFORE `STOP RUN`
           STOP RUN.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SPAQMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE SPAQMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPAQMU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY SPAQMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE SPAQMU-FORM-FIELDS TO WR-BUF.
           MOVE SPAQMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.

      ******************************************
       MISC-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/DISCONNECT_SQL.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPAQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

