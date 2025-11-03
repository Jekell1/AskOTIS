       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPMENU.
      *****************************************************************
      *
      *  DIR : LP
      *  DESC: MAIN MENU LOAN SYSTEM MENU
      *
      *=================================================================
      * REV:
      * MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *            BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *            AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      * BLV 021398 CHANGED #10 APS TO OPMENU
      * JTG 122298 ADDED #13 DIRECT TO NOTES
      * MJD 121214 ADDED TCLP-MAIN-WINDOW-HANDLE TO SCREEN DISPLAY.
      * MJD 130109 ADDED TCLP-MAIN-WINDOW-HANDLE TO DISPLAY OMITTED,
      *            ADDED HELP DIR.
      * BAH 130530 REMOVED F6-RESET PRINTER, FOR A30, DO NOT NEED
      * BLM 150615 ADDED BACK ALTERNATE LOGIN & WHOAMI LOGIC FROM
      *            MENU1C.
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
      *****************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPMENU_DEF.CPY".
       COPY "LIBLP/LPMENU_WKS.CPY".
      *01  EXTERNAL-WORKER-BUF-TEST    EXTERNAL.
      *    03  E-PATH.
      *        05  E-SLASH     PIC X.
      *        05  E-FIL       PIC X(18).
      *        05  E-SLASH2    PIC X.
      *        05  E-DIR       PIC X(2).
      *    03  E-REL         PIC X(35).
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      * NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      * 'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      * NAME IN THE TRACE LOG.
      
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPMENU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           MOVE 0 TO RETURN-CODE.

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
           MOVE SPACES    TO WR-BUF.
           PERFORM CALL-WRFORM.

      * DETERMINE IF YOU ARE HERE FROM A REMOTE LOGIN
           MOVE EXT-CONAME-BRNO-BUF TO BRNO-BUF.
           CALL "WHOAMI" USING FORM-PATH RLOGIN-BUF EXIT-PATHNAME.
           CANCEL "WHOAMI".
           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF RLOGIN-BUF = SPACES
              MOVE "N" TO RLOGIN-FG
                          RLOGIN-ALLOW-REMOTE
                          RLOGIN-ALLOW-LOCAL
                          RLOGIN-IS-LOCAL
                          RLOGIN-IS-REMOTE.
           IF RLOGIN-FG = "Y"
              IF RLOGIN-ALLOW-REMOTE = "Y"
                 OR RLOGIN-ALLOW-LOCAL = "Y"
              MOVE PROMPT-MSG TO PROMPT-BUF
           ELSE
              MOVE SPACES TO PROMPT-BUF.

           IF RLOGIN-IS-REMOTE = "Y" OR RLOGIN-IS-LOCAL = "Y"
              IF RLOGIN-IS-REMOTE = "Y"
                 MOVE "REMOTE" TO REMOTE-BUF
              ELSE
                 MOVE SPACES TO REMOTE-BUF
              END-IF
              MOVE REMOTE-INFO-BUF TO WR-BUF
              MOVE BRNO-LIST-FLASH TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE SPACES          TO REMOTE-BUF
              MOVE REMOTE-INFO-BUF TO WR-BUF
              MOVE BRNO-LIST       TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE PROMPT-BUF  TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  N020SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO FORM-BUF.

           GO TO MENU-EXIT.

       MENU-EXIT.
      *----------------------------------------------------------

           IF VDUSTATUS = "12"
              GO TO EXIT-PROG.

      * F4 - GLOBAL MENU
           IF VDUSTATUS = "18"
              MOVE "GB/GBMENU" TO FORM-NAM
              GO TO RUN-PROG.

      * F9 - SQL INFO
           IF VDUSTATUS = "1("
              MOVE "GB/SQLINF" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           IF FORM-BUF = 11
              IF RLOGIN-FG NOT = "Y"
                 GO TO GET-MENU-SELECTION.

           GO TO M01 M02 M03 M04 M05
                 M06 M07 M08 M09 M10 M11
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/LONP" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/LPRPMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/LPIQMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "LP/LPCPMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "SP/EOMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "LP/LPEDMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "LP/LPEMMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "LP/LPEYMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "SP/SPMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "SP/OPMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "LP/OTHBR" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPMENU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           GO TO DISPLAY-MENU.

       EXIT-PROG.
           DESTROY LPMENU-SCREEN.
      *    MOVE "LOGOFF" TO FORM-PROGX.
      *    CALL "LOGUID" USING FORM-PATHNAME
      *                        FORM-PROGX
      *                        EXIT-PATHNAME.
      *    CANCEL "LOGUID".

           MOVE "OTIS SESSION COMPLETED" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME            TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPMENU"                 TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

      * NEEDS TO BE LAST BEFORE 'STOP RUN'
           PERFORM SQL-DISCONNECT. 

      *    MOVE "AFTER SQL-DISCONNECT"   TO EXT-TCLP-TRACE-MESS.
      *    MOVE FORM-PATHNAME            TO EXT-TCLP-TRACE-FORM-PATH.
      *    MOVE "LPMENU"                 TO EXT-TCLP-TRACE-FORM-PROGX.
      *    PERFORM TRACE-SEND-MESS.
      
           STOP RUN.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LPMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPMENU-HELP--DIR  TO HELP-LINK-DIR.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LPMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPMENU-FORM-FIELDS TO WR-BUF.
           MOVE LPMENU-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

       MISC-ROUTINES SECTION.
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/DISCONNECT_SQL.CPY".
      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

