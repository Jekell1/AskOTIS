       IDENTIFICATION DIVISION.
       PROGRAM-ID.      KILUSR.
       AUTHOR.             MJD.
       DATE-WRITTEN.    021898.
      ******************************************************************
      *   DIRECTORY  : GB
      *
      *   DESCRIPTION: CALLED PROGRAM TO CALL XKILUSER
      *
      * REV:
      *  MG 991007 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BAH 140515 REMOVED KILUSR_CMD.CPY, PUT IN WORKING STORAGE
      * BAH 2023.1005 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/KILUSR.CBL S34 05/24/21-15:35:51 >".

      *=================================================================
      *
      *     P R O G R A M - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------
       01  ERRCD                              PIC X    VALUE SPACES.

       01  KIL-MSG.
           03  FILLER                         PIC X(25) VALUE
               "PREPARING TO KILL USER : ".
           03  K-USERID                       PIC X(10).
           03  FILLER                         PIC X(18) VALUE
               "  IS THIS CORRECT?".

       01  MESS-LIST                          PIC X(5)  VALUE "MESS.".
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************

      *        "/$REL/SH/EC /$REL/SH/KILUSR.SH ".
       01  RUN-KILUSR.
           03  FILLER            PIC X    VALUE "/".
           03  KILUSR-REL        PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  KILUSR-EC         PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  KILUSR-REL2       PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  KILUSR-SHELL      PIC X(10) VALUE "KILUSR.SH ".
           03  KILUSR-USERID     PIC X(10) VALUE SPACES.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  KILUSR-SEQ        VALUE 1.

       01  KILUSR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/KILUSR_DEF.CPY".
       COPY "LIBGB/KILUSR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGB/KILUSR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE "KILUSR" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS KILUSR-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO END-PROGRAM.

       ENTER-USERID.
           PERFORM CLEAR-LEGEND.
           MOVE "E RA010USER1." TO VDU.
           PERFORM SCREENIO.

      * TEST F7
           IF F7-KEY
              GO TO END-PROGRAM.

           IF NOT ENTER-KEY
               GO TO ENTER-USERID.

           MOVE VDUBUF TO KILUSR-USERID.

           IF KILUSR-USERID = SPACES
              GO TO ENTER-USERID.

       ENTER-ACCEPT.
           MOVE KILUSR-USERID TO K-USERID.
           MOVE KIL-MSG TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.

      * TEST F7
           IF F7-KEY
              GO TO END-PROGRAM.

           IF UP-KEY
              GO TO ENTER-USERID.

           IF NOT ENTER-KEY
               GO TO ENTER-ACCEPT.

           IF VDUBUF NOT = "Y"
              GO TO ENTER-ACCEPT.

      *    PERFORM ENDWIN-CALL.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO KILUSR-REL KILUSR-REL2.

           CALL "C$TOLOWER" USING KILUSR-EC, VALUE 3.
           CALL "C$TOLOWER" USING KILUSR-SHELL, VALUE 10.
           MOVE RUN-KILUSR TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT-BAD
              STRING "XKILLUSER IS NOT AVAILABLE, STATUS = ("
                       STAT,
                         ")!"
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS.

           GO TO ENTER-USERID.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "KILUSR" TO PASSWDW-PGM.
           MOVE KILUSR-SEQ TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO KILL USERS ON THE SYSTEM" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO KILUSR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO KILUSR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY KILUSR-SCREEN UPON KILUSR-WINDOW-HANDLE.
           MOVE KILUSR-FORM-FIELDS TO WR-BUF.
           MOVE KILUSR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE KILUSR-HELP--FILE TO HELP-LINK-FILE.
           MOVE KILUSR-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/KILUSR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-ROUTINE.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY KILUSR-SCREEN.        
           DESTROY KILUSR-WINDOW-HANDLE.
           EXIT PROGRAM.
