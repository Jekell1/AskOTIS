       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLIMIT.
       DATE-WRITTEN.    011696.
      ******************************************************************
      *
      *                      ( WI )
      *              APPROVAL LIMIT WINDOW
      *
      * REV:
      *   042996 BLL CHANGED OPENS & IO ON GIFILE; MERCURY WAS GETTING
      *              REWRITE ERRORS OF 49 ON GI
      *
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
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/GLIMIT.CBL S34 05/24/21-13:06:35 >".

      ******************************************************************
      *
      *    P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
   
       01  GLIMIT-LINES          PIC 99   VALUE 12.
       01  GLIMIT-COLUMNS        PIC 99   VALUE 33.
       01  GLIMIT-BEGIN-Y        PIC 99   VALUE 11.
       01  GLIMIT-BEGIN-X        PIC 99   VALUE 10.

       01  ERRCD                    PIC X          VALUE SPACES.
       01  CASE-BUF                 PIC X(4).

       01  USER-GROUP           PIC X(3).

       01  WS-MESSAGES.
           03  LIMIT-ZERO-MESS      PIC X(51)      VALUE
               "ZERO LIMIT WILL NOT ALLOW THIS BRANCH TO MAKE LOANS".
           03  CHANGE-BRANCH-MESS   PIC X(56)      VALUE
              "CHANGING THIS BRANCH IS NOT ALLOWED WITHOUT PASSWORD!!!".

      ******************************************************************
      *
      *    E N T E R E D - P A R A M E T E R S
      *
      ******************************************************************
       01  ENTERED-PARAMETERS.
           03  EP-BRANCH            PIC 9(4).
           03  EP-CLASS             PIC 9(2).

       01  EP-APP-LIMIT             PIC 9(7).

      ******************************************************************
      *
      *    D I S P L A Y
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER               PIC X(4)  VALUE
           "010.".
       01  DISPLAY-BUF.
           03  F01                  PIC Z,ZZZ,ZZ9.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/GLIMSCW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       01  GLIMIT-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GLIMIT_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/GLIMIT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GLIMIT_SCN.CPY".
      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "GLIMIT" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE GLIMIT-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  GLIMIT-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    GLIMIT-COLUMNS + VDU-WIN-W-MOD
                      LINES   GLIMIT-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS GLIMIT-WINDOW-HANDLE.


       ENTER-BRANCH.
           PERFORM FORM-RESET.

           MOVE "F6-SCAN, F7-EXIT:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )     GO TO ENTER-BRANCH.
           IF ( VDUSTATUS = "1>" )     GO TO END-ROUTINE.

           IF ( VDUSTATUS = "1<" )
              PERFORM APP-LIMIT-SCAN
              IF ( GLIMSC-STATUS NOT = "00" )
                 GO TO ENTER-BRANCH
              ELSE
                 GO TO VALID-DISPLAY-OF-FIELDS.

           IF ( VDUSTATUS NOT = "00" ) GO TO ENTER-BRANCH.
           IF ( VDUNUM0   = ZEROES )   GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BR-NO EP-BRANCH.

           PERFORM GET-BR.

           IF ( IO-FG NOT = 0 )
              GO TO ENTER-BRANCH.

      * PASSWORD NEEDED IF NOT LOGIN FOR THAT BRANCH.
           MOVE "OK" TO PASSWDW-STAT.
           PERFORM CHECK-FOR-PASSWORD.

           IF ( PASSWDW-STAT NOT = "OK" )
              GO TO ENTER-BRANCH.

       ENTER-CLASS.

           MOVE "^/V, F1-CLEAR, F6-SCAN:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020LOAN-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO ENTER-BRANCH.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1<" )
              PERFORM CLASS-SCAN
              IF ( IO-FG NOT = 0 )
                 GO TO ENTER-CLASS.

           IF ( VDUSTATUS NOT = "00" ) GO TO ENTER-CLASS.
           IF ( VDUNUM0   = ZEROES )   GO TO ENTER-CLASS.

           MOVE VDUNUM0 TO LC-CODE EP-CLASS.
           PERFORM GET-LC.

           IF ( IO-FG NOT = 0 )
              GO TO ENTER-CLASS.

      * IF VALID BRNO & CLASS, DISPLAY THE FIELDS.

       VALID-DISPLAY-OF-FIELDS.

           PERFORM DISPLAY-SCREEN.

           IF ( IO-FG  = 9 )
              GO TO ENTER-BRANCH.

       ENTER-LIMIT.

           MOVE "^/V, F1-CLEAR, ENTER CHANGE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNN070010." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-CLASS.

           IF ( VDUSTATUS NOT = "00" ) GO TO ENTER-LIMIT.

           IF ( VDUNUM0 = ZEROES )
              MOVE LIMIT-ZERO-MESS TO MESS
              PERFORM SEND-MESS.

           MOVE VDUNUM0 TO EP-APP-LIMIT.

       ENTER-FUNCTION-KEY.

           MOVE "^/V, F1-CLEAR, F7-UPDATE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-LIMIT.

           IF ( VDUSTATUS = "1>" )
              PERFORM UPDATE-GI
              GO TO ENTER-BRANCH.

           GO TO ENTER-FUNCTION-KEY.

      ******************************************************************
       CHECK-FOR-PASSWORD SECTION.
      ******************************************************************

      * CHECKING TO SEE IF LOGIN ID IS A GROUP MEMBER

           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO USER-GROUP WGR-WORD.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

      * CHECKING TO SEE IF BRANCH ENTERED IS WITHIN THE MEMBER.

           PERFORM OPEN-GR1-FILE.
           MOVE USER-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE EP-BRANCH  TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.

           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

           GO TO EXIT-CHECK-FOR-PASSWORD.

      * LOGIN ID OR BRANCH ENTERED IS NOT VALID,
      * MUST ENTER PASSWORD TO MAKE CHANGE TO LIMIT.

       PASSWORD-REQUIRED-BR.

           PERFORM VERIFY-PASSWORD.

           IF ( PASSWDW-STAT NOT = "OK" )
              MOVE CHANGE-BRANCH-MESS TO MESS
              PERFORM SEND-MESS.

       EXIT-CHECK-FOR-PASSWORD.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "GLIMIT"                     TO PASSWDW-PGM.
           MOVE 1                            TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW THIS BRANCH:"       TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                  TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************************************
       DISPLAY-SCREEN SECTION.
      ******************************************************************

           MOVE EP-BRANCH TO GI-BRANCH.
           MOVE EP-CLASS  TO GI-CLASS.

           PERFORM GET-GI.

           IF ( IO-FG  = 9 )
              MOVE "PROBLEM GETTING APPROVAL LIMIT INFO" TO MESS
              PERFORM SEND-MESS
              GO TO DISPLAY-SCREEN-EXIT.

           MOVE GI-LN-APPROVAL-LIMIT TO F01.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-SCREEN-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-GI SECTION.
      ******************************************************************
           PERFORM OPEN-GI1-FILE.
           MOVE EP-BRANCH    TO GI-BRANCH.
           MOVE EP-CLASS     TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE EP-APP-LIMIT TO GI-LN-APPROVAL-LIMIT.

           PERFORM REWRITE-GI1-FILE.

           PERFORM CLOSE-GI1-FILE.

       EXIT-UPDATE-GI.
           EXIT.

      ******************************************************************
       APP-LIMIT-SCAN SECTION.
      ******************************************************************

           MOVE ZEROES     TO GLIMSC-BRANCH GLIMSC-CLASS.

           MOVE "WI/GLIMSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLIMSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( GLIMSC-STATUS NOT = "00" )
               GO TO EXIT-APP-LIMIT-SCAN.

           MOVE "SNNN040BRANCH." TO VDU.
           MOVE GLIMSC-BRANCH    TO VDUNUM0 EP-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN020LOAN-CLASS." TO VDU.
           MOVE GLIMSC-CLASS         TO VDUNUM0 EP-CLASS.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.

       EXIT-APP-LIMIT-SCAN.
           EXIT.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************

           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.

           MOVE "SNNN020LOAN-CLASS." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0          TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON GLIMIT-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY GLIMIT-SCREEN UPON GLIMIT-WINDOW-HANDLE.
           MOVE GLIMIT-FORM-FIELDS TO WR-BUF.
           MOVE GLIMIT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE GLIMIT-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLIMIT-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GLIMIT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      *-----------------------------------------------------------------
       GET-GI.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.

      *-----------------------------------------------------------------
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A210WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGI1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLIMIT-SCREEN.
           DESTROY GLIMIT-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
