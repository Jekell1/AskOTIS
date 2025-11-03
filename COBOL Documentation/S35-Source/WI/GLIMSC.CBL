       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLIMSC.
       DATE-WRITTEN.    011696.
      ******************************************************************
      *
      *                      ( WI )
      *              LOAN APPORVAL LIMIT SCAN WINDOW
      *                 ONLY CALLED IN WI/GLIMIT
      *
      * SHARES PASSWORD WITH WI/GLIMIT OPT 1
      *    GROUP NAME MUST MATCH YOUR LOGIN ID (A0001CS WILL ASK FOR A
      *        PASSWORD IF YOU ENTER GROUPD "CFS")
      *    BRANCH MUST EXIST WITHIN YOUR GROUP (GROUP "CS" MUST HAVE BRANCH
      *        10 IN IT IF YOU ENTER BRANCH 10, OR WILL ASK PASSWORD)
      *
      *=================================================================
      * REV:
      * GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * BAH 150516 REMOVED ALL MOVES TO EXT-CONAME-USER-GROUP, USE WGR-WORD,
      *            DO NOT WANT TO SET AN EXTERNAL VARIABLE IN HERE!
      *  CS 141114 CHANGED VDU-WCOLOR-TCLP/VDU-WCTRL-TCLP TO
      *            VDU-WCOLOR-SCAN/VDU-WCTRL-SCAN  
      *
      * KEC 2017.0216 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          INCREASE PAGE-KEY TO 7 BYTES TO MATCH GI1-KEY.
      *  CS 190516 CHANGED SELECT-KEY TO X(7) FROM X(6), WHEN YOU SELECTED
      *            FOR EXAMPLE CLASS 11, IT DISPLAYED THE INFO FOR CLASS10
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
           "@(#)WI/GLIMSC.CBL S34 05/25/21-10:36:30 >".

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

      ******************************************************************
      *
      *     M I S C E L L E N O U S
      *
      ******************************************************************
       01  GLIMSC-LINES          PIC 99   VALUE 22.
       01  GLIMSC-COLUMNS        PIC 99   VALUE 25.
       01  GLIMSC-BEGIN-Y        PIC 99   VALUE 2.
       01  GLIMSC-BEGIN-X        PIC 99   VALUE 55.

       01  ERRCD                    PIC X       VALUE SPACES.
       01  CASE-BUF                 PIC X(4).
       01  CURRENT-BRANCH           PIC 9(4)    VALUE ZEROES.
       01  ENT-GROUP                PIC X(4).
       01  GROUP-FLAG               PIC X(1).
       01  HOLD-WGR-ERR             PIC 9(1)    VALUE ZEROES.

       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(7)    OCCURS 12 TIMES.

       01  WS-MESSAGES.
           03  VIEW-BRANCH-MESS     PIC X(55)      VALUE
              "VIEWING THIS BRANCH IS NOT ALLOWED WITHOUT PASSWORD!!!".
           03  VIEW-GROUP-MESS      PIC X(54)      VALUE
              "VIEWING THIS GROUP IS NOT ALLOWED WITHOUT PASSWORD!!!".

       01  ONE                      PIC S9         VALUE 1.

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY             PIC X(7)       OCCURS 250 TIMES.
           03  PAGE-GROUP-COUNTER   PIC 9(3)       OCCURS 250 TIMES.

       01  SUB                      PIC 99.
       01  SUB-GR                   PIC 999.

       01  WS-GROUP-COUNTER         PIC 9(3).

      *-----------------------------------------------------------------
      *    PAGES
      *-----------------------------------------------------------------
       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 250.
       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "120.".

      *-----------------------------------------------------------------
      *    LEGENDS.
      *-----------------------------------------------------------------
       01  LEGENDS.
           03  LEGEND-01            PIC X(19)    VALUE
                                                 "F6-SCAN,F7, BR/GRP:".
           03  LEGEND-02            PIC X(17)    VALUE
                                                 "F1-CLEAR, SELECT:".

      ******************************************************************
      *
      *    E N T E R E D - P A R A M E T E R S
      *
      ******************************************************************
       01  ENTERED-PARAMETERS.
           03  EP-GROUP             PIC 9(4).
           03  EP-GROUPX            REDEFINES EP-GROUP
                                    PIC X(4).

      ******************************************************************
      *
      *    F L D S E L
      *
      ******************************************************************
       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".

       01  KEY-SUBSCRIPT REDEFINES FLDSEL-FIELD.
           03  KEY-SUB              PIC 99.
           03  FILLER               PIC XX.

       01  FLDSEL-LIST.
           03  FILLER               PIC X(48)  VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120.".
       01  FLDSEL-BUF.
           03  FLDSEL-GR.
               05  FLDSEL-GR-BRANCH   PIC 9(4).
               05  FILLER             PIC X(2).
               05  FLDSEL-GR-CLASS    PIC 9(2).
               05  FILLER             PIC X(2).
               05  FLDSEL-GR-LIMIT    PIC Z,ZZZ,ZZ9.

      ******************************************************************
      *
      *    D I S P L A Y
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER               PIC X(48)  VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120 ".
           03  FILLER               PIC X(15)  VALUE
           "PGNO ENDMORE:F.".

       01  DISPLAY-BUF.
           03  D-LINE         OCCURS 12 TIMES.
               05  DISPLAY-BRANCH       PIC ZZZ9.
               05  FILLER               PIC X(2).
               05  DISPLAY-CLASS        PIC 9(2).
               05  FILLER               PIC X(2).
               05  DISPLAY-LIMIT        PIC Z,ZZZ,ZZ9.
           03  DISPLAY-PAGENO       PIC ZZ9.
           03  DISPLAY-END-MORE     PIC X(7).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GLIMSC_DEF.CPY".
       COPY "LIBWI/GLIMSC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  GLIMSC-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GLIMSCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GLIMSC_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME GLIMSC-BUF EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO GLIMSC-STATUS.

           MOVE "GLIMSC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 03 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 20 * VDU-WIN-C-OFFSET
                   SIZE 25 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
      *             BACKGROUND-COLOR IS 1
      *             WITH NO WRAP
                   HANDLE  IS GLIMSC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       ENTER-GROUP.


           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R  A000GROUP." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUP
              ELSE
                 MOVE "S  A000GROUP." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF ( VDUSTATUS = "10" )           GO TO ENTER-GROUP.
           IF ( VDUSTATUS = "1>" )           GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-GROUP.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP.

           IF ( WGR-TYPE = "A" )
              PERFORM OPEN-GR1-FILE
              MOVE "S  A000GROUP." TO VDU
              MOVE ENT-GROUP       TO VDUBUF GR-ID EP-GROUPX
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0   TO CURRENT-BRANCH
              PERFORM SEARCH-GR-INIT
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"             TO GROUP-FLAG.
           MOVE WGR-SRESULT     TO EP-GROUP CURRENT-BRANCH.
           MOVE "SN N040GROUP." TO VDU.
           MOVE EP-GROUP        TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           MOVE "OK" TO PASSWDW-STAT.
           PERFORM CHECK-FOR-PASSWORD.

           IF ( PASSWDW-STAT NOT = "OK" )
              GO TO ENTER-GROUP.

      *=================================================================
      *
      *    BEGINNING SCAN ON THE ENTERED GROUP ID.
      *
      *=================================================================
       BEGIN-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

           MOVE CURRENT-BRANCH TO GI-BRANCH.
           MOVE ZEROES         TO GI-CLASS WS-GROUP-COUNTER.
           MOVE GI1-KEY        TO PAGE-KEY(1).
           PERFORM OPEN-GI1-FILE.

      *----------------------------------------------------------------
       START-BUILD-PAGENO.

           ADD ONE TO CURRENT-PAGE.
           PERFORM SET-GROUP-COUNTER.

           IF ( CURRENT-PAGE < 1 )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           IF ( CURRENT-PAGE > PAGE-COUNT )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           IF ( GROUP-FLAG = "Y" )
              PERFORM SETUP-GR-FILE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1      TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO GI1-KEY.
           PERFORM START-GI1-FILE.

      *-----------------------------------------------------------------
      *
      *    BUILD PAGE
      *
      *-----------------------------------------------------------------
       BUILD-PAGENO.

           PERFORM NEXT-VALID-GI.

           IF ( IO-FG NOT = 0 ) OR ( SUB > 12 )
              GO TO DISPLAY-PAGE.

           MOVE GI1-KEY      TO SELECT-KEY(SUB).

      *----------------------------------------------------------------
       MOVE-TO-DISPLAY.
           MOVE GI-BRANCH            TO DISPLAY-BRANCH(SUB).
           MOVE GI-CLASS             TO DISPLAY-CLASS(SUB).
           MOVE GI-LN-APPROVAL-LIMIT TO DISPLAY-LIMIT(SUB).

      *----------------------------------------------------------------
       INCREMENT-SUB.
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *-----------------------------------------------------------------
      *
      *    DISPLAY PAGE
      *
      *-----------------------------------------------------------------
       DISPLAY-PAGE.
           IF ( IO-FG NOT = 0 )
              IF ( SUB = 1 )
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-GROUP.

           MOVE "MORE..." TO DISPLAY-END-MORE.

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-FG = 0 )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE GI1-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO DISPLAY-END-MORE
              ELSE
                 MOVE "* END *" TO DISPLAY-END-MORE.

           MOVE CURRENT-PAGE TO DISPLAY-PAGENO.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
      *
      *    SELECT FIELD FROM PAGES
      *
      *-----------------------------------------------------------------
       SELECT-FIELD-FROM-DISPLAY.

           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:

           IF ( FLDSEL-STAT = "1U" )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:

           IF ( FLDSEL-STAT = "1P" )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:

           IF ( FLDSEL-STAT = "1N" ) OR ( FLDSEL-STAT = "1D" )
              IF ( CURRENT-PAGE NOT < PAGE-COUNT )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:

           IF ( FLDSEL-STAT = "1H" )
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

      * TEST FOR TO RE-ENTER BRANCH OR GROUP-ID.

           IF ( FLDSEL-STAT = "10" )
              GO TO ENTER-GROUP.

           IF ( NOT (FLDSEL-STAT = "00" OR "1<") )
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST IF TO ACCEPT CURRENT KEY

           IF (FLDSEL-STAT = "00")
              MOVE "00"                TO GLIMSC-STATUS
              MOVE SELECT-KEY(KEY-SUB) TO GLIMSC-KEY
              GO TO END-ROUTINE.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      ******************************************************************
       SETUP-GR-FILE SECTION.
      ******************************************************************
           MOVE 0         TO SUB-GR.
           MOVE EP-GROUPX TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       FIND-GR.

           IF ( SUB-GR < PAGE-GROUP-COUNTER(CURRENT-PAGE) )
              ADD 1 TO SUB-GR
           ELSE
              MOVE 0 TO WS-GROUP-COUNTER
              MOVE WGR-SRESULT TO CURRENT-BRANCH
              GO TO EXIT-SETUP-GR-FILE.

           PERFORM SEARCH-GR-NEXT.

           GO TO FIND-GR.

       EXIT-SETUP-GR-FILE.
           EXIT.

      ******************************************************************
       CHECK-FOR-PASSWORD SECTION.
      ******************************************************************

      * CHECKING TO SEE IF LOGIN ID IS A GROUP MEMBER

           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO WGR-WORD.
           PERFORM CHECK-MEMBER.
           PERFORM OPEN-GR1-FILE.

           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

           IF ( EP-GROUP NUMERIC )
              GO TO CHECK-IF-BRANCH-IN-MEMBER
           ELSE
              GO TO CHECK-IF-GROUP-IN-MEMBER.

      * CHECKING TO SEE IF BRANCH ENTERED IS WITHIN THE MEMBER.
       CHECK-IF-BRANCH-IN-MEMBER.

           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE EP-GROUP   TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.

           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

           GO TO EXIT-CHECK-FOR-PASSWORD.

      * CHECKING TO SEE IF GROUP ENTERED IS WITHIN THE MEMBER.
       CHECK-IF-GROUP-IN-MEMBER.

           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE EP-GROUPX  TO WGR-TEST-GROUP.
           PERFORM VERIFY-GROUP-MEMBER.
           MOVE WGR-ERR TO HOLD-WGR-ERR.
           MOVE EP-GROUPX  TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           IF ( HOLD-WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

           GO TO EXIT-CHECK-FOR-PASSWORD.

      * LOGIN ID OR BRANCH/GROUP ENTERED IS NOT VALID,
      * MUST ENTER PASSWORD TO MAKE CHANGE TO LIMIT.
      *
      * SHARES PASSWORD WITH WI/GLIMIT OPT 1
      *    GROUP NAME MUST MATCH YOUR LOGIN ID (A0001CS WILL ASK FOR A
      *        PASSWORD IF YOU ENTER GROUPD "CFS")
      *    BRANCH MUST EXIST WITHIN YOUR GROUP (GROUP "CS" MUST HAVE BRANCH
      *        10 IN IT IF YOU ENTER BRANCH 10, OR WILL ASK PASSWORD)

       PASSWORD-REQUIRED-BR.

           PERFORM VERIFY-PASSWORD.

           IF ( PASSWDW-STAT NOT = "OK" )
              IF ( GROUP-FLAG = "Y" )
                 MOVE VIEW-GROUP-MESS TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE VIEW-BRANCH-MESS TO MESS
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

      *****************************************************************
       NEXT-VALID-GI SECTION.
      *****************************************************************
       NEXT-VALID-GI-FILE.

           IF ( CURRENT-BRANCH = ZEROES )
              GO TO NEXT-VALID-GROUP.

           PERFORM READ-GI1-FILE-NEXT.

           IF ( IO-FG = 9 )
              GO TO EXIT-VALID-GI.

           IF ( GI-BRANCH NOT = CURRENT-BRANCH )
              IF ( GROUP-FLAG = "Y" )
                 GO TO NEXT-VALID-GROUP
              ELSE
                 MOVE 9 TO IO-FG.

           GO TO EXIT-VALID-GI.

      *----------------------------------------------------------------
       NEXT-VALID-GROUP.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = 0 )
              MOVE 9 TO IO-FG
              GO TO EXIT-VALID-GI
           ELSE
              ADD  1           TO WS-GROUP-COUNTER
              MOVE WGR-SRESULT TO GI-BRANCH CURRENT-BRANCH
              MOVE ZEROES      TO GI-CLASS
              PERFORM START-GI1-FILE
              GO TO NEXT-VALID-GI-FILE.

       EXIT-VALID-GI.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY GLIMSC-SCREEN UPON GLIMSC-WINDOW-HANDLE.
           MOVE GLIMSC-FORM-FIELDS TO WR-BUF.
           MOVE GLIMSC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE GLIMSC-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLIMSC-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GLIMSC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       GET-GI.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.

      *-----------------------------------------------------------------
       SET-GROUP-COUNTER.
      D    STOP MESS.
           IF ( FLDSEL-STAT = "1N" OR "1D" )
              IF ( CURRENT-PAGE > 1 )
                 COMPUTE PAGE-GROUP-COUNTER(CURRENT-PAGE) =
                         WS-GROUP-COUNTER +
                         PAGE-GROUP-COUNTER(CURRENT-PAGE - 1)
              ELSE
                 MOVE 1 TO PAGE-GROUP-COUNTER(CURRENT-PAGE).

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A190WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLIMSC-SCREEN.
           DESTROY GLIMSC-WINDOW-HANDLE.
           EXIT PROGRAM.
