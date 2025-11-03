       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       TZSCAN.
       AUTHOR.              MJD.
       DATE-WRITTEN. 2000/08/02.
      *******************************************************************
      *   TIME ZONE FILE SCAN
      *
      *  NOTE: THIS SCAN WORKS SEVERAL DIFFERENT WAYS:
      *     1) NORMAL SCAN  TZSCAN-TZ = 0
      *                     TZSCAN-PF-ONLY NOT = "Y"
      *                     TZSCAN-PF-AUTOSTART NOT = "Y"
      *        THIS IS A NORMAL SCAN PROMPTING USER FOR AREA CODE AND
      *        PREFIX WITH NO EDITS.
      *
      *     2) AUTO ENTER AREA CODE   TZSCANW-AC NOT = 0
      *        IN THIS MODE, THE SCAN IS PASSED AN AREA CODE AND THE
      *        USER IS ONLY PROMPTED FOR A PREFIX. THE USER HAS THE
      *        ABILITY TO CHANE THE AREA CODE ONCE INSIDE THE SCAN.
      *
      *     3) PREFIX ONLY  TZSCANW-AC NOT = 0
      *                     TZSCAN-PF-ONLY = "Y"
      *        SIMILAR TO ABOVE WITH THE FOLOWING EXCEPTION:
      *        ONLY ALLOW THE USER TO SCAN THE PREFIXES WITHIN A GIVEN
      *        AREA CODE.  AREA CODE FIELD CAN NOT BE ACCESSED WHILE
      *        IN THIS MODE.
      *
      *     3) AUTOSTART    TZSCANW-AC NOT = 0
      *                     TZSCAN-PF-AUTOSTART = "Y"
      *        THIS WILL SKIP THE ENTRY OF AREA CODE AND PREFIX AND
      *        START THE FILE WITH THE VALUES PASSED FROM THE CALLING
      *        PROGRAM.  THIS OPTION CAN BE COMBINED WITH ABOVE OPTIONS.
      *
      *    NOTE: TZSCAN-PF-ONLY AND TZSCAN-PF-AUTOSTART ARE REST TO
      *          SPACES UPON EXIT.
      ******************************************************************
      * REV:
      * BLM 20220204 ADD CHECK IN BUILD-PAGENO:
      *    IF SUB = 1
      *       IF CURRENT-PAGE > 1
      *          IF TZ1-KEY NOT = PAGE-KEY-S(CURRENT-PAGE)
      *             GO TO BUILD-PAGENO.
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/TZSCAN.CBL S35 08/11/22-14:40:55 >".
       COPY "LIBLP/LP01TZ.CPY".
       COPY "LIBLP/LP01TZ_SQL.CPY".
       COPY "LIBLP/LPWSTZ.CPY".

       01  TZSCAN-LINES             PIC 99    VALUE 17.
       01  TZSCAN-COLUMNS           PIC 99    VALUE 61.
       01  TZSCAN-BEGIN-Y           PIC 99    VALUE 03.
       01  TZSCAN-BEGIN-X           PIC 99    VALUE 03.
       01  ERRCD                    PIC X      VALUE " ".
       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  LEGEND-01                  PIC X(44)  VALUE
           " ENTER AREA CODE FOR REGION, F3-ALL, F7-EXIT".
       01  LEGEND-02                  PIC X(44)  VALUE
           "    ENTER PREFIX FOR REGION, F3-ALL, F7-EXIT".
       01  LEGEND-FLDSEL              PIC X(44)  VALUE
           " <ENTER>-SELECT, F1-RESET, F7-EXIT, F8-HELP ".
       01  TZ-PROMPT                  PIC X(57)  VALUE
           "DIALING CODE    DESCRIPTION              STATE  ZONE  DLS".

       01  ALP-TZ.
           03  ALP-AC               PIC X(3).
           03  ALP-HYPHEN1          PIC X.
           03  ALP-PF               PIC X(3).

       01  NUM-TZ                   PIC 9(6).
       01  FILLER                   REDEFINES NUM-TZ.
           03  NUM-AC               PIC X(3).
           03  NUM-PF               PIC X(3).

       01  SCAN-KEY.
           03  SCAN-AC              PIC 9(3).
           03  SCAN-PF              PIC 9(3).

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-TZ        PIC X(11).
           03  FILLER           PIC X(05).
           03  FLDSEL-DESC      PIC X(23).
           03  FILLER           PIC X(04).
           03  FLDSEL-STATE     PIC X(02).
           03  FILLER           PIC X(03).
           03  FLDSEL-ZONE      PIC 9(03).
           03  FILLER           PIC X(04).
           03  FLDSEL-DLS       PIC X(01).
           03  FILLER           PIC X(01).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  PROMPT-LIST              PIC X(6)  VALUE "000:S.".

       01  HOLD-TZ              PIC 9(06).
       01  FILLER REDEFINES HOLD-TZ.
           03  HOLD-AC          PIC 9(03).
           03  HOLD-PF          PIC 9(03).

       01  TZSCAN-WINDOW-HANDLE  PIC X(10).
       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
                   07  D-TZ         PIC X(11).
                   07  FILLER       PIC X(05).
                   07  D-DESC       PIC X(23).
                   07  FILLER       PIC X(04).
                   07  D-STATE      PIC X(02).
                   07  FILLER       PIC X(03).
                   07  D-ZONE       PIC X(03).
                   07  FILLER       PIC X(04).
                   07  D-DLS        PIC X(01).
                   07  FILLER       PIC X(01).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 50.
       01  PAGE-KEY.
           03  FILLER               OCCURS 50 TIMES.
               05  PAGE-KEY-S.
                   07  PAGE-KEY-AC  PIC 9(3).
                   07  PAGE-KEY-PF  PIC 9(3).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TZSCAN_DEF.CPY".
       COPY "LIBWI/TZSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/TZSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TZSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME TZSCAN-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
       MAIN-MODULE.
           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO TZSCAN-STATUS.

           MOVE "TZSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE TZSCAN-BEGIN-Y
                   AT COL  TZSCAN-BEGIN-X
                   SIZE    TZSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   TZSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS TZSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE TZ-PROMPT TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * TRY TO VALIDATE INCOMMING KEYS

           IF NOT (TZSCANW-AC NUMERIC)
              MOVE 0 TO TZSCANW-AC.
           IF NOT (TZSCANW-PF NUMERIC)
              MOVE 0 TO TZSCANW-PF.

           MOVE TZSCAN-TZ TO HOLD-TZ.

      *    ******* SEE TOP OF PROGRAM FOR DESCRIPTION OF OPTIONS *******
      *
      * IF YOU WANT TO SEARCH PREFIX ONLY, AREA CODE MUST BE NON ZERO

           IF TZSCANW-AC = 0 AND TZSCAN-PF-ONLY = "Y"
              MOVE "N" TO TZSCAN-PF-ONLY.

      * IF AREA CODE AND PREFIX ARE SET, THEN SCAN CAN DEFAULT TO
      *  TO THESE INITIAL SETTINGS. THIS IS USEFULL WHEN ENTERRING
      *  ENDING RANGES THAT MUST FOLLOW A BEGINNING RANGE.

           IF TZSCANW-AC NOT = 0
              MOVE "SNNN030AC." TO VDU
              MOVE TZSCANW-AC TO VDUNUM0 SCAN-AC
              PERFORM SCREENIO
              IF TZSCAN-PF-AUTOSTART = "Y"
                 MOVE "SNNN030PF." TO VDU
                 MOVE TZSCANW-PF TO VDUNUM0 SCAN-PF
                 PERFORM SCREENIO
                 GO TO SKIP-ENTER-PF.
           MOVE ZEROS TO TZSCAN-TZ.

           IF TZSCAN-PF-ONLY = "Y" AND TZSCAN-PF-AUTOSTART = "Y"
              GO TO SKIP-ENTER-PF.

      *************************************
       ENTER-SCANKEY.
      *************************************
       ENTER-AC.
           IF TZSCAN-PF-ONLY = "Y"
              MOVE "SNNN030AC." TO VDU
              MOVE HOLD-AC TO VDUNUM0 SCAN-AC
              PERFORM SCREENIO
              GO TO ENTER-PF.

           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENRN030AC." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-ROUTINE.
           IF F3-KEY OR (ENTER-KEY AND VDUNUM0 = 0)
              MOVE "S  A030AC." TO VDU
              MOVE "ALL" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A030PF." TO VDU
              MOVE "ALL" TO VDUBUF
              PERFORM SCREENIO
              MOVE ZEROS TO SCAN-AC SCAN-PF
              GO TO SKIP-ENTER-PF.
           IF NOT ENTER-KEY
              GO TO ENTER-AC.
           MOVE VDUNUM0 TO SCAN-AC.

       ENTER-PF.
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENRN030PF." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-AC.
           IF F7-KEY
              GO TO END-ROUTINE.
           IF F3-KEY
              MOVE "S  A030PF." TO VDU
              MOVE "ALL" TO VDUBUF
              PERFORM SCREENIO
              MOVE 0 TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF NOT ENTER-KEY
              GO TO ENTER-SCANKEY.
           MOVE VDUNUM0 TO SCAN-PF.

       SKIP-ENTER-PF.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-TZ1-FILE.

           MOVE SCAN-KEY TO PAGE-KEY-S(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY-S(CURRENT-PAGE) TO TZ1-KEY.

           PERFORM START-TZ1-FILE.

       BUILD-PAGENO.
           PERFORM READ-TZ1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           IF TZSCAN-PF-ONLY = "Y"
              IF TZ-CODE NOT = HOLD-AC
                 GO TO BUILD-PAGENO.

           MOVE TZ-CODE TO NUM-AC.
           MOVE TZ-PREFIX TO NUM-PF.
           IF NUM-TZ = 0
              GO TO BUILD-PAGENO.
            
           PERFORM NUMTZ-TO-ALPTZ.
           MOVE ALP-TZ   TO D-TZ(SUB)
           MOVE TZ-DESC  TO D-DESC(SUB).
           MOVE TZ-STATE TO D-STATE(SUB).
           MOVE TZ-DLS   TO D-DLS(SUB).
           EVALUATE TRUE
              WHEN TZ-IS-EASTERN   MOVE "EST" TO D-ZONE(SUB)
              WHEN TZ-IS-CENTRAL   MOVE "CST" TO D-ZONE(SUB)
              WHEN TZ-IS-MOUNTAIN  MOVE "MST" TO D-ZONE(SUB)
              WHEN TZ-IS-PACIFIC   MOVE "PST" TO D-ZONE(SUB)
              WHEN TZ-IS-ALASKA    MOVE "AST" TO D-ZONE(SUB)
              WHEN TZ-IS-HAWAII    MOVE "HST" TO D-ZONE(SUB)
           END-EVALUATE.

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-SCANKEY.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE TZ1-KEY TO PAGE-KEY-S(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-FLDSEL TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANKEY.
           IF NOT (FLDSEL-STAT = "00" OR "18" OR "1<")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-TZ TO ALP-TZ.
           PERFORM ALPTZ-TO-NUMTZ.
           MOVE NUM-TZ TO TZSCAN-TZ.

           MOVE "00" TO TZSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           DISPLAY TZSCAN-SCREEN UPON TZSCAN-WINDOW-HANDLE.
           MOVE TZSCAN-FORM-FIELDS TO WR-BUF.
           MOVE TZSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE TZSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TZSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TZSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMTZ-TO-ALPTZ.
           MOVE NUM-AC TO ALP-AC.
           MOVE NUM-PF TO ALP-PF.
           MOVE "-" TO ALP-HYPHEN1.
       ALPTZ-TO-NUMTZ.
           MOVE ALP-AC TO NUM-AC.
           MOVE ALP-PF TO NUM-PF.
           IF NUM-TZ NOT NUMERIC
              MOVE 0 TO NUM-TZ.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TZ1-FILE.
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBLP/LPTZ1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************
       END-ROUTINE SECTION.
      *******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TZSCAN-SCREEN.
           DESTROY TZSCAN-WINDOW-HANDLE.

           MOVE SPACES TO TZSCAN-PF-ONLY
                          TZSCAN-PF-AUTOSTART.
       STOP-RUN.
           EXIT PROGRAM.

