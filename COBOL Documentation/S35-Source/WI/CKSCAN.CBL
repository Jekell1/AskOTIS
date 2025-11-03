       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CKSCAN.
      ******************************************************************
      *              LOAN CHECK FILE WINDOW SCAN
      *
      * REV:
      * GLB  030519 FIX BUG IF NOTHING TO DISPLAY;ALLOW UP ARROW
      * GLB  030612 ALLOW ENTRY OF CHECK TYPE TO LIMIT DISPLAY
      * BLF  060216 INCREASE MAX PAGES TO 100
      * BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/CKSCAN S35 05/03/24-13:20:42 >".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".

       01  CKSCAN-LINES          PIC 99  VALUE 20.
       01  CKSCAN-COLUMNS        PIC 99  VALUE 78.
       01  CKSCAN-BEGIN-Y        PIC 99  VALUE 2.
       01  CKSCAN-BEGIN-X        PIC 99  VALUE 2.

       01  ERRCD                    PIC X      VALUE SPACES.
       01  DATE1                    PIC 9(8).
       01  DATE2                    PIC 9(8).
       01  CHECK1                   PIC 9(6).
       01  CHECK2                   PIC 9(6).

       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  BATCHID.
           03  FILLER               PIC XX     VALUE "B-".
           03  BATCHID-MASK         PIC 9999.
       01  SCAN-TYPE                PIC 9      VALUE 2.
       01  CHECK-TYPE               PIC X(3)   VALUE "ALL".

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(20)  VALUE
           "100 110 120 130 140.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY       PIC Z(6).
           05  FILLER           PIC X(71).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "140.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(35)  VALUE
           "100 110 120 130 140 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 14 TIMES.
                07  D-KEY        PIC Z(6).
                07  D-T-VOID     PIC X.
                07  FILLER       PIC X.
                07  D-T-TYPE     PIC XX.
                07  FILLER       PIC X.
                07  D-T-DATE     PIC 99/99/99.
                07  FILLER       PIC X.
                07  D-T-PAYEE    PIC X(18).
                07  D-T-AMT      PIC ZZZZZZ9.99-.
                07  D-T-REFX.
                    09  D-T-REF  PIC 9(6)      BLANK ZERO.
                07  FILLER       PIC X.
                07  D-T-REASON   PIC X(21).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 100.
       01  PAGE-KEY-01.
            05  PAGE-KEY              OCCURS 100 TIMES.
                07  PAGE-KEY-BRNO    PIC 9(4).
                07  PAGE-KEY-NO      PIC 9(6).


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".

       COPY "LIBWI/CKSCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CKSCAN_DEF.CPY".
       COPY "LIBWI/CKSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  CKSCAN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CKSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CKSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CKSCAN-BUF EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
       MAIN-MODULE.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "XX" TO CKSCAN-STATUS.

           MOVE "CKSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CKSCAN-BEGIN-Y
                   AT COL  CKSCAN-BEGIN-X
                   SIZE    CKSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CKSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CKSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "SN N040BRNO." TO VDU.
           MOVE CKSCANW-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE 2     TO SCAN-TYPE.
           MOVE "ALL" TO CHECK-TYPE.

           MOVE "S  A000TYPE." TO VDU.
           MOVE CHECK-TYPE TO VDUBUF.
           PERFORM SCREENIO.

           IF CKSCAN-NOSTOP-FG  = "Y"
              MOVE " " TO VDUBUF
              MOVE 0 TO DATE1 DATE2 CHECK1 CHECK2
              GO TO ENTER-BRNO-CONT
           ELSE
              GO TO ENTER-BRNO.

      *************************************
      *    ENTER CKFILE SCAN TYPE
      *************************************
       ENTER-TYPE.

           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS  = "16"
              MOVE "S  A000TYPE." TO VDU
              MOVE "ALL" TO VDUBUF CHECK-TYPE
              PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           IF VDUBUF = SPACES
              MOVE "S  A000TYPE." TO VDU
              MOVE CHECK-TYPE TO VDUBUF CHECK-TYPE
              PERFORM SCREENIO.
           MOVE VDUBUF TO CHECK-TYPE.
           MOVE "S  A000TYPE." TO VDU.
           MOVE CHECK-TYPE TO VDUBUF.
           PERFORM SCREENIO.

      *************************************
      *    ENTER BRANCH NO.
      *************************************
       ENTER-BRNO.
           IF SCAN-TYPE = 1
              MOVE LEGEND-02 TO MESS
           ELSE
              MOVE LEGEND-03 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRNO.
           IF VDUNUM0 = 0
              MOVE "SN N040BRNO." TO VDU
              MOVE CKSCANW-BRNO TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO CKSCANW-BRNO.

       ENTER-BRNO-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           MOVE CKSCANW-BRNO TO PAGE-KEY-BRNO(PAGE-COUNT).
           MOVE 0            TO PAGE-KEY-NO(PAGE-COUNT).
       CK-01.
           IF CKSCAN-NOSTOP-FG  = "Y"
              MOVE " " TO VDUBUF
              MOVE 0 TO DATE1 DATE2 CHECK1 CHECK2
              MOVE " " TO CKSCAN-NOSTOP-FG
              GO TO START-BUILD-PAGENO.
           MOVE LEGEND-03B TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060CK1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "ID") GO TO DT-01.
           MOVE VDUNUM0 TO CHECK1.
           IF CHECK1 = 0
              MOVE 0 TO CHECK2
              GO TO DT-01.
       CK-02.
           MOVE "EN N060CK2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "ID") GO TO CK-02.
           MOVE VDUNUM0 TO CHECK2.

       DT-01.
           IF CKSCAN-NOSTOP-FG NOT = " "
              MOVE 0 TO DATE1
              MOVE " " TO CKSCAN-NOSTOP-FG
              GO TO START-BUILD-PAGENO.
           MOVE LEGEND-03C TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U" GO TO CK-01.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "ID") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO DATE1.
           IF DATE1 = 0
              MOVE 0 TO DATE2
              GO TO START-BUILD-PAGENO.

       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "ID") GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO DATE2.

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           PERFORM OPEN-CK1-FILE.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO CK1-KEY.

           PERFORM START-CK1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-CK1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (SUB > 14) OR 
                (CK-BRNO NOT = CK-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           IF CK-BRNO NOT = CKSCANW-BRNO
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF CK-NO < CHECK1
              GO TO BUILD-PAGENO.
           IF CHECK2 NOT = 0
              IF CK-NO > CHECK2
                 GO TO BUILD-PAGENO.

           IF DATE1 = 0
              GO TO BUILD-PAGENO2.

           IF CK-DATE < DATE1
              GO TO BUILD-PAGENO.

           IF DATE2 = 0
              GO TO BUILD-PAGENO2.

           IF CK-DATE > DATE2
              GO TO BUILD-PAGENO.

       BUILD-PAGENO2.
           IF CHECK-TYPE NOT = "ALL"
              IF CK-TYPE NOT = CHECK-TYPE
                 GO TO BUILD-PAGENO.

           IF CK-VOID = "Y"
              MOVE "V"  TO D-T-VOID(SUB)
           ELSE
           IF CK-VOID = "R"
              MOVE "R"  TO D-T-VOID(SUB).

           MOVE CK-NO   TO D-KEY(SUB).
           MOVE CK-TYPE TO D-T-TYPE(SUB).
           MOVE CK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-T-DATE(SUB).
           MOVE CK-PAYEE    TO D-T-PAYEE(SUB).
           MOVE CK-AMT      TO D-T-AMT(SUB).
           IF CK-BATCHID NOT = 0
              MOVE CK-BATCHID TO BATCHID-MASK
              MOVE BATCHID TO D-T-REF(SUB)
           ELSE
              MOVE CK-REF TO D-T-REF(SUB).

           MOVE CK-REASON TO D-T-REASON(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO D-T-PAYEE(2)
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-BRNO.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE CK1-KEY TO PAGE-KEY(PAGE-COUNT)
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

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-04 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
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
              GO TO ENTER-BRNO.
           IF NOT (FLDSEL-STAT = "00" OR "18" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO CKSCANW-NO.

           MOVE "00"       TO CKSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY CKSCAN-SCREEN UPON CKSCAN-WINDOW-HANDLE.
           MOVE CKSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CKSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE CKSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CKSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************
       IO-ROUTINES SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CK1-FILE.
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CKSCAN-SCREEN.
           DESTROY CKSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
