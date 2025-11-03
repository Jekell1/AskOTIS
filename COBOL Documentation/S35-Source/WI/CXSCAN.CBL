       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CXSCAN.
      ******************************************************************
      *
      * DESCRIPTION: TEMPORARY CHECK FILE WINDOW SCAN
      *
      *=================================================================
      * REV:
      * GLB 000501 ADD DATES
      * GLB 010816 ADD DEALER NO.
      * MJD 131120 A30 CONVERSION COMPLETED.  CHANGED FORM FIEL
      *            TYPE" TO "ATYPE" TO AVOID CONVERSION
      *            CONFLICTS IN THE SCN WORKED (CXSCAN-TYPE)
      * KEC 2016.1011 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED PAGE-KEY2 TO MATCH SIZE OF CX2-KEY (60).
      * BAH 170414 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181010 GLOBAL CXFILE
      * BAH 2024.0426 MADE A CALLED PROGRAM, NOT WINDOW
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
           "@(#)WI/CXSCAN.CBL S35 04/16/24-13:13:35 >".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".

       01  CXSCAN-LINES          PIC 99  VALUE 17.
       01  CXSCAN-COLUMNS        PIC 99  VALUE 80.
       01  CXSCAN-BEGIN-Y        PIC 99  VALUE 6.
       01  CXSCAN-BEGIN-X        PIC 99  VALUE 1.

       01  ERRCD                    PIC X      VALUE SPACES.

       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  BATCHID.
           03  FILLER               PIC XX     VALUE "B-".
           03  BATCHID-MASK         PIC 9999.
       01  SCAN-TYPE                PIC 9      VALUE 2.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-FIELD-NUM        REDEFINES FLDSEL-FIELD.
           03  FLDSEL-FIELD-N       PIC 99.
           03  FILLER               PIC XX.
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-T.
               05  FLDSEL-T-TYPE    PIC XX.
               05  FILLER           PIC XX.
               05  FLDSEL-T-PAYEE   PIC X(26).
               05  FLDSEL-T-AMT     PIC ZZZZZZ9.99-.
               05  FILLER           PIC X.
               05  FLDSEL-T-REF     PIC 9(6).
               05  FILLER           PIC XX.
               05  FLDSEL-T-REASON  PIC X(27).
           03  FLDSEL-P            REDEFINES FLDSEL-T.
               05  FLDSEL-P-PAYEE   PIC X(26).
               05  FLDSEL-P-AMT     PIC ZZZZZZ9.99-.
               05  FILLER           PIC X.
               05  FLDSEL-P-TYPE    PIC XX.
               05  FILLER           PIC XX.
               05  FLDSEL-P-REF     PIC 9(6).
               05  FILLER           PIC XX.
               05  FLDSEL-P-REASON  PIC X(27).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  PROMPT-LIST              PIC X(6)  VALUE "000:S.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-T.
                   07  D-T-TYPE     PIC XX.
                   07  FILLER       PIC XX.
                   07  D-T-PAYEE    PIC X(26).
                   07  D-T-AMT      PIC ZZZZZZ9.99-.
                   07  FILLER       PIC X.
                   07  D-T-REFX.
                       09  D-T-REF  PIC 9(6)      BLANK ZERO.
                   07  FILLER       PIC XX.
                   07  D-T-REASON   PIC X(27).
               05  D-P             REDEFINES D-T.
                   07  D-P-PAYEE    PIC X(26).
                   07  D-P-AMT      PIC ZZZZZZ9.99-.
                   07  FILLER       PIC X.
                   07  D-P-TYPE     PIC XX.
                   07  FILLER       PIC XX.
                   07  D-P-REFX.
                       09  D-P-REF  PIC 9(6)      BLANK ZERO.
                   07  FILLER       PIC XX.
                   07  D-P-REASON   PIC X(27).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).
       01  NON-DISP-BUF.
           03  D-LINE2               OCCURS 9 TIMES.
               05  D-SEQNO          PIC 9999.

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 20.

       01  PAGE-KEY.
           03  FILLER                   OCCURS 50 TIMES.
               05  PAGE-KEY2.
                   07  PAGE-KEY2-BRNO   PIC 9(4).
                   07  PAGE-KEY2-PAYEE  PIC X(60).
               05  PAGE-KEY1.
                   07  PAGE-KEY-BRNO    PIC 9(4).
                   07  PAGE-KEY-TYPE    PIC X(2).
                   07  PAGE-KEY-SEQNO   PIC 9(4).


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/CXSCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       01 CXSCAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CXSCAN_DEF.CPY".
       COPY "LIBWI/CXSCAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CXSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CXSCAN_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME CXSCAN-BUF EXIT-PATHNAME.

      ***************************
      *      MAIN PROGRAM MODULE
      ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
      D    STOP MESS.

           MOVE "XX" TO CXSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CXSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE CXSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                    SCREEN COL CXSCAN-BEGIN-X * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 17 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CXSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE 2 TO SCAN-TYPE VDUNUM0.
           MOVE "SNNN010TYPE." TO VDU.
           PERFORM SCREENIO.

           PERFORM SEND-SORT-PROMPT.

           MOVE " " TO VDUBUF.

           GO TO ENTER-SCANKEY-CONT.

      *************************************
      *    ENTER CXFILE SCAN TYPE
      *    1- CHECK TYPE
      *    2- PAYEE NAME
      *************************************
       ENTER-TYPE.
           MOVE "S  A010TYPE." TO VDU.
           MOVE "_"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           IF NOT (VDUNUM0 = 1 OR 2)
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO SCAN-TYPE.

           PERFORM SEND-SORT-PROMPT.

       ENTER-SCANKEY.
           IF SCAN-TYPE = 1
              MOVE LEGEND-02 TO MESS
           ELSE
              MOVE LEGEND-03 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA200SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           IF SCAN-TYPE = 1
              MOVE CXSCANW-BRNO TO PAGE-KEY-BRNO(PAGE-COUNT)
              MOVE VDUBUF       TO PAGE-KEY-TYPE(PAGE-COUNT)
              MOVE 0            TO PAGE-KEY-SEQNO(PAGE-COUNT)
              MOVE 0   TO PAGE-KEY2-BRNO(PAGE-COUNT)
              MOVE " " TO PAGE-KEY2-PAYEE(PAGE-COUNT).
           IF SCAN-TYPE = 2
              MOVE CXSCANW-BRNO TO PAGE-KEY2-BRNO(PAGE-COUNT)
              MOVE VDUBUF       TO PAGE-KEY2-PAYEE(PAGE-COUNT)
              MOVE 0 TO PAGE-KEY-BRNO(PAGE-COUNT)
              MOVE " " TO PAGE-KEY-TYPE(PAGE-COUNT)
              MOVE 0 TO PAGE-KEY-SEQNO(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF
                          NON-DISP-BUF.
           MOVE 1 TO SUB.
           PERFORM OPEN-CX1-FILE.

           IF SCAN-TYPE = 1
              MOVE PAGE-KEY1(CURRENT-PAGE) TO CX1-KEY
           ELSE
              MOVE PAGE-KEY2-BRNO(CURRENT-PAGE) TO CX-BRNO
              MOVE PAGE-KEY2-PAYEE(CURRENT-PAGE) TO CX-PAYEE.

           MOVE CX-PATH-OWNBR TO CX-BRNO.

           PERFORM START-CXFILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-CXFILE.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           IF (CX-BRNO NOT = CXSCANW-BRNO)
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SCAN-TYPE = 1
              MOVE CX-TYPE TO D-T-TYPE(SUB)
              MOVE CX-PAYEE TO D-T-PAYEE(SUB)
              MOVE CX-AMT TO D-T-AMT(SUB)
              IF CX-BATCHID NOT = 0
                 MOVE CX-BATCHID TO BATCHID-MASK
                 MOVE BATCHID TO D-T-REF(SUB)
              ELSE
                 MOVE CX-REF TO D-T-REF(SUB)
              END-IF
              MOVE CX-REASON TO D-T-REASON(SUB)
              MOVE CX-SEQNO TO D-SEQNO(SUB)
           ELSE
              MOVE CX-PAYEE TO D-P-PAYEE(SUB)
              MOVE CX-AMT TO D-P-AMT(SUB)
              MOVE CX-TYPE TO D-P-TYPE(SUB)
              IF CX-BATCHID NOT = 0
                 MOVE CX-BATCHID TO BATCHID-MASK
                 MOVE BATCHID TO D-P-REF(SUB)
              ELSE
                 MOVE CX-REF TO D-P-REF(SUB)
              END-IF
              MOVE CX-REASON TO D-P-REASON(SUB)
              MOVE CX-SEQNO TO D-SEQNO(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
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
                    MOVE CX1-KEY TO PAGE-KEY1(PAGE-COUNT)
                    MOVE CX-BRNO  TO PAGE-KEY2-BRNO(PAGE-COUNT)
                    MOVE CX-PAYEE TO PAGE-KEY2-PAYEE(PAGE-COUNT)
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
                 GO TO ENTER-SCANKEY
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

           IF SCAN-TYPE = 1
              MOVE FLDSEL-T-TYPE TO CXSCANW-TYPE
              MOVE D-SEQNO(FLDSEL-FIELD-N) TO CXSCANW-SEQNO
           ELSE
              MOVE FLDSEL-P-TYPE TO CXSCANW-TYPE
              MOVE D-SEQNO(FLDSEL-FIELD-N) TO CXSCANW-SEQNO.

           MOVE "00" TO CXSCAN-STATUS.

           GO TO END-ROUTINE.

      ***************************************
       CXFILE-IO-ROUTINES SECTION.
      ***************************************
       START-CXFILE.
           IF SCAN-TYPE = 1
              PERFORM START-CX1-FILE
           ELSE
           IF SCAN-TYPE = 2
              PERFORM START-CX2-FILE.
       READ-CXFILE.
           IF SCAN-TYPE = 1
              PERFORM READ-CX1-FILE-NEXT
           ELSE
           IF SCAN-TYPE = 2
              PERFORM READ-CX2-FILE-NEXT
              IF IO-FG = 0
                    AND SUB = 1
                      AND CURRENT-PAGE NOT = 1
                 PERFORM READ-CX2-FILE-NEXT
                      UNTIL (CX1-KEY = PAGE-KEY1(CURRENT-PAGE)
                                 OR IO-FG = 9).

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           DISPLAY CXSCAN-SCREEN UPON CXSCAN-WINDOW-HANDLE.
           MOVE CXSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CXSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CXSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CXSCAN-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CXSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-SORT-PROMPT.
           IF SCAN-TYPE = 1
              MOVE TYPE-PROMPT TO WR-BUF
           ELSE
             MOVE PAYEE-PROMPT TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CX2-FILE.
           PERFORM CLOSE-CX1-FILE.
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPCX2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CXSCAN-SCREEN.
           DESTROY CXSCAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
