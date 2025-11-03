       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FMSCN2.
      ******************************************************************
      *  FORM REQUEST FILE WINDOW SCAN
      *
      *  GLB 020109 START FM-FILE WITH INCOMING FMSCAN-FM-INITKEY
      *  BLV 020213 EXPAND SELECT-KEY FROM 14 TO 20; FM-BRNO2 WAS
      *             GETTING TRUNCATED & SO CORRECT KEY WAS NOT
      *             RETURNED TO FMMAIN
      *  GLB 030327 FIX DISPLAY OF CHECK FORM REQUESTS
      *  GLB 030804 FIX DISPLAY OF NOTE FORM REQUESTS
      *   CS 141114 CHANGED VDU-WCOLOR-TCLP/VDU-WCTRL-TCLP TO VDU-WCOLOR-SCAN/
      *             VDU-WCTRL-SCAN  
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/FMSCN2.CBL S34 05/21/21-15:30:47 >".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".

       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  HOLD-FMNO               PIC 9(4).
       01  INSPECT-W               PIC X(34).
       01  ADD-ON                  PIC X(16).
       01  SUB                     PIC 9(6).
       01  FORMAT-LOAN             PIC Z(6).
       01  FORMAT-SEQNO            PIC 999.
       01  DETAIL-WORKER           PIC X(40).
       01  FIRST-TIME              PIC X VALUE "Y".

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  KEY-SUBSCRIPT REDEFINES FLDSEL-FIELD.
           03  KEY-SUB              PIC 99.
           03  FILLER               PIC XX.
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC 9(4).
           05  FILLER              PIC X.
           05  FLDSEL-PRINT        PIC X.
           05  FILLER              PIC X.
           05  FLDSEL-DESC         PIC X(26).
           05  FLDSEL-BRNO         PIC X(4).
           05  FILLER              PIC X.
           05  FLDSEL-DETAIL       PIC X(24).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC 9(4).
               05  FILLER          PIC X.
               05  D-PRINT         PIC X.
               05  FILLER          PIC X.
               05  D-DESC-AREA.
                   07  D-DESC          PIC X(26).
               05  D-DESC-AREA2 REDEFINES D-DESC-AREA.
                   07  D-CHECK-AREA.
                       09  D-BATCHX   PIC X(5).
                       09  D-BATCHID  PIC Z(10).
                       09  FILLER     PIC X.
                   07  D-DESC-AMTX PIC X.
                   07  D-DESC-AMT  PIC Z(6).99.
               05  D-BRNO          PIC Z(4).
               05  FILLER          PIC X.
               05  D-DETAIL        PIC X(24).

           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 300.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 300 TIMES.
               05  PAGE-INITKEY    PIC X(20).
               05  PAGE-FMNO       PIC 9(4).
               05  PAGE-PRINT      PIC X.
       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(20) OCCURS 9 TIMES.



       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FMSCN2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/FMSCN2_DEF.CPY".
       COPY "LIBWI/FMSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/FMSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/FMSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME FMSCAN-BUF EXIT-PATHNAME.

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "XX"     TO FMSCAN-STATUS.
           MOVE "FMSCN2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 02
                   AT COL  12
                   SIZE 66 + VDU-WIN-W-MOD
                   LINES 15 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS FMSCN2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE HOLD-FMNO.
           PERFORM OPEN-FM1-FILE.
           MOVE 0 TO PAGE-FMNO(PAGE-COUNT).
           IF FIRST-TIME = "Y"
              MOVE FMSCAN-FM-INITKEY TO PAGE-INITKEY(PAGE-COUNT)
           ELSE
              MOVE SPACES TO PAGE-INITKEY(PAGE-COUNT).

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

           MOVE FM-PATH-OWNBR              TO FM-BRNO.
           MOVE PAGE-FMNO(CURRENT-PAGE)    TO FM-FMNO.
           MOVE PAGE-INITKEY(CURRENT-PAGE) TO FM-INITKEY.

           PERFORM START-FM1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           IF FMSCAN-FM-FMNO NOT = " "
              IF FMSCAN-FM-FMNO NOT = FM-FMNO
                 GO TO BUILD-PAGENO.

           IF FM-FMNO NOT = HOLD-FMNO
              PERFORM OPEN-FD1-FILE
              MOVE FM-FMNO TO FD-FMNO
              PERFORM READ-FD1-FILE
              IF IO-FG = 9
                 MOVE "INVALID FORM NO." TO FD-DESC.

           MOVE FM-FMNO    TO D-KEY(SUB).
           MOVE FM-PRINT   TO D-PRINT(SUB).
           MOVE FD-DESC    TO D-DESC(SUB).
           MOVE FM-BRNO    TO D-BRNO(SUB).
           MOVE FM-INITKEY TO SELECT-KEY(SUB).

           MOVE SPACES TO DETAIL-WORKER.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF LPWSFM-EDIT-FM-TYPE NOT NUMERIC
              GO TO FORMAT-CHECK-DETAIL.

       FORMAT-LOAN-DETAIL.
           PERFORM OPEN-LN1-FILE.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LN-SSNO(1).

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              MOVE "INVALID LOAN#" TO INSPECT-W
           ELSE
              PERFORM SETUP-BORROWER-NAME.

           MOVE LPWSFM-EDIT-FM-ACCTNO TO FORMAT-LOAN.
           STRING FORMAT-LOAN "; " INSPECT-W DELIMITED BY SIZE
              INTO DETAIL-WORKER.

           GO TO CONTINUE-BUILD-PAGENO.
       FORMAT-CHECK-DETAIL.

           PERFORM OPEN-CX1-FILE.
           MOVE LPWSFM-EDIT-FM-BRNO   TO CX-BRNO.
           MOVE LPWSFM-EDIT-FM-TYPE   TO CX-TYPE.
           MOVE LPWSFM-EDIT-FM-SEQNO  TO CX-SEQNO.
           PERFORM READ-CX1-FILE.
           PERFORM CLOSE-CX1-FILE.
           IF IO-FG = 9
              MOVE 0 TO CX-ACCTNO CX-AMT CX-BATCHID
              MOVE LPWSFM-EDIT-FM-BRNO      TO CX-BRNO
              MOVE LPWSFM-EDIT-FM-TYPE      TO CX-TYPE
              MOVE LPWSFM-EDIT-FM-SEQNO     TO CX-SEQNO
              MOVE FM-TRDATE         TO CX-DATE
              MOVE "--- PRINTED ---" TO CX-PAYEE.
           IF CX-BATCHID NOT = 0
              MOVE "BATCH" TO D-BATCHX(SUB)
              MOVE CX-BATCHID TO D-BATCHID(SUB).
           MOVE CX-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE CX-SEQNO     TO FORMAT-SEQNO.
           MOVE CX-ACCTNO    TO FORMAT-LOAN.
           IF CX-ACCTNO NOT = 0
              IF CX-BATCHID = 0
                 MOVE FORMAT-LOAN TO D-BATCHID(SUB).
           IF CX-AMT NOT = 0
                MOVE CX-AMT TO D-DESC-AMT(SUB)
                MOVE "$"    TO D-DESC-AMTX(SUB).
           STRING "CHECK-" FORMAT-SEQNO
              "TYPE-" CX-TYPE  DELIMITED BY SIZE
              INTO DETAIL-WORKER.
           MOVE DETAIL-WORKER TO D-CHECK-AREA(SUB).
           MOVE SPACES TO DETAIL-WORKER.
           STRING ALP-DATE " " CX-PAYEE
              DELIMITED BY SIZE INTO DETAIL-WORKER.

       CONTINUE-BUILD-PAGENO.
           MOVE DETAIL-WORKER TO D-DETAIL(SUB).
           MOVE FM-FMNO TO HOLD-FMNO.
           ADD 1 TO SUB.

           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE FM-FMNO TO PAGE-FMNO(PAGE-COUNT)
                    MOVE FM-INITKEY TO PAGE-INITKEY(PAGE-COUNT)
                    MOVE FM-PRINT   TO PAGE-PRINT(PAGE-COUNT)
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
           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
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
           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY          TO FMSCAN-KEY FMSCAN-FM-FMNO.
           MOVE SELECT-KEY(KEY-SUB) TO FMSCAN-FM-INITKEY.

           MOVE "00" TO FMSCAN-STATUS.

           GO TO END-ROUTINE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY FMSCN2-SCREEN UPON FMSCN2-WINDOW-HANDLE.
           MOVE FMSCN2-FORM-FIELDS TO WR-BUF.
           MOVE FMSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE FMSCN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE FMSCN2-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/FMSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       SETUP-BORROWER-NAME.
           MOVE BW-FNAME TO INSPECT-W.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+               " BY ADD-ON.
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPFM1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCX1RN.CPY".


       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FMSCN2-SCREEN.
           DESTROY FMSCN2-WINDOW-HANDLE.
           EXIT PROGRAM.

