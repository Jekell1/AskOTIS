       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AQSCAN.
      **********************************************************
      *  AUDITOR QUEUE FILE  WINDOW SCAN
      *    CALLED FROM LP/AUDIQ0
      *
      * REV:
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *  BLV 032697 IF EXTRACTION HAS NOT BEEN RUN, SEND MESSAGE
      *             AND SET LINK-STATUS BAD; DON'T GO TO IO-ERROR
      *  BLF 070921 ADDED LOGIC TO READ LN FROM ARCHIVE FILES TO
      *             INCLUDE ARCHIVED LOANS IN INQUIRY, WORLD PR# 547
      * BAH 181218 COMMENTED OUT ARCHIVE
      *  AMM 190124 L2G CHANGES AQ FILE
      **********************************************************
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
           "@(#)WI/AQSCAN S35 07/24/24-10:57:03 >".

       COPY "LIBSP/SP01AQ.CPY".
       COPY "LIBSP/SP01AQ_SQL.CPY".
       COPY "LIBSP/SPWSAQ.CPY".
       COPY "LIBSP/SP01AQS.CPY".
       COPY "LIBSP/SP01AQS_SQL.CPY".
       COPY "LIBSP/SPWSAQS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  AQSCAN-LINES              PIC 99  VALUE 10.
       01  AQSCAN-COLUMNS            PIC 99  VALUE 29.
       01  AQSCAN-BEGIN-Y            PIC 99  VALUE 5.
       01  AQSCAN-BEGIN-X            PIC 99  VALUE 5.
       01  ERRCD                   PIC X      VALUE SPACES.

       01  SCANTYPE                PIC X.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  DISPLAY-AQSCAN-LIST.
           03  FILLER              PIC X(16)  VALUE
           "010 020 030 040.".
       01  DISPLAY-AQSCAN-BUF.
           03  D-AQS-TOT-REC        PIC ZZZZ9.
           03  D-AQS-WORKED         PIC ZZZZ9.
           03  D-AQS-TO-WORK        PIC ZZZZ9.
           03  D-AQS-NEXT-SEQNO     PIC ZZZZ9.

       01  A-NAME                  PIC X(16).

       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-SEQNO         PIC ZZZZ9.
           03  FILLER               PIC XX.
           03  FLDSEL-ACCTNO        PIC X(16).
           03  FILLER               REDEFINES FLDSEL-ACCTNO.
               05  FILLER           PIC X(3).
               05  FLDSEL-LN-ACCTNO PIC 9(6).
           03  FILLER               PIC XX.
           03  FLDSEL-NAME          PIC X(24).
           03  FILLER               PIC X.
           03  FLDSEL-LOANDATE      PIC 99/99/99.
           03  FILLER               PIC X(3).
           03  FLDSEL-WORKED-FG     PIC X.

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-SEQNO         PIC ZZZZ9.
               05  FILLER          PIC X.
               05  D-ARCH-FG       PIC X.
               05  D-LN-ACCTNO     PIC X(16).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(24).
               05  FILLER          PIC X.
               05  D-LOANDATE      PIC 99/99/99.
               05  FILLER          PIC X(3).
               05  D-WORKED-FG     PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 50.
       01  PAGE-KEY-01.
           03  PAGE-KEY            OCCURS 50 TIMES.
               05  PAGE-SEQNO      PIC 9(6).
               05  PAGE-LOANDATE   PIC 9(8).
               05  PAGE-BRNO       PIC 9(4).
               05  PAGE-ACCTNO     PIC 9(8).


       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  AQSCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/AQSCAN_DEF.CPY".
       COPY "LIBWI/AQSCN2_DEF.CPY".
       COPY "LIBWI/AQSCAN_WKS.CPY".
       COPY "LIBWI/AQSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/AQSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/AQSCAN_SCN.CPY".
       COPY "LIBWI/AQSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME AQSCAN-BUF EXIT-PATHNAME.

      *******************************
       MAIN-PROGRAM SECTION.
      *******************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE "XX" TO AQSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "AQSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE AQSCAN-BEGIN-Y
                   AT COL  AQSCAN-BEGIN-X
                   SIZE    AQSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   AQSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS AQSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE AQSCAN-BRANCH TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM OPEN-AQS1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE AQSCAN-BRANCH TO AQS-BRNO.
           PERFORM READ-AQS1-FILE.
           IF IO-FG = 9
              MOVE"EXTRACTION HAS NOT BEEN RUN" TO MESS
              PERFORM SEND-MESS
              MOVE "09" TO FLDSEL-STAT
              GO TO END-ROUTINE.

           MOVE AQS-TOT-REC TO D-AQS-TOT-REC.
           MOVE AQS-WORKED-REC TO D-AQS-WORKED.
           SUBTRACT AQS-WORKED-REC FROM AQS-TOT-REC
                                  GIVING D-AQS-TO-WORK.
           MOVE AQS-NEXT-SEQNO TO D-AQS-NEXT-SEQNO.
           MOVE DISPLAY-AQSCAN-BUF TO WR-BUF.
           MOVE DISPLAY-AQSCAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "9" TO FLDSEL-STAT.
       ENTER-SEL.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM MORE-INFO
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "1>"
              GO TO ENTER-SEL.

           GO TO END-ROUTINE.

      *************************************
       MORE-INFO SECTION.
      *************************************
           MOVE 17          TO WINDOW-LINES.
           MOVE 66          TO WINDOW-COLUMNS.
           MOVE 6           TO WINDOW-BEGIN-Y.
           MOVE 14          TO WINDOW-BEGIN-X.
           MOVE "AQSCN2"    TO VDU-FORM-NAME.
           MOVE "WI/AQSCN2" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY AQSCN2-SCREEN.

           MOVE AQSCN2-FORM-FIELDS TO WR-BUF.
           MOVE AQSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       MORE-INFO-BEGIN.
           MOVE "U" TO SCANTYPE.
           GO TO ENTER-SCANTYPE-CONT.

      *************************************
       ENTER-SCANTYPE.
      *************************************
           MOVE "F7-EXIT, F1-TOGGLE (WORK/UNWORK):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MORE-INFO-CLOSE-WINDOW.
           IF VDUSTATUS = "10"
              IF SCANTYPE = "W"
                 MOVE "U" TO SCANTYPE
              ELSE
                 MOVE "W" TO SCANTYPE
              END-IF
              GO TO ENTER-SCANTYPE-CONT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANTYPE.

       ENTER-SCANTYPE-CONT.
           MOVE "S  A024DESC." TO VDU.
           IF SCANTYPE = "U"
              MOVE "OF UNWORKED ACCOUNTS" TO VDUBUF
           ELSE
              MOVE "OF WORKED ACCOUNTS" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-AQ2-FILE.
           MOVE 1 TO PAGE-SEQNO(PAGE-COUNT).
           MOVE 0 TO PAGE-LOANDATE(PAGE-COUNT)
                     PAGE-BRNO(PAGE-COUNT)
                     PAGE-ACCTNO(PAGE-COUNT).

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
           MOVE AQ-PATH-OWNBR               TO AQ-BRNO.
           MOVE PAGE-SEQNO(CURRENT-PAGE)    TO AQ-SEQNO.
           MOVE PAGE-LOANDATE(CURRENT-PAGE) TO AQ-LOANDATE.
           IF AQ-LOANDATE = 0
              MOVE 19000101                 TO AQ-LOANDATE.
           MOVE PAGE-BRNO(CURRENT-PAGE)     TO AQ-BRNO.
           MOVE PAGE-ACCTNO(CURRENT-PAGE)   TO AQ-ACCTNO.

           PERFORM START-AQ2-FILE.

      *************************************
       BUILD-PAGENO.
      *************************************
           PERFORM READ-AQ2-FILE-NEXT.
           IF IO-FG NOT = 0 OR AQ-BRNO NOT = AQ-PATH-OWNBR
              GO TO DISPLAY-PAGE.
           IF SCANTYPE = "U"
              IF AQ-WORKED-FG = "Y"
                 GO TO BUILD-PAGENO
              ELSE
                 NEXT SENTENCE
           ELSE
              IF SCANTYPE = "W"
                 IF AQ-WORKED-FG NOT = "Y"
                    GO TO BUILD-PAGENO.
           IF SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE AQ-SEQNO TO D-SEQNO(SUB).
           MOVE AQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.
           IF IO-FG = 9
              MOVE "<NOT ON FILE>" TO D-NAME(SUB)
           ELSE
              PERFORM GET-SP
              IF IO-FG = 9
                 GO TO IO-ERROR
              ELSE
                 MOVE LN-SSNO(1) TO BW-SSNO
                 PERFORM GET-BW
                 IF IO-FG = 9
                    MOVE "<NOT ON FILE>" TO D-NAME(SUB)
                 ELSE
                    MOVE BW-LNAME TO MESS
                    MOVE BW-FNAME TO A-NAME
                    PERFORM SET-AQ-LNAME
                    MOVE MESS TO D-NAME(SUB)
                 END-IF
                 PERFORM LPSTAT-DISPLAY
                 MOVE LPS-DISPLAY-LNNO TO D-LN-ACCTNO(SUB).

           MOVE AQ-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE(SUB).
           MOVE AQ-WORKED-FG TO D-WORKED-FG(SUB).

       BUILD-PAGE-CONT.
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
                 GO TO ENTER-SCANTYPE.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE AQ-SEQNO    TO PAGE-SEQNO(PAGE-COUNT)
                    MOVE AQ-LOANDATE TO PAGE-LOANDATE(PAGE-COUNT)
                    MOVE AQ-BRNO     TO PAGE-BRNO(PAGE-COUNT)
                    MOVE AQ-ACCTNO   TO PAGE-ACCTNO(PAGE-COUNT)
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
           MOVE " F1, SELECT A LOAN:" TO MESS.
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
              GO TO MORE-INFO-CLOSE-WINDOW.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANTYPE.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-NAME = "<NOT ON FILE>"
              MOVE"INVALID SELECTION" TO MESS
              PERFORM SEND-MESS
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-LN-ACCTNO TO AQSCAN-ACCTNO.
           MOVE "00"             TO AQSCAN-STATUS.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY AQSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "AQSCAN" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           EXIT.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY AQSCAN-SCREEN UPON AQSCAN-WINDOW-HANDLE.
           MOVE AQSCAN-FORM-FIELDS TO WR-BUF.
           MOVE AQSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE AQSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE AQSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/AQSCAN_EVA.CPY".
              COPY "LIBWI/AQSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SET-AQ-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE "  " TO D-ARCH-FG(SUB).
       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-AQS1-FILE.
           PERFORM CLOSE-AQ2-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPAQSGS_SQL.CPY".
       COPY "LIBSP/SPAQGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPAQ2RN.CPY".
       COPY "LIBSP/SPAQS1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY AQSCAN-SCREEN.
           DESTROY AQSCAN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
