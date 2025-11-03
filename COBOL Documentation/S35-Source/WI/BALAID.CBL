       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BALAID.
       DATE-WRITTEN.    082989.
      *****************************************************************
      *  DESC:  DAILY BALANCING AIDS
      *         CALLED FROM LP/LONPL0.C
      *
      * BAH 021094 ADDED EMLNRG
      * BAH 060394 ADDED EMPORG
      * BAH 050895 ADDED EMSFRG
      * BAH 060595 ADDED EMALRG (ALLOTMENT)
      * BAH 080995 ADDED EMCASH (CASH TRANSACTIONS)
      * MJD 081696 ADDED EMTRR2 (CASH DRAWER TRANSACTION JOURNAL)
      * BLV 990201 ADDED NPTRRG (NOTES PAYABLE TRANSACTION REGISTER)
      * MJD 140509 CHANGED WINDOW PROCESS TO WORK LIKE IT DOES IN PGMENU (F12)
      *            THIS RESOLVED ISSUES WITH SCREENS BLEEDING THROUGH
      *            REMOVED WINDOWS COPYBOOK.
      * MJD 140604 REMOVED TEMP WINDOW FROM F12, NO LONGER NEEDED.
      *            ADDED EXT- CONTROL WORKERS.
      *  CS 150209 ADDED EXIT-PATHNAME TO CALL LP/EMCKRG,LP/EMSFRG,LP/EMLNRG
      *            LP/EMCARG,LP/POFFRG,LP/EMALRG,LP/EMCASH,LP/EMTRR2,NP/NPTRRG
      *            BECAUSE GETTING LINKAGE ERRORS
      * MJD 170103 ADDED LOGIC TO TELL FLDSEL TO SEARCH COLUMNS 1-2
      * KEC 180222 ADDED 12. EMTRR3 (DEBIT BALANCING REPORT) WORLD #1305
      * BAH 180228 ADDED 13. DBREPT (DEBIT CARD NETSPEND REPORT) WORLD #1306
      * BAH 2020.1117 CHANGED THE CALL AND CANCEL TO HAVE THE PROGRAM NAME IN
      *            IT THE PROGRAMS WERE NOT GETTING CANCELED AND SECOND TIME YOU
      *            RAN THEM, VALUES WERE NOT INITIALIZED IN WORKING STORAGE.
      * BAH 2021.1123 CHANGED 13. DBREPT (DEBIT CARD PROCEEDS REPORT) #1527
      * JKC 2024.0131 ADDED TR DBREPT (DEBIT CARD PROCEEDS REPORT) #1527
      * JKC 2024.0124 ADDED OTIS TRACE LOGGING LOGIC (LIBCL/TRACE)
      *
      *****************************************************************
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
           "@(#)WI/BALAID.CBL S35 02/01/24-09:14:56 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************


       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS        REDEFINES ROUTE-BUF
                                   PIC XX.
       01  SUB                     PIC 9(6) VALUE 0.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER PIC X(28) VALUE "010 020 030 040 050 060 070 ".
           03  FILLER PIC X(24) VALUE "080 090 100 110 120 130.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(3).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "130.".

       01  DISPLAY-LIST.
           03  FILLER PIC X(28) VALUE "010 011 020 021 030 031 ".
           03  FILLER PIC X(28) VALUE "040 041 050 051 060 061 ".
           03  FILLER PIC X(28) VALUE "070 071 080 081 090 091 ".
           03  FILLER PIC X(28) VALUE "100 101 110 111 120 121 ".
           03  FILLER PIC X(28) VALUE "130 131.".

       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 13 TIMES.
               05  FILLER          PIC X(38).

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BALAID_DEF.CPY".
       COPY "LIBWI/BALAID_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  BALAID-WINDOW-HANDLE         PIC X(10).

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BALAID_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BALAID" TO VDU-FORM-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "BALAID"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/CHKSEC.CPY".
           PERFORM SETUP-WINDOW.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE " F7-EXIT, F6-VIEW, SELECT OPTION(RTN):"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
      * TELL FLDSEL TO LOOK AT COLUMN 1-2 WHEN SERCHING.
           MOVE "L" TO FSEL-SEARCH-TYPE.
           MOVE 1 TO FSEL-SEARCH-COL-BEGIN.
           MOVE 2 TO FSEL-SEARCH-COL-END.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1<"
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "GB/PERUSE" TO FORM-NAM
              CALL "PERUSE" USING FORM-PATH EXIT-PATHNAME
              CANCEL "PERUSE"
              PERFORM SETUP-WINDOW
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.

           IF NOT (FLDSEL-STAT = "00")
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TRANSACTION REGISTER
           IF FLDSEL-KEY = " 1."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMTRRG" TO FORM-NAM
              CALL "EMTRRG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMTRRG"
              PERFORM SETUP-WINDOW
           ELSE
      * LOAN REGISTER
           IF FLDSEL-KEY = " 2."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMLNRG" TO FORM-NAM
              CALL "EMLNRG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMLNRG"
              PERFORM SETUP-WINDOW
           ELSE
      * SALES REGISTER
           IF FLDSEL-KEY = " 3."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMSFRG" TO FORM-NAM
              CALL "EMSFRG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMSFRG"
              PERFORM SETUP-WINDOW
           ELSE
      * CASH RECEIPT/DISBURSMENT REGISTER
           IF FLDSEL-KEY = " 4."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMCARG" TO FORM-NAM
              CALL "EMCARG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMCARG"
              PERFORM SETUP-WINDOW
           ELSE
      * CHECK REGISTER
           IF FLDSEL-KEY = " 5."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMCKRG" TO FORM-NAM
              CALL "EMCKRG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMCKRG"
              PERFORM SETUP-WINDOW
           ELSE
      * DAILY RECAP JOURNAL
           IF FLDSEL-KEY = " 6."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/RECAPJ" TO FORM-NAM
              CALL "RECAPJ" USING FORM-PATH
              CANCEL "RECAPJ"
              PERFORM SETUP-WINDOW
           ELSE
      * PAYOFF REGISTER
           IF FLDSEL-KEY = " 7."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMPORG" TO FORM-NAM
              CALL "EMPORG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMPORG"
              PERFORM SETUP-WINDOW
           ELSE
      * ALLOTMENT REGISTER
           IF FLDSEL-KEY = " 8."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMALRG" TO FORM-NAM
              CALL "EMALRG" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMALRG"
              PERFORM SETUP-WINDOW
           ELSE
      * CASH TRANSACTION REGISTER
           IF FLDSEL-KEY = " 9."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMCASH" TO FORM-NAM
              CALL "EMCASH" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMCASH"
              PERFORM SETUP-WINDOW
           ELSE
      * CASH DRAWER TRANSACTION JOURNAL
           IF FLDSEL-KEY = "10."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMTRR2" TO FORM-NAM
              CALL "EMTRR2" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMTRR2"
              PERFORM SETUP-WINDOW
           ELSE
      * NOTES PAYABLE TRANSACTION REGISTER
           IF FLDSEL-KEY = "11."
              GO TO SELECT-FIELD-FROM-DISPLAY
           ELSE
      * DEBIT BALANCING REPORT
           IF FLDSEL-KEY = "12."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/EMTRR3" TO FORM-NAM
              CALL "EMTRR3" USING FORM-PATH EXIT-PATHNAME
              CANCEL "EMTRR3"
              PERFORM SETUP-WINDOW
           ELSE
      * DEBIT CARD NETSPEND REPORT
           IF FLDSEL-KEY = "13."
              CLOSE WINDOW BALAID-WINDOW-HANDLE
              MOVE "LP/DBREPT" TO FORM-NAM
              CALL "DBREPT" USING FORM-PATH EXIT-PATHNAME
              CANCEL "DBREPT"
              PERFORM SETUP-WINDOW.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY BALAID-SCREEN UPON BALAID-WINDOW-HANDLE.
           MOVE BALAID-FORM-FIELDS TO WR-BUF.
           MOVE BALAID-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE BALAID-HELP--FILE TO HELP-LINK-FILE.
           MOVE BALAID-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BALAID_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       SETUP-WINDOW SECTION.
      *************************************
           MOVE "BALAID" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 03 * VDU-WIN-L-OFFSET
                   AT COL 20 * VDU-WIN-C-OFFSET
                   SIZE 42 + VDU-WIN-W-MOD
                   LINES 19 + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-FWIN
                   CONTROL VALUE IS VDU-WCTRL-FWIN
                   BLANK SCREEN
                   HANDLE IS BALAID-WINDOW-HANDLE.

           PERFORM FORM-RESET.
           PERFORM BUILD-PAGENO.
           PERFORM DISPLAY-PAGE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO SECTION.
           MOVE SPACES TO DISPLAY-BUF.

           MOVE " 1. TRANSACTION REGISTER"              TO D-LINE(1).
           MOVE " 2. LOAN REGISTER"                     TO D-LINE(2).
           MOVE " 3. SALES REGISTER"                    TO D-LINE(3).
           MOVE " 4. CASH RECEIPT/DISBURSMENT REGISTER" TO D-LINE(4).
           MOVE " 5. CHECK REGISTER"                    TO D-LINE(5).
           MOVE " 6. DAILY RECAP JOURNAL"               TO D-LINE(6).
           MOVE " 7. PAYOFF REGISTER"                   TO D-LINE(7).
           MOVE " 8. ALLOTMENT REGISTER"                TO D-LINE(8).
           MOVE " 9. CASH TRANSACTION REGISTER"         TO D-LINE(9).
           MOVE "10. CASH DRAWER TRANSACTION REGISTER"  TO D-LINE(10).
           MOVE "11. N/A"                               TO D-LINE(11).
           MOVE "12. DEBIT BALANCING REPORT"            TO D-LINE(12).
           MOVE "13. DEBIT CARD PROCEEDS REPORT"        TO D-LINE(13).

      *************************************
       DISPLAY-PAGE SECTION.
      *************************************
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       SUSPEND.
      *    CALL SUSP.
           CALL "C$SLEEP" USING 3.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ERROR SECTION.
      ********************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

       END-ROUTINE.
       EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "BALAID"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BALAID-SCREEN.
           DESTROY BALAID-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
