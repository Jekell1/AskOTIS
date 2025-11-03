       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LESCAN.
       DATE-WRITTEN.    122391.
      *****************************************************
      *          WINDOW LOAN (GEORGIA) RESTRICTIONS
      *              CALLED FROM WI/GAINQY
      *
      * REV:
      *  101794 BAH REMOVED ALL OCCURANCES OF LP2
      *  053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      *  111095 BAH REMOVE WORK FILE
      *  160511  CS REMOVED REDEF LE-FLAG/LE-SSNO MADE SEPARATE FIELDS
      *  BAH 170418 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181030 GLOBAL LEFILE
      *  BAH 190417 WKR-NUMBER AND WKO-NUMBER WERE BEING DIVIDED BY 100
      *             TO REMOVE THE 01 ON THE END FROM A15, BUT CAUSED
      *             TRUNCATION IN A30, QA0006
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      **************************************************************
      *  WORK FILE FOR PRINT DETAIL
      ****************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-SEQNO        PIC 9(5).
           03  WKR-AREA.
               05  FILLER          PIC X.
               05  WKR-NUMBER      PIC 9(6).
               05  FILLER          PIC XX.
               05  WKR-LOANDATE    PIC 99/99/99.
               05  FILLER          PIC XX.
               05  WKR-LNAMT       PIC Z(5).99-.
               05  FILLER          PIC XX.
               05  WKR-CURBAL      PIC Z(5).99-.
               05  FILLER          PIC XX.
               05  WKR-POFFDATE    PIC 99/99/99  BLANK ZERO.
               05  FILLER          PIC X(7).
               05  WKR-TYPE        PIC X(4).
               05  FILLER          PIC X(8).
               05  WKR-RESTRICT    PIC Z(5).99-  BLANK ZERO.
               05  FILLER          PIC X.
           03  WKO-AREA           REDEFINES WKR-AREA.
               05  FILLER          PIC X.
               05  WKO-MAKER-CD    PIC X(7).
               05  FILLER          PIC XXX.
               05  WKO-NAME        PIC X(20).
               05  FILLER          PIC XXXX.
               05  WKO-NUMBER      PIC 9(6).
               05  FILLER          PIC XXX.
               05  WKO-LOANDATE    PIC 99/99/99.
               05  FILLER          PIC XX.
               05  WKO-OSAMT       PIC Z(5).99-.
               05  FILLER          PIC XX.
               05  WKO-MKR         PIC X(3).
               05  FILLER          PIC X.
               05  WKO-OBLIG       PIC Z(5).99-  BLANK ZERO.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/LESCAN.CBL S34 05/24/21-10:01:29 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       01  WK-IFN              PIC X(2)       VALUE "WK".

       01  LESCAN-LINES                PIC 99   VALUE 13.
       01  LESCAN-COLUMNS              PIC 99   VALUE 78.
       01  LESCAN-BEGIN-Y              PIC 99   VALUE 2.
       01  LESCAN-BEGIN-X              PIC 99   VALUE 5.

       01  ERRCD               PIC X          VALUE " ".
       01  HOLD-KEY-DOWN       PIC X(5)       VALUE SPACES.
       01  HOLD-KEY-UP         PIC X(5)       VALUE SPACES.
       01  IO-TYPE             PIC X(3)       VALUE SPACES.
       01  FULL-PAGE           PIC X          VALUE " ".
       01  ENTRY-FLAG          PIC 9          VALUE 0.
       01  ENDORSER-PRINTED    PIC X          VALUE " ".
       01  LSUB                PIC 9(3) COMP  VALUE 0.
       01  OSUB                PIC 9(3) COMP  VALUE 0.
       01  TOT-ENDOR-OBLIG     PIC S9(7)V99   COMP.

      **************************************************
      *    HEADER BUFFER
      **************************************************
       01  HEAD-LIST           PIC X(22)    VALUE
           "TITLE HEAD1:S HEAD2:S.".
       01  HEAD-R-BUF.
           03                  PIC X(40)    VALUE
           "    LOANS CONSIDERED FOR RESTRICTION    ".
           03  FILLER          PIC X(39)    VALUE
           "                                       ".
           03  FILLER          PIC X(39)    VALUE
           "                --- RESTRICTIONS ---   ".
           03  FILLER          PIC X(39)    VALUE
           " ACCTNO  LOANDATE  LOAN AMT    BALANCE ".
           03  FILLER          PIC X(39)    VALUE
           "  PAIDOFF        TYPE           MONEY  ".
       01  HEAD-O-BUF.
           03                  PIC X(40)    VALUE
           "   MAKER OBLIGATIONS AT LOAN BOOKING   ".
           03  FILLER          PIC X(39)    VALUE
           "                                       ".
           03  FILLER          PIC X(39)    VALUE
           "                               RUNNING ".
           03  FILLER          PIC X(39)    VALUE
           "           NAME                    ACCT".
           03  FILLER          PIC X(39)    VALUE
           "NO   LOANDATE  LOAN AMT   M      OBLIG ".

      **************************************************
      *    LINE BUFFER
      **************************************************
       01  LINE-LIST.
           03  FILLER          PIC X(32)    VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08.".
       01  LINE-BUFFER         VALUE SPACES.
           03  D-LINE          OCCURS 8
                               PIC X(78).
       01  L-BUF               PIC X(78) VALUE SPACES.

       01  LEGEND              PIC X(70).
       01  LESCAN-LEGEND1.
           03  FILLER                   PIC X(40) VALUE
           " F7-EXIT, F4-RESTRICT, F5-OBLIG, SCROLL ".
           03  FILLER                   PIC X(15) VALUE
           "(UP)-^, (DOWN)-".
           03  FILLER                   PIC X     VALUE X'76'.
           03  FILLER                   PIC X     VALUE ":".

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       01  LESCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LESCAN_DEF.CPY".
       COPY "LIBWI/LESCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LESCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LESCAN_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME LESCAN-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-BW1-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "LESCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE LESCAN-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  LESCAN-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   SIZE    LESCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   LESCAN-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LESCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.
      * CREATE WORK FILE TO HOLD (SCREEN) REPORT LINES:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           PERFORM SET-HEADER.
           PERFORM BUILD-REPORT-LINES.

           MOVE "Y" TO FULL-PAGE.
           MOVE 0 TO WK-SEQNO.
           MOVE WK-KEY TO HOLD-KEY-DOWN.
           MOVE 99999 TO WK-KEY
                         HOLD-KEY-UP.

           PERFORM DISPLAY-UP.

           PERFORM SELECTION-ROUTINE.
           IF IO-TYPE = "NXT"
              GO TO MAIN-MODULE.

           GO TO END-ROUTINE.

      ******************************************
       SELECTION-ROUTINE SECTION.
      ******************************************
           MOVE LESCAN-LEGEND1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "18"
              IF LESCAN-TYPE = "O"
                 MOVE "R" TO LESCAN-TYPE
                 MOVE "NXT" TO IO-TYPE
                 GO TO SELECTION-ROUTINE-EXIT.
           IF VDUSTATUS = "1:"
              IF LESCAN-TYPE = "R"
                 MOVE "O" TO LESCAN-TYPE
                 MOVE "NXT" TO IO-TYPE
                 GO TO SELECTION-ROUTINE-EXIT.

           IF VDUSTATUS = "1U"
              PERFORM DISPLAY-UP
              GO TO SELECTION-ROUTINE.
           IF VDUSTATUS = "1D"
              PERFORM DISPLAY-DOWN
              GO TO SELECTION-ROUTINE.
           IF VDUSTATUS NOT = "1>"
              GO TO SELECTION-ROUTINE.
           MOVE "END" TO IO-TYPE.

       SELECTION-ROUTINE-EXIT.
           EXIT.

      **************************************************
       DISPLAY-DOWN SECTION.
      **************************************************
           MOVE " " TO LINE-BUFFER L-BUF.
           MOVE 0 TO LSUB.
           MOVE HOLD-KEY-DOWN TO WK-KEY.
           IF WK-SEQNO = 99999
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-EXIT.
           MOVE WK-KEY TO HOLD-KEY-UP.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM START-WK-FILE.
           MOVE 0 TO ENTRY-FLAG.
       DISPLAY-DOWN-NEXT.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-BYPASS.
           PERFORM READ-WK-FILE-NEXT.
       DISPLAY-DOWN-BYPASS.
           ADD 1 TO LSUB.
           IF IO-FG = 0
              PERFORM SETUP-LINE.
           MOVE L-BUF TO D-LINE(LSUB).
           MOVE SPACES TO L-BUF.
           IF LSUB = 8
              GO TO DISPLAY-DOWN-XFER.
           IF IO-FG = 0
              GO TO DISPLAY-DOWN-NEXT.
           IF FULL-PAGE = "Y"
              MOVE "N" TO FULL-PAGE
              MOVE 99999 TO WK-SEQNO
              MOVE WK-KEY TO HOLD-KEY-UP
              PERFORM DISPLAY-UP
              GO TO DISPLAY-DOWN-EXIT.
       DISPLAY-DOWN-XFER.
           PERFORM DISPLAY-ALL-LINES.

       DISPLAY-DOWN-EXIT.
           IF IO-FG = 0
              MOVE "Y" TO FULL-PAGE
              MOVE WK-KEY TO HOLD-KEY-DOWN
           ELSE
              MOVE 99999 TO WK-SEQNO
              MOVE WK-KEY TO HOLD-KEY-DOWN.
           PERFORM CLOSE-WK-FILE.

      ********************************************
       DISPLAY-UP SECTION.
      ********************************************
           MOVE " " TO LINE-BUFFER L-BUF.
           MOVE 9 TO LSUB.
           MOVE HOLD-KEY-UP TO WK-KEY.
           IF WK-SEQNO = 0
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.
           MOVE WK-KEY TO HOLD-KEY-DOWN.
           PERFORM OPEN-WK-FILE.
           PERFORM START-WK-FILE-LE.
           MOVE 0 TO ENTRY-FLAG.
       DISPLAY-UP-NEXT.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-BYPASS.
           PERFORM READ-WK-FILE-PREVIOUS.
       DISPLAY-UP-BYPASS.
           SUBTRACT 1 FROM LSUB.
           IF LSUB < 1
              MOVE "Y" TO FULL-PAGE
              GO TO DISPLAY-UP-XFER.
           IF IO-FG = 0
              PERFORM SETUP-LINE.
           MOVE L-BUF TO D-LINE(LSUB).
           MOVE SPACES TO L-BUF.
           IF IO-FG = 0
              GO TO DISPLAY-UP-NEXT.
           IF FULL-PAGE = "Y"
              MOVE "N" TO FULL-PAGE
              MOVE 0 TO WK-SEQNO
              MOVE WK-KEY TO HOLD-KEY-DOWN
              PERFORM DISPLAY-DOWN
              GO TO DISPLAY-UP-EXIT.
       DISPLAY-UP-XFER.
           PERFORM DISPLAY-ALL-LINES.

       DISPLAY-UP-EXIT.
           IF IO-FG = 0
              MOVE "Y" TO FULL-PAGE
              MOVE WK-KEY TO HOLD-KEY-UP
           ELSE
              MOVE 0 TO WK-SEQNO
              MOVE WK-KEY TO HOLD-KEY-UP.
           PERFORM CLOSE-WK-FILE.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           IF LESCAN-TYPE = "R"
              MOVE WKR-AREA TO L-BUF
           ELSE
              MOVE WKO-AREA TO L-BUF.

      ******************************************
       BUILD-REPORT-LINES SECTION.
      ******************************************
           PERFORM OPEN-WK-FILE.
           MOVE 1 TO WK-SEQNO.

           IF LESCAN-TYPE = "R"
              PERFORM GET-RESTRICT-REC
           ELSE
              PERFORM GET-OBLIG-REC.
           PERFORM CLOSE-WK-FILE.

       BUILD-REPORT-LINES-EXIT.
           EXIT.

      ************************************************
       GET-RESTRICT-REC SECTION.
      ************************************************

           PERFORM OPEN-LE1-FILE.

           MOVE LESCAN-BRNO   TO LE-BRNO.                 
           MOVE LESCAN-ACCTNO TO LE-ACCTNO.
           MOVE "R"           TO LE-TYPE.
           MOVE "D"           TO LE-FLAG.
           MOVE 0             TO LE-SSNO.
           MOVE 0             TO LE-OSACCTNO.

           PERFORM START-LE1-FILE.
       GET-RESTRICT-REC-NEXT.
           PERFORM READ-LE1-FILE-NEXT.
           IF IO-FG = 9 OR (LE-BRNO NOT = LESCAN-BRNO)    
                 OR (LE-ACCTNO NOT = LESCAN-ACCTNO)
              GO TO GET-RESTRICT-REC-EXIT.

           IF LE-TYPE NOT = "R"
              GO TO GET-RESTRICT-REC-NEXT.

           IF LE-R-SALEFIN = "Y"
              GO TO GET-RESTRICT-REC-NEXT.

           MOVE " " TO WKR-AREA.
           IF LE-FLAG = "P"
              MOVE "PAID" TO WKR-TYPE
           ELSE
              IF LE-FLAG = "R"
                 MOVE "RNWL" TO WKR-TYPE
              ELSE
                 IF LE-FLAG = "D"
                    IF LE-R-RESTRICTED-MONEY = 0
                       MOVE " " TO WKR-TYPE
                    ELSE
                       MOVE "DUAL" TO WKR-TYPE.

           MOVE LE-OSACCTNO   TO WKR-NUMBER.
           MOVE LE-R-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO WKR-LOANDATE.
           MOVE LE-R-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO WKR-POFFDATE.
           MOVE LE-R-LNAMT    TO WKR-LNAMT.
           MOVE LE-R-RESTRICTED-MONEY TO WKR-RESTRICT.
           MOVE LE-R-BALANCE  TO WKR-CURBAL.
           PERFORM WRITE-WK-FILE.

           ADD 1 TO WK-SEQNO.
           GO TO GET-RESTRICT-REC-NEXT.

       GET-RESTRICT-REC-EXIT.
           PERFORM CLOSE-LE1-FILE.

      ************************************************
       GET-OBLIG-REC SECTION.
      ************************************************
           MOVE 0 TO OSUB.
       GET-OBLIG-REC-NEXT.
           ADD 1 TO OSUB.
           IF OSUB > 4
              GO TO GET-OBLIG-REC-EXIT.

           MOVE " " TO WKO-AREA.
           PERFORM GET-BORROWER.
           IF IO-FG = 9
              GO TO GET-OBLIG-REC-EXIT.

           PERFORM GET-OBLIG-ENDOR.
           IF ENDORSER-PRINTED = "Y"
              GO TO GET-OBLIG-REC-NEXT.
           MOVE TOT-ENDOR-OBLIG TO WKO-OBLIG.
           PERFORM WRITE-WK-FILE.
           ADD 1 TO WK-SEQNO.
           GO TO GET-OBLIG-REC-NEXT.

       GET-OBLIG-REC-EXIT.
           PERFORM CLOSE-LE1-FILE.

      ************************************************
       GET-BORROWER SECTION.
      ************************************************
           PERFORM OPEN-BW1-FILE.
           MOVE 9 TO IO-FG.
           IF LESCAN-SSNO(OSUB) = 0
              GO TO GET-BORROWER-EXIT.

           MOVE LESCAN-SSNO(OSUB) TO BW-SSNO.
           IF LESCAN-MAKERCD(OSUB) = "M"
              MOVE "(MAKER)" TO WKO-MAKER-CD
           ELSE
           IF LESCAN-MAKERCD(OSUB) = "J"
              MOVE "(JOINT)" TO WKO-MAKER-CD
           ELSE
           IF LESCAN-MAKERCD(OSUB) = "E"
              MOVE "(ENDOR)" TO WKO-MAKER-CD.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO WKO-NAME
              MOVE 0 TO IO-FG
              GO TO GET-BORROWER-EXIT.
           MOVE SPACES TO WKO-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO WKO-NAME.
           MOVE LESCAN-LNAMT TO TOT-ENDOR-OBLIG.

       GET-BORROWER-EXIT.
           PERFORM CLOSE-BW1-FILE.

      ******************************************
       GET-OBLIG-ENDOR SECTION.
      ******************************************
           PERFORM OPEN-LE1-FILE.

           MOVE "N" TO ENDORSER-PRINTED.
           MOVE LESCAN-BRNO   TO LE-BRNO.             
           MOVE LESCAN-ACCTNO TO LE-ACCTNO.
           MOVE "E"           TO LE-TYPE.
           MOVE SPACES        TO LE-FLAG.
           MOVE BW-SSNO       TO LE-SSNO.
           MOVE ZEROS         TO LE-OSACCTNO.


           PERFORM START-LE1-FILE.
       GET-OBLIG-ENDOR-NEXT.
           PERFORM READ-LE1-FILE-NEXT.
           IF IO-FG = 9 OR (LE-BRNO NOT = LESCAN-BRNO)  
              GO TO GET-OBLIG-ENDOR-EXIT.

           IF LE-ACCTNO NOT = LESCAN-ACCTNO
              GO TO GET-OBLIG-ENDOR-EXIT.

           IF LE-TYPE NOT = "E"
              GO TO GET-OBLIG-ENDOR-NEXT.

           IF LE-SSNO NOT = BW-SSNO
              GO TO GET-OBLIG-ENDOR-NEXT.

           MOVE LE-OSACCTNO     TO WKO-NUMBER.
           MOVE LE-E-OSDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO WKO-LOANDATE.
           MOVE LE-E-OSAMT      TO WKO-OSAMT.
           ADD LE-E-OSAMT       TO TOT-ENDOR-OBLIG.
           MOVE LE-E-OSMAKERCD  TO WKO-MKR.
           MOVE TOT-ENDOR-OBLIG TO WKO-OBLIG.
           PERFORM WRITE-WK-FILE.

           ADD 1 TO WK-SEQNO.
           MOVE " " TO WKO-AREA.
           MOVE "Y" TO ENDORSER-PRINTED.
           GO TO GET-OBLIG-ENDOR-NEXT.

       GET-OBLIG-ENDOR-EXIT.
           PERFORM CLOSE-LE1-FILE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LESCAN-SCREEN UPON LESCAN-WINDOW-HANDLE.
           MOVE LESCAN-FORM-FIELDS TO WR-BUF.
           MOVE LESCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LESCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LESCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LESCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE "WSEL:D." TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-ALL-LINES.
           MOVE LINE-BUFFER TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SET-HEADER.
           IF LESCAN-TYPE = "R"
              MOVE HEAD-R-BUF TO WR-BUF
              MOVE HEAD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE HEAD-O-BUF TO WR-BUF
              MOVE HEAD-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLE1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LESCAN-SCREEN.
           DESTROY LESCAN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
