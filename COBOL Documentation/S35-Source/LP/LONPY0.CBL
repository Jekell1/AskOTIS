       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPY0.
       DATE-WRITTEN.    080191.
      *****************************************************************
      *                 (LP)
      *  DESCRIPTION:   COLLATERAL ID INQUIRY (FROM LT FILE)
      *  REV:
      *
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      * BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 181212 GLOBAL IXFILE
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
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
           "@(#)LP/LONPY0.CBL S35 02/20/24-15:24:23 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".

       01  SCREEN-TABLE.
           03  SCREEN-LINE-1          OCCURS 30 TIMES.
               05  D-ID               PIC X(20).
               05  FILLER             PIC X.
               05  D-ACCT             PIC X(16).
               05  FILLER             PIC X.
               05  D-NAME             PIC X(21).
               05  FILLER             PIC X.
               05  D-DATE             PIC 99/99/99.
               05  FILLER             PIC X.
               05  D-BAL              PIC ZZZZ,ZZ9.99.
           03  SCREEN-LINE-2          OCCURS 30 TIMES.
               05  FILLER             PIC X(21).
               05  D-DESCRIPTION      PIC X(10).
               05  FILLER             PIC X.
               05  D-MAKE             PIC X(15).
               05  FILLER             PIC X.
               05  D-MODEL            PIC X(15).
               05  FILLER             PIC X.
               05  D-YEAR             PIC X(4).
               05  FILLER             PIC X.
               05  D-STYLE            PIC X(8).
               05  FILLER             PIC XXX.

       01  HOLD-ID.
           03  FILLER                 PIC X(20).
           03  ID-END                 PIC X(10).
       01  COMMA-CHAR                 PIC XX VALUE ", ".

       01  C-MESS.
           03  FILLER                 PIC X(32) VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08 ".
           03  FILLER                 PIC X(39) VALUE
           "L09 L10 L11 L12 L13 L14 L15 L16 COLLID.".

       01  MAX-BEG-LINE               PIC S999 COMP.
       01  LINE-MAX                   PIC S999 COMP.
       01  TAB-SUB                    PIC S999 COMP.
       01  SCREEN-SUB                 PIC S999 COMP.
       01  BORROWER-ME       PIC X(30).
       01  ADD-ON            PIC X(25).
       01  IO-TYPE           PIC X(3).
       01  FIRST-TIME        PIC 9     VALUE 0.
       01  FIRST-SCAN        PIC 9     VALUE 0.
       01  DEFAULT1-KEY       PIC X.
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE " ".
           88  SCAN-ON       VALUE "ON".
       01  ENTER-SEQNO       PIC X     VALUE " ".

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  SCREEN-BUF.
           03  SC-BUF           OCCURS 8 TIMES.
               05  SCREEN-BUF-1 PIC X(80).
               05  SCREEN-BUF-2 PIC X(80).

       01  SCREEN-LIST.
           03  FILLER           PIC X(32) VALUE
               "L01 L02 L03 L04 L05 L06 L07 L08 ".
           03  FILLER           PIC X(32) VALUE
               "L09 L10 L11 L12 L13 L14 L15 L16.".

       01  LEGEND-01.
           03  FILLER            PIC X(33) VALUE
           "F1-CLEAR, F7-MENU, ^-SCROLL BACK,".
           03  FILLER            PIC X     VALUE X'76'.
           03  FILLER            PIC X(11) VALUE
           "-SCROLL FWD".

       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLP/LONPY_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBLP/LONPY_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".


       SCREEN SECTION.
       COPY "LIBLP/LONPY_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      *********************************
       MAIN-PROGRAM SECTION.
      *********************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPY" TO VDU-FORM-NAME.

      D    STOP MESS.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE " " TO ROUTE-CD
              MOVE 1 TO FIRST-TIME
           ELSE
              MOVE SPACES TO WR-BUF
              MOVE C-MESS TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM MODIFY-ROUTINE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       ENTER-KEYS-NO.
           MOVE "Y"    TO ENTER-SEQNO.
           MOVE SPACES TO IO-TYPE LN-REC.

           MOVE "S  A000MSEL." TO VDU.
           MOVE "F7-MENU, F6-SCAN, ENTER COLLATERAL ID:"
                                               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A200COLLID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPY-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-KEYS-NO
               ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTER-KEYS.

           IF VDUSTATUS = "1<"
              MOVE SPACE TO IX-VIN
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = "CLR"
                 MOVE SPACES TO WR-BUF
                 MOVE C-MESS TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUBUF = SPACE
              GO TO ENTER-KEYS-NO.

           MOVE VDUBUF TO IX-VIN HOLD-ID.

           PERFORM OPEN-IX2-FILE.
           MOVE BR-NO  TO IX-BRNO
                          QIX2-WBEG-BRNO
                          QIX2-WEND-BRNO.
           MOVE IX-VIN TO QIX2-WBEG-VIN
                          QIX2-WEND-VIN.

           PERFORM START-IX2-FILE.
           PERFORM READ-IX2-FILE-NEXT.
           IF IO-FG = 9 OR (IX-BRNO NOT = IX-PATH-OWNBR)
                         OR (IX-VIN NOT = HOLD-ID)
              MOVE "COLLATERAL ID NOT FOUND" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO WR-BUF
              MOVE C-MESS TO WR-LIST
              PERFORM CALL-WRFORM
              PERFORM CLOSE-IX2-FILE
              GO TO ENTER-KEYS-NO.

           PERFORM LOAD-TABLE.

           MOVE 1 TO TAB-SUB.
           PERFORM DISPLAY-SCREEN.

           PERFORM CLOSE-IX2-FILE.

       END-ENTER-KEYS.
           EXIT.

      **************************************
       ENTER-SCANKEY SECTION.
      **************************************
           MOVE "Y" TO DEFAULT1-KEY.
       ENTER-SCAN.
           MOVE 1 TO SCAN-FLAG.
           PERFORM OPEN-IX2-FILE.
       ENTER-KEY.
           MOVE "S  A000MSEL." TO VDU.
           IF BR-COLL-ID-SCAN = "S"
              MOVE "F1-CLEAR, F6-SCAN, ENTER LAST 6 :" TO VDUBUF
           ELSE
              MOVE "F1-CLEAR, F6-SCAN, COLLATERAL :" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A200SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              MOVE SPACES TO WR-BUF
              MOVE C-MESS TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO ENTER-SCAN.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = " "
              IF FIRST-SCAN NOT = 0
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF TO IX-VIN.
           MOVE 0 TO SCAN-FLAG.
       READ-FILES-NEXT.
           MOVE 0 TO FIRST-SCAN.

           MOVE BR-NO  TO IX-BRNO.

           PERFORM START-IX2-FILE.
           PERFORM READ-IX2-FILE-NEXT.
           IF IO-FG = 9 OR (IX-BRNO NOT = IX-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM LOAD-TABLE.
           MOVE 1 TO TAB-SUB.
           PERFORM DISPLAY-SCREEN.
           IF LINE-MAX > 8
              MOVE "MORE THAN 8 ENTRIES EXIST" TO MESS
              PERFORM SEND-MESS.

           MOVE 1 TO SCAN-FLAG.
           MOVE 1 TO FIRST-SCAN.
           MOVE " " TO SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-IX2-FILE.

       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A200SORTKEY." TO VDU.
           PERFORM SCREENIO.

       EXIT-ENTER-SCANKEY.
           EXIT.

      *************************************
       MODIFY-ROUTINE SECTION.
      *************************************
       RE-ENTER.
           MOVE "S  A000MSEL." TO VDU.
           IF LINE-MAX > 8
              MOVE LEGEND-01 TO VDUBUF
           ELSE
              MOVE "F1-CLEAR, F7-MENU" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

      * F4
           IF VDUSTATUS = "1U"
              COMPUTE TAB-SUB = TAB-SUB - 15
              IF TAB-SUB < 1
                 MOVE 1 TO TAB-SUB
                 PERFORM DISPLAY-SCREEN
              ELSE
                 PERFORM DISPLAY-SCREEN.

      * F5
           IF VDUSTATUS = "1D"
              ADD 1 TO TAB-SUB
              IF TAB-SUB > MAX-BEG-LINE
                 MOVE MAX-BEG-LINE TO TAB-SUB
                 PERFORM DISPLAY-SCREEN
              ELSE
                 PERFORM DISPLAY-SCREEN.

      * F1
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-MODIFY-ROUTINE.

      * F7
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPY-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO RE-ENTER
               ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO EXIT-MODIFY-ROUTINE.

           GO TO RE-ENTER.

       EXIT-MODIFY-ROUTINE.
           EXIT.

      **********************************
       LOAD-TABLE SECTION.
      **********************************

           MOVE SPACES TO SCREEN-TABLE.

           PERFORM OPEN-LTC1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO TAB-SUB.
           MOVE IX-VIN TO HOLD-ID.
       NEXT-ENTRY.
           IF IX-VIN NOT = HOLD-ID
              MOVE "ZZZZZZZZZZ" TO ID-END
              MOVE HOLD-ID TO IX-VIN
              GO TO EXIT-LOAD-TABLE.

           IF IX-CODE = "C" AND (IX-SEQNO = 1 OR 2) OR
                                (IX-SEQNO = 3)
              MOVE LTC-PATH-OWNBR     TO LTC-BRNO
              MOVE IX-ACCTNO          TO LTC-ACCTNO LN-ACCTNO
              MOVE 1                  TO LTC-SEQNO
              PERFORM READ-LTC1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 NEXT SENTENCE
           ELSE
              PERFORM READ-IX2-FILE-NEXT
              IF IO-FG NOT = 0 OR (IX-PATH-OWNBR NOT = IX-BRNO)
                 GO TO EXIT-LOAD-TABLE
              ELSE
                 GO TO NEXT-ENTRY.

           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM READ-IX2-FILE-NEXT
              IF IO-FG NOT = 0 OR (IX-PATH-OWNBR NOT = IX-BRNO)
                 GO TO EXIT-LOAD-TABLE
              ELSE
                 GO TO NEXT-ENTRY.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      * IX-SEQNO = SUBSCRIPT OF THE LT VIN
           MOVE LTC-VIN(IX-SEQNO) TO D-ID(TAB-SUB).

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-ACCT(TAB-SUB).
           MOVE BW-LNAME TO BORROWER-ME.
           MOVE BW-FNAME TO ADD-ON.
           INSPECT BORROWER-ME REPLACING FIRST "  " BY "+ ".
           STRING BORROWER-ME COMMA-CHAR ADD-ON DELIMITED BY "+"
              INTO BORROWER-ME.
           MOVE BORROWER-ME TO D-NAME(TAB-SUB).
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE(TAB-SUB).
           MOVE LN-CURBAL TO D-BAL(TAB-SUB).

           MOVE LTC-DESC(IX-SEQNO) TO D-DESCRIPTION(TAB-SUB).
           MOVE LTC-MAKE(IX-SEQNO) TO D-MAKE(TAB-SUB).
           MOVE LTC-MODEL(IX-SEQNO)TO D-MODEL(TAB-SUB).
           MOVE LTC-YEAR(IX-SEQNO) TO D-YEAR(TAB-SUB).
           MOVE LTC-STYLE(IX-SEQNO)TO D-STYLE(TAB-SUB).

           MOVE TAB-SUB TO LINE-MAX.
           IF MAX-BEG-LINE > 22
              MOVE 23 TO MAX-BEG-LINE
           ELSE
           IF LINE-MAX > 8
              COMPUTE MAX-BEG-LINE = LINE-MAX - 8
           ELSE
              MOVE 1 TO MAX-BEG-LINE.

           IF LINE-MAX > 29
              MOVE "ZZZZZZZZZZ" TO ID-END
              MOVE HOLD-ID TO IX-VIN
              GO TO EXIT-LOAD-TABLE.

           PERFORM READ-IX2-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (IX-BRNO NOT = IX-PATH-OWNBR)
              MOVE "ZZZZZZZZZZ" TO ID-END
              MOVE HOLD-ID TO IX-VIN
              GO TO EXIT-LOAD-TABLE.

           ADD 1 TO TAB-SUB.
           GO TO NEXT-ENTRY.

       EXIT-LOAD-TABLE.
           EXIT.


      *****************************************
       DISPLAY-SCREEN SECTION.
      *****************************************

           MOVE SPACE TO SCREEN-BUF.
           MOVE 1 TO SCREEN-SUB.
       NEXT-DISPLAY.
           MOVE SCREEN-LINE-1(TAB-SUB) TO SCREEN-BUF-1(SCREEN-SUB).
           MOVE SCREEN-LINE-2(TAB-SUB) TO SCREEN-BUF-2(SCREEN-SUB).
           IF SCREEN-SUB = 8 OR
              TAB-SUB = LINE-MAX
              NEXT SENTENCE
           ELSE
              ADD 1 TO TAB-SUB SCREEN-SUB
              GO TO NEXT-DISPLAY.

           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-DISPLAY-SCREEN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************************************
       FORM-RESET SECTION.
      **************************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPY-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPY-FORM-FIELDS TO WR-BUF.
           MOVE LONPY-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A200CNAME." TO VDU.
           MOVE EXT-CONAME-CONAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A080SDATE." TO VDU.
           MOVE EXT-CONAME-SYSDATE TO VDUBUF.
           PERFORM SCREENIO.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPY-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPY-HELP--DIR TO HELP-LINK-DIR.

      ************************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPY_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       MISC-ROUTINES SECTION.
      **************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IX2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIX2RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF LN-REC NOT = " "
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE BW-SSNO TO BORROWER-KEY.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPY-SCREEN.
           EXIT PROGRAM.
