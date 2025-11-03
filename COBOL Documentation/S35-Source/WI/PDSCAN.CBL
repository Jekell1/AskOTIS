       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PDSCAN.
      *********************************************************************
      *  SCRATCH PAD FILE (LOAN RECALL & QUOTES)
      *                      WINDOW SCAN
      * REV:
      *  JTG 010495 CORRECTED TITLE TO DISPLAY 'S.F.' OR 'LOAN' SELECTION
      *  JTG 081397 ADDED PDSCAN-GIFTNET AND REMOVED PD2-CAN-NO
      *  GLF 091897 REPLACED ANY PROMPT THAT DISPLAYS WORD 'GIFTNET' W/'WCBC'
      *  JTG 100197 CHANGED TO TEST PDSCAN-BALLOON, TO SELECT ONLY PD-RECORDS
      *             THAT HAVE AN AMOUNT IN PD-LASTPYAMT
      *  JTG 032498 CHANGED TEST FOR SINGLE PAY LOANS
      *  JTG 101498 ADDED PDSCAN-REVOLVING LOGIC, REGENCY REVOL
      *  BLV 990416 IF D-NAME(SUB) IS BLANK, DON'T PROCEED, GO TO READ-NEXT;
      *             NOT SURE WHERE BLANK PD2-BWNAMES ARE COMING FROM BUT
      *             THEY WILL LOCK UP FLDSEL
      *  JTG 101498 CHANGED TEST ON PDSCAN-REVOLVING TO LET " " == "N"
      *             RE: CAN LOANS MADE BEFORE REVOLVING EXISTED AND
      *             PD-REVOLVING IS FILLER SPACE ' '      BC ACCEPTANCE, LORI
      * BAH 20190627 REMOVED PD-REVOLVING, PT0153
      **************************************************************************
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
           "@(#)WI/PDSCAN A34 10/23/20-08:03:11 >".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".

       01  PDSCAN-LINES              PIC 99  VALUE 16.
       01  PDSCAN-COLUMNS            PIC 99  VALUE 39.
       01  PDSCAN-BEGIN-Y            PIC 99  VALUE 3.
       01  PDSCAN-BEGIN-X            PIC 99  VALUE 2.
       01  ERRCD                   PIC X VALUE " ".
       01  ONE                     PIC S9     VALUE 1.
       01  IX1                     PIC 99.
       01  IX2                     PIC 99.
       01  SUB                     PIC 99.

       01  ALP-PD.
           03  ALP-PD123           PIC X(3).
           03  ALP-HYPHEN1         PIC X.
           03  ALP-PD45            PIC X(2).
           03  ALP-HYPHEN2         PIC X.
           03  ALP-PD6789          PIC X(4).

       01  NUM-PD                  PIC 9(9).
       01  FILLER REDEFINES NUM-PD.
           03  NUM-PD123           PIC X(3).
           03  NUM-PD45            PIC X(2).
           03  NUM-PD6789          PIC X(4).


       01  PD-ALP-TABLE.
           03  PD-ALP-TBL          OCCURS 32 TIMES
                                   PIC X.

       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(11).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(22).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  PDSCAN-WINDOW-HANDLE    PIC X(10).
       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC X(11).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(22).
               05  D-NAME-X       REDEFINES D-NAME.
                   07  FILLER      PIC X(20).
                   07  D-VOID-FILL PIC X.
                   07  D-VOID-FG   PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 50.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 50 TIMES
                                       PIC X(20).

      *01  MORE-LIST.
      *    03  FILLER     PIC X(39)  VALUE
      *    "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
      *    03  FILLER     PIC X(16)  VALUE
      *    "LIA OPEN CLOSED.".
      *01  MORE-BUF.
      *    03  M-NAME    PIC X(30).
      *    03  M-ADR1    PIC X(30).
      *    03  M-ADR2    PIC X(30).
      *    03  M-ADR3    PIC X(30).
      *    03  M-PHONE   PIC X(14).
      *    03  M-EMPL    PIC X(20).
      *    03  M-SPOUSE  PIC X(28).
      *    03  M-LIA     PIC Z,ZZZ,ZZ9.99.
      *    03  M-OPEN    PIC ZZ.
      *    03  M-CLOSED  PIC ZZ.

       COPY "LIBGB/PHON400.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PDSCAN_DEF.CPY".
       COPY "LIBWI/PDSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/PDSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PDSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME PDSCAN-BUF EXIT-PATHNAME.

      *********************************
      *      MAIN PROGRAM MODULE
      *********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "00" TO PDSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "PDSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE PDSCAN-BEGIN-Y
                   AT COL  PDSCAN-BEGIN-X
                   SIZE    PDSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   PDSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS PDSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A220TITLE." TO VDU.
           IF PDSCAN-ID = "L"
              IF PDSCAN-BALLOON = "Y"
                 MOVE " BALLOON LOAN RECALL" TO VDUBUF
              ELSE
                 MOVE "  DIRECT LOAN RECALL" TO VDUBUF
           ELSE
           IF PDSCAN-ID = "S"
              MOVE " SALES FINANCE RECALL" TO VDUBUF
           ELSE
           IF PDSCAN-ID = "Q"
              IF PDSCAN-SALEFIN = "Y"
                 MOVE " S.F. QUOTE SELECTION" TO VDUBUF
              ELSE
                 IF PDSCAN-GIFTNET = "Y"
                    MOVE " WCBC QUOTE SELECT" TO VDUBUF
                 ELSE
                    IF PDSCAN-BALLOON = "Y"
                       MOVE " BALLOON QUOTE SELECT" TO VDUBUF
                    ELSE
                       MOVE " LOAN QUOTE SELECTION" TO VDUBUF
           ELSE
              GO TO EXIT-PROG.
           PERFORM SCREENIO.
           MOVE " " TO VDUBUF.
           GO TO ENTER-SCANKEY-CONT.

      *************************************
      *    ENTER SCAN KEY
      *************************************
       ENTER-SCANKEY.
           IF PDSCAN-ID = "L" OR "S"
              MOVE "F7-EXIT, ENTER LAST NAME, FIRST:" TO MESS
           ELSE
           IF PDSCAN-ID = "Q"
              MOVE "F7-EXIT, ENTER THE TITLE OF QUOTE:" TO MESS
           ELSE
              MOVE "F7-EXIT, ENTER 2 DIGIT CAN LOAN NO:" TO MESS
           PERFORM SEND-LEGEND.
           MOVE "E  A250SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-PD2-FILE.
           PERFORM ARRANGE-PD-KEY.
           MOVE PD-ALP-TABLE TO PAGE-KEY(PAGE-COUNT).
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
           IF PDSCAN-ID = "S"
              MOVE "L" TO PD2-ID
           ELSE
              IF PDSCAN-ID = "C"
                 MOVE "Q" TO PD2-ID
              ELSE
                 MOVE PDSCAN-ID TO PD2-ID.
           MOVE PDSCAN-BRANCH          TO PD2-BRANCH.
           MOVE PAGE-KEY(CURRENT-PAGE) TO PD2-BWNAME.

           PERFORM START-PD2-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-PD2-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF PDSCAN-ID = "Q" OR "C"
              IF PD2-ID NOT = "Q"
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF PD2-ID NOT = "L"
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           IF (PD2-BRANCH NOT = PDSCAN-BRANCH)
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB > 9
              GO TO DISPLAY-PAGE.
           IF PDSCAN-SALEFIN NOT = " "
              IF PDSCAN-SALEFIN NOT = PD-SALEFIN
                 GO TO BUILD-PAGENO.
           IF PDSCAN-SMALLOAN NOT = " "
              IF PDSCAN-SMALLOAN NOT = PD-SMALLOAN
                 GO TO BUILD-PAGENO.
           IF PDSCAN-SERVACCT NOT = " "
              IF PDSCAN-SERVACCT NOT = PD-SERVACCT
                 GO TO BUILD-PAGENO.
           IF PDSCAN-LOANTYPE NOT = " "
              IF PDSCAN-LOANTYPE NOT = PD-LOANTYPE
                 GO TO BUILD-PAGENO.
      *-------------------------------------------------------------
      *    ALL PD-RECORDS THAT ARE PRODUCED FROM
      *        SFQUOT.C, BLQUOT.C AND ZONPC0.C OR VIA CALCZ3.C
      *        CONTAIN AN AMOUNT IN PD-LASTPYAMT, EXCEPT;
      *        SINGLE PAYS WHICH HAVE LASTPYAMT = ZERO.
      *          NOTE: SINGLE PAYMENT LOANS CAN ONLY BE BOOKED
      *                  VIA CALCZ3.C
      *    ALL PD-RECORDS THAT ARE PRODUCED FROM
      *        LNQUOT.C, CLQUOT.C XONPC0.C AND LONPC0.C
      *        OR VIA CALCZL.C CONTAIN ZERO IN PD-LASTPYAMT, EXCEPT
      *        SINGLE PAYS WHICH HAVE LASTPYAMT = ZERO.
      *          NOTE: SINGLE PAYMENT LOANS CAN ONLY BE BOOKED
      *                  VIA CALCZ3.C
      *-------------------------------------------------------------
           IF PD-SALEFIN = "N"
              IF PDSCAN-BALLOON NOT = " "
                 IF (PD-ORGTERM = 1 AND PD-NOREGPY = 1)
                         OR (PD-ORGTERM > 1 AND PD-LASTPYAMT = 0)
                    GO TO BUILD-PAGENO
                 END-IF
              ELSE
                 IF NOT ( (PD-ORGTERM = 1 AND PD-NOREGPY = 1)
                              OR (PD-ORGTERM > 1 AND PD-LASTPYAMT = 0)
                        )
                    GO TO BUILD-PAGENO.

           IF PDSCAN-GIFTNET NOT = " "
              IF PDSCAN-GIFTNET = "N"
                     AND PD-GIFTNET = " "
                 NEXT SENTENCE
              ELSE
                 IF PDSCAN-GIFTNET NOT = PD-GIFTNET
                    GO TO BUILD-PAGENO.

           IF PDSCAN-ID = "L" OR "S"
              MOVE PD-SOC-SEC TO NUM-PD
              PERFORM NUMPD-TO-ALPPD
              MOVE ALP-PD TO D-KEY(SUB)
              MOVE PD2-BWNAME TO D-NAME(SUB)
           ELSE
              MOVE PD2-BWNAME TO D-LINE(SUB).

      * 4/16/99 - FLDSEL WILL LOCK UP USER IF LINE IS BLANK - NOT
      * SURE WHERE QUOTES WITH BLANK PD2-BWNAMES ARE COMING FROM -
      * THIS WILL AT LEAST STOP LOCK UPS  - BLV:
           IF D-LINE(SUB) = SPACES
              GO TO BUILD-PAGENO.

      * TEST FOR VOIDED GIFTNET SEND ORDERS:
           IF PD-GIFTNET = "Y"
              IF PD-GARNISH-EXTR = "V"
                 MOVE " " TO D-VOID-FILL(SUB)
                 MOVE "V" TO D-VOID-FG(SUB)
              ELSE
                 IF PD-GARNISH-EXTR = "Y"
                    MOVE " " TO D-VOID-FILL(SUB)
                    MOVE "E" TO D-VOID-FG(SUB)
              ELSE
                 IF PD-GARNISH-EXTR = "X"
                    MOVE " " TO D-VOID-FILL(SUB)
                    MOVE "X" TO D-VOID-FG(SUB).

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
                    MOVE PD2-BWNAME TO PAGE-KEY(PAGE-COUNT)
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
           IF PDSCAN-ID = "L"
              MOVE "F1, SELECT A DIRECT LOAN:" TO MESS
           ELSE
           IF PDSCAN-ID = "S"
              MOVE "F1, SELECT A SALES FINANCE CONTRACT:" TO MESS
           ELSE
           IF PDSCAN-ID = "Q"
              MOVE "F1, SELECT A LOAN QUOTE:" TO MESS
           ELSE
              MOVE "F1, SELECT A CAN LOAN:" TO MESS.
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
           IF NOT (FLDSEL-STAT = "00" )
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF PDSCAN-ID = "L" OR "S"
              MOVE FLDSEL-KEY TO ALP-PD
              PERFORM ALPPD-TO-NUMPD
              MOVE NUM-PD TO PDSCAN-NO
              MOVE FLDSEL-NAME TO PDSCAN-BWNAME
              MOVE "00"        TO PDSCAN-STATUS
           ELSE
              MOVE "00"       TO PDSCAN-STATUS
              MOVE FLDSEL-BUF TO PDSCAN-BWNAME.

           GO TO END-ROUTINE.


      *********************************
       ARRANGE-PD-KEY SECTION.
      *********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO PD-ALP-TABLE.
       ARR-PD-KEY-AGAIN.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO PD-ALP-TBL(IX2)
           ELSE
              MOVE 16 TO IX2.
           IF IX1 < 20
              IF IX2 < 32
                 ADD 1 TO IX1
                 ADD 1 TO IX2
                 GO TO ARR-PD-KEY-AGAIN.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMPD-TO-ALPPD.
           MOVE NUM-PD123 TO ALP-PD123.
           MOVE NUM-PD45 TO ALP-PD45.
           MOVE NUM-PD6789 TO ALP-PD6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPPD-TO-NUMPD.
           MOVE ALP-PD123 TO NUM-PD123.
           MOVE ALP-PD45 TO NUM-PD45.
           MOVE ALP-PD6789 TO NUM-PD6789.
           IF NUM-PD NOT NUMERIC
              MOVE 0 TO NUM-PD.

      *********************************
       FORM-RESET SECTION.
      *********************************
           DISPLAY PDSCAN-SCREEN UPON PDSCAN-WINDOW-HANDLE.
           MOVE PDSCAN-FORM-FIELDS TO WR-BUF.
           MOVE PDSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************
       SETUP-LINK-INFO SECTION.
      *********************************
           MOVE PDSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *********************************
       VDU-CONVERT-FIELD SECTION.
      *********************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PDSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD2-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPPD2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDSCAN-SCREEN.
           DESTROY PDSCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.

