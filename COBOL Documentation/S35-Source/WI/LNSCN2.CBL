       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNSCN2.
      ******************************************************************
      *
      *  LOAN FILE WINDOW SCAN
      *
      *=================================================================
      * REV:
      * JTG 082494 ADDED READ PREVIOUS LOGIC
      * JTG 103196 ADDED 'LPUPWKW' & 'LPUPWK'
      * KEC 112696 CHANGED LN-POCD FOR RESCISSION.
      * GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * BLV 032598 EXPAND PAGES FROM 8 TO 50; FIX LOOP ON STARTING LH
      * GLF 032399 ADD BRANCH FILE FOR LPLCAP TEST TO BR-LATECHG-ON-ALLOTMENT
      *  CS 141114 CHANGED VDU-WCOLOR-TCLP/VDU-WCTRL-TCLP TO VDU-WCOLOR-SCAN/
      *            VDU-WCTRL-SCAN  
      * BAH 160128 SPLIT OUT LTFILE, PD0003
      * MJD 170301 MOVED FILLER BETWEEN  D-JDPLDT AND D-PROFILE TO END OF
      *            D-LINE.  MAKER CODE WAS NOT DISPLAYING IN WINDOW.
      * BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0530 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LINE-LN-KEY & PAGE-LINE-LN-KEY
      *              TO LINE-LN-ACCTNO & PAGE-LINE-LN-ACCTNO.
      *
      * BAH 181218 GLOBAL LHFILE
      * BAH 181218 GLOBAL LNFILE
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220826 HARD CODED START
      * BAH 20230714 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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
           "@(#)WI/LNSCN2.CBL S35 02/22/24-12:50:39 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

       01  LNSCN2-LINES          PIC 99    VALUE 11.
       01  LNSCN2-COLUMNS        PIC 99    VALUE 78.
       01  LNSCN2-BEGIN-Y        PIC 99    VALUE 2.
       01  LNSCN2-BEGIN-X        PIC 99    VALUE 10.

       01  ERRCD                   PIC X VALUE " ".
       01  ONE                     PIC S9     VALUE 1.
       01  IO-TYPE                 PIC XXX.
       01  BW-SOLICIT              PIC X      VALUE " ".
       01  SYSTEM-DATE             PIC 9(8).

       01  SUB                     PIC 99.
       01  FLD-SUB                 PIC 99.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(16)  VALUE
           "010 020 030 040.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(16).
           05  FILLER              PIC X(54).
           05  FLDSEL-CURLN-FG     PIC X.
           05  FILLER              PIC X(4).
       01  FLDSEL-BUF-RDF          REDEFINES FLDSEL-BUF.
           05  FILLER              PIC X(52).
           05  FLDSEL-DELINQ       PIC ZZZZ,ZZZ.ZZ-.
           05  FLDSEL-PROFILE      REDEFINES FLDSEL-DELINQ
                                   PIC X(12).
           05  FILLER              PIC X(11).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "040.".
       01  MAX-FIELDS              PIC 99    VALUE 4.
       01  EOF-FG                  PIC X.

       01  DISPLAY-LIST.
           03  FILLER              PIC X(31)  VALUE
           "010 020 030 040 ENDMORE:F PGNO.".
       01  DISPLAY-BUF.
           03  D-LINE                OCCURS 4 TIMES.
               05  D-KEY             PIC X(16).
               05  D-LNDATE          PIC 9(6).
               05  D-LOAN-AMT        PIC ZZZZZZ.ZZ.
               05  FILLER            PIC X.
               05  D-TRM             PIC ZZZ.
               05  FILLER            PIC X.
               05  D-DEALER          PIC ZZZZ.ZZ.
               05  FILLER            PIC X.
               05  D-PLFG            PIC X.
               05  D-JDPLDT          PIC 9(6)  BLANK ZERO.
               05  D-DELINQ          PIC ZZZZ,ZZZ.ZZ-.
               05  D-PROFILE         REDEFINES D-DELINQ
                                     PIC X(12).
               05  D-BAL.
                   07  D-LOAN-BAL    PIC ZZZZZZ.ZZ-.
                   07  D-MKR         PIC X.
               05  D-POFF            REDEFINES D-BAL.
                   07  FILLER        PIC X.
                   07  D-POCD        PIC XX.
                   07  D-PAIDOFF     PIC 9(6).
                   07  FILLER        PIC XX.
               05  FILLER            PIC X.

           03  D-END-MORE            PIC X(7).
           03  D-PAGENO              PIC ZZ9.

       01  LINE-KEY.
           03  LINE-PTR              PIC 9.
           03  LINE-LN-ACCTNO        PIC 9(8).

       01  SKIP-START-FG             PIC X.

       01  CURRENT-PAGE                 PIC S999.
       01  PAGE-COUNT                   PIC 999.
       01  MAX-PAGES                    PIC 999    VALUE 50.
       01  PAGE-LINE-KEY-TBL            VALUE ZEROS.
           03  PAGE-LINE-KEY            OCCURS 50.
               05  PAGE-LINE-PTR        PIC 9.
               05  PAGE-LINE-LN-ACCTNO  PIC 9(8).

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


       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LHSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  LNSCN2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LNSCN2_DEF.CPY".
       COPY "LIBWI/LNSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LNSCN2W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LNSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME LNSCN2-BUF EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "XX" TO LNSCN2-STATUS.

           MOVE LNSCN2-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSTEM-DATE.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "LNSCN2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE LNSCN2-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  LNSCN2-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
               COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   SIZE    LNSCN2-COLUMNS + VDU-WIN-W-MOD
                   LINES   LNSCN2-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LNSCN2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-SP1-FILE.
       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-LH1-FILE.
           MOVE 1 TO LINE-PTR.
           MOVE " " TO SKIP-START-FG.

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF IO-TYPE FLDSEL-BUF.
           MOVE 1 TO FLD-SUB.
           IF PAGE-LINE-LN-ACCTNO(CURRENT-PAGE) = 0
              MOVE LINE-PTR TO PAGE-LINE-PTR(CURRENT-PAGE)
              MOVE 0 TO LINE-LN-ACCTNO
                        PAGE-LINE-LN-ACCTNO(CURRENT-PAGE)
           ELSE
              MOVE PAGE-LINE-KEY(CURRENT-PAGE) TO LINE-KEY.

       BUILD-PAGENO.
           PERFORM READ-LOANS.
           IF IO-TYPE = "END" OR FLD-SUB = MAX-FIELDS
              GO TO DISPLAY-PAGE.
            IF FLD-SUB = 1
               MOVE LINE-KEY TO PAGE-LINE-KEY(CURRENT-PAGE).
           ADD 1 TO FLD-SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF PAGE-COUNT = 1
              IF FLD-SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO LOANS FOUND FOR BORROWER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           IF EOF-FG = "Y"
              MOVE "* END *" TO D-END-MORE
           ELSE
              MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-TYPE = " "
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "Y" TO EOF-FG
                 IF IO-TYPE = "END" AND FLD-SUB = 1
                    MOVE -1 TO ONE
                    MOVE LAST-FIELD TO FLDSEL-FIELD
                    MOVE " " TO SKIP-START-FG
                    GO TO START-BUILD-PAGENO
                 ELSE
                    MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE " F7 EXIT, SELECT CODE(RTN):" TO MESS.
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
                 MOVE " " TO SKIP-START-FG
                             EOF-FG
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 MOVE " " TO SKIP-START-FG
                             EOF-FG
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
               OR EOF-FG = "Y"
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
              MOVE " " TO SKIP-START-FG
                          EOF-FG
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF ( FLDSEL-DELINQ NOT NUMERIC )
              IF ( FLDSEL-PROFILE = "*RESCISSION*" ) OR
                 ( FLDSEL-PROFILE = " *RESCINDED*" )
                 GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO LPS-DISPLAY-LNNO.
           IF LPS-DASH = "H"
              MOVE "SELECTION OF LOANS IN HISTORY NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO SELECT-FIELD-FROM-DISPLAY
           ELSE
              MOVE LPS-NUMBER TO LNSCN2-ACCTNO
              MOVE "00"       TO LNSCN2-STATUS.

           IF FLDSEL-STAT = "1<"
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.


      *********************************************
       READ-LOANS SECTION.
      *********************************************
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           GO TO READ-LOANS-4  READ-LOANS-5
      *          READ-LOANS-6  READ-LOANS-7
                 READ-LOANS-LH
              DEPENDING LINE-PTR.

       READ-LOANS-4.
           IF (SKIP-START-FG = " ")
                 OR (FLD-SUB = 1 AND LINE-LN-ACCTNO NOT = 0)
              MOVE LNSCN2-KEY TO LN-SSNO(1)
                                 QLN4-WEND-SSNO1
                                 QLN4-WBEG-SSNO1
              MOVE LN-OWNBR   TO QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR
              PERFORM START-LN4-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.
       READ-LOANS-4-NEXT.
           PERFORM READ-LN4-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LN-SSNO(1) = LNSCN2-KEY OR (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-VOID )
                    GO TO READ-LOANS-4-NEXT
                 ELSE
                    IF LINE-LN-ACCTNO = 0
                       PERFORM SETUP-LINE
                       GO TO READ-LOANS-EXIT
                    ELSE
                       IF FLD-SUB = 1 AND LN-ACCTNO NOT = LINE-LN-ACCTNO
                          GO TO READ-LOANS-4-NEXT
                       ELSE
                          PERFORM SETUP-LINE
                          GO TO READ-LOANS-EXIT.
           ADD 1 TO LINE-PTR.
           MOVE " " TO SKIP-START-FG.

       READ-LOANS-5.
           IF (SKIP-START-FG = " ")
                 OR (FLD-SUB = 1 AND LINE-LN-ACCTNO NOT = 0)
              MOVE LNSCN2-KEY TO LN-SSNO(2)
                                 QLN5-WEND-SSNO2
                                 QLN5-WBEG-SSNO2
              MOVE LN-OWNBR   TO QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR
              PERFORM START-LN5-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.
       READ-LOANS-5-NEXT.
           PERFORM READ-LN5-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LN-SSNO(2) = LNSCN2-KEY OR (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-VOID )
                    GO TO READ-LOANS-5-NEXT
                 ELSE
                    IF LINE-LN-ACCTNO = 0
                       PERFORM SETUP-LINE
                       GO TO READ-LOANS-EXIT
                    ELSE
                       IF FLD-SUB = 1 AND LN-ACCTNO NOT = LINE-LN-ACCTNO
                          GO TO READ-LOANS-5-NEXT
                       ELSE
                          PERFORM SETUP-LINE
                          GO TO READ-LOANS-EXIT.
           ADD 1 TO LINE-PTR.
           MOVE " " TO SKIP-START-FG.

      *READ-LOANS-6.
      *    IF (SKIP-START-FG = " ")
      *          OR (FLD-SUB = 1 AND LINE-LN-ACCTNO NOT = 0)
      *       MOVE LNSCN2-KEY TO LN-SSNO(3)
      *                          QLN6-WEND-SSNO3
      *                          QLN6-WBEG-SSNO3
      *       MOVE LN-OWNBR   TO QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR
      *       PERFORM START-LN6-FILE-NOT-GREATER
      *       MOVE "Y" TO SKIP-START-FG.
      *READ-LOANS-6-NEXT.
      *    PERFORM READ-LN6-FILE-PREVIOUS.
      *    IF IO-FG = 0
      *       IF LN-SSNO(3) = LNSCN2-KEY OR (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-VOID )
      *             GO TO READ-LOANS-6-NEXT
      *          ELSE
      *             IF LINE-LN-ACCTNO = 0
      *                PERFORM SETUP-LINE
      *                GO TO READ-LOANS-EXIT
      *             ELSE
      *                IF FLD-SUB = 1 AND LN-ACCTNO NOT = LINE-LN-ACCTNO
      *                   GO TO READ-LOANS-6-NEXT
      *                ELSE
      *                   PERFORM SETUP-LINE
      *                   GO TO READ-LOANS-EXIT.
      *    ADD 1 TO LINE-PTR.
      *    MOVE " " TO SKIP-START-FG.

      *READ-LOANS-7.
      *    IF (SKIP-START-FG = " ")
      *          OR (FLD-SUB = 1 AND LINE-LN-ACCTNO NOT = 0)
      *       MOVE LNSCN2-KEY TO LN-SSNO(4)
      *                          QLN7-WEND-SSNO4
      *                          QLN7-WBEG-SSNO4
      *       MOVE LN-OWNBR   TO QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR
      *       PERFORM START-LN7-FILE-NOT-GREATER
      *       MOVE "Y" TO SKIP-START-FG.
      *READ-LOANS-7-NEXT.
      *    PERFORM READ-LN7-FILE-PREVIOUS.
      *    IF IO-FG = 0
      *       IF LN-SSNO(4) = LNSCN2-KEY OR (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-VOID )
      *             GO TO READ-LOANS-7-NEXT
      *          ELSE
      *             IF LINE-LN-ACCTNO = 0
      *                PERFORM SETUP-LINE
      *                GO TO READ-LOANS-EXIT
      *             ELSE
      *                IF FLD-SUB = 1 AND LN-ACCTNO NOT = LINE-LN-ACCTNO
      *                   GO TO READ-LOANS-7-NEXT
      *                ELSE
      *                   PERFORM SETUP-LINE
      *                   GO TO READ-LOANS-EXIT.
      *    ADD 1 TO LINE-PTR.
      *    MOVE " " TO SKIP-START-FG.

       READ-LOANS-LH.
           MOVE LH-PATH-OWNBR TO LH-BRNO QLH1-WBEG-BRNO
                                         QLH1-WEND-BRNO.
           MOVE LNSCN2-KEY    TO LH-SSNO QLH1-WBEG-SSNO
                                         QLH1-WEND-SSNO.
           IF (SKIP-START-FG = " ")
              MOVE 99999999 TO LH-ACCTNO QLH1-WEND-ACCTNO
              MOVE 0        TO           QLH1-WBEG-ACCTNO
              PERFORM START-LH1-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.
           IF (FLD-SUB = 1 AND LINE-LN-ACCTNO NOT = 0)
              MOVE LINE-LN-ACCTNO TO LH-ACCTNO QLH1-WEND-ACCTNO
              MOVE 0              TO           QLH1-WBEG-ACCTNO
              PERFORM START-LH1-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.

       READ-LOANS-LH-NEXT.
           PERFORM READ-LH1-FILE-PREVIOUS.
           IF (IO-FG NOT = 0) OR (LH-SSNO NOT = LNSCN2-KEY)
                               OR (LH-BRNO NOT = LH-PATH-OWNBR)
              MOVE "END" TO IO-TYPE
           ELSE
              IF LINE-LN-ACCTNO = 0
                 PERFORM SETUP-LINE-HISTORY
                 GO TO READ-LOANS-EXIT
              ELSE
                 IF FLD-SUB = 1 AND LH-ACCTNO NOT = LINE-LN-ACCTNO
                    GO TO READ-LOANS-LH-NEXT
                 ELSE
                    PERFORM SETUP-LINE-HISTORY
                    GO TO READ-LOANS-EXIT.

       READ-LOANS-EXIT.
           EXIT.

      ********************************************
       SETUP-LINE SECTION.
      ********************************************

           PERFORM OPEN-BR-FILE.
           MOVE LN-OWNBR TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE LN-ACCTNO TO LINE-LN-ACCTNO.
           PERFORM GET-SP.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-KEY(FLD-SUB).

           MOVE LN-LOANDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-LNDATE(FLD-SUB).
           MOVE LN-LNAMT         TO D-LOAN-AMT(FLD-SUB).
           MOVE LN-ORGTERM       TO D-TRM(FLD-SUB).
           MOVE LN-DLNO          TO D-DEALER(FLD-SUB).
           IF LN-PLDATE NOT = 0
              MOVE "P" TO D-PLFG(FLD-SUB)
              MOVE LN-PLDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-JDPLDT(FLD-SUB).
           IF LN-JDDATE NOT = 0
              MOVE "J" TO D-PLFG(FLD-SUB)
              MOVE LN-JDDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-JDPLDT(FLD-SUB).

           IF ( LN-POCD-OPEN ) AND ( LN-POFFDATE = 0 )
              MOVE LN-CURBAL TO D-LOAN-BAL(FLD-SUB)
           ELSE
           IF ( LN-POCD-INRESCISSION ) OR ( LN-POCD-RESCIND )
              MOVE LN-CURBAL TO D-LOAN-BAL(FLD-SUB)
           ELSE
              MOVE LN-POCD TO D-POCD(FLD-SUB)
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAIDOFF(FLD-SUB).

           IF LN-SSNO(1) = LNSCN2-KEY
              MOVE LN-MAKERCD(1) TO D-MKR(FLD-SUB)
           ELSE
              IF LN-SSNO(2) = LNSCN2-KEY
                 MOVE LN-MAKERCD(2) TO D-MKR(FLD-SUB)
              ELSE
                 IF LN-SSNO(3) = LNSCN2-KEY
                    MOVE LN-MAKERCD(3) TO D-MKR(FLD-SUB)
                 ELSE
                    IF LN-SSNO(4) = LNSCN2-KEY
                       MOVE LN-MAKERCD(4) TO D-MKR(FLD-SUB).

           PERFORM SET-DELINQ-VALUE.

       SETUP-LINE-EXIT.
           EXIT.

      ********************************************
       SETUP-LINE-HISTORY SECTION.
      ********************************************
           MOVE LH-ACCTNO TO LINE-LN-ACCTNO.
           PERFORM LHSTAT-DISPLAY.

           MOVE "H" TO LHS-DASH.
           MOVE LHS-DISPLAY-LNNO TO D-KEY(FLD-SUB).
           MOVE LH-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-LNDATE(FLD-SUB).
           MOVE LH-LNAMT         TO D-LOAN-AMT(FLD-SUB).
           MOVE LH-ORGTERM       TO D-TRM(FLD-SUB).
           MOVE LH-DLNO          TO D-DEALER(FLD-SUB).
      * LOAN BAL ALWAYS = 0 FOR HISTORY
           MOVE LH-MAKERCD       TO D-MKR(FLD-SUB).
           MOVE LH-POCD          TO D-POCD(FLD-SUB).
           MOVE LH-PYPROFILE     TO D-PROFILE(FLD-SUB).
           INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-".
           MOVE LH-POFFDATE TO D-PAIDOFF(FLD-SUB).

       SETUP-LINE-HISTORY-EXIT.
           EXIT.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LNSCN2-SCREEN UPON LNSCN2-WINDOW-HANDLE.
           MOVE LNSCN2-FORM-FIELDS TO WR-BUF.
           MOVE LNSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LNSCN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNSCN2-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LNSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       SET-DELINQ-VALUE.

           IF ( LNSCN2-DELINQ-CD = "R" )
              MOVE LN-RECPROFILE(1) TO D-PROFILE(FLD-SUB)
              INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-"
           ELSE
           IF ( LNSCN2-DELINQ-CD = "C" )
              MOVE LN-CONPROFILE(1) TO D-PROFILE(FLD-SUB)
              INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-"
           ELSE
           IF ( LN-POCD-INRESCISSION )
              MOVE "*RESCISSION*" TO D-PROFILE(FLD-SUB)
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE " *RESCINDED*" TO D-PROFILE(FLD-SUB)
           ELSE
           IF ( LN-POCD = " " ) AND ( LN-POFFDATE = 0 )
              MOVE LN-1STPYDATE   TO CEPP1-DATE
              MOVE SYSTEM-DATE    TO CEPP2-DATE
              MOVE D-MKR(FLD-SUB) TO CEPP-MAKERCD
              PERFORM TOTAL-DELINQUENCY
              MULTIPLY -1 BY CDEL-ARREARAGE GIVING D-DELINQ(FLD-SUB)
           ELSE
              MOVE LN-CONPROFILE(1) TO D-PROFILE(FLD-SUB)
              INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-".

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LHSTAT.CPY".

       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-ROUTINE SECTION.
      **********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNSCN2-SCREEN.
           DESTROY LNSCN2-WINDOW-HANDLE.
           EXIT PROGRAM.
