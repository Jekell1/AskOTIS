       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWLNS3.
      *************************************************************************
      *  LOAN FILE WINDOW SCAN
      *
      *   COPIED FROM BWLNS2 - NEED A NEW VERSION FOR COLLECTIONS
      *             TO INCORPORATE ACCT INQUIRY SELECTION OPTION
      *
      *  CALLED FROM : LP/LONPG0, LP/LONPF0, LP/LONPG3, LP/LONPG5, LP/LONPG7,
      *                LP/LONPQ1, WI/BILNSC, WI/BWCREA, WI/BWLNSC, WI/CHBORR,
      *                WI/MODBOR, WI/COSIGN
      * REV:
      *  JTG 032593 REVISED TO PASS BR-REC AND GB-REC
      *  JTG 082494 ADDED READ PREVIOUS LOGIC
      *  JTG 090495 ADDED TEST FOR GP-NO-LC-IN-ARREARAGE
      *  JFF 112295 ADDED ALL DATA PATH LOGIC
      *  KEC 112696 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.  ADDED BW-FILE.
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 061897 ADDED BRANCH PATH LOGIC TO BWFILE. FIXED BUG
      *             WITH DELINQUENCY - BWLNS3-SYSDATE WAS NET SET
      *             IN CALLING PROGRAM.
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  BLV 032598 EXPAND PAGES FROM 8 TO 50; FIX LOOP ON STARTING LH
      *  BLV 042298 WAS CHECKING GP-NO-LC-IN-ARREARAGE BUT DIDN'T HAVE
      *             GP-REC; ADDED GPENV ROUTINE
      *  GLF 032399 ADD BRANCH FILE
      *  MJD 060912 COPIED FROM BWLNS2 - NEED A NEW VERSION FOR COLLECTIONS
      *             TO INCORPORATE ACCT INQUIRY SELECTION OPTION
      *  MJD 061005 ADDED THE ABILITY TO ALLOW, WHEN YOU F3 (SPINQ1) TO
      *             HIGHLIGHT AN ACCOUNT AND BRING IT UP IN THE COLLECTION
      *             WORK SCRREN FORMAT, JUST LIKE IT WOULD HAVE HAD YOU USED
      *             THE F10 AND ENTERRED THE ACCOUNT MANUALLY. THIS IS LIKE
      *             BWLNS2 EXCEPT IT ALLOWS USERS TO SELECT A LOAN AND PASS
      *             LOAN NUMBER TO COLLECTION WORK SCREEN.             #2843
      *  BLM 081217 DISPLAY "A" IN ACCT# FOR ARCHIVED LOAN, WORLD PR# 547
      *  BLM 130823 WHEN I CONVERTED THIS TO A30, I WAS COMPARING TO BWLNS2
      *             & ADDED A COUPLE THINGS THAT SEEMED TO BE MISSING OR
      *             INCORRECT.  MY BAD, BUT IT SEEMED EASIER TO DO SINCE MY
      *             BRAIN WAS INTO IT.  SEE NOTES IN BWLNS2 FOR REGAN PL#704
      *             & REGACC PR# 304. ALSO ADDED AN IF IN THE HISTORY READ
      *             THAT SKIPS PAYOFFS IN LH.
      *  BLM 140902 BEFORE CALLING XPINQ, MOVE "Y" TO BWLNS3-SCANYN.  XPINQ
      *             MOVED "E" TO THIS FLAG, SO IF YOU SELECTED AN ACCT HERE
      *             THEN F7ED BACK & CHOSE ANOTHER, THE 2ND ACCOUNT WOULD 
      *             NOT DISPLAY BECAUSE THE FLAG WAS = "E"
      *  BLM 141104 REMOVE LOGIC CHECK BWLNS3-BR-MACHINE, LEFT OVER FROM
      *             BWLNS2, NOT NEEDED.  ONLY CALLED FROM SPINQ/XPINQ
      *   CS 141114 CHANGED VDU-WCOLOR-TCLP/VDU-WCTRL-TCLP TO VDU-WCOLOR-SCAN/
      *             VDU-WCTRL-SCAN  
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *  MJD 170301 MOVED FILLER BETWEEN  D-JDPLDT AND D-PROFILE TO END OF
      *             D-LINE.  MAKER CODE WAS NOT DISPLAYING IN WINDOW.
      *  BAH 170407 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0531 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LINE-LN-KEY & PAGE-LINE-LN-KEY
      *              TO LINE-LN-ACCTNO & PAGE-LINE-LN-ACCTNO.
      *
      * BAH 181218 GLOBAL LHFILE
      * BAH 181218 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 20220803 HARD CODED START
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/BWLNS3 S35 04/29/25-11:53:16 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
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

       01  BWLNS3-LINES             PIC 99    VALUE 13.
       01  BWLNS3-COLUMNS           PIC 99    VALUE 78.
       01  BWLNS3-BEGIN-Y           PIC 99    VALUE 10.
       01  BWLNS3-BEGIN-X           PIC 99    VALUE 2.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  IO-TYPE                 PIC XXX.
       01  SYSTEM-DATE             PIC 9(8).

       01  BWLNS3-LINK-BUF.
           03  BWLNS3-SCANYN       PIC X(1) VALUE "Y".
           03  BWLNS3-LINK-ACCTNO  PIC 9(8).

       01  SUB                     PIC 99.
       01  FLD-SUB                 PIC 99.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FILLER                 REDEFINES FLDSEL-FIELD.
           03  FIELD-ROW           PIC 99.
           03  FIELD-COL           PIC 9.
           03  FIELD-PERIOD        PIC X.
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(24)  VALUE
           "010 020 030 040 050 060.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(16).
           05  FILLER              PIC X(57).
           05  FLDSEL-CURLN-FG     PIC X.
           05  FILLER              PIC X(4).
       01  FLDSEL-BUF-RDF          REDEFINES FLDSEL-BUF.
           05  FILLER              PIC X(55).
           05  FLDSEL-DELINQ       PIC ZZZZ,ZZZ.ZZ-.
           05  FLDSEL-PROFILE      REDEFINES FLDSEL-DELINQ
                                   PIC X(12).
           05  FILLER              PIC X(11).

       01  ORIG-ACCTNO                    PIC 9(8).
       01  MAKE-ANOTHER-SEL-MESS          PIC X(58) VALUE
           "LOAN IS ALREADY DISPLAYED, PLEASE MAKE ANOTHER SELECTION".

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "060.".
       01  MAX-FIELDS              PIC 99    VALUE 6.
       01  EOF-FG                  PIC X.

       01  BWLNS3-WINDOW-HANDLE    PIC X(10).
       01  DISPLAY-LIST.
           03  FILLER              PIC X(39)  VALUE
           "010 020 030 040 050 060 ENDMORE:F PGNO.".
       01  DISPLAY-BUF.
           03  D-LINE                OCCURS 6 TIMES.
               05  D-KEY             PIC X(16).
               05  D-LNDATE          PIC 99/99/99.
               05  FILLER            PIC X.
               05  D-LOAN-AMT        PIC ZZZZZZ.ZZ.
               05  FILLER            PIC X.
               05  D-TRM             PIC ZZZ.
               05  FILLER            PIC X.
               05  D-DEALER          PIC ZZZZ.ZZ.
               05  FILLER            PIC X.
               05  D-PLFG            PIC X.
               05  D-JDPLDT          PIC 9(6)  BLANK ZERO.
      *        05  FILLER            PIC X.
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

       01  SEQNO-BUF.
           03  SEQNO-TAB             OCCURS 6.
               05  SEQNO             PIC 99.

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


       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LHSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BWLNS3_DEF.CPY".
       COPY "LIBWI/BWLNS3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BWLNS3W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BWLNS3_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME BWLNS3-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "00" TO BWLNS3-STATUS.

           MOVE "BWLNS3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE BWLNS3-BEGIN-Y
                   AT COL  BWLNS3-BEGIN-X
                   SIZE    BWLNS3-COLUMNS + VDU-WIN-W-MOD
                   LINES   BWLNS3-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS BWLNS3-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE BWLNS3-ACCTNO TO ORIG-ACCTNO.
           MOVE BWLNS3-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSTEM-DATE.

           PERFORM GET-GPENV.

           PERFORM OPEN-SP1-FILE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "NO GLOBAL FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

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
           PERFORM VARYING FLD-SUB FROM 1 BY 1 UNTIL FLD-SUB > 6
                MOVE 0 TO SEQNO(FLD-SUB)
           END-PERFORM.

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
           IF BWLNS3-SCAN-ONLY = "Y"
              MOVE " F7-EXIT, SCAN LOANS:" TO MESS
           ELSE
              MOVE " F7-EXIT, SELECT LOAN (RTN):" TO MESS.
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
           IF NOT (FLDSEL-STAT = "00" )
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF BWLNS3-SCAN-ONLY = "Y"
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
              IF BWLNS3-CUR-ONLY NOT = " "
                           AND FLDSEL-CURLN-FG NOT = "."
                 MOVE "SELECTION OF PAID OFF LOANS NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 GO TO SELECT-FIELD-FROM-DISPLAY
           ELSE
              MOVE LPS-NUMBER TO BWLNS3-ACCTNO.

           IF FLDSEL-STAT = "00"
              IF BWLNS3-ACCTNO <> ORIG-ACCTNO
                 PERFORM LOAN-INQUIRY
              ELSE
                 MOVE MAKE-ANOTHER-SEL-MESS TO MESS
                 PERFORM SEND-MESS
              END-IF
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
              MOVE BWLNS3-KEY TO LN-SSNO(1)
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1
              MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR
              PERFORM START-LN4-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.
       READ-LOANS-4-NEXT.
           PERFORM READ-LN4-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LN-SSNO(1) = BWLNS3-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-VOID )
                    GO TO READ-LOANS-4-NEXT
                 ELSE
                    IF BWLNS3-ALL-LOANS = "N"
                              AND LN-CURBAL = 0
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
              MOVE BWLNS3-KEY TO LN-SSNO(2)
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2
              MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR
              PERFORM START-LN5-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.
       READ-LOANS-5-NEXT.
           PERFORM READ-LN5-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LN-SSNO(2) = BWLNS3-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-VOID )
                    GO TO READ-LOANS-5-NEXT
                 ELSE
                    IF BWLNS3-ALL-LOANS = "N"
                              AND LN-CURBAL = 0
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
      *       MOVE BWLNS3-KEY TO LN-SSNO(3)
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3
      *       MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR
      *       PERFORM START-LN6-FILE-NOT-GREATER
      *       MOVE "Y" TO SKIP-START-FG.
      *READ-LOANS-6-NEXT.
      *    PERFORM READ-LN6-FILE-PREVIOUS.
      *    IF IO-FG = 0
      *       IF LN-SSNO(3) = BWLNS3-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-VOID )
      *             GO TO READ-LOANS-6-NEXT
      *          ELSE
      *             IF BWLNS3-ALL-LOANS = "N"
      *                       AND LN-CURBAL = 0
      *                GO TO READ-LOANS-6-NEXT
      *             ELSE
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
      *       MOVE BWLNS3-KEY TO LN-SSNO(4)
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4
      *       MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR
      *       PERFORM START-LN7-FILE-NOT-GREATER
      *       MOVE "Y" TO SKIP-START-FG.
      *READ-LOANS-7-NEXT.
      *    PERFORM READ-LN7-FILE-PREVIOUS.
      *    IF IO-FG = 0
      *       IF LN-SSNO(4) = BWLNS3-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-VOID )
      *             GO TO READ-LOANS-7-NEXT
      *          ELSE
      *             IF BWLNS3-ALL-LOANS = "N"
      *                       AND LN-CURBAL = 0
      *                GO TO READ-LOANS-7-NEXT
      *             ELSE
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

           IF SKIP-START-FG = " "
              PERFORM SET-LH-WBEG-WEND
              MOVE 99999999 TO LH-ACCTNO QLH1-WEND-ACCTNO
              PERFORM START-LH1-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.
           IF (FLD-SUB = 1 AND LINE-LN-ACCTNO NOT = 0)
              PERFORM SET-LH-WBEG-WEND
              MOVE LINE-LN-ACCTNO TO LH-ACCTNO QLH1-WEND-ACCTNO
              PERFORM START-LH1-FILE-NOT-GREATER
              MOVE "Y" TO SKIP-START-FG.

      ****************************************************************
       READ-LOANS-LH-NEXT.
           PERFORM READ-LH1-FILE-PREVIOUS.
           IF (IO-FG NOT = 0) OR (LH-SSNO NOT = BWLNS3-KEY)
                               OR (LH-BRNO NOT = LH-PATH-OWNBR)
                                OR (BWLNS3-ALL-LOANS = "N")
              MOVE "END" TO IO-TYPE
           ELSE
              IF (LH-ARCHIVE-FG = "Y" AND LH-POCD = "**")
                 GO TO READ-LOANS-LH-NEXT
              END-IF
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

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * STORE LOAN SEQNO FOR EXACT RECALL OF SELECTED LOAN:
           MOVE 1 TO SEQNO(FLD-SUB).

           MOVE LN-ACCTNO TO LINE-LN-ACCTNO.
           PERFORM GET-SP.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-KEY(FLD-SUB).
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LNDATE(FLD-SUB).
           MOVE LN-LNAMT TO D-LOAN-AMT(FLD-SUB).
           MOVE LN-ORGTERM TO D-TRM(FLD-SUB).
           MOVE LN-DLNO TO D-DEALER(FLD-SUB).
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

           IF LN-SSNO(1) = BWLNS3-KEY
              MOVE LN-MAKERCD(1) TO D-MKR(FLD-SUB)
           ELSE
              IF LN-SSNO(2) = BWLNS3-KEY
                 MOVE LN-MAKERCD(2) TO D-MKR(FLD-SUB)
              ELSE
                 IF LN-SSNO(3) = BWLNS3-KEY
                    MOVE LN-MAKERCD(3) TO D-MKR(FLD-SUB)
                 ELSE
                    IF LN-SSNO(4) = BWLNS3-KEY
                       MOVE LN-MAKERCD(4) TO D-MKR(FLD-SUB).

           PERFORM SET-DELINQ-VALUE.

       SETUP-LINE-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINE-HISTORY SECTION.
      ****************************************************

      * STORE HISTORY LOAN SEQNO FOR EXACT RECALL OF SELECTED LOAN:
           MOVE 1 TO SEQNO(FLD-SUB).

           MOVE LH-ACCTNO TO LINE-LN-ACCTNO.
           PERFORM LHSTAT-DISPLAY.
           IF LH-ARCHIVE-FG = "Y"
              MOVE "A" TO LHS-DASH
           ELSE
              MOVE "H" TO LHS-DASH.
           MOVE LHS-DISPLAY-LNNO TO D-KEY(FLD-SUB).
           MOVE LH-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LNDATE(FLD-SUB).
           MOVE LH-LNAMT TO D-LOAN-AMT(FLD-SUB).
           MOVE LH-ORGTERM TO D-TRM(FLD-SUB).
           MOVE LH-DLNO TO D-DEALER(FLD-SUB).
      * LOAN BAL ALWAYS = 0 FOR HISTORY
           MOVE LH-MAKERCD TO D-MKR(FLD-SUB).
           MOVE LH-POCD TO D-POCD(FLD-SUB).
           MOVE LH-PYPROFILE TO D-PROFILE(FLD-SUB).
           INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-".
           MOVE LH-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PAIDOFF(FLD-SUB).

       SETUP-LINE-HISTORY-EXIT.
           EXIT.

      ******************************************************************
       LOAN-INQUIRY SECTION.
      ******************************************************************

      D    STOP MESS.
           MOVE "Y" TO BWLNS3-SCANYN.
           MOVE BWLNS3-ACCTNO TO BWLNS3-LINK-ACCTNO.
           MOVE "SP/XPINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                EXIT-PATHNAME
                                BWLNS3-LINK-BUF.
           CANCEL FORM-PROGX.

       LOAN-INQUIRY-EXIT.
           EXIT.

       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LHSTAT.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SET-LH-WBEG-WEND.
           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE BWLNS3-KEY    TO LH-SSNO
                                 QLH1-WBEG-SSNO
                                 QLH1-WEND-SSNO.
           MOVE ALL "0"       TO QLH1-WBEG-ACCTNO.
       SET-DELINQ-VALUE.
      *    IF ( BWLNS3-DELINQ-CD = "R" )
      *       MOVE LN-RECPROFILE(1) TO D-PROFILE(FLD-SUB)
      *       INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-"
      *    ELSE
      *    IF ( BWLNS3-DELINQ-CD = "C" )
      *       MOVE LN-CONPROFILE(1) TO D-PROFILE(FLD-SUB)
      *       INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-"
      *    ELSE
           IF ( LN-POCD-INRESCISSION )
              MOVE "*RESCISSION*" TO D-PROFILE(FLD-SUB)
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE " *RESCINDED*" TO D-PROFILE(FLD-SUB)
           ELSE
           IF ( LN-POCD-OPEN ) AND ( LN-POFFDATE = 0 )
              MOVE LN-1STPYDATE   TO CEPP1-DATE
              MOVE SYSTEM-DATE    TO CEPP2-DATE
              MOVE D-MKR(FLD-SUB) TO CEPP-MAKERCD
              IF ( GP-NO-LC-IN-ARREARAGE  )
                 PERFORM CONTRACTUAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-DELINQUENT
                             GIVING D-DELINQ(FLD-SUB)
              ELSE
                 PERFORM TOTAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-ARREARAGE
                             GIVING D-DELINQ(FLD-SUB)
           ELSE
              MOVE LN-CONPROFILE(1) TO D-PROFILE(FLD-SUB)
              INSPECT D-PROFILE(FLD-SUB) REPLACING ALL "9" BY "-".

       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.


      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           DISPLAY BWLNS3-SCREEN UPON BWLNS3-WINDOW-HANDLE.
           MOVE BWLNS3-FORM-FIELDS TO WR-BUF.
           MOVE BWLNS3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE BWLNS3-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWLNS3-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BWLNS3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       IO-ROUTINES SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************
       END-ROUTINE SECTION.
      *****************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWLNS3-SCREEN.
           DESTROY BWLNS3-WINDOW-HANDLE.

           MOVE FLDSEL-STAT TO BWLNS3-STATUS.

       STOP-RUN.
           EXIT PROGRAM.
