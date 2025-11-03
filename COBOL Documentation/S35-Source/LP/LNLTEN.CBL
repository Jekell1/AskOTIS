       IDENTIFICATION DIVISION.
       PROGRAM-ID.   LNLTEN.
       DATE-WRITTEN. 061013.
      ******************************************************************
      *                         (LP)
      *   REPORT OF RENEWALS WHERE THE NEW LOAN IS LESS THAN A 10%
      *   INCREASE. TWO METHODS NEEDED.
      *     METHOD 1 = AMT FINANCED ON THE PB LOAN, SUBTRACT THE PRIOR
      *                LOAN PAYOFF, THEN DIVIDE BY THE PRIOR LOAN PAYOFF
      *     METHOD 2 = PROCEEDS ON THE PB LOAN DIVIDED BY THE PRIOR LOAN
      *                PAYOFF
      * WALKS ACROSS BRANCHES
      *
      * REV:
      * BAH 130614 NEW, WORLD PR#896
      * BAH 170608 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0608 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * KEC 2018.0424 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181210 GLOBAL LNFILE
      * BAH 20190926 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *  SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR    PIC 9(4).
               05  WK-CLASS PIC 9(3).
           03  WK-NO-METHOD1        PIC S9(9)     COMP-3.
           03  WK-AMT-METHOD1       PIC S9(9)V99  COMP-3.
           03  WK-NO-METHOD2        PIC S9(9)     COMP-3.
           03  WK-AMT-METHOD2       PIC S9(9)V99  COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LNLTEN.CBL S35 02/20/24-13:07:55 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  WK-IFN               PIC X(4)  VALUE "WK".

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL  OCCURS 10 TIMES.
               05  WS-GTT-CLASS1-5       OCCURS 2 TIMES.
                   07  WS-GTT-NO-METHOD1    PIC S9(9)       COMP-3.
                   07  WS-GTT-AMT-METHOD1   PIC S9(9)V99    COMP-3.
                   07  WS-GTT-NO-METHOD2    PIC S9(9)       COMP-3.
                   07  WS-GTT-AMT-METHOD2   PIC S9(9)V99    COMP-3.

       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.
       01  HOLD-LN-CLASS        PIC 9(3)  VALUE 0.
       01  HOLD-METHOD          PIC X     VALUE SPACES.


       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  WS-AMOUNT       PIC S9(9)V99  VALUE 0.
       01  WS-AMTFIN       PIC S9(9)V99  VALUE 0.
       01  PRIOR-PAYOFF    PIC S9(9)V99  VALUE 0.
       01  TOT-NO          PIC S9(9)     VALUE 0.
       01  TOT-AMT         PIC S9(11)V99  VALUE 0.
       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".


       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
       01  LEGEND-DTE-01.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING LOAN DATE:".
       01  LEGEND-DTE-02.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING LOAN DATE:".

      *-----------------------------------------------------------------
      *     S C R E E N   F I E L D S   *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  INCLUDE-DETAIL  PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  GTT-ISO         PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.


      *-----------------------------------------------------------------
      *     P R I N T   B U F F E R S   *
      *-----------------------------------------------------------------

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(18)    VALUE " ".
           03  H-NAME           PIC X(50)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(20)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
               "NEW LOANS < 10% INCREASE REPORT".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(56)  VALUE
           "BRNO   ACCTNO          SRC  LOANDATE      AMTFIN        ".
           03  FILLER           PIC X(24)  VALUE
           "PR POFF   PROCEEDS     M".

       01  TOTAL-HEAD1.
           03  FILLER           PIC X(20) VALUE
           "BRANCH/GROUP".
           03  FILLER           PIC X(15) VALUE SPACES.
           03  FILLER           PIC X(7)  VALUE
           "FROM:  ".
           03  TOT-BEG-DATE     PIC 99/99/99.
           03  FILLER           PIC X(6)  VALUE
           " TO:  ".
           03  TOT-END-DATE     PIC 99/99/99.
       01  TOTAL-HEAD2.
           03  FILLER           PIC X(31) VALUE SPACES.
           03  FILLER           PIC X(11) VALUE SPACES.
           03  FILLER           PIC X(8)  VALUE
           "METHOD 1".
           03  FILLER           PIC X(26) VALUE SPACES.
           03  FILLER           PIC X(8)  VALUE
           "METHOD 2".

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO              PIC 9(4).
           03  FILLER              PIC X(3).
           03  D-LOAN-NUM          PIC X(13).
           03  FILLER              PIC X(3).
           03  D-SRCD              PIC X(3).
           03  FILLER              PIC X(3).
           03  D-LOANDATE          PIC 99/99/99.
           03  FILLER              PIC X(3).
           03  D-AMTFIN            PIC Z(5)9.99.
           03  FILLER              PIC X(6).
           03  D-PRPOFF            PIC Z(5)9.99.
           03  FILLER              PIC X(2).
           03  D-PROCEEDS          PIC Z(5)9.99.
           03  FILLER              PIC X(2).
           03  D-METHOD            PIC X.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
               05  FILLER           PIC X(6).

           03  FILLER       PIC X(2).
           03  G-CLASS-PROMPT       PIC X(9).
           03  G-NO-1       PIC ZZZZ,ZZZ,ZZ9-.
           03  G-AMT-1      PIC ZZZZ,ZZZ,ZZ9.99-.
           03  G-NO-2       PIC ZZZZ,ZZZ,ZZ9-.
           03  G-AMT-2      PIC ZZZZ,ZZZ,ZZ9.99-.


      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LNLTEN_DEF.CPY".
       COPY "LIBLP/LNLTEN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/LNLTEN_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***************************************************
       MAIN-MODULE SECTION.
      ***************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNLTEN" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              BW-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO TO LN-OWNBR.
           MOVE 1     TO LN-CLASS.
           MOVE 0     TO LN-ACCTNO.

           PERFORM START-LN3-FILE.

      *-----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * '**', '*I' "*R' "*U'
           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              GO TO NEXT-LN.

      * ONLY LOOK AT LOANS WITH A SOURCE CODE OF PB
           IF LN-SRCD NOT = "PB"
              GO TO NEXT-LN.

           IF LN-CLASS NOT = 1
              IF LN-CLASS NOT = 5
                 GO TO NEXT-LN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO NEXT-LN.

           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-BRANCH-HEADINGS.

      *-----------------------------------------------------------------
      * TEST BREAK IN BRANCH
      *-----------------------------------------------------------------
           IF BR-NO NOT = HOLD-BRNO
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              MOVE LN-CLASS TO HOLD-LN-CLASS
              IF INCLUDE-DETAIL = "Y"
                 PERFORM SET-BRANCH-HEADINGS.

           PERFORM LN-DETAIL.

           GO TO NEXT-LN.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.

           IF ( GTT-ISO NOT = "S" )
              MOVE 99 TO LN-COUNT
              PERFORM GROUP-TOTAL-TABLE
              PERFORM PRINT-RANGE-LINES
           ELSE
              PERFORM PRINT-RANGE-LINES.


           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LNLTEN-QUEUE-POS TO Q-POS.
           MOVE LNLTEN-COPIES-POS TO C-POS.
           MOVE LNLTEN-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                   END-BRANCH.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE

           MOVE "N" TO RECORDS-FOUND.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE.

      * CAN NOT INITIALIZE THESE OR IT WIPES OUT THE EXTERNAL VARIABLES
      *    MOVE SPACES TO TOTAL-LEV-TABLE.
      *    MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO INCL-DETAIL.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "PLEASE ENTER A GROUP" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       ENT-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO GTT-ISO-PARAGRAPH.

           MOVE LEGEND-DTE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENT-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------------
       ENT-END-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO GTT-ISO-PARAGRAPH.
           MOVE LEGEND-DTE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENT-BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENT-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "END DATE CANNOT BE PRIOR TO BEGINNING DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENT-BEG-DATE.

       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO INCL-DETAIL.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO BR-01
              ELSE
                 GO TO ENT-END-DATE.

           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

       INCL-DETAIL.
           IF GTT-ISO = "O"
              MOVE "S   010DETAIL." TO VDU
              MOVE "N" TO INCLUDE-DETAIL VDUBUF
              PERFORM SCREENIO
              GO TO END-PARM.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENT-END-DATE.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO INCL-DETAIL.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO INCL-DETAIL.

           MOVE VDUBUF TO INCLUDE-DETAIL.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       LN-DETAIL SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
               MOVE SPACES TO BW-REC.

           PERFORM LPSTAT-DISPLAY.
           MOVE BR-NO            TO D-BRNO.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-SRCD          TO D-SRCD.
           MOVE LN-LOANDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-LOANDATE.

           PERFORM LOAN-CALCULATIONS.

           COMPUTE WS-AMTFIN = TOTAL-NOTE - LN-INTCHG - LN-SERCHG.
           COMPUTE PRIOR-PAYOFF ROUNDED = LN-PRBAL + LN-PRINTDUE
                                       - LN-PRFINCHGRB - LN-PRINSRB.


           MOVE WS-AMTFIN        TO D-AMTFIN.
           MOVE PRIOR-PAYOFF     TO D-PRPOFF.
           MOVE LN-PROCEEDS      TO D-PROCEEDS.

           MOVE SPACES TO HOLD-METHOD.
      * METHOD 1 = AMOUNT FINANCED ON THE PB LOAN, SUBTRACT PRIOR LOAN PAYOFF,
      *            TAKE THAT AMT AND DIVIDE BY THE PRIOR LOAN PAYOFF, IF
      *            THE RESULT IS < 10% (.10), ADD TO METHOD ONE
           COMPUTE WS-AMOUNT ROUNDED =
                         (WS-AMTFIN - PRIOR-PAYOFF) / PRIOR-PAYOFF.
           IF WS-AMOUNT < .10
              MOVE "1" TO HOLD-METHOD.

      * METHOD 2 = PROCEEDS ON THE PB LOAN AND DIVIDE BY THE PRIOR
      *            LOAN PAYOFF, IF THE RESULT IS < 10% (.10),
      *            ADD TO METHOD TWO

           COMPUTE WS-AMOUNT ROUNDED = LN-PROCEEDS  / PRIOR-PAYOFF.
           IF WS-AMOUNT < .10
              IF HOLD-METHOD = "1"
                 MOVE "B" TO HOLD-METHOD
              ELSE
                 MOVE "2" TO HOLD-METHOD.

           MOVE HOLD-METHOD TO D-METHOD.

           IF INCLUDE-DETAIL = "Y"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

       END-LN-DETAIL.
           PERFORM CREATE-WK.

       EXIT-LN-DETAIL.
           EXIT.

      **********************
       CREATE-WK SECTION.
      **********************

           MOVE BR-NO TO SAVE-WK-BR.
           MOVE LN-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NO-METHOD1 WK-AMT-METHOD1
                     WK-NO-METHOD2 WK-AMT-METHOD2.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
      * METHOD 1 = AMOUNT FINANCED ON THE PB LOAN, SUBTRACT PRIOR LOAN PAYOFF,
      *            TAKE THAT AMT AND DIVIDE BY THE PRIOR LOAN PAYOFF, IF
      *            THE RESULT IS < 10% (.10), ADD TO METHOD ONE
           COMPUTE WS-AMOUNT ROUNDED =
                         (WS-AMTFIN - PRIOR-PAYOFF) / PRIOR-PAYOFF.
           IF WS-AMOUNT < .10
              ADD 1 TO WK-NO-METHOD1
              ADD TOTAL-NOTE TO WK-AMT-METHOD1.

      * METHOD 2 = PROCEEDS ON THE PB LOAN AND DIVIDE BY THE PRIOR
      *            LOAN PAYOFF, IF THE RESULT IS < 10% (.10),
      *            ADD TO METHOD TWO

           COMPUTE WS-AMOUNT ROUNDED = LN-PROCEEDS  / PRIOR-PAYOFF.
           IF WS-AMOUNT < .10
              ADD 1 TO WK-NO-METHOD2
              ADD TOTAL-NOTE TO WK-AMT-METHOD2.

       UPDATE-WK.
           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           IF WK-CLASS = 1
             ADD WK-NO-METHOD1 TO WS-GTT-NO-METHOD1(GTT-TOTAL-SUB, 1)
             ADD WK-AMT-METHOD1 TO WS-GTT-AMT-METHOD1(GTT-TOTAL-SUB, 1)
             ADD WK-NO-METHOD2 TO WS-GTT-NO-METHOD2(GTT-TOTAL-SUB, 1)
             ADD WK-AMT-METHOD2 TO WS-GTT-AMT-METHOD2(GTT-TOTAL-SUB, 1)
           ELSE
            ADD WK-NO-METHOD1 TO WS-GTT-NO-METHOD1(GTT-TOTAL-SUB,2)
            ADD WK-AMT-METHOD1 TO WS-GTT-AMT-METHOD1(GTT-TOTAL-SUB,2)
            ADD WK-NO-METHOD2 TO WS-GTT-NO-METHOD2(GTT-TOTAL-SUB,2)
            ADD WK-AMT-METHOD2 TO WS-GTT-AMT-METHOD2(GTT-TOTAL-SUB,2).

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
           MOVE SPACES      TO WK-KEY.
           MOVE WGR-SRESULT TO WK-BR.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

        GTT-NEXT-WK1.
           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ************************************
           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      *********************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      *********************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           MOVE GR-LEVEL  TO G-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      *****************************
       GTT-PRINT-TOTALS SECTION.
      *****************************

           MOVE "CLASS 1: " TO G-CLASS-PROMPT.
           MOVE WS-GTT-NO-METHOD1(GTT-TOTAL-SUB, 1) TO G-NO-1.
           MOVE WS-GTT-AMT-METHOD1(GTT-TOTAL-SUB, 1) TO G-AMT-1.
           MOVE WS-GTT-NO-METHOD2(GTT-TOTAL-SUB, 1) TO G-NO-2.
           MOVE WS-GTT-AMT-METHOD2(GTT-TOTAL-SUB, 1) TO G-AMT-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CLASS 5: " TO G-CLASS-PROMPT.
           MOVE WS-GTT-NO-METHOD1(GTT-TOTAL-SUB, 2) TO G-NO-1.
           MOVE WS-GTT-AMT-METHOD1(GTT-TOTAL-SUB, 2) TO G-AMT-1.
           MOVE WS-GTT-NO-METHOD2(GTT-TOTAL-SUB, 2) TO G-NO-2.
           MOVE WS-GTT-AMT-METHOD2(GTT-TOTAL-SUB, 2) TO G-AMT-2.
           MOVE 1                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL  : " TO G-CLASS-PROMPT.
           COMPUTE TOT-NO =
              WS-GTT-NO-METHOD1(GTT-TOTAL-SUB, 1) +
              WS-GTT-NO-METHOD1(GTT-TOTAL-SUB, 2).
           MOVE TOT-NO TO G-NO-1.
           COMPUTE TOT-AMT =
              WS-GTT-AMT-METHOD1(GTT-TOTAL-SUB, 1) +
              WS-GTT-AMT-METHOD1(GTT-TOTAL-SUB, 2).
           MOVE TOT-AMT TO G-AMT-1.
           COMPUTE TOT-NO =
              WS-GTT-NO-METHOD2(GTT-TOTAL-SUB, 1) +
              WS-GTT-NO-METHOD2(GTT-TOTAL-SUB, 2).
           MOVE TOT-NO TO G-NO-2.
           COMPUTE TOT-AMT =
              WS-GTT-AMT-METHOD2(GTT-TOTAL-SUB, 1) +
              WS-GTT-AMT-METHOD2(GTT-TOTAL-SUB, 2).
           MOVE TOT-AMT TO G-AMT-2.

           MOVE 1                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF GTT-HEADER-FG NOT = "Y"
              MOVE HEAD3 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE 2 TO LN-FEED
              MOVE BEG-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO TOT-BEG-DATE
              MOVE END-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO TOT-END-DATE
              MOVE TOTAL-HEAD1 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 1 TO LN-FEED
              MOVE TOTAL-HEAD2 TO PR-REC
              PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO LNLTEN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNLTEN-SYSDATE.
           DISPLAY LNLTEN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNLTEN-FORM-FIELDS TO WR-BUF.
           MOVE LNLTEN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LNLTEN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNLTEN-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LNLTEN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE HOLD-BRNAME TO H-BRDESC.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      ******************************************************
       IO-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LNLTEN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
