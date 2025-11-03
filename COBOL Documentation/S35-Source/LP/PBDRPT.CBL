       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PBDRPT.
       DATE-WRITTEN. 051613.
      ******************************************************************
      *                         (LP)
      *    DESCRIPTION:  FINDS PBD ACCOUNTS OPEN ON BEG-DATE
      *                  ACCUM TOTALS BASED ON WHAT STATUS OF ACCT
      *                  WAS ON ENDING DATE (PAID OUT, PL'ED, ETC)
      *                        (LNFILE)
      *                  GROUP TOTALS WITH OPTIONAL DETAIL LINE
      * WALKS ACROSS BRANCHES
      *
      * REV:
      *  BAH 170711 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0423 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 2018.1211 GLOBAL LNFILE
      * BAH 2019.0924 REMOVED ACCESS-CALL LNFILE
      * BAH 2020.0827 REMOVED RESET ON BR1 
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
           03  WK-NOLOANS           PIC S9(9)     COMP-3.
           03  WK-PAIDOUT           PIC S9(9)     COMP-3.
           03  WK-PB                PIC S9(9)     COMP-3.
           03  WK-PL                PIC S9(9)     COMP-3.
           03  WK-PLAMT             PIC S9(9)V99  COMP-3.
           03  WK-OPEN              PIC S9(9)     COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PBDRPT.CBL S35 02/20/24-14:50:24 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
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
               05  WS-GTT-NOLOANS        PIC S9(9)     COMP-3.
               05  WS-GTT-PAIDOUT        PIC S9(9)     COMP-3.
               05  WS-GTT-PB             PIC S9(9)     COMP-3.
               05  WS-GTT-PL            PIC S9(9)     COMP-3.
               05  WS-GTT-PLAMT          PIC S9(9)V99  COMP-3.
               05  WS-GTT-OPEN           PIC S9(9)     COMP-3.


       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".


       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
       01  LEGEND-DTE-01.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING PAYOFF DATE:".
       01  LEGEND-DTE-02.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING PAYOFF DATE:".
       01  LEGEND-AC-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING ACCOUNT ".
           03  FILLER           PIC X(17)  VALUE
               "NUMBER. (F3-ALL):".
       01  LEGEND-AC-02.
           03  FILLER           PIC X(37)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING ACCOUNT ".
           03  FILLER           PIC X(8)  VALUE
               "NUMBER.:".

      *-----------------------------------------------------------------
      *     S C R E E N   F I E L D S   *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
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
           03  FILLER           PIC X(29) VALUE
               "PB DELINQUENT ACTIVITY REPORT".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(45)  VALUE
           "BRNO   ACCTNO          ENT DATE  POFF DATE   ".
           03  FILLER           PIC X(54)  VALUE
           "  POCD   PL DATE        PL AMT ".

       01  TOTAL-HEAD1.
           03  FILLER           PIC X(20) VALUE
           "BRANCH/GROUP".
           03  FILLER           PIC X(7)  VALUE
           "AS OF: ".
           03  TOT-BEG-DATE     PIC 99/99/99.
           03  FILLER           PIC X(3)  VALUE SPACES.
           03  FILLER           PIC X(23) VALUE
           "  -------------------  ".
           03  FILLER           PIC X(7)  VALUE
           "AS OF: ".
           03  TOT-END-DATE     PIC 99/99/99.
           03  FILLER           PIC X(24) VALUE
           "  --------------------  ".
       01  TOTAL-HEAD2.
           03  FILLER           PIC X(22) VALUE SPACES.
           03  FILLER           PIC X(13) VALUE
           "  OPEN LOANS ".
           03  FILLER           PIC X(13) VALUE
           "    PAID OUT ".
           03  FILLER           PIC X(13) VALUE
           "       PB'ED ".
           03  FILLER           PIC X(13) VALUE
           " CHARGED OFF ".
           03  FILLER           PIC X(15) VALUE
           "CHARGE-OFF AMT ".
           03  FILLER           PIC X(13) VALUE
           "       OPEN  ".

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO              PIC 9(4).
           03  FILLER              PIC X(3).
           03  D-LOAN-NUM          PIC X(13).
           03  FILLER              PIC X(3).
           03  D-ENTDATE           PIC 99/99/99.
           03  FILLER              PIC X(3).
           03  D-POFFDATE          PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X(6).
           03  D-POCD              PIC XX.
           03  FILLER              PIC X(3).
           03  D-PLDATE            PIC 99/99/99 BLANK ZERO.
           03  D-PLAMT             PIC ZZZ,ZZZ,ZZZ.99.
           03  FILLER              PIC X(3).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-PROMPT1        PIC X(7).
           03  FILLER           PIC X.
           03  G-GRNO           PIC X(4).
           03  G-BRNO           REDEFINES G-GRNO
                                PIC 9(4).
           03  FILLER           PIC X.
           03  G-GR-LEVEL       PIC 9(1).
           03  FILLER           PIC X(6).

           03  FILLER       PIC X(2).
           03  G-LOANS      PIC ZZZZ,ZZZ,ZZ9-.
           03  G-PAIDOUT    PIC ZZZZ,ZZZ,ZZ9-.
           03  G-PB         PIC ZZZZ,ZZZ,ZZ9-.
           03  G-PL         PIC ZZZZ,ZZZ,ZZ9-.
           03  G-PLAMT      PIC ZZZ,ZZZ,ZZZ.99.
           03  G-OPEN       PIC ZZZZ,ZZZ,ZZ9-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.
               04  R-NUM-REDEF2 REDEFINES R-NUM.
                   05  R-DATE   PIC 9999/99/99.

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
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/PBDRPT_DEF.CPY".
       COPY "LIBLP/PBDRPT_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/PBDRPT_SCN.CPY".

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

      ****************************************************
       MAIN-MODULE SECTION.
      ****************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PBDRPT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

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

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

      *-----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * '**', '*I' "*R' "*U'
           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

           IF LN-ENTDATE > BEG-DATE
              GO TO NEXT-LN.

           IF LN-POFFDATE NOT = 0
              IF LN-POFFDATE NOT > BEG-DATE
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF LN-PLDATE NOT > BEG-DATE
                 GO TO NEXT-LN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM GET-SRCD-SUB.
           IF NOT SRCD-IS-PBD
              GO TO NEXT-LN.
      *-----------------------------------------------------------------
      * TEST FIRST TIME THROUGH
      *-----------------------------------------------------------------
       CONTINUE-PROCESSING.
           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              PERFORM SET-BRANCH-HEADINGS.

      *-----------------------------------------------------------------
      * TEST BREAK IN BRANCH
      *-----------------------------------------------------------------
           IF BR-NO NOT = HOLD-BRNO
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
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
           MOVE PBDRPT-QUEUE-POS TO Q-POS.
           MOVE PBDRPT-COPIES-POS TO C-POS.
           MOVE PBDRPT-PRU-POS TO P-POS.
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
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE "N" TO RECORDS-FOUND.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

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
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO ENT-BEG-DATE.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENT-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO GTT-ISO-PARAGRAPH.

           MOVE LEGEND-DTE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
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
                 GO TO CL-02
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
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
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
           MOVE LN-ENTDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-ENTDATE.
           MOVE LN-POFFDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-POFFDATE.
           MOVE LN-PO-TRCD       TO D-POCD.
           MOVE LN-PLDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-PLDATE.
           MOVE LN-PLAMT         TO D-PLAMT.

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

           MOVE BR-NO    TO SAVE-WK-BR.
           MOVE LN-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS
                     WK-PAIDOUT
                     WK-PB
                     WK-PL
                     WK-PLAMT
                     WK-OPEN.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           ADD 1 TO WK-NOLOANS.

           IF LN-POFFDATE NOT = 0
              IF LN-POFFDATE NOT > END-DATE
                 IF LN-PO-TRCD = "PB"
                    ADD 1 TO WK-PB
                    GO TO UPDATE-WK
                 ELSE
                    ADD 1 TO WK-PAIDOUT
                    GO TO UPDATE-WK.

           IF LN-PLDATE NOT = 0
              IF LN-PLDATE NOT > END-DATE
                 ADD 1 TO WK-PL
                 ADD LN-PLAMT TO WK-PLAMT
                 GO TO UPDATE-WK.

           ADD 1 TO WK-OPEN.

       UPDATE-WK.
           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           ADD WK-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB).
           ADD WK-PAIDOUT TO WS-GTT-PAIDOUT(GTT-TOTAL-SUB).
           ADD WK-PB      TO WS-GTT-PB(GTT-TOTAL-SUB).
           ADD WK-PL      TO WS-GTT-PL(GTT-TOTAL-SUB).
           ADD WK-PLAMT   TO WS-GTT-PLAMT(GTT-TOTAL-SUB).
           ADD WK-OPEN    TO WS-GTT-OPEN(GTT-TOTAL-SUB).

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
       GTT-START-WK1.
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
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB) TO G-LOANS.
           MOVE WS-GTT-PAIDOUT(GTT-TOTAL-SUB) TO G-PAIDOUT.
           MOVE WS-GTT-PB(GTT-TOTAL-SUB)      TO G-PB.
           MOVE WS-GTT-PL(GTT-TOTAL-SUB)      TO G-PL.
           MOVE WS-GTT-PLAMT(GTT-TOTAL-SUB)   TO G-PLAMT.
           MOVE WS-GTT-OPEN(GTT-TOTAL-SUB)    TO G-OPEN.

           MOVE 2                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      **************************************************************************
       GET-SRCD-SUB SECTION.
      **************************************************************************

      * THIS LOGIC IS JUST LIKE LIBLP/LPSRCD, EXCEPT I STARRED OF
      * THE LINE THAT CHECKS FOR LN-PLCD = " ".  THESE ACCOUNTS MAY BE
      * CHARGED OFF, BUT WEREN'T CHARGED OFF AS OF THE BEG DATE, & WE
      * WANT TO KNOW IF THEY WERE PBD'S, DON'T CARE ABOUT PL SOURCE CODES  BARB

           MOVE 0 TO SRCD.
           MOVE "N" TO SRCD-FG.

           PERFORM GET-SRCD-SUB-NO-PL.
           GO TO EXIT-GET-SRCD.

       EXIT-GET-SRCD.
           EXIT.

      **************************************************************************
       GET-SRCD-SUB-NO-PL SECTION.
      **************************************************************************

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE 2 TO SRCD
                 GO TO GET-SRCD-SUB-EXIT
              ELSE
                 IF LN-SRCD = "RF"
                    MOVE 9 TO SRCD
                    GO TO GET-SRCD-SUB-EXIT.

           IF LN-SRCD = "NB"
              MOVE 1 TO SRCD
           ELSE
           IF LN-SRCD = "RF"
              MOVE 2 TO SRCD
           ELSE
           IF LN-SRCD = "FB"
              MOVE 3 TO SRCD
           ELSE
           IF LN-SRCD = "PB"
              PERFORM GET-SRCD-PB
           ELSE
           IF LN-SRCD = "RN"
              MOVE 7 TO SRCD
           ELSE
           IF LN-SRCD = "DM" AND GP-NO-SC-USE-DM = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "SC" AND GP-NO-SC-USE-DM NOT = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "RB"
              MOVE 10 TO SRCD
           ELSE
           IF LN-SRCD = "RO"
              MOVE 11 TO SRCD
           ELSE
              MOVE 9 TO SRCD.
       GET-SRCD-SUB-EXIT.
           EXIT.

      **************************************************************************
       GET-SRCD-PB SECTION.
      **************************************************************************

      * DETERMINE IF ACCOUNT IS 'PB' 'PBD' OR 'PBI'
           MOVE 4 TO SRCD.
           IF SP-PBDEL-DAYS NOT = 0
              IF LN-PRPDUE-DAYS > SP-PBDEL-DAYS
      * PBD
                 MOVE 5 TO SRCD
                 GO TO GET-SRCD-PB-EXIT.
           IF SP-PBINCR-PERCENT NOT = 0
              IF SP-PBINCR-PERCENT = 1
                 IF LN-LNAMT > LN-PRLNAMT
      * PBI
                    MOVE 6 TO SRCD
                 END-IF
              ELSE
                 COMPUTE SRCD-WORKER ROUNDED =
                     LN-PRLNAMT - 0.01
                         + LN-PRLNAMT * SP-PBINCR-PERCENT / 100
                 IF LN-LNAMT > SRCD-WORKER
                    MOVE 6 TO SRCD.

       GET-SRCD-PB-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE HOLD-BRNAME TO H-BRDESC.

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
              MOVE TOTAL-HEAD2 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
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
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG PL DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END PL DATE" TO RANGE-LINE.
           MOVE END-DATE   TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO PBDRPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PBDRPT-SYSDATE.
           DISPLAY PBDRPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PBDRPT-FORM-FIELDS TO WR-BUF.
           MOVE PBDRPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE PBDRPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE PBDRPT-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PBDRPT_EVA.CPY".
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
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
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
              DESTROY PBDRPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
