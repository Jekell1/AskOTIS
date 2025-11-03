       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PBDRP3.
       DATE-WRITTEN. 052113.
      ******************************************************************
      *                         (LP)
      *    DESCRIPTION:  FINDS ACCOUNTS OPEN ON BEG-DATE
      *                  INCLUDE ACCTS BASED ON REC OR CONTRACTUAL
      *                  STATUS (FROM PROFILE) ON BEG DATE
      *                  ACCUM TOTALS BASED ON WHAT STATUS OF ACCT
      *                  WAS ON ENDING DATE (PAID OUT, PL'ED, ETC)
      *                        (LNFILE)
      *                  GROUP TOTALS WITH OPTIONAL DETAIL LINE
      * WALKS ACROSS BRANCHES
      *
      * REV:
      * BLM 130521 NEW, WORLD PR# 886
      * BLM 140112 FIXED ROUTINE TO GET POSTING DATE OF CHARGE-OFF. WHEN
      *            READING LP FOR PL POST DATE, IT WAS CORRECT FOR P2-P5
      *            BUF NOT FOR PL, SO PL'S WOULD HAVE BEEN USING PLDATE
      *            FROM LNFILE,  WORLD PR# 1045/TEST-PL
      *  BAH 170711 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0423 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *               LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *               WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *               LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *               PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 2018.1211 GLOBAL LNFILE
      * BAH 2019.0919 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2020.0827 REMOVED RESET ON BR1 
      * BAH 2021.1021 DONT NEED REPOSITIONING OF KEY FOR LN3-FILE-NEXT.
      *               ORIGINAL CURSOR HOLDS RECORDS, COMMENTED OUT
      *               START-LN3-FILE-GREATER
      * BAH 2022.0803 HARD CODED START
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
           03  WK-PB-PL             PIC S9(9)     COMP-3.
           03  WK-PB-PLAMT          PIC S9(9)V99  COMP-3.
           03  WK-PB-PAIDOUT        PIC S9(9)     COMP-3.
           03  WK-PB-OPEN           PIC S9(9)     COMP-3.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PBDRP3.CBL S34 05/24/21-08:10:07 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
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
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

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

       01  HOLD-PROFILE-5       PIC 9(12) OCCURS 5.
       01  HOLD-PROFILEY REDEFINES HOLD-PROFILE-5.
           03  HOLD-PROFILE     PIC 9 OCCURS 60.

       01  PROFILE-DATE         PIC 9(8).
       01  TOTAL-SKIPPED        PIC 9(7) VALUE 0.
       01  HOLD-PL-TRDATE       PIC 9(8).
       01  TEST-LN-PLCD.
           03  TEST-PLCD        PIC X.
           03  TEST-PLTYPE      PIC X.
       01  HOLD-PO-TRDATE       PIC 9(8).
       01  TEST-LN-POCD         PIC XX.

       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL  OCCURS 10 TIMES.
               05  WS-GTT-NOLOANS        PIC S9(9)     COMP-3.
               05  WS-GTT-PAIDOUT        PIC S9(9)     COMP-3.
               05  WS-GTT-PB             PIC S9(9)     COMP-3.
               05  WS-GTT-PL             PIC S9(9)     COMP-3.
               05  WS-GTT-PLAMT          PIC S9(9)V99  COMP-3.
               05  WS-GTT-OPEN           PIC S9(9)     COMP-3.
               05  WS-GTT-PB-PL          PIC S9(9)     COMP-3.
               05  WS-GTT-PB-PLAMT       PIC S9(9)V99  COMP-3.
               05  WS-GTT-PB-PAIDOUT     PIC S9(9)     COMP-3.
               05  WS-GTT-PB-OPEN        PIC S9(9)     COMP-3.


       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP.

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.

       01  HOLD-LN3-KEY.
           03  HOLD-LN-OWNBR        PIC 9(4).
           03  HOLD-LN-CLASS        PIC 9(3).
           03  HOLD-LN-ACCTNO       PIC 9(8).
       01  PB-COUNT                     PIC 99.
       01  PRIOR-LOAN-TO-FIND           PIC 9(8).
       01  HOLD-SSNO                    PIC 9(9).

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
           03  MONTH-END-DATE  PIC 9(8).
           03  PBD-FG          PIC X.
           03  RECCON          PIC X.
           03  DEL-SUB         PIC 9.
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
           03  FILLER           PIC X(25)    VALUE " ".
           03  H-NAME           PIC X(50)    VALUE " ".
           03  FILLER           PIC X(33)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(29) VALUE
               "DELINQUENT ACTIVITY REPORT: ".
           03  H-DEL            PIC X(5).
           03  FILLER           PIC X(5)   VALUE "DAYS".
           03  FILLER           PIC X      VALUE " ".
           03  H-RECCON         PIC X(13).
           03  FILLER           PIC X(4)   VALUE " ".
           03  H-PBDS           PIC X(14)   VALUE " ".
           03  FILLER           PIC X(4)   VALUE " ".
           03  FILLER           PIC X(11)   VALUE "MONTH-END: ".
           03  H-MONTH-END      PIC 99/99/99.

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(29)  VALUE
           "BRNO LOAN#  ENT DATE  PODATE ".
           03  FILLER           PIC X(20)  VALUE
           " CD PLDATE   PL AMT ".
           03  FILLER           PIC X(28)  VALUE
           "PB LN# PLDATE   PODATE  CD  ".
           03  FILLER           PIC X(28)  VALUE
           "PB LN# PLDATE   PODATE  CD  ".
           03  FILLER           PIC X(28)  VALUE
           "PB LN# PLDATE   PODATE  CD  ".

       01  TOTAL-HEAD1.
           03  FILLER           PIC X(16) VALUE
           "BR/GROUP".
           03  FILLER           PIC X(7)  VALUE
           "AS OF: ".
           03  TOT-BEG-DATE     PIC 99/99/99.
           03  FILLER           PIC X(41) VALUE
           "  ------------------------------------  ".
           03  FILLER           PIC X(7)  VALUE
           "AS OF: ".
           03  TOT-END-DATE     PIC 99/99/99.
           03  FILLER           PIC X(42) VALUE
           "  --------------------------------------  ".
       01  TOTAL-HEAD2.
           03  FILLER           PIC X(21) VALUE SPACES.
           03  FILLER           PIC X(11) VALUE
           "OPEN LOANS ".
           03  FILLER           PIC X(10) VALUE
           " PAID OUT ".
           03  FILLER           PIC X(10) VALUE
           "    PB'ED ".
           03  FILLER           PIC X(10) VALUE
           "CHRGD OFF ".
           03  FILLER           PIC X(15) VALUE
           "   CHG-OFF AMT ".
           03  FILLER           PIC X(10) VALUE
           "    OPEN  ".
           03  FILLER           PIC X(24) VALUE
           "    PB'ED & CHARGED OFF ".
           03  FILLER           PIC X(10) VALUE
           " PB/PDOUT ".
           03  FILLER           PIC X(10) VALUE
           " PB/OPEN ".

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO              PIC 9(4).
           03  FILLER              PIC X.
           03  D-NUMBER            PIC ZZ9999.
           03  FILLER              PIC X.
           03  D-ENTDATE           PIC 99/99/99.
           03  FILLER              PIC X.
           03  D-POFFDATE          PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X.
           03  D-POCD              PIC XX.
           03  FILLER              PIC X.
           03  D-PLDATE            PIC 99/99/99 BLANK ZERO.
           03  D-PLAMT             PIC ZZZZ.99.
           03  FILLER              PIC X.
           03  D-PB-INFO OCCURS 3.
               05  D-PB-NUMBER      PIC ZZ9999.
               05  FILLER           PIC X.
               05  D-PB-PLDATE      PIC 99/99/99 BLANK ZERO.
               05  FILLER           PIC X.
               05  D-PB-POFFDATE    PIC 99/99/99 BLANK ZERO.
               05  FILLER           PIC X.
               05  D-PB-POCD        PIC XX.
               05  FILLER           PIC X.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-PROMPT1        PIC X(7).
           03  FILLER           PIC X.
           03  G-GRNO           PIC X(4).
           03  G-BRNO           REDEFINES G-GRNO
                                PIC 9(4).
           03  FILLER           PIC XX.
           03  FILLER           PIC X(6).

           03  FILLER       PIC X(2).
           03  G-LOANS      PIC Z,ZZZ,ZZ9-.
           03  G-PAIDOUT    PIC Z,ZZZ,ZZ9-.
           03  G-PB         PIC Z,ZZZ,ZZ9-.
           03  G-PL         PIC Z,ZZZ,ZZ9-.
           03  G-PLAMT      PIC ZZZ,ZZZ,ZZZ.99.
           03  G-OPEN       PIC Z,ZZZ,ZZ9-.
           03  G-PB-PL      PIC Z,ZZZ,ZZ9-.
           03  G-PB-PLAMT   PIC ZZZ,ZZZ,ZZZ.99.
           03  G-PB-PAIDOUT PIC Z,ZZZ,ZZ9-.
           03  G-PB-OPEN    PIC Z,ZZZ,ZZ9-.

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
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/PBDRP3_DEF.CPY".
       COPY "LIBLP/PBDRP3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/PBDRP3_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***************************************************
       MAIN-MODULE SECTION.
      ***************************************************

           PERFORM SQL-CONNECT.
       
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PBDRP3" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE "B"                        TO LP-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LP1-FILE.

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
              PERFORM GET-PO-TRANS-DATE
              IF HOLD-PO-TRDATE NOT = 0
                 IF HOLD-PO-TRDATE NOT > BEG-DATE
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              PERFORM GET-PL-TRANS-DATE
              IF HOLD-PL-TRDATE NOT = 0
                 IF HOLD-PL-TRDATE NOT > BEG-DATE
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

       CHECK-PBD-ACCTS.
           IF PBD-FG = "I"
              GO TO CHECK-BEG-DATE-DELINQUENCY.
           PERFORM GET-SRCD-SUB.
           IF PBD-FG = "O"
              IF NOT SRCD-IS-PBD
                 GO TO NEXT-LN.
           IF PBD-FG = "S"
              IF SRCD-IS-PBD
                 GO TO NEXT-LN.

       CHECK-BEG-DATE-DELINQUENCY.
           MOVE MONTH-END-DATE TO PROFILE-DATE.
           IF LN-POFFDATE NOT = 0
              IF HOLD-PO-TRDATE NOT > MONTH-END-DATE
                 MOVE HOLD-PO-TRDATE TO PROFILE-DATE.

           IF RECCON = "C"
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                 MOVE LN-CONPROFILE(SUB) TO HOLD-PROFILE-5(SUB)
              END-PERFORM
           ELSE
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                 MOVE LN-RECPROFILE(SUB) TO HOLD-PROFILE-5(SUB)
              END-PERFORM.

           MOVE BEG-DATE     TO NUM-DATE.
           MOVE PROFILE-DATE TO SYS-DATE.
           PERFORM TIMALL.
           COMPUTE SUB = ELAPSED-MONTHS + 1.

      * TABLE IS ONLY 60, I DON'T THINK THIS WOULD HAPPEN?
           IF SUB > 60
              ADD 1 TO TOTAL-SKIPPED
              GO TO NEXT-LN.

      * 9 MEANS ACCT'S PROFILE NOT UPDATED THAT MONTH, MAYBE NEW ACCT
      * (MONTH END DOESN'T UPDATE PROFILE IF 1ST PAYDATE AFTER MONTH END DATE
           IF HOLD-PROFILE(SUB) = 9
              ADD 1 TO TOTAL-SKIPPED
              GO TO NEXT-LN.

           IF HOLD-PROFILE(SUB) < DEL-SUB
              ADD 1 TO TOTAL-SKIPPED
              GO TO NEXT-LN.

      * TEST FIRST TIME THROUGH

           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              PERFORM SET-BRANCH-HEADINGS.

      * TEST BREAK IN BRANCH
           IF BR-NO NOT = HOLD-BRNO
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              IF INCLUDE-DETAIL = "Y"
                 PERFORM SET-BRANCH-HEADINGS.

           PERFORM LN-DETAIL.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.

           GO TO NEXT-GROUP-LOC.

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
           MOVE PBDRP3-QUEUE-POS TO Q-POS.
           MOVE PBDRP3-COPIES-POS TO C-POS.
           MOVE PBDRP3-PRU-POS   TO P-POS.
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
           MOVE EXT-EOBUF-DATE2 TO END-DATE MONTH-END-DATE.

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

       ENT-MONTH-END.
           IF EXT-ROUTE-BUF NOT = "00" GO TO GTT-ISO-PARAGRAPH.
           MOVE "ENTER DATE OF MOST RECENT MONTH-END" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060MONTHEND." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO ENT-END-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENT-MONTH-END.
           MOVE VDU-DATE-YYYYMMDD TO MONTH-END-DATE.

       INCLUDE-PBD-ACCTS.
           MOVE "INCLUDE PB DELINQUENT ACCOUNTS?"
               TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PBD." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO ENT-MONTH-END.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO INCLUDE-PBD-ACCTS.
           IF NOT ( VDUBUF = "S" OR "I" OR "O" )
              MOVE "ENTER 'S' (SKIP), 'I' (INCLUDE) OR 'O' (ONLY)"
                 TO MESS
              PERFORM SEND-MESS
              GO TO INCLUDE-PBD-ACCTS.
           MOVE VDUBUF TO PBD-FG.
       REC-OR-CONTRACTUAL.
           MOVE "ENTER 'R' FOR RECENCY, 'C' FOR CONTRACTUAL"
               TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RECCON." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              GO TO INCLUDE-PBD-ACCTS.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO REC-OR-CONTRACTUAL.
           IF NOT ( VDUBUF = "R" OR "C" )
              MOVE "ENTER 'R' FOR RECENCY, 'C' FOR CONTRACTUAL"
                 TO MESS
              PERFORM SEND-MESS
              GO TO REC-OR-CONTRACTUAL.
           MOVE VDUBUF TO RECCON.
       ENTER-DELINQUENCY.
           MOVE "ENTER MINIMUM BEG DATE DELINQUENCY (1-7)"
               TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010DEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              GO TO REC-OR-CONTRACTUAL.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-DELINQUENCY.
           IF VDUNUM0 < 1 OR > 7
              MOVE "ENTER NUMBER FROM 1 (30) TO 7 (210+)"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DELINQUENCY.
           MOVE VDUNUM0 TO DEL-SUB.
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

           MOVE BR-NO            TO D-BRNO.
           MOVE LN-ACCTNO        TO D-NUMBER.
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
                     WK-OPEN
                     WK-PB-PL
                     WK-PB-PLAMT
                     WK-PB-PAIDOUT
                     WK-PB-OPEN.

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
                    PERFORM FIND-PB-CHAIN
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

           IF INCLUDE-DETAIL = "Y"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

       EXIT-CREATE-WK.
           EXIT.


      **********************
       FIND-PB-CHAIN SECTION.
      **********************
           MOVE LN-OWNBR TO HOLD-LN-OWNBR.
           MOVE LN-CLASS TO HOLD-LN-CLASS.
           MOVE LN-ACCTNO TO HOLD-LN-ACCTNO.
           MOVE LN-ACCTNO TO PRIOR-LOAN-TO-FIND.
           MOVE LN-SSNO(1) TO HOLD-SSNO.
           MOVE 0 TO PB-COUNT.
       START-BORROWER-LOANS.
           ADD 1 TO PB-COUNT.

           MOVE HOLD-LN-OWNBR  TO LN-OWNBR
                                  QLN4-WBEG-OWNBR
                                  QLN4-WEND-OWNBR.
           MOVE HOLD-SSNO      TO LN-SSNO(1)
                                  QLN4-WBEG-SSNO1
                                  QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
       NEXT-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF (IO-BAD) OR (LN-SSNO(1) NOT = HOLD-SSNO)
                        OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO FIND-PB-CHAIN-EXIT.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN4.

      * IF THESE MATCH, WE FOUND THE PB RENEWAL LOAN
           IF LN-PRLNNO NOT = PRIOR-LOAN-TO-FIND
              GO TO NEXT-LN4.

      * WE KNOW THIS PB WAS IN DATE RANGE, SINCE PB WAS B4 END-DATE
           IF LN-PLDATE = 0
              IF LN-POFFDATE = 0
                 ADD 1 TO WK-PB-OPEN
                 GO TO FINISH-DETAIL-LINE.

      * IF PAID OFF AFTER END-DATE, COUNT AS OPEN:
           IF LN-POFFDATE NOT = 0
              IF LN-POFFDATE > END-DATE
                 ADD 1 TO WK-PB-OPEN
                 GO TO FINISH-DETAIL-LINE
              ELSE
                 IF LN-PO-TRCD NOT = "PB"
                    ADD 1 TO WK-PB-PAIDOUT
                    GO TO FINISH-DETAIL-LINE
                 ELSE
                    MOVE LN-ACCTNO   TO PRIOR-LOAN-TO-FIND
                                        D-PB-NUMBER(PB-COUNT)
                    MOVE LN-PLDATE   TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO D-PB-PLDATE(PB-COUNT)
                    MOVE LN-POFFDATE TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO D-PB-POFFDATE(PB-COUNT)
                    MOVE LN-PO-TRCD  TO D-PB-POCD(PB-COUNT)
                    GO TO START-BORROWER-LOANS.

      * IF CHARGED OFF BEFORE END-DATE, COUNT AS OPEN:
           IF LN-PLDATE NOT = 0
              IF LN-PLDATE NOT > END-DATE
                 ADD 1 TO WK-PB-PL
                 ADD LN-PLAMT TO WK-PB-PLAMT
                 GO TO FINISH-DETAIL-LINE
              ELSE
                 ADD 1 TO WK-PB-OPEN
                 GO TO FINISH-DETAIL-LINE.

           GO TO NEXT-LN4.

       FINISH-DETAIL-LINE.
           MOVE LN-ACCTNO   TO D-PB-NUMBER(PB-COUNT).
           MOVE LN-PLDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PB-POFFDATE(PB-COUNT).
           MOVE LN-PO-TRCD  TO D-PB-POCD(PB-COUNT).
       FIND-PB-CHAIN-EXIT.
           EXIT.

      ******************************************************************
       GET-PL-TRANS-DATE SECTION.
      ******************************************************************

      * IF ACCT WAS CHARGED OFF AFTER MONTH-END-DATE, PROCESS AS
      * NORMAL OPEN ACCT, OTHERWISE PROCESS AS P&L.  NOTE:
      * WILL HAVE TO READ LP TO GET TRANS DATE SINCE PL'S ARE
      * BACK-DATED, AT LEAST P2'S:

           MOVE LN-PLDATE TO HOLD-PL-TRDATE.
           MOVE "PL" TO TEST-LN-PLCD.
           IF LN-PLTYPE = "2" OR "3" OR "4" OR "5"
              MOVE LN-PLTYPE TO TEST-PLTYPE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-LP.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO EXIT-GET-LP-INFO.

           IF LP-BRNO NOT = LN-OWNBR OR
               LP-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-GET-LP-INFO.

           IF LP-TRCD NOT = TEST-LN-PLCD
              GO TO NEXT-LP.

           IF LP-REV NOT = SPACES
              GO TO NEXT-LP.

           MOVE LP-TRDATE TO HOLD-PL-TRDATE.
       EXIT-GET-LP-INFO.
           EXIT.

      ****************************************
       GET-PO-TRANS-DATE SECTION.
      ****************************************

           MOVE LN-POFFDATE TO HOLD-PO-TRDATE.
           MOVE LN-PO-TRCD  TO TEST-LN-POCD.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-PO-LP.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO EXIT-GET-LP-PO-INFO.

           IF LP-BRNO NOT = LN-OWNBR OR
               LP-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-GET-LP-PO-INFO.

           IF LP-TRCD NOT = TEST-LN-POCD
              GO TO NEXT-PO-LP.

           IF LP-REV NOT = SPACES
              GO TO NEXT-PO-LP.

           MOVE LP-TRDATE TO HOLD-PO-TRDATE.
       EXIT-GET-LP-PO-INFO.
           EXIT.


      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ************* *****************************************************
           ADD WK-NOLOANS    TO WS-GTT-NOLOANS(GTT-TOTAL-SUB).
           ADD WK-PAIDOUT    TO WS-GTT-PAIDOUT(GTT-TOTAL-SUB).
           ADD WK-PB         TO WS-GTT-PB(GTT-TOTAL-SUB).
           ADD WK-PL         TO WS-GTT-PL(GTT-TOTAL-SUB).
           ADD WK-PLAMT      TO WS-GTT-PLAMT(GTT-TOTAL-SUB).
           ADD WK-OPEN       TO WS-GTT-OPEN(GTT-TOTAL-SUB).
           ADD WK-PB-PL      TO WS-GTT-PB-PL(GTT-TOTAL-SUB).
           ADD WK-PB-PLAMT   TO WS-GTT-PB-PLAMT(GTT-TOTAL-SUB).
           ADD WK-PB-PAIDOUT TO WS-GTT-PB-PAIDOUT(GTT-TOTAL-SUB).
           ADD WK-PB-OPEN    TO WS-GTT-PB-OPEN(GTT-TOTAL-SUB).

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
           MOVE WS-GTT-PB-PL(GTT-TOTAL-SUB)   TO G-PB-PL.
           MOVE WS-GTT-PB-PLAMT(GTT-TOTAL-SUB) TO G-PB-PLAMT.
           MOVE WS-GTT-PB-PAIDOUT(GTT-TOTAL-SUB) TO G-PB-PAIDOUT.
           MOVE WS-GTT-PB-OPEN(GTT-TOTAL-SUB) TO G-PB-OPEN.

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

      * OKAY, THIS LOGIC IS JUST LIKE LIBLP/LPSRCD, EXCEPT I STARRED OF
      * THE LINE THAT CHECKS FOR LN-PLCD = " ".  THESE ACCOUNTS MAY BE
      * CHARGED OFF, BUT WEREN'T CHARGED OFF AS OF THE BEG DATE, & WE
      * WANT TO KNOW IF THEY WERE PBD'S, DON'T CARE ABOUT PL SOURCE CODES  BARB

           MOVE 0 TO SRCD.
           MOVE "N" TO SRCD-FG

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
           MOVE MONTH-END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-MONTH-END.
           IF DEL-SUB = 1
              MOVE "  30" TO H-DEL
           ELSE IF DEL-SUB = 2
              MOVE "  60" TO H-DEL
           ELSE IF DEL-SUB = 3
              MOVE "  90" TO H-DEL
           ELSE IF DEL-SUB = 4
              MOVE " 120" TO H-DEL
           ELSE IF DEL-SUB = 5
              MOVE " 150" TO H-DEL
           ELSE IF DEL-SUB = 6
              MOVE " 180" TO H-DEL
           ELSE IF DEL-SUB = 7
              MOVE "210+" TO H-DEL.
           IF RECCON = "R"
              MOVE "(RECENCY)" TO H-RECCON
           ELSE
              MOVE "(CONTRACTUAL)" TO H-RECCON.
           IF PBD-FG = "O"
              MOVE "PBD'S ONLY" TO H-PBDS
           ELSE IF PBD-FG = "I"
              MOVE "PBD'S INCLUDED" TO H-PBDS
           ELSE IF PBD-FG = "S"
              MOVE "PBD'S SKIPPED" TO H-PBDS.

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


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO PBDRP3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PBDRP3-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PBDRP3-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PBDRP3-FORM-FIELDS TO WR-BUF.
           MOVE PBDRP3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE PBDRP3-HELP--FILE TO HELP-LINK-FILE.
           MOVE PBDRP3-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PBDRP3_EVA.CPY".
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
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
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
              DESTROY PBDRP3-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
