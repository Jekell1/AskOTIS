       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLOSRP.
       DATE-WRITTEN.    090413.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  GEORGIA CLOSING FEE REPORT
      *                  WORLD PR# 921
      *
      * REV:
      * BLM 2013-0904 NEW, WORLD PR# 921
      * BAH 2017-0517 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018-1205 GLOBAL LNFILE
      * BAH 2019-0919 REMOVED ACCESS-CALL ON LNFILE
      * JKC 2023-0413 RESTORED BACK `IF ( LN-POCD-NONLOAN )` SO THAT
      *          IT DOES NOT SKIP PROCESSING RECORDS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CLOSRP.CBL J35 12/21/23-08:57:16 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
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
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       PIC 9999 COMP OCCURS 15.

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   M I S C E L L E N O U S - W O R K E R S
      *
      ******************************************************************
       01  LEVEL-SUB            PIC 9999.
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.

       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".

       01  FIRST-TIME           PIC X         VALUE "Y".
       01  NEW-BRANCH-FG        PIC X         VALUE "Y".
       01  LEGEND               PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST          PIC X(09)     VALUE "LEGEND:B.".
       01  HOLD-BRANCH          PIC 9(4)       COMP   VALUE 0.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR                         PIC 9(4).
           03  END-BR                         PIC 9(4).
           03  BEG-CLASS                      PIC 9(3).
           03  END-CLASS                      PIC 9(3).
           03  BEG-LN-DATE                    PIC 9(8).
           03  END-LN-DATE                    PIC 9(8).
           03  GROUP-FLAG                     PIC X.
           03  ENT-GROUP                      PIC X(4).
           03  INCLUDE-DETAIL                 PIC X.
           03  GTT-ISO         PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.

       01  VERSION-CONTROL                    PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       01  HOLD-CLASS              PIC 999             VALUE 0.
      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB            OCCURS 10.
               05  TOT-COUNT        PIC S9(10)    COMP.
               05  TOT-CLOSING-FEE  PIC S9(10)V99 COMP.
       01  WS-HOLD-GROUP.
           05  WS-HOLD-PROMPT   PIC X(7).
           05  FILLER           PIC X(13).
      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(9)     VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(9)     VALUE " ".
           03  FILLER           PIC X(31)    VALUE
           "GEORGIA 4% CLOSING FEE REPORT: ".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(6)    VALUE " THRU ".
           03  H-END-DATE       PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(9)  VALUE "BRANCH  :".
           03  H-GRID.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(44)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(24) VALUE
               "BRNO  ACCOUNT NO.       ".
           03  FILLER           PIC X(27) VALUE
               "LOANDATE  NAME             ".
           03  FILLER           PIC X(32) VALUE
               "   LOAN AMOUNT           4% FEE ".

      *-----------------------------------------------------------------
       01  TOTAL-HEAD.
           03  FILLER           PIC X(38) VALUE SPACES.
           03  FILLER           PIC X(13) VALUE
               "   NO. LOANS ".
           03  FILLER           PIC X(16) VALUE
               "         4% FEE ".
      *-----------------------------------------------------------------
       01  STAR-LINE.
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(25) VALUE
           "-------------------------".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL1             REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z999.
           03  FILLER           PIC XX.
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC XX.
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC XX.
           03  D-LNAME          PIC X(17).
           03  D-LNAMT          PIC ZZZZ,ZZZ,ZZZ.ZZ-.
           03  D-CLOSING-FEE    PIC ZZZZ,ZZZ,ZZZ.ZZ-.
      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  G-GROUP              PIC X(20).
           03  G-GROUP-2 REDEFINES G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(8).
           03  FILLER               PIC X(31).
           03  G-COUNT              PIC ZZZ,ZZZ,ZZZ,ZZ9-.
           03  G-CLOSING-FEE        PIC ZZZZ,ZZZ,ZZ9.99-.

       01  RANGE-LINE          REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(30).
           03  RANGE-SELECTIONS.
               05  RANGE-SEL1       PIC X(10).
               05  FILLER           REDEFINES RANGE-SEL1.
                   07  RANGE-DATE1  PIC ZZZZ/ZZ/ZZ.
               05  RANGE-DASH       PIC X.
               05  RANGE-SEL2       PIC X(10).
               05  FILLER           REDEFINES RANGE-SEL2.
                   07  RANGE-DATE2  PIC ZZZZ/ZZ/ZZ.
               05  FILLER           PIC X(10).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CLOSRP_DEF.CPY".
       COPY "LIBLP/CLOSRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CLOSRP_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLOSRP" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-PR-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
           ELSE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BR
              PERFORM PRINT-BRANCH-GRAND-TOTALS
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE BR-NO TO HOLD-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE 9999 TO PROGID-SLEEP-COUNTER
              PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE "Y"       TO NEW-BRANCH-FG.

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

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           PERFORM SEND-PROGRESS-INDICATOR.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF ( LN-CLASS < BEG-CLASS ) GO TO NEXT-LN.

           IF ( LN-CLASS > END-CLASS )
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF LN-POINTS = 0
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-LN-DATE OR LN-LOANDATE > END-LN-DATE
              GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           IF NEW-BRANCH-FG = "Y"
              MOVE "N" TO NEW-BRANCH-FG
              MOVE LN-CLASS TO HOLD-CLASS.

           IF INCLUDE-DETAIL = "Y"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE "INVALID BOR!" TO BW-LNAME.
           IF INCLUDE-DETAIL = "Y"
              PERFORM PROCESS-DETAIL-LINE.

           MOVE "N" TO FIRST-TIME.
           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-LN.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND.
           IF EXT-ROUTE-BUF = "00"
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       PROCESS-DETAIL-LINE SECTION.
      ******************************************************************
           MOVE BR-NO            TO D-BRNO.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-LOANDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-LOANDATE.
           MOVE BW-LNAME         TO D-LNAME.
           MOVE LN-LNAMT         TO D-LNAMT.
           MOVE LN-POINTS        TO D-CLOSING-FEE.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CLOSRP-QUEUE-POS TO Q-POS.
           MOVE CLOSRP-COPIES-POS TO C-POS.
           MOVE CLOSRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

      *-----------------------------------------------------------------
       COPY "LIBGB/EOINIT.CPY".
      *-----------------------------------------------------------------

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
                 MOVE "N" TO GROUP-FLAG
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BR
                 MOVE 9999 TO END-BR
                 MOVE "Y" TO GROUP-FLAG
              END-IF
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE BEG-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE END-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.

       INITIALIZATION-MISC.
           MOVE LOW-VALUES TO TOTAL-TABLE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LN-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-LN-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DT-02.

      *-----------------------------------------------------------------
       BR-01.
           MOVE "E  A040BR1." TO VDU.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO ENTER-BEG-CLASS.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR        TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BR = 0) AND (END-BR = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO BR-02.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( END-CLASS = ZEROES ) GO TO ENTER-END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO DT-01.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE 99                  TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

      *=================================================================
       DT-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO GTT-ISO-PARAGRAPH.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO ENTER-END-CLASS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.

      *-----------------------------------------------------------------
       DT-02.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO GTT-ISO-PARAGRAPH.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-LN-DATE.
           IF END-LN-DATE < BEG-LN-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO GTT-ISO-PARAGRAPH.

      *-----------------------------------------------------------------
       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------

       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO INCL-DETAIL.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO BR-01
              ELSE
                 GO TO DT-02.

           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "O" OR "S")
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(1)
                          TOTAL-LEVEL-TABLE(9).
      *-----------------------------------------------------------------
       INCL-DETAIL.
           IF GTT-ISO = "O"
              MOVE "S   010DETAIL." TO VDU
              MOVE "N" TO INCLUDE-DETAIL VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO DT-02.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO INCL-DETAIL.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO INCL-DETAIL.

           MOVE VDUBUF TO INCLUDE-DETAIL.

      *-----------------------------------------------------------------
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A040BR2." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE BEG-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE END-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DETAIL."    TO VDU.
           MOVE INCLUDE-DETAIL      TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "N"
              MOVE 9 TO TOT-SUB
              PERFORM ADD-TOTALS
              MOVE 1 TO TOT-SUB
              PERFORM ADD-TOTALS
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-TPLUS TO GROUP-SUB.

      *----------------------------------------------------------------
       CHECK-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO TOT-SUB
              GO TO CHECK-TOTAL-FLAG.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.

       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

       CONTINUE-CHECK-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.


      ******************************************************************
       TOTAL-LEVEL-CHECK SECTION.
      ******************************************************************
           MOVE 15 TO GROUP-SUB.

       LEVEL-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           IF WGR-SCURR(GROUP-SUB) = PREV-SCURR(GROUP-SUB)
              GO TO CONTINUE-LOOP.

           MOVE PREV-SCURR(GROUP-SUB) TO PREV-SUB.

           IF PREV-SUB = 0
              GO TO CONTINUE-LOOP.

           IF WGR-SID(PREV-SUB) NUMERIC
              GO TO CHECK-BRANCH-SID.

           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           IF TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE GR-LEVEL TO TOT-SUB.

      *----------------------------------------------------------------
       DO-GROUP-TOTALS.
           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           PERFORM PRINT-TOTALS.

           GO TO CONTINUE-LOOP.

      *----------------------------------------------------------------
       CHECK-BRANCH-SID.
           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.
           MOVE 9 TO TOT-SUB.
      *    PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

      *----------------------------------------------------------------
       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      ***********************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      ***********************************************
           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.
           MOVE 9 TO TOT-SUB.
           PERFORM PRINT-TOTALS.

      ***********************************************
       PRINT-BRANCH-GRAND-TOTALS SECTION.
      ***********************************************
           IF BEG-BR = END-BR
              GO TO EXIT-PRINT-BRANCH-GRAND.

           MOVE 0 TO H-BRNO.
           MOVE "GRAND TOTALS" TO H-BRNAME.
           MOVE 1 TO TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.
       EXIT-PRINT-BRANCH-GRAND.
           EXIT.

      ***********************************************
       PRINT-TOTALS SECTION.
      ***********************************************
           IF TOT-COUNT(TOT-SUB) = 0
              IF TOT-CLOSING-FEE(TOT-SUB) = 0
                 GO TO EXIT-PRINT-TOTALS.

           MOVE G-GROUP TO WS-HOLD-GROUP.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE WS-HOLD-GROUP TO G-GROUP.

           IF TOT-SUB = 9
              MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT
              MOVE BR-NO     TO G-BRNO
           ELSE
              MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT
              MOVE GR-ID     TO G-GRNO.
           MOVE TOT-COUNT(TOT-SUB)       TO G-COUNT.
           MOVE TOT-CLOSING-FEE(TOT-SUB) TO G-CLOSING-FEE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF INCLUDE-DETAIL = "Y"
              IF TOT-COUNT(TOT-SUB) NOT = 0
                 MOVE STAR-LINE TO DETAIL-LINE
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-TOTALS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).

      ***********************************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************************
       HEAD-PAGE SECTION.
      ***********************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GTT-ISO NOT = "O"
              MOVE BR-NO TO H-BRNO
              MOVE BR-NAME TO H-BRNAME
              MOVE HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GTT-ISO = "O"
              MOVE TOTAL-HEAD TO PR-REC
           ELSE
              MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GTT-ISO = "O"
              MOVE 3 TO LN-COUNT
           ELSE
              MOVE 4 TO LN-COUNT.
       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.


      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "GROUP            :" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BRANCH RANGE     :" TO RANGE-LINE
              MOVE BEG-BR TO RANGE-SEL1
              MOVE " - " TO RANGE-DASH
              MOVE END-BR TO RANGE-SEL2
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CLASS RANGE      :" TO RANGE-LINE.
           MOVE BEG-CLASS     TO RANGE-SEL1.
           MOVE " - " TO RANGE-DASH.
           MOVE END-CLASS     TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LOAN DATE RANGE  :" TO RANGE-LINE.
           MOVE BEG-LN-DATE TO RANGE-DATE1.
           MOVE " - " TO RANGE-DASH.
           MOVE END-LN-DATE TO RANGE-DATE2.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "INCLUDE DETAIL   :" TO RANGE-LINE.
           MOVE INCLUDE-DETAIL TO RANGE-SEL1.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LEVEL TOTALS        :" TO RANGE-LINE.
           MOVE TOTAL-LEV-TABLE TO RANGE-SEL1.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           MOVE BR-NO                  TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CLOSRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLOSRP-SYSDATE.
           DISPLAY CLOSRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLOSRP-FORM-FIELDS TO WR-BUF.
           MOVE CLOSRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CLOSRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLOSRP-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CLOSRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       ADD-TOTALS.
           ADD 1            TO TOT-COUNT(TOT-SUB).
           ADD LN-POINTS    TO TOT-CLOSING-FEE(TOT-SUB).
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CLOSRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
