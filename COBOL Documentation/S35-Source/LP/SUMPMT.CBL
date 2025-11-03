       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SUMPMT.
       DATE-WRITTEN.    081905.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  BANKING STATS MATURITY REPORT
      *                  FROM BNKRP1, JUST PRINTS GROUP SUMMARY TOTALS
      *                  WITH VARIABLE NUMBER OF BUCKETS UNTIL
      *                  EXPIRATION, WORLD PR# 425
      *
      * REV:
      * BLF 050908 CHANGE TO ADD PAST DUE AMOUNTS TO MONTH OF MATURITY
      *            INSTEAD OF MONTH 0
      * BLF 050915 FIXED MONTHS-TILL-EXPIRATION ROUTINE TO USE YEAR-END
      *            DATE INSTEAD OF SYSTEM DATE
      * BLF 061115 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *            EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      * BLF 090327 FIXED TO CHECK BEGINNING & ENDING LOAN DATE RANGE,
      *            REGMGM, PER JIM
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *  CS 170710 ACTIVATE 8 DIGIT DATES
      * BAH 181211 GLOBAL LNFILE
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE
      * RMZ 20230926 PERFORMANCE TUNING - ADDED STORED PROCEDURE 
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
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
      *   SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-GRLEVEL PIC 9.
               05  WK-MONTH   PIC 9(3).
           03  WK-BRNO        PIC 9(4)      COMP.
           03  WK-QTY         PIC S9(9)     COMP.
           03  WK-BALANCE     PIC S9(10)V99 COMP.
           03  WK-NET-RECV    PIC S9(10)V99 COMP.
           03  WK-CASH-FLOW   PIC S9(10)V99 COMP.
           03  FILLER         PIC X.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/SUMPMT.CBL S35 02/20/24-13:48:10 >".

       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
      *-----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS1-WBEG-LOANDATE   PIC X(10).                   
       01  QWS1-WEND-LOANDATE   PIC X(10).                
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       PIC 9999 COMP OCCURS 15.

       01  HOLD-WK-KEY.
           03  HOLD-WK-GRLEVEL  PIC 9.
           03  HOLD-WK-MONTH    PIC 9(3).

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
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  SUB                  PIC 9(2)     COMP.
       01  MONTH-SUB            PIC 9(6)     VALUE 0.
       01  LEVEL-SUB            PIC 9999.
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  MESS-LIST            PIC X(5)      VALUE "MESS.".

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  LEGEND               PIC X(80)    VALUE SPACES.
       01  LEGEND-LIST          PIC X(09)    VALUE "LEGEND:B.".
       01  MESS-BUF.
           03  FILLER           PIC X(30)   VALUE
           "PREPARING REPORT FOR BRANCH:".
           03  MESS-LOC         PIC 9(4).
       01  VALID-DATE           PIC X        VALUE "Y".
       01  FUTURE-PMTS          PIC S9(9)V99 COMP.
       01  PAST-PMTS            PIC S9(9)V99 COMP.
       01  PCT-WK               PIC S9(3)V99 COMP.
       01  HOLD-BRANCH          PIC 9(4)     COMP   VALUE 0.
       01  HOLD-TYPE            PIC 9(2)     COMP   VALUE 0.
       01  MONTHS-TILL-EXPIRATION      PIC 9(03)     VALUE ZEROES.
       01  MONTHS-TILL-EXP-STAT        PIC X(01)     VALUE SPACES.
           88  MONTHS-TILL-EXP-STAT-BAD    VALUE "X".
           88  MONTHS-TILL-EXP-STAT-GOOD   VALUE SPACES.

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
           03  YEAR-END-DATE                  PIC 9(8).
           03  GROUP-FLAG                     PIC X.
           03  ENT-GROUP                      PIC X(4).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.

       01  VERSION-CONTROL                    PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB           OCCURS 10.
               05  TOT-QTY         PIC S9(9)     COMP.
               05  TOT-BALANCE     PIC S9(10)V99 COMP.
               05  TOT-NET-RECV    PIC S9(10)V99 COMP.
               05  TOT-CASH-FLOW   PIC S9(10)V99 COMP.
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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(42)    VALUE
           "  - - - CASH FLOW MATURITY REPORT - - -   ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  H-BRGR           PIC X(10) VALUE "BRANCH :".
           03  H-GRID.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(21) VALUE " ".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(6)  VALUE " THRU ".
           03  H-END-DATE       PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(010)   VALUE "CLASS  :".
           03  H-BEG-CLASS      PIC 9(002)   VALUE ZEROES.
           03  FILLER           PIC X(001)   VALUE SPACES.
           03  H-CLASS-DESC     PIC X(119)   VALUE SPACES.
           03  H-CLASS-RDF      REDEFINES H-CLASS-DESC.
               05  H-TO-CLASS   PIC X(002).
               05  FILLER       PIC X(001).
               05  H-END-CLASS  PIC 9(002).
               05  FILLER       PIC X(114).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(15) VALUE
               "MONTHS TIL EXP".
           03  FILLER           PIC X(18) VALUE
           "     QTY--------% ".
           03  FILLER           PIC X(28) VALUE
           "           BALANCE--------% ".
           03  FILLER           PIC X(28) VALUE
           "        NET RECEIV--------% ".
           03  FILLER           PIC X(28) VALUE
           "         CASH FLOW--------% ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL1             REDEFINES DETAIL-LINE.
           03  D-MONTHS         PIC ZZZZZZZ9.
           03  FILLER           PIC X(7).
           03  D-QTY            PIC Z(8)-.
           03  FILLER           PIC X(2).
           03  D-QTY-PCT        PIC ZZ9.99-          BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-CURBAL         PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-BAL-PCT        PIC ZZ9.99-          BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-NET            PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-NET-PCT        PIC ZZ9.99-          BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-CASH-FLOW      PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-CASH-FLOW-PCT  PIC ZZ9.99-          BLANK ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE          REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SEL        PIC X(10).
           03  FILLER          REDEFINES RANGE-SEL.
               05  RANGE-DATE   PIC ZZ/ZZ/ZZ.
               05  FILLER       PIC XX.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/RBACTW.CPY".
      *COPY "LIBLP/RNFOR10W.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/SUMPMT_DEF.CPY".
       COPY "LIBLP/SUMPMT_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/SUMPMT_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SUMPMT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "REPORT IN PROGRESS.........." TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
           ELSE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)
           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BR
              PERFORM PRINT-BRANCH-GRAND-TOTALS
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE "Y" TO NEW-BRANCH.

           MOVE BR-NO TO HOLD-BRANCH.
           MOVE 0 TO HOLD-TYPE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

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

           PERFORM START-LN3-FILE. *> SSP - LNFILE

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED - LNFILE
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF ( LN-CLASS < BEG-CLASS ) GO TO NEXT-LN.

           IF ( LN-CLASS > END-CLASS )
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN.

           IF LN-CURBAL = 0
              IF LN-INTBAL = 0
                 IF LN-LCBAL = 0
                    IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

           MOVE "Y" TO VALID-DATE.

           IF LN-SALEFIN = "Y"
              MOVE LN-PURDATE TO SYS-DATE
           ELSE
              MOVE LN-LOANDATE TO SYS-DATE.
           IF SYS-DATE < BEG-LN-DATE OR
               SYS-DATE > END-LN-DATE
              MOVE "N" TO VALID-DATE.

           IF VALID-DATE = "N"
              GO TO NEXT-LN.

           PERFORM GET-SP.

           MOVE YEAR-END-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE 0 TO REB-REBATE.

           IF IBPC-FG = "P"
              IF LN-INT-REBATE = " " OR "C"
                 MOVE YEAR-END-DATE TO REB-PAYDATE
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 MOVE 7 TO REB-SUB
                 MOVE "RI" TO REB-LPTRCD
                 MOVE LN-INTCHG TO REB-TOTCHG
                 PERFORM REBATE.

           PERFORM GET-MONTHS-TILL-EXPIRATION.

           MOVE "N" TO FIRST-TIME.
           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              PERFORM RANGE-LINES
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE SUMPMT-QUEUE-POS TO Q-POS.
           MOVE SUMPMT-COPIES-POS TO C-POS.
           MOVE SUMPMT-PRU-POS   TO P-POS.
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
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE 9999 TO PROGID-SLEEP-COUNTER
              MOVE ZEROES TO BR-NO
              PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-LC1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - -{ SET CLASS HEADER }
           IF ( BEG-CLASS = END-CLASS )
              MOVE BEG-CLASS TO H-BEG-CLASS LC-CODE
              PERFORM READ-LC1-FILE
              MOVE LC-DESC   TO H-CLASS-DESC
           ELSE
              MOVE BEG-CLASS TO H-BEG-CLASS
              MOVE "TO"      TO H-TO-CLASS
              MOVE END-CLASS TO H-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           PERFORM FIND-DEFAULT-YEAR-END.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO YEAR1.

           PERFORM FIND-DEFAULT-YEAR-END.
           MOVE "S  8080YEAR." TO VDU.
           MOVE YEAR-END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

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

       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR        TO VDUNUM0.
           PERFORM SCREENIO.

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

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       ENTER-BEG-CLASS.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO BR-02.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

       ENTER-END-CLASS.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

           MOVE VDUNUM0 TO END-CLASS.

           IF ( END-CLASS = ZEROES ) GO TO ENTER-END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO DT-01.

       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE 99                  TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

       DT-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO YEAR1.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO ENTER-END-CLASS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.

       DT-02.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO YEAR1.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD  TO END-LN-DATE.
       
           IF END-LN-DATE < BEG-LN-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO YEAR1.

       ALL-DT.
           MOVE "S  8080DT1."  TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2."  TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       YEAR1.
           PERFORM FIND-DEFAULT-YEAR-END.
           MOVE "S  8080YEAR." TO VDU.
           MOVE YEAR-END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "ERNM060YEAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF ( UP-KEY )
              IF ( EXT-ROUTE-BUF = "00" )
                 GO TO DT-02
              ELSE
                 GO TO ENTER-END-CLASS.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO YEAR-END-DATE.

           IF YEAR-END-DATE = 0
              GO TO YEAR1.

           MOVE YEAR-END-DATE TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE " INVALID YEAR-END DATE!" TO MESS
              PERFORM SEND-MESS
              GO TO YEAR1.

       CHECK-LEVEL-PRINTS.
           IF ( GROUP-FLAG = "Y" )
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(1)
                          TOTAL-LEVEL-TABLE(9).

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
       
           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE BEG-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE END-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       
           MOVE "S  8080DT2." TO VDU.
           MOVE END-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       
           PERFORM FIND-DEFAULT-YEAR-END.
           MOVE "S  8080YEAR." TO VDU.
           MOVE YEAR-END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *****************************************
       FIND-DEFAULT-YEAR-END SECTION.
      *****************************************
       
      * NEED TO READ THE GPFILE HERE AND NOT USE GPENV;
      * SOME GP FIELDS MAY HAVE CHANGED SINCE GPENV WAS SET SO
      * NEED TO MAKE SURE WE HAVE THE LATEST UP-TP-DATE GP-REC.
       
           MOVE 0 TO YEAR-END-DATE.

           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           PERFORM CLOSE-GP-FILE.

           MOVE GP-END-FISCAL-DATE(12) TO YEAR-END-DATE.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "N"
              MOVE 9 TO HOLD-WK-GRLEVEL TOT-SUB
              PERFORM UPDATE-WK-REC
              MOVE 1 TO HOLD-WK-GRLEVEL TOT-SUB
              PERFORM UPDATE-WK-REC
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-TPLUS TO GROUP-SUB.

       CHECK-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO HOLD-WK-GRLEVEL TOT-SUB
              GO TO CHECK-TOTAL-FLAG.

           MOVE WGR-SID-LEVEL(LEVEL-SUB) TO HOLD-WK-GRLEVEL TOT-SUB.

       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(HOLD-WK-GRLEVEL) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM UPDATE-WK-REC.

       CONTINUE-CHECK-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.

      ******************************************************************
      *   CHECK FOR LEVEL SUMMARY TOTALS
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP
      *   WHICH POINT TO WGR-SID TABLE TO SHOW CURRENT
      *   GROUP HIERARCHY:  WGR-SCURR(1) POINTS TO WGR-SID
      *   OF LOW LEVEL (SELECTED GROUP); WGR-SCURR(WGR-TPLUS)
      *   POINTS TO THE WGR-SID OF THE HIGHEST LEVEL WHICH
      *   IS THE BRANCH NUMBER CURRENTLY BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE
      *   TABLE SAVED FROM THE PREVIOUS GROUP, PRINTING &
      *   ZEROING TOTALS FOR EACH LEVEL WHICH HAS CHANGED
      *   STARTING WITH THE HIGHEST (BRANCH) LEVEL & WORKING
      *   THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
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

           MOVE WGR-SID-LEVEL(PREV-SUB) TO SUB.
           IF TOTAL-LEVEL-TABLE(SUB) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           MOVE GR-LEVEL TO HOLD-WK-GRLEVEL TOT-SUB.

       DO-GROUP-TOTALS.
           MOVE "GROUP :" TO H-BRGR.
           MOVE GR-ID     TO H-GRID.
           MOVE GR-NAME TO H-BRNAME.
           PERFORM HEAD-PAGE.

           PERFORM PREPARE-REPORT.
           PERFORM CLEAR-GROUP-LEVEL-RECS.

           GO TO CONTINUE-LOOP.

       CHECK-BRANCH-SID.
           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE "BRANCH:" TO H-BRGR.
           MOVE BR-NO     TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           MOVE 9 TO HOLD-WK-GRLEVEL TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PREPARE-REPORT.
           PERFORM CLEAR-GROUP-LEVEL-RECS.

       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      ***************************************************************
       CLEAR-GROUP-LEVEL-RECS SECTION.
      ***************************************************************

           MOVE HOLD-WK-GRLEVEL TO WK-GRLEVEL.
           MOVE 0 TO WK-MONTH.
           PERFORM START-WK-FILE.

       READ-WK-DELETE-NEXT.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CLEAR-GROUP-RECS.

           IF WK-GRLEVEL NOT = HOLD-WK-GRLEVEL
              GO TO EXIT-CLEAR-GROUP-RECS.

           PERFORM DELETE-WK-FILE.
           GO TO READ-WK-DELETE-NEXT.

       EXIT-CLEAR-GROUP-RECS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).

      ******************************************************************
       UPDATE-WK-REC SECTION.
      ******************************************************************
           MOVE 0 TO MONTH-SUB.

       SET-WK-BR.
           MOVE MONTH-SUB TO HOLD-WK-MONTH.
           MOVE HOLD-WK-KEY TO WK-KEY.

       FIND-WK-RECORD.
           PERFORM READ-WK-FILE.

           IF IO-BAD
              MOVE 0 TO WK-BRNO
                        WK-QTY
                        WK-BALANCE
                        WK-NET-RECV
                        WK-CASH-FLOW
              MOVE HOLD-WK-KEY TO WK-KEY
              PERFORM WRITE-WK-FILE
              IF IO-BAD
                 GO TO IO-ERROR.

      * NET RECEIVABLE

           IF MONTH-SUB = MONTHS-TILL-EXPIRATION
              IF LN-PLDATE = 0
                 IF LN-CURBAL > 0
                  COMPUTE WK-NET-RECV = WK-NET-RECV
                                      + LN-CURBAL
                                      - REB-REBATE
                  COMPUTE TOT-NET-RECV(TOT-SUB) = TOT-NET-RECV(TOT-SUB)
                                                + LN-CURBAL
                                                - REB-REBATE
                  ADD LN-CURBAL TO WK-BALANCE TOT-BALANCE(TOT-SUB)
                  ADD 1 TO WK-QTY TOT-QTY(TOT-SUB)
                  COMPUTE FUTURE-PMTS = LN-SUM-REGPYAMT *
                                        MONTHS-TILL-EXPIRATION
                  COMPUTE PAST-PMTS = LN-CURBAL - FUTURE-PMTS
                  ADD PAST-PMTS TO WK-CASH-FLOW
                                   TOT-CASH-FLOW(TOT-SUB).

           IF MONTH-SUB > 0
              ADD LN-SUM-REGPYAMT TO WK-CASH-FLOW
                                     TOT-CASH-FLOW(TOT-SUB).

           PERFORM REWRITE-WK-FILE.

           ADD 1 TO MONTH-SUB.
           IF MONTH-SUB NOT > MONTHS-TILL-EXPIRATION
              GO TO SET-WK-BR.

       EXIT-UPDATE-WK.
           EXIT.

      *************************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      *************************************************

           MOVE "BRANCH:" TO H-BRGR.
           MOVE BR-NO     TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           MOVE 9 TO HOLD-WK-GRLEVEL TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PREPARE-REPORT.
           PERFORM CLEAR-GROUP-LEVEL-RECS.

      *************************************************
       PRINT-BRANCH-GRAND-TOTALS SECTION.
      *************************************************
           IF BEG-BR = END-BR
              GO TO EXIT-PRINT-BRANCH-GRAND.

           MOVE 0 TO H-BRNO.
           MOVE "GRAND TOTALS" TO H-BRNAME.
           MOVE 1 TO HOLD-WK-GRLEVEL TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PREPARE-REPORT.

       EXIT-PRINT-BRANCH-GRAND.
           EXIT.

      *************************************************
       PREPARE-REPORT SECTION.
      *************************************************

           MOVE HOLD-BRANCH TO MESS-LOC.

           MOVE HOLD-WK-GRLEVEL TO WK-GRLEVEL.
           MOVE 0 TO WK-MONTH.
           PERFORM START-WK-FILE.

       READ-WK-NEXT.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO PRINT-TOTAL-LINE.

           IF WK-GRLEVEL NOT = HOLD-WK-GRLEVEL
              GO TO PRINT-TOTAL-LINE.

           MOVE WK-KEY TO HOLD-WK-KEY.

       PRINT-LINE.

           MOVE WK-MONTH      TO D-MONTHS.
           MOVE WK-QTY        TO D-QTY.
           IF TOT-QTY(TOT-SUB) NOT = 0
              COMPUTE PCT-WK ROUNDED = (WK-QTY * 100)
                                     / TOT-QTY(TOT-SUB)
              MOVE PCT-WK     TO D-QTY-PCT.
           MOVE WK-BALANCE    TO D-CURBAL.
           IF TOT-BALANCE(TOT-SUB) NOT = 0
              COMPUTE PCT-WK ROUNDED = (WK-BALANCE * 100)
                                     / TOT-BALANCE(TOT-SUB)
              MOVE PCT-WK     TO D-BAL-PCT.
           MOVE WK-NET-RECV   TO D-NET.
           IF TOT-NET-RECV(TOT-SUB) NOT = 0
              COMPUTE PCT-WK ROUNDED = (WK-NET-RECV * 100)
                                     / TOT-NET-RECV(TOT-SUB)
              MOVE PCT-WK     TO D-NET-PCT.
           MOVE WK-CASH-FLOW  TO D-CASH-FLOW.
           IF TOT-CASH-FLOW(TOT-SUB) NOT = 0
              COMPUTE PCT-WK ROUNDED = (WK-CASH-FLOW * 100)
                                     / TOT-CASH-FLOW(TOT-SUB)
              MOVE PCT-WK     TO D-CASH-FLOW-PCT.

           PERFORM WRITE-DETAIL-LINE.

           GO TO READ-WK-NEXT.

       PRINT-TOTAL-LINE.

           MOVE "------ TOTALS -------" TO DETAIL-LINE.
           MOVE TOT-QTY(TOT-SUB)        TO D-QTY.
           MOVE 100.00                  TO D-QTY-PCT.
           MOVE TOT-BALANCE(TOT-SUB)    TO D-CURBAL.
           MOVE 100.00                  TO D-BAL-PCT.
           MOVE TOT-NET-RECV(TOT-SUB)   TO D-NET.
           MOVE 100.00                  TO D-NET-PCT.
           MOVE TOT-CASH-FLOW(TOT-SUB)  TO D-CASH-FLOW.
           MOVE 100.00                  TO D-CASH-FLOW-PCT.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PREPARE-REPORT.
           EXIT.

      ******************************************************************
      *    DESC:  GETTING THE NUMBER OF MONTHS LEFT TILL LOAN EXPIRATION
      *    IN  :  LN-REC
      *    OUT :  MONTHS-TILL-EXPIRATION
      *           MONTHS-TILL-EXP-STAT
      *-----------------------------------------------------------------
      *    FOR ALL OF THIS TO WORK FOR ACCOUNTS PAST MATURITY, THE
      *    FOLLOWING SHOULD BE DONE:
      *
      *      1.  IN THE FIRST BRACKET (AMT01-BUF), YOU MUST SET TO IT
      *          LESS THAN ONE (1.00) FOR MONTHS-TILL-EXPIRATION WILL BE
      *          ZEROES IF PAST MATURITY.
      *
      *      2.  IN THE SECOND BRACKET (AMT02-BUF), YOU MUST SET TO IT
      *          ONE (1.00) OR BEYOND.  THIS WILL THEN PLACE 'ACTIVE'
      *          ACCOUNTS WITHIN THE REMAINING BRACKETS.
      *
      *      *NOTE* PLACING 1 IN MONTHS-TILL-EXPIRATION IF IT IS NOT
      *             PAST MATURITY AND IT IS LESS THAN ONE MONTH BEFORE
      *             EXPIRATION BECAUSE IT NEEDS TO HAVE SOMETHING OTHER
      *             THAN ZEROES OR IT WILL BE CONSIDERED AS PAST
      *             MATURITY.
      *
      ******************************************************************
       GET-MONTHS-TILL-EXPIRATION SECTION.
      ******************************************************************

           MOVE ZEROES TO MONTHS-TILL-EXPIRATION.
           MOVE SPACES TO MONTHS-TILL-EXP-STAT.

           MOVE SPACES          TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE       TO SYS-DATE.
           MOVE YEAR-END-DATE   TO NUM-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.

           IF ( ELAPSED-DAYS < ZEROES )
              MOVE "X" TO MONTHS-TILL-EXP-STAT
              GO TO GET-MONTHS-TILL-EXP-EXIT.

           IF ( ELAPSED-MONTHS = ZEROES )
              MOVE 1              TO MONTHS-TILL-EXPIRATION
           ELSE
              MOVE ELAPSED-MONTHS TO MONTHS-TILL-EXPIRATION.

       GET-MONTHS-TILL-EXP-EXIT.
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
      
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD5 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE 6 TO LN-COUNT.
       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           PERFORM HEAD-PAGE.
           MOVE 3 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP     " TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BR TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BR TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGIN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS     TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "END   CLASS" TO RANGE-LINE.
           MOVE END-CLASS     TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGIN LOAN DATE" TO RANGE-LINE.
           MOVE BEG-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "END LOAN DATE" TO RANGE-LINE.
           MOVE END-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "YEAR END " TO RANGE-LINE.
           MOVE YEAR-END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "LEVEL TOTALS" TO RANGE-LINE.
           MOVE TOTAL-LEV-TABLE TO RANGE-SEL.
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

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO SUMPMT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SUMPMT-SYSDATE.
           DISPLAY SUMPMT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SUMPMT-FORM-FIELDS TO WR-BUF.
           MOVE SUMPMT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE SUMPMT-HELP--FILE TO HELP-LINK-FILE.
           MOVE SUMPMT-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/SUMPMT_EVA.CPY".
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
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF NOT IO-GOOD
                 MOVE "INVALID STATE PARAMETER RECORD" TO MESS
                 PERFORM SEND-MESS
                 MOVE "X" TO EXT-EOBUF-ERRCD
                 GO TO IO-ERROR.

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
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/RBACT.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGPRN.CPY".
   
      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LNFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

              MOVE BEG-LN-DATE          TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WBEG-LOANDATE.
              MOVE END-LN-DATE          TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WEND-LOANDATE.


      *  STORED PROCEDURE:
      *
      *  SELECT LNFILE.LN_OWNBR,
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_CLASS,
      *         LNFILE.LN_SUBCLASS,
      *         LNFILE.LN_LAWCODE,
      *         LNFILE.LN_ORGST,
      *         LNFILE.LN_SPRCLASS,
      *         LNFILE.LN_CONVERCD, 
      *         LNFILE.LN_POCD, 
      *         LNFILE.LN_STATUSFG, 
      *         LNFILE.LN_SALEFIN, 
      *         LNFILE.LN_LOANTYPE, 
      *         LNFILE.LN_ADDON_FG, 
      *         LNFILE.LN_PAY_SCHLD_FG, 
      *         CAST(LNFILE.LN_LOANDATE         AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_PURDATE          AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INTDATE          AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_CASHDATE         AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_ORIG_1STPYDATE   AS VARCHAR(10)),
      *         CAST(LNFILE.LN_1STPYDATE        AS VARCHAR(10)),
      *         CAST(LNFILE.LN_JDDATE           AS VARCHAR(10)),
      *         CAST(LNFILE.LN_PLDATE           AS VARCHAR(10)),
      *         CAST(LNFILE.LN_ACCELDATE        AS VARCHAR(10)),
      *         CAST(LNFILE.LN_REDUDATE         AS VARCHAR(10)),
      *         LNFILE.LN_APRATE,
      *         LNFILE.LN_EFFRATE,
      *         LNFILE.LN_SMPRATE,
      *         LNFILE.LN_OVRDRATE,
      *         LNFILE.LN_JDRATE,
      *         LNFILE.LN_LNAMT,
      *         LNFILE.LN_FINCHG,
      *         LNFILE.LN_INTCHG,
      *         LNFILE.LN_SERCHG,
      *         LNFILE.LN_EXTCHG,
      *         LNFILE.LN_MAINTFEE,
      *         LNFILE.LN_POINTS,
      *         LNFILE.LN_ORGTERM,
      *         LNFILE.LN_1STPYAMT,
      *         LNFILE.LN_NOREGPY,
      *         LNFILE.LN_REGPYAMT,
      *         LNFILE.LN_LASTPYAMT,
      *         LNFILE.LN_FEECODE_1, 
      *         LNFILE.LN_FEECODE_2, 
      *         LNFILE.LN_FEECODE_3, 
      *         LNFILE.LN_FEECODE_4, 
      *         LNFILE.LN_FEECODE_5, 
      *         LNFILE.LN_FEECODE_6, 
      *         LNFILE.LN_FEECODE_7, 
      *         LNFILE.LN_FEECODE_8, 
      *         LNFILE.LN_FEECODE_9, 
      *         LNFILE.LN_FEECODE_10, 
      *         LNFILE.LN_FEECODE_11, 
      *         LNFILE.LN_FEECODE_12, 
      *         LNFILE.LN_FEECODE_13, 
      *         LNFILE.LN_FEECODE_14, 
      *         LNFILE.LN_FEECODE_15, 
      *         LNFILE.LN_FEECODE_16, 
      *         LNFILE.LN_FEECODE_17, 
      *         LNFILE.LN_FEECODE_18, 
      *         LNFILE.LN_FEECODE_19, 
      *         LNFILE.LN_FEECODE_20, 
      *         LNFILE.LN_FEEAMT_1,
      *         LNFILE.LN_FEEAMT_2,
      *         LNFILE.LN_FEEAMT_3,
      *         LNFILE.LN_FEEAMT_4,
      *         LNFILE.LN_FEEAMT_5,
      *         LNFILE.LN_FEEAMT_6,
      *         LNFILE.LN_FEEAMT_7,
      *         LNFILE.LN_FEEAMT_8,
      *         LNFILE.LN_FEEAMT_9,
      *         LNFILE.LN_FEEAMT_10,
      *         LNFILE.LN_FEEAMT_11,
      *         LNFILE.LN_FEEAMT_12,
      *         LNFILE.LN_FEEAMT_13,
      *         LNFILE.LN_FEEAMT_14,
      *         LNFILE.LN_FEEAMT_15,
      *         LNFILE.LN_FEEAMT_16,
      *         LNFILE.LN_FEEAMT_17,
      *         LNFILE.LN_FEEAMT_18,
      *         LNFILE.LN_FEEAMT_19,
      *         LNFILE.LN_FEEAMT_20,
      *         LNFILE.LN_FEEOURS_1, 
      *         LNFILE.LN_FEEOURS_2, 
      *         LNFILE.LN_FEEOURS_3, 
      *         LNFILE.LN_FEEOURS_4, 
      *         LNFILE.LN_FEEOURS_5, 
      *         LNFILE.LN_FEEOURS_6, 
      *         LNFILE.LN_FEEOURS_7, 
      *         LNFILE.LN_FEEOURS_8, 
      *         LNFILE.LN_FEEOURS_9, 
      *         LNFILE.LN_FEEOURS_10, 
      *         LNFILE.LN_FEEOURS_11, 
      *         LNFILE.LN_FEEOURS_12, 
      *         LNFILE.LN_FEEOURS_13, 
      *         LNFILE.LN_FEEOURS_14, 
      *         LNFILE.LN_FEEOURS_15, 
      *         LNFILE.LN_FEEOURS_16, 
      *         LNFILE.LN_FEEOURS_17, 
      *         LNFILE.LN_FEEOURS_18, 
      *         LNFILE.LN_FEEOURS_19, 
      *         LNFILE.LN_FEEOURS_20, 
      *         LNFILE.LN_FEEFINCHG_1, 
      *         LNFILE.LN_FEEFINCHG_2, 
      *         LNFILE.LN_FEEFINCHG_3, 
      *         LNFILE.LN_FEEFINCHG_4, 
      *         LNFILE.LN_FEEFINCHG_5, 
      *         LNFILE.LN_FEEFINCHG_6, 
      *         LNFILE.LN_FEEFINCHG_7, 
      *         LNFILE.LN_FEEFINCHG_8, 
      *         LNFILE.LN_FEEFINCHG_9, 
      *         LNFILE.LN_FEEFINCHG_10, 
      *         LNFILE.LN_FEEFINCHG_11, 
      *         LNFILE.LN_FEEFINCHG_12, 
      *         LNFILE.LN_FEEFINCHG_13, 
      *         LNFILE.LN_FEEFINCHG_14, 
      *         LNFILE.LN_FEEFINCHG_15, 
      *         LNFILE.LN_FEEFINCHG_16, 
      *         LNFILE.LN_FEEFINCHG_17, 
      *         LNFILE.LN_FEEFINCHG_18, 
      *         LNFILE.LN_FEEFINCHG_19, 
      *         LNFILE.LN_FEEFINCHG_20, 
      *         LNFILE.LN_INSURED, 
      *         LNFILE.LN_INSTYPES, 
      *         LNFILE.LN_INSCOMP_1,
      *         LNFILE.LN_INSCOMP_2,
      *         LNFILE.LN_INSCOMP_3,
      *         LNFILE.LN_INSCOMP_4,
      *         LNFILE.LN_INSCOMP_5,
      *         LNFILE.LN_INSCOMP_6,
      *         LNFILE.LN_INSCOMP_7,
      *         LNFILE.LN_INSCOMP_8,
      *         LNFILE.LN_INSCOMP_9,
      *         LNFILE.LN_INSCOMP_10,
      *         LNFILE.LN_INSOURS_1, 
      *         LNFILE.LN_INSOURS_2, 
      *         LNFILE.LN_INSOURS_3, 
      *         LNFILE.LN_INSOURS_4, 
      *         LNFILE.LN_INSOURS_5, 
      *         LNFILE.LN_INSOURS_6, 
      *         LNFILE.LN_INSOURS_7, 
      *         LNFILE.LN_INSOURS_8, 
      *         LNFILE.LN_INSOURS_9, 
      *         LNFILE.LN_INSOURS_10, 
      *         CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)), 
      *         LNFILE.LN_INSPREM_1,
      *         LNFILE.LN_INSPREM_2,
      *         LNFILE.LN_INSPREM_3,
      *         LNFILE.LN_INSPREM_4,
      *         LNFILE.LN_INSPREM_5,
      *         LNFILE.LN_INSPREM_6,
      *         LNFILE.LN_INSPREM_7,
      *         LNFILE.LN_INSPREM_8,
      *         LNFILE.LN_INSPREM_9,
      *         LNFILE.LN_INSPREM_10,
      *         LNFILE.LN_INSCOVR_1,
      *         LNFILE.LN_INSCOVR_2,
      *         LNFILE.LN_INSCOVR_3,
      *         LNFILE.LN_INSCOVR_4,
      *         LNFILE.LN_INSCOVR_5,
      *         LNFILE.LN_INSCOVR_6,
      *         LNFILE.LN_INSCOVR_7,
      *         LNFILE.LN_INSCOVR_8,
      *         LNFILE.LN_INSCOVR_9,
      *         LNFILE.LN_INSCOVR_10,
      *         LNFILE.LN_INSFINCHG_1, 
      *         LNFILE.LN_INSFINCHG_2, 
      *         LNFILE.LN_INSFINCHG_3, 
      *         LNFILE.LN_INSFINCHG_4, 
      *         LNFILE.LN_INSFINCHG_5, 
      *         LNFILE.LN_INSFINCHG_6, 
      *         LNFILE.LN_INSFINCHG_7, 
      *         LNFILE.LN_INSFINCHG_8, 
      *         LNFILE.LN_INSFINCHG_9, 
      *         LNFILE.LN_INSFINCHG_10, 
      *         LNFILE.LN_CURBAL,
      *         LNFILE.LN_INTBAL,
      *         LNFILE.LN_LCBAL,
      *         LNFILE.LN_OTHBAL,
      *         LNFILE.LN_OT2BAL,
      *         LNFILE.LN_ANTICERN_1,
      *         LNFILE.LN_ANTICERN_2,
      *         LNFILE.LN_ANTICERN_3,
      *         LNFILE.LN_ANTICERN_4,
      *         LNFILE.LN_ANTICERN_5,
      *         LNFILE.LN_ANTICERN_6,
      *         LNFILE.LN_ANTICERN_7,
      *         LNFILE.LN_ANTICERN_8,
      *         LNFILE.LN_ANTICERN_9,
      *         LNFILE.LN_ANTICERN_10,
      *         LNFILE.LN_ANTICADJ_1,
      *         LNFILE.LN_ANTICADJ_2,
      *         LNFILE.LN_ANTICADJ_3,
      *         LNFILE.LN_ANTICADJ_4,
      *         LNFILE.LN_ANTICADJ_5,
      *         LNFILE.LN_ANTICADJ_6,
      *         LNFILE.LN_ANTICADJ_7,
      *         LNFILE.LN_ANTICADJ_8,
      *         LNFILE.LN_ANTICADJ_9,
      *         LNFILE.LN_ANTICADJ_10,
      *         LNFILE.LN_UFERN_1,
      *         LNFILE.LN_UFERN_2,
      *         LNFILE.LN_UFERN_3,
      *         LNFILE.LN_UFERN_4,
      *         LNFILE.LN_UFERN_5,
      *         LNFILE.LN_UFERN_6,
      *         LNFILE.LN_UFERN_7,
      *         LNFILE.LN_UFERN_8,
      *         LNFILE.LN_UFERN_9,
      *         LNFILE.LN_UFERN_10,
      *         LNFILE.LN_TOTINT,
      *         LNFILE.LN_TOTDEF,
      *         LNFILE.LN_TOTNODEF,
      *         LNFILE.LN_CL_REBATE, 
      *         LNFILE.LN_AH_REBATE, 
      *         LNFILE.LN_PP_REBATE, 
      *         LNFILE.LN_INT_REBATE, 
      *         LNFILE.LN_SC_REBATE, 
      *         LNFILE.LN_MF_REBATE, 
      *         LNFILE.LN_DF_REBATE, 
      *         LNFILE.LN_EX_REBATE, 
      *         LNFILE.LN_O1_REBATE, 
      *         LNFILE.LN_O2_REBATE, 
      *         LNFILE.LN_O3_REBATE, 
      *         LNFILE.LN_O4_REBATE, 
      *         LNFILE.LN_O5_REBATE, 
      *         LNFILE.LN_O6_REBATE, 
      *         LNFILE.LN_O7_REBATE, 
      *         LNFILE.LN_UN_O7_REB, 
      *         LNFILE.LN_UN_O8_REB, 
      *         LNFILE.LN_UN_O9_REB, 
      *         LNFILE.LN_UN_O10_REB, 
      *         CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)), 
      *         LNFILE.LN_CREDITLIM,
      *         LNFILE.LN_OLDACCTNO,
      *         LNFILE.LN_PRLNNO,
      *         LNFILE.LN_SUM_FINCHG,
      *         LNFILE.LN_SUM_ORGTERM,
      *         LNFILE.LN_SUM_REGPYAMT,
      *         LNFILE.LN_UNITPER_CD, 
      *         LNFILE.LN_UNITPER_FREQ,
      *         LNFILE.LN_REPOCD 
      *
      *    FROM DBO.LNFILE  LNFILE
      *
      *   WHERE (  LNFILE.LN_OWNBR          = @QLN3_WBEG_OWNBR
      *     AND    LNFILE.LN_CLASS    BETWEEN @QLN3_WBEG_CLASS
      *                                   AND @QLN3_WEND_CLASS 
      *     AND    LNFILE.LN_LOANDATE BETWEEN @QWS1_WBEG_LOANDATE 
      *                                   AND @QWS1_WEND_LOANDATE )
      *
      *     AND (  LNFILE.LN_POCD      NOT IN ('**', '*I', '*R', '*U') )
      *     AND (  LNFILE.LN_PLDATE         = '1900-01-01' )
      *     AND (  LNFILE.LN_CURBAL        <> 0
      *      OR    LNFILE.LN_INTBAL        <> 0
      *      OR    LNFILE.LN_LCBAL         <> 0
      *      OR    LNFILE.LN_OTHBAL        <> 0 ) 
      *
      *   ORDER BY LNFILE.LN_OWNBR,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_ACCTNO; 


           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_SUMPMT_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR, 
                :QLN3-WBEG-CLASS, 
                :QLN3-WEND-CLASS,
                :QWS1-WBEG-LOANDATE,
                :QWS1-WEND-LOANDATE )
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.  
    
           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL 
                   FETCH NEXT LN3_CURSOR INTO 
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD, 
                     :QLN-POCD, 
                     :QLN-STATUSFG, 
                     :QLN-SALEFIN, 
                     :QLN-LOANTYPE, 
                     :QLN-ADDON-FG, 
                     :QLN-PAY-SCHLD-FG, 
                     :QLN-LOANDATE,          
                     :QLN-PURDATE,           
                     :QLN-INTDATE,           
                     :QLN-CASHDATE,          
                     :QLN-ORIG-1STPYDATE,    
                     :QLN-1STPYDATE,         
                     :QLN-JDDATE,            
                     :QLN-PLDATE,            
                     :QLN-ACCELDATE,         
                     :QLN-REDUDATE,         
                     :QLN-APRATE,
                     :QLN-EFFRATE,
                     :QLN-SMPRATE,
                     :QLN-OVRDRATE,
                     :QLN-JDRATE,
                     :QLN-LNAMT,
                     :QLN-FINCHG,
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-POINTS,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11, 
                     :QLN-FEECODE-12, 
                     :QLN-FEECODE-13, 
                     :QLN-FEECODE-14, 
                     :QLN-FEECODE-15, 
                     :QLN-FEECODE-16, 
                     :QLN-FEECODE-17, 
                     :QLN-FEECODE-18, 
                     :QLN-FEECODE-19, 
                     :QLN-FEECODE-20, 
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-FEEOURS-1, 
                     :QLN-FEEOURS-2, 
                     :QLN-FEEOURS-3, 
                     :QLN-FEEOURS-4, 
                     :QLN-FEEOURS-5, 
                     :QLN-FEEOURS-6, 
                     :QLN-FEEOURS-7, 
                     :QLN-FEEOURS-8, 
                     :QLN-FEEOURS-9, 
                     :QLN-FEEOURS-10, 
                     :QLN-FEEOURS-11, 
                     :QLN-FEEOURS-12, 
                     :QLN-FEEOURS-13, 
                     :QLN-FEEOURS-14, 
                     :QLN-FEEOURS-15, 
                     :QLN-FEEOURS-16, 
                     :QLN-FEEOURS-17, 
                     :QLN-FEEOURS-18, 
                     :QLN-FEEOURS-19, 
                     :QLN-FEEOURS-20, 
                     :QLN-FEEFINCHG-1, 
                     :QLN-FEEFINCHG-2, 
                     :QLN-FEEFINCHG-3, 
                     :QLN-FEEFINCHG-4, 
                     :QLN-FEEFINCHG-5, 
                     :QLN-FEEFINCHG-6, 
                     :QLN-FEEFINCHG-7, 
                     :QLN-FEEFINCHG-8, 
                     :QLN-FEEFINCHG-9, 
                     :QLN-FEEFINCHG-10, 
                     :QLN-FEEFINCHG-11, 
                     :QLN-FEEFINCHG-12, 
                     :QLN-FEEFINCHG-13, 
                     :QLN-FEEFINCHG-14, 
                     :QLN-FEEFINCHG-15, 
                     :QLN-FEEFINCHG-16, 
                     :QLN-FEEFINCHG-17, 
                     :QLN-FEEFINCHG-18, 
                     :QLN-FEEFINCHG-19, 
                     :QLN-FEEFINCHG-20, 
                     :QLN-INSURED, 
                     :QLN-INSTYPES, 
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSOURS-1, 
                     :QLN-INSOURS-2, 
                     :QLN-INSOURS-3, 
                     :QLN-INSOURS-4, 
                     :QLN-INSOURS-5, 
                     :QLN-INSOURS-6, 
                     :QLN-INSOURS-7, 
                     :QLN-INSOURS-8, 
                     :QLN-INSOURS-9, 
                     :QLN-INSOURS-10, 
                     :QLN-INSEFF-DATE-1, 
                     :QLN-INSEFF-DATE-2, 
                     :QLN-INSEFF-DATE-3, 
                     :QLN-INSEFF-DATE-4, 
                     :QLN-INSEFF-DATE-5, 
                     :QLN-INSEFF-DATE-6, 
                     :QLN-INSEFF-DATE-7, 
                     :QLN-INSEFF-DATE-8, 
                     :QLN-INSEFF-DATE-9, 
                     :QLN-INSEFF-DATE-1, 
                     :QLN-INSEXP-DATE-1,  
                     :QLN-INSEXP-DATE-2,  
                     :QLN-INSEXP-DATE-3,  
                     :QLN-INSEXP-DATE-4,  
                     :QLN-INSEXP-DATE-5,  
                     :QLN-INSEXP-DATE-6,  
                     :QLN-INSEXP-DATE-7,  
                     :QLN-INSEXP-DATE-8,  
                     :QLN-INSEXP-DATE-9,  
                     :QLN-INSEXP-DATE-10, 
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-INSFINCHG-1, 
                     :QLN-INSFINCHG-2, 
                     :QLN-INSFINCHG-3, 
                     :QLN-INSFINCHG-4, 
                     :QLN-INSFINCHG-5, 
                     :QLN-INSFINCHG-6, 
                     :QLN-INSFINCHG-7, 
                     :QLN-INSFINCHG-8, 
                     :QLN-INSFINCHG-9, 
                     :QLN-INSFINCHG-10, 
                     :QLN-CURBAL,
                     :QLN-INTBAL,
                     :QLN-LCBAL,
                     :QLN-OTHBAL,
                     :QLN-OT2BAL,
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-UFERN-1,
                     :QLN-UFERN-2,
                     :QLN-UFERN-3,
                     :QLN-UFERN-4,
                     :QLN-UFERN-5,
                     :QLN-UFERN-6,
                     :QLN-UFERN-7,
                     :QLN-UFERN-8,
                     :QLN-UFERN-9,
                     :QLN-UFERN-10,
                     :QLN-TOTINT,
                     :QLN-TOTDEF,
                     :QLN-TOTNODEF,
                     :QLN-CL-REBATE, 
                     :QLN-AH-REBATE, 
                     :QLN-PP-REBATE, 
                     :QLN-INT-REBATE, 
                     :QLN-SC-REBATE, 
                     :QLN-MF-REBATE, 
                     :QLN-DF-REBATE, 
                     :QLN-EX-REBATE, 
                     :QLN-O1-REBATE, 
                     :QLN-O2-REBATE, 
                     :QLN-O3-REBATE, 
                     :QLN-O4-REBATE, 
                     :QLN-O5-REBATE, 
                     :QLN-O6-REBATE, 
                     :QLN-O7-REBATE, 
                     :QLN-UN-O7-REB, 
                     :QLN-UN-O8-REB, 
                     :QLN-UN-O9-REB, 
                     :QLN-UN-O10-REB, 
                     :QLN-DATE-PAID-LAST,  
                     :QLN-CREDITLIM,
                     :QLN-OLDACCTNO,
                     :QLN-PRLNNO,
                     :QLN-SUM-FINCHG,
                     :QLN-SUM-ORGTERM,
                     :QLN-SUM-REGPYAMT,
                     :QLN-UNITPER-CD, 
                     :QLN-UNITPER-FREQ,
                     :QLN-REPOCD 
              END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
               GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
               PERFORM GET-LN-FIELDS.
 
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
  
      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.


      * COPY "LIBLP/LPLC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LC1-FILE. *> EMBEDDED - LCFILE
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   LCFILE.LC_DESC 
              INTO 
                  :QLC-CODE, 
                  :QLC-DESC
              FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.
           PERFORM CLOSE-IT.


       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
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
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              MOVE "LP/LPEYMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SUMPMT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
