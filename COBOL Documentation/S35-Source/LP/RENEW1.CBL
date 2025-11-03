       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RENEW1.
       DATE-WRITTEN.    080213.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  PB LESS THAN 10% REPORT/EXTRACT
      *                  WORLD PR# 916
      *
      * REV:
      * BLM 130802 NEW, WORLD PR# 916
      * BLM 130912 ADD TOTAL-NOTE TO DETAIL-LINE , FIX GROUP HEADING, #928
      * BLM 131003 ADD TOTAL-NOTE TO EXTRACT FILE, WORLD PR# 936
      * BLM 131017 ADD OPTION TO SELECT WHICH METHOD TO CALC WHETHER
      *            RENEWAL IS LESS THAN 10%. WORLD PR# 938
      * BLM 131021 FOR METHOD 2 OF COMPUTING LOANS < 10%, ADD FEE
      *            CODES 90, 92 & 95 TO PROCEEDS BEFORE DIVIDING BY
      *            PRIOR-PAYOFF, WORLD PR# 939
      * BLM 131022 REMOVE ROUNDING FROM COMPUTATION OF LOANS < 10%, SO
      *            RESULT MUST BE AT LEAST 10.0 TO BE A > 10% RENEWAL
      *            WORLD PR# 939
      * KEC 2017.0223 CHANGED WS CLASS FIELDS TO MATCH LN-CLASS & LC-CODE.
      * BAH 2017.0712 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.1211 GLOBAL LNFILE
      * BAH 2019.0930 REMOVED ACCESS-CALL LNFILE
      * RMZ 2023.0925 PERFORMANCE TUNING - ADDED STORED PROCEDURE 
      * RMZ 2024.0116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * BAH 2024.0620 GROUP TOTAL FLAGS WERE NOT DISPLAYING IN BATCH PAGE 2;
      *               MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP #1661 S35Q-72
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      **************************************************
      *   EXTRACT FILE OF PB'S < 10%
      **************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-BRNO                    PIC 9(4).
           03  WK-CLASS                   PIC 9(3).
           03  WK-ACCTNO                  PIC 9(6).
           03  WK-LOANDATE-MMDDYY         PIC 9(6).
           03  WK-LNAME                   PIC X(16).
           03  WK-TOTAL-NOTE              PIC 9(6).99-.
           03  WK-AMT-FINANCED            PIC 9(6).99-.
           03  WK-PRIOR-PAYOFF            PIC 9(6).99-.
           03  WK-PROCEEDS                PIC 9(6).99-.
           03  WK-TERM                    PIC 9(3).
           03  WK-CR                      PIC X.
           03  WK-LF                      PIC X.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/RENEW1.CBL S35 02/20/24-15:28:47 >".

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

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS1-WBEG-LOANDATE   PIC X(10).                   
       01  QWS1-WEND-LOANDATE   PIC X(10).                
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-KEY               PIC 9(6)     VALUE 0.
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK-PATH.
           03  WK-PATH-BASE            PIC X(9).
           03  FILLER                   PIC X(1)   VALUE "/".
           03  WK-PATH-MACHINE          PIC X(02).
           03  FILLER                   PIC X(12)  VALUE "/SP/RENEW10.".
           03  WK-PATH-DATE-YYMMDD      PIC 9(6).

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
       01  SUB                  PIC 9(3)     COMP.
       01  LEVEL-SUB            PIC 9999.
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  TOT-FEES             PIC 9(5)V99.

       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".

       01  FIRST-TIME           PIC X         VALUE "Y".
       01  NEW-BRANCH-FG        PIC X         VALUE "Y".
       01  LEGEND               PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST          PIC X(09)     VALUE "LEGEND:B.".
       01  HOLD-BRANCH          PIC 9(4)       COMP   VALUE 0.
       01  WS-AMOUNT       PIC S9(9)V99  VALUE 0.
       01  WS-AMTFIN       PIC S9(9)V99  VALUE 0.
       01  PRIOR-PAYOFF    PIC S9(9)V99  VALUE 0.
       01  MESS-LIST            PIC X(5)      VALUE "MESS.".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  BEG-BR                PIC 9(4).
           03  END-BR                PIC 9(4).
           03  BEG-CLASS             PIC 9(3).
           03  END-CLASS             PIC 9(3).
           03  BEG-LN-DATE           PIC 9(8).
           03  END-LN-DATE           PIC 9(8).
           03  GROUP-FLAG            PIC X.
           03  ENT-GROUP             PIC X(4).
           03  INCLUDE-DETAIL        PIC X.
           03  CREATE-FILE-FG        PIC X.
           03  GTT-ISO               PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.
           03  CALC-METHOD           PIC 9.

       01  VERSION-CONTROL                    PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       01  HOLD-CLASS              PIC 9(3)           VALUE 0.

      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB            OCCURS 10.
               05  CLASS-TABLE      OCCURS 100.
                   07  TOT-COUNT        PIC S9(10)    COMP.
                   07  TOT-TOTAL-NOTE   PIC S9(10)V99 COMP.
                   07  TOT-AMTFIN       PIC S9(10)V99 COMP.
                   07  TOT-PRIOR-PAYOFF PIC S9(10)V99 COMP.
                   07  TOT-PROCEEDS     PIC S9(10)V99 COMP.
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
           03  FILLER           PIC X(24)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(44)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER           PIC X(9)     VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(36)    VALUE
           "PB RENEWAL < 10% REPORT/EXTRACTION: ".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(6)    VALUE " THRU ".
           03  H-END-DATE       PIC 99/99/99.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-METHOD         PIC X(42)    VALUE " ".

       
       01  HEAD3.
           03  FILLER           PIC X(9)  VALUE "BRANCH  :".
           03  H-GRID.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(44)    VALUE SPACES.

       
       01  HEAD4.
           03  FILLER           PIC X(24) VALUE
               "BRNO  ACCOUNT NO.       ".
           03  FILLER           PIC X(27) VALUE
               "LOANDATE  NAME             ".
           03  FILLER           PIC X(48) VALUE
               "     TOTAL NOTE    AMT FINANCED    PRIOR PAYOFF ".
           03  FILLER           PIC X(22) VALUE
               "       PROCEEDS   TERM".
       
       01  TOTAL-HEAD.
           03  FILLER           PIC X(38) VALUE SPACES.
           03  FILLER           PIC X(13) VALUE
               "   NO. LOANS ".
           03  FILLER           PIC X(48) VALUE
               "     TOTAL NOTE    AMT FINANCED    PRIOR PAYOFF ".
           03  FILLER           PIC X(22) VALUE
               "       PROCEEDS       ".
       
       01  STAR-LINE.
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(41) VALUE
           "-----------------------------------------".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       
       01  DETAIL1             REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z999.
           03  FILLER           PIC XX.
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC XX.
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC XX.
           03  D-LNAME          PIC X(17).
           03  D-TOTAL-NOTE     PIC ZZZZ,ZZZ,ZZZ.ZZ-.
           03  D-AMTFIN         PIC ZZZZ,ZZZ,ZZZ.ZZ-.
           03  D-PRIOR-PAYOFF   PIC ZZZZ,ZZZ,ZZZ.ZZ-.
           03  D-PROCEEDS       PIC ZZZZ,ZZZ,ZZZ.ZZ-.
           03  FILLER           PIC XXX.
           03  D-TERM           PIC Z99.
       
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  G-GROUP              PIC X(20).
           03  G-GROUP-2 REDEFINES G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(8).
           03  FILLER         PIC X(2).
           03  G-CLASS-PROMPT.
               05  G-CLASS-P1  PIC X(6).
               05  G-CLASS-NO  PIC 99.
               05  G-CLASS-P2  PIC X(7).
               05  G-CLASS-COL PIC X.
           03  G-COUNT         PIC ZZZZ,ZZZ,ZZ9-.
           03  G-TOTAL-NOTE    PIC ZZZZ,ZZZ,ZZ9.99-.
           03  G-AMTFIN        PIC ZZZZ,ZZZ,ZZ9.99-.
           03  G-PRIOR-PAYOFF  PIC ZZZZ,ZZZ,ZZ9.99-.
           03  G-PROCEEDS      PIC ZZZZ,ZZZ,ZZ9.99-.

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
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/RENEW1_DEF.CPY".
       COPY "LIBLP/RENEW1_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/RENEW1_SCN.CPY".

       
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
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

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RENEW1" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE BR-NO TO HOLD-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           IF EXT-ROUTE-BUF = "00"
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

           PERFORM START-LN3-FILE. *> SSP - LN,BW
       
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-PROGRESS-INDICATOR.

      *  POCD - **  IS A DELETED CONTRACT
       
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.
       
           IF ( LN-CLASS < BEG-CLASS )
              GO TO NEXT-LN.

           IF ( LN-CLASS > END-CLASS )
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF LN-SRCD NOT = "PB"
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-LN-DATE OR LN-LOANDATE > END-LN-DATE
              GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR.

           PERFORM LOAN-CALCULATIONS.

           COMPUTE WS-AMTFIN = TOTAL-NOTE - LN-INTCHG - LN-SERCHG.
           COMPUTE PRIOR-PAYOFF ROUNDED = LN-PRBAL + LN-PRINTDUE
                                       - LN-PRFINCHGRB - LN-PRINSRB.

      * METHOD 1: AMOUNT FINANCED ON THE PB LOAN, SUBTRACT PRIOR LOAN PAYOFF,
      *           TAKE THAT AMT AND DIVIDE BY THE PRIOR LOAN PAYOFF,
      *           INCLUDE IF THE RESULT IS < 10% (.10)

      * METHOD 2 = ADD PROCEEDS ON THE PB LOAN PLUS FEE CODES 90, 92 & 95,
      *            AND DIVIDE THAT BY THE PRIOR LOAN PAYOFF,
      *            & SEE IF THE RESULT IS < 10% (.10)

           IF CALC-METHOD = 2
              MOVE 0 TO TOT-FEES
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                 IF LN-FEECODE(SUB) = "90"
                    ADD LN-FEEAMT(SUB) TO TOT-FEES
                 END-IF
                 IF LN-FEECODE(SUB) = "92"
                    ADD LN-FEEAMT(SUB) TO TOT-FEES
                 END-IF
                 IF LN-FEECODE(SUB) = "95"
                    ADD LN-FEEAMT(SUB) TO TOT-FEES
                 END-IF
              END-PERFORM.

           IF CALC-METHOD = 1
              COMPUTE WS-AMOUNT =
                         (WS-AMTFIN - PRIOR-PAYOFF) / PRIOR-PAYOFF
           ELSE
              COMPUTE WS-AMOUNT =
                           (LN-PROCEEDS + TOT-FEES)  / PRIOR-PAYOFF.

           IF WS-AMOUNT NOT < .10
              GO TO NEXT-LN.


           IF NEW-BRANCH-FG = "Y"
              MOVE "N" TO NEW-BRANCH-FG
              MOVE LN-CLASS TO HOLD-CLASS.

           IF (INCLUDE-DETAIL = "Y" OR CREATE-FILE-FG = "Y")
              MOVE LN-SSNO(1) TO BW-SSNO
      *        PERFORM READ-BW1-FILE
      *        IF IO-BAD
               IF BW-LNAME = SPACES
                 MOVE "INVALID BOR!" TO BW-LNAME.
           IF INCLUDE-DETAIL = "Y"
              PERFORM PROCESS-DETAIL-LINE.
           IF CREATE-FILE-FG = "Y"
              PERFORM CREATE-EXTRACT-FILE.

           MOVE "N" TO FIRST-TIME.
           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-LN.

       
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           GO TO NEXT-GROUP-LOC.

       
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
           MOVE TOTAL-NOTE       TO D-TOTAL-NOTE.
           MOVE WS-AMTFIN        TO D-AMTFIN.
           MOVE PRIOR-PAYOFF     TO D-PRIOR-PAYOFF.
           MOVE LN-PROCEEDS      TO D-PROCEEDS.
           MOVE LN-ORGTERM       TO D-TERM.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       CREATE-EXTRACT-FILE SECTION.
      ******************************************************************
           MOVE BR-NO            TO WK-BRNO.
           MOVE LN-CLASS         TO WK-CLASS.
           MOVE LN-ACCTNO        TO WK-ACCTNO.
           MOVE LN-LOANDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO WK-LOANDATE-MMDDYY.
           MOVE BW-LNAME         TO WK-LNAME.
           MOVE TOTAL-NOTE       TO WK-TOTAL-NOTE.
           MOVE WS-AMTFIN        TO WK-AMT-FINANCED.
           MOVE PRIOR-PAYOFF     TO WK-PRIOR-PAYOFF.
           MOVE LN-PROCEEDS      TO WK-PROCEEDS.
           MOVE LN-ORGTERM       TO WK-TERM.
           MOVE X"0D"            TO WK-CR.
           MOVE X"0A"            TO WK-LF.
           ADD 1                 TO WK-KEY.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE RENEW1-QUEUE-POS TO Q-POS.
           MOVE RENEW1-COPIES-POS TO C-POS.
           MOVE RENEW1-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

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

           IF EXT-ROUTE-BUF = "00"
              MOVE 9999 TO PROGID-SLEEP-COUNTER
              MOVE ZEROES TO BR-NO         
              PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BEG-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE END-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.

           IF CALC-METHOD = 1
              MOVE "METHOD 1: (AMTFIN - PR PAYOFF) / PR PAYOFF"
                                                          TO H-METHOD
           ELSE
              MOVE "    METHOD 2: PROCEEDS / PRIOR LOAN PAYOFF"
                                                          TO H-METHOD.
           MOVE LOW-VALUES TO TOTAL-TABLE.
           IF CREATE-FILE-FG = "N"
              GO TO EXIT-INITIALIZE.

      * CREATE OUTPUT ASCII FILE:
           IF EXT-ROUTE-BUF = "70"
              PERFORM OPEN-WK-FILE-OUTPUT
              GO TO EXIT-INITIALIZE.

           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-INITIALIZE.

           MOVE EXT-FILPATH-BASE    TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WK-PATH-MACHINE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYMMDD.
           MOVE DATE-YYMMDD         TO WK-PATH-DATE-YYMMDD.
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-WK-FILE-OUTPUT
              GO TO EXIT-INITIALIZE.

        INITIALIZATION-ASK.
           PERFORM ALARM.
           MOVE "FILE ALREADY EXISTS, CONTINUE (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA010CONT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO INITIALIZATION-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO EXIT-INITIALIZE.
           IF VDUBUF NOT = "Y"
              GO TO INITIALIZATION-ASK.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO VDUBUF.
           MOVE "S  A010CONT." TO VDU.
           PERFORM SCREENIO.

           PERFORM OPEN-WK-FILE-OUTPUT.

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
              GO TO EXTRACT-FILE-YN.

       
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

      *=================================================================
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

      *=================================================================
       DT-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-CALC-METHOD.
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
              GO TO ENTER-CALC-METHOD.
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

           GO TO ENTER-CALC-METHOD.

       
       ALL-DT.
           MOVE "S  8080DT1."  TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2."  TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       
       ENTER-CALC-METHOD.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010METHOD." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO BR-01
              ELSE
                 GO TO DT-02.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-CALC-METHOD.

           IF NOT ( VDUNUM0 = 1 OR 2 )
              GO TO ENTER-CALC-METHOD.

           MOVE VDUNUM0 TO CALC-METHOD.

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
              GO TO ENTER-CALC-METHOD.

           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(1)
                          TOTAL-LEVEL-TABLE(9).
       
       INCL-DETAIL.
           IF GTT-ISO = "O"
              MOVE "S  A010DETAIL." TO VDU
              MOVE "N" TO INCLUDE-DETAIL VDUBUF
              PERFORM SCREENIO
              GO TO EXTRACT-FILE-YN.

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

       
       EXTRACT-FILE-YN.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010FILEYN." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO INCL-DETAIL.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO EXTRACT-FILE-YN.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO EXTRACT-FILE-YN.

           MOVE VDUBUF TO CREATE-FILE-FG.

           IF CREATE-FILE-FG = "Y"
               MOVE 0  TO PASSWDW-SEQ
               MOVE "CREATE AN ASCII OUTPUT FILE"
                  TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  GO TO EXTRACT-FILE-YN.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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

       
           MOVE "SNNN010METHOD."    TO VDU.
           MOVE CALC-METHOD         TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "S  A010GTT-ISO."   TO VDU.
           MOVE GTT-ISO             TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010DETAIL."    TO VDU.
           MOVE INCLUDE-DETAIL      TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010FILEYN."    TO VDU.
           MOVE CREATE-FILE-FG      TO VDUBUF.
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
       ADD-TOTALS SECTION.
      ******************************************************************
           ADD 1            TO TOT-COUNT(TOT-SUB, LN-CLASS)
                               TOT-COUNT(TOT-SUB, 100).
           ADD TOTAL-NOTE   TO TOT-TOTAL-NOTE(TOT-SUB, LN-CLASS)
                               TOT-TOTAL-NOTE(TOT-SUB, 100).
           ADD WS-AMTFIN    TO TOT-AMTFIN(TOT-SUB, LN-CLASS)
                               TOT-AMTFIN(TOT-SUB, 100).
           ADD PRIOR-PAYOFF TO TOT-PRIOR-PAYOFF(TOT-SUB, LN-CLASS)
                               TOT-PRIOR-PAYOFF(TOT-SUB, 100).
           ADD LN-PROCEEDS  TO TOT-PROCEEDS(TOT-SUB, LN-CLASS)
                               TOT-PROCEEDS(TOT-SUB, 100).
       EXIT-ADD-TOTALS.
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

       
       DO-GROUP-TOTALS.
           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           PERFORM PRINT-TOTALS.

           GO TO CONTINUE-LOOP.

       
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

       
       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       
       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

       
      ***************************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      ***************************************************
           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.
           MOVE 9 TO TOT-SUB.
           PERFORM PRINT-TOTALS.

      ***************************************************
       PRINT-BRANCH-GRAND-TOTALS SECTION.
      ***************************************************
           IF BEG-BR = END-BR
              GO TO EXIT-PRINT-BRANCH-GRAND.

           MOVE 0 TO H-BRNO.
           MOVE "GRAND TOTALS" TO H-BRNAME.
           MOVE 1 TO TOT-SUB.
           PERFORM HEAD-PAGE.

           PERFORM PRINT-TOTALS.

       EXIT-PRINT-BRANCH-GRAND.
           EXIT.

      ***************************************************
       PRINT-TOTALS SECTION.
      ***************************************************
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
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 99
              IF NOT (TOT-COUNT(TOT-SUB SUB) =  0 AND
                      TOT-TOTAL-NOTE(TOT-SUB SUB) = 0 AND
                      TOT-AMTFIN(TOT-SUB SUB) = 0 AND
                      TOT-PRIOR-PAYOFF(TOT-SUB SUB) = 0 AND
                      TOT-PROCEEDS(TOT-SUB SUB) = 0)
                 MOVE "CLASS " TO G-CLASS-P1
                 MOVE SUB TO G-CLASS-NO
                 MOVE " TOTALS " TO G-CLASS-P2
                 MOVE ":" TO G-CLASS-COL
                 MOVE TOT-COUNT(TOT-SUB SUB)        TO G-COUNT
                 MOVE TOT-TOTAL-NOTE(TOT-SUB SUB)   TO G-TOTAL-NOTE
                 MOVE TOT-AMTFIN(TOT-SUB SUB)       TO G-AMTFIN
                 MOVE TOT-PRIOR-PAYOFF(TOT-SUB SUB) TO G-PRIOR-PAYOFF
                 MOVE TOT-PROCEEDS(TOT-SUB SUB)     TO G-PROCEEDS
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF TOT-SUB = 9
              MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT
              MOVE BR-NO     TO G-BRNO
           ELSE
              MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT
              MOVE GR-ID     TO G-GRNO.
           MOVE TOT-COUNT(TOT-SUB SUB)        TO G-COUNT.
           MOVE TOT-TOTAL-NOTE(TOT-SUB SUB)   TO G-TOTAL-NOTE.
           MOVE TOT-AMTFIN(TOT-SUB SUB)       TO G-AMTFIN.
           MOVE TOT-PRIOR-PAYOFF(TOT-SUB SUB) TO G-PRIOR-PAYOFF.
           MOVE TOT-PROCEEDS(TOT-SUB SUB)     TO G-PROCEEDS.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF INCLUDE-DETAIL = "Y"
              MOVE STAR-LINE TO DETAIL-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 99 TO LN-COUNT.

       EXIT-PRINT-TOTALS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).

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

           IF GTT-ISO NOT = "O"
              MOVE BR-NO TO H-BRNO
              MOVE BR-NAME TO H-BRNAME
              MOVE HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
       
           IF GTT-ISO = "O"
              MOVE TOTAL-HEAD TO PR-REC
           ELSE
              MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           IF GTT-ISO = "O"
              MOVE 3 TO LN-COUNT
           ELSE
              MOVE 4 TO LN-COUNT.
       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.


      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "RENEW1" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD TO " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

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
       
           MOVE "CLASS RANGE      :" TO RANGE-LINE.
           MOVE BEG-CLASS     TO RANGE-SEL1.
           MOVE " - " TO RANGE-DASH.
           MOVE END-CLASS     TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "LOAN DATE RANGE  :" TO RANGE-LINE.
           MOVE BEG-LN-DATE TO RANGE-DATE1.
           MOVE " - " TO RANGE-DASH.
           MOVE END-LN-DATE TO RANGE-DATE2.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE DETAIL   :" TO RANGE-LINE.
           MOVE INCLUDE-DETAIL TO RANGE-SEL1.
           PERFORM WRITE-DETAIL-LINE.
       
           IF CREATE-FILE-FG = "Y"
              MOVE "EXTRACT FILE PATH:" TO RANGE-LINE
              MOVE WK-PATH TO RANGE-SELECTIONS
              PERFORM WRITE-DETAIL-LINE.
       
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

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO RENEW1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RENEW1-SYSDATE.
           DISPLAY RENEW1-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RENEW1-FORM-FIELDS TO WR-BUF. 
           MOVE RENEW1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE RENEW1-HELP--FILE TO HELP-LINK-FILE.
           MOVE RENEW1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/RENEW1_EVA.CPY".
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
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPAMTS.CPY".
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
       START-LN3-FILE. *> SSP - LN,BW
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
      *         LNFILE.LN_SSNO1,
      *         LNFILE.LN_MAKERCD1,
      *         LNFILE.LN_SSNO2,
      *         LNFILE.LN_MAKERCD2,
      *         LNFILE.LN_SSNO3,
      *         LNFILE.LN_MAKERCD3,
      *         LNFILE.LN_SSNO4,
      *         LNFILE.LN_MAKERCD4, 
      *         LNFILE.LN_ORGST, 
      *         LNFILE.LN_SPRCLASS,
      *         LNFILE.LN_SRCD, 
      *         LNFILE.LN_POCD, 
      *         LNFILE.LN_LOANTYPE, 
      *         LNFILE.LN_STATFLAGS, 
      *         CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_1STPYDATE	    AS VARCHAR(10)), 
      *         LNFILE.LN_INTCHG,
      *         LNFILE.LN_SERCHG,
      *         LNFILE.LN_EXTCHG,
      *         LNFILE.LN_MAINTFEE,
      *         LNFILE.LN_PROCEEDS,
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
      *         LNFILE.LN_TOTNODEF,
      *         LNFILE.LN_CREDITLIM,
      *         LNFILE.LN_PRBAL,
      *         LNFILE.LN_PRINTDUE,
      *         LNFILE.LN_PRFINCHGRB,
      *         LNFILE.LN_PRINSRB,
      *         LNFILE.LN_SUM_ORGTERM,
      *         LNFILE.LN_UNITPER_CD,
      *         LNFILE.LN_UNITPER_FREQ,
      *         ISNULL(BWFILE.BW_OWNBR, 0),
      *         ISNULL(BWFILE.BW_SSNO, 0),
      *         ISNULL(BWFILE.BW_LNAME,' '),
      *         ISNULL(BWFILE.BW_FNAME,' '),
      *         ISNULL(BWFILE.BW_SOLICIT,' ')
      *
      *    FROM DBO.LNFILE  LNFILE
      *
      *    LEFT OUTER JOIN 
      *         DBO.BWFILE  BWFILE
      *      ON (  LNFILE.LN_OWNBR          = BWFILE.BW_OWNBR	
      *     AND    LNFILE.LN_SSNO1          = BWFILE.BW_SSNO )
      *
      *   WHERE (  LNFILE.LN_OWNBR          = @QLN3_WBEG_OWNBR
      *     AND    LNFILE.LN_CLASS    BETWEEN @QLN3_WBEG_CLASS
      *                                   AND @QLN3_WEND_CLASS 
      *     AND    LNFILE.LN_LOANDATE BETWEEN @QWS_WBEG_LOANDATE 
      *                                   AND @QWS_WEND_LOANDATE )
      *
      *     AND (  LNFILE.LN_POCD     NOT IN ('**', '*I', '*R', '*U'))
      *
      *    ORDER BY LNFILE.LN_OWNBR,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_ACCTNO; 


           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_RENEW1_LNFILE_SELECT 
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
       READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.   
    
           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL 
                   FETCH NEXT LN3_CURSOR INTO 
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4, 
                     :QLN-ORGST, 
                     :QLN-SPRCLASS,
                     :QLN-SRCD, 
                     :QLN-POCD, 
                     :QLN-LOANTYPE, 
                     :QLN-STATFLAGS, 
                     :QLN-LOANDATE,        
                     :QLN-ORIG-1STPYDATE,  
                     :QLN-1STPYDATE, 
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-PROCEEDS,
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
                     :QLN-TOTNODEF,
                     :QLN-CREDITLIM,
                     :QLN-PRBAL,
                     :QLN-PRINTDUE,
                     :QLN-PRFINCHGRB,
                     :QLN-PRINSRB,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QBW-OWNBR,  
                     :QBW-SSNO,  
                     :QBW-LNAME, 
                     :QBW-FNAME, 
                     :QBW-SOLICIT 
              END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
               GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
               PERFORM GET-LN-FIELDS
               PERFORM GET-BW-FIELDS.
 
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


       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
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
              DESTROY RENEW1-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
