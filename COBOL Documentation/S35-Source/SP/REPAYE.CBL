       IDENTIFICATION DIVISION.
       PROGRAM-ID.      REPAYE.
       DATE-WRITTEN.    11/28/2022.
      ***************************************************************
      *
      *    DESC:  CREATE ASCII LOAN FILE AND PAYMENT FILE
      *           ALL OPEN ACCOUNTS, NO RESCIND,
      *           NO DISCHARGE, POOLID < 80000 SET "Y"
      *           EXCLUDE PL < 01/01/2010
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/REPAY/REPAYLOAN.R?.YYMMDD
      *                                              REPAYPYMT.R?.YYMMDD
      *
      *  REV:
      * BAH 20221128 COPIED FROM REPAYE, NEW FOR REPAY #1579
      * BAH 20240221 ADD 2 NEW FIELDS TO END OF REPAY LOAN EXTRACT FILE FOR
      *              FUND TO DEBIT; WS-REPAY-F2D-AMOUNT AND WS-REPAY-IS-FUNDABLE
      *                                                        #1646
      * BAH 20240429 ADD WS-REPAY-NO-NXDUEAMT AND WS-REPAY-CLASS TO END OF
      *              REPAY RECORD #1655
      * MB  20250314 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *              ADDED STATE PARAMETER ARRAY PROCESSING   S35Q-184
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-LN-FILE ASSIGN OUTPUT WK-LN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LP-FILE ASSIGN OUTPUT WK-LP-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *   LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  WK-LN-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LN-REC.
           03  WK-LN-BYTE                PIC X OCCURS 2000 TIMES.

       FD  WK-LP-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LP-REC.
           03  WK-LP-BYTE                PIC X OCCURS 2000 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/REPAYE S35 04/29/24-10:48:58 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    W K - P A T H
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WK-LN-PATH.
           03  WK-LN-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  FILLER                       PIC X(17) 
                 VALUE "/REPAY/REPAYLOAN.".
           03  WK-LN-MACHINE                PIC X(2).
           03  FILLER                       PIC X VALUE ".".
           03  WK-LN-PATH-YR                PIC 99.
           03  WK-LN-PATH-MO                PIC 99.
           03  WK-LN-PATH-DA                PIC 99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    W K - L P - P A T H
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WK-LP-PATH.
           03  WK-LP-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  FILLER                       PIC X(17) 
                 VALUE "/REPAY/REPAYPYMT.".
           03  WK-LP-MACHINE                PIC XX.
           03  FILLER                       PIC X VALUE ".".
           03  WK-LP-PATH-YR                PIC 99.
           03  WK-LP-PATH-MO                PIC 99.
           03  WK-LP-PATH-DA                PIC 99.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-BEG-LN-DATE            PIC X(10).
       01  PASS-END-LN-DATE            PIC X(10).
       01  RETR-BW2-LNAME              PIC X(35).
       01  RETR-BW2-FNAME              PIC X(35).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LP1-CONN-OPEN-SW            PIC X    VALUE "N".
       01  LN3-CONN-OPEN-SW            PIC X    VALUE "N".
       01  TOT-BRANCH-LN-RECS          PIC 9(9) VALUE 0.

       01  WS-LN-REC.
           03  WS-LN-BRACCTNO              PIC Z(12).
           03  WS-LN-NUMBER                PIC Z(8).
           03  WS-LN-OWNBR                 PIC Z(6).
           03  WS-LN-SSNO1                 PIC 9(9).
           03  WS-BW-FNAME                 PIC X(16).
           03  WS-BW-LNAME                 PIC X(16).
           03  WS-BW-ADR1                  PIC X(24).
           03  WS-BW-ADR2                  PIC X(24).
           03  WS-BW-CITY                  PIC X(15).
           03  WS-BW-STATE                 PIC X(2).
           03  WS-BW-ZIP                   PIC X(09).
           03  WS-BW-AGE                   PIC 9(8).
           03  WS-BW-PHONE                 PIC 9(10).
           03  WS-BW-CELL-PHONE            PIC 9(10).
           03  WS-LN-LOANDATE              PIC 9(8).
           03  WS-LN-LNAMT                 PIC Z(8).99.
           03  WS-LN-BAL-OR-PO             PIC Z(8).99.
           03  WS-LN-1STPYAMT              PIC Z(8).99.
           03  WS-LN-REGPYAMT              PIC Z(8).99.
           03  WS-LN-AMT-PAID-LAST         PIC Z(8).99.
           03  WS-LN-DATE-PAID-LAST        PIC 9(8).
           03  WS-LN-ORGTERM               PIC Z(5).
           03  WS-NO-TRUE-DUE-DATE         PIC 9(8).
           03  WS-NO-TDUEAMT               PIC Z(8).99.
           03  WS-LN-PAST-DUE-AMT          PIC Z(8).99.
           03  WS-CUSTOM                   PIC X.
           03  WS-AVAIL-CREDIT-FG          PIC X.
           03  WS-TDUE-PAYOFF              PIC Z(8).99.
           03  WS-MAX-PAY-AMT              PIC Z(8).99.
           03  WS-1STPYDATE                PIC 9(8).
           03  WS-NO-NXDUE-DATE            PIC 9(8).
           03  WS-INT-PORTION-OF-PAY       PIC Z(8).99.
           03  WS-NO-PAST-DUE-DAYS         PIC Z(5).
           03  WS-NO-TDUELC                PIC Z(8).99.
           03  WS-BW-CO-FNAME              PIC X(37).
           03  WS-BW-CO-LNAME              PIC X(37).
           03  WS-IS-RESTRICTED            PIC X.
           03  WS-BRANCH-STATE             PIC XX.
           03  WS-REPAY-F2D-AMOUNT         PIC Z(8).99.
           03  WS-REPAY-IS-FUNDABLE        PIC X.
           03  WS-REPAY-NO-NXDUEAMT        PIC Z(8).99.
           03  WS-REPAY-CLASS                    PIC 99.


       01  WS-LP-REC.
           03  WS-LP-BRACCTNO                PIC Z(12).
           03  WS-LP-OWNBR                   PIC Z(6).
           03  WS-LP-NUMBER                  PIC Z(8).
           03  WS-LP-SEQNO                   PIC Z(6).
           03  WS-LP-PAYDATE                 PIC 9(8).
           03  WS-LP-TRAMT                   PIC Z(8).99.
           03  WS-LP-TRCD                    PIC X(2).
           03  WS-LP-REFNO                   PIC X(5).

       01  WK-LN-IFN        PIC X(4)     VALUE "WKLN".
       01  WK-LP-IFN        PIC X(4)     VALUE "WKLP".

       01  JUSTIFY-TYPE     PIC X VALUE "L".
       01  NXDUE-REBATE     PIC S9(7)V99 COMP-3.
       01  PRIOR-REBATE     PIC S9(7)V99 COMP-3.
       01  PER-DIEM-RATE    PIC 999V999.

       01  TODAYS-DATE      PIC 9(8).
 
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).

       01  ZIP-WORK             PIC X(10).
       01  X REDEFINES ZIP-WORK.
           03  FILLER           PIC X(6).
           03  PART-2           PIC X(4).
       01  X1 REDEFINES ZIP-WORK.
           03  FILLER           PIC X(5).
           03  PART-2-A         PIC X(4).
           03  FILLER           PIC X.
       01  X3 REDEFINES ZIP-WORK.
           03  PART-3-ZIP       PIC X(5).
           03  PART-3-DASH      PIC X(1).
           03  PART-3-EXT       PIC X(4).

       01  CR-1          PIC X VALUE X"0D".
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  CUSTOM-FLAG     PIC X.

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           " ---- REPAY DATA EXTRACTION ----".

       01  HEAD3.
           03  FILLER           PIC X(9)    VALUE "BRANCH NO".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "TOTAL LN AMT".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "TOTAL FILES".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  RANGE1D REDEFINES RANGE1 PIC ZZ99/99/99.
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).
           03  RANGE2D REDEFINES RANGE2 PIC ZZ99/99/99.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/REPAYE_DEF.CPY".
       COPY "LIBSP/REPAYE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".

       COPY "LIBLP/LPCRNOW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/REPAYE_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-LP-FILE WK-LN-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
      *     PERFORM CLOSE-LTB1-FILE.
      *     PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LP1-CONNECTION.
           CLOSE
               PR-FILE WK-LP-FILE WK-LN-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "REPAYE" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
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
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO LN-PATH-OVERRIDE
                              LTD-PATH-OVERRIDE
                              LTB-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LP-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
      *     PERFORM OPEN-LTB1-FILE.
      *     PERFORM OPEN-LTD1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LP1-FILE.

      D    STOP MESS.
           MOVE 0 TO TOT-BRANCH-LN-RECS.
           PERFORM PROCESS-LN.

           IF TOT-BRANCH-LN-RECS > 0
              PERFORM PROCESS-LP.

           PERFORM CLOSE-LN3-FILE.
      *     PERFORM CLOSE-LTB1-FILE.
      *     PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LP1-FILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      **************************
       PROCESS-LN SECTION.
      **************************
           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST   TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
      *     MOVE 0         TO LN-ACCTNO
      *                       QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.  *> SSP LN BW(2), LTB, LTD

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.  *> SSP LN BW(2), LTB, LTD
           IF IO-BAD                    *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-PROCESS-LN.

           ADD 1 TO TOTAL-RECORDS.

           IF LN-OWNBR > END-BR
              GO TO EXIT-PROCESS-LN.

      * ALSO CHECKED IN SSP
           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      * ALSO CHECKED IN SSP
           IF LN-POCD-NONLOAN
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      * EXCLUDE ZERO BALANCES
      * ALSO CHECKED IN SSP
           IF LN-CURBAL = 0 AND LN-INTBAL = 0 AND LN-LCBAL = 0
               AND LN-OTHBAL = 0
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.
           
      *CHECK FOR NO PAYOFF DATE ON PL.
      * ALSO CHECKED IN SSP
           IF LN-PLDATE NOT = ZEROS
              IF LN-POFFDATE NOT = 0
                 ADD 1 TO TOTAL-BYPASSED
                 GO TO NEXT-LN.

      * ALSO CHECKED IN SSP
           IF LN-DLPOOL NOT < 80000
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * ALSO CHECKED IN SSP
           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      * EXCLUDE PL < 01/01/2010
      * ALSO CHECKED IN SSP
           IF LN-PLDATE NOT = ZEROS
              IF LN-PLDATE NOT > 20100101
                 ADD 1 TO  TOTAL-BYPASSED
                 GO TO NEXT-LN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 ADD 1 TO TOTAL-BYPASSED
                 GO TO NEXT-LN.

           PERFORM CREATE-LN-WORK.
      *     PERFORM PROCESS-LP.

           GO TO NEXT-LN.

       EXIT-PROCESS-LN.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE REPAYE-QUEUE-POS TO Q-POS.
           MOVE REPAYE-COPIES-POS TO C-POS.
           MOVE REPAYE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           MOVE 0 TO TOTAL-RECORDS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD  TO TODAYS-DATE.


           MOVE EXT-FILPATH-MACHINE TO WK-LN-MACHINE
                                       WK-LP-MACHINE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-LN-PATH-DA
                                  WK-LP-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-LN-PATH-MO
                                  WK-LP-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-LN-PATH-YR
                                  WK-LP-PATH-YR.

           CALL "C$TOLOWER" USING WK-LN-PATH1,VALUE 12.
           PERFORM OPEN-WK-LN-FILE-OUTPUT.

           CALL "C$TOLOWER" USING WK-LP-PATH1,VALUE 12.
           PERFORM OPEN-WK-LP-FILE-OUTPUT.

           PERFORM ARRAYSP-LOAD.

           MOVE BEG-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-BEG-LN-DATE.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-END-LN-DATE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.


      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

       BR-01.
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
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
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
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO BEGDATE-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-CUSTOM-FLAG.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              IF BEG-DATE > END-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO ENTER-CUSTOM-FLAG.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-CUSTOM-FLAG.
           MOVE "E  A010CUSTOM." TO VDU.
           PERFORM SCREENIO.
           
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENDDATE-02 
              ELSE
                 GO TO CL-02.
           MOVE VDUBUF TO CUSTOM-FLAG.
           IF NOT (CUSTOM-FLAG = "Y" OR "N")
               GO TO ENTER-CUSTOM-FLAG.
       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010CUSTOM." TO VDU.
           MOVE CUSTOM-FLAG TO VDUBUF.
           PERFORM SCREENIO.

      ****************************
       CREATE-LN-WORK SECTION.
      ****************************

      *     MOVE LN-ACCTNO          TO CRNOR2-ACCTNO.
      *     MOVE TODAYS-DATE        TO CRNOR2-SYSDATE.
      *     MOVE "N"                TO CRNOR2-LPREC-FG.
      *     MOVE "CL/CRNOR2" TO FORM-NAM.
      *     CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK
      *                           NO-REC POFF-WORKERS
      *                           LN-REC.
      *     CANCEL FORM-PROGX.

           MOVE LN-MAKERCD(1) TO CRNO-MAKERCD.
           MOVE TODAYS-DATE   TO CRNO-PAYDATE.
           MOVE "N"           TO CRNO-LPREC-FG.
           PERFORM CREATE-NOREC.

      * SSP WILL SKIP ANY LN ROW IF BW NOT FOUND
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO CREATE-LN-WORK-EXIT.

           MOVE SPACES TO WK-LN-REC WS-LN-REC.

           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           CALL "C$JUSTIFY" USING WS-BRACCT, JUSTIFY-TYPE.
           
           MOVE WS-BRACCT TO WS-LN-BRACCTNO.

           MOVE LN-ACCTNO  TO WS-LN-NUMBER.
           CALL "C$JUSTIFY" USING WS-LN-NUMBER, JUSTIFY-TYPE.           
           MOVE LN-OWNBR   TO WS-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LN-OWNBR, JUSTIFY-TYPE.           

           IF LN-CURBAL > NO-TDUEPOFF
              MOVE LN-CURBAL TO WS-LN-BAL-OR-PO
           ELSE
              MOVE NO-TDUEPOFF TO WS-LN-BAL-OR-PO.
           CALL "C$JUSTIFY" USING WS-LN-BAL-OR-PO, JUSTIFY-TYPE.           
  
           MOVE LN-1STPYAMT   TO WS-LN-1STPYAMT.
           CALL "C$JUSTIFY" USING WS-LN-1STPYAMT, JUSTIFY-TYPE.           

           MOVE LN-REGPYAMT   TO WS-LN-REGPYAMT.
           CALL "C$JUSTIFY" USING WS-LN-REGPYAMT, JUSTIFY-TYPE.           

           MOVE LN-AMT-PAID-LAST   TO WS-LN-AMT-PAID-LAST.
           CALL "C$JUSTIFY" USING WS-LN-AMT-PAID-LAST, JUSTIFY-TYPE.           

           MOVE LN-DATE-PAID-LAST  TO WS-LN-DATE-PAID-LAST. 

           MOVE LN-ORGTERM         TO WS-LN-ORGTERM.
           CALL "C$JUSTIFY" USING WS-LN-ORGTERM, JUSTIFY-TYPE.           

           MOVE LN-LOANDATE TO WS-LN-LOANDATE.

           MOVE LN-LNAMT        TO WS-LN-LNAMT.
           CALL "C$JUSTIFY" USING WS-LN-LNAMT, JUSTIFY-TYPE.           

           MOVE NO-TRUE-DUE-DATE TO WS-NO-TRUE-DUE-DATE.
           
           MOVE NO-TDUEAMT     TO WS-NO-TDUEAMT.
           CALL "C$JUSTIFY" USING WS-NO-TDUEAMT, JUSTIFY-TYPE.           
           
           IF NO-TDUEAMT > LN-REGPYAMT
              COMPUTE WS-LN-PAST-DUE-AMT = NO-TDUEAMT - LN-REGPYAMT
           ELSE
              MOVE 0 TO WS-LN-PAST-DUE-AMT.
           CALL "C$JUSTIFY" USING WS-LN-PAST-DUE-AMT, JUSTIFY-TYPE.

           MOVE NO-TDUEPOFF TO WS-TDUE-PAYOFF.
           CALL "C$JUSTIFY" USING WS-TDUE-PAYOFF, JUSTIFY-TYPE.

      * MAXPAYMENTAMT = (NO-TDUEPOFF) -
      *                 (MINIMUM PAY AMT + OVERPAY-BUFFER)
      * MINIMUM = 25.00
      * OVERPAY BUFFER = 1.00

           IF NO-TDUEPOFF < 26
              MOVE 0 TO WS-MAX-PAY-AMT
           ELSE
              COMPUTE WS-MAX-PAY-AMT = NO-TDUEPOFF - 26.00.
           CALL "C$JUSTIFY" USING WS-MAX-PAY-AMT, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE TO WS-1STPYDATE.

           MOVE NO-NXDUE-DATE TO WS-NO-NXDUE-DATE.

           MOVE NO-TDUELC     TO WS-NO-TDUELC.
           CALL "C$JUSTIFY" USING WS-NO-TDUELC, JUSTIFY-TYPE.           

           MOVE NO-PAST-DUE-DAYS TO WS-NO-PAST-DUE-DAYS.
           CALL "C$JUSTIFY" USING WS-NO-PAST-DUE-DAYS, JUSTIFY-TYPE.           

      * THE CFPB HAS DECIDED TO MOVE FORWARD WITH A REQUIREMENT THAT ANY
      * SCHEDULED ELECTRONIC PAYMENT NEEDS TO HAVE THE FIRST PAYMENT
      * SCHEDULED ITEMIZED WITH PAYMENT AMOUNT, INTEREST AMOUNT, AND
      * PRINCIPAL AMOUNT. ADD FIELDS FOR NEXT SCHEDULED DUE DATE,
      * CALCULATED INTERST PORTION, AND UPDATED 1ST PAYMENT DUE DATE.

      *    ADD UPDATED FIRST PAYMENT DATE, NEXT SCHEDULED DUE DATE, AND
      *    INTEREST PORTION OF PAYMENT. FOR PRECOMPUTED STATUS, CALCULATE
      *    AN INTEREST REBATE AS OF NO-NXDUE, THEN CALCULATE AN INTEREST
      *    REBATE AS OF ONE PAY PERIOD PRIOR (1 MONTH USING SAME DAY OF
      *    MONTH) AND THE DIFFERENCE IS THE AMOUNT TO SEND IN INTEREST
      *    PORTION OF PAYMENT. FOR ACCOUNTS IN SIMPLE INTEREST STATUS
      *    (ACCELERATED OR PAST MATURITY) CALCULATE A PER-DIEM BASED ON THE
      *    CURRENT BALANCE AND RATE USING 365 YRTPE AND MULITPLY BY 30 DAYS

           MOVE TODAYS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           IF NOT ( IBPC-FG = "P" AND IBPC-EXPIRED-FLAG = " " )
              PERFORM IB-REBATE
           ELSE
              PERFORM PRE-COMPUTE-REBATE.

           MOVE LN-SSNO(1) TO WS-LN-SSNO1.

           MOVE BW-FNAME TO WS-BW-FNAME.
           MOVE BW-LNAME TO WS-BW-LNAME.
           MOVE BW-ADR1 TO  WS-BW-ADR1.
           MOVE BW-ADR2 TO  WS-BW-ADR2.
           MOVE BW-CITY TO  WS-BW-CITY.
           MOVE BW-STATE TO WS-BW-STATE.
           MOVE BW-ZIP TO ZIP-WORK.
      * IF ZIP IS IN XXXXX-XXXX FORMAT THEN SHIFT EXT TO PUT INTO
      *    XXXXXXXXX FORMAT
           IF PART-3-DASH = "-"
              MOVE PART-2 TO PART-2-A
              MOVE SPACE TO JUNK.
           CALL "C$JUSTIFY" USING ZIP-WORK, JUSTIFY-TYPE.
           MOVE ZIP-WORK TO WS-BW-ZIP.

      * IF BW-DATE = ZEROS (1900-01-01 IN SQL) 
      *  NEED TO MOVE 20000000 TO WS-BW-AGE TO MATCH
      *  A15.
      * NOTE: WS-BW-AGE IS ACTUALLY BIRTH DATE ***
           IF BW-BDATE = ZEROS
              MOVE 20000000 TO WS-BW-AGE
           ELSE
              MOVE BW-BDATE TO WS-BW-AGE.

           MOVE BW-PHONE        TO WS-BW-PHONE.
           MOVE BW-CELL-PHONE   TO WS-BW-CELL-PHONE.
           IF WS-BW-CELL-PHONE = 2020202020 OR
               WS-BW-CELL-PHONE = 0202020202
                 MOVE ZEROS TO WS-BW-CELL-PHONE.
           IF NO-AVAILCREDIT > 0
              MOVE "Y" TO WS-AVAIL-CREDIT-FG 
           ELSE 
              MOVE "N" TO WS-AVAIL-CREDIT-FG.

      * WHEN THE CUSTOM-FLAG IS "Y", ALWAYS SET THE ACCEPT PAYMENT INDICATOR
      * (CUSTOM) TO "Y" FOR ALL LOANS, EVEN P&L.
      * OTHERWISE, THE ACCEPT PAYMENT INDICATOR WILL REMAIN "N" FOR LOANS
      * WITH A P&L DATE.
           IF CUSTOM-FLAG = "Y"
              MOVE "Y" TO WS-CUSTOM
           ELSE
              IF LN-PLDATE = ZEROS 
                 MOVE "Y" TO WS-CUSTOM 
              ELSE
                 MOVE "N" TO WS-CUSTOM.

           IF LN-SSNO(2) NOT = 0
      *        MOVE LN-SSNO(2) TO BW-SSNO
      *        PERFORM READ-BW1-FILE
      *        IF IO-FG = 0
      *           MOVE BW-FNAME TO WS-BW-CO-FNAME
      *           MOVE BW-LNAME TO WS-BW-CO-LNAME.
              MOVE RETR-BW2-FNAME TO WS-BW-CO-FNAME
              MOVE RETR-BW2-LNAME TO WS-BW-CO-LNAME.


      * IS RESTRICTED   ** IF BOTH LEGAL AND BANKRUPT, SET AS "B"
      * B - IN BANKRUPTCY STATUS
      * L - IN LEGAL STATUS

           MOVE "N" TO WS-IS-RESTRICTED.

           IF LN-LEGALDATE NOT = 0
              MOVE "L" TO WS-IS-RESTRICTED.

           IF LN-PLDATE NOT = 0
              IF LN-TRW-SPECIAL-CODE = "AU"
                 MOVE "B" TO WS-IS-RESTRICTED.

      * LTB FIELDS RETRIEVED IN SSP
           IF LN-BNKRPTDATE NOT = 0
      *        MOVE LN-OWNBR TO LTB-BRNO   
      *        MOVE LN-ACCTNO TO LTB-ACCTNO 
      *        MOVE 1         TO LTB-SEQNO
      *        PERFORM READ-LTB1-FILE
      *        IF IO-BAD
               IF LTB-BRNO = 0    *> WILL = 0 IF ROW NOT FOUND IN SSP
                 MOVE "B" TO WS-IS-RESTRICTED
              ELSE
                 IF (LTB-REAFF-DATE NOT = 0 AND LTB-RESCIND-DATE = 0)
                    MOVE "N" TO WS-IS-RESTRICTED
                 ELSE
                    IF (LTB-REAFF-DATE = 0) OR
                        (LTB-RESCIND-DATE NOT = 0) OR
                          (LTB-DISCHARGE-DATE NOT = 0)
                             MOVE "B" TO WS-IS-RESTRICTED.
              
           MOVE BR-STATE TO WS-BRANCH-STATE.

      * FUND TO DEBIT
      * LTD FIELD RETRIEVED IN SSP
      *     MOVE LN-ACCTNO TO LTD-ACCTNO.
      *     MOVE 1         TO LTD-SEQNO.
      *     PERFORM READ-LTD1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE 0 TO LTD-DEBIT-CARD-AMT.

           MOVE LTD-DEBIT-CARD-AMT TO WS-REPAY-F2D-AMOUNT.
           CALL "C$JUSTIFY" USING WS-REPAY-F2D-AMOUNT, JUSTIFY-TYPE.

           MOVE "0" TO WS-REPAY-IS-FUNDABLE.

           MOVE NO-NXDUEAMT TO WS-REPAY-NO-NXDUEAMT.
           CALL "C$JUSTIFY" USING WS-REPAY-NO-NXDUEAMT, JUSTIFY-TYPE.

           MOVE LN-CLASS    TO WS-REPAY-CLASS.

           STRING
              WS-LN-BRACCTNO         ,"|"
              WS-LN-NUMBER           ,"|"
              WS-LN-OWNBR            ,"|"
              WS-LN-SSNO1            ,"|"
              WS-BW-FNAME            ,"|" 
              WS-BW-LNAME            ,"|"
              WS-BW-ADR1             ,"|"
              WS-BW-ADR2             ,"|"
              WS-BW-CITY             ,"|"
              WS-BW-STATE            ,"|"
              WS-BW-ZIP              ,"|"
              WS-BW-AGE              ,"|"
              WS-BW-PHONE            ,"|"
              WS-BW-CELL-PHONE       ,"|"
              WS-LN-LOANDATE         ,"|"
              WS-LN-LNAMT            ,"|"
              WS-LN-BAL-OR-PO        ,"|"
              WS-LN-1STPYAMT         ,"|"
              WS-LN-REGPYAMT         ,"|"
              WS-LN-AMT-PAID-LAST    ,"|"
              WS-LN-DATE-PAID-LAST   ,"|"
              WS-LN-ORGTERM          ,"|"
              WS-NO-TRUE-DUE-DATE    ,"|"
              WS-NO-TDUEAMT          ,"|"
              WS-LN-PAST-DUE-AMT     ,"|"
              WS-CUSTOM              ,"|"
              WS-AVAIL-CREDIT-FG     ,"|"
              WS-TDUE-PAYOFF         ,"|"
              WS-MAX-PAY-AMT         ,"|"
              WS-1STPYDATE           ,"|"
              WS-NO-NXDUE-DATE       ,"|"
              WS-INT-PORTION-OF-PAY  ,"|"
              WS-NO-PAST-DUE-DAYS    ,"|"
              WS-NO-TDUELC           ,"|"
              WS-BW-CO-FNAME         ,"|"
              WS-BW-CO-LNAME         ,"|"
              WS-IS-RESTRICTED       ,"|"
              WS-BRANCH-STATE        ,"|"
              WS-REPAY-F2D-AMOUNT    ,"|"
              WS-REPAY-IS-FUNDABLE   ,"|"
              WS-REPAY-NO-NXDUEAMT         ,"|"
              WS-REPAY-CLASS,CR-1
              DELIMITED BY "  " INTO WK-LN-REC.

           PERFORM WRITE-WK-LN-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "N" TO FIRST-TIME.
           ADD 1 TO TOTAL-INCLUDED TOT-BRANCH-LN-RECS.

       CREATE-LN-WORK-EXIT.
           EXIT.

      ****************************
       PROCESS-LP SECTION.
      ****************************
      *    MOVE LP-PATH-OWNBR TO LP-BRNO
      *                          QLP1-WBEG-BRNO
      *                          QLP1-WEND-BRNO.
      *    MOVE LN-ACCTNO     TO LP-ACCTNO
      *                          QLP1-WBEG-ACCTNO
      *                          QLP1-WEND-ACCTNO.
      *    MOVE 0             TO LP-SEQNO
      *                          QLP1-WBEG-SEQNO.
      *    MOVE ALL "9"       TO QLP1-WEND-SEQNO.

      * LP CURSOR DRIVEN FROM LNFILE (USES LNFILES PARMS)
           PERFORM START-LP1-FILE.  *> SSP - LP 

       NEXT-LP.
           PERFORM READ-LP1-FILE-NEXT.  *> SSP - LP 
           IF IO-BAD
              GO TO PROCESS-LP-EXIT.

      *     IF (LP-ACCTNO NOT = LN-ACCTNO) OR
      *          (LP-BRNO NOT = LP-PATH-OWNBR)
      *        GO TO PROCESS-LP-EXIT.

      * ALSO CHECKED IN SSP.
           IF NOT (LP-TRCD = "PY" OR "PA" OR "RV")
              GO TO NEXT-LP.

      * ALSO CHECKED IN SSP.
           IF LP-TRCD = "RV" 
              IF  LP-REVTRCD NOT = "PA"
                  AND LP-REVTRCD NOT = "PY"
                    GO TO NEXT-LP.


           MOVE SPACES    TO WK-LP-REC WS-LP-REC.
           MOVE BR-NO     TO WS-OWNBR.
           MOVE LP-ACCTNO TO WS-NUMBER.
           CALL "C$JUSTIFY" USING WS-BRACCT,JUSTIFY-TYPE.

           MOVE WS-BRACCT TO WS-LP-BRACCTNO.


           MOVE BR-NO    TO WS-LP-OWNBR.
           CALL "C$JUSTIFY" USING WS-LP-OWNBR, JUSTIFY-TYPE.

           MOVE LP-ACCTNO   TO WS-LP-NUMBER.
           CALL "C$JUSTIFY" USING WS-LP-NUMBER,JUSTIFY-TYPE.

           MOVE LP-SEQNO    TO WS-LP-SEQNO.
           CALL "C$JUSTIFY" USING WS-LP-SEQNO,JUSTIFY-TYPE.

           MOVE LP-PAYDATE  TO WS-LP-PAYDATE. 

           MOVE LP-TRAMT    TO WS-LP-TRAMT.
           CALL "C$JUSTIFY" USING WS-LP-TRAMT,JUSTIFY-TYPE.

           MOVE LP-TRCD     TO WS-LP-TRCD.
           MOVE LP-REFNO    TO WS-LP-REFNO.

           STRING
              WS-LP-BRACCTNO         ,"|"
              WS-LP-OWNBR            ,"|"
              WS-LP-NUMBER           ,"|"
              WS-LP-SEQNO            ,"|"
              WS-LP-PAYDATE          ,"|"
              WS-LP-TRAMT            ,"|"
              WS-LP-TRCD             ,"|"
              WS-LP-REFNO,CR-1             
              DELIMITED BY "  " INTO WK-LP-REC.
              PERFORM WRITE-WK-LP-FILE.

              IF IO-FG NOT = 0
                 GO TO IO-ERROR.
           
           GO TO NEXT-LP.

       PROCESS-LP-EXIT.
           EXIT.

      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.


           MOVE "BEG CLASS - END CLASS" TO RANGE-SELECTION.
           MOVE BEG-CLASS TO RANGE1N.
           MOVE END-CLASS TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE - END DATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
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

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ********************************************************
       IB-REBATE SECTION.
      ********************************************************

           MOVE "PY"             TO RATE-LPTRCD.
           MOVE TODAYS-DATE      TO RATE-DATE IBPC-DATE.
      *    MOVE LN-1STPYDATE     TO RATE-DATE IBPC-DATE.
           MOVE LN-MAKERCD(1)    TO RATE-MAKERCD.
           PERFORM SET-PY-RATE
           COMPUTE PER-DIEM-RATE ROUNDED = INDU-WORKER / INDU-WK
           COMPUTE WS-INT-PORTION-OF-PAY = PER-DIEM-RATE * 30.
           CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE.

      ********************************************************
       PRE-COMPUTE-REBATE SECTION.
      ********************************************************

           MOVE 0 TO REB-REBATE NXDUE-REBATE PRIOR-REBATE.

      * REBATE ALREADY GIVEN OR INTEREST CANCELLED
           IF NOT (LN-INT-REBATE = " " OR "C")

              MOVE 0 TO WS-INT-PORTION-OF-PAY

              CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE
              GO TO EXIT-PRE-COMPUTE-REBATE.

      * CALCULATE AN INTEREST REBATE AS OF NO-NXDUE

           MOVE NO-NXDUE-DATE TO REB-PAYDATE
           MOVE LN-ORGTERM    TO REB-ORGTERM
           MOVE 7             TO REB-SUB
           MOVE "RI"          TO REB-LPTRCD
           MOVE LN-INTCHG     TO REB-TOTCHG
           PERFORM REBATE.
           MOVE REB-REBATE TO NXDUE-REBATE.

      * FOR ACCOUNTS THAT HAVE NOT MADE THEIR FIRST PAYMENT (NO-NXDUE =
      * THE FIRST PAYMENT DUE DATE, USE THE ORIGINAL INTEREST + EXTENSION
      * CHARGE (RATHER THAN TRYING TO CALC A REBATE) AND TAKE THE DIFFERENCE
      * BETWEEN THAT AND AN RI ON THE FIRST DUE (NO-NXDUE)
           IF NO-NXDUE-DATE = LN-1STPYDATE
              COMPUTE WS-INT-PORTION-OF-PAY =
                  (LN-INTCHG + LN-EXTCHG ) - NXDUE-REBATE
              CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE
              GO TO EXIT-PRE-COMPUTE-REBATE.

      * NOW CALCULATE AN INTEREST REBATE AS OF ONE PAY PERIOD PRIOR
      * (1 MONTH USING SAME DAY OF MONTH)
           MOVE -1            TO NDTE-HOLD.
           MOVE NO-NXDUE-DATE TO NDTE-DATE.
           PERFORM INCREMENT-MONTHS.

           MOVE NDTE-DATE  TO REB-PAYDATE
           MOVE LN-ORGTERM TO REB-ORGTERM
           MOVE 7          TO REB-SUB
           MOVE "RI"       TO REB-LPTRCD
           MOVE LN-INTCHG  TO REB-TOTCHG
           PERFORM REBATE.
           MOVE REB-REBATE TO PRIOR-REBATE.

           COMPUTE WS-INT-PORTION-OF-PAY = NXDUE-REBATE - PRIOR-REBATE.
           CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE.

       EXIT-PRE-COMPUTE-REBATE.
           EXIT.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".

       COPY "LIBLP/LPCRNO.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/LPTDEL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO REPAYE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO REPAYE-SYSDATE.
           DISPLAY REPAYE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE REPAYE-FORM-FIELDS TO WR-BUF.
           MOVE REPAYE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE REPAYE-HELP--FILE TO HELP-LINK-FILE.
           MOVE REPAYE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/REPAYE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      **********************
       IO-ROUTINE SECTION.
      **********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLTD1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

           IF LP1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LP1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LP1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LP1-FILE.    *> SSP - LP  LN

           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

      *   SELECT LPFILE.LP_BRNO,
      *          LPFILE.LP_ACCTNO,
      *          LPFILE.LP_SEQNO,
      *          CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)), 
      *          LPFILE.LP_TRCD, 
      *          LPFILE.LP_REFNO, 
      *          LPFILE.LP_TRAMT,
      *          LPFILE.LP_REVTRCD
      * FROM DBO.LNFILE LNFILE
      * INNER JOIN DBO.LPFILE LPFILE
      *   ON LPFILE.LP_BRNO   = LNFILE.LN_OWNBR 
      *  AND LPFILE.LP_ACCTNO = LNFILE.LN_ACCTNO
      * WHERE LNFILE.LN_OWNBR    = @QLN3_WBEG_OWNBR
      *   AND LNFILE.LN_CLASS    BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *   AND LNFILE.LN_POCD     NOT IN ('**', '*I', '*R', '*U')
      *   AND LNFILE.LN_LOANDATE BETWEEN @BEG_LNDATE AND @END_LNDATE
      *   AND LNFILE.LN_DLPOOL < 80000
      *   AND (LNFILE.LN_CURBAL > 0 OR LNFILE.LN_INTBAL > 0 OR LNFILE.LN_LCBAL > 0 OR LNFILE.LN_OTHBAL > 0)
      *   AND (LNFILE.LN_PLDATE = '1900-01-01' OR LNFILE.LN_PLDATE > '2010-01-01')
      *   AND (LNFILE.LN_PLDATE = '1900-01-01' OR LNFILE.LN_POFFDATE = '1900-01-01')
      * ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO, LPFILE.LP_SEQNO


           EXEC SQL
            DECLARE LP1_CURSOR CURSOR FOR
                   :LP1-RETURN-CODE = 
            EXEC SSP_REPAYE_LPFILE_SELECT
                (:QLN3-WBEG-OWNBR,:QLN3-WBEG-CLASS,
                 :QLN3-WEND-CLASS,:PASS-BEG-LN-DATE,:PASS-END-LN-DATE)
            END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO LP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LP1-FILE-NEXT.    *> SSP - LP 
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH LP1_CURSOR INTO
                 :QLP-BRNO,
                 :QLP-ACCTNO,
                 :QLP-SEQNO,
                 :QLP-PAYDATE,
                 :QLP-TRCD, 
                 :QLP-REFNO, 
                 :QLP-TRAMT,
                 :QLP-REVTRCD
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE LP-ACCTNO            TO QLP1-WEND-ACCTNO
              MOVE ALL "0"              TO QLP1-WBEG-SEQNO
              MOVE LP-SEQNO             TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR_NG CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_USERID,
                   CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),
                   LPFILE.LP_POSTTIME,
                   CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),
                   CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),
                   LPFILE.LP_TRCD,
                   LPFILE.LP_REFNO,
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   LPFILE.LP_IBPC,
                   LPFILE.LP_MANUAL_RB,
                   LPFILE.LP_INSTYPES,
                   LPFILE.LP_REV,
                   LPFILE.LP_REVTRCD,
                   LPFILE.LP_REVSEQNO,
                   LPFILE.LP_STATUSFG,
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),
                   CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_LCPAID,
                   CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   LPFILE.LP_COLLECTOR,
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   LPFILE.LP_ALT_PREPAY_FG,
                   LPFILE.LP_REPAY_TRANS_ID
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO    = :QLP1-WEND-BRNO
              AND LPFILE.LP_ACCTNO  = :QLP1-WEND-ACCTNO
              AND LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO 
            ORDER BY LPFILE.LP_BRNO DESC,
                     LPFILE.LP_ACCTNO DESC,
                     LPFILE.LP_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_NG INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LP1_CURSOR 
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LP1-CONNECTION.

           IF LP1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LP1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LP1-CONN-OPEN-SW.

      *COPY "LIBLP/LPLN3RN.CPY".
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
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.  *> SSP LN BW(2), LTB, LTD
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
              :LN3-RETURN-CODE = 
               EXEC SSP_REPAYE_LNFILE_SELECT
                (:QLN3-WBEG-OWNBR,:QLN3-WBEG-CLASS,
                 :QLN3-WEND-CLASS,:PASS-BEG-LN-DATE,:PASS-END-LN-DATE)
               END-EXEC.

      *  SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_ORGBR,
      *            LNFILE.LN_OWNST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_MIDSTREAM,
      *            LNFILE.LN_PURCD,
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_COLLCD,
      *            LNFILE.LN_COLLID,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            LNFILE.LN_PLTYPE,
      *            LNFILE.LN_CONFIDENTCD,
      *            LNFILE.LN_ALLOTCD,
      *            LNFILE.LN_RECASTCD,
      *            LNFILE.LN_SKIP_TRW,
      *            LNFILE.LN_STATUSFG,
      *            LNFILE.LN_SMALLOAN,
      *            LNFILE.LN_SALEFIN,
      *            LNFILE.LN_SERVACCT,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_ADDON_FG,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            LNFILE.LN_LC_CODE,
      *            LNFILE.LN_CAN_LOAN_NO,
      *            LNFILE.LN_STATFLAGS,
      *            CAST(LNFILE.LN_ENTDATE            AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LOANDATE           AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PURDATE            AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTDATE            AS VARCHAR(10)),
      *            CAST(LNFILE.LN_CASHDATE           AS VARCHAR(10)),
      *            CAST(LNFILE.LN_FILEDATE           AS VARCHAR(10)),
      *            CAST(LNFILE.LN_RESCISSION_DATE    AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PART_RECOURSE_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE     AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE          AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE             AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE             AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PL_TRDATE          AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ACCELDATE          AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDUDATE           AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE           AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PO_TRDATE          AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LEGALDATE          AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_DISMISS_DATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DEBTAIDATE            AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REPODATE              AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDEEMDATE            AS VARCHAR(10)),
      *            CAST(LNFILE.LN_SOLDDATE              AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INS_CONFIRM_DATE      AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_RN_FOR10_DATE    AS VARCHAR(10)),
      *            CAST(LNFILE.LN_TRW_DOFD_DATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_PYPA_DATE        AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BT_TRDATE             AS VARCHAR(10)),
      *            CAST(LNFILE.LN_METRO2_LAST_PMT_DATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_FOLLOWUP_DATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_ACCRUAL_DATE     AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_YLDRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_ENDORATE,
      *            LNFILE.LN_JDRATE,
      *            LNFILE.LN_ACCRATE,
      *            LNFILE.LN_REDURATE,
      *            LNFILE.LN_CONRATE,
      *            LNFILE.LN_SERVPER,
      *            LNFILE.LN_SELLPR,
      *            LNFILE.LN_DOWNPY,
      *            LNFILE.LN_TRADEIN,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_FINCHG,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_POINTS,
      *            LNFILE.LN_PROCEEDS,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_PROCASH,
      *            LNFILE.LN_TILLNO,
      *            LNFILE.LN_STATE_FLAG,
      *            LNFILE.LN_STATE_WORKER,
      *            LNFILE.LN_STATE_WORKER2,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSELIGIBLE,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSOURS_1,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOMM_1,
      *            LNFILE.LN_INSANTICERN_1,
      *            LNFILE.LN_INSANTICADJ_1,
      *            LNFILE.LN_INSUFERN_1,
      *            LNFILE.LN_INSACCERN_1,
      *            LNFILE.LN_INSEARN_ORDER_1,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSOURS_2,
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOMM_2,
      *            LNFILE.LN_INSANTICERN_2,
      *            LNFILE.LN_INSANTICADJ_2,
      *            LNFILE.LN_INSUFERN_2,
      *            LNFILE.LN_INSACCERN_2,
      *            LNFILE.LN_INSEARN_ORDER_2,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSOURS_3,
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOMM_3,
      *            LNFILE.LN_INSANTICERN_3,
      *            LNFILE.LN_INSANTICADJ_3,
      *            LNFILE.LN_INSUFERN_3,
      *            LNFILE.LN_INSACCERN_3,
      *            LNFILE.LN_INSEARN_ORDER_3,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSOURS_4,
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOMM_4,
      *            LNFILE.LN_INSANTICERN_4,
      *            LNFILE.LN_INSANTICADJ_4,
      *            LNFILE.LN_INSUFERN_4,
      *            LNFILE.LN_INSACCERN_4,
      *            LNFILE.LN_INSEARN_ORDER_4,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSOURS_5,
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOMM_5,
      *            LNFILE.LN_INSANTICERN_5,
      *            LNFILE.LN_INSANTICADJ_5,
      *            LNFILE.LN_INSUFERN_5,
      *            LNFILE.LN_INSACCERN_5,
      *            LNFILE.LN_INSEARN_ORDER_5,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSOURS_6,
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOMM_6,
      *            LNFILE.LN_INSANTICERN_6,
      *            LNFILE.LN_INSANTICADJ_6,
      *            LNFILE.LN_INSUFERN_6,
      *            LNFILE.LN_INSACCERN_6,
      *            LNFILE.LN_INSEARN_ORDER_6,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSOURS_7,
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOMM_7,
      *            LNFILE.LN_INSANTICERN_7,
      *            LNFILE.LN_INSANTICADJ_7,
      *            LNFILE.LN_INSUFERN_7,
      *            LNFILE.LN_INSACCERN_7,
      *            LNFILE.LN_INSEARN_ORDER_7,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSOURS_8,
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOMM_8,
      *            LNFILE.LN_INSANTICERN_8,
      *            LNFILE.LN_INSANTICADJ_8,
      *            LNFILE.LN_INSUFERN_8,
      *            LNFILE.LN_INSACCERN_8,
      *            LNFILE.LN_INSEARN_ORDER_8,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSOURS_9,
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOMM_9,
      *            LNFILE.LN_INSANTICERN_9,
      *            LNFILE.LN_INSANTICADJ_9,
      *            LNFILE.LN_INSUFERN_9,
      *            LNFILE.LN_INSACCERN_9,
      *            LNFILE.LN_INSEARN_ORDER_9,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSCOMM_10,
      *            LNFILE.LN_INSANTICERN_10,
      *            LNFILE.LN_INSANTICADJ_10,
      *            LNFILE.LN_INSUFERN_10,
      *            LNFILE.LN_INSACCERN_10,
      *            LNFILE.LN_INSEARN_ORDER_10,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            LNFILE.LN_GARNISH_EXTR,
      *            LNFILE.LN_SHIP_SAME_FG,
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_TOT_PARTIAL_CHGOFF,
      *            LNFILE.LN_LPO,
      *            LNFILE.LN_REP_CD,
      *            LNFILE.LN_EMP_ID,
      *            LNFILE.LN_PROCESS_CD,
      *            LNFILE.LN_WCBC_CREDITLIM,
      *            LNFILE.LN_FOLLOWUP_CD,
      *            LNFILE.LN_MIP_FG,
      *            LNFILE.LN_MIP_MTD_CLAHPAID,
      *            CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_MIP_TOT_CLPAID,
      *            CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_MIP_TOT_AHPAID,
      *            LNFILE.LN_IL_GROSS_INCOME,
      *            LNFILE.LN_IL_GROSS_INCOME2,
      *            LNFILE.LN_RESCISSION,
      *            LNFILE.LN_INTERIM_TRW,
      *            LNFILE.LN_DLNO,
      *            LNFILE.LN_DLPOOL,
      *            LNFILE.LN_DLRECOURSE,
      *            LNFILE.LN_DLPARTRCAMT,
      *            LNFILE.LN_ORIGPURAMT,
      *            LNFILE.LN_PURBAL,
      *            LNFILE.LN_DLRVPRIN_1,
      *            LNFILE.LN_DLRVPRIN_2,
      *            LNFILE.LN_DLRVPRIN_3,
      *            LNFILE.LN_DLRVCHGS_1,
      *            LNFILE.LN_DLRVCHGS_2,
      *            LNFILE.LN_DLRVCHGS_3,
      *            LNFILE.LN_DLPARVHELD,
      *            LNFILE.LN_DLPARVPAID,
      *            LNFILE.LN_DLDISC,
      *            CAST(LNFILE.LN_LCPDTH_DATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
      *            LNFILE.LN_STMT_COUNTER,
      *            LNFILE.LN_PRIOR_STMT_COUNTER,
      *            LNFILE.LN_FROZEN_REASON,
      *            LNFILE.LN_COUPON_STATEMENT,
      *            LNFILE.LN_MONTHEND_BAL,
      *            LNFILE.LN_ANTICTERM,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_ACCERN_1,
      *            LNFILE.LN_EARN_ORDER_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_ACCERN_2,
      *            LNFILE.LN_EARN_ORDER_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_ACCERN_3,
      *            LNFILE.LN_EARN_ORDER_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_ACCERN_4,
      *            LNFILE.LN_EARN_ORDER_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_ACCERN_5,
      *            LNFILE.LN_EARN_ORDER_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_ACCERN_6,
      *            LNFILE.LN_EARN_ORDER_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_ACCERN_7,
      *            LNFILE.LN_EARN_ORDER_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_ACCERN_8,
      *            LNFILE.LN_EARN_ORDER_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_ACCERN_9,
      *            LNFILE.LN_EARN_ORDER_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_ACCERN_10,
      *            LNFILE.LN_EARN_ORDER_10,
      *            LNFILE.LN_LMO_PRIOR_INTEREST,
      *            LNFILE.LN_PRIOR_UNEARNED,
      *            LNFILE.LN_ERNDELQ_STOP,
      *            LNFILE.LN_NONACRERN,
      *            LNFILE.LN_PRIOR_NONACRERN,
      *            CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_ERN_IB_PRINBAL,
      *            LNFILE.LN_ERN_IB_INTOWE,
      *            LNFILE.LN_TOTERN,
      *            LNFILE.LN_TOTOTH,
      *            LNFILE.LN_TOTINT,
      *            LNFILE.LN_TOTLCHG,
      *            LNFILE.LN_TOTDEF,
      *            LNFILE.LN_TOTSERACCT,
      *            LNFILE.LN_LCWAIVERS,
      *            LNFILE.LN_INTWAIVERS,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTEXCPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_YTDNODEF,
      *            LNFILE.LN_TOTNODEFACT,
      *            LNFILE.LN_YTDNODEFACT,
      *            LNFILE.LN_TOTNOALDEL,
      *            LNFILE.LN_YTDNOALDEL,
      *            LNFILE.LN_YTDINTPD,
      *            LNFILE.LN_LYRINTPD,
      *            LNFILE.LN_TOTINTPD,
      *            LNFILE.LN_RECASTINT,
      *            LNFILE.LN_RECASTPRIN,
      *            LNFILE.LN_CL_REBATE,
      *            LNFILE.LN_AH_REBATE,
      *            LNFILE.LN_PP_REBATE,
      *            LNFILE.LN_INT_REBATE,
      *            LNFILE.LN_SC_REBATE,
      *            LNFILE.LN_MF_REBATE,
      *            LNFILE.LN_DF_REBATE,
      *            LNFILE.LN_EX_REBATE,
      *            LNFILE.LN_O1_REBATE,
      *            LNFILE.LN_O2_REBATE,
      *            LNFILE.LN_O3_REBATE,
      *            LNFILE.LN_O4_REBATE,
      *            LNFILE.LN_O5_REBATE,
      *            LNFILE.LN_O6_REBATE,
      *            LNFILE.LN_O7_REBATE,
      *            LNFILE.LN_UN_O7_REB,
      *            LNFILE.LN_UN_O8_REB,
      *            LNFILE.LN_UN_O9_REB,
      *            LNFILE.LN_UN_O10_REB,
      *            LNFILE.LN_PLREASON,
      *            LNFILE.LN_COLLECTOR,
      *            LNFILE.LN_ACTIONCD,
      *            LNFILE.LN_ACTIONCD2,
      *            LNFILE.LN_ATCODE,
      *            LNFILE.LN_RECODE,
      *            LNFILE.LN_ESCROW_FG,
      *            LNFILE.LN_PROMDAYS_PASTDUE,
      *            LNFILE.LN_PROMID,
      *            CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),
      *            LNFILE.LN_PROMAMT,
      *            CAST(LNFILE.LN_PROMDATE2       AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DATE_PAID_LAST  AS VARCHAR(10)),
      *            LNFILE.LN_AMT_PAID_LAST,
      *            LNFILE.LN_LASTCFORMNO,
      *            CAST(LNFILE.LN_LASTCFORMDATE   AS VARCHAR(10)),
      *            LNFILE.LN_DELCNT,
      *            LNFILE.LN_DELCNT2,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_CRSCORE,
      *            LNFILE.LN_LASTSFORMNO,
      *            CAST(LNFILE.LN_LASTSFORMDATE   AS VARCHAR(10)),
      *            LNFILE.LN_SOLCNT,
      *            LNFILE.LN_MESSAGE,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_PRBAL,
      *            LNFILE.LN_PRINTDUE,
      *            LNFILE.LN_PRFINCHGRB,
      *            LNFILE.LN_PRINSRB,
      *            LNFILE.LN_PRPOCD,
      *            CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),
      *            LNFILE.LN_PRDELINQAMT,
      *            LNFILE.LN_PRLNAMT,
      *            LNFILE.LN_PRPDUE_DAYS,
      *            LNFILE.LN_PRDLDISC_UNEARN,
      *            LNFILE.LN_PRCONPROFILE,
      *            LNFILE.LN_SUM_ORIGPURAMT,
      *            LNFILE.LN_SUM_LNAMT,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_INTCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_SUM_REGPYAMT,
      *            LNFILE.LN_SUM_EXTCHG,
      *            LNFILE.LN_SUM_SERCHG,
      *            LNFILE.LN_SUM_MAINTFEE,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_DAYS_IN_UNITPER,
      *            LNFILE.LN_UNITPER_PER_YEAR,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_BR_TFR_FG,
      *            LNFILE.LN_O3INS_DLPROC,
      *            LNFILE.LN_INSPREM_DLADJ,
      *            LNFILE.LN_PLAMT,
      *            LNFILE.LN_MTD_PRIN,
      *            LNFILE.LN_MTD_LC,
      *            LNFILE.LN_MTD_INT,
      *            LNFILE.LN_MTD_OT,
      *            LNFILE.LN_MTD_EX,
      *            LNFILE.LN_BNKRPT_CHAPTER,
      *            LNFILE.LN_BMO_CON_STATUS,
      *            LNFILE.LN_BMO_REC_STATUS,
      *            LNFILE.LN_APP_NUMBER,
      *            LNFILE.LN_GIFTNET,
      *            LNFILE.LN_GIFTNET_OUR_PO,
      *            LNFILE.LN_PROC_DISBURSED,
      *            LNFILE.LN_NOODDPY,
      *            LNFILE.LN_CONPROFILE_1,
      *            LNFILE.LN_CONPROFILE_2,
      *            LNFILE.LN_CONPROFILE_3,
      *            LNFILE.LN_CONPROFILE_4,
      *            LNFILE.LN_CONPROFILE_5,
      *            LNFILE.LN_RECPROFILE_1,
      *            LNFILE.LN_RECPROFILE_2,
      *            LNFILE.LN_RECPROFILE_3,
      *            LNFILE.LN_RECPROFILE_4,
      *            LNFILE.LN_RECPROFILE_5,
      *            LNFILE.LN_TRW_STATUS,
      *            LNFILE.LN_ORIGIN,
      *            LNFILE.LN_PO_TRCD,
      *            LNFILE.LN_NEXIS_CRSCORE,
      *            LNFILE.LN_TRW_B_CONSUMER_IND,
      *            LNFILE.LN_PARVHELD_PAID_FG,
      *            LNFILE.LN_ACCT_FROZEN,
      *            LNFILE.LN_TRW_SPECIAL_CODE,
      *            LNFILE.LN_CREATION_STATUS,
      *            LNFILE.LN_TOTNOLCHG,
      *            LNFILE.LN_LBOX,
      *            LNFILE.LN_EXTRA_PRIN_LFP,
      *            LNFILE.LN_REMAIN_INT_SINCE_LFP,
      *            LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
      *            LNFILE.LN_INTERIM_COUPON,
      *            LNFILE.LN_DLCOMM_O3_CMSI,
      *            LNFILE.LN_DLCOMM_PP_CMSI,
      *            LNFILE.LN_TRW_J_CONSUMER_IND,
      *            LNFILE.LN_ACH,
      *            LNFILE.LN_1099_C_FG,
      *            LNFILE.LN_TRW_COMPLIANCE_CODE,
      *            LNFILE.LN_UNWIND_REASON_CODE,
      *            LNFILE.LN_PBI_PBD_FG,
      *            LNFILE.LN_OLD_ACCT_FORMAT_FG,
      *            LNFILE.LN_APPROVED_BY_USER,
      *            LNFILE.LN_APPROVED_BY_CODE,
      *            CAST(LNFILE.LN_APPROVED_BY_DATE  AS VARCHAR(10)),
      *            LNFILE.LN_APPROVED_BY_TIME,
      *            CAST(LNFILE.LN_LTOUCH_DATE       AS VARCHAR(10)),
      *            LNFILE.LN_LTOUCH_TIME,
      *            LNFILE.LN_LOS_APP_ID,
      *            BWFILE.BW_OWNBR, 
      *            BWFILE.BW_SSNO, 
      *            BWFILE.BW_LNAME, 
      *            BWFILE.BW_FNAME, 
      *            BWFILE.BW_ADR1, 
      *            BWFILE.BW_ADR2, 
      *            BWFILE.BW_CITY, 
      *            BWFILE.BW_STATE, 
      *            BWFILE.BW_ZIP, 
      *            CAST(BWFILE.BW_BDATE AS VARCHAR(10)), 
      *            BWFILE.BW_PHONE,
      *            BWFILE.BW_CELL_PHONE,
      *            ISNULL(BWFILE2.BW_LNAME,''),
      *            ISNULL(BWFILE2.BW_FNAME,''),
      *            ISNULL(LTDFILE.LTD_DEBIT_CARD_AMT,0),
      *            ISNULL(LTBFILE.LTB_BRNO,0),
      *            ISNULL(CAST(LTBFILE.LTB_REAFF_DATE     AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LTBFILE.LTB_RESCIND_DATE   AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LTBFILE.LTB_DISCHARGE_DATE AS VARCHAR(10)),'0')
      *    FROM DBO.LNFILE LNFILE
      *    LEFT OUTER JOIN DBO.BWFILE BWFILE2
      *      ON BWFILE2.BW_OWNBR = LNFILE.LN_OWNBR 
      *     AND BWFILE2.BW_SSNO  = LNFILE.LN_SSNO2
      *    LEFT OUTER JOIN DBO.LTDFILE LTDFILE
      *      ON LTDFILE.LTD_BRNO   = LNFILE.LN_OWNBR 
      *     AND LTDFILE.LTD_ACCTNO = LNFILE.LN_ACCTNO
      *     AND LTDFILE.LTD_SEQNO  = 1
      *    LEFT OUTER JOIN DBO.LTBFILE LTBFILE
      *      ON LTBFILE.LTB_BRNO   = LNFILE.LN_OWNBR 
      *     AND LTBFILE.LTB_ACCTNO = LNFILE.LN_ACCTNO
      *     AND LTBFILE.LTB_SEQNO  = 1
      *    INNER JOIN DBO.BWFILE BWFILE
      *     ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR 
      *  AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1
      *  WHERE LNFILE.LN_OWNBR    = @QLN3_WBEG_OWNBR
      *    AND LNFILE.LN_CLASS    BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *    AND LNFILE.LN_POCD     NOT IN ('**', '*I', '*R', '*U')
      *    AND LNFILE.LN_LOANDATE BETWEEN @BEG_LNDATE AND @END_LNDATE
      *    AND (LNFILE.LN_CURBAL > 0 OR LNFILE.LN_INTBAL > 0 OR LNFILE.LN_LCBAL > 0 OR LNFILE.LN_OTHBAL > 0)
      *    AND LNFILE.LN_DLPOOL < 80000
      *    AND (LNFILE.LN_PLDATE = '1900-01-01' OR LNFILE.LN_PLDATE > '2010-01-01')
      *    AND (LNFILE.LN_PLDATE = '1900-01-01' OR LNFILE.LN_POFFDATE = '1900-01-01')
      *    ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO 

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW(2), LTB, LTD
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC QLTD-REC QLTB-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LN3_CURSOR INTO 
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
                   :QLN-ORGBR,
                   :QLN-OWNST,
                   :QLN-SPRCLASS,
                   :QLN-CONVERCD,
                   :QLN-MIDSTREAM,
                   :QLN-PURCD,
                   :QLN-SRCD,
                   :QLN-COLLCD,
                   :QLN-COLLID,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-CONFIDENTCD,
                   :QLN-ALLOTCD,
                   :QLN-RECASTCD,
                   :QLN-SKIP-TRW,
                   :QLN-STATUSFG,
                   :QLN-SMALLOAN,
                   :QLN-SALEFIN,
                   :QLN-SERVACCT,
                   :QLN-LOANTYPE,
                   :QLN-ADDON-FG,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LC-CODE,
                   :QLN-CAN-LOAN-NO,
                   :QLN-STATFLAGS,
                   :QLN-ENTDATE            ,
                   :QLN-LOANDATE           ,
                   :QLN-PURDATE            ,
                   :QLN-INTDATE            ,
                   :QLN-CASHDATE           ,
                   :QLN-FILEDATE           ,
                   :QLN-RESCISSION-DATE    ,
                   :QLN-PART-RECOURSE-DATE ,
                   :QLN-ORIG-1STPYDATE     ,
                   :QLN-1STPYDATE          ,
                   :QLN-JDDATE             ,
                   :QLN-PLDATE             ,
                   :QLN-PL-TRDATE          ,
                   :QLN-ACCELDATE          ,
                   :QLN-REDUDATE           ,
                   :QLN-POFFDATE           ,
                   :QLN-PO-TRDATE          ,
                   :QLN-LEGALDATE          ,
                   :QLN-BNKRPTDATE         ,
                   :QLN-BNKRPT-DISMISS-DATE   ,
                   :QLN-BNKRPT-DISCHARGE-DATE ,
                   :QLN-BNKRPT-REAFFIRM-DATE  ,
                   :QLN-DEBTAIDATE            ,
                   :QLN-REPODATE              ,
                   :QLN-REDEEMDATE            ,
                   :QLN-SOLDDATE              ,
                   :QLN-INS-CONFIRM-DATE      ,
                   :QLN-PRIOR-RN-FOR10-DATE   ,
                   :QLN-LAST-RN-FOR10-DATE    ,
                   :QLN-TRW-DOFD-DATE         ,
                   :QLN-LAST-PYPA-DATE        ,
                   :QLN-BT-TRDATE             ,
                   :QLN-METRO2-LAST-PMT-DATE  ,
                   :QLN-FOLLOWUP-DATE         ,
                   :QLN-LAST-ACCRUAL-DATE     ,
                   :QLN-APRATE,
                   :QLN-EFFRATE,
                   :QLN-SMPRATE,
                   :QLN-YLDRATE,
                   :QLN-OVRDRATE,
                   :QLN-ENDORATE,
                   :QLN-JDRATE,
                   :QLN-ACCRATE,
                   :QLN-REDURATE,
                   :QLN-CONRATE,
                   :QLN-SERVPER,
                   :QLN-SELLPR,
                   :QLN-DOWNPY,
                   :QLN-TRADEIN,
                   :QLN-LNAMT,
                   :QLN-FINCHG,
                   :QLN-INTCHG,
                   :QLN-SERCHG,
                   :QLN-EXTCHG,
                   :QLN-MAINTFEE,
                   :QLN-POINTS,
                   :QLN-PROCEEDS,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PROCASH,
                   :QLN-TILLNO,
                   :QLN-STATE-FLAG,
                   :QLN-STATE-WORKER,
                   :QLN-STATE-WORKER2,
                   :QLN-FEECODE-1,
                   :QLN-FEEAMT-1,
                   :QLN-FEEOURS-1,
                   :QLN-FEEFINCHG-1,
                   :QLN-FEECODE-2,
                   :QLN-FEEAMT-2,
                   :QLN-FEEOURS-2,
                   :QLN-FEEFINCHG-2,
                   :QLN-FEECODE-3,
                   :QLN-FEEAMT-3,
                   :QLN-FEEOURS-3,
                   :QLN-FEEFINCHG-3,
                   :QLN-FEECODE-4,
                   :QLN-FEEAMT-4,
                   :QLN-FEEOURS-4,
                   :QLN-FEEFINCHG-4,
                   :QLN-FEECODE-5,
                   :QLN-FEEAMT-5,
                   :QLN-FEEOURS-5,
                   :QLN-FEEFINCHG-5,
                   :QLN-FEECODE-6,
                   :QLN-FEEAMT-6,
                   :QLN-FEEOURS-6,
                   :QLN-FEEFINCHG-6,
                   :QLN-FEECODE-7,
                   :QLN-FEEAMT-7,
                   :QLN-FEEOURS-7,
                   :QLN-FEEFINCHG-7,
                   :QLN-FEECODE-8,
                   :QLN-FEEAMT-8,
                   :QLN-FEEOURS-8,
                   :QLN-FEEFINCHG-8,
                   :QLN-FEECODE-9,
                   :QLN-FEEAMT-9,
                   :QLN-FEEOURS-9,
                   :QLN-FEEFINCHG-9,
                   :QLN-FEECODE-10,
                   :QLN-FEEAMT-10,
                   :QLN-FEEOURS-10,
                   :QLN-FEEFINCHG-10,
                   :QLN-FEECODE-11,
                   :QLN-FEEAMT-11,
                   :QLN-FEEOURS-11,
                   :QLN-FEEFINCHG-11,
                   :QLN-FEECODE-12,
                   :QLN-FEEAMT-12,
                   :QLN-FEEOURS-12,
                   :QLN-FEEFINCHG-12,
                   :QLN-FEECODE-13,
                   :QLN-FEEAMT-13,
                   :QLN-FEEOURS-13,
                   :QLN-FEEFINCHG-13,
                   :QLN-FEECODE-14,
                   :QLN-FEEAMT-14,
                   :QLN-FEEOURS-14,
                   :QLN-FEEFINCHG-14,
                   :QLN-FEECODE-15,
                   :QLN-FEEAMT-15,
                   :QLN-FEEOURS-15,
                   :QLN-FEEFINCHG-15,
                   :QLN-FEECODE-16,
                   :QLN-FEEAMT-16,
                   :QLN-FEEOURS-16,
                   :QLN-FEEFINCHG-16,
                   :QLN-FEECODE-17,
                   :QLN-FEEAMT-17,
                   :QLN-FEEOURS-17,
                   :QLN-FEEFINCHG-17,
                   :QLN-FEECODE-18,
                   :QLN-FEEAMT-18,
                   :QLN-FEEOURS-18,
                   :QLN-FEEFINCHG-18,
                   :QLN-FEECODE-19,
                   :QLN-FEEAMT-19,
                   :QLN-FEEOURS-19,
                   :QLN-FEEFINCHG-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-20,
                   :QLN-FEEOURS-20,
                   :QLN-FEEFINCHG-20,
                   :QLN-INSURED,
                   :QLN-INSTYPES,
                   :QLN-INSELIGIBLE,
                   :QLN-INSCOMP-1,
                   :QLN-INSOURS-1,
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEXP-DATE-1 ,
                   :QLN-INSPREM-1,
                   :QLN-INSCOVR-1,
                   :QLN-INSCOMM-1,
                   :QLN-INSANTICERN-1,
                   :QLN-INSANTICADJ-1,
                   :QLN-INSUFERN-1,
                   :QLN-INSACCERN-1,
                   :QLN-INSEARN-ORDER-1,
                   :QLN-INSFINCHG-1,
                   :QLN-INSCOMP-2,
                   :QLN-INSOURS-2,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEXP-DATE-2 ,
                   :QLN-INSPREM-2,
                   :QLN-INSCOVR-2,
                   :QLN-INSCOMM-2,
                   :QLN-INSANTICERN-2,
                   :QLN-INSANTICADJ-2,
                   :QLN-INSUFERN-2,
                   :QLN-INSACCERN-2,
                   :QLN-INSEARN-ORDER-2,
                   :QLN-INSFINCHG-2,
                   :QLN-INSCOMP-3,
                   :QLN-INSOURS-3,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEXP-DATE-3 ,
                   :QLN-INSPREM-3,
                   :QLN-INSCOVR-3,
                   :QLN-INSCOMM-3,
                   :QLN-INSANTICERN-3,
                   :QLN-INSANTICADJ-3,
                   :QLN-INSUFERN-3,
                   :QLN-INSACCERN-3,
                   :QLN-INSEARN-ORDER-3,
                   :QLN-INSFINCHG-3,
                   :QLN-INSCOMP-4,
                   :QLN-INSOURS-4,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEXP-DATE-4 ,
                   :QLN-INSPREM-4,
                   :QLN-INSCOVR-4,
                   :QLN-INSCOMM-4,
                   :QLN-INSANTICERN-4,
                   :QLN-INSANTICADJ-4,
                   :QLN-INSUFERN-4,
                   :QLN-INSACCERN-4,
                   :QLN-INSEARN-ORDER-4,
                   :QLN-INSFINCHG-4,
                   :QLN-INSCOMP-5,
                   :QLN-INSOURS-5,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEXP-DATE-5 ,
                   :QLN-INSPREM-5,
                   :QLN-INSCOVR-5,
                   :QLN-INSCOMM-5,
                   :QLN-INSANTICERN-5,
                   :QLN-INSANTICADJ-5,
                   :QLN-INSUFERN-5,
                   :QLN-INSACCERN-5,
                   :QLN-INSEARN-ORDER-5,
                   :QLN-INSFINCHG-5,
                   :QLN-INSCOMP-6,
                   :QLN-INSOURS-6,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEXP-DATE-6 ,
                   :QLN-INSPREM-6,
                   :QLN-INSCOVR-6,
                   :QLN-INSCOMM-6,
                   :QLN-INSANTICERN-6,
                   :QLN-INSANTICADJ-6,
                   :QLN-INSUFERN-6,
                   :QLN-INSACCERN-6,
                   :QLN-INSEARN-ORDER-6,
                   :QLN-INSFINCHG-6,
                   :QLN-INSCOMP-7,
                   :QLN-INSOURS-7,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEXP-DATE-7 ,
                   :QLN-INSPREM-7,
                   :QLN-INSCOVR-7,
                   :QLN-INSCOMM-7,
                   :QLN-INSANTICERN-7,
                   :QLN-INSANTICADJ-7,
                   :QLN-INSUFERN-7,
                   :QLN-INSACCERN-7,
                   :QLN-INSEARN-ORDER-7,
                   :QLN-INSFINCHG-7,
                   :QLN-INSCOMP-8,
                   :QLN-INSOURS-8,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEXP-DATE-8 ,
                   :QLN-INSPREM-8,
                   :QLN-INSCOVR-8,
                   :QLN-INSCOMM-8,
                   :QLN-INSANTICERN-8,
                   :QLN-INSANTICADJ-8,
                   :QLN-INSUFERN-8,
                   :QLN-INSACCERN-8,
                   :QLN-INSEARN-ORDER-8,
                   :QLN-INSFINCHG-8,
                   :QLN-INSCOMP-9,
                   :QLN-INSOURS-9,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEXP-DATE-9 ,
                   :QLN-INSPREM-9,
                   :QLN-INSCOVR-9,
                   :QLN-INSCOMM-9,
                   :QLN-INSANTICERN-9,
                   :QLN-INSANTICADJ-9,
                   :QLN-INSUFERN-9,
                   :QLN-INSACCERN-9,
                   :QLN-INSEARN-ORDER-9,
                   :QLN-INSFINCHG-9,
                   :QLN-INSCOMP-10,
                   :QLN-INSOURS-10,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-10 ,
                   :QLN-INSPREM-10,
                   :QLN-INSCOVR-10,
                   :QLN-INSCOMM-10,
                   :QLN-INSANTICERN-10,
                   :QLN-INSANTICADJ-10,
                   :QLN-INSUFERN-10,
                   :QLN-INSACCERN-10,
                   :QLN-INSEARN-ORDER-10,
                   :QLN-INSFINCHG-10,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-GARNISH-EXTR,
                   :QLN-SHIP-SAME-FG,
                   :QLN-AD-D-INS-COVR,
                   :QLN-TOT-PARTIAL-CHGOFF,
                   :QLN-LPO,
                   :QLN-REP-CD,
                   :QLN-EMP-ID,
                   :QLN-PROCESS-CD,
                   :QLN-WCBC-CREDITLIM,
                   :QLN-FOLLOWUP-CD,
                   :QLN-MIP-FG,
                   :QLN-MIP-MTD-CLAHPAID,
                   :QLN-MIP-CLPDTH-DATE ,
                   :QLN-MIP-TOT-CLPAID,
                   :QLN-MIP-AHPDTH-DATE ,
                   :QLN-MIP-TOT-AHPAID,
                   :QLN-IL-GROSS-INCOME,
                   :QLN-IL-GROSS-INCOME2,
                   :QLN-RESCISSION,
                   :QLN-INTERIM-TRW,
                   :QLN-DLNO,
                   :QLN-DLPOOL,
                   :QLN-DLRECOURSE,
                   :QLN-DLPARTRCAMT,
                   :QLN-ORIGPURAMT,
                   :QLN-PURBAL,
                   :QLN-DLRVPRIN-1,
                   :QLN-DLRVPRIN-2,
                   :QLN-DLRVPRIN-3,
                   :QLN-DLRVCHGS-1,
                   :QLN-DLRVCHGS-2,
                   :QLN-DLRVCHGS-3,
                   :QLN-DLPARVHELD,
                   :QLN-DLPARVPAID,
                   :QLN-DLDISC,
                   :QLN-LCPDTH-DATE  ,
                   :QLN-INTPDTH-DATE ,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-LCPAID,
                   :QLN-LCPARTIALS,
                   :QLN-STMT-COUNTER,
                   :QLN-PRIOR-STMT-COUNTER,
                   :QLN-FROZEN-REASON,
                   :QLN-COUPON-STATEMENT,
                   :QLN-MONTHEND-BAL,
                   :QLN-ANTICTERM,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICADJ-1,
                   :QLN-UFERN-1,
                   :QLN-ACCERN-1,
                   :QLN-EARN-ORDER-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICADJ-2,
                   :QLN-UFERN-2,
                   :QLN-ACCERN-2,
                   :QLN-EARN-ORDER-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICADJ-3,
                   :QLN-UFERN-3,
                   :QLN-ACCERN-3,
                   :QLN-EARN-ORDER-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICADJ-4,
                   :QLN-UFERN-4,
                   :QLN-ACCERN-4,
                   :QLN-EARN-ORDER-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICADJ-5,
                   :QLN-UFERN-5,
                   :QLN-ACCERN-5,
                   :QLN-EARN-ORDER-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICADJ-6,
                   :QLN-UFERN-6,
                   :QLN-ACCERN-6,
                   :QLN-EARN-ORDER-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICADJ-7,
                   :QLN-UFERN-7,
                   :QLN-ACCERN-7,
                   :QLN-EARN-ORDER-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICADJ-8,
                   :QLN-UFERN-8,
                   :QLN-ACCERN-8,
                   :QLN-EARN-ORDER-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICADJ-9,
                   :QLN-UFERN-9,
                   :QLN-ACCERN-9,
                   :QLN-EARN-ORDER-9,
                   :QLN-ANTICERN-10,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-10,
                   :QLN-EARN-ORDER-10,
                   :QLN-LMO-PRIOR-INTEREST,
                   :QLN-PRIOR-UNEARNED,
                   :QLN-ERNDELQ-STOP,
                   :QLN-NONACRERN,
                   :QLN-PRIOR-NONACRERN,
                   :QLN-ERN-IB-PDTH-DATE ,
                   :QLN-ERN-IB-PRINBAL,
                   :QLN-ERN-IB-INTOWE,
                   :QLN-TOTERN,
                   :QLN-TOTOTH,
                   :QLN-TOTINT,
                   :QLN-TOTLCHG,
                   :QLN-TOTDEF,
                   :QLN-TOTSERACCT,
                   :QLN-LCWAIVERS,
                   :QLN-INTWAIVERS,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTEXCPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-YTDNODEF,
                   :QLN-TOTNODEFACT,
                   :QLN-YTDNODEFACT,
                   :QLN-TOTNOALDEL,
                   :QLN-YTDNOALDEL,
                   :QLN-YTDINTPD,
                   :QLN-LYRINTPD,
                   :QLN-TOTINTPD,
                   :QLN-RECASTINT,
                   :QLN-RECASTPRIN,
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
                   :QLN-PLREASON,
                   :QLN-COLLECTOR,
                   :QLN-ACTIONCD,
                   :QLN-ACTIONCD2,
                   :QLN-ATCODE,
                   :QLN-RECODE,
                   :QLN-ESCROW-FG,
                   :QLN-PROMDAYS-PASTDUE,
                   :QLN-PROMID,
                   :QLN-PROMDATE ,
                   :QLN-PROMAMT,
                   :QLN-PROMDATE2       ,
                   :QLN-DATE-PAID-LAST  ,
                   :QLN-AMT-PAID-LAST,
                   :QLN-LASTCFORMNO,
                   :QLN-LASTCFORMDATE   ,
                   :QLN-DELCNT,
                   :QLN-DELCNT2,
                   :QLN-CREDITLIM,
                   :QLN-CRSCORE,
                   :QLN-LASTSFORMNO,
                   :QLN-LASTSFORMDATE   ,
                   :QLN-SOLCNT,
                   :QLN-MESSAGE,
                   :QLN-OLDACCTNO,
                   :QLN-PRLNNO,
                   :QLN-PRBAL,
                   :QLN-PRINTDUE,
                   :QLN-PRFINCHGRB,
                   :QLN-PRINSRB,
                   :QLN-PRPOCD,
                   :QLN-PRPODATE ,
                   :QLN-PRDELINQAMT,
                   :QLN-PRLNAMT,
                   :QLN-PRPDUE-DAYS,
                   :QLN-PRDLDISC-UNEARN,
                   :QLN-PRCONPROFILE,
                   :QLN-SUM-ORIGPURAMT,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-FINCHG,
                   :QLN-SUM-INTCHG,
                   :QLN-SUM-ORGTERM,
                   :QLN-SUM-REGPYAMT,
                   :QLN-SUM-EXTCHG,
                   :QLN-SUM-SERCHG,
                   :QLN-SUM-MAINTFEE,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-DAYS-IN-UNITPER,
                   :QLN-UNITPER-PER-YEAR,
                   :QLN-REPOCD,
                   :QLN-BR-TFR-FG,
                   :QLN-O3INS-DLPROC,
                   :QLN-INSPREM-DLADJ,
                   :QLN-PLAMT,
                   :QLN-MTD-PRIN,
                   :QLN-MTD-LC,
                   :QLN-MTD-INT,
                   :QLN-MTD-OT,
                   :QLN-MTD-EX,
                   :QLN-BNKRPT-CHAPTER,
                   :QLN-BMO-CON-STATUS,
                   :QLN-BMO-REC-STATUS,
                   :QLN-APP-NUMBER,
                   :QLN-GIFTNET,
                   :QLN-GIFTNET-OUR-PO,
                   :QLN-PROC-DISBURSED,
                   :QLN-NOODDPY,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5,
                   :QLN-TRW-STATUS,
                   :QLN-ORIGIN,
                   :QLN-PO-TRCD,
                   :QLN-NEXIS-CRSCORE,
                   :QLN-TRW-B-CONSUMER-IND,
                   :QLN-PARVHELD-PAID-FG,
                   :QLN-ACCT-FROZEN,
                   :QLN-TRW-SPECIAL-CODE,
                   :QLN-CREATION-STATUS,
                   :QLN-TOTNOLCHG,
                   :QLN-LBOX,
                   :QLN-EXTRA-PRIN-LFP,
                   :QLN-REMAIN-INT-SINCE-LFP,
                   :QLN-REMAIN-PRIN-SINCE-LFP,
                   :QLN-INTERIM-COUPON,
                   :QLN-DLCOMM-O3-CMSI,
                   :QLN-DLCOMM-PP-CMSI,
                   :QLN-TRW-J-CONSUMER-IND,
                   :QLN-ACH,
                   :QLN-1099-C-FG,
                   :QLN-TRW-COMPLIANCE-CODE,
                   :QLN-UNWIND-REASON-CODE,
                   :QLN-PBI-PBD-FG,
                   :QLN-OLD-ACCT-FORMAT-FG,
                   :QLN-APPROVED-BY-USER,
                   :QLN-APPROVED-BY-CODE,
                   :QLN-APPROVED-BY-DATE  ,
                   :QLN-APPROVED-BY-TIME,
                   :QLN-LTOUCH-DATE,
                   :QLN-LTOUCH-TIME,
                   :QLN-LOS-APP-ID,
                   :QBW-OWNBR, 
                   :QBW-SSNO, 
                   :QBW-LNAME, 
                   :QBW-FNAME, 
                   :QBW-ADR1, 
                   :QBW-ADR2, 
                   :QBW-CITY, 
                   :QBW-STATE, 
                   :QBW-ZIP, 
                   :QBW-BDATE,
                   :QBW-PHONE,
                   :QBW-CELL-PHONE,
                   :RETR-BW2-LNAME,
                   :RETR-BW2-FNAME,
                   :QLTD-DEBIT-CARD-AMT,
                   :QLTB-BRNO,
                   :QLTB-REAFF-DATE,
                   :QLTB-RESCIND-DATE,
                   :QLTB-DISCHARGE-DATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS
              PERFORM GET-LTB-FIELDS
              PERFORM GET-LTD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.
      *-----------------------------------------------------------------

       OPEN-WK-LN-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LN-IFN TO E-FILE.
           OPEN OUTPUT WK-LN-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LN-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LN-FILE
              GO TO OPEN-WK-LN-FILE-OUTPUT.
       WRITE-WK-LN-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LN-IFN TO E-FILE.
           WRITE WK-LN-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LN-FILE.
       CLOSE-WK-LN-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LN-FILE.

       OPEN-WK-LP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LP-IFN TO E-FILE.
           OPEN OUTPUT WK-LP-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LP-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LP-FILE
              GO TO OPEN-WK-LP-FILE-OUTPUT.
       WRITE-WK-LP-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LP-IFN TO E-FILE.
           WRITE WK-LP-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LP-FILE.
       CLOSE-WK-LP-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LP-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **********************
       END-PROGRAM SECTION.
      **********************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY REPAYE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
