       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FEUNR1.
       DATE-WRITTEN.    11102004.
      ******************************************************************
      *   DIR : CL
      *   DESC: UNEARNED FEE CODE REPORT
      *
      *   UP TO 3 ENTERED FEE CODES PER REPORT
      *      FEE CODES AND REBATE METHODS ENTERED ON SCREEN
      *
      *   THIS REPORT PRODUCES THE FOLLOWING:
      *     1. THE TOTAL NO. OF ACCOUNTS WITH MATCHING FEE CODES
      *     2. THE ENDING UNEARNED FOR THE PREVIOUS
      *        PERIOD ENDING
      *     3. NEW FEE CODE ACCOUNTS
      *     4. THE ENDING UNEARNED FOR THIS PERIOD
      *
      *   NOTE:
      *        THIS PROGRAM NEEDS MODIFICATION IF EVER,
      *        IT ATTEMPS TO UPDATE THE LN-FILE.
      *
      * REVS:
      * 091395 KEC ADDED GROUP LOGIC
      * 092595 BAH GET-COMMISSION HAS A PERIOD IN THE MIDDLE OF AN IF
      * KEC 013096 ADDED LOGIC TO PRINT A BREAKDOWN OF DEBITS AND
      *            CREDITS AND THE GL ACCOUNT NUMBER.
      * KEC 021596 CHANGED CALCULATION OF WS-NET-INCOME TO MINUS
      *            NEW-REFUNDS(SUBSCRIPT).
      * KEC 021596 REMOVED THE CHANGED MADE TO THE CALCULATION OF
      *            WS-NET-INCOME TO MINUS NEW-REFUNDS(SUBSCRIPT).  ALSO,
      *            ADDED NEW FLAG (WS-NET-INCOME-CALC) TO PREVENT ADDING
      *            TO WS-NET-INCOME TWICE.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      * BAH 073097 ADDED REB-METHOD 7 AND 9 FROM A48
      * JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90
      *  CS 042498 ADD TO TEST OF PB,RN,SC NEW CODES RB AND RO
      * BAH 060498 GI RECAP WAS ALWAYS USING LEVEL LIFE ACCOUNTS, TEST IF
      *            SP-INSCOM(3) IS 0, THEN USE DECREASING LIFE GI ACCOUNTS
      *            NORTHSTATE, PL# 182
      * JTG 070398 CHANGED TO CORRECTLY REFLECT VOIDED ACCOUNTS DONE THE MONTH
      *            REPORTED
      * JTG 110298 CORRECTED BOOKED & VOIDED ACCOUNTS DONE THE MONTH REPORTED
      * GLF 033099 ADD TRCDS OF 'PZ' AND 'PN'
      * JTG 990604 CHANGED TO ADD TO NEW PREMIUMS WHEN LOAN WAS BOOKED AND
      *            PAIDOUT IN SAME MONTH AS REPORT MONTH
      * JTG 990616 ADDED 'ST' ADDON TEST
      * JTG 990618 ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC
      *            WHICH WILL ALLOW SELECTING BR-INS-ADDON 'Y' OR 'O'
      *            WHEN BR-REB-METHOD-.() = 4 OR 5
      * BLV 990909 TEST FOR LP-SSNO OF 9'S FOR REGMGM & SKIP, THEIR TRAN
      *            CODES FROM OLD SYSTEM INCLUDED "CC", MESSED UP REPORT!
      * GLF 091499 ADD INCLUDE, SKIP, ONLY OF P&L ACCOUNTS PER JR XREL
      * MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * MJD 991001 ADDED LOGIC TO COMPUTE COMMISSION AS EITHER % OR
      *            FIXED AMOUNT  (TFC PR#41)
      * BLV 991220 FIX COMPUTE-BEG-UNEARNED ROUTINE -  WAS ADDING TO NEW
      *            PREMIUMS IF ACCT BOOKED & PAID OFF THIS MONTH - CHANGED
      *            TO NOT ADD IF ACCT WAS TRANSFERED.  UNTIL FIX, ACCOUNTS
      *            BOOKED & TRANSFERRED IN SAME MONTH WOULD HAVE NEW PREM
      *            BUT NO ENDING, SO GL SUMMARY WOULD SAY TO TAKE INCOME
      *            IN FORMER BRANCH EVEN THO CURRENT BRANCH WOULD EVENTUALLY
      *            TAKE INCOME.
      * JTG 001114 ADDED  78  LN-INSTBL-SIZE  VALUE 46 FOR CL/ICUNR*.C PROGRAMS
      * BAH 010625 ADDED BR-REB-METHOD-2(4) AS EARNINGS FOR LEVEL LIFE VERSUS
      *            THE BR-REB-METHOD(1) FOR DECREASING. WILL ONLY SEE A
      *            DIFFERENCE IF THIS FIELD IS NOT 0. WILL PRINT A "D" OR "L"
      *            AFTER THE ACCOUNT NO., REGWAN #1500
      *            ALSO REMOVED FORMULA DESCRIPTION FROM THE HEADING (HEAD4B)
      * BLV 011004 SKIP LOANS BOOKED & VOIDED IN SAME MONTH, FOR EXAMPLE:
      *            LOAN BOOKED, ADDON INSURANCE POSTED, ADDON REVERSED,
      *            LOAN VOIDED.  THIS WILL GIVE READ ERROR ON LT BECAUSE VOID
      *            DELETES ALL LT'S, & HERE THE ADDON PMT TRIES TO READ THE LT.
      * JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      * BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *            LIKE "PE", REGACC PR# 0007
      * JTG 030304 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      * BAH 031212 CREATE AN EXTRACT FILE WITH GL ENTRIES FOR REGWAN PR# 2209
      * JTG 040621 CHANGED ICUNR(1,2,3).C UNEARNED INSURANCE REPORTS TO
      *            ACCUMULATE BOTH DEBIT & CREDIT AMOUNTS IN THE INTERNAL
      *            WORK RECORDS SO WHEN GLNO'S FOR UNEARNED AND INCOME ARE
      *            THE SAME, THEY WILL HOLD BOTH AMOUNTS AND PRINT AN EQUAL
      *            AMOUNT OF DB/CR'S.                               WORLD #J045
      * JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *            RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *            OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      * JTG 041110 PROGRAMMED UNEARNED FEE CODE REPORT            LENDMARK 2387
      * JTG 041130 CHANGED TEST ON LN-FEEOURS() TO TEST FOR = TO " "
      *            FOR DIRECT LOANS IN SEARCH FOR MATCHING FEE CODE
      *            ON THIS ACCOUNT                                 LENDMARK 2387
      * JTG 041130 FILES WERE NOT BEING CLOSED AT END OF GROUP
      * JTG 050113 ADDED 'LOAN ENTERED START DATE' TO REPORT       LENDMARK 2444
      *  CS 050316 TRANSFERRED (BT) ACCOUNTS WERE REFLECTED IN ALL BRANCHES
      *            ADDED CHECK TO SKIP IF LN-POCD-TRANSFERRED (POCD = BT)
      *                                                     [PR#2487 LENDMARK]
      *  CS 050317 CHANGED 50316 CHANGE TO SKIP ALL ZERO BALANCE ACCOUNTS.
      *            PAID ACCTS HAD REMAINING UNEARNED (UNLIKE INSURANCE THERE
      *            IS NO REBATE.  ALSO TRANSFERRED ACCOUNTS WERE REFLECTED IN
      *            MULTIPLE BRANCHES... NET EFFECT WAS NEGATIVE EARNINGS
      *                                                    [PR#2487 LENDMARK]
      *   CS 151125 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *   CS 160129 REMOVED GI COPY MEMBERS, ITS NOT USED
      * KEC 2018.0424 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *              LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *              WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *              LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *              PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 2018.1203 GLOBAL LNFILE
      * BAH 2018.1221 GLOBAL LPFILE
      * BAH 2019.0918 REMOVED ACCESS-CALL ON LNFILE
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
      *
      *   SUMMARY TOTALS WORK RECORD
      *
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO   PIC 9(4).
               05  WK-CLASS  PIC 9(3).
               05  WK-AC     PIC 9(8).
           03  WK-QTY        PIC 9(6)     COMP-3 OCCURS 3.
           03  WK-AMT        PIC S9(7)V99 COMP-3 OCCURS 3.
           03  WK-BEG        PIC S9(7)V99 COMP-3 OCCURS 3.
           03  WK-END        PIC S9(7)V99 COMP-3 OCCURS 3.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/FEUNR1 S35 04/01/24-09:50:15 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN               PIC XX VALUE "WK".
       01  WK-PATH              PIC X(35).

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  TOT-WORKERS.
           03  TOT-LV               OCCURS 2.
               05  TOT-LC           OCCURS 101.
                   07  TOT-QTY      PIC 9(6)     COMP-3 OCCURS 3.
                   07  TOT-AMT      PIC S9(7)V99 COMP-3 OCCURS 3.
                   07  TOT-BEG      PIC S9(7)V99 COMP-3 OCCURS 3.
                   07  TOT-END      PIC S9(7)V99 COMP-3 OCCURS 3.

       01  IC                   PIC 9(6) COMP-3.

      * LOAN CLASS (1-101)
       01  LC                   PIC 9(6) COMP-3.

      * TOTAL LEVEL
       01  LV                   PIC 9(6) COMP-3.

      * FEE CODE (#1 #2 #3)
       01  TP                   PIC 9(6) COMP-3.

       01  HOLD-OWNBR           PIC 9(4) VALUE 9999.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  ACCRUAL-MONTHS       PIC 9(3)     OCCURS 3.
       01  REBDAYS-MONTH        PIC 9(2)     OCCURS 3.
       01  XUB                  PIC 9(6)     COMP-3.
       01  FUB                  PIC 9(6)     COMP-3.
       01  SUB                  PIC 9(6)     COMP-3.
       01  SAVE-WK-KEY          PIC X(15).
       01  REC-EXISTS           PIC X        VALUE "N".
       01  PROCESS-ERROR.
           03  FILLER           PIC X(28)    VALUE
               "ERROR PROCESSING LOAN NO:  ".
           03  PROCESS-NUMBER   PIC 9(8).
       01  METHOD-4-FG          PIC X.
       01  METHOD-4-BEG-PAYDATE PIC 9(8)     COMP-3.
       01  METHOD-4-END-PAYDATE PIC 9(8)     COMP-3.
       01  METHOD-5-FG          PIC X.
       01  METHOD-5-BEG-PAYMNTD PIC 9(7)V99  COMP-3.
       01  METHOD-5-END-PAYMNTD PIC 9(7)V99  COMP-3.
       01  PAYMNTD              PIC 9(7)V99  COMP-3.
       01  SAVE-LN-TOTPAYMNTD   PIC 9(7)V99  COMP-3.

      *-----------------------------------------------------------------
       01  UNRP-SUMW         PIC S9(5)      COMP-3.
       01  UNRP-SUMW1        PIC S9(5)      COMP-3.
       01  UNRP-WORKER       PIC S9(7)V9(8) COMP-3.

      *-----------------------------------------------------------------
       01  RV-CNT               PIC 99.
       01  RV-TABLE.
           03  RV-DATES         OCCURS 30 TIMES.
               05  RV-DATE      PIC 9(8).
               05  FILLER       REDEFINES RV-DATE.
                   07  RV-CC    PIC 99.
                   07  RV-YR    PIC 99.
                   07  RV-MO    PIC 99.
                   07  RV-DA    PIC 99.

      *-----------------------------------------------------------------
       01  DASH-LINE.
           03  FILLER           PIC X(22)    VALUE ALL "-".
           03  FILLER           PIC X(3)     VALUE SPACES.
           03  FILLER           PIC X(35)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(35)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(35)    VALUE ALL "-".

       01  LAST-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  HOLD-BR              PIC 9(4)     COMP-3 VALUE 0.

       01  FEEAMT-TAB.
           03  FEEAMT           PIC S9(7)V99 OCCURS 3.
           03  FEEOURS          PIC X        OCCURS 3.
           03  FEEFINCHG        PIC X        OCCURS 3.

       01  UNEARNED-TAB.
           03  BEG-UNEARNED     PIC S9(7)V99 OCCURS 3.
           03  NEW-FEEAMT       PIC S9(7)V99 OCCURS 3.
           03  NEW-REFUNDS      PIC S9(7)V99 OCCURS 3.
           03  END-UNEARNED     PIC S9(7)V99 OCCURS 3.
           03  CHK-UNEARNED     PIC X        OCCURS 3.

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
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE "COMPANY ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(40)    VALUE
           "      UNEARNED FEE CODE REPORT   ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(56) VALUE " ".
           03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(26) VALUE " ".
           03  FILLER           PIC X(19) VALUE "-------- FEE CODE (".
           03  HEAD4-FEE1       PIC XX.
           03  FILLER           PIC X(13) VALUE ") -------    ".
           03  FILLER           PIC X(19) VALUE "-------- FEE CODE (".
           03  HEAD4-FEE2       PIC XX.
           03  FILLER           PIC X(15) VALUE ") --------     ".
           03  FILLER           PIC X(19) VALUE "-------- FEE CODE (".
           03  HEAD4-FEE3       PIC XX.
           03  FILLER           PIC X(13) VALUE ") --------   ".


      * BAH 7/16/01 REMOVED BECAUSE WHEN LOOPING THRU BRANCH FILE, YOU
      * END UP WITH THE NEXT BRANCH, THEN GO TO END-ROUTINE AND PRINT
      * FROM THE WORK FILE, YOU DONT EVEN HAVE THE CORRECT BRANCHES INFO !
      *-----------------------------------------------------------------
      *01  HEAD4B.
      *    03  FILLER           PIC X(35) VALUE SPACES.
      *    03  H-CL-METHOD      PIC X(13).
      *    03  FILLER           PIC X(24) VALUE SPACES.
      *    03  H-AH-METHOD      PIC X(13).
      *    03  FILLER           PIC X(22) VALUE SPACES.
      *    03  H-PP-METHOD      PIC X(13).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(50) VALUE
           "                         # FEE    BEG         NEW ".
           03  FILLER           PIC X(50) VALUE
           "    END    # FEE    BEG         NEW     END    # F".
           03  FILLER           PIC X(32) VALUE
           "EE    BEG         NEW     END   ".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(50) VALUE
           "ACCOUNT #    BR CL       ACCTS  UNEARNED     FEES ".
           03  FILLER           PIC X(50) VALUE
           "  UNEARNED ACCTS  UNEARNED     FEES   UNEARNED ACC".
           03  FILLER           PIC X(32) VALUE
           "TS  UNEARNED     FEES   UNEARNED".

      *-----------------------------------------------------------------
       01  HEAD6X               REDEFINES HEAD6.
           03  HEAD6X-ALT       PIC X(22).
           03  FILLER           PIC X(110).

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-AC             PIC 9(8).
           03  FILLER           PIC X(2).
           03  FILLER           PIC X.
           03  D-BR             PIC 9(4).
           03  FILLER           PIC X.
           03  D-LC             PIC 99.
           03  FILLER           PIC X(108).

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC X.
           03  D-BRNAME         PIC X(13).
           03  D-COLON          PIC X.
           03  FILLER           PIC X.
           03  D-CLASS          PIC 99.
           03  FILLER           PIC XX.
           03  D-INS            OCCURS 3.
               05  D-QTY        PIC Z(5).
               05  D-BEG        PIC Z(6)9.99  BLANK ZERO.
               05  D-AMT        PIC Z(6)9.99- BLANK ZERO.
               05  D-END        PIC Z(6)9.99  BLANK ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(25).
           03  RANGE-SEL        PIC X(107).
           03  FILLER          REDEFINES RANGE-SEL.
               05  FILLER       PIC X(5).
               05  RANGE-SEL2   PIC X(102).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       COPY "LIBCL/FEUNRPW.CPY".

       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/FEUNRP_DEF.CPY".
       COPY "LIBLP/FEUNRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       01  PRU-PATH-X                PIC X(90).

       SCREEN SECTION.
       COPY "LIBLP/FEUNRP_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME PRU-PATH-X EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
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

           MOVE PRU-PATH-X TO PRU-PATH.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FEUNRP" TO VDU-FORM-NAME.

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

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE.

           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LN3-FILE.


           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.

      *-----------------------------------------------------------------
       START-KEY-REPORT.
           MOVE BEG-LC  TO LN-CLASS
                           QLN3-WBEG-CLASS.
           MOVE END-LC  TO QLN3-WEND-CLASS.
           MOVE 0       TO LN-ACCTNO
                           QLN3-WBEG-ACCTNO.
           MOVE ALL "9" TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

      *-----------------------------------------------------------------
       NEXT-KEY-REPORT.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * SKIP ACCOUNTS IN RESCESSION OR THAT RESCINDED (DID NOT MAKE THE LOAN)
           IF LN-POCD-INRESCISSION OR
              LN-POCD-RESCIND
              GO TO NEXT-KEY-REPORT.

      * SKIP VOIDED LOANS THAT WERE NOT VOIDED THIS ACCOUNTING MONTH
           IF LN-POCD-VOID
              MOVE LN-POFFDATE        TO NUM-DATE
              MOVE REPORT-ENDING-DATE TO SYS-DATE
              IF NOT (NUM-MO = S-MM AND NUM-YR = S-YY)
                 GO TO NEXT-KEY-REPORT.

      * SKIP VOIDED LOANS BOOKED & VOIDED IN THE SAME ACCOUNTING MONTH
           IF LN-POCD-VOID
              MOVE LN-POFFDATE TO NUM-DATE
              MOVE LN-ENTDATE  TO SYS-DATE
              IF (NUM-MO = S-MM AND NUM-YR = S-YY)
                 GO TO NEXT-KEY-REPORT.

      *------------------------------------------------------------
      * CHANGED TO SKIP ALL ZERO BALANCE ACCOUNTS UNLESS ZERO BALANCE ONLY
      * IS 'Y'. PAID ACCTS HAD REMAINING *UNEARNED (UNLIKE INSURANCE THERE
      *IS NO REBATE.  ALSO TRANSFERRED ACCOUNTS WERE REFLECTED IN MULTIPLE
      * BRANCHES... NET EFFECT WAS NEGATIVE EARNINGS [PR#2487 LENDMARK]
      *-----------------------------------------------------------
           IF ZBAL-ONLY = "N"
              IF LN-CURBAL = ZERO
                GO TO NEXT-KEY-REPORT.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-OWNBR > HOLD-OWNBR
              MOVE LN-OWNBR TO HOLD-OWNBR
              GO TO START-KEY-REPORT.

           IF LN-CLASS < BEG-LC
              GO TO START-KEY-REPORT.

           IF LN-CLASS > END-LC
              IF LN-OWNBR = HOLD-OWNBR
                 GO TO END-OF-GROUP
              ELSE
                 GO TO NEXT-KEY-REPORT.

           IF LN-ENTDATE < ENTERED-START-DATE
              GO TO NEXT-KEY-REPORT.

           IF ZBAL-ONLY = "Y"
              IF LN-CURBAL NOT = 0
                 GO TO NEXT-KEY-REPORT.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-KEY-REPORT.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-KEY-REPORT.

      * CLEAR FEEAMT-TAB TABLE FOR THIS ACCOUNT
      
           PERFORM VARYING FUB FROM 1 BY 1 UNTIL FUB > 3
               MOVE 0   TO FEEAMT(FUB)
               MOVE " " TO FEEOURS(FUB)
                           FEEFINCHG(FUB)
           END-PERFORM.

      * SEARCH FOR MATCHING FEE CODE ON THIS ACCOUNT

           PERFORM VARYING FUB FROM 1 BY 1 UNTIL FUB > 3
                     AFTER SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) NOT = " "
                 IF FEE-CODE(FUB) = LN-FEECODE(SUB)
                    IF LN-FEEOURS(SUB) = "Y" OR " "
                       ADD LN-FEEAMT(SUB)   TO FEEAMT(FUB)
                       MOVE LN-FEEOURS(SUB) TO FEEOURS(FUB)
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

           IF FEEAMT(1) = 0
              IF FEEAMT(2) = 0
                 IF FEEAMT(3) = 0
                    GO TO NEXT-KEY-REPORT.

           MOVE LN-OWNBR TO HOLD-OWNBR.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 GO TO IO-ERROR.

           MOVE ZEROS TO RV-CNT RV-TABLE.

           PERFORM CREATE-TOTAL-RECS.

           GO TO NEXT-KEY-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS
           ELSE
              PERFORM PRINT-REPORT.

           PERFORM RANGE-LINES.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE "I" TO PL.

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.


           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

      * CLEAR FIELDS RELATED TO ICUNRP.C INSURANCE PROGRAM
      * WHICH CAME OUT OF BRANCH RECORD
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE 0 TO ACCRUAL-MONTHS(SUB)
                        REBDAYS-MONTH(SUB)
           END-PERFORM.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY    TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "UNEARNED FEE CODE REPORT IN PROGRESS...." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           GO TO EXIT-INITIALIZE.

       END-INITIALIZE.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *   T O T A L   R E C S
      *
      ******************************************************************
       CREATE-TOTAL-RECS SECTION.

           PERFORM CHECK-UNEARNED.

           MOVE 0 TO IC.

       CREATE-TOTAL-RECS-NEXT.
           ADD 1 TO IC.

      * EVEN IF NO UNEARNED REMAINS
      * STILL REPORT IF THERE WAS A BEGINNING UNEARNED:

           IF END-UNEARNED(IC) = 0
              IF BEG-UNEARNED(IC) = 0
                 IF NEW-FEEAMT(IC) = 0
                    IF NEW-REFUNDS(IC) = 0
                       GO TO CREATE-TOTAL-RECS-CHECK-END.

           MOVE 0         TO WK-AC.
           MOVE LN-OWNBR  TO WK-BRNO.
           MOVE LN-CLASS  TO WK-CLASS.
           PERFORM CREATE-WK.

           IF ( ACCOUNT-DETAIL = "Y" )
              MOVE LN-ACCTNO TO WK-AC
              PERFORM CREATE-WK.

           MOVE "N" TO FIRST-TIME.

       CREATE-TOTAL-RECS-CHECK-END.
           IF IC < 3
              GO TO CREATE-TOTAL-RECS-NEXT.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      ******************************************************************
      *
      *    C R E A T E - W K
      *
      ******************************************************************
       CREATE-WK SECTION.
       SAVE-WORK-KEY.
           MOVE "N" TO REC-EXISTS.
           MOVE WK-KEY TO SAVE-WK-KEY.

       CHECK-REC-EXISTS.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              GO TO UPDATE-WK-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.
           MOVE 0 TO TP.

       NEXT-WK-CLEAR.
           ADD 1 TO TP.
           MOVE 0 TO WK-QTY(TP).
           MOVE 0 TO WK-AMT(TP).
           MOVE 0 TO WK-BEG(TP).
           MOVE 0 TO WK-END(TP).

           IF TP < 3
              GO TO NEXT-WK-CLEAR.

           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

       UPDATE-WK-REC.
           ADD BEG-UNEARNED(IC) TO WK-BEG(IC).

           IF END-UNEARNED(IC) NOT = 0
               OR NEW-FEEAMT(IC) NOT = 0
                OR NEW-REFUNDS(IC) NOT = 0
              ADD 1 TO WK-QTY(IC)
              ADD NEW-FEEAMT(IC) TO WK-AMT(IC)
              ADD NEW-REFUNDS(IC)  TO WK-AMT(IC)
              ADD END-UNEARNED(IC) TO WK-END(IC).

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
      *
      *    P R I N T   R E P O R T
      *
      ******************************************************************
       PRINT-REPORT SECTION.

           IF ACCOUNT-DETAIL = "Y"
              PERFORM PRINT-DETAIL
              MOVE 99 TO LN-COUNT.

           MOVE "LOCATION         CLASS" TO HEAD6X-ALT.
           MOVE "Y" TO FIRST-TIME.
           MOVE 2 TO LV.
           PERFORM CLEAR-TOTALS.
           CLOSE WK-FILE.
           PERFORM OPEN-WK-FILE.

       NEXT-WK-READ.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO PRINT-REPORT-EXIT.
           IF WK-AC NOT = 0
              GO TO NEXT-WK-READ.

           UNLOCK WK-FILE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WK-BRNO TO LAST-BR
              PERFORM BR-HEADER.

           IF WK-BRNO NOT = LAST-BR
              PERFORM BR-TOTALS
              PERFORM BR-HEADER.

           IF SUMMARY-ONLY = "Y"
              MOVE WK-CLASS TO D-CLASS.

           MOVE 0 TO TP.

      *-----------------------------------------------------------------
       NEXT-WK-ACCUM.
           ADD 1 TO TP.

           ADD WK-QTY(TP) TO
               TOT-QTY(1 WK-CLASS TP) TOT-QTY(1 101 TP).

           ADD WK-AMT(TP) TO
               TOT-AMT(1 WK-CLASS TP) TOT-AMT(1 101 TP).

           ADD WK-BEG(TP) TO
               TOT-BEG(1 WK-CLASS TP) TOT-BEG(1 101 TP).

           ADD WK-END(TP) TO
               TOT-END(1 WK-CLASS TP) TOT-END(1 101 TP).

           IF SUMMARY-ONLY = "Y"
              MOVE WK-QTY(TP) TO D-QTY(TP)
              MOVE WK-AMT(TP) TO D-AMT(TP)
              MOVE WK-BEG(TP) TO D-BEG(TP)
              MOVE WK-END(TP) TO D-END(TP).

           IF TP < 3
              GO TO NEXT-WK-ACCUM.

           IF SUMMARY-ONLY = "Y"
              PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-WK-READ.

      *-----------------------------------------------------------------
       PRINT-REPORT-EXIT.
           IF FIRST-TIME = "N"
              PERFORM BR-TOTALS.
      ******************************************************************
      *
      *    P R I N T   D E T A I L S
      *
      ******************************************************************
       PRINT-DETAIL SECTION.
           CLOSE WK-FILE.
           PERFORM OPEN-WK-FILE.
           MOVE SPACES TO WK-KEY.
           MOVE 0      TO WK-BRNO WK-CLASS WK-AC.
           PERFORM START-WK-FILE.

       PRINT-DETAIL-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO PRINT-DETAIL-EXIT.

           UNLOCK WK-FILE.
           IF WK-AC = 0
              GO TO PRINT-DETAIL-NEXT.

      * NEEDED BECAUSE WHEN BR-NO > END-BRANCH, YOU MIGHT GO INTO HERE
      * AND YOU WILL HAVE THE WRONG BRANCH FOR THE DETAIL YOU ARE PRINTING
      * WHICH MAY CAUSE BAD HEADINGS IN H-CL-METHOD.

           IF HOLD-BR = 0
              MOVE WK-BRNO TO HOLD-BR BR-NO
              PERFORM GET-BR
           ELSE
              IF HOLD-BR NOT = WK-BRNO
                 MOVE WK-BRNO TO BR-NO HOLD-BR
                 PERFORM GET-BR.

           MOVE WK-AC TO D-AC.
           MOVE WK-BRNO TO D-BR.
           MOVE WK-CLASS TO D-LC.
           MOVE 0 TO XUB.

       PRINT-DETAIL-INS.
           ADD 1 TO XUB.
           MOVE WK-AMT(XUB) TO D-AMT(XUB).
           MOVE WK-BEG(XUB) TO D-BEG(XUB).
           MOVE WK-END(XUB) TO D-END(XUB).

           IF XUB < 3
              GO TO PRINT-DETAIL-INS.

           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-DETAIL-NEXT.

       PRINT-DETAIL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    B R A N C H    T O T A L S
      *
      ******************************************************************
       BR-TOTALS SECTION.
           MOVE 1 TO LV.
           PERFORM TOTAL-PRINT.
           MOVE 2 TO LN-FEED.
           MOVE WK-BRNO TO LAST-BR.

      ******************************************************************
      *
      *    P R I N T   T O T A L   L I N E
      *
      ******************************************************************
       TOTAL-PRINT SECTION.
           IF SUMMARY-ONLY = "Y"
              MOVE "TOTAL" TO DETAIL-LINE
              MOVE ":" TO D-COLON.

           MOVE 0 TO TP.

       NEXT-TOT-PRINT.
           ADD 1 TO TP.
           MOVE TOT-QTY(LV 101 TP) TO D-QTY(TP).
           MOVE TOT-AMT(LV 101 TP) TO D-AMT(TP).
           MOVE TOT-BEG(LV 101 TP) TO D-BEG(TP).
           MOVE TOT-END(LV 101 TP) TO D-END(TP).

           MOVE 0 TO TOT-QTY(LV 101 TP) TOT-AMT(LV 101 TP)
              TOT-BEG(LV 101 TP) TOT-END(LV 101 TP).

           IF TP < 3
              GO TO NEXT-TOT-PRINT.

           PERFORM WRITE-DETAIL-LINE.

       TOTAL-PRINT-EXIT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *   C L E A R   T O T A L   T A B L E
      *
      ******************************************************************
       CLEAR-TOTALS SECTION.
           MOVE 0 TO TP.

       CLEAR-NEXT-TYPE.
           ADD 1 TO TP.
           MOVE 0 TO TOT-QTY(1 1 TP).
           MOVE 0 TO TOT-AMT(1 1 TP).
           MOVE 0 TO TOT-BEG(1 1 TP).
           MOVE 0 TO TOT-END(1 1 TP).

           IF TP < 3
              GO TO CLEAR-NEXT-TYPE.

           MOVE 1 TO LC.

       CLEAR-NEXT-CLASS.
           ADD 1 TO LC.
           MOVE TOT-LC(1 1) TO TOT-LC(1 LC).

           IF LC < 101
              GO TO CLEAR-NEXT-CLASS.

           IF LV = 2
              MOVE TOT-LV(1) TO TOT-LV(2).

      ******************************************************************
      *
      *    C H E C K   U N E A R N E D
      *
      ******************************************************************
       CHECK-UNEARNED SECTION.

      * THIS CLEAR IS NECESSARY FOR TEST IN COMPUTE-BEG-UNEARNED:

           MOVE " " TO METHOD-4-FG
                       METHOD-5-FG.

           MOVE 0 TO XUB.

       CHECK-UNEARNED-NEXT.
           ADD 1 TO XUB.

      * CLEAR WORKERS:
           MOVE 0 TO BEG-UNEARNED(XUB) END-UNEARNED(XUB)
                     NEW-FEEAMT(XUB) NEW-REFUNDS(XUB).

           MOVE "N" TO CHK-UNEARNED(XUB).
           PERFORM COMPUTE-BEG-END-UNEARNED.

       CHECK-UNEARNED-SKIP.
           IF XUB < 3
              GO TO CHECK-UNEARNED-NEXT.

      * IF ALL THREE CHK-UNEARNED FLAGS = 'N',
      *    THAN IT IS NOT NECESSARY TO SEARCH FOR ADDONS,
      *    ADJUSTMENTS, CANCELLATIONS OR REBATES:

           IF CHK-UNEARNED(1) = "N" AND
              CHK-UNEARNED(2) = "N" AND
              CHK-UNEARNED(3) = "N"
              GO TO EXIT-CHECK-UNEARNED.

       END-CHECK-UNEARNED.
           MOVE 0 TO XUB.

       CHECK-UNEARNED-BEG-ONLY.
           ADD 1 TO XUB.

           IF CHK-UNEARNED(XUB) = " "
              PERFORM COMPUTE-BEG-END-UNEARNED.

           IF XUB < 3
              GO TO CHECK-UNEARNED-BEG-ONLY.

       EXIT-CHECK-UNEARNED.
           EXIT.

      ******************************************************************
      *
      *    COMPUTE BEGINNING UNEARNED
      *
      ******************************************************************
       COMPUTE-BEG-UNEARNED SECTION.
           IF REBATE-METHOD(XUB) = 4
              IF METHOD-4-FG = " "
                 PERFORM FIND-METHOD-4-DATES
                 MOVE "Y" TO METHOD-4-FG.

           IF REBATE-METHOD(XUB) = 5
              IF METHOD-5-FG = " "
                 PERFORM FIND-METHOD-5-DATES
                 MOVE "Y" TO METHOD-5-FG.

      * TEST FOR NEW FEEAMT AND NEW VOIDED FEEAMT:

           IF LN-CONVERCD = " "
              MOVE LN-POFFDATE TO NUM-DATE
              MOVE LN-ENTDATE  TO SYS-DATE
      
      * TEST IF ACCOUNT PAIDOFF (NOT VOIDED) THE SAME MONTH AS BOOKED
      * AND LOAN WAS BOOKED THIS ACCOUNTING PERIOD
      * THEN ADD TO NEW FEEAMT
      
              IF (NUM-MO = S-MM AND NUM-YR = S-YY) AND
                 (LN-POCD-POFF) AND (NOT LN-POCD-TRANSFERRED)
                  IF LN-ENTDATE > PRIOR-ENDING-DATE
                     ADD LN-INSCOMM(XUB) TO NEW-FEEAMT(XUB)
                  END-IF
              ELSE
      
      * TEST IF ACCOUNT WAS NOT PAIDOFF OR VOIDED THE SAME MONTH
      * AS BOOKED
      
              IF NOT (NUM-MO = S-MM AND NUM-YR = S-YY)
      
      * TEST FOR ACCOUNT VOIDED BUT NOT IN THE SAME MONTH AS BOOKED
      * THAN SUBTRACT FROM NEW FEEAMT
      
                 IF LN-POCD-VOID
                    SUBTRACT FEEAMT(XUB) FROM NEW-FEEAMT(XUB)
                 ELSE
      
      * TEST IF ACCOUNT WAS NOT PAIDOFF OR VOIDED THE SAME MONTH
      * AS BOOKED AND LOAN WAS BOOKED THIS ACCOUNTING PERIOD
      * THEN ADD TO NEW FEES
      
                    IF LN-ENTDATE > PRIOR-ENDING-DATE
                       ADD FEEAMT(XUB) TO NEW-FEEAMT(XUB).

      
      * TEST FOR REBATE DATE PRIOR TO ENTRY-DATE:
      
           IF REBATE-METHOD(XUB) = 4
              MOVE METHOD-4-BEG-PAYDATE TO REB-PAYDATE
           ELSE
              IF REBATE-METHOD(XUB) = 5
                 MOVE METHOD-5-BEG-PAYMNTD TO PAYMNTD
              ELSE
                 MOVE PRIOR-ENDING-DATE TO REB-PAYDATE.

           IF LN-CONVERCD = " "
              MOVE PRIOR-ENDING-DATE TO NUM-DATE
              MOVE LN-ENTDATE        TO SYS-DATE
              PERFORM TIM365
           ELSE
              MOVE -1 TO ELAPSED-DAYS.

           IF ELAPSED-DAYS NOT > 0
              IF FEEAMT(XUB) NOT = 0
                 IF (REBATE-METHOD(XUB) = 4 OR 5) AND
                    (LN-DATE-PAID-LAST = 0)
                    ADD FEEAMT(XUB) TO BEG-UNEARNED(XUB)
                 ELSE
                    PERFORM UNEARNED-VIA-REBATE
                    COMPUTE BEG-UNEARNED(XUB) ROUNDED =
                       BEG-UNEARNED(XUB) + REB-REBATE.

      ******************************************************************
      *
      *    COMPUTE BEGINNING & ENDING UNEARNED
      *
      ******************************************************************
       COMPUTE-BEG-END-UNEARNED SECTION.
           PERFORM COMPUTE-BEG-UNEARNED.

           IF REBATE-METHOD(XUB) = 4
              MOVE METHOD-4-END-PAYDATE TO REB-PAYDATE
           ELSE
           IF REBATE-METHOD(XUB) = 5
              MOVE METHOD-5-END-PAYMNTD TO PAYMNTD
           ELSE
              MOVE REPORT-ENDING-DATE TO REB-PAYDATE.

           IF NOT LN-POCD-VOID
              IF FEEAMT(XUB) NOT = 0
                    IF (REBATE-METHOD(XUB) = 4 OR 5) AND
                       (LN-DATE-PAID-LAST = 0)
                       ADD FEEAMT(XUB) TO END-UNEARNED(XUB)
                    ELSE
                       PERFORM UNEARNED-VIA-REBATE
                       COMPUTE END-UNEARNED(XUB) ROUNDED =
                         END-UNEARNED(XUB) + REB-REBATE.

      ******************************************************************
      *
      *    WRITE DETAIL LINE
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
      *
      *    HEAD PAGE ROUTINE
      *
      ******************************************************************
       HEAD-PAGE SECTION.
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
      
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE FEE-CODE(1) TO HEAD4-FEE1.
           MOVE FEE-CODE(2) TO HEAD4-FEE2.
           MOVE FEE-CODE(3) TO HEAD4-FEE3.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD5 TO PR-REC
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD6 TO PR-REC
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE DASH-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

      
       END-HEAD-PAGE.
           MOVE 9 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           GO TO EXIT-HEAD-PAGE.
      
       EXIT-HEAD-PAGE.
           EXIT.

      ******************************************************************
      *
      *   R A N G E   L I N E S
      *
      ******************************************************************
       RANGE-LINES SECTION.
           MOVE 3 TO LN-FEED.
      
           MOVE "FEE CODE | EARN METHOD" TO RANGE-LINE.
           MOVE FEE-CODE(1)              TO RANGE-SEL.
           MOVE REBATE-METHOD(1) TO MESS.
           PERFORM GET-REBATE-METHOD.
           STRING REBATE-METHOD(1) "-" MESS
              DELIMITED BY SIZE INTO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

           IF FEE-CODE(2) NOT = " "
              MOVE FEE-CODE(2)              TO RANGE-SEL
              MOVE REBATE-METHOD(2) TO MESS
              PERFORM GET-REBATE-METHOD
              STRING REBATE-METHOD(2) "-" MESS
                 DELIMITED BY SIZE INTO RANGE-SEL2
              PERFORM WRITE-DETAIL-LINE.

           IF FEE-CODE(3) NOT = " "
              MOVE FEE-CODE(3)              TO RANGE-SEL
              MOVE REBATE-METHOD(3) TO MESS
              PERFORM GET-REBATE-METHOD
              STRING REBATE-METHOD(3) "-" MESS
                 DELIMITED BY SIZE INTO RANGE-SEL2
              PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE P&L ACCOUNTS ?" TO RANGE-LINE.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "MONTH AND YEAR" TO RANGE-LINE.
           MOVE MMYY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "LOAN ENTERED START DATE" TO RANGE-LINE.
           MOVE ENTERED-START-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ACCOUNT DETAIL" TO RANGE-LINE.
           MOVE ACCOUNT-DETAIL TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ZERO BALANCE ONLY ?" TO RANGE-LINE.
           MOVE ZBAL-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE P&L ACCOUNTS ?" TO RANGE-LINE.
           MOVE PL TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.
      
      ******************************************************************
      *
      *    COMPUTE UNEARNED VIA REBATE
      *
      ******************************************************************
       UNEARNED-VIA-REBATE SECTION.
           MOVE 0 TO REB-REBATE.

      * REMOVE REFERENCE TO INSURANCE SET TO INTEREST
      
           MOVE 7 TO REB-SUB.

           MOVE LN-ORGTERM  TO REB-ORGTERM.
           MOVE FEEAMT(XUB) TO REB-TOTCHG.

      * 5 = RULE OF 78THS PRINCIPLE REDUCTION:

           IF REBATE-METHOD(XUB) = 5
              PERFORM METHOD-5-CALCULATION
              GO TO END-UNEARNED-VIA-REBATE.

      * 7 = ACTUARIAL 52:
      
           IF REBATE-METHOD(XUB) = 7
              PERFORM METHOD-7-CALCULATION
              GO TO END-UNEARNED-VIA-REBATE.

      * 1 OR 4 OR 6 = RULE OF 78THS FIXED TO THE FOLLOWING FORMULATION:
      
           IF REBATE-METHOD(XUB) = 1 OR 4 OR 6
              MOVE 1 TO SP-RBFRMLA(REB-SUB)
              MOVE "B" TO SP-RBDTE(REB-SUB 1)
                          SP-RBDTE(REB-SUB 2)
              MOVE 360 TO SP-RBYRTYPE(REB-SUB 1)
                          SP-RBYRTYPE(REB-SUB 2)
              MOVE 0 TO SP-RBMINRET(REB-SUB)
              MOVE 0 TO SP-RBNOMON(REB-SUB)
              MOVE 0 TO SP-RBDEDCHG(REB-SUB)
              MOVE 0 TO SP-RBRECAST(REB-SUB)
              MOVE 0 TO SP-RBEARLY(REB-SUB)
              MOVE 0 TO SP-RBDEFER(REB-SUB)
              MOVE 100 TO SP-RBPCT(REB-SUB)
              MOVE 0 TO SP-RBREDUC(REB-SUB)
              MOVE 0 TO SP-RBMIN(REB-SUB)
              MOVE 0 TO SP-RBSPOPT1(REB-SUB)
              MOVE 0 TO SP-RBSPOPT2(REB-SUB)
              MOVE 0 TO SP-RBMANAGER(REB-SUB)
              MOVE REBDAYS-MONTH(XUB) TO SP-RBDAYS(REB-SUB 1)
                                         SP-RBDAYS(REB-SUB 2).


      * 2 = PRO RATA FIXED TO THE FOLLOWING FORMULATION:
      
           IF REBATE-METHOD(XUB) = 2
              MOVE 5 TO SP-RBFRMLA(REB-SUB)
              MOVE "B" TO SP-RBDTE(REB-SUB 1)
                          SP-RBDTE(REB-SUB 2)
              MOVE 360 TO SP-RBYRTYPE(REB-SUB 1)
                          SP-RBYRTYPE(REB-SUB 2)
              MOVE 0 TO SP-RBMINRET(REB-SUB)
              MOVE 0 TO SP-RBNOMON(REB-SUB)
              MOVE 0 TO SP-RBDEDCHG(REB-SUB)
              MOVE 0 TO SP-RBRECAST(REB-SUB)
              MOVE 0 TO SP-RBEARLY(REB-SUB)
              MOVE 0 TO SP-RBDEFER(REB-SUB)
              MOVE 100 TO SP-RBPCT(REB-SUB)
              MOVE 0 TO SP-RBREDUC(REB-SUB)
              MOVE 0 TO SP-RBMIN(REB-SUB)
              MOVE 0 TO SP-RBSPOPT1(REB-SUB)
              MOVE 0 TO SP-RBSPOPT2(REB-SUB)
              MOVE 0 TO SP-RBMANAGER(REB-SUB)
              MOVE REBDAYS-MONTH(XUB) TO SP-RBDAYS(REB-SUB 1)
                                         SP-RBDAYS(REB-SUB 2).

      * 3 = REFUND STANDARD METHOD:
      *     USE STATE PARAMETERS FOR DETAIL TO REBATE

      * SECURITY FINANCE, EARN 25% UPFRONT:
           IF REBATE-METHOD(XUB) = 9
              COMPUTE REB-TOTCHG ROUNDED = REB-TOTCHG * .75.

           PERFORM REBATE.

      * 6 = 78THS / PRORATA AVERAGE METHOD:

           IF REBATE-METHOD(XUB) = 6
              COMPUTE REB-REBATE ROUNDED =
                 REB-REBATE / 2 * (1 + (REB-ORGTERM + 1)
                                          / (REB-REMTERM + 1)
                                  ).

       END-UNEARNED-VIA-REBATE.
           EXIT.

      ******************************************************************
      *    METHOD 5 CALCULATION
      *    RE: PRINCIPLE REDUCTION
      ******************************************************************
       METHOD-5-CALCULATION SECTION.

      * DETERMINE NO. OF WHOLE AND FRACTIONAL PAYMENTS:
           MOVE LN-TOTPAYMNTD TO SAVE-LN-TOTPAYMNTD.
           MOVE PAYMNTD TO LN-TOTPAYMNTD.
           MOVE LN-ORIG-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE SAVE-LN-TOTPAYMNTD TO LN-TOTPAYMNTD.

           COMPUTE UNRP-WORKER =
              (REB-ORGTERM * (REB-ORGTERM + 1)) / 2.

           IF UNRP-WORKER NOT = 0
              COMPUTE UNRP-WORKER ROUNDED =
                  REB-TOTCHG  / UNRP-WORKER.

      * SETUP SUM OF WHOLE REMAINING PAYMENTS:
           COMPUTE UNRP-SUMW =
              ((REB-ORGTERM - PDTH-PAYS) *
                    (REB-ORGTERM - PDTH-PAYS + 1)) / 2.

      * SETUP SUM OF WHOLE REMAINING PAYMENTS PLUS 1:
           COMPUTE UNRP-SUMW1 =
              ((REB-ORGTERM - PDTH-PAYS - 1) *
                    (REB-ORGTERM - PDTH-PAYS)) / 2.

      * COMPUTE UNEARNED THRU THIS PAYMENT:
           COMPUTE REB-REBATE ROUNDED = UNRP-WORKER *
              (UNRP-SUMW + PDTH-REMAIN * (UNRP-SUMW1 - UNRP-SUMW)).

       METHOD-5-CALCULATION-EXIT.
           EXIT.

      ****************************************
      *    METHOD 7 CALCULATION
      *    RE: ACTUARIAL ACCRUAL
      ****************************************
       METHOD-7-CALCULATION SECTION.
           MOVE REB-PAYDATE    TO REGZ-DATE.
           MOVE LN-LOANDATE    TO REGZ-LOANDATE.
           MOVE LN-INTDATE     TO REGZ-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO REGZ-1STPYDATE.
           MOVE LN-ORGTERM     TO REGZ-ORGTERM.
           MOVE LN-1STPYAMT    TO REGZ-1STPYAMT.
           MOVE LN-REGPYAMT    TO REGZ-REGPYAMT.
           MOVE LN-LASTPYAMT   TO REGZ-LASTPYAMT.
           MOVE LN-FINCHG      TO REGZ-FINCHG.
           MOVE LN-APRATE      TO REGZ-APRATE.
           MOVE "A"            TO REGZ-DISFRMLA.
           MOVE "N"            TO REGZ-INCLUDE-DEF-FG.
           MOVE LN-TOTNODEF    TO REGZ-TOTNODEF.

           PERFORM REGZ-UNEARNED-CALC.

           COMPUTE REB-REBATE ROUNDED =
               REGZ-UNEARNED * FEEAMT(XUB) / LN-FINCHG.

       METHOD-7-CALCULATION-EXIT.
           EXIT.


      ******************************************************************
      *
      *    FIND METHOD 4 (CASH METHOD RE: INTEREST) REBATE
      *
      ******************************************************************
       FIND-METHOD-4-DATES SECTION.
           MOVE LN-ENTDATE TO METHOD-4-BEG-PAYDATE
                              METHOD-4-END-PAYDATE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

      *-----------------------------------------------------------------
       FIND-METHOD-4-DATES-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO FIND-METHOD-4-DATES-END.

           IF LP-SSNO = 999999999
              GO TO FIND-METHOD-4-DATES-NEXT.

           IF LP-TRCD = "RV"
              IF RV-CNT < 30
                 ADD 1 TO RV-CNT
                 MOVE LP-TRDATE TO NUM-DATE
                 MOVE NUM-MO TO RV-MO(RV-CNT)
                 MOVE NUM-YR TO RV-YR(RV-CNT)
                 GO TO FIND-METHOD-4-DATES-NEXT
              ELSE
                 MOVE LN-ACCTNO TO PROCESS-NUMBER
                 MOVE PROCESS-ERROR TO MESS
                 PERFORM SEND-MESS
                 GO TO FIND-METHOD-4-DATES-END.

           IF LP-REV NOT = "Y"
              IF RV-CNT = 0
                 GO TO FIND-METHOD-4-DATES-NO-RV.

           IF RV-CNT = 0
              MOVE LN-ACCTNO TO PROCESS-NUMBER
              MOVE PROCESS-ERROR TO MESS
              PERFORM SEND-MESS
              GO TO FIND-METHOD-4-DATES-END.

           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "PO" OR "PB" OR "SC"
                                     OR "RN" OR "RB" OR "RO"
                                      OR "AH" OR "PZ" OR "PN"
                                       OR "IN")
              SUBTRACT 1 FROM RV-CNT
              GO TO FIND-METHOD-4-DATES-NEXT.

           MOVE LP-TRDATE TO NUM-DATE.

           IF NUM-MO = RV-MO(RV-CNT)
              IF NUM-YR = RV-YR(RV-CNT)
                 SUBTRACT 1 FROM RV-CNT
                 GO TO FIND-METHOD-4-DATES-NEXT.

           SUBTRACT 1 FROM RV-CNT.

           IF METHOD-4-BEG-PAYDATE NOT = LN-ENTDATE
              GO TO FIND-METHOD-4-DATES-NEXT.

           IF PRIOR-ENDING-DATE >= LP-PAYDATE
              MOVE LP-PAYDATE TO METHOD-4-BEG-PAYDATE.

           GO TO FIND-METHOD-4-DATES-NEXT.

      *-----------------------------------------------------------------
      * TEST FOR ACTIVITY THIS MONTH:
      *-----------------------------------------------------------------
       FIND-METHOD-4-DATES-NO-RV.
           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "BF" OR "PO" OR "PB"
                                     OR "SC" OR "RN" OR "RB"
                                      OR "RO" OR "AH" OR "PZ"
                                       OR "PN" OR "IN")
              GO TO FIND-METHOD-4-DATES-NEXT.

           IF METHOD-4-END-PAYDATE = LN-ENTDATE
              MOVE LP-PAYDATE TO METHOD-4-END-PAYDATE.

           IF PRIOR-ENDING-DATE < LP-PAYDATE
              GO TO FIND-METHOD-4-DATES-NEXT.

           MOVE LP-PAYDATE TO METHOD-4-BEG-PAYDATE.

       FIND-METHOD-4-DATES-END.
           EXIT.

      ******************************************************************
      *
      *    FIND METHOD 5 (CASH METHOD RE: INTEREST) REBATE
      *
      ******************************************************************
       FIND-METHOD-5-DATES SECTION.
           MOVE LN-TOTPAYMNTD TO METHOD-5-BEG-PAYMNTD
                                 METHOD-5-END-PAYMNTD.
           IF SP-CONTRFRMLA = "A"
              SUBTRACT LN-TOTLCHG FROM METHOD-5-BEG-PAYMNTD
                                       METHOD-5-END-PAYMNTD.

           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE 99999     TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

      *-----------------------------------------------------------------
       FIND-METHOD-5-DATES-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LN-OWNBR) OR
                         (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO FIND-METHOD-5-DATES-END.

           IF LP-SSNO = 999999999
              GO TO FIND-METHOD-5-DATES-NEXT.

           IF LP-TRCD = "RV"
              IF RV-CNT < 30
                 ADD 1 TO RV-CNT
                 MOVE LP-TRDATE TO RV-DATE(RV-CNT)
                 GO TO FIND-METHOD-5-DATES-NEXT
              ELSE
                 MOVE LN-ACCTNO TO PROCESS-NUMBER
                 MOVE PROCESS-ERROR TO MESS
                 PERFORM SEND-MESS
                 GO TO FIND-METHOD-5-DATES-END.

           IF LP-REV NOT = "Y"
              IF RV-CNT = 0
                 GO TO FIND-METHOD-5-DATES-NO-RV.

           IF RV-CNT = 0
              MOVE LN-ACCTNO TO PROCESS-NUMBER
              MOVE PROCESS-ERROR TO MESS
              PERFORM SEND-MESS
              GO TO FIND-METHOD-5-DATES-END.

           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "PO" OR "PB" OR "SC"
                                     OR "RN" OR "RB" OR "RO"
                                      OR "AH" OR "PZ" OR "PN"
                                       OR "IN")
              SUBTRACT 1 FROM RV-CNT
              GO TO FIND-METHOD-5-DATES-NEXT.

           MOVE LP-TRDATE TO NUM-DATE.

           IF NUM-MO = RV-MO(RV-CNT)
              IF NUM-YR = RV-YR(RV-CNT)
                 SUBTRACT 1 FROM RV-CNT
                 GO TO FIND-METHOD-5-DATES-NEXT.


           IF RV-DATE(RV-CNT) > PRIOR-ENDING-DATE
              ADD LP-APCUR TO METHOD-5-BEG-PAYMNTD
              ADD LP-APINT TO METHOD-5-BEG-PAYMNTD.

           SUBTRACT 1 FROM RV-CNT.
           GO TO FIND-METHOD-5-DATES-NEXT.

      *-----------------------------------------------------------------
      * TEST FOR ACTIVITY THIS MONTH:
      *-----------------------------------------------------------------
       FIND-METHOD-5-DATES-NO-RV.
           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "PO" OR "PB" OR "SC"
                                     OR "RN" OR "RB" OR "RO"
                                      OR "AH" OR "PN" OR "PZ"
                                       OR "IN")
              GO TO FIND-METHOD-5-DATES-NEXT.

           IF LP-TRDATE > PRIOR-ENDING-DATE
              SUBTRACT LP-APCUR FROM METHOD-5-BEG-PAYMNTD
              SUBTRACT LP-APINT FROM METHOD-5-BEG-PAYMNTD
              GO TO FIND-METHOD-5-DATES-NEXT.

       FIND-METHOD-5-DATES-END.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPREGZ.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO FEUNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FEUNRP-SYSDATE.
           DISPLAY FEUNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FEUNRP-FORM-FIELDS TO WR-BUF.
           MOVE FEUNRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE FEUNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE FEUNRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/FEUNRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    M I S C   P A R A G R A P H S
      *
      ******************************************************************
       MISC-PARA SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       GET-REBATE-METHOD.
           IF MESS = 01
              MOVE "RULE OF 78THS" TO MESS
           ELSE
           IF MESS = 02
              MOVE "PRO RATA" TO MESS
           ELSE
           IF MESS = 03
              MOVE "REFUND METHOD (SPR)" TO MESS
           ELSE
           IF MESS = 04
              MOVE "RULE 78THS CASH-7" TO MESS
           ELSE
           IF MESS = 05
              MOVE "RULE 78THS CASH-8" TO MESS
           ELSE
           IF MESS = 06
              MOVE "78THS/PRORATA AVG" TO MESS
           ELSE
           IF MESS = 07
              MOVE "ACTUARIAL ACCRUAL" TO MESS
           ELSE
              MOVE "25%UPFRONT,78TH" TO MESS.

      *-----------------------------------------------------------------
       BR-HEADER.
           MOVE LAST-BR TO BR-NO.
           PERFORM GET-BR.
           MOVE LAST-BR TO D-BRNO.
           MOVE BR-NAME TO D-BRNAME.
           MOVE ":" TO D-COLON.
      *-----------------------------------------------------------------
       GET-BR.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "* INVALID *" TO BR-NAME.

      ******************************************************************
      *
      *     I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *     E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE WK-PATH    TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY FEUNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
