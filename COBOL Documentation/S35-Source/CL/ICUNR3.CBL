       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ICUNR3.
       DATE-WRITTEN.    071095.
      ******************************************************************
      *   DIR : CL
      *   DESC: UNEARNED INSURANCE PREMIUM REPORT
      *
      *              OTHER 3 INSURANCE
      *              OTHER 4 INSURANCE
      *
      *   THIS REPORT PRODUCES THE FOLLOWING:
      *     1. THE TOTAL NO. OF ACCOUNTS WITH INSURANCE
      *        STILL INEFFECT.
      *     2. THE ENDING UNEARNED FOR THE PREVIOUS
      *        PERIOD ENDING.
      *     3. NEW PREMIUMS (COMM) AND  ADJUSTMENTS FOR THE PERIOD.
      *     4. THE ENDING UNEARNED FOR THIS PERIOD.
      *
      *   NOTE:
      *        THIS PROGRAM NEEDS MODIFICATION IF EVER,
      *        IT ATTEMPS TO UPDATE THE LN-FILE.
      *
      * REV:
      *  091395 KEC ADDED GROUP LOGIC
      *  092895 JTG FORCED $0.01 TO COVERAGE IF IT WAS 'ZERO' FOR
      *             'LPITRM' CALCULATION
      * KEC 013096 ADDED LOGIC TO PRINT A BREAKDOWN OF DEBITS AND
      *            CREDITS AND THE GL ACCOUNT NUMBER.
      * KEC 021596 CHANGED CALCULATION OF WS-NET-INCOME TO MINUS
      *            NEW-REFUNDS(SUBSCRIPT).
      * KEC 021596 REMOVED THE CHANGED MADE TO THE CALCULATION OF
      *            WS-NET-INCOME TO MINUS NEW-REFUNDS(SUBSCRIPT).  ALSO,
      *            ADDED NEW FLAG (WS-NET-INCOME-CALC) TO PREVENT ADDING
      *            TO WS-NET-INCOME TWICE.
      * JTG 071796 CHANGED TO WORK WITH UNIT PERIODS
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      * BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      * BAH 073097 ADDED REB-METHOD 7 AND 9 FROM A48
      *  CS 042798 ADDED RB,RO TO TEST FOR LP-TRCD (RE METHOD 4,5 REBATE)
      * JTG 070398 CHANGED TO CORRECTLY REFLECT VOIDED ACCOUNTS DONE THE MONTH
      *            REPORTED
      * JTG 110298 CORRECTED BOOKED & VOIDED ACCOUNTS DONE THE MONTH REPORTED
      * GLF 033099 ADD TRCD'S 'PN' AND 'PZ' FOR TFC
      * JTG 990604 CHANGED TO ADD TO NEW PREMIUMS WHEN LOAN WAS BOOKED AND
      *            PAIDOUT IN SAME MONTH AS REPORT MONTH
      * JTG 990616 ADDED 'ST' ADDON TEST
      * JTG 990618 ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC
      *            WHICH WILL ALLOW SELECTING BR-INS-ADDON 'Y' OR 'O'
      *            WHEN BR-REB-METHOD-.() = 4 OR 5
      * GLF 091599 ADD INCLUDE, SKIP, ONLY OF P&L ACCOUNTS PER JR XREL
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
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      * BLV 011004 SKIP LOANS BOOKED & VOIDED IN SAME MONTH, FOR EXAMPLE:
      *            LOAN BOOKED, ADDON INSURANCE POSTED, ADDON REVERSED,
      *            LOAN VOIDED.  THIS WILL GIVE READ ERROR ON LT BECAUSE VOID
      *            DELETES ALL LT'S, & HERE THE ADDON PMT TRIES TO READ THE LT.
      * JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      * JTG 021002 CORRECTED 'IF' WITH MISSING PERIOD        FIRST COASTAL FCFS
      *            CAUSING METHOD 5 TO TAKE EARNINGS ON OLD PAIDOUT ACCOUNTS
      *            BECAUSE IT WAS NOT GETTING THE DATE TEST AFTER THE 'IF'
      * BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *            LIKE "PE", REGACC PR# 0007
      * JTG 030305 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      * BAH 031212 CREATE AN EXTRACT FILE WITH GL ENTRIES FOR REGWAN PR# 2209
      * JTG 040621 CHANGED ICUNR(1,2,3).C UNEARNED INSURANCE REPORTS TO
      *            ACCUMULATE BOTH DEBIT & CREDIT AMOUNTS IN THE INTERNAL
      *            WORK RECORDS SO WHEN GLNO'S FOR UNEARNED AND INCOME ARE
      *            THE SAME, THEY WILL HOLD BOTH AMOUNTS AND PRINT AN EQUAL
      *            AMOUNT OF DB/CR'S.                               WORLD #J045
      * JTG 040726 CORRECTED BUG IN COMPUTE-BEG-UNEARNED SECTION
      *            MISSING END-IF, PROBLEM WHEN UNEARNED WAS CODED FORMULA 5
      *            ON PAGE 3 OF BRANCH RECORD, BEGINNING UNEARNED DISTORTED
      *                                                             REGMGM #J048
      * JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *            RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *            OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      * BAH 041130 FILES WERE NOT GETTING CLOSED AT END OF GROUP
      * BLF 050721 ADDED NEW OPTION TO PRINT OR SUPPRESS BRANCH TOTALS,
      *            WORLD PR# 413
      * BLF 050721 ALSO, FIX INS COMPANY TOTAL LINE PRINTING " *INVALID *" FOR
      *            INS COMPANY NAME (ICFILE NOT OPEN)
      * BLF 050802 REPORT WAS STILL PRINTING BRANCH/CLASS SUMMARY LINES WHEN
      *            YOU SELECTED "N" FOR PRINT BRANCH TOTALS.  ALSO, FIXED
      *            TO CLEAR DETAIL LINE ON CONDITIONAL PRINTS (SOME GARBAGE
      *            LEFT ON LINES.  WORLD PR# 413
      * BLF 050802 ADD PRINT-BR-TOTS TO RANGES, FIX CHANGE TO WORK IN
      *            EXISTING BATCHES. ALSO TRY TO SUPPRESS SOME BLANK PAGES
      *            PRINTING WITH JUST HEADINGS.  WORLD PR# 413
      * BAH 070208 TREAT UNWIND ACCOUNTS JUST LIKE A VOID, REGACC PL# 575
      *  CS 100104 WHEN COMPUTING UNEARNED VIA REBATE SHOULD NOT BE INCLUDING
      *            THE REBATE ON THE TAX AMOUNT(BR-FORCE-VALID-CITY-COUNTY =Y)
      *            ADDED TEST AFTER PERFORMING REBATE TO SUBTRACT THE TAX
      *            PORTION FROM THE REBATE   WORLD PR#666
      *   CS 151124 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      *   CS 160120 REMOVED BR-REB-METHOD-2 AND CHANGED TEST ON LEVEL LIFE
      *             TO BR-REB-METHOD(7)
      *   IN A30 
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *   CS 160121 REMOVED LTFILE, USED LTI, ADDED PRU_LNK TO LINKAGE
      *  BAH 170324 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0424 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 2018.07.02 ADDED ADDONSPRW AND SET ADDONSPR-SUB INSTEAD OF
      *                REB-SUB2. DONT SHARE REBATEW WORKERS ANYMORE!
      * BAH 181203 GLOBAL LNFILE
      * BAH 181221 GLOBAL LPFILE
      * BAH 2019.0308 CHANGED LP-ADDON TO USE LP-REFNO
      * BAH 2019.0918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2022.0106 INCREASED ALL TOTAL WORKERS AND EXPANDED PRINT LINE #1535
      * JKC 2022.0809 PR#1565 SECURITIZATION - ADDED FILE EXTRACTION
      *               ADDED EXPORT-FILE-YNO
      * BAH 2022.0803 HARD CODED START
      * MB  2023.0824 PERFORMANCE TUNING - ADDED STORED PROCEDURES
      *              ADDED STATE PARAMETER ARRAY PROCESSING
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

      * ICUNR3 EXPORT FILE
           SELECT ICUNR3-FILE ASSIGN OUTPUT ICUNR3-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
      *SELECT GLEXT-FILE ASSIGN GLEXT-PATH
      *       ORGANIZATION RELATIVE
      *       ACCESS DYNAMIC
      *       LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *       RELATIVE KEY GLEXT-KEY
      *       FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    I C U N R 3 - F I L E
      *
      ******************************************************************
       FD  ICUNR3-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  ICUNR3-REC.
           03  ICUNR3-BYTE             PIC X(01)     OCCURS 250 TIMES.

      ******************************************************************
      *
      *   SUMMARY TOTALS WORK RECORD
      *
      *  TOTAL RECS WILL HAVE 0 IN WK-CD & WK-AC
      *  DETAIL RECS WILL HAVE 1 IN WK-CD & ACCT # IN WK-AC
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CD     PIC 99       COMP.
               05  WK-IC     PIC 99       COMP.
               05  WK-ST     PIC XX.
               05  WK-BR     PIC 9(4)     COMP.
               05  WK-CLASS  PIC 9(3).
               05  WK-AC     PIC 9(8)     COMP.
           03  WK-QTY        PIC 9(7)     COMP-3 OCCURS 2.
           03  WK-AMT        PIC S9(9)V99 COMP-3 OCCURS 2.
           03  WK-BEG        PIC S9(9)V99 COMP-3 OCCURS 2.
           03  WK-END        PIC S9(9)V99 COMP-3 OCCURS 2.
           03  WK-ACTIONCD2  PIC X(4).

      ******************************************************************
      *    FOR TOTALS FOR GI-UPDATE
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-GI-BR-ACCOUNT        PIC 9(11).
               05  FILLER REDEFINES WK2-GI-BR-ACCOUNT.
                   07  WK2-GI-BRNO      PIC 9(4).
                   07  WK2-GI-GLNO      PIC 9(7).
           03  WK2-NET-INCOME           PIC S9(9)V99.
           03  WK2-NET-UNEARNED         PIC S9(9)V99.
           03  WK2-CREDIT-DEBIT         PIC X(1).

      * REGWAN PR# 2209
      *FD  GLEXT-FILE
      *    LABEL RECORDS ARE STANDARD.
      * RSZ = 150
      *01  GLEXT-REC.
      *    03  GLEXT-ACCOUNT-NUMBER.
      *        05  GLEXT-ACCT-LAST-6     PIC 9(6).
      *        05  GLEXT-BRNO            PIC 9(4).
      *        05  FILLER REDEFINES GLEXT-BRNO.
      *            07  GLEXT-BRNO-LAST-2 PIC 9(2).
      *            07  GLEXT-ACCT-ZEROS  PIC 9(2).
      *    03  GLEXT-VOUCHER-NUMBER      PIC 9(8).
      *    03  GLEXT-TRAN-CODE           PIC 9(3).
      *    03  GLEXT-AMT                 PIC 9(10)V99.
      *    03  GLEXT-TRAN-DATE           PIC 9(6).
      *    03  GLEXT-VENDOR-NUMBER       PIC 9(4).
      *    03  GLEXT-RECURRING-FREQUENCY PIC 9(1).
      *    03  GLEXT-MAX-NUMBER-OF-TRANS PIC 9(4).
      *    03  GLEXT-ITEM-ADDRESS        PIC X(10).
      *    03  FILLER                    PIC X(12).
      *    03  GLEXT-DESC                PIC X(79).
      *    03  GLEXT-CR                  PIC X.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/ICUNR3.CBL S35 05/06/24-10:05:09 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
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
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
      *-----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-WBEG-IC                   PIC 99.
       01  PASS-WEND-IC                   PIC 99.
       01  PASS-WBEG-ST                   PIC XX.
       01  PASS-WEND-ST                   PIC XX.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LP1-CONN-OPEN-SW     PIC X        VALUE "N".
       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N". 

       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-PATH              PIC X(35).
       01  WK2-PATH             PIC X(35).
      *01  GLEXT-IFN            PIC X(5)     VALUE "GLEXT".
      *01  GLEXT-KEY            PIC 9(8)     VALUE 0.

      *=================================================================
      *    /USR/DATA/R#/FTP/ICUNR3.TXT
      *=================================================================
       01  ICUNR3-PATH                 PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES ICUNR3-PATH.
           03  FILLER                  PIC X(01).   
           03  ICUNR3-PATH-FIL         PIC X(39).   
       01  FILLER                      REDEFINES ICUNR3-PATH.
           03  ICUNR3-PATH-SLASH-1     PIC X(01).   
           03  ICUNR3-PATH-DIR-1       PIC X(03).   
           03  ICUNR3-PATH-SLASH-2     PIC X(01).   
           03  ICUNR3-PATH-DIR-2       PIC X(04).   
           03  ICUNR3-PATH-SLASH-3     PIC X(01).   
           03  ICUNR3-PATH-DIR-3       PIC X(02).   
           03  ICUNR3-PATH-SLASH-4     PIC X(01).   
           03  ICUNR3-PATH-DIR-4       PIC X(03).   
           03  ICUNR3-PATH-SLASH-5     PIC X(01).   
           03  ICUNR3-PATH-FILENAME    PIC X(22).   

       01  ICUNR3-DIR-FTP              PIC X(03)     VALUE "FTP".
       01  ICUNR3-EXT-TXT              PIC X(04)     VALUE ".TXT".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS LOGIC IS USED FOR DEBUG:
      *    IF YOU WANT TO EXAMINE THIS ROUTINE FOR EX. 03-005737
      *    YOU WILL SET BREAK-ACCTNO = 005737 SET A BREAK AT 
      *    THE 'IF' STATEMENT FOR BREAK-ACCTNO
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF LN-ACCTNO = BREAK-ACCTNO
      *       MOVE BREAK-ACCTNO TO BREAK-ACCTNO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  BREAK-ACCTNO                      PIC 9(8).

      ******************************************************************
      *
      *   I C U N R 3 - F I L E - W O R K E R S
      *
      *=================================================================
      * EDF = EDITED DISPLAY FIELDS (FOR PLACING INTO EXPORT RECORD)
      ******************************************************************
        01  EDF-D-AC                   PIC 9(8).
        01  EDF-D-IC                   PIC 9(2).
        01  EDF-D-BR                   PIC 9(4).
        01  EDF-D-AMT                  PIC 9(7).99-  OCCURS 2.
        01  EDF-D-BEG                  PIC 9(7).99   OCCURS 2.
        01  EDF-D-END                  PIC 9(7).99   OCCURS 2.

        01  EDF-SUB                    PIC 9(2).

      *-----------------------------------------------------------------
       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

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
                   07  TOT-QTY      PIC 9(7)     COMP-3 OCCURS 2.
                   07  TOT-AMT      PIC S9(9)V99 COMP-3 OCCURS 2.
                   07  TOT-BEG      PIC S9(9)V99 COMP-3 OCCURS 2.
                   07  TOT-END      PIC S9(9)V99 COMP-3 OCCURS 2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INS CO (1-99)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  IC                   PIC 9(6) COMP-3.
       01  SUB                  PIC 9(6) COMP-3.
       01  SUB-WK2              PIC 9(1).
       01  IDX                  PIC 9(6) COMP-3.
       01  IDY                  PIC 9(6) COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAN CLASS (1-101)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LC                   PIC 9(6) COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TOTAL LEVEL (ST IC)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LV                   PIC 9(6) COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INS TYPE (O3 O4)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TP                   PIC 9(6) COMP-3.
       01  TOTAL-DEBITS         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOTAL-CREDITS        PIC S9(9)V99 COMP-3 VALUE 0.

       01  HOLD-OWNBR           PIC 9(4) VALUE 9999.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  TOTAL-TYPE           PIC X(6)     VALUE SPACES.
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  COMM-SUB             PIC 999      COMP-3.
       01  SAVE-IC-NAME         PIC X(20).
       01  WK-IC-COMMRATE       PIC S9(3)V99  COMP-3.
       01  SAVE-WK-KEY          PIC X(13).
       01  SAVE-WK2-KEY         PIC 9(11).
       01  WK-COMMISSION        PIC S9(9)V99 COMP-3.
       01  REC-EXISTS           PIC X        VALUE "N".
       01  PROCESS-ERROR.
           03  FILLER           PIC X(28)    VALUE
               "ERROR PROCESSING LOAN NO:  ".
           03  PROCESS-NUMBER   PIC 9(8).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    METHOD-4 = OTHER INSURANCE 3 ( O3 )
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  METHOD-4-FG           PIC X.
       01  METHOD-4-BEG-PAYDATE PIC 9(8)     COMP-3.
       01  METHOD-4-END-PAYDATE PIC 9(8)     COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    METHOD-5 = OTHER INSURANCE 4 ( O4 )
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  METHOD-5-FG          PIC X.
       01  METHOD-5-BEG-PAYMNTD PIC 9(7)V99  COMP-3.
       01  METHOD-5-END-PAYMNTD PIC 9(7)V99  COMP-3.

       01  PAYMNTD              PIC 9(7)V99  COMP-3.
       01  SAVE-LN-TOTPAYMNTD   PIC 9(7)V99  COMP-3.
       01  HOLD-LP-ADDON.
           03  HOLD-LP-ADDON-TEXT  PIC X(3).
           03  HOLD-LP-ADDON-TYPE  PIC X(2).

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
           03  FILLER           PIC X(24)    VALUE ALL "-".
           03  FILLER           PIC X(2)     VALUE SPACES.
           03  FILLER           PIC X(42)    VALUE ALL "-".
           03  FILLER           PIC X(4)     VALUE SPACES.
           03  FILLER           PIC X(42)    VALUE ALL "-".

       01  LAST-IC              PIC 99       COMP-3 VALUE 0.
       01  LAST-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  LAST-ST              PIC XX            VALUE " ".

       01  UNEARNED-TAB.
           03  BEG-UNEARNED     PIC S9(9)V99 OCCURS 2.
           03  NEW-PREMIUMS     PIC S9(9)V99 OCCURS 2.
           03  NEW-REFUNDS      PIC S9(9)V99 OCCURS 2.
           03  END-UNEARNED     PIC S9(9)V99 OCCURS 2.
           03  INS-INEFFECT     PIC X        OCCURS 2.
           03  CHK-UNEARNED     PIC X        OCCURS 2.

       01  HOLD-LN-INSCODES     PIC XXX.
       01  HOLD-LN-O3-REBATE    PIC X.
       01  HOLD-LN-O4-REBATE    PIC X.
       01  HOLD-LN-INSTBL-01.
           03  HOLD-LN-INSTBL       OCCURS 2 TIMES
                                    PIC X(LN-INSTBL-SIZE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    HOLD-GI-BR-ACCOUNT(1) = CREDIT OTHER 3 INS.
      *    HOLD-GI-BR-ACCOUNT(2) = DEBIT  OTHER 3 INS.
      *    HOLD-GI-BR-ACCOUNT(3) = CREDIT OTHER 4 INS.
      *    HOLD-GI-BR-ACCOUNT(4) = DEBIT  OTHER 4 INS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  HOLD-GI-BR-ACCOUNTNO.
           03  HOLD-GI-BR-ACCOUNT PIC 9(11)        OCCURS 4 TIMES.

       01  HOLD-WK2-BRANCH      PIC 9(4).
       01  HOLD-WK2-CLASS       PIC 9(3).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WS-NET-INCOME(1)   = OTHER 3 INS.
      *    WS-NET-INCOME(2)   = OTHER 4 INS.
      *    WS-NET-INCOME-CALC = TO PREVENT ADDING TWICE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WS-FIELDS.
           03  WS-NET-INCOME        PIC S9(9)V99     OCCURS 2 TIMES.
           03  WS-NET-INCOME-FLAG   PIC X(1)         VALUE "N".
           03  WS-NET-INCOME-CALC   PIC X(1)         VALUE "N".
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
           03  HEAD2X           PIC X(40)    VALUE
           "   UNEARNED INSURANCE PREMIUM REPORT  ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(56) VALUE " ".
           03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(55) VALUE
           "                          ----------- OTHER 3 INSURANCE".
           03  FILLER           PIC X(47) VALUE
           " ------------    ------------ OTHER 4 INSURANCE".
           03  FILLER           PIC X(13) VALUE
           " ----------- ".

      *-----------------------------------------------------------------
      * BAH 1/07/2022 REMOVED BECAUSE WHEN LOOPING THRU BRANCH FILE, YOU
      * END UP WITH THE NEXT BRANCH, THEN GO TO END-ROUTINE AND PRINT
      * FROM THE WORK FILE, YOU DONT EVEN HAVE THE CORRECT BRANCHES INFO !
      *01  HEAD4B.
      *    03  FILLER           PIC X(39) VALUE SPACES.
      *    03  H-O3-METHOD      PIC X(13).
      *    03  FILLER           PIC X(34) VALUE SPACES.
      *    03  H-O4-METHOD      PIC X(13).


       01  HEAD5.
           03  FILLER           PIC X(56) VALUE
           "                           # INS      BEG       NEWPREM ".
           03  FILLER           PIC X(56) VALUE
           "      END         # INS     BEG      NEWPREM      END   ".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(56) VALUE
           "ACCOUNT #  CO ST BR   CL   ACCTS   UNEARNED     REFUNDS ".
           03  FILLER           PIC X(56) VALUE
           "    UNEARNED      ACCTS   UNEARNED   REFUNDS    UNEARNED".

      *-----------------------------------------------------------------
       01  HEAD6X               REDEFINES HEAD6.
           03  HEAD6X-ALT       PIC X(24).
           03  FILLER           PIC X(108).


      *-----------------------------------------------------------------
      *
      *    FOR WK2-FILE OUTPUT.
      *
      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER           PIC X(30) VALUE
           "GENERAL LEDGER NAME".
           03  FILLER           PIC X(57) VALUE
           "    ACCOUNT NUMBER    ----DEBIT---      ---CREDIT---".
      ******************************************************************
      *
      *    D E T A I L    L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-AC             PIC 9(8).
           03  FILLER           PIC X(3).
           03  D-IC             PIC 99.
           03  FILLER           PIC X.
           03  D-ST             PIC XX.
           03  FILLER           PIC X.
           03  D-BR             PIC 9(4).
           03  FILLER           PIC X.
           03  D-LC             PIC 99.
           03  FILLER           PIC X(110).

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC X.
           03  D-BRNAME         PIC X(13).
           03  D-COLON          PIC X.
           03  FILLER           PIC X.
           03  D-CLASS          PIC 99.
           03  FILLER           PIC XX.
           03  D-INS            OCCURS 2.
               05  D-QTY        PIC Z(7).
               05  FILLER       PIC X(1).
               05  D-BEG        PIC Z(7)9.99  BLANK ZERO.
               05  FILLER       PIC X(1).
               05  D-AMT        PIC Z(7)9.99- BLANK ZERO.
               05  FILLER       PIC X(1).
               05  D-END        PIC Z(7)9.99  BLANK ZERO.
               05  FILLER       PIC X(3).

      *-----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(9).
           03  D-STATE          PIC XX.
           03  FILLER           PIC X(121).

      *-----------------------------------------------------------------
       01  DETAIL4 REDEFINES DETAIL-LINE.
           03  D-GL-NAME        PIC X(30).
           03  FILLER           PIC X(5).
           03  D-GI-ACCT        PIC 9999/9999999   BLANK WHEN ZERO.
           03  FILLER           PIC X(5).
           03  D-NET-DEBIT      PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X(5).
           03  D-NET-CREDIT     PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X(78).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(25).
           03  RANGE-SEL        PIC X(107).
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       COPY "LIBCL/ICUNRPW.CPY".

       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ICUNRP_DEF.CPY".
       COPY "LIBLP/ICUNRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       01  PRU-PATH-X                PIC X(90).
       COPY "LIBGB/PRU_LNK.CPY".

       SCREEN SECTION.
       COPY "LIBLP/ICUNRP_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME PRU-PATH-X PRU-LINK EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE ICUNR3-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LP1-CONNECTION.
           CLOSE
               ICUNR3-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

      D    STOP MESS.
           PERFORM SQL-CONNECT.

       KEY-REPORT.
           MOVE PRU-PATH-X TO PRU-PATH.
      *    MOVE ICUNRP-QUEUE-POS TO Q-POS.
      *    MOVE ICUNRP-PRU-POS   TO P-POS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ICUNRP" TO VDU-FORM-NAME.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
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

           MOVE "B"                        TO IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              LX-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-IC1-FILE.
           PERFORM OPEN-LX1-FILE.
           PERFORM OPEN-LTI1-FILE.

           MOVE BR-NO TO LN-OWNBR.

       START-KEY-REPORT.
           MOVE LN-OWNBR TO QLN3-WBEG-OWNBR
                            QLN3-WEND-OWNBR.

           MOVE BEG-LC  TO LN-CLASS
                           QLN3-WBEG-CLASS.
           MOVE END-LC  TO QLN3-WEND-CLASS.

           MOVE 0       TO LN-ACCTNO
                           QLN3-WBEG-ACCTNO.
           MOVE ALL "9" TO QLN3-WEND-ACCTNO.

           MOVE BEG-IC  TO PASS-WBEG-IC.
           MOVE END-IC  TO PASS-WEND-IC.

           MOVE BEG-ST  TO PASS-WBEG-ST.
           MOVE END-ST  TO PASS-WEND-ST.

           IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM START-LN3-FILE.   *> SSP LN GI

      *-----------------------------------------------------------------
       NEXT-KEY-REPORT.
           PERFORM READ-LN3-FILE-NEXT.   *> EMBEDDED LN GI
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS LOGIC IS USED FOR DEBUG:
      *    IF YOU WANT TO EXAMINE THIS ROUTINE FOR EX. 03-005737
      *    YOU WILL SET BREAK-ACCTNO = 005737 SET A BREAK AT LINE
      *    BLINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-ACCTNO = BREAK-ACCTNO
              MOVE BREAK-ACCTNO TO BREAK-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BR-REB-METHOD-2( 1 & 2 ) = OTHER INSURANCE 3 & 4
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-OWNBR NOT = HOLD-OWNBR
              MOVE BR-REB-METHOD(6) TO REBATE-METHOD(1)
              MOVE BR-REB-METHOD(8) TO REBATE-METHOD(2)
              MOVE 0 TO REBATE-METHOD(3).

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

      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO NEXT-KEY-REPORT.

      *-----------------------------------------------------------------------
      * SKIP ACCOUNTS IN RESCESSION OR THAT RESCINDED (DID NOT MAKE THE LOAN)
      *-----------------------------------------------------------------------
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO NEXT-KEY-REPORT.

      *--------------------------------------------------------------
      * SKIP VOIDED LOANS THAT WERE NOT VOIDED THIS ACCOUNTING MONTH
      * UNWINDS SHOULD BE TREATED SAME AS VOIDS REGACC PL# 575
      *--------------------------------------------------------------
           IF LN-POCD-VOID OR LN-POCD-UNWIND
              MOVE LN-POFFDATE        TO NUM-DATE
              MOVE REPORT-ENDING-DATE TO SYS-DATE
              IF NOT (NUM-MO = S-MM AND NUM-YR = S-YY)
                 GO TO NEXT-KEY-REPORT.
      *--------------------------------------------------------------
      * SKIP VOIDED LOANS BOOKED & VOIDED IN THE SAME ACCOUNTING MONTH
      * UNWINDS SHOULD BE TREATED SAME AS VOIDS REGACC PL# 575
      *--------------------------------------------------------------
           IF LN-POCD-VOID OR LN-POCD-UNWIND
              MOVE LN-POFFDATE TO NUM-DATE
              MOVE LN-ENTDATE  TO SYS-DATE
              IF (NUM-MO = S-MM AND NUM-YR = S-YY)
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

      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-INSCOMP(6) < BEG-IC
              IF LN-INSCOMP(7) < BEG-IC
                 GO TO NEXT-KEY-REPORT.

      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-INSCOMP(6) > END-IC
              IF LN-INSCOMP(7) > END-IC
                 GO TO NEXT-KEY-REPORT.

           MOVE LN-OWNBR TO HOLD-OWNBR.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

      *     IF SP1-KEY NOT = LPWSSP-SP1-KEY
      *        MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
      *        IF IO-BAD
      *           GO TO IO-ERROR.

           MOVE SP1-KEY TO LPWSSP-SP1-KEY
           PERFORM ARRAYSP-READ.
           IF IO-BAD
             GO TO IO-ERROR.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE LN-INSPREM(6) TO LN-INSCOMM(6)
              MOVE LN-INSPREM(7) TO LN-INSCOMM(7).

           MOVE ZEROS TO RV-CNT RV-TABLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *    CREATE WK2-FILE AND READ GI-FILE ONCE PER BRANCH/CLASS.
      *
      *  GI-UNCL2; GI-UNAH; GI-UNPP; GI-UNO1; GI-UNO2; GI-UNO3; GI-UNO4
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( FIRST-TIME NOT = "Y" )
              IF ( HOLD-WK2-BRANCH NOT = LN-OWNBR ) OR
                 ( HOLD-WK2-CLASS  NOT = LN-CLASS )
                 PERFORM CREATE-WK2
                 MOVE LN-OWNBR TO HOLD-WK2-BRANCH
                 MOVE LN-CLASS TO HOLD-WK2-CLASS
                 PERFORM GET-GI
                 MOVE GI-UNO3 TO HOLD-GI-BR-ACCOUNT(1)
                 MOVE GI-O3   TO HOLD-GI-BR-ACCOUNT(2)
                 MOVE ZEROES  TO HOLD-GI-BR-ACCOUNT(3)
                 MOVE ZEROES  TO HOLD-GI-BR-ACCOUNT(4)
                 MOVE ZEROES  TO WS-NET-INCOME(1) WS-NET-INCOME(2)
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE LN-OWNBR TO HOLD-WK2-BRANCH
              MOVE LN-CLASS TO HOLD-WK2-CLASS
              PERFORM GET-GI
              MOVE GI-UNO3 TO HOLD-GI-BR-ACCOUNT(1)
              MOVE GI-O3   TO HOLD-GI-BR-ACCOUNT(2)
              MOVE ZEROES  TO HOLD-GI-BR-ACCOUNT(3)
              MOVE ZEROES  TO HOLD-GI-BR-ACCOUNT(4)
              MOVE ZEROES  TO WS-NET-INCOME(1) WS-NET-INCOME(2).

           PERFORM CREATE-TOTAL-RECS.

           GO TO NEXT-KEY-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTI1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS
           ELSE
              PERFORM PRINT-REPORT.

           PERFORM RANGE-LINES.

           MOVE "Y" TO WS-NET-INCOME-FLAG.
           PERFORM CREATE-WK2.
           PERFORM PRINT-TOTAL-WK2.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE "I" TO PL.

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-GI1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET ICUNR3-PATH BY GETTING CURRENT USERS MACHINE VIA FIL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
           CALL "C$TOLOWER"             USING ICUNR3-DIR-FTP, VALUE 3.
           CALL "C$TOLOWER"             USING ICUNR3-EXT-TXT, VALUE 4.

           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
              GO TO IO-ERROR.

           MOVE "/"                        TO ICUNR3-PATH-SLASH-1.
           MOVE GETENV-BUF                 TO ICUNR3-PATH-FIL.
           MOVE ICUNR3-DIR-FTP             TO ICUNR3-PATH-DIR-4.
           MOVE SPACES                     TO ICUNR3-PATH-SLASH-5.
           MOVE SPACES                     TO ICUNR3-PATH-FILENAME.
           MOVE ICUNR3-PATH                TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE SPACES                  TO MESS
                 STRING ICUNR3-PATH, " DOES NOT EXIST"
                        DELIMITED BY "  "   INTO MESS
                 PERFORM SEND-MESS
              END-IF
              GO TO END-INITIALIZE.

           MOVE "/"                        TO ICUNR3-PATH-SLASH-5.

           STRING "ICUNR3", ICUNR3-EXT-TXT
                  DELIMITED BY "  "      INTO ICUNR3-PATH-FILENAME.

           IF ( EXPORT-FILE-YNO = "Y" ) OR
              ( EXPORT-FILE-YNO = "O" )
              PERFORM OPEN-ICUNR3-FILE-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS....." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM ARRAYSP-LOAD.

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
           MOVE 5 TO IDX.

       CREATE-TOTAL-RECS-NEXT.
           ADD 1 TO IC IDX.

           IF LN-INSCOMP(IDX) < BEG-IC OR LN-INSCOMP(IDX) > END-IC
              GO TO CREATE-TOTAL-RECS-CHECK-END.

      * EVEN IF NO UNEARNED REMAINS;
      * TEST TO SEE IF INSURANCE IS STILL INEFFECT.
      * IF NOT INEFFECT, STILL REPORT IF THERE
      * WAS A BEGINNING UNEARNED:

           IF END-UNEARNED(IC) = 0
              MOVE IDX TO ITRM-SUB
              MOVE REPORT-ENDING-DATE TO ITRM-TODAY
              IF LN-INSCOVR(IDX) = 0
                 MOVE 0.01 TO LN-INSCOVR(IDX)
              END-IF
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO INS-INEFFECT(IC)
               ELSE
                 IF BEG-UNEARNED(IC) = 0
                    IF NEW-PREMIUMS(IC) = 0
                       IF NEW-REFUNDS(IC) = 0
                          GO TO CREATE-TOTAL-RECS-CHECK-END.

           MOVE 0 TO WK-AC WK-CD.
           MOVE LN-INSCOMP(IDX) TO WK-IC.
           MOVE LN-ORGST TO WK-ST.
           MOVE LN-OWNBR TO WK-BR.
           MOVE LN-CLASS TO WK-CLASS.
           MOVE "Y" TO WS-NET-INCOME-CALC.
           PERFORM CREATE-WK.
           MOVE "N" TO WS-NET-INCOME-CALC.

           IF ACCOUNT-DETAIL = "Y"
              MOVE 1 TO WK-CD
              MOVE LN-ACCTNO TO WK-AC
              PERFORM CREATE-WK.

           MOVE "N" TO FIRST-TIME.

       CREATE-TOTAL-RECS-CHECK-END.
           IF IC < 2
              GO TO CREATE-TOTAL-RECS-NEXT.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************

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

           PERFORM VARYING TP FROM 1 BY 1 UNTIL TP > 2
              MOVE 0 TO WK-QTY(TP)
                        WK-AMT(TP)
                        WK-BEG(TP)
                        WK-END(TP)
           END-PERFORM.

           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

       UPDATE-WK-REC.
           ADD BEG-UNEARNED(IC) TO WK-BEG(IC).

           IF END-UNEARNED(IC) NOT = 0
                 OR INS-INEFFECT(IC) = "Y"
                  OR NEW-PREMIUMS(IC) NOT = 0
                   OR NEW-REFUNDS(IC) NOT = 0
              ADD 1 TO WK-QTY(IC)
              ADD NEW-PREMIUMS(IC) TO WK-AMT(IC)
              ADD NEW-REFUNDS(IC) TO WK-AMT(IC)
              ADD END-UNEARNED(IC) TO WK-END(IC).

           MOVE LN-ACTIONCD2 TO WK-ACTIONCD2.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.

           IF ( WS-NET-INCOME-CALC = "Y" )
              COMPUTE WS-NET-INCOME(IC) = WS-NET-INCOME(IC) +
                                          BEG-UNEARNED(IC)  +
                                          NEW-PREMIUMS(IC)  +
                                          NEW-REFUNDS(IC)   -
                                          END-UNEARNED(IC).

      ******************************************************************
      * DESC: INSERT RECORD INTO ICUNR3-FILE USING CURRENT DATA.
      ******************************************************************
       UPDATE-ICUNR3-FILE SECTION.
      ******************************************************************

      * SET EDITED DISPLAY FIELDS (EDF) FOR ICUNR3-REC

           MOVE WK-AC                      TO EDF-D-AC.
           MOVE WK-IC                      TO EDF-D-IC.
           MOVE WK-BR                      TO EDF-D-BR.

           PERFORM VARYING EDF-SUB FROM 1 BY 1 UNTIL EDF-SUB > 2
              MOVE WK-AMT(EDF-SUB)         TO EDF-D-AMT(EDF-SUB)
              MOVE WK-BEG(EDF-SUB)         TO EDF-D-BEG(EDF-SUB)
              MOVE WK-END(EDF-SUB)         TO EDF-D-END(EDF-SUB)
           END-PERFORM.

           MOVE SPACES                     TO ICUNR3-REC.

           STRING EDF-D-AC,                   HEX-TAB,
                  EDF-D-IC,                   HEX-TAB,
                  WK-ST,                      HEX-TAB,
                  EDF-D-BR,                   HEX-TAB,
                  WK-CLASS,                   HEX-TAB,
                  EDF-D-BEG(1),               HEX-TAB,
                  EDF-D-AMT(1),               HEX-TAB,
                  EDF-D-END(1),               HEX-TAB,
                  EDF-D-BEG(2),               HEX-TAB,
                  EDF-D-AMT(2),               HEX-TAB,
                  EDF-D-END(2),               HEX-TAB,
                  WK-ACTIONCD2,               HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE      INTO ICUNR3-REC.

           PERFORM WRITE-ICUNR3-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       UPDATE-ICUNR3-FILE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT SECTION.
      ******************************************************************

           IF ACCOUNT-DETAIL = "Y"
              PERFORM PRINT-DETAIL
              MOVE 99 TO LN-COUNT
              IF ( EXPORT-FILE-YNO = "O" )
                 GO TO PRINT-REPORT-EXIT.

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

           UNLOCK WK-FILE.

           IF WK-CD NOT = 0
              GO TO PRINT-REPORT-EXIT.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WK-IC TO LAST-IC
              MOVE WK-ST TO LAST-ST
              MOVE WK-BR TO LAST-BR
              PERFORM BR-HEADER.

           IF WK-IC NOT = LAST-IC
              PERFORM IC-TOTALS
              PERFORM BR-HEADER.

           IF WK-ST NOT = LAST-ST
              PERFORM STATE-TOTALS
              PERFORM BR-HEADER.

           IF WK-BR NOT = LAST-BR
              PERFORM BRANCH-TOTALS
              PERFORM BR-HEADER.

           IF SUMMARY-ONLY = "Y"
              MOVE WK-CLASS TO D-CLASS.

           MOVE 0 TO TP.

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

           IF TP < 2
              GO TO NEXT-WK-ACCUM.

           IF SUMMARY-ONLY = "Y"
              IF PRINT-BRANCH-TOTS NOT = "N"
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 MOVE SPACES TO DETAIL-LINE.

           GO TO NEXT-WK-READ.

       PRINT-REPORT-EXIT.
           PERFORM IC-TOTALS.

      ******************************************************************
       PRINT-DETAIL SECTION.
      ******************************************************************
           CLOSE WK-FILE.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-KEY.
           MOVE 1 TO WK-CD.
           MOVE 0 TO WK-IC WK-BR WK-CLASS WK-AC.
           PERFORM START-WK-FILE.

       PRINT-DETAIL-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO PRINT-DETAIL-EXIT.

           UNLOCK WK-FILE.
           MOVE WK-AC TO LN-ACCTNO.
           MOVE LN-ACCTNO TO D-AC.
           MOVE WK-IC TO D-IC.
           MOVE WK-ST TO D-ST.
           MOVE WK-BR TO D-BR.
           MOVE WK-CLASS TO D-LC.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              MOVE WK-AMT(SUB) TO D-AMT(SUB)
              MOVE WK-BEG(SUB) TO D-BEG(SUB)
              MOVE WK-END(SUB) TO D-END(SUB)
           END-PERFORM.

           IF ( EXPORT-FILE-YNO = "Y" )
              PERFORM UPDATE-ICUNR3-FILE
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF ( EXPORT-FILE-YNO = "N" )
              PERFORM WRITE-DETAIL-LINE
           ELSE
              PERFORM UPDATE-ICUNR3-FILE.

           GO TO PRINT-DETAIL-NEXT.

       PRINT-DETAIL-EXIT.
           EXIT.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE 1 TO LV.
           MOVE "BRANCH" TO TOTAL-TYPE.
           PERFORM TOTAL-PRINT.
           MOVE 2 TO LN-FEED.
           MOVE WK-BR TO LAST-BR.

      ******************************************************************
       STATE-TOTALS SECTION.
      ******************************************************************
           PERFORM BRANCH-TOTALS.
           PERFORM ST-HEADER.
           MOVE 0 TO LC.

       NEXT-ST-LC.
           ADD 1 TO LC.
           MOVE 1 TO LV.
           PERFORM CHECK-0.

           IF LV = 0
              GO TO NEXT-ST-CHECK.

           IF SUMMARY-ONLY = "Y"
              MOVE LC TO D-CLASS.

           MOVE 0 TO TP.

       NEXT-ST-TOT-PR.
           ADD 1 TO TP.
           ADD TOT-QTY(1 LC TP) TO
               TOT-QTY(2 LC TP) TOT-QTY(1 101 TP).

           ADD TOT-AMT(1 LC TP) TO
               TOT-AMT(2 LC TP) TOT-AMT(1 101 TP).

           ADD TOT-BEG(1 LC TP) TO
               TOT-BEG(2 LC TP) TOT-BEG(1 101 TP).

           ADD TOT-END(1 LC TP) TO
               TOT-END(2 LC TP) TOT-END(1 101 TP).

           IF SUMMARY-ONLY = "Y"
              MOVE TOT-QTY(1 LC TP) TO D-QTY(TP)
              MOVE TOT-AMT(1 LC TP) TO D-AMT(TP)
              MOVE TOT-BEG(1 LC TP) TO D-BEG(TP)
              MOVE TOT-END(1 LC TP) TO D-END(TP).

           IF TP < 2
              GO TO NEXT-ST-TOT-PR.

           IF SUMMARY-ONLY = "Y"
              PERFORM WRITE-DETAIL-LINE.

       NEXT-ST-CHECK.
           IF LC < 100
              GO TO NEXT-ST-LC.

           MOVE 1 TO LV.
           MOVE "STATE " TO TOTAL-TYPE.
           PERFORM TOTAL-PRINT.
           MOVE 1 TO LV.
           PERFORM CLEAR-TOTALS.

       ST-TOTALS-EXIT.
           MOVE WK-ST TO LAST-ST.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       IC-TOTALS SECTION.
      ******************************************************************
           PERFORM STATE-TOTALS.
           PERFORM IC-HEADER.
           MOVE 0 TO LC.

       NEXT-IC-LC.
           ADD 1 TO LC.
           MOVE 2 TO LV.
           PERFORM CHECK-0.

           IF LV = 0
              GO TO NEXT-IC-CHECK.

           IF SUMMARY-ONLY = "Y"
              MOVE LC TO D-CLASS.

           MOVE 0 TO TP.

       NEXT-IC-TOT-PR.
           ADD 1 TO TP.
           ADD TOT-QTY(2 LC TP) TO TOT-QTY(2 101 TP).
           ADD TOT-AMT(2 LC TP) TO TOT-AMT(2 101 TP).
           ADD TOT-BEG(2 LC TP) TO TOT-BEG(2 101 TP).
           ADD TOT-END(2 LC TP) TO TOT-END(2 101 TP).

           IF SUMMARY-ONLY = "Y"
              MOVE TOT-QTY(2 LC TP) TO D-QTY(TP)
              MOVE TOT-AMT(2 LC TP) TO D-AMT(TP)
              MOVE TOT-BEG(2 LC TP) TO D-BEG(TP)
              MOVE TOT-END(2 LC TP) TO D-END(TP).

           IF TP < 2
              GO TO NEXT-IC-TOT-PR.

           IF SUMMARY-ONLY = "Y"
              PERFORM WRITE-DETAIL-LINE.

       NEXT-IC-CHECK.
           IF LC < 100
              GO TO NEXT-IC-LC.

           MOVE 2 TO LV.
           MOVE "INS CO" TO TOTAL-TYPE.
           PERFORM TOTAL-PRINT.
           MOVE 2 TO LV.
           PERFORM CLEAR-TOTALS.

       IC-TOTALS-EXIT.
           MOVE WK-IC TO LAST-IC.
           MOVE 99 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     P R I N T   T O T A L   L I N E
      *
      ******************************************************************
       TOTAL-PRINT SECTION.
           IF SUMMARY-ONLY = "Y"
              MOVE "TOTAL" TO DETAIL-LINE
              MOVE ":" TO D-COLON.

           IF TOT-QTY (LV 101 1) = 0
            IF TOT-AMT (LV 101 1) = 0
             IF TOT-BEG (LV 101 1) = 0
              IF TOT-END (LV 101 1) = 0
               IF TOT-QTY (LV 101 2) = 0
                IF TOT-AMT (LV 101 2) = 0
                 IF TOT-BEG (LV 101 2) = 0
                  IF TOT-END (LV 101 2) = 0
                     MOVE SPACES TO DETAIL-LINE
                     GO TO TOTAL-PRINT-EXIT.
           MOVE 0 TO TP.

       NEXT-TOT-PRINT.
           ADD 1 TO TP.
           MOVE TOT-QTY(LV 101 TP) TO D-QTY(TP).
           MOVE TOT-AMT(LV 101 TP) TO D-AMT(TP).
           MOVE TOT-BEG(LV 101 TP) TO D-BEG(TP).
           MOVE TOT-END(LV 101 TP) TO D-END(TP).

           MOVE 0 TO TOT-QTY(LV 101 TP) TOT-AMT(LV 101 TP)
                       TOT-BEG(LV 101 TP) TOT-END(LV 101 TP).

           IF TP < 2
              GO TO NEXT-TOT-PRINT.

           IF (PRINT-BRANCH-TOTS NOT = "Y" AND TOTAL-TYPE = "BRANCH")
              MOVE SPACES TO DETAIL-LINE
           ELSE
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-TYPE = "STATE "
              MOVE DASH-LINE TO DETAIL-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

       TOTAL-PRINT-EXIT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       CLEAR-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING TP FROM 1 BY 1 UNTIL TP > 2
              MOVE 0 TO TOT-QTY(1 1 TP)
                        TOT-AMT(1 1 TP)
                        TOT-BEG(1 1 TP)
                        TOT-END(1 1 TP)
           END-PERFORM.

           PERFORM VARYING LC FROM 1 BY 1 UNTIL LC > 101
              MOVE TOT-LC(1 1) TO TOT-LC(1 LC)
           END-PERFORM.

           IF LV = 2
              MOVE TOT-LV(1) TO TOT-LV(2).

      ******************************************************************
       CHECK-0 SECTION.
      ******************************************************************
           MOVE 0 TO SUB.

       CHECK-0-NEXT.
           ADD 1 TO SUB.

           IF TOT-BEG(LV LC SUB) NOT = 0 OR
              TOT-END(LV LC SUB) NOT = 0
              GO TO CHECK-0-EXIT.

           IF SUB < 2
              GO TO CHECK-0-NEXT.

           MOVE 0 TO LV.

       CHECK-0-EXIT.
           EXIT.

      ******************************************************************
       CHECK-UNEARNED SECTION.
      ******************************************************************
           MOVE LN-INSCODES  TO HOLD-LN-INSCODES.
           MOVE LN-O3-REBATE TO HOLD-LN-O3-REBATE.
           MOVE LN-O4-REBATE TO HOLD-LN-O4-REBATE.

      * THIS CLEAR IS NECESSARY FOR TEST IN COMPUTE-BEG-UNEARNED:

           MOVE " " TO HOLD-LP-ADDON.
           MOVE " " TO METHOD-4-FG
                       METHOD-5-FG.

           MOVE 0 TO SUB.
           MOVE 5 TO IDX.

       CHECK-UNEARNED-NEXT.
           ADD 1 TO SUB IDX.

      * CLEAR WORKERS:
           MOVE 0 TO BEG-UNEARNED(SUB) END-UNEARNED(SUB)
                     NEW-PREMIUMS(SUB) NEW-REFUNDS(SUB).
           MOVE " " TO CHK-UNEARNED(SUB) INS-INEFFECT(SUB).

      * TEST FOR REGIONAL MANAGEMENT
      *          ADDON INSURANCE
      *          CASH EARNINGS 4 OR 5
      *          ...IF SO SET INSPREM & COMMISSION TO ZERO
      *
      *    IF GP-I-AM-REGMGM
      *       IF LN-ADDON-FG = "Y"
      *          IF REBATE-METHOD(SUB) = 4 OR 5
      *             IF LN-INSEFF-DATE(IDX) NOT = LN-LOANDATE
      *                MOVE "N" TO CHK-UNEARNED(SUB)
      *                GO TO CHECK-UNEARNED-SKIP.

      * SAVE INSURANCE TABLE FOR COMPUTE OF ENDING UNEARNED:
           MOVE LN-INSTBL(IDX) TO HOLD-LN-INSTBL(SUB).

      * FOR INS THAT HAS NOT BEEN REBATED,
      * DIRECTLY COMPUTE BEG & END UNEARNED:
           IF LN-INSCOMP(IDX) = 0          OR
              LN-INSCOMP(IDX) < BEG-IC     OR
              LN-INSCOMP(IDX) > END-IC     OR
              LN-INSEFF-DATE(IDX) = 0           OR
              LN-INSOURS(IDX) NOT = "Y"
              MOVE "N" TO CHK-UNEARNED(SUB)
              GO TO CHECK-UNEARNED-SKIP.

           ADD 5 IDX GIVING IDY.

           IF LN-REBATE(IDY) = " " AND LN-ADDON-FG = " "
              MOVE "N" TO CHK-UNEARNED(SUB)
              PERFORM COMPUTE-BEG-END-UNEARNED.

       CHECK-UNEARNED-SKIP.
           IF SUB < 2
              GO TO CHECK-UNEARNED-NEXT.

      * IF ALL THREE CHK-UNEARNED FLAGS = 'N',
      * THAN IT IS NOT NECESSARY TO SEARCH FOR ADDONS,
      * ADJUSTMENTS, CANCELLATIONS OR REBATES:
           IF CHK-UNEARNED(1) = "N" AND
              CHK-UNEARNED(2) = "N"
              GO TO EXIT-CHECK-UNEARNED.

      * REBATE OR CANCELLATION HAS OCCURRED;
      * IF IT OCCURRED THIS MONTH, FIND BEGINNING INS
      * UNEARNED (ENDING BALANCE IS 0):
           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.  *> SSP LP

       CHECK-UNEARNED-TRANS.
           PERFORM READ-LP1-FILE-PREVIOUS.  *> EMBEDDED LP
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO END-CHECK-UNEARNED.

           IF LP-REV = "Y" OR LP-TRCD = "RV"
              GO TO CHECK-UNEARNED-TRANS.

           IF LP-SSNO = 999999999
              GO TO CHECK-UNEARNED-TRANS.

           MOVE LP-REFNO TO HOLD-LP-ADDON.

      * TEST FOR ACTIVITY THIS MONTH AND
      * SKIP TRANSACTIONS THAT DID NOT OCCUR THIS MONTH
           IF LP-TRDATE NOT > PRIOR-ENDING-DATE
              GO TO END-CHECK-UNEARNED.

      * RESTORE LN-INSPREM AND LN-INSCOMM TO END OF MONTH AMT
      * TO ALLOW FOR CORRECT REBATING
           IF LP-TRCD = "C3"
              MOVE 1 TO SUB
              PERFORM RESTORE-PREM-COMM
              GO TO CHECK-UNEARNED-TRANS
           ELSE
              IF LP-TRCD = "C4"
                 MOVE 2 TO SUB
                 PERFORM RESTORE-PREM-COMM
                 GO TO CHECK-UNEARNED-TRANS.

           IF LP-TRCD = "R3"
              MOVE 1 TO SUB
              PERFORM RESTORE-REBATES
              GO TO CHECK-UNEARNED-TRANS
           ELSE
              IF LP-TRCD = "R4"
                 MOVE 2 TO SUB
                 PERFORM RESTORE-REBATES
                 GO TO CHECK-UNEARNED-TRANS.

           IF LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST"
              IF HOLD-LP-ADDON-TYPE = "O3" OR "O4"
                 PERFORM RESTORE-PREM-COMM-ADDONS
                 GO TO CHECK-UNEARNED-TRANS.

           GO TO CHECK-UNEARNED-TRANS.

       END-CHECK-UNEARNED.
           MOVE 0 TO SUB.

       CHECK-UNEARNED-BEG-ONLY.
           ADD 1 TO SUB.

           IF CHK-UNEARNED(SUB) = " "
              PERFORM COMPUTE-BEG-END-UNEARNED.

           IF SUB < 2
              GO TO CHECK-UNEARNED-BEG-ONLY.

       EXIT-CHECK-UNEARNED.
           EXIT.

      ******************************************************************
       RESTORE-REBATES SECTION.
      ******************************************************************
           ADD 5 SUB GIVING IDX.
           ADD 5 IDX GIVING IDY.

           IF LN-INSPREM(IDX) = 0
              PERFORM GET-INS-COMMISSION
              IF WK-IC-COMMRATE >= 0
                 COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * WK-IC-COMMRATE / 100
              ELSE
                 COMPUTE WK-COMMISSION = WK-IC-COMMRATE * -1
              END-IF
           ELSE
              COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * LN-INSCOMM(IDX)
                               / LN-INSPREM(IDX).

           SUBTRACT WK-COMMISSION FROM NEW-REFUNDS(SUB).
           MOVE " " TO LN-REBATE(IDY).

      ******************************************************************
       RESTORE-PREM-COMM SECTION.
      ******************************************************************
           ADD 5 SUB GIVING IDX.
           ADD 5 IDX GIVING IDY.

           IF LN-INSPREM(IDX) = 0
              PERFORM GET-INS-COMMISSION
              IF WK-IC-COMMRATE >= 0
                 COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * WK-IC-COMMRATE / 100
              ELSE
                 COMPUTE WK-COMMISSION = WK-IC-COMMRATE * -1
              END-IF
           ELSE
              COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * LN-INSCOMM(IDX)
                               / (LN-INSPREM(IDX) + LN-INSCOMM(IDX)).

           SUBTRACT WK-COMMISSION FROM NEW-REFUNDS(SUB).
           MOVE " " TO LN-REBATE(IDY).
           ADD LP-TRAMT TO LN-INSPREM(IDX).
           ADD WK-COMMISSION TO LN-INSCOMM(IDX).

      ******************************************************************
       RESTORE-PREM-COMM-ADDONS SECTION.
      ******************************************************************

           MOVE LN-OWNBR    TO LTI-BRNO.
           MOVE LN-ACCTNO   TO LTI-ACCTNO.
           MOVE LP-LT-SEQNO TO LTI-SEQNO.
           PERFORM READ-LTI1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE LTI-INSPREM TO LTI-INSCOMM.

           IF HOLD-LP-ADDON-TYPE = "O3"
              MOVE 1 TO SUB
           ELSE
              IF HOLD-LP-ADDON-TYPE = "O4"
                 MOVE 2 TO SUB.

           ADD 5 SUB GIVING IDX.
           ADD 5 IDX GIVING IDY.

           IF HOLD-LP-ADDON-TEXT = "ADJ"
              SUBTRACT LTI-INSPREM FROM LN-INSPREM(IDX)
              SUBTRACT LTI-INSCOMM FROM LN-INSCOMM(IDX)
              ADD LTI-INSCOMM TO NEW-PREMIUMS(SUB)
              GO TO RESTORE-PREM-COMM-ADDONS-EXIT.

           IF HOLD-LP-ADDON-TEXT = "CNL"
              SUBTRACT LTI-INSPREM FROM LN-INSPREM(IDX)
              SUBTRACT LTI-INSCOMM FROM LN-INSCOMM(IDX)
              ADD LTI-INSCOMM TO NEW-REFUNDS(SUB)

              MOVE " " TO LN-REBATE(IDY)
              GO TO RESTORE-PREM-COMM-ADDONS-EXIT.

           IF HOLD-LP-ADDON-TEXT = "ADD"
              ADD LTI-INSCOMM TO NEW-PREMIUMS(SUB)
              MOVE LN-OWNBR  TO LX-BRNO
              MOVE LN-ACCTNO TO LX-ACCTNO
              MOVE LP-SEQNO  TO LX-SEQNO
              PERFORM READ-LX1-FILE
              IF IO-GOOD AND (LXA-CODE = "A")
                 MOVE LXA-LN-INSCOMP TO LN-INSCOMP(IDX)
                 MOVE LXA-LN-INSOURS TO LN-INSOURS(IDX)
                 MOVE LXA-LN-INSEFF-DATE TO LN-INSEFF-DATE(IDX)
                 MOVE LXA-LN-INSEXP-DATE TO LN-INSEXP-DATE(IDX)
                 MOVE LXA-LN-INSPREM TO LN-INSPREM(IDX)
                 MOVE LXA-LN-INSCOVR TO LN-INSCOVR(IDX)
                 MOVE LXA-LN-INSCOMM TO LN-INSCOMM(IDX)
                 MOVE LXA-LN-INSCODES TO LN-INSCODES
                 IF SUB = 1
                    MOVE LXA-LN-INS-REBATE TO LN-O3-REBATE
                 ELSE
                 IF SUB = 2
                    MOVE LXA-LN-INS-REBATE TO LN-O4-REBATE.

       RESTORE-PREM-COMM-ADDONS-EXIT.
           EXIT.

      ******************************************************************
       GET-INS-COMMISSION SECTION.
      ******************************************************************
           MOVE IC-NAME TO SAVE-IC-NAME.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE 100.00 TO WK-IC-COMMRATE.
              GO TO GET-INS-COMMISSION-EXIT.

           MOVE LN-INSCOMP(IDX) TO IC-CODE.
           MOVE LN-INSCOMP(IDX) TO IC-CODE.

           IF IO-BAD
              MOVE 0 TO WK-IC-COMMRATE
              GO TO GET-INS-COMMISSION-EXIT.

           ADD 3 IDX GIVING COMM-SUB.
           MOVE IC-COMMRATE(COMM-SUB) TO WK-IC-COMMRATE.

       GET-INS-COMMISSION-EXIT.
           MOVE SAVE-IC-NAME TO IC-NAME.

      ******************************************************************
       COMPUTE-BEG-UNEARNED SECTION.
      ******************************************************************
           IF REBATE-METHOD(SUB) = 4
              IF METHOD-4-FG = " "
                 PERFORM FIND-METHOD-4-DATES
                 MOVE "Y" TO METHOD-4-FG.

           IF REBATE-METHOD(SUB) = 5
              IF METHOD-5-FG = " "
                 IF LN-POFFDATE NOT = 0
                    IF LN-POFFDATE NOT > PRIOR-ENDING-DATE
                       MOVE 0 TO BEG-UNEARNED(SUB)
                       GO TO COMPUTE-BEG-UNEARNED-EXIT
                    END-IF
                 END-IF
                 PERFORM FIND-METHOD-5-DATES
                 MOVE "Y" TO METHOD-5-FG.

           ADD 5 SUB GIVING IDX.
           ADD 5 IDX GIVING IDY.

      * TEST FOR NEW PREMIUM AND NEW VOIDED PREMIUM:

           IF LN-CONVERCD = " "
             IF LN-INSEFF-DATE(IDX) = LN-INTDATE
               IF HOLD-LP-ADDON-TEXT NOT = "ADD"
                  MOVE LN-POFFDATE TO NUM-DATE
                  MOVE LN-ENTDATE TO SYS-DATE
      * TEST IF ACCOUNT PAIDOFF (NOT VOIDED) THE SAME MONTH AS BOOKED
      * AND LOAN WAS BOOKED THIS ACCOUNTING PERIOD THEN ADD TO NEW PREMIUMS
                  IF (NUM-MO = S-MM AND NUM-YR = S-YY) AND
                     (LN-POCD-POFF) AND (NOT LN-POCD-TRANSFERRED)
                      IF LN-ENTDATE > PRIOR-ENDING-DATE
                         ADD LN-INSCOMM(IDX) TO NEW-PREMIUMS(SUB)
                      END-IF
                  ELSE
      * TEST IF ACCOUNT WAS NOT PAIDOFF OR VOIDED THE SAME MONTH AS BOOKED
                  IF NOT (NUM-MO = S-MM AND NUM-YR = S-YY)
      * TEST FOR ACCOUNT VOIDED BUT NOT IN THE SAME MONTH AS BOOKED
      * THEN SUBTRACT FROM NEW PREMIUMS
                    IF LN-POCD-VOID OR LN-POCD-UNWIND
                       SUBTRACT LN-INSCOMM(IDX) FROM NEW-PREMIUMS(SUB)
                    ELSE
      * TEST IF ACCOUNT WAS NOT PAIDOFF OR VOIDED THE SAME MONTH
      * AS BOOKED AND LOAN WAS BOOKED THIS ACCOUNTING PERIOD
      * THEN ADD TO NEW PREMIUMS
                       IF LN-ENTDATE > PRIOR-ENDING-DATE
                          ADD LN-INSCOMM(IDX) TO NEW-PREMIUMS(SUB).

      * TEST FOR REBATE DATE PRIOR TO ENTRY-DATE

           IF REBATE-METHOD(SUB) = 4
              MOVE METHOD-4-BEG-PAYDATE TO REB-PAYDATE
           ELSE
              IF REBATE-METHOD(SUB) = 5
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
              IF LN-INSPREM(IDX) NOT = 0
                 IF LN-REBATE(IDY) = " "
                    IF (REBATE-METHOD(SUB) = 4 OR 5)
                                 AND (LN-DATE-PAID-LAST = 0)
                       ADD LN-INSCOMM(IDX) TO BEG-UNEARNED(SUB)
                    ELSE
                       PERFORM UNEARNED-VIA-REBATE
                       COMPUTE BEG-UNEARNED(SUB) ROUNDED =
                          BEG-UNEARNED(SUB)
                            + REB-REBATE * LN-INSCOMM(IDX)
                                    / LN-INSPREM(IDX).

       COMPUTE-BEG-UNEARNED-EXIT.
           EXIT.

      ******************************************************************
       COMPUTE-BEG-END-UNEARNED SECTION.
      ******************************************************************
           PERFORM COMPUTE-BEG-UNEARNED.

           IF REBATE-METHOD(SUB) = 4
              MOVE METHOD-4-END-PAYDATE TO REB-PAYDATE
           ELSE
              IF REBATE-METHOD(SUB) = 5
                 IF LN-POFFDATE NOT = 0
                    IF LN-POFFDATE NOT > PRIOR-ENDING-DATE
                       MOVE 0 TO END-UNEARNED(SUB)
                       GO TO COMPUTE-END-UNEARNED-EXIT
                    END-IF
                 END-IF
                 MOVE METHOD-5-END-PAYMNTD TO PAYMNTD
              ELSE
                 MOVE REPORT-ENDING-DATE   TO REB-PAYDATE.

           ADD 5 SUB GIVING IDX.
           ADD 5 IDX GIVING IDY.

           MOVE REPORT-ENDING-DATE TO REB-PAYDATE.
           MOVE HOLD-LN-INSCODES TO LN-INSCODES.

           IF SUB = 1
              MOVE HOLD-LN-O3-REBATE TO LN-O3-REBATE
           ELSE
              IF SUB = 2
                 MOVE HOLD-LN-O4-REBATE TO LN-O4-REBATE.

           MOVE HOLD-LN-INSTBL(SUB) TO LN-INSTBL(IDX).

           IF NOT LN-POCD-VOID
            IF NOT LN-POCD-UNWIND
              IF LN-INSPREM(IDX) NOT = 0
                 IF LN-REBATE(IDY) = " "
                    IF (REBATE-METHOD(SUB) = 4 OR 5) AND
                       (LN-DATE-PAID-LAST = 0)
                       ADD LN-INSCOMM(IDX) TO END-UNEARNED(SUB)
                    ELSE
                       PERFORM UNEARNED-VIA-REBATE
                       COMPUTE END-UNEARNED(SUB) ROUNDED =
                            END-UNEARNED(SUB) + REB-REBATE *
                            LN-INSCOMM(IDX) / LN-INSPREM(IDX).

       COMPUTE-END-UNEARNED-EXIT.
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

           IF ( WS-NET-INCOME-FLAG = "Y" )
              GO TO HEAD-NET-INCOME.

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

       HEAD-NET-INCOME.
           MOVE HEAD7 TO PR-REC
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       EXIT-HEAD-PAGE.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           MOVE 3 TO LN-FEED.
      
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
      
           MOVE "BEG INS CO" TO RANGE-LINE.
           MOVE BEG-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "O3 REBATE METHOD" TO RANGE-LINE.
           MOVE BR-REB-METHOD(7) TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "O4 REBATE METHOD" TO RANGE-LINE.
           MOVE BR-REB-METHOD(8) TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "O3 REB DAYS/FULL MO." TO RANGE-LINE.
           MOVE BR-REBDAYS-MONTH(7) TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "O4 REB DAYS/FULL MO." TO RANGE-LINE.
           MOVE BR-REBDAYS-MONTH(8) TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "O3 MONTHS TO ACCRUE " TO RANGE-LINE.
           MOVE BR-ACCRUAL-MONTHS(6) TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "O4 MONTHS TO ACCRUE " TO RANGE-LINE.
           MOVE BR-ACCRUAL-MONTHS(7) TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "MONTH AND YEAR" TO RANGE-LINE.
           MOVE MMYY TO RANGE-SEL.
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
      
           MOVE "CLASS TOTAL SUMMARIES ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PRINT BRANCH TOTALS ?" TO RANGE-LINE.
           MOVE PRINT-BRANCH-TOTS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
      ******************************************************************
       UNEARNED-VIA-REBATE SECTION.
      ******************************************************************
           MOVE 0 TO REB-REBATE.

           IF SUB = 1
              MOVE 14 TO REB-SUB.

           IF SUB = 2
              MOVE 15 TO REB-SUB.

           ADD 5 SUB GIVING IDY.

           IF BR-ACCRUAL-MONTHS(SUB) = 0
              MOVE IDY TO ITRM-SUB
              MOVE 0 TO ITRM-TODAY
              IF LN-INSCOVR(IDY) = 0
                 MOVE 0.01 TO LN-INSCOVR(IDY)
              END-IF
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO REB-ORGTERM
           ELSE
              MOVE BR-ACCRUAL-MONTHS(SUB) TO REB-ORGTERM.

           MOVE LN-INSPREM(IDY) TO REB-TOTCHG.

      * 5 = RULE OF 78THS PRINCIPLE REDUCTION:

           IF REBATE-METHOD(SUB) = 5
              PERFORM METHOD-5-CALCULATION
              GO TO END-UNEARNED-VIA-REBATE.

      * 7 = ACTUARIAL 52:
           IF REBATE-METHOD(SUB) = 7
              PERFORM METHOD-7-CALCULATION
              GO TO END-UNEARNED-VIA-REBATE.

           IF LN-ADDON-FG = "Y"
              PERFORM SET-ADDON-INS.

      * 1 OR 4 OR 6 = RULE OF 78THS FIXED TO THE FOLLOWING FORMULATION:

           IF REBATE-METHOD(SUB) = 1 OR 4 OR 6
              MOVE 1 TO SP-RBFRMLA(REB-SUB)
              MOVE "B" TO SP-RBDTE(REB-SUB 1)
                          SP-RBDTE(REB-SUB 2)
              MOVE BR-REBDAYS-MONTH(SUB) TO SP-RBDAYS(REB-SUB 1)
                                            SP-RBDAYS(REB-SUB 2)
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
              MOVE 0 TO SP-RBMANAGER(REB-SUB).

      * 2 = PRO RATA FIXED TO THE FOLLOWING FORMULATION:

           IF REBATE-METHOD(SUB) = 2
              MOVE 5 TO SP-RBFRMLA(REB-SUB)
              MOVE "B" TO SP-RBDTE(REB-SUB 1)
                          SP-RBDTE(REB-SUB 2)
              MOVE BR-REBDAYS-MONTH(SUB) TO SP-RBDAYS(REB-SUB 1)
                                            SP-RBDAYS(REB-SUB 2)
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
              MOVE 0 TO SP-RBMANAGER(REB-SUB).

      * 3 = REFUND STANDARD METHOD:
      * USE STATE PARAMETERS FOR DETAIL TO REBATE.

      * SECURITY FINANCE, EARN 25% UPFRONT:

           IF REBATE-METHOD(SUB) = 9
              COMPUTE REB-TOTCHG ROUNDED = REB-TOTCHG * .75.

           IF REBATE-METHOD(SUB) = 3
              MOVE SUB TO REB-SUB2 ADDONSPR-SUB
              PERFORM FIND-ADDON-SPR
              PERFORM REBATE
              PERFORM FIND-ADDON-SPR-RESTORE
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 SUBTRACT INS-TAX-RB FROM REB-REBATE
              END-IF
           ELSE
              PERFORM REBATE
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 SUBTRACT INS-TAX-RB FROM REB-REBATE.

      * 6 = 78THS / PRORATA AVERAGE METHOD:

           IF REBATE-METHOD(SUB) = 6
              COMPUTE REB-REBATE ROUNDED =
                 REB-REBATE / 2 * (1 + (REB-ORGTERM + 1)
                                         / (REB-REMTERM + 1)
                                  ).

       END-UNEARNED-VIA-REBATE.
           EXIT.

      ******************************************************************
      *
      *    FIND METHOD 4 (CASH METHOD RE: INTEREST) REBATE
      *
      ******************************************************************
       FIND-METHOD-4-DATES SECTION.
      ******************************************************************
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

       FIND-METHOD-4-DATES-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO FIND-METHOD-4-DATES-END.

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
                                      OR "AH" OR "PN" OR "PZ"
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

           IF PRIOR-ENDING-DATE NOT < LP-PAYDATE
              MOVE LP-PAYDATE TO METHOD-4-BEG-PAYDATE.

           GO TO FIND-METHOD-4-DATES-NEXT.

      * TEST FOR ACTIVITY THIS MONTH:
       FIND-METHOD-4-DATES-NO-RV.
           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "BF" OR "PO" OR "PB"
                                     OR "SC" OR "RN" OR "RB"
                                      OR "RO" OR "AH" OR "PN"
                                       OR "PZ" OR "IN")
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
      ******************************************************************
           MOVE LN-TOTPAYMNTD TO METHOD-5-BEG-PAYMNTD
                                 METHOD-5-END-PAYMNTD.

           IF SP-CONTRFRMLA = "A"
              SUBTRACT LN-TOTLCHG FROM METHOD-5-BEG-PAYMNTD
                                       METHOD-5-END-PAYMNTD.

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

       FIND-METHOD-5-DATES-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO FIND-METHOD-5-DATES-END.

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
                                      OR "AH" OR "PN" OR "PZ"
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

      * TEST FOR ACTIVITY THIS MONTH:
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
      *    METHOD 5 CALCULATION   RE: PRINCIPLE REDUCTION
      *
      ******************************************************************
       METHOD-5-CALCULATION SECTION.
      ******************************************************************

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
      *    METHOD 7 CALCULATION  RE: ACTUARIAL ACCRUAL
      ****************************************
       METHOD-7-CALCULATION SECTION.
      ****************************************
           MOVE REB-PAYDATE       TO REGZ-DATE.
           MOVE LN-LOANDATE       TO REGZ-LOANDATE.
           MOVE LN-INTDATE        TO REGZ-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO REGZ-1STPYDATE.
           MOVE LN-ORGTERM        TO REGZ-ORGTERM.
           MOVE LN-1STPYAMT       TO REGZ-1STPYAMT.
           MOVE LN-REGPYAMT       TO REGZ-REGPYAMT.
           MOVE LN-LASTPYAMT      TO REGZ-LASTPYAMT.
           MOVE LN-FINCHG         TO REGZ-FINCHG.
           MOVE LN-APRATE         TO REGZ-APRATE.
           MOVE "A"               TO REGZ-DISFRMLA.
           MOVE "N"               TO REGZ-INCLUDE-DEF-FG.
           MOVE LN-TOTNODEF       TO REGZ-TOTNODEF.

           PERFORM REGZ-UNEARNED-CALC.

           COMPUTE REB-REBATE ROUNDED =
               REGZ-UNEARNED * LN-INSPREM(IDX) / LN-FINCHG.

       METHOD-7-CALCULATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    SETUP FOR INSURANCE ADDON REFUND
      *
      ******************************************************************
       SET-ADDON-INS SECTION.
      ******************************************************************
           MOVE LN-INSEFF-DATE(IDX) TO REB-LN-LOANDATE
                                  REB-LN-PURDATE
                                  REB-LN-INTDATE
                                  REB-LN-INSEFF
                                  NDTE-DATE.

           MOVE LN-INSEXP-DATE(IDX) TO REB-LN-INSEXP.

           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO REB-LN-ORIG-1STPYDATE.
           MOVE 0 TO REB-LN-TOTNODEF.
           MOVE LN-INSCOVR(IDX) TO REB-LN-INSCOVR.
           MOVE "N" TO REB-LN-SALEFIN.
           MOVE "Y" TO REB-ADDON-FG.

      ******************************************************************
       CREATE-WK2 SECTION.
      ******************************************************************

           MOVE 1 TO SUB-WK2.

       SAVE-WORK-KEY2.

           MOVE HOLD-GI-BR-ACCOUNT(SUB-WK2) TO WK2-GI-BR-ACCOUNT.
           MOVE "N"                         TO REC-EXISTS.
           MOVE WK2-KEY                     TO SAVE-WK2-KEY.

       CHECK-REC2-EXISTS.

           PERFORM READ-WK2-FILE.
           IF ( IO-GOOD )
              GO TO UPDATE-WK2-REC.

           IF ( REC-EXISTS = "Y" )
              GO TO IO-ERROR.

           MOVE " " TO WK2-REC.
           MOVE SAVE-WK2-KEY TO WK2-KEY.
           MOVE 0 TO WK2-NET-INCOME
                     WK2-NET-UNEARNED.

           PERFORM WRITE-WK2-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC2-EXISTS.

       UPDATE-WK2-REC.

           IF SUB-WK2 = 1
              ADD WS-NET-INCOME(1) TO WK2-NET-UNEARNED
           ELSE
           IF SUB-WK2 = 2
              ADD WS-NET-INCOME(1) TO WK2-NET-INCOME
           ELSE
           IF SUB-WK2 = 3
              ADD WS-NET-INCOME(2) TO WK2-NET-UNEARNED
           ELSE
              ADD WS-NET-INCOME(2) TO WK2-NET-INCOME.

           IF ( SUB-WK2 = 1 OR 3)
              MOVE "D" TO WK2-CREDIT-DEBIT
           ELSE
              MOVE "C" TO WK2-CREDIT-DEBIT.

           PERFORM REWRITE-WK2-FILE.

       CHECK-FOR-NEXT-WK2.

           IF ( SUB-WK2 < 4 )
              ADD 1 TO SUB-WK2
              GO TO SAVE-WORK-KEY2.

       EXIT-CREATE-WK2.
           EXIT.

      ******************************************************************
       PRINT-TOTAL-WK2 SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.
           CLOSE WK2-FILE.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE 0 TO TOTAL-DEBITS TOTAL-CREDITS.

           MOVE 1 TO WK2-GI-BR-ACCOUNT.
           PERFORM START-WK2-FILE.

       PRINT-WK2-NEXT.

           PERFORM READ-WK2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO EXIT-PRINT-TOTAL-WK2.

           UNLOCK WK2-FILE.

           MOVE WK2-GI-BR-ACCOUNT TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE " "               TO DETAIL-LINE
              MOVE "NOT ON FILE ***" TO GL-DESCRIPTION.

           MOVE " "            TO DETAIL-LINE.
           MOVE GL-DESCRIPTION TO D-GL-NAME.
           MOVE WK2-GI-BR-ACCOUNT  TO D-GI-ACCT.
           INSPECT D-GI-ACCT REPLACING ALL "/" BY "-".

           IF WK2-NET-UNEARNED NOT = 0
              ADD WK2-NET-UNEARNED  TO TOTAL-DEBITS
              MOVE WK2-NET-UNEARNED TO D-NET-DEBIT.

           IF WK2-NET-INCOME NOT = 0
              ADD WK2-NET-INCOME TO TOTAL-CREDITS
              COMPUTE D-NET-CREDIT = WK2-NET-INCOME * -1.

           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-WK2-NEXT.

       EXIT-PRINT-TOTAL-WK2.
           MOVE "TOTALS :" TO D-GL-NAME.
           MOVE TOTAL-DEBITS TO D-NET-DEBIT.
           COMPUTE D-NET-CREDIT = ( TOTAL-CREDITS * ( -1 ) )
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM CLOSE-GL1-FILE.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPREGZ.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO ICUNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ICUNRP-SYSDATE.
           DISPLAY ICUNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ICUNRP-FORM-FIELDS TO WR-BUF.
           MOVE ICUNRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE ICUNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICUNRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ICUNRP_EVA.CPY".
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
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *-----------------------------------------------------------------
       GET-GI.
      *  GI DATA INCLUDE IN STORED PROCEDURE SQL
           MOVE LN-OWNBR TO GI-BRANCH.
           MOVE LN-CLASS TO GI-CLASS.
      *    PERFORM READ-GI1-FILE.

      *-----------------------------------------------------------------
       IC-HEADER.
           PERFORM OPEN-IC1-FILE.

           MOVE LAST-IC TO IC-CODE.
           PERFORM READ-IC1-FILE.
           IF IO-BAD
              MOVE "* INVALID *" TO IC-NAME.
           PERFORM CLOSE-IC1-FILE.

           MOVE LAST-IC TO D-BRNO.
           MOVE IC-NAME TO D-BRNAME.
           MOVE ":" TO D-COLON.

      *-----------------------------------------------------------------
       ST-HEADER.
           MOVE "STATE OF" TO DETAIL-LINE.
           MOVE LAST-ST TO D-STATE.
           MOVE ":" TO D-COLON.

      *-----------------------------------------------------------------
       BR-HEADER.
           MOVE LAST-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "* INVALID *" TO BR-NAME.

           MOVE LAST-BR TO D-BRNO.
           MOVE BR-NAME TO D-BRNAME.
           MOVE ":" TO D-COLON.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

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
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.   *> SSP LN GI
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.

           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS, LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

      *     SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_SALEFIN,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_ADDON_FG,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_FINCHG,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_POINTS,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSELIGIBLE,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSCOMM_1,
      *            LNFILE.LN_INSCOMM_2,
      *            LNFILE.LN_INSCOMM_3,
      *            LNFILE.LN_INSCOMM_4,
      *            LNFILE.LN_INSCOMM_5,
      *            LNFILE.LN_INSCOMM_6,
      *            LNFILE.LN_INSCOMM_7,
      *            LNFILE.LN_INSCOMM_8,
      *            LNFILE.LN_INSCOMM_9,
      *            LNFILE.LN_INSCOMM_10,
      *            LNFILE.LN_INSANTICERN_1,
      *            LNFILE.LN_INSANTICERN_2,
      *            LNFILE.LN_INSANTICERN_3,
      *            LNFILE.LN_INSANTICERN_4,
      *            LNFILE.LN_INSANTICERN_5,
      *            LNFILE.LN_INSANTICERN_6,
      *            LNFILE.LN_INSANTICERN_7,
      *            LNFILE.LN_INSANTICERN_8,
      *            LNFILE.LN_INSANTICERN_9,
      *            LNFILE.LN_INSANTICERN_10,
      *            LNFILE.LN_INSANTICADJ_1,
      *            LNFILE.LN_INSANTICADJ_2,
      *            LNFILE.LN_INSANTICADJ_3,
      *            LNFILE.LN_INSANTICADJ_4,
      *            LNFILE.LN_INSANTICADJ_5,
      *            LNFILE.LN_INSANTICADJ_6,
      *            LNFILE.LN_INSANTICADJ_7,
      *            LNFILE.LN_INSANTICADJ_8,
      *            LNFILE.LN_INSANTICADJ_9,
      *            LNFILE.LN_INSANTICADJ_10,
      *            LNFILE.LN_INSUFERN_1,
      *            LNFILE.LN_INSUFERN_2,
      *            LNFILE.LN_INSUFERN_3,
      *            LNFILE.LN_INSUFERN_4,
      *            LNFILE.LN_INSUFERN_5,
      *            LNFILE.LN_INSUFERN_6,
      *            LNFILE.LN_INSUFERN_7,
      *            LNFILE.LN_INSUFERN_8,
      *            LNFILE.LN_INSUFERN_9,
      *            LNFILE.LN_INSUFERN_10,
      *            LNFILE.LN_INSACCERN_1,
      *            LNFILE.LN_INSACCERN_2,
      *            LNFILE.LN_INSACCERN_3,
      *            LNFILE.LN_INSACCERN_4,
      *            LNFILE.LN_INSACCERN_5,
      *            LNFILE.LN_INSACCERN_6,
      *            LNFILE.LN_INSACCERN_7,
      *            LNFILE.LN_INSACCERN_8,
      *            LNFILE.LN_INSACCERN_9,
      *            LNFILE.LN_INSACCERN_10,
      *            LNFILE.LN_INSEARN_ORDER_1,
      *            LNFILE.LN_INSEARN_ORDER_2,
      *            LNFILE.LN_INSEARN_ORDER_3,
      *            LNFILE.LN_INSEARN_ORDER_4,
      *            LNFILE.LN_INSEARN_ORDER_5,
      *            LNFILE.LN_INSEARN_ORDER_6,
      *            LNFILE.LN_INSEARN_ORDER_7,
      *            LNFILE.LN_INSEARN_ORDER_8,
      *            LNFILE.LN_INSEARN_ORDER_9,
      *            LNFILE.LN_INSEARN_ORDER_10,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_TOTINT,
      *            LNFILE.LN_TOTLCHG,
      *            LNFILE.LN_TOTDEF,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_TOTNOALDEL,
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
      *            LNFILE.LN_ACTIONCD2,
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_REPOCD,
      *            ISNULL(GIFILE.GI_BRANCH,0),
      *            ISNULL(GIFILE.GI_CLASS,0),
      *            ISNULL(GIFILE.GI_UNO3,0),
      *            ISNULL(GIFILE.GI_O3,0)
      *      FROM DBO.LNFILE LNFILE
      *      LEFT OUTER JOIN DBO.GIFILE GIFILE
      *       ON GIFILE.GI_BRANCH = LNFILE.LN_OWNBR
      *       AND GIFILE.GI_CLASS = LNFILE.LN_CLASS
      *      WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_OWNBR
      *       AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *       AND LNFILE.LN_ORGST BETWEEN @PASS_WBEG_ST AND @PASS_WEND_ST
      *       AND (LNFILE.LN_INSCOMP_6 !< @PASS_WBEG_IC OR LNFILE.LN_INSCOMP_7 !< @PASS_WBEG_IC)
      *       AND (LNFILE.LN_INSCOMP_6 !> @PASS_WEND_IC OR LNFILE.LN_INSCOMP_7 !> @PASS_WEND_IC)	
      *       AND LNFILE.LN_POCD NOT IN ('*I','*R') 
      *     ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_ICUNR3_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR,
                    :QLN3-WBEG-CLASS, :QLN3-WEND-CLASS, 
                    :PASS-WBEG-IC, :PASS-WEND-IC,
                    :PASS-WBEG-ST, :PASS-WEND-ST)
                END-EXEC.

            EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.  *> EMBEDDED LN GI
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QGI-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN3_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-CONVERCD,
                   :QLN-POCD,
                   :QLN-SALEFIN,
                   :QLN-LOANTYPE,
                   :QLN-ADDON-FG,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-ENTDATE,
                   :QLN-LOANDATE,
                   :QLN-PURDATE,
                   :QLN-INTDATE,
                   :QLN-CASHDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-PLDATE,
                   :QLN-POFFDATE,
                   :QLN-APRATE,
                   :QLN-EFFRATE,
                   :QLN-SMPRATE,
                   :QLN-OVRDRATE,
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
                   :QLN-INSELIGIBLE,
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
                   :QLN-INSEFF-DATE-10,
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
                   :QLN-INSCOMM-1,
                   :QLN-INSCOMM-2,
                   :QLN-INSCOMM-3,
                   :QLN-INSCOMM-4,
                   :QLN-INSCOMM-5,
                   :QLN-INSCOMM-6,
                   :QLN-INSCOMM-7,
                   :QLN-INSCOMM-8,
                   :QLN-INSCOMM-9,
                   :QLN-INSCOMM-10,
                   :QLN-INSANTICERN-1,
                   :QLN-INSANTICERN-2,
                   :QLN-INSANTICERN-3,
                   :QLN-INSANTICERN-4,
                   :QLN-INSANTICERN-5,
                   :QLN-INSANTICERN-6,
                   :QLN-INSANTICERN-7,
                   :QLN-INSANTICERN-8,
                   :QLN-INSANTICERN-9,
                   :QLN-INSANTICERN-10,
                   :QLN-INSANTICADJ-1,
                   :QLN-INSANTICADJ-2,
                   :QLN-INSANTICADJ-3,
                   :QLN-INSANTICADJ-4,
                   :QLN-INSANTICADJ-5,
                   :QLN-INSANTICADJ-6,
                   :QLN-INSANTICADJ-7,
                   :QLN-INSANTICADJ-8,
                   :QLN-INSANTICADJ-9,
                   :QLN-INSANTICADJ-10,
                   :QLN-INSUFERN-1,
                   :QLN-INSUFERN-2,
                   :QLN-INSUFERN-3,
                   :QLN-INSUFERN-4,
                   :QLN-INSUFERN-5,
                   :QLN-INSUFERN-6,
                   :QLN-INSUFERN-7,
                   :QLN-INSUFERN-8,
                   :QLN-INSUFERN-9,
                   :QLN-INSUFERN-10,
                   :QLN-INSACCERN-1,
                   :QLN-INSACCERN-2,
                   :QLN-INSACCERN-3,
                   :QLN-INSACCERN-4,
                   :QLN-INSACCERN-5,
                   :QLN-INSACCERN-6,
                   :QLN-INSACCERN-7,
                   :QLN-INSACCERN-8,
                   :QLN-INSACCERN-9,
                   :QLN-INSACCERN-10,
                   :QLN-INSEARN-ORDER-1,
                   :QLN-INSEARN-ORDER-2,
                   :QLN-INSEARN-ORDER-3,
                   :QLN-INSEARN-ORDER-4,
                   :QLN-INSEARN-ORDER-5,
                   :QLN-INSEARN-ORDER-6,
                   :QLN-INSEARN-ORDER-7,
                   :QLN-INSEARN-ORDER-8,
                   :QLN-INSEARN-ORDER-9,
                   :QLN-INSEARN-ORDER-10,
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
                   :QLN-TOTLCHG,
                   :QLN-TOTDEF,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
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
                   :QLN-ACTIONCD2,
                   :QLN-DATE-PAID-LAST,
                   :QLN-CREDITLIM,
                   :QLN-OLDACCTNO,
                   :QLN-PRLNNO,
                   :QLN-SUM-FINCHG,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QGI-BRANCH,
                   :QGI-CLASS,
                   :QGI-UNO3,
                   :QGI-O3
 
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
              PERFORM GET-GI-FIELDS.

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



      *COPY "LIBLP/LPLP1RN.CPY".
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
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER.  *> SSP LP
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

      *   SELECT LPFILE.LP_BRNO,
      *            LPFILE.LP_ACCTNO,
      *            LPFILE.LP_SEQNO,
      *            CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),
      *            CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)), 
      *            LPFILE.LP_TRCD, 
      *            LPFILE.LP_REFNO, 
      *            LPFILE.LP_TRAMT,
      *            LPFILE.LP_APINT,
      *            LPFILE.LP_APCUR,
      *            LPFILE.LP_SSNO,
      *            LPFILE.LP_LT_SEQNO,
      *            LPFILE.LP_REV
      *     FROM DBO.LPFILE
      *     WHERE LPFILE.LP_BRNO    = @QLP1_WEND_BRNO
      *       AND LPFILE.LP_ACCTNO  = @QLP1_WEND_ACCT
      *       AND LPFILE.LP_SEQNO  <= @QLP1_WEND_SEQ
      *     ORDER BY LPFILE.LP_SEQNO  DESC

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LP1_CURSOR_NG CURSOR FOR
                   :LP1-RETURN-CODE = 
            EXEC SSP_ICUNR3_LPFILE_SELECT_NG
                  ( :QLP1-WEND-BRNO, :QLP1-WEND-ACCTNO,
                    :QLP1-WEND-SEQNO) 
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

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS.  *> EMBEDDED LP
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_NG INTO 
                   :QLP-BRNO,
                   :QLP-ACCTNO,
                   :QLP-SEQNO,
                   :QLP-TRDATE,
                   :QLP-PAYDATE,
                   :QLP-TRCD,
                   :QLP-REFNO,
                   :QLP-TRAMT,
                   :QLP-APINT,
                   :QLP-APCUR,
                   :QLP-SSNO,
                   :QLP-LT-SEQNO,
                   :QLP-REV
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       CLOSE-LP1-CONNECTION.

           IF LP1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LP1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LP1-CONN-OPEN-SW.


       COPY "LIBLP/LPLX1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
      *=================================================================
       OPEN-ICUNR3-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE ICUNR3-PATH TO E-FILE.
           OPEN OUTPUT ICUNR3-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-ICUNR3-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE ICUNR3-FILE
              GO TO OPEN-ICUNR3-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-ICUNR3-FILE.

           PERFORM READ-IT.
           MOVE ICUNR3-PATH TO E-FILE.
           READ ICUNR3-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-ICUNR3-FILE.

      *-----------------------------------------------------------------
       WRITE-ICUNR3-FILE.

           PERFORM WRITE-IT.
           MOVE ICUNR3-PATH TO E-FILE.
           WRITE ICUNR3-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-ICUNR3-FILE.

      *-----------------------------------------------------------------
       CLOSE-ICUNR3-FILE.

           PERFORM CLOSE-IT.
           CLOSE ICUNR3-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE WK-PATH  TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
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
              DESTROY ICUNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
