       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AVAILC.
       DATE-WRITTEN.    051492.
      ******************************************************************
      *
      *    DIR :  LP
      *    DESC:  CUSTOM AVAILABILITY REPORT
      *
      * ONLY RUNS IN CURRENT BRANCH, DOES NOT CROSS DATA PATHS
      *    FOR : AMERICAN THRIFT
      *
      *=================================================================
      * REV:
      * JFF 101195 ADDED CLASS SCAN
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * 012097 BAH ADDED BR-AVAIL-CREDIT = "S" FOR WDT, TEST AGAINST
      *            THEIR NEW AVCR-CURRENT-AVAIL FIELD
      * GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      * MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *            ADDED GET-BR TO  MISC PARAGRAPHS
      * JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  CS 042798 ADDED NEW RENEWAL CODES "RB" AND "RO" TO TEST FOR RENEWALS
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * BAH 000124 ADDED BEGIN AND ENDING DEALER RANGE, CENTRAL, PR# 1050
      * BAH 001017 ADDED CENTS TO PAYOFF BALANCE, SMOKY PR# 1331 (OK BY AMTH)
      *                       AND AVAILABLE
      * BAH 050118 ADDED CHECK OF BW-CONFIDENTIAL, PR# 2343
      * BAH 060718 ENTER PAYOFF DATE ON SCREEN TO BE USED IN AVAILABLE CREDIT
      *            COMPUTATION, DEFAULTS TO SYSTEM DATE, ACE PR# 2813
      * BAH 121220 ADDED ZERO BAL ENTRY, MOTOR CREDIT CORP PR# 4179
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181203 GLOBAL LNFILE
      * BAH 2020-0702 MOVED OPEN-BR-FILE #TST001
      * JKC 2021-0823 ADDED LOGIC TO RESTORE EXTERNAL REPORT LINES
      *          FIELD THAT WAS ALTERED; IT USED TO BE AN LOCAL/INTERNAL
      *          FIELDS BUT NOW IS EXTERNAL WHICH NEEDS TO BE RESTORED.
      *
      * JKC 2022-0222 AUTO-POPULATE BEG-BRANCH & END-BRANCH WITH USER'S
      *               BRANCH SINCE THIS PROGRAM ONLY EXECUTES USING
      *               THE USER'S BRANCH.
      *
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * MB  20240625 CORRECT BEG-BRANCH END-BRANCH NO VALUE S35Q-73
      *              ADDED STATE PARAMETER ARRAY PROCESSING
      *              REMOVED USE OF WORK FILE
      *              ADDED STORED PROCEDURE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *********************************
      *  ALPHA WORK FILE
      *********************************
      * FD  WK-FILE
      *     LABEL RECORDS ARE STANDARD.
      * 01  WK-REC.
      *    03  WK-KEY.
      *        05  WK-BRNO         PIC 9(4).
      *        05  WK-CLASS        PIC 9(3).
      *        05  WK-LNAME        PIC X(16).
      *        05  WK-ACCTNO       PIC 9(8).
      *    03  WK-AVAIL            PIC 9(7)V99.
      *    03  WK-POFF             PIC 9(7)V99.
      *    03  WK-HIGH             PIC 9(7).
      *    03  WK-SSNO             PIC 9(10).
 
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/AVAILC.CBL S35 02/20/24-15:25:26 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
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
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-ZBAL                   PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW            PIC X  VALUE "N".

       01  WK-IFN                      PIC XX       VALUE "WK".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99   VALUE 0.
       01  LN-COUNT             PIC 999  VALUE 99.
       01  PAGE-NUMBER          PIC 9(4) VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  TOTAL-PAYOFF         PIC S9(9)    COMP-3 VALUE 0.
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  HOLD-BRNO            PIC 9(4)     VALUE 0.
       01  HOLD-CLASS           PIC 9(3)     VALUE 0.
       01  C-S-Z                PIC X(85).
       01  STATE                PIC XX.
       01  ZIP                  PIC X(10).
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".
       01  WK-AVAIL            PIC 9(7)V99.
       01  WK-POFF             PIC 9(7)V99.
       01  WK-HIGH             PIC 9(7).
       01  WK-ACCTNO           PIC 9(8).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-NUMBER      PIC 9(8).
           03  END-NUMBER      PIC 9(8).
           03  BEG-AVAIL       PIC 9(6).
           03  END-AVAIL       PIC 9(6).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  POFF-DATE       PIC 9(8).
           03  ZBAL            PIC X.

       01  VERSION-CONTROL     PIC X VALUE "D".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  FILLER           PIC X(30)    VALUE
           "CREDIT AVAILABILITY REPORT".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(13).
           03  FILLER      PIC X(7)     VALUE " ".
           03  FILLER      PIC X(4)     VALUE "FOR ".
           03  H-RUNDATE   PIC 99/99/99.

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-LCNO      PIC 99.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(13).

       01  HEAD5A.
           03  FILLER      PIC X(44)    VALUE " ".
           03  FILLER      PIC X(10)    VALUE "ACCT".
           03  FILLER      PIC X(4)     VALUE "HIGH".
       01  HEAD5B.
           03  FILLER      PIC X(31)    VALUE " NAME/ADDRESS ".
           03  FILLER      PIC X(32)    VALUE
               "PHONE       NUMBER    CREDIT    ".
           03  FILLER      PIC X(18)    VALUE
               "PAYOFF  AVAILABLE".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-NAME      PIC X(26).
           03  FILLER      PIC XX.
           03  D-PHONE     PIC 999/999/9999.
           03  FILLER      PIC XXX.
           03  D-NUMBER    PIC Z(6).
           03  FILLER      PIC XXX.
           03  D-HCREDIT   PIC Z(5)9.
           03  FILLER      PIC X.
           03  D-PAYOFF    PIC Z(6)9.99.
           03  FILLER      PIC X(2).
           03  D-AVAIL     PIC Z(5)9.99.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(60).
           03  RANGE-X REDEFINES RANGE-SELECTION.
               05  RANGE-DEALER PIC 9(4).99.
               05  FILLER       PIC X(53).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AVAILC_DEF.CPY".
       COPY "LIBLP/AVAILC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/AVAILC_SCN.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.  *> WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
      *     CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AVAILC" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.   *>  SSP LN BW
           IF IO-BAD       *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *        GO TO WK-ROUTINE.
              GO TO END-ROUTINE.

      *     IF LN-OWNBR > END-BRANCH
      *        GO TO WK-ROUTINE.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-NUMBER OR LN-ACCTNO > END-NUMBER
              GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

      * THESE "IFS" ARE IN LPAVCR, SO DONT BOTHER WITH LPPOFF IF
      *  MEET THESE
           IF LN-CREDITLIM = 0
                   OR (LN-PLDATE NOT = 0)
                    OR (LN-JDDATE NOT = 0)
                     OR (LN-REPOCD = "X" OR "S")
                      OR (LN-BNKRPTDATE NOT = 0)
                       OR (LN-CONFIDENTCD = "Y")
              GO TO NEXT-LN.

      * ONLY WANT CURRENT OPEN OR PAID ACCOUNTS, DONT PRINT PB'S
           IF LN-POCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              GO TO NEXT-LN.

           IF ZBAL = "O"
              IF ( LN-CURBAL NOT = 0 ) OR
                 ( LN-INTBAL NOT = 0 ) OR
                 ( LN-LCBAL  NOT = 0 ) OR
                 ( LN-OTHBAL NOT = 0 )
                     GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN.

      *     MOVE LN-SSNO(1) TO BW-SSNO WK-SSNO.
      *      PERFORM GET-BW.
           IF BW-CONFIDENTIAL = "Y"
              GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-BAD
                 GO TO NEXT-LN.

           IF BR-NO NOT = LN-OWNBR
              MOVE LN-OWNBR TO BR-NO
              PERFORM GET-BR.

           MOVE POFF-DATE     TO AVCR-PAYDATE.
           MOVE LN-MAKERCD(1) TO AVCR-MAKERCD.
           MOVE " "           TO AVCR-POFF-FG.
           PERFORM COMPUTE-AVAILABLE-CREDIT.

      * IF WDT
           IF BR-AVAIL-CREDIT = "S"
              IF AVCR-CURRENT-AVAIL < BEG-AVAIL OR > END-AVAIL
                 GO TO NEXT-LN
              ELSE
                 NEXT SENTENCE
           ELSE
              IF AVCR-AVAILCREDIT < BEG-AVAIL OR > END-AVAIL
                 GO TO NEXT-LN.

      *     MOVE LN-OWNBR   TO WK-BRNO.
      *     MOVE BW-LNAME   TO WK-LNAME.
      *     MOVE LN-SSNO(1) TO WK-SSNO.
      *     MOVE LN-CLASS   TO WK-CLASS.
           MOVE LN-ACCTNO  TO WK-ACCTNO.

           MOVE AVCR-AVAILCREDIT TO WK-AVAIL.
           MOVE AVCR-POFF-NETDUE TO WK-POFF.
           MOVE LN-CREDITLIM     TO WK-HIGH.

      *     MOVE BW-LNAME TO WK-BW-LNAME.
      *     MOVE BW-FNAME TO WK-BW-FNAME.
      *     MOVE BW-ADR1  TO WK-BW-ADR1.
      *     MOVE BW-ADR2  TO WK-BW-ADR2.
      *     MOVE BW-CITY  TO WK-BW-CITY.
      *     MOVE BW-STATE TO WK-BW-STATE.
      *     MOVE BW-ZIP   TO WK-BW-ZIP.
      *     MOVE BW-PHONE TO WK-BW-PHONE.
      *     MOVE BW-PRIOR-RN-FOR10-DATE TO 
      *               WK-BW-PRIOR-RN-FOR10-DATE.
      *     MOVE BW-CONFIDENTIAL TO WK-BW-CONFIDENTIAL.
      *
      *     PERFORM WRITE-WK-FILE.
      *
      *     GO TO NEXT-LN.

      * WK-ROUTINE.
      *     PERFORM CLOSE-WK-FILE.
      *
      *     PERFORM OPEN-WK-FILE.
      *     MOVE SPACES TO WK-LNAME.
      *     MOVE 0      TO WK-BRNO WK-CLASS WK-ACCTNO.
      *     PERFORM START-WK-FILE.
      * NEXT-WK.
      *     PERFORM READ-WK-FILE-NEXT.
      *     IF NOT IO-GOOD
      *        GO TO END-ROUTINE.

      *     MOVE WK-SSNO TO BW-SSNO.
      *     PERFORM GET-BW.
      *     IF NOT IO-GOOD
      *        GO TO NEXT-WK.

      *     MOVE WK-BW-LNAME TO BW-LNAME.
      *     MOVE WK-BW-FNAME TO BW-FNAME.
      *     MOVE WK-BW-ADR1  TO BW-ADR1.
      *     MOVE WK-BW-ADR2  TO BW-ADR2.
      *     MOVE WK-BW-CITY  TO BW-CITY.
      *     MOVE WK-BW-STATE TO BW-STATE.
      *     MOVE WK-BW-ZIP   TO BW-ZIP. 
      *     MOVE WK-BW-PHONE TO BW-PHONE.
      *     MOVE WK-BW-PRIOR-RN-FOR10-DATE TO BW-PRIOR-RN-FOR10-DATE.
      *     MOVE WK-BW-CONFIDENTIAL TO BW-CONFIDENTIAL.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-BRNO
              MOVE LN-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

      *     IF WK-BRNO NOT = HOLD-BRNO
      *        MOVE WK-BRNO  TO HOLD-BRNO
      *        MOVE WK-CLASS TO HOLD-CLASS
      *        PERFORM SET-CLASS-HEADINGS
      *        PERFORM SET-BRANCH-HEADINGS.

           IF LN-CLASS NOT = HOLD-CLASS
              MOVE LN-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

      *     GO TO NEXT-WK.
           GO TO NEXT-LN.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "TOTAL NET PAYOFF........................." TO D-NAME.
           MOVE TOTAL-PAYOFF TO D-PAYOFF.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE AVAILC-QUEUE-POS  TO Q-POS.
           MOVE AVAILC-COPIES-POS TO C-POS.
           MOVE AVAILC-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * TO PREVENT A SPLIT OF THE BORROWER AND ADDRESS
           SUBTRACT 4 FROM EXT-CONAME-RPT-LINES.
           MOVE 0 TO TOTAL-PAYOFF.

      * CREATE INDEX KEY FILE FOR ALPHA OUTPUT
      *    MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *    PERFORM MKTEMP-CALL.
      *    IF STAT NOT = "00"
      *       MOVE "CANNOT MAKE WORK KEY FILE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.
      *
      *     PERFORM OPEN-WK-FILE-OUTPUT.
      *     PERFORM CLOSE-WK-FILE.
      *     PERFORM OPEN-WK-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-LC1-FILE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "NO GLOBAL RECORD FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS     TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CLASS     TO QLN3-WEND-CLASS.
           MOVE BEG-NUMBER    TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE END-NUMBER    TO QLN3-WEND-ACCTNO.

           MOVE ZBAL          TO PASS-ZBAL.

           PERFORM START-LN3-FILE.   *>  SSP LN BW

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE POFF-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-RUNDATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           PERFORM ARRAYSP-LOAD.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE2 TO POFF-DATE.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO ENTER-ZBAL.

      * FORCE USER'S BRANCH
           MOVE "SNNN040BR1." TO VDU.    
           MOVE EXT-FILPATH-BRNO TO VDUNUM0 BEG-BRANCH END-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU. 
           MOVE EXT-FILPATH-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1<"
             MOVE "BEG" TO CLASS-FG
             PERFORM CLASS-SCAN
             IF NOT IO-GOOD
             GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO AC-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AC-01.
           MOVE VDUNUM0 TO BEG-NUMBER.
       AC-02.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AC-02.
           MOVE VDUNUM0 TO END-NUMBER.
           IF BEG-NUMBER > END-NUMBER
              PERFORM INVALID-RANGE
              GO TO AC-01.
           GO TO AV-01.
       ALL-AC.
           MOVE "SNNN060AC1." TO VDU.
           MOVE 0 TO BEG-NUMBER VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999 TO END-NUMBER VDUNUM0.
           PERFORM SCREENIO.
       AV-01.
           MOVE "ENNN060AV1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-AV.
           IF VDUSTATUS = "1U" GO TO AC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AV-01.
           MOVE VDUNUM0 TO BEG-AVAIL.
       AV-02.
           MOVE "ENNN060AV2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AV-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AV-02.
           MOVE VDUNUM0 TO END-AVAIL.
           IF BEG-AVAIL > END-AVAIL
              PERFORM INVALID-RANGE
              GO TO AV-01.
           GO TO DL-01.
       ALL-AV.
           MOVE "SNNN060AV1." TO VDU.
           MOVE 0 TO BEG-AVAIL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AV2." TO VDU.
           MOVE 999999 TO END-AVAIL VDUNUM0.
           PERFORM SCREENIO.
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO AV-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO ENTER-POFF-DATE.

       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

       ENTER-POFF-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE "S  8000POFFDATE." TO VDU
           MOVE DATE-YYYYMMDD TO POFF-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "ERNM060POFFDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-POFF-DATE.

           MOVE VDU-DATE-YYYYMMDD TO POFF-DATE.

       ENTER-ZBAL.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-POFF-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ZBAL.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ENTER-ZBAL.

       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060AC1." TO VDU.
           MOVE BEG-NUMBER TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N060AC2." TO VDU.
           MOVE END-NUMBER TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N060AV1." TO VDU.
           MOVE BEG-AVAIL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060AV2." TO VDU.
           MOVE END-AVAIL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.

      ****************************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                INTO D-NAME.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO D-PHONE
              INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE WK-HIGH   TO D-HCREDIT.
           MOVE WK-POFF   TO D-PAYOFF.
           ADD WK-POFF    TO TOTAL-PAYOFF.
           MOVE WK-AVAIL  TO D-AVAIL.
           MOVE WK-ACCTNO TO D-NUMBER.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      * OTHER PRINTS ARENT USING WRITE-DETAIL-LINE SO THEY DONT
      * DO TOP OF FORM BETWEEN THE NAME AND ADDRESS

           MOVE BW-ADR1 TO D-NAME.
           ADD 1 TO LN-COUNT.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO DETAIL-LINE.

           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2 TO D-NAME
              ADD 1 TO LN-COUNT
              MOVE DETAIL-LINE TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE SPACES TO DETAIL-LINE.

           MOVE BW-CITY  TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP   TO ZIP.
           PERFORM CAT-CSZ.
           MOVE C-S-Z    TO D-NAME.
           ADD 1 TO LN-COUNT.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************

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
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5A TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5B TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 5 TO LN-FEED.
           MOVE "BEGINNING BRANCH   :" TO RANGE-LINE.
           MOVE BEG-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING NUMBER   :" TO RANGE-LINE.
           MOVE BEG-NUMBER             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-NUMBER             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING AVAIL    :" TO RANGE-LINE.
           MOVE BEG-AVAIL              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-AVAIL              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DEALER             TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-DEALER             TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *******************************************************
       CLASS-SCAN SECTION.
      *******************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

       COPY "LIBLP/ARRAYSP.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO AVAILC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AVAILC-SYSDATE.
           DISPLAY AVAILC-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AVAILC-FORM-FIELDS TO WR-BUF.
           MOVE AVAILC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE AVAILC-HELP--FILE TO HELP-LINK-FILE.
           MOVE AVAILC-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AVAILC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
      * COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       GET-BR.
           PERFORM READ-BR-FILE.
       GET-BW.
           PERFORM READ-BW1-FILE.
           IF NOT IO-GOOD
              MOVE "INVALID" TO BW-LNAME.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
       SET-CLASS-HEADINGS.
           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           MOVE LC-DESC TO H-LCDESC.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO BR-NO H-BRNO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           MOVE BR-NAME TO H-BRDESC.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
      * COPY "LIBGB/GBWKIN.CPY".

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
       START-LN3-FILE.    *>  SSP  LN BW
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_AVAILC_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR, 
                    :QLN3-WBEG-CLASS, :QLN3-WEND-CLASS, 
                    :QLN3-WBEG-ACCTNO,:QLN3-WEND-ACCTNO,
                    :PASS-ZBAL) 
                END-EXEC.

      *     SELECT LNFILE.LN_OWNBR,
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
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_CONFIDENTCD,
      *            LNFILE.LN_ALLOTCD,
      *            LNFILE.LN_STATUSFG,
      *            LNFILE.LN_SALEFIN,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_ADDON_FG,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            LNFILE.LN_LC_CODE,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PURDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE    AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE    AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDUDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_ENDORATE,
      *            LNFILE.LN_JDRATE,
      *            LNFILE.LN_ACCRATE,
      *            LNFILE.LN_REDURATE,
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
      *            CAST(LNFILE.LN_INSEXP_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9 AS VARCHAR(10)),
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
      *            LNFILE.LN_MIP_FG,
      *            CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_DLNO,
      *            CAST(LNFILE.LN_LCPDTH_DATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
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
      *            LNFILE.LN_ESCROW_FG,
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_LBOX,
      *            BWFILE.BW_OWNBR,
      *            BWFILE.BW_SSNO,
      *            BWFILE.BW_LNAME,
      *            BWFILE.BW_FNAME,
      *            BWFILE.BW_ADR1,
      *            BWFILE.BW_ADR2,
      *            BWFILE.BW_CITY,
      *            BWFILE.BW_STATE,
      *            BWFILE.BW_ZIP,
      *            BWFILE.BW_PHONE,
      *            CAST(BWFILE.BW_LAST_RN_FOR10_DATE AS VARCHAR(10)),
      *            BWFILE.BW_CONFIDENTIAL
      *     FROM DBO.LNFILE LNFILE
      *    INNER JOIN DBO.BWFILE BWFILE
      *       ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR
      *      AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1
      *      AND BWFILE.BW_CONFIDENTIAL != ''Y''
      *      WHERE LNFILE.LN_OWNBR       = @WBEG-OWNBR
      *        AND LNFILE.LN_CLASS BETWEEN @WBEG-CLASS   AND @WEND-CLASS
      *        AND LNFILE.LN_ACCTNO BETWEEN @WBEG-ACCTNO AND @WEND-ACCTNO 
      *        AND LNFILE.LN_POCD NOT IN (''**'',''*I'',''*R'', ''*U'',''PB'',''RN'',''SC'',''RB'',''RO'')
      *        AND LNFILE.LN_CREDITLIM   != 0
      *        AND LNFILE.LN_PLDATE      = ''1900-01-01''
      *        AND LNFILE.LN_JDDATE      = ''1900-01-01''
      *        AND LNFILE.LN_BNKRPTDATE  = ''1900-01-01''
      *        AND LNFILE.LN_CONFIDENTCD != ''Y''
      *        AND LNFILE.LN_REPOCD NOT IN (''X'', ''S'') '
      *   IF @ZBAL  = 'O'
      *        ' AND LNFILE.LN_CURBAL = 0
      *            AND LNFILE.LN_INTBAL = 0  
      *            AND LNFILE.LN_LCBAL  = 0 
      *            AND LNFILE.LN_OTHBAL = 0 '
      *   IF @ZBAL  = 'S'
      *        ' AND LNFILE.LN_CURBAL != 0
      *            AND LNFILE.LN_INTBAL != 0  
      *            AND LNFILE.LN_LCBAL  != 0 
      *            AND LNFILE.LN_OTHBAL != 0 '
      *    ORDER BY LNFILE.LN_CLASS, BWFILE.BW_LNAME, LNFILE.LN_ACCTNO 

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *>  SSP LN BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LN3_CURSOR 
               INTO :QLN-OWNBR,
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
                   :QLN-CONVERCD,
                   :QLN-POCD,
                   :QLN-CONFIDENTCD,
                   :QLN-ALLOTCD,
                   :QLN-STATUSFG,
                   :QLN-SALEFIN,
                   :QLN-LOANTYPE,
                   :QLN-ADDON-FG,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LC-CODE,
                   :QLN-LOANDATE,
                   :QLN-PURDATE ,
                   :QLN-INTDATE ,
                   :QLN-CASHDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE     ,
                   :QLN-JDDATE   ,
                   :QLN-PLDATE   ,
                   :QLN-ACCELDATE,
                   :QLN-REDUDATE ,
                   :QLN-POFFDATE ,
                   :QLN-BNKRPTDATE,
                   :QLN-APRATE,
                   :QLN-EFFRATE,
                   :QLN-SMPRATE,
                   :QLN-OVRDRATE,
                   :QLN-ENDORATE,
                   :QLN-JDRATE,
                   :QLN-ACCRATE,
                   :QLN-REDURATE,
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
                   :QLN-MIP-FG,
                   :QLN-MIP-CLPDTH-DATE,
                   :QLN-MIP-AHPDTH-DATE,
                   :QLN-DLNO,
                   :QLN-LCPDTH-DATE ,
                   :QLN-INTPDTH-DATE,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-LCPAID,
                   :QLN-LCPARTIALS,
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
                   :QLN-ESCROW-FG,
                   :QLN-DATE-PAID-LAST,
                   :QLN-CREDITLIM,
                   :QLN-OLDACCTNO,
                   :QLN-PRLNNO,
                   :QLN-SUM-FINCHG,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-LBOX,
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-ADR1,
                   :QBW-ADR2,
                   :QBW-CITY,
                   :QBW-STATE,
                   :QBW-ZIP,
                   :QBW-PHONE,
                   :QBW-LAST-RN-FOR10-DATE,
                   :QBW-CONFIDENTIAL 
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
              PERFORM GET-BW-FIELDS.

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


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *     MOVE MKTEMP-BUF TO ACCESS-BUF.
      *     PERFORM REMOVE-WORKFILE.

      * RESTORE EXTERNAL REPORT LINES FIELD THAT WAS ALTERED
           ADD 4 TO EXT-CONAME-RPT-LINES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPRPMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AVAILC-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
