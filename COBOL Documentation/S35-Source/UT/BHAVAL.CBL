       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BHAVAL.
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
      *********************************
      *  ALPHA WORK FILE
      *********************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO      PIC 9(4).
               05  WK-CLASS     PIC 9(3).
               05  WK-LNAME     PIC X(16).
               05  WK-ACCTNO    PIC 9(8).
           03  WK-AVAIL     PIC 9(7)V99.
           03  WK-POFF      PIC 9(7)V99.
           03  WK-HIGH      PIC 9(7).
           03  WK-SSNO      PIC 9(10).

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

       01  WK-IFN        PIC XX       VALUE "WK".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99   VALUE 0.
       01  LN-COUNT         PIC 999  VALUE 99.
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
               PR-FILE WK-FILE.
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
           CLOSE WK-FILE.
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
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO WK-ROUTINE.

           IF LN-ORGST = "OK"
              IF LN-CLASS = 8
                 IF LN-SUBCLASS = 1 OR 2
                    MOVE LN-ACCTNO TO MESS-X-MON-1
                    PERFORM SEND-MESS.

           GO TO NEXT-LN.

           IF LN-OWNBR > END-BRANCH
              GO TO WK-ROUTINE.

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

           MOVE LN-SSNO(1) TO BW-SSNO WK-SSNO.
           PERFORM GET-BW.
           IF BW-CONFIDENTIAL = "Y"
              GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 GO TO NEXT-LN.

           IF BR-NO NOT = LN-OWNBR
              MOVE LN-OWNBR TO BR-NO
              PERFORM GET-BR.

           MOVE POFF-DATE     TO AVCR-PAYDATE.
           MOVE LN-MAKERCD(1) TO AVCR-MAKERCD.
           MOVE " " TO AVCR-POFF-FG.
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


           MOVE LN-SSNO(1) TO WK-SSNO.
           MOVE BW-LNAME TO WK-LNAME.
           MOVE LN-OWNBR TO WK-BRNO.
           MOVE LN-CLASS TO WK-CLASS.
           MOVE LN-ACCTNO TO WK-ACCTNO.

           MOVE AVCR-AVAILCREDIT TO WK-AVAIL.
           MOVE AVCR-POFF-NETDUE TO WK-POFF.
           MOVE LN-CREDITLIM TO WK-HIGH.
           PERFORM WRITE-WK-FILE.

           GO TO NEXT-LN.

       WK-ROUTINE.
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.
           MOVE SPACES TO WK-LNAME.
           MOVE 0      TO WK-BRNO WK-CLASS WK-ACCTNO.
           PERFORM START-WK-FILE.
       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF NOT IO-GOOD
              GO TO END-ROUTINE.

           MOVE WK-SSNO TO BW-SSNO.
           PERFORM GET-BW.
           IF NOT IO-GOOD
              GO TO NEXT-WK.

           IF FIRST-TIME = "Y"
              MOVE WK-BRNO TO HOLD-BRNO
              MOVE WK-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF WK-BRNO NOT = HOLD-BRNO
              MOVE WK-BRNO TO HOLD-BRNO
              MOVE WK-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF WK-CLASS NOT = HOLD-CLASS
              MOVE WK-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-WK.

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
           MOVE AVAILC-QUEUE-POS TO Q-POS.
           MOVE AVAILC-COPIES-POS TO C-POS.
           MOVE AVAILC-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
      D    STOP MESS.

      * TO PREVENT A SPLIT OF THE BORROWER AND ADDRESS
           SUBTRACT 4 FROM EXT-CONAME-RPT-LINES.
           MOVE 0 TO TOTAL-PAYOFF.

      * CREATE INDEX KEY FILE FOR ALPHA OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

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

           PERFORM START-LN3-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
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
           MOVE EXT-FILPATH-BRNO TO VDUNUM0.  
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

           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           MOVE C-S-Z TO D-NAME.
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
       COPY "LIBGB/MKTEMP.CPY".
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
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
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

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
