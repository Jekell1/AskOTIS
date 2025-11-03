       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AUDIQ0.
       DATE-WRITTEN.    013184.
      ***********************************************************
      *                 (LP)
      *  DESCRIPTION:   AUDITOR INQUIRY.
      *
      * REV:
      *  012188 LS  AFTER DISPLAYING LOAN REC,IF OWNING
      *             BRANCH NOT = LOGIN BRANCH, MESSAGE
      *             WILL DISPLAY
      *  052088 BR  DISPLAY PRIORITY BW MEMO IF
      *             BR-PRIO-MEMO-FLAG = "Y"
      *  012489 SLC WHEN SCANNING WE NOW READ SPR FOR EACH
      *             LOAN TO CORRECT DELINQUENCY PROBLEM.
      *  012789 SLC REMOVED COPYFILES LPINDU AND LPINDUW.
      *             THEY ARE NOW PART OF LPRATE.
      *  013089 BAH ADDED SECOND PROMISE DATE TO SCREEN
      *  032789 JTG CHANGED TO DISPLAY LN-MESSAGE IN BOLD
      *  110289 BAH RESET AVAILABLE CREDIT ON SCANNING
      *  113089 SLC DISPLAY LATE CHARGE ALWAYS.
      *  031590 SLC ADDED LISTING OF LOANS WITH MULTIPLE SEQ
      *  040590 JTG CHANGED REFERENCE FROM RECENCY TO RECDEL
      *  040990 JTG ADDED DISPLAY OF LN-TOTNOALDEL (PACESETTER)
      *  052190 JTG ADDED DISPLAY OF AVCR-RESTRICT-FG (GA)
      *  080390 BAH DISPLAY LN-SUM-LNAMT LN-SUM-ORGTERM LN-SUM-FINCHG
      *                     LN-SUM-REGPYAMT
      *  080790 BAH ADD LT-FILE FOR ADDONS (SPPDTH)
      *  091090 BAH CHANGED REFERENCE OF RECDEL BACK TO RECENCY
      *  091390 SLC INITIALIZE LCHG-LATE TO PDTH-CUR-SCHD-PAY.
      *  091490 SLC ADDED LPSCHD(W) TO UNPACK PMT SCHD IN LATE CHARGES.
      *  102190 JTG CHANGED DISPLAY OF TRCD'S
      *  072591 JTG CHANGED LPLT1IN TO LPLT1RN
      *  052792 JTG DISPLAYED CUR-SCHD-PAY INSTEAD OF REGPYAMT
      *  060192 JTG CHANGED TO USE RECDEL NOT RECENCY
      *  072092 BAH ADDED H071 TO DISPLAY "S" IF LN-PAY-SCHLD-FG = Y
      *  100192 BAH ADDED LC-DISPLAY-ERN = 'P', PAYMNTD
      *  123094 BAH ADDED WI-AU-SYSDATE FOR HARD COPY PRINT FROM F9
      *             ALL ACCTS WERE MATURED WITHOUT SYSDATE.
      *  050995 BAH ADDED COMAKER DISPLAY, BEFORE IT TESTS FOR SPOUSE
      *  080895 MJD COPIED SP/CPINQ0.C TO LP/AUDIQ0.C AND REMOVED
      *             F5 OPTION FROM TOP. REPLACED CLMENUW WITH AUMENUW.
      *  081695 JTG ADDED AUDITOR QUEUE LOGIC
      *  083195 KEC ADDED LIBLP/LPNOCL
      *  090495 JTG ADDED TEST FOR GP-NO-LC-IN-ARREARAGE
      *  MJD 042496 CHANGED TO ACCEPT 8 DIGIT ACCOUNT NUMBER FROM BWLNSC.
      *             REPLACED LNDISP WITH MORELN WINDOW.
      *  MJD 071096 ADDED LOGIC TO DISPLAY LCPDTH AS MMDD FOR WEEKLY
      *             PAYFREQ.
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  MJD 071996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  BLL 072596 MOVED MORELNW SO 03 LEVEL WAS UNDER WINDOWSW
      *  BAH 111596 ADDED BR-AVAIL-CREDIT = "S" FOR WDT (335)
      *  KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  MJD 100497 EXPANDED BW-LOANS TO PIC 9(2)
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  JTG 010904 ALLOWED IB TYPE LOANS TO ACCELERATE     JIM R. (ALL) PR#1582
      *  BLV 020628 ADDED GET-NEXT-FG TO AUTOMATICALLY GET NEXT QUESD ACCOUNT
      *             IF STAUS COMES BACHK FROM AUDIQ1.C
      *             CHANGED F4-NEXT TO BE F2-NEXT
      *  BLV 020628 IF WORLD, THEN SET GET NEXT FG TO "GA" SO THAT LP/AUDIQ1.C
      *             KNOWS TO DISPLAY GAINQU WINDOW AUTOMATICALLY THE FIRST TIME
      *             AN ACCOUNT IS BROUGHT UP.
      *  BLF 070919 IF RETURNING FROM F6 SCAN WITH BWLNSC-ARCHIVE-FG = "A",
      *             SET UP ARCHIVE PATHS FOR LN, LP, & LT; RESET TO NORMAL
      *             ../ PATHS AT BEGINNING OF ENTER-KEYS.  WHEN ACCT # ENTERED,       *             IF GP-PURGE-TO-ARCHIVE-FG = "Y", CHECK FOR LOAN IN ARCHIVE
      *             & DISPLAY IF FOUND, WORLD PR# 547
      *  KEC 120212 {PR#03978} PARADATA <A15> [DAVID SINCLAIR]
      *             CREATED THE ABILITY TO CHOOSE THE WAY THE 'MESSAGE'
      *             IS DISPLAYED ON THE ACCOUNT INQUIRY, PAYMENT
      *             POSTING AND COLLECTION WORK SCREENS.
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *   CS 141031 ADDED A 'DESTROY ALL CONTROLS' IN THE FORM RESET, TOOK A
      *             GOOD 10 SECONDS TO CLEAR WHEN I ENTERED ONLY 5 ACCTS AND 
      *             EXITED
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180928 GLOBAL FILES
      * BAH 181126 GLOBAL LHFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 181221 GLOBAL LPFILE
      * AMM 190326 SPLIT OUT AQ FILE, AQS
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20190813 INITIALIZE THE DELINQ-PROF1 TO "X" SINCE THE NON-DISPLAYS
      *              DONT WORK ANYMORE. PROFILES WERE NOT DISPLAYING IN A30
      *              LIKE THEY WERE IN A15. QA0076
      * BAH 2022.0818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 2024.0725 HARD CODE AQ2 STATE FOR SEQNO S35Q-97
      * BAH 2024.0816 DISPLAY XXX-XX-9999 FOR ALL SSNO'S #1673 S35Q-118
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/AUDIQ0 S35 08/16/24-12:00:10 >".
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
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBSP/SP01AQ.CPY".
       COPY "LIBSP/SP01AQ_SQL.CPY".
       COPY "LIBSP/SPWSAQ.CPY".
       COPY "LIBSP/SP01AQS.CPY".
       COPY "LIBSP/SP01AQS_SQL.CPY".
       COPY "LIBSP/SPWSAQS.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".


       01  IO-TYPE           PIC X(3).
       01  FIRST-TIME        PIC 9     COMP VALUE 0.

       01  HOLD-BW-SSNO      PIC 9(11) VALUE 0.
       01  NAME-WORKER       PIC X(40) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.
       01  MAKER-CODE        PIC X     VALUE SPACES.
       01  ENTER-SEQNO       PIC X     VALUE SPACES.
       01  HOLD-NEXT-SEQNO   PIC 9(6)  VALUE 0.
       01  PHONE-EDIT        PIC 9(10) VALUE 0.
       01  PHONE-WORK REDEFINES PHONE-EDIT.
           03  PE-AREA       PIC 999.
           03  PE-PHONE      PIC 9(7).

       01  DIS-BRANCH.
           03  FILLER         PIC X(21)  VALUE
           "A/C OWNED BY BRANCH# ".
           03  DIS-BR         PIC ZZZ9.
       01  HOLD-SSNO         PIC 9(9).
       01  NUM-BW-WK         REDEFINES HOLD-SSNO.
           03  NUM-BW1       PIC 999.
           03  NUM-BW2       PIC 99.
           03  NUM-BW3       PIC 9999.
       01  ALP-BW.
           03  ALP-BW1       PIC 999.
           03  ALP-HYPHEN1   PIC X.
           03  ALP-BW2       PIC 99.
           03  ALP-HYPHEN2   PIC X.
           03  ALP-BW3       PIC 9999.

       01  PROFILE-EDIT      PIC 9(12).
       01  PROFILE-ED REDEFINES PROFILE-EDIT
                             PIC X(12).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  HIGH-BUF.
           03  HIGH-CREDIT-X-BUF         PIC X(13).
           03  HIGH-CREDIT-BUF           PIC Z(6)B.
           03  AVAIL-BUF                 PIC Z(7)9.99B.
           03  AVAIL-SPACE               REDEFINES AVAIL-BUF.
               05  FILLER                PIC X(11).
               05  AVAIL-BUF-RESTRICT-FG PIC X.

       01  AVA-BUF.
           03  AVAIL-X-BUF         PIC X(11).


       01  HIGH-LIST.
           03  FILLER          PIC X(12) VALUE
               "H22 H23 H25.".
       01  HIGH-LIST-ATTR.
           03  FILLER          PIC X(18) VALUE
               "H22:R H23:R H25:R.".

       01  AVA-LIST.
           03  FILLER          PIC X(4) VALUE
               "H24.".
       01  AVA-LIST-ATTR.
           03  FILLER          PIC X(7) VALUE
               "H24:RF.".

       01  DISPLAY-BORR-BUF  VALUE " ".
           03  D-SSNO5       PIC X(7).
           03  D-SSNO        PIC 9(4).
           03  D-LOANS       PIC ZZ.
           03  D-NAME        PIC X(34) OCCURS 4.
           03  D-SP-TAG      PIC X(7).
           03  D-SP-SSNO5    PIC X(7).
           03  D-SP-SSNO     PIC 9(4).
           03  D-SP2         PIC X(25).
           03  D-MEMO        PIC X(25).

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM    PIC X(16).
           03  D-MAKER       PIC X(8).
           03  D-H1          PIC 99/99/99  BLANK ZERO.
           03  D-H2          PIC X(8).
           03  D-H3          PIC 99/99/99  BLANK ZERO.
           03  D-H4          PIC ZZZZ9.99  BLANK ZERO.
           03  D-H5-PMPT.
               05  FILLER    PIC X(3).
               05  D-H5-P1   PIC X.
               05  D-H5-P2   PIC 999.
               05  D-H5-P3   PIC X.
           03  D-H5          PIC 999/9999.
           03  D-H5-RED REDEFINES D-H5.
               05  FILLER    PIC XXX.
               05  D-H5A     PIC X.
               05  FILLER    PIC X(4).
           03  D-H6          PIC ZZZZZ9.99.
           03  D-H7          PIC ZZZZ9.99.
           03  D-H71         PIC X.
           03  D-H8          PIC ZZZZ9.99-.
           03  D-H9          PIC ZZZZ9.99-.
           03  D-B1          PIC X(8).
           03  D-H10         PIC ZZZZ9.99-.
           03  D-H11A.
               05  D-H11     PIC ZZZ9.
           03  D-H12         PIC X(12).
           03  D-H13A.
               05  D-H13     PIC ZZZ9.
           03  D-H14         PIC X(12).
           03  D-H15         PIC ZZZ.ZZ.
           03  D-H16         PIC ZZZ.
           03  D-H17.
               05  D-H17-MO   PIC 99.
               05  D-H17-DAYR PIC 99.
           03  D-H18         PIC 99        BLANK ZERO.
           03  D-H181        PIC 99        BLANK ZERO.
           03  D-H19         PIC ZZ9.999.
           03  D-H20         PIC X(4).
           03  D-COLLCD      PIC XX.
           03  D-COLLID      PIC X(20).
           03  D-PROMDT1     PIC 99/99/99  BLANK ZERO.
           03  D-PROMDT2     PIC 99/99/99  BLANK ZERO.
           03  D-DLRNUM      PIC 9(4).99.
           03  D-PROMAMT     PIC Z(5).ZZ-.
           03  D-RCSTAG      PIC X(3).
           03  D-RCOURS      PIC X(8).
           03  D-NRCOURS     REDEFINES D-RCOURS PIC ZZZZ9.99.
           03  D-DELCNT      PIC ZZ.
           03  D-COLCTID     PIC XXX.

       01  DISPLAY-BAL-BUF   VALUE " ".
           03  FILLER        PIC X(31).
           03  D-PDTH        PIC 9(4).
           03  D-OTHBAL      PIC Z(5).99-.
           03  D-INTBAL      PIC Z(4).99-.
           03  D-CURBAL      PIC Z(5)9.99.

       01  D-MESSAGE         PIC X(30).
       01  MESSAGE-LIST      PIC X(8)  VALUE "MESSAGE.".
       01  MESSAGE-LIST-ATTR PIC X(11) VALUE "MESSAGE:R.".
       01  MESSAGE-ATTR      PIC XX.

       01  DISPLAY-BORR-LIST.
           03  FILLER        PIC X(38) VALUE
           "SOC LOANS A1 A2 A3 A4 SP0 SP1 SP2 P01.".

       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(55) VALUE
           "LNNO LNSEQ MAKER H01 H02 H03 H04 H05A H05 H06 H07 H071 ".
           03  FILLER        PIC X(58) VALUE
           "H08 H09 B1:S H10 H11 H12 H13 H14 H15 H16 H17 H18 H181 H19 ".
           03  FILLER        PIC X(41) VALUE
           "H20 COLLCD COLLID PROMDT1 PROMDT2 DLRNUM ".
           03  FILLER        PIC X(37)  VALUE
           "PROMAMT RCSTAG RCOURS DELCNT COLCTID.".

       01  DISPLAY-BAL-LIST.
           03  FILLER        PIC X(9) VALUE
           "MEMODESC.".

       01  LINE-LIST.
           03  FILLER          PIC X(32)    VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08.".

       COPY "LIBLP/LPTRCD_CMD.CPY".
       01  HEAD-1.
           03  FILLER        PIC X(40) VALUE
           "ACCOUNT #         DATE  LOAN AMT TRM  DE".
           03  FILLER        PIC X(16) VALUE
           "ALER COL PL/JD  ".
           03  DELINQ-PROMPT PIC X(11).
           03  FILLER        PIC X(13) VALUE
           "  BAL/PDOFF M".

      ************************************
      *     C O M M O N   M E M O R Y    *
      ************************************
       COPY "LIBLP/LPLOANW.CPY".

       01  DELINQ-SCAN             VALUE "X  ".
           03  DELINQ-PROF1        PIC X.
           03  DELINQ-PROF2        PIC X.
           03  DELINQ-PROF3        PIC X.

       01  DELINQ-VALUE.
           03  DELINQ-ALP.
               05  DELINQ-NUM             PIC ZZZZ,ZZ9.99- BLANK ZERO.
       01  DELINQ-CODE                    PIC X.

       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBWI/AQSCANW.CPY".

       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".

       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".

      *    LP-FILE NOT USED IN THIS PROGRAM
      *01  LP-TRCD     PIC XX.
      *01  LP-APINT    PIC S9(5)V99 VALUE +0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AUDIQ_DEF.CPY".
       COPY "LIBLP/AUDIQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA.
           03  AUDIQ-PROG-BUF      PIC X(400).
           03  SOC-BUF             PIC X(11)    VALUE SPACES.
           03  SOC-BUF-ED          REDEFINES SOC-BUF.
               05  SOC-ED1         PIC 999.
               05  FILLER          PIC X.
               05  SOC-ED2         PIC 99.
               05  FILLER          PIC X.
               05  SOC-ED3         PIC 9999.
           03  INTERRUPT-FG              PIC X.
           03  AVAILABLE-BUF             PIC X(12).
           03  ARCHIVE-FG                PIC X.
           03  GET-NEXT-FG               PIC X(02).
           03  FLEX-INQ-BUF              PIC 9(3).
           03  AUDIQ-WINDOW-HANDLE PIC X(10).

       SCREEN SECTION.
       COPY "LIBLP/AUDIQ_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************
       MAIN-PROGRAM SECTION.
      *********************************

           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AUDIQ" TO VDU-FORM-NAME.

           MOVE AUDIQ-PROG-BUF TO PROG-BUF.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM FORM-RESET.

           MOVE SPACES TO ERRCD LN-REC.

           MOVE "S  A000TRDATE." TO VDU.
           MOVE EXT-CONAME-SYSDATE TO VDUBUF ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.
           PERFORM SCREENIO.

      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           PERFORM SEND-DELINQ-SCAN.
       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE SPACES TO ROUTE-CD
              MOVE 1 TO FIRST-TIME
           ELSE
              PERFORM FORM-RESET.

           MOVE "S  A220TITLE:FR." TO VDU.
           MOVE "    AUDITOR INQUIRY" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE NOT = "PST"
              GO TO MAIN-MODULE.

           PERFORM GET-LC.
           IF IO-BAD
              MOVE "INVALID CLASS" TO LC-DESC.

           MOVE "S  A010TYP." TO VDU.
           PERFORM SCREENIO.

           MOVE "S  A160SORTKEY." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "1" TO NEW-PROG-SUB.
           MOVE "P" TO ERRCD.

           MOVE "MEMODESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM SET-COLLECTOR-INFO.
           GO TO END-ROUTINE.

      ************************************
      *                                  *
      *    E N T E R   L N - K E Y       *
      *                                  *
      ************************************
       ENTER-KEYS SECTION.

           PERFORM SEND-HEAD-1.
       ENTER-KEYS-NO.
           MOVE "S  A220TITLE:FR." TO VDU.
           MOVE "    ACCOUNT INQUIRY" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "Y" TO ENTER-SEQNO.
           MOVE SPACES TO IO-TYPE LN-REC.
           MOVE "S  A000MSEL." TO VDU.
           STRING
               "F7, F3-CODE, F2-NEXT LOAN, F5-QUEUE, "
                        "F6-SCAN, ENTER LOAN#:"
                          DELIMITED BY SIZE INTO VDUBUF.
           PERFORM SCREENIO.

           IF GET-NEXT-FG NOT = SPACES
              MOVE SPACES TO GET-NEXT-FG
              MOVE "12" TO VDUSTATUS
              GO TO SKIP-TO-NEXT-ACCOUNT.

           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16"
              PERFORM TRCD-DISPLAY
              GO TO ENTER-KEYS-NO.
       SKIP-TO-NEXT-ACCOUNT.
           IF VDUSTATUS = "12"
              PERFORM FIND-NX-ACCT-QUEUED
              IF NOT IO-GOOD
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.
           IF VDUSTATUS = "1:"
              PERFORM AQ-SCAN
              IF NOT IO-GOOD
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF NOT IO-GOOD
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              MOVE "E" TO ERRCD
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.
           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
           GO TO READ-LNFILE1.

       READ-LNFILE1.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
      *       IF GP-PURGE-TO-ARCHIVE-FG NOT = "Y"
                 MOVE "INVALID ACCOUNT NUMBER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEYS-NO.
      *       ELSE
      *          PERFORM CHECK-FOR-LOAN-IN-ARCHIVE
      *          IF IO-BAD
      *             MOVE "INVALID ACCOUNT NUMBER" TO MESS
      *             PERFORM SEND-MESS
      *             GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF BORROWER-KEY NOT NUMERIC
              MOVE 0 TO BORROWER-KEY
            ELSE
              IF BORROWER-KEY NOT = LN-SSNO(1)
                           AND NOT = LN-SSNO(2)
                            AND NOT = LN-SSNO(3)
                             AND NOT = LN-SSNO(4)
                 MOVE 0 TO BORROWER-KEY.
           IF BORROWER-KEY NOT = 0
              MOVE BORROWER-KEY TO BW-SSNO
              MOVE 0 TO BORROWER-KEY
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.

           PERFORM GET-BW.

           MOVE "PST" TO IO-TYPE.
           PERFORM DISPLAY-BORROWER.
           PERFORM DISPLAY-LOAN.
           IF LN-OWNBR NOT = BRANCH
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

           PERFORM UPDATE-AQ-FILE.

       END-ENTER-KEYS.
           EXIT.

      ***********************************************
      *    ALLOW FOR F9 COLLECTOR MENU
      ***********************************************
       SET-COLLECTOR-INFO SECTION.
           MOVE SPACES                    TO EXT-WI-CL-WK-PATH.
           MOVE TRANS-DATE                TO EXT-WI-CL-TRANS-DATE.
           MOVE LN-OWNBR                  TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO                 TO EXT-WI-CL-ACCTNO.
           MOVE LN-SSNO(1)                TO EXT-WI-CL-SSNO(1).
           MOVE LN-SSNO(2)                TO EXT-WI-CL-SSNO(2).
           MOVE LN-SSNO(3)                TO EXT-WI-CL-SSNO(3).
           MOVE LN-SSNO(4)                TO EXT-WI-CL-SSNO(4).
           MOVE LN-MAKERCD(1)             TO EXT-WI-CL-MAKERCD(1).
           MOVE LN-MAKERCD(2)             TO EXT-WI-CL-MAKERCD(2).
           MOVE LN-MAKERCD(3)             TO EXT-WI-CL-MAKERCD(3).
           MOVE LN-MAKERCD(4)             TO EXT-WI-CL-MAKERCD(4).
      * SET OF (WI-CL-SSNO-NO) DO DONE IN 'DISPLAY-LOAN':
           MOVE LN-DLNO                   TO EXT-WI-CL-DLNO.
           MOVE LN-SALEFIN                TO EXT-WI-CL-SALEFIN.
           MOVE ARCHIVE-FG                TO EXT-WI-CL-ARCHIVE-FG.
           MOVE EXT-CONAME-SYSDATE        TO EXT-WI-CL-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO EXT-WI-CL-PASSWD.

      ****************************************
      *    FIND NEXT ACCOUNT NO. IN QUEUE
      ****************************************
       FIND-NX-ACCT-QUEUED SECTION.
           PERFORM OPEN-AQS1-FILE.
           MOVE BRANCH TO AQS-BRNO.
           PERFORM READ-AQS1-FILE.
           IF IO-BAD
              PERFORM CLOSE-AQS1-FILE
              MOVE "PROBLEM FINDING NEXT ACCOUNT IN QUEUE!" TO MESS
              PERFORM SEND-MESS
              GO TO END-FIND-NX-ACCT-QUEUED.

           IF AQS-NEXT-SEQNO > AQS-TOT-REC
              PERFORM CLOSE-AQS1-FILE
              MOVE 9 TO IO-FG
              MOVE "NO MORE RECORDS IN YOUR QUEUE!" TO MESS
              PERFORM SEND-MESS
              GO TO END-FIND-NX-ACCT-QUEUED.

           MOVE AQS-NEXT-SEQNO TO HOLD-NEXT-SEQNO.
           ADD 1 TO AQS-NEXT-SEQNO.
           PERFORM REWRITE-AQS1-FILE.
           PERFORM CLOSE-AQS1-FILE.

           PERFORM OPEN-AQ2-FILE.
           MOVE BRANCH          TO AQ-BRNO
                                   QAQ2-WBEG-BRNO
                                   QAQ2-WEND-BRNO.
           MOVE HOLD-NEXT-SEQNO TO AQ-SEQNO
                                   QAQ2-WBEG-SEQNO
                                   QAQ2-WEND-SEQNO.
           MOVE 19000101        TO AQ-LOANDATE
                                   SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QAQ2-WBEG-LOANDATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QAQ2-WEND-LOANDATE.
           MOVE 0                    TO AQ-ACCTNO
                                        QAQ2-WBEG-ACCTNO.
           MOVE ALL "9"              TO QAQ2-WEND-ACCTNO.
           PERFORM START-AQ2-FILE.
           PERFORM READ-AQ2-FILE-NEXT.
           PERFORM CLOSE-AQ2-FILE.
           IF IO-GOOD
              IF ( AQ-BRNO NOT = AQ-PATH-OWNBR )
                 MOVE 9 TO IO-FG
              ELSE
              IF AQ-WORKED-FG = "Y"
                 GO TO FIND-NX-ACCT-QUEUED
              ELSE
                 MOVE AQ-BRNO   TO LN-OWNBR
                 MOVE AQ-ACCTNO TO LN-ACCTNO.

       END-FIND-NX-ACCT-QUEUED.
           EXIT.

      ***********************************************
      *    UPDATE AUDITOR QUEUE
      ***********************************************
       UPDATE-AQ-FILE SECTION.
           PERFORM OPEN-AQ1-FILE.
           MOVE LN-OWNBR  TO AQ-BRNO.
           MOVE LN-ACCTNO TO AQ-ACCTNO.
           PERFORM READ-AQ1-FILE.
           IF IO-GOOD
              IF AQ-WORKED-FG = " "
                 MOVE "Y" TO AQ-WORKED-FG
                 PERFORM REWRITE-AQ1-FILE
                 MOVE BRANCH TO AQS-BRNO
                 PERFORM OPEN-AQS1-FILE
                 PERFORM READ-AQS1-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 ELSE
                    ADD 1 TO AQS-WORKED-REC
                    PERFORM REWRITE-AQS1-FILE
                    PERFORM CLOSE-AQS1-FILE.
           PERFORM CLOSE-AQ1-FILE.

      ***********************************************
      *    AUDITOR QUEUE SCAN
      ***********************************************
       AQ-SCAN SECTION.
           MOVE EXT-CONAME-USER-LOC TO AQSCAN-BRANCH.
           MOVE "WI/AQSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH AQSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF AQSCAN-STATUS NOT = "00" OR AQSCAN-ACCTNO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-AQ-SCAN.
           MOVE AQSCAN-BRANCH TO LN-OWNBR.
           MOVE AQSCAN-ACCTNO TO LN-ACCTNO.
           MOVE 0 TO IO-FG.

       EXIT-AQ-SCAN.
           EXIT.

      *******************************
       BWLN-SCAN SECTION.
      *******************************
           MOVE 0                  TO BWLNSC-ACCTNO.
           MOVE EXT-CONAME-SYSDATE TO BWLNSC-SYSDATE.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BWLN-SCAN.
           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE BWLNSC-SSNO   TO BW-SSNO BORROWER-KEY.
           MOVE 0             TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

      *****************************************
       DISPLAY-BORROWER SECTION.
      *****************************************

           MOVE " " TO DISPLAY-BORR-BUF.
           MOVE "XXX-XX-" TO D-SSNO5.
           MOVE BW-SSNO  TO D-SSNO HOLD-SSNO.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME(1).
           MOVE BW-ADR1 TO D-NAME(2).
           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2 TO D-NAME(3).
           MOVE " " TO NAME-WORKER.
           MOVE BW-CITY TO NAME-WORKER.
           MOVE BW-STATE TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", *".
           INSPECT NAME-WORKER REPLACING FIRST "*               "
             BY NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY " *".
           MOVE BW-ZIP TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "*               "
             BY NAME.
           IF BW-ADR2 = SPACES
              MOVE NAME-WORKER TO D-NAME(3)
           ELSE
              MOVE NAME-WORKER TO D-NAME(4).
           MOVE BW-LOANS TO D-LOANS.

           IF LN-MAKERCD(2) = "J"
      * TEST THAT YOU DONT HAVE ACCOUNT PULLED UP BY SECOND MAKER !!
              IF LN-SSNO(2) NOT = 0 AND (LN-SSNO(2) NOT = BW-SSNO)
      * SAVE ORIGINAL BORROWER, CANT ASSUME ITS LN-SSNO(1)
                 MOVE BW-SSNO TO HOLD-BW-SSNO
                 MOVE LN-SSNO(2) TO BW-SSNO
                 PERFORM GET-BW
                 IF IO-GOOD
                    MOVE "JOINT :" TO D-SP-TAG
                    MOVE "XXX-XX-" TO D-SP-SSNO5
                    MOVE BW-SSNO   TO D-SP-SSNO
                    MOVE BW-LNAME TO NAME-WORKER
                    MOVE BW-FNAME TO NAME
                  INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?"
                  INSPECT NAME-WORKER REPLACING FIRST "                "
                                                          BY NAME
                  INSPECT NAME-WORKER REPLACING ALL "?" BY " "
                    MOVE NAME-WORKER TO D-SP2
      * GET ORIGINAL BORROWER BACK IN THE BUFFER
                    MOVE HOLD-BW-SSNO TO BW-SSNO
                    PERFORM GET-BW
                    GO TO END-DISPLAY-BORR
                 ELSE
                    MOVE HOLD-BW-SSNO TO BW-SSNO
                    PERFORM GET-BW
                    GO TO END-DISPLAY-BORR.

           IF BW-SPSSNO = 0
            IF BW-SPLNAME = SPACES
             IF BW-SPFNAME = SPACES
              GO TO END-DISPLAY-BORR.

           MOVE "SPOUSE:" TO D-SP-TAG.
           IF BW-SPSSNO NOT = 0
              MOVE "XXX-XX-" TO D-SP-SSNO5
              MOVE BW-SPSSNO TO D-SP-SSNO.
           MOVE BW-SPLNAME TO NAME-WORKER.
           MOVE BW-SPFNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-SP2.
       END-DISPLAY-BORR.
      * BRFILE IS READ IN MAIN-ROUTINE
           IF BR-PRIO-MEMO-FLAG = "Y"
              PERFORM GET-PRIORITY-MEMO.
           MOVE DISPLAY-BORR-BUF TO WR-BUF.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *       GET PRIORITY MEMO
      *************************************
       GET-PRIORITY-MEMO SECTION.

           MOVE BW-SSNO  TO BM-SSNO.
           MOVE 0        TO BM-DATE.
           MOVE 1        TO BM-SEQ.
           PERFORM OPEN-BM1-FILE.
           PERFORM READ-BM1-FILE.
           IF IO-GOOD
              MOVE BM-MEMO TO D-MEMO.
           PERFORM CLOSE-BM1-FILE.

      /****************************************
      *          DISPLAY THE LOAN
      *****************************************
       DISPLAY-LOAN SECTION.

           MOVE " " TO DISPLAY-LOAN-BUF.
           PERFORM GET-SP.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO MAKER-CODE
              MOVE 1 TO EXT-WI-CL-SSNO-NO
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO MAKER-CODE
              MOVE 2 TO EXT-WI-CL-SSNO-NO
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO MAKER-CODE
              MOVE 3 TO EXT-WI-CL-SSNO-NO
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO MAKER-CODE
              MOVE 4 TO EXT-WI-CL-SSNO-NO
              PERFORM DISPLAY-MAKER.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H1.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-H2.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H3.
           MOVE LN-SUM-FINCHG TO D-H4.

           MOVE BW-PHONE TO PHONE-EDIT.
           MOVE "PH:" TO D-H5-PMPT.
           MOVE "(" TO D-H5-P1.
           MOVE PE-AREA TO D-H5-P2.
           MOVE ")" TO D-H5-P3.
           MOVE PE-PHONE TO D-H5.
           MOVE "-" TO D-H5A.

           MOVE LN-SUM-LNAMT TO D-H6.
           MOVE PDTH-CUR-SCHD-PAY TO D-H7.
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE "S" TO D-H71.
           MOVE LN-LYRINTPD TO D-H8.
           MOVE LN-YTDINTPD TO D-H9.
           PERFORM GET-LC.
           PERFORM COMPUTE-EARNINGS.
           IF LC-DISPLAY-ERN = "A"
              MOVE "ACC-EARN" TO D-B1
              MOVE EARN-TOT-ACCERN TO D-H10
           ELSE
           IF LC-DISPLAY-ERN = "U"
              MOVE "UNEARNED" TO D-B1
              MOVE EARN-TOT-UNEARN TO D-H10
           ELSE
           IF LC-DISPLAY-ERN = "T"
              MOVE "TOT-EARN" TO D-B1
              MOVE LN-TOTERN TO D-H10
           ELSE
              IF LC-DISPLAY-ERN = "I"
                 MOVE TRANS-DATE TO POFF-PAYDATE
                 MOVE "M"        TO POFF-MAKERCD
                 MOVE "PO"       TO POFF-LPTRCD
                 MOVE "N"        TO POFF-DEF-STOP
                 PERFORM PAYOFF-LOAN-ROUTINE
                 MOVE "TDUE INT" TO D-B1
                 MOVE POFF-INTDUE TO D-H10
           ELSE
              MOVE " PAYMNTD" TO D-B1
              MOVE LN-TOTPAYMNTD TO D-H10.

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

      * 'X' IS INITIAL FORM VALUE
           IF DELINQ-PROF1 = "X"
              MOVE LC-PROF TO DELINQ-SCAN
              MOVE "D" TO DELINQ-PROF3.

           IF CPOT30 = "Y"
              MOVE "<P>" TO D-H11A
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-H11A
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-H11A
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-H11A
           ELSE
              MOVE CONTRACTUAL TO D-H11.

           MOVE DELINQ-PROF1 TO DELINQ-CODE.
           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO D-H12.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-H13A
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-H13A
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-H13A
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-H13A
           ELSE
              MOVE RECDEL TO D-H13.

           MOVE DELINQ-PROF2 TO DELINQ-CODE.
           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO D-H14.

           MOVE LN-LCPDTH-DATE    TO LCHG-LCPDTH-DATE.
           MOVE PDTH-CUR-SCHD-PAY TO LCHG-LATE LCHG-CURPAY.
           PERFORM LATE-CHARGE-CALCULATION.

           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE LN-SUM-ORGTERM TO D-H16.
           IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                                  AND (LN-JDDATE = 0)
                                   AND (LN-ACCELDATE = 0)
              MOVE LCHG-CHARGE TO D-H15
              MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM TO D-H17-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE DATE-YYYYMMDD-DD TO D-H17-DAYR
              ELSE
                 MOVE DATE-YYYYMMDD-YY TO D-H17-DAYR.

           MOVE LN-TOTNODEF TO D-H18.
           MOVE LN-TOTNOALDEL TO D-H181.
           MOVE LN-OVRDRATE TO D-H19.
           MOVE LN-ACTIONCD TO D-H20.
           MOVE LN-DLNO TO D-DLRNUM.
           MOVE LN-COLLECTOR TO D-COLCTID.
           IF LN-SALEFIN = "Y" AND LN-SERVACCT = "N"
               PERFORM RECOURSE-RTN
           ELSE
               MOVE SPACES TO D-RCSTAG D-RCOURS.
           MOVE LN-DELCNT TO D-DELCNT.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PROMDT1.
           IF BR-SECOND-PROMISE = "Y"
              MOVE LN-PROMDATE2 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PROMDT2
           ELSE
              MOVE 0 TO D-PROMDT2.
           MOVE LN-PROMAMT TO D-PROMAMT.
           MOVE LN-COLLCD TO D-COLLCD.
           MOVE LN-COLLID TO D-COLLID.
           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LN-MESSAGE TO D-MESSAGE.
           PERFORM SET-MESSAGE-ATTR.

           IF LN-MESSAGE = SPACES
              MOVE SPACES TO MESSAGE-ATTR
              MOVE D-MESSAGE TO WR-BUF
              MOVE MESSAGE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE D-MESSAGE TO WR-BUF
              MOVE MESSAGE-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM.

           IF BR-AVAIL-CREDIT = "Y"
              MOVE TRANS-DATE TO AVCR-PAYDATE
              MOVE MAKER-CODE TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
              MOVE " HIGH CREDIT:" TO HIGH-CREDIT-X-BUF
              MOVE " AVAILABLE:" TO AVAIL-X-BUF
              MOVE LN-CREDITLIM TO HIGH-CREDIT-BUF
              MOVE AVCR-AVAILCREDIT TO AVAIL-BUF
              MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
              MOVE HIGH-BUF TO WR-BUF
              MOVE HIGH-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE AVA-BUF TO WR-BUF
              MOVE AVA-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
           IF BR-AVAIL-CREDIT = "S"
              MOVE TRANS-DATE TO AVCR-PAYDATE
              MOVE MAKER-CODE TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
              MOVE " CUR AVAIL:" TO HIGH-CREDIT-X-BUF
              MOVE " AVAILABLE:" TO AVAIL-X-BUF
      * - - - - - - SETTING CUR. AVAILABLE BY HIGH CREDIT IN LOAN RECORD
              MOVE AVCR-AVAILCREDIT TO HIGH-CREDIT-BUF
      * - - - - - - SETTING AVAILABLE BY A NET PAYOFF
              MOVE AVCR-CURRENT-AVAIL TO AVAIL-BUF
              MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
              MOVE HIGH-BUF TO WR-BUF
              MOVE HIGH-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE AVA-BUF TO WR-BUF
              MOVE AVA-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE SPACE TO HIGH-BUF AVA-BUF
              MOVE HIGH-BUF TO WR-BUF
              MOVE HIGH-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE AVA-BUF TO WR-BUF
              MOVE AVA-LIST TO WR-LIST
              PERFORM CALL-WRFORM.


           IF IO-TYPE = "PST"
              PERFORM SEND-LOAN-MESS
              GO TO DISPLAY-LOAN-EXIT.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE TO D-PDTH.
           MOVE LN-OTHBAL TO D-OTHBAL.
           ADD LN-LCBAL LN-INTBAL GIVING D-INTBAL.
           MOVE LN-CURBAL TO D-CURBAL.
           MOVE DISPLAY-BAL-BUF TO WR-BUF.
           MOVE DISPLAY-BAL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM SEND-LOAN-MESS.

       DISPLAY-LOAN-EXIT.
           EXIT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".

       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/GPENV.CPY".


      *************************************
       TRCD-DISPLAY SECTION.
      *************************************
       TRCD-PAGE1.
           MOVE "HEAD:S." TO WR-LIST.
           MOVE TRCD-LINE TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE LINE-LIST TO WR-LIST.
           MOVE TRCD-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       TRCD-ASK.
           MOVE "S  A040MSEL." TO VDU.
           MOVE "<ENTER> - NEXT PAGE, F7-DONE" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "1>"
              MOVE TRCD-BUF2 TO WR-BUF
              MOVE LINE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "S  A040MSEL." TO VDU
              MOVE "<ENTER> - NEXT PAGE, F7-DONE" TO VDUBUF
              PERFORM SCREENIO
              MOVE "E RA000SEL." TO VDU
              PERFORM SCREENIO
              IF VDUSTATUS NOT = "1>"
                 GO TO TRCD-PAGE1.

           MOVE SPACES TO WR-BUF.
           MOVE "L08 L07 L06 L05 L04 L03 L02 L01." TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-HEAD-1.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DESTROY ALL CONTROLS.
           DISPLAY AUDIQ-SCREEN UPON AUDIQ-WINDOW-HANDLE.
           MOVE AUDIQ-FORM-FIELDS TO WR-BUF.
           MOVE AUDIQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE AUDIQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE AUDIQ-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AUDIQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      /********************************
      *   M I S C   P A R A G R A P H *
      *********************************
       MISC-PARAGRAPHS SECTION.
      *LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( LP-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( LP-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
      *       MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
      *       MOVE LP-ALL-PATH             TO LP-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( LP-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO LP-ALL-BASE
      *       MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
      *       MOVE FILPATH-DATA            TO LP-ALL-DATA
      *       MOVE LP-ALL-PATH             TO LP-PATH.
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       GET-DELINQ-VALUE.
           IF DELINQ-CODE = "R"
              MOVE LN-RECPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED TO DELINQ-ALP
           ELSE IF DELINQ-CODE = "C"
              MOVE LN-CONPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED TO DELINQ-ALP
           ELSE
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE TRANS-DATE   TO CEPP2-DATE
              MOVE MAKER-CODE   TO CEPP-MAKERCD
              IF GP-NO-LC-IN-ARREARAGE
                 PERFORM CONTRACTUAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-DELINQUENT GIVING DELINQ-NUM
              ELSE
                 PERFORM TOTAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-ARREARAGE GIVING DELINQ-NUM.
       SEND-DELINQ-SCAN.
      *    X IS INITIAL FORM VALUE
           IF DELINQ-PROF1 = "X"
              MOVE "D" TO DELINQ-PROF3.
           MOVE DELINQ-PROF3 TO DELINQ-CODE.
           IF DELINQ-CODE = "R"
              MOVE "REC/DELINQ" TO DELINQ-PROMPT
           ELSE IF DELINQ-CODE = "C"
              MOVE "CONTRACTUAL" TO DELINQ-PROMPT
           ELSE
              MOVE "DELINQUENCY" TO DELINQ-PROMPT.
       RECOURSE-RTN.
           IF LN-DLRECOURSE = "F"
               MOVE "FULL    " TO D-RCOURS.
           IF LN-DLRECOURSE = "N"
               MOVE "NON     " TO D-RCOURS.
           IF LN-DLRECOURSE = "P"
               MOVE LN-DLPARTRCAMT TO D-NRCOURS.
           MOVE "RC:" TO D-RCSTAG.
       GET-LC.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
       BLINK-STARS.
           MOVE "S  A000STAR1." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR1." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           PERFORM SCREENIO.
       SEND-LOAN-MESS.
           IF LN-MESSAGE NOT = SPACES
              PERFORM ALARM
              PERFORM BLINK-STARS
              PERFORM ALARM.
       DISPLAY-MAKER.
           IF MAKER-CODE = "M"
              MOVE "MAKER" TO D-MAKER.
           IF MAKER-CODE = "J"
              MOVE "JOINT" TO D-MAKER.
           IF MAKER-CODE = "E"
              MOVE "ENDORSER" TO D-MAKER.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-HEAD-1.
           MOVE "S  A000HEAD:S." TO VDU.
           MOVE HEAD-1 TO VDUBUF.
           PERFORM SCREENIO.
       SET-MESSAGE-ATTR.
           IF ( LN-MESSAGE = SPACES )
              MOVE "MESSAGE."              TO MESSAGE-LIST-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-FLASH )
              MOVE "MESSAGE:F."            TO MESSAGE-LIST-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-REVERSE )
              MOVE "MESSAGE:R."            TO MESSAGE-LIST-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-BOTH )
              MOVE "MESSAGE:FR."           TO MESSAGE-LIST-ATTR
           ELSE
              MOVE "MESSAGE."              TO MESSAGE-LIST-ATTR.

       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

      *********************************
      *      F I L E   I / O          *
      *********************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-AQS1-FILE.
           PERFORM CLOSE-AQ2-FILE.
           PERFORM CLOSE-AQ1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBSP/SPAQSGS_SQL.CPY".
       COPY "LIBSP/SPAQGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBM1RN.CPY".
       COPY "LIBSP/SPAQ1IN.CPY".
       COPY "LIBSP/SPAQ2RN.CPY".
       COPY "LIBSP/SPAQS1IN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE SECTION.
       END-RUN.
           IF LN-REC NOT = SPACES
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE 0 TO BORROWER-KEY.
           PERFORM CLOSE-FILES.

      * NECESSARY BECAUSE A15 USED XFER/FLDVAL OF SCREEN FIELD

           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
           MOVE ALP-BW TO SOC-BUF.
           MOVE AVAIL-SPACE TO AVAILABLE-BUF.

           MOVE "GA" TO GET-NEXT-FG.

       EXIT-PROG.
       STOP-RUN.
           MOVE PROG-BUF TO AUDIQ-PROG-BUF.
           EXIT PROGRAM.
