       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPL0.
       DATE-WRITTEN.    012384.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   OPEN/CLOSE CASH DRAWERS
      *
      *    CLOSE/UPDATE DAY; CALLS CL/CK1497 TO BALANCE GL 1497000
      *                      TYPE "YES" TO CONTINUE; CALLS LONPR0 - BANK BAL
      *                      LONPR0;
      *                         CALLS LP/LPEOD5 FOR RECAP AND GL UPDATE
      *                         CALLS SP/EORUN WHICH RUNS THE BRANCH END OF
      *                             DAY BATCH WHICH IS CURRENTLY "QED"
      *                    
      *
      *  REV:
      *  BAH 050195 ADDED TEST OF ALL RENEWALS BEING DONE
      *  BAH 050195 ADDED OPTIN TO REMOVE ONE SIDED RENEWAL
      *             RECORDS IN THE PD-FILE
      *  KEC 102795 ADDED F3-SCAN FOR SCANNING CA-FILE.
      *  KEC 102795 INCREASED MSEL IN THE FORM FOR THE INCREASE IN THE
      *             ENTER DRAWER LEGEND.
      *  BAH 072696 ADDED GP-BATCH-MORE-THAN-DAY TO NOT FORCE YOU TO
      *             RELEASE DEALER BATCH CHECKS DAILY LIKE MERCURY (INSTANT)
      *  BAH 112596 ADDED TEST ON CX-RESCISSION, DONT STOP THE DAY
      *             FROM CLOSING IF THESE CX RECORDS EXIST.. (#401)
      *  MJD 120596 ADDED TEST OF OVER/SHORT SO THAT IT DOES NOT OVERFLOW
      *             TD-OVERSHORT (PIC S9(5)V99)
      *  JTG 012397 CHANGED ROUTINE SEARCHING FOR BATCH DT'S
      *             RE: GP-BATCH-MORE-THAN-DAY = "Y"
      *             TO READ ON DT3 KEY TO AVOID READING THE ENTIRE FILE
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *   CS 112497 ADDED CA-OPENID TO IDENTIFY LOGIN THAT OPENED CASH DRAWER
      *  MJD 112497 EXPANDED CA-REC TO 108 BYTES
      *  BLV 050798 CHECK FOR BULK TRANSFER IN PROGRESS TAGS & STOP
      *             CLOSING IF FOUND
      *  BLV 050798 ADD BRANCH# TO BPEOD PATH FOR CHECKING BATCH PMTS
      *  BAH 062498 CHANGED CA-TILLNO TO ALPHANUMERIC, ADDED CA-TILLNO-ZERO
      *  BLV 980924 ADDED CA-NET-CASH-NOTES TO CLOSING ROUTINES
      *  CS  222798 ADDED TEST OF BR-ALPHA-DRAWERS TO FORCE CASH DRAWER 1 - 9
      *  BAH 041399 IF BR-AUTO-OPEN-BAL = "Y", YOU CANT HAVE ALPHA DRAWERS
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRWAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  GLB 990810 RESET PASSWORD BEFORE VERIFICATION ENTRY
      *  GLB 990811 FIX BUG; DONT RE-READ CA-FILE IF NEW-FLAG = 2
      *  090199 GLF 1: CHANGED CASCANW - MOVED WINDOW DOWN 3 LINES
      *             2: CHANGED CASCAN.CMD - MADE SEPARATE LEGENDS FOR EACH
      *                SCAN TYPE
      *             3: CHANGED CASCAN - MADE SURE PROPER LEGEND DISPLAYS FOR
      *                EACH ENTRY FIELD ACCORDING TO SCAN TYPE
      *             PER JTG
      *  MJD 000222 ADDED BR-HIDE-END-BAL (WORLD PR#219) IF SET TO YES THEN
      *             HIDE ENDING BALANCES AND WRITE TRW RECORD IF OVERSHORT
      *             NOT = 0
      *  MJD 000309 ADDED LOGIC TO STOP USERS FROM HITTING <F1> KEY WHILE
      *             IN THE BALANCED BY FIELD (4) WHEN BR-HIDE-END-BAL = "Y"
      *  BLV 000725 IF RECORD UNCHANGED, DON'T UPDATE BRANCH OPEN BAL;
      *             BUG WAS: DRAWER IS CLOSED & UPDATED.  USER-A CALLS
      *             UP DRAWER, USER-B CALLS UP DRAWER & CHANGES DEPOSIT
      *             & UPDATES CAFILE & BRFILE.  THEN USER-A HITS RETURN
      *             WITHOUT CHANGING ANYTHING - SO CA WAS SKIPPED BUT
      *             BRFILE WAS UPDATED TO OPENING BAL BEFORE USER-B'S
      *             CHANGE BUT CAFILE REFLECTS USER-B'S CHANGE
      *  BLV 021021 IF I-AM-WORLD, CALL PROGRAM CL/CK1497 TO READ
      *             TODAY'S TR'S & VERIFY THAT GL ACCOUNT 1497 WILL NET
      *             TO 0, OTHERWISE DON'T ALLOW CLOSE, WORLD PR# 304
      *  BLV 021113 FIX CHECK BATCH ROUTINE TO CORRECTRLY CHECK FOR
      *             REVERSALS BASED ON NEW WAY THAT IT WORKS, REGWAN
      *             PL# 384
      *  BLV 030211 CHECK GLOBAL PARAMETER FOR EARLIEST CLOSING TIME; IF
      *             SET, CHECK FOR PASSWORD IF TRANS DATE = SYS DATE AND
      *             CURRENT TIME EARLIER THAN ALLOWED, REGACC PR# 0018
      *  BLV 040609 PUT BACK LOGGING WHEN DRAWER IS REOPENED - LOGS
      *             DRAWER # AND OPENBAL IN CHG LOG
      *  BAH 051222 PASSWORD ENTRY WAS ALLOWING <ENTER> THRU RE-ENTRY
      *             AFTER PRESSING F1 TO TRY AND GET OUT, THEN USER DID NOT
      *             KNOW WHAT THE PASSWORD WAS...WORLD PR# 447
      *              ENTER A NEW PASSWORD FIRST TIME IN, THEN ON THE RE-ENTER
      *              THEY REALIZED THEY ARE IN WRONG DRAWER, AND PRESS F1,
      *              WHICH WOULD DO NOTHING, THEN THEY PRESSED <ENTER> AND
      *              IT ACCEPTED IT AND TOOK THEM TO DESCRIPTION. BUT THE
      *              PASSWORD WAS SPACES AND PAY POST DOES NOT ALLOW SPACES
      *  BAH 060322 IF GP-LOAN-ACH-PROCESSING = Y, CHECK FOR EXISTING
      *             ACH INPUT RECORDS FOR THIS BRANCH & DATE IN EWFILE;
      *             IF FOUND, DON'T ALLOW CLOSING OF DAY, LENDMARK PR# 2711
      *  BAH 061107 ADDED CHECK ON CX-RESCISSION = "U" IF THE CX-DATE DOES
      *             NOT MATCH THE TRANS-DATE, IT STILL NEEDS TO STOP YOU
      *             FROM CLOSING. THESE ARE CHECKS THAT CAME FROM A LOAN THAT
      *             WAS JUST UPDATED OUT OF RESCISSION AND THEY MUST PRINT
      *             THESE CHECKS. THEY WERE FALLING THRU CAUSE THE CX-DATE
      *             IS BACK WHEN THE LOAN WAS BOOKED, SO WE NOW FLAG THEM
      *             WITH CX-RESCISSION = "U" SO WE CAN FIND THEM, REGWAN PL#556
      *   CS 111122 WAS ALLOWING YOU TO CLOSE DAYS WITH OUTSTANDING LOAN (NON
      *             RECISSION) CHECKS. HAPPENED WHEN BR-ALLOW-SYS-AS-EFFDATE
      *             'Y', LOAN WAS BACKDATED, CX HAD DATE OF LOAN SO IT HIT TEST
      *             IF CX-DATE NOT = TRAN DATE, CX-RECISSION NOT "U", SO IT
      *             GOES TO NEXT CX AND ALLOWS A CLOSE REGMGM PR#763
      *  BAH 140516 REMOVED REL FROM BPEOD-CMD, C$TOLOWER ON BPEOD-PATH
      *  BAH 140516 REMOVED BULKEOD-CMD, C$TOLOWER ON BULKEOD-PATH
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  BAH 160307 CHANGED DRAWER TO 9(2) NOT PIC X. PD0003
      *  BLM 160922 IF BR-ALLOW-SCANNED-CKS IS "Y", DISPLAY SOME PROMPTS
      *             WITHOUT "CASH" AND ENTER CHECK DEPOSIT AMT, 
      *             WORLD PR# 1188
      *  BLM 161014 FIX TO POPULATE SPANISH PROMPTS, WORLD PR# 1188
      *  BLM 161019 SUBRACT CHECK-DEPOSIT FROM BRANCH OPEN BAL
      *             ALLOW ENTRY OF OPEN BAL IF AUTO-OPEN & TILL # IS >
      *             21, ONLY UPDATE BRANCH IF TILL# < 21, WORLD PR# 1188A
      *  BLM 161115 DON'T SUBTRACT CA-CHECK-DEPOSIT FROM BRANCH OPENING
      *             BAL UNLESS BR-ALLOW-SCANNED-CKS = Y, COULD CAUSE
      *             OPENING BALANCE OF -202020, SUPPORT PL# 954
      *  KEC 2016.1128 CHANGED EW-TRANS-DATE TO BE ACTIVE CCYYMMDD.
      *                ADDED LIBGB/DATER (DATERW WAS ALREADY IN HERE).  NOT
      *                SURE WHY DATER WAS NOT IN HERE AND HARDCODED THE
      *                TWO PARAGRAPHS.  CHANGED IT TO NOW USE DATER.
      * BAH 170629 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0912 PR#1246 WORLD, WHEN ADDING THIS PR# TO A30, I NOTICED THAT
      *               A COPY MEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *               - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *               - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *               - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      * BAH 2018.1010 GLOBAL CXFILE
      * KEC 2018.1023 CONVERTED EWFILE FOR A32 & A34 
      * BAH 2018.1029 GLOBAL DTFILE
      * BAH 2019.0111 GLOBAL PDFILE
      * BAH 2019.0919 REMOVED ACCESS-CALL ON EWFILE
      *  CS 2019.1010 REMOVE /USR/TMP/BULKEOD FILE ONCE YOU HAVE 
      *               DETERMINED A BULK IN PROGESS, IT WILL BE RECREATED
      *               ON ATTEMPT TO CLOSE
      * BAH 2019.1028 FLEXIBLE WEEKEND SCHEDULE, NEED TO GIVE BRANCHES,
      *               THE OPTION OF CLOSING EARLY ON WEEKENDS ONLY, #1396
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS AND 
      *               BR-STATE = "MX"
      * BAH 2023.1011 REMOVED EWFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPL0.CBL S35 02/20/24-13:07:03 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".

      *=================================================================
      *   CACH-REC FROM 'REPLACING LEADING' ON CA-REC.    CHANGED-CA-REC
      *   CAOR-REC FROM 'REPLACING LEADING' ON CA-REC.   ORIGINAL-CA-REC
      *=================================================================
       COPY "LIBLP/LP01CA.CPY" REPLACING LEADING "CA" BY "CACH".
       COPY "LIBLP/LP01CA.CPY" REPLACING LEADING "CA" BY "CAOR".

       01  HOLD-CA-REC                 PIC X(CA-SIZE).

      *-----------------------------------------------------------------
       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  SUB             PIC 9(3) VALUE 0.
       01  TRW-SUB         PIC 9(3) VALUE 0.

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  EDIT-LN-ACCTNO  PIC Z(8).
       01  DEFAULT-BRANCH  PIC 9(4)      VALUE 0.
       01  CASH-WK         PIC S9(7)V99  VALUE +0.
       01  ENTER-BALBY     PIC X         VALUE "N".
       01  ENTER-DEPOSIT   PIC X         VALUE "N".
       01  ENTER-2ND-DEPOSIT PIC X         VALUE "N".
       01  TILL-CLOSED     PIC X         VALUE "N".
       01  VALID-CLOSE     PIC X.
       01  HOLD-DUE        PIC S9(9)V99.

       01  DRAWER-LEGEND.
           03  FILLER      PIC X(40)     VALUE
               "ENTER DRAWER #, F3-SCAN, F4-BALANCING AI".
           03  FILLER      PIC X(25)     VALUE
               "DS, F7-MENU, F6-CLOSE DAY".

       01  REOPEN-MESS     PIC X(24)   VALUE "CASH DRAWER ALREADY OPEN".

       01  C-MSG.
           03  FILLER      PIC X(9)      VALUE "DRAWER # ".
           03  C-TILLNO    PIC Z9.
           03  FILLER      PIC X(17)     VALUE " WAS NOT CLOSED !".

       01  DT-MSG.
           03  FILLER      PIC X(9)      VALUE "DEALER # ".
           03  D-DT-DLNUM  PIC ZZZ9.99.
           03  FILLER      PIC X(9)      VALUE "DEALER # ".
           03  D-DT-NUMBER PIC ZZZZZ9.
           03  FILLER      PIC X(19)     VALUE " WAS NOT RELEASED !".

       01  OPEN-MSG.
           03  FILLER      PIC X(22)     VALUE "DRAWER WILL OPEN WITH ".
           03  OPEN-BAL    PIC $(7).99.

       01  HOLD-OPENBAL    PIC 9(6).99.

      ******************************************************************
      *
      *   S C R E E N   B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-010       PIC X(30).
           03  D-020       PIC Z,ZZZ,ZZ9.99-.
           03  D-021       PIC X(8).
           03  D-022A      PIC X(13).
           03  D-022       REDEFINES D-022A
                           PIC Z,ZZZ,ZZ9.99-.
           03  D-023A      PIC X(13).
           03  D-023       REDEFINES D-023A
                           PIC Z,ZZZ,ZZ9.99-.
           03  D-024A      PIC X(13).
           03  D-024       REDEFINES D-024A
                           PIC Z,ZZZ,ZZ9.99-.
           03  D-030       PIC Z,ZZZ,ZZ9.99-.
           03  D-031       PIC X(8).
           03  D-032       PIC Z,ZZZ,ZZ9.99-.
           03  D-040       PIC X(10).
           03  D-050       PIC Z,ZZZ,ZZ9.99-.
           03  D-060       PIC Z,ZZZ,ZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48)  VALUE
           "010 020 021 022 023 024 030 031 032 040 050 060.".

       01  PROMPT-BUF.
           03  PROMPT2     PIC X(34).
           03  PROMPT2A    PIC X(34).
           03  PROMPT2B    PIC X(34).
           03  PROMPT3     PIC X(34).
           03  PROMPT3A    PIC X(34).
           03  PROMPT5     PIC X(34).
           03  PROMPT6     PIC X(34).
       01  PROMPT-LIST.
           03  FILLER      PIC X(26)  VALUE
           "PROMPT2 PROMPT2A PROMPT2B ".
           03  FILLER      PIC X(17)  VALUE
           "PROMPT3 PROMPT3A ".
           03  FILLER      PIC X(16)  VALUE
           "PROMPT5 PROMPT6.".

      ******************************************************************
      *
      *   E L E M E N T   S P E C S
      *
      ******************************************************************

      * GLB  07/22/99
       COPY "LIBGB/CAPASSWDW.CPY".
       01  CAPASSWD-MLIST      PIC X(4) VALUE "NEW.".
       01  CAPASSWD-MBUF       PIC X(24).

       01  MAX             PIC 99   VALUE 5.
       01  SPEC-TABLE.
      * 1  CA-DESC
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 2  CA-BEGBAL
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 3  CA-ENDBAL
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 4  CA-BALBY
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 5  CA-CHECK-DEPOSIT
           03  FILLER      PIC X(8) VALUE "E  S072N".
      * 6  CA-DEPOSIT
           03  FILLER      PIC X(8) VALUE "E  S072N".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 6 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      *********************************
      * BULK XFER EOD CHECK  - COMMANDS
      *********************************
      * "/$REL/SH/EC /$REL/SH/BULKEOD.SH ".
       01  BULKEOD-CMD.
           03  FILLER              PIC X    VALUE "/".
           03  BULKEOD-REL         PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  BULKEOD-EC          PIC X(3) VALUE "EC ".
           03  FILLER              PIC X    VALUE "/".
           03  BULKEOD-REL2        PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  BULKEOD-SHELL       PIC X(11) VALUE "BULKEOD.SH ".
           03  BULKEOD-BR          PIC 9(4).
           03  FILLER              PIC X    VALUE " ".
           03  BULKEOD-DATA        PIC X(18).

       01  BULKEOD-PATH.
           03  FILLER          PIC X(16) VALUE
               "/USR/TMP/BULKEOD".
           03  BULKEOD-PATH-BR PIC 9999.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBCL/CK1497W.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/CASCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPL_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONPL_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPL_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPL" TO VDU-FORM-NAME.

      * TO GET THE FORM TO DISPLAY
      *    PERFORM FORM-RESET.

      *    CALL FLDVAL USING STAT PROG-LIST PROG-BUF.
      *    IF STAT NOT = "00"
      *       PERFORM FORM-IT
      *       GO TO IO-ERROR.

           MOVE BRANCH TO DEFAULT-BRANCH BPEOD-BR.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

      *-----------------------------------------------------------------
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           MOVE SPACES TO CAPASSWD-OKAY.
           IF BR-PASSWORD-DRAWER NOT = "Y"
              GO TO MAIN-CONTINUE.

           PERFORM ENTER-CASH-DRAWER-PASSWORD.
           IF CAPASSWD-OKAY = "N"
              GO TO MAIN-MODULE.

           IF NEW-FLAG = 1
              MOVE "S  A000NEW." TO VDU
              MOVE "*** NEW ***" TO VDUBUF
              PERFORM SCREENIO.

       MAIN-CONTINUE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       ENTER-KEYS SECTION.
      ******************************************************************

           PERFORM SEND-BRANCH-NAME.
           IF IO-FG = 9
              GO TO ENTER-ALTERNATE-BR.

           GO TO ENTER-KEYS-NO.

       ENTER-ALTERNATE-BR.
           MOVE "ENTER BRANCH #, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              PERFORM DESTROY-LONPL-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-ALTERNATE-BR
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTER-KEYS.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ALTERNATE-BR.

           IF VDUNUM0 = 0
              GO TO ENTER-ALTERNATE-BR.

           MOVE VDUNUM0 TO DEFAULT-BRANCH BPEOD-BR.
           PERFORM SEND-BRANCH-NAME.
           IF IO-FG = 9
              GO TO ENTER-ALTERNATE-BR.

      *-----------------------------------------------------------------
      *    ENTERING TILL NUMBER
      *-----------------------------------------------------------------
       ENTER-KEYS-NO.
           MOVE 0              TO NEW-FLAG.
           MOVE SPACES         TO IO-TYPE.
           MOVE DRAWER-LEGEND  TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N020CANO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "16" )
              PERFORM LOAD-CA1-FILE
              MOVE CA-ALL-PATH    TO CASCAN-CA-PATH
              MOVE DEFAULT-BRANCH TO CASCANW-BRNO
              MOVE TRANS-DATE     TO CASCAN-DATE
              MOVE 0              TO CASCAN-TILLNO 
              MOVE "L"            TO CASCAN-TYPE
              MOVE "WI/CASCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CASCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF ( CASCAN-STATUS NOT = "00" )
                 GO TO ENTER-KEYS-NO
              ELSE
                 MOVE CASCAN-CA-KEY  TO HOLD-CA1-KEY
                 MOVE CASCAN-CA-PATH TO CA-ALL-PATH
                 MOVE HOLD-CA-TILLNO    TO CA-TILLNO
                 MOVE HOLD-CA-BRNO      TO CA-BRNO
                 MOVE HOLD-CA-DATE      TO CA-DATE
                 PERFORM DISPLAY-KEY
                 GO TO READ-CAFILE.

           IF VDUSTATUS = "1>"
              PERFORM DESTROY-LONPL-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-KEYS
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTER-KEYS.


           IF VDUSTATUS = "18"
              PERFORM BALAID-WINDOW
              GO TO ENTER-KEYS-NO.

           IF VDUSTATUS = "1<"
              PERFORM CLOSE-DAY
              GO TO ENTER-KEYS-NO.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.

           MOVE VDUNUM0 TO CA-TILLNO HOLD-CA-TILLNO.
           IF CA-TILLNO = 0
              GO TO ENTER-KEYS-NO.
      * ONLY ALLOW UP TO 35 FOR NOW BECAUSE IN TRANS POSTING, UNTIL WE HAVE
      * SPACE, THEY ONLY HAVE 1 CHARACTER FOR A DRAWER AND 32 WILL BE "W"
      * 33 WILL BE "X", ETC.. ONLY CAN DO UP TO 35 FOR "Z"
           IF CA-TILLNO > 35
              MOVE "CAN ONLY USE UP TO 35" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

      *
      *    IF BR-ALPHA-DRAWERS = "N"
      *       IF CA-TILLNO NOT NUMERIC
      *          MOVE "CASH DRAWERS 1 - 9 ONLY" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO ENTER-KEYS-NO
      *       ELSE
      *          NEXT SENTENCE
      *    ELSE
      *       IF CA-TILLNO NOT NUMERIC AND CA-TILLNO NOT
      *                                       ALPHABETIC-UPPER
      *          GO TO ENTER-KEYS-NO
      *       ELSE
      *          IF BR-AUTO-OPEN-BAL = "Y"
      *             IF CA-TILLNO ALPHABETIC-UPPER
      *                MOVE
      *             "CAN'T HAVE ALPHA DRAWERS WITH AUTO OPEN BALANCES"
      *                    TO MESS
      *                PERFORM SEND-MESS
      *                GO TO ENTER-KEYS-NO.

           MOVE DEFAULT-BRANCH TO CA-BRNO HOLD-CA-BRNO.
           MOVE TRANS-DATE TO CA-DATE HOLD-CA-DATE.

      *-----------------------------------------------------------------
       READ-CAFILE.
           MOVE SPACES TO IO-TYPE.
           MOVE "N" TO CHFG(3) CHFG(4) CHFG(5) CHFG(6).
           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.

           IF IO-FG = 0
              MOVE CA-REC TO CAOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE "O" TO CAPASSWD-STATUS
           ELSE
              MOVE "N" TO CAPASSWD-STATUS
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS.
           EXIT.

      *********************************************************
      *     PASSWORD/TEST THE CASH DRAWER KEY BRNO,DATE,TILL
      *     "N"  =  NEW CASH DRAWER
      *     "O"  =  OLD CASH DRAWER
      *     "M"  =  MODIFY PASSWORD TO OLD DRAWER
      *********************************************************
       ENTER-CASH-DRAWER-PASSWORD SECTION.
      *********************************************************

       SETUP-CASH-DRAWER-MSG.
           MOVE "S  A000MSEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO CAPASSWD-BUF.
           MOVE SPACES TO CAPASSWD-OKAY.
           IF CAPASSWD-STATUS = "N"
              MOVE "ENTER DRAWER PASSWORD:" TO CAPASSWD-MBUF
           ELSE
              MOVE "ENTER OLD PASSWORD:" TO CAPASSWD-MBUF.
           MOVE CAPASSWD-MBUF TO WR-BUF.
           MOVE CAPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-CASH-DRAWER-PASS1.
           MOVE "E  A040CAPASSWD." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO CAPASSWD-BUF.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CASH-DRAWER-PASS1.

           IF CAPASSWD-STATUS = "N"
              IF CAPASSWD-BUF = SPACES
                 MOVE "PASSWORD REQUIRED, PLEASE TRY AGAIN" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-CASH-DRAWER-PASS1
              END-IF
              GO TO VERIFY-UNIQUE-PASSWORD
           ELSE
              GO TO TEST-CASH-DRAWER-PASS.
       VERIFY-UNIQUE-PASSWORD.
           MOVE CA-REC TO HOLD-CA-REC.
           MOVE CA1-KEY TO HOLD-CA1-KEY.

           PERFORM FIND-CASH-DRAWER.
           MOVE HOLD-CA1-KEY TO CA1-KEY.
           IF NEW-FLAG  = 0
              PERFORM OPEN-CA1-FILE
              PERFORM READ-CA1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR
              END-IF
              PERFORM CLOSE-CA1-FILE.

           MOVE HOLD-CA-REC TO CA-REC.

           IF CAPASSWD-OKAY = "Y"
              MOVE CAPASSWD-BUF TO CA-PASSWORD
              GO TO ENTER-CASH-DRAWER-PASS2.

           MOVE "PASSWORD IS NOT UNIQUE, TRY AGAIN!" TO MESS.
           MOVE "N" TO CAPASSWD-OKAY.

           GO TO EXIT-CASH-DRAWER-PASSWORD.

       ENTER-CASH-DRAWER-PASS2.
           MOVE "RE-ENTER NEW PASSWORD:" TO CAPASSWD-MBUF.
           IF CAPASSWD-STATUS = "M"
              MOVE "RE-ENTER OLD PASSWORD:" TO CAPASSWD-MBUF.
           MOVE CAPASSWD-MBUF TO WR-BUF.
           MOVE CAPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO CAPASSWD-BUF.
           MOVE "E  A040CAPASSWD." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO CAPASSWD-BUF.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CASH-DRAWER-PASS2.

       TEST-CASH-DRAWER-PASS.
           IF CAPASSWD-UNIVERSAL NOT = SPACES
              IF CAPASSWD-BUF = CAPASSWD-UNIVERSAL
                 MOVE CA-PASSWORD TO CAPASSWD-BUF.
           IF CAPASSWD-BUF = CA-PASSWORD
              IF CAPASSWD-STATUS = "M"
                 MOVE "N" TO CAPASSWD-STATUS
                 GO TO SETUP-CASH-DRAWER-MSG
              END-IF
              MOVE "Y" TO CAPASSWD-OKAY
              GO TO EXIT-CASH-DRAWER-PASSWORD.
           IF CAPASSWD-STATUS = "N"
              MOVE "NEW PASSWORD VERIFICATION MISMATCH, TRY AGAIN!"
              TO MESS
           ELSE
              MOVE "PASSWORD MISMATCH, MODIFICATIONS DENIED !!"
              TO MESS.
           PERFORM SEND-MESS.
           MOVE "N" TO CAPASSWD-OKAY.
       EXIT-CASH-DRAWER-PASSWORD.
           MOVE SPACES TO CAPASSWD-BUF CAPASSWD-MBUF.

      **********************************************************
       FIND-CASH-DRAWER SECTION.
      **********************************************************
           MOVE " " TO CAPASSWD-OKAY.
           PERFORM OPEN-CA1-FILE.

           MOVE HOLD-CA1-KEY TO CA1-KEY.

           MOVE 0            TO CA-TILLNO.

           IF CA-DATE = 0
              MOVE 19000101  TO CA-DATE.

           PERFORM START-CA1-FILE.
       NEXT-CA-PASSWORD.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9 OR (CA-BRNO NOT = HOLD-CA-BRNO)
              GO TO NO-MATCH-PASSWORD.

           IF CA-DATE NOT = HOLD-CA-DATE
              GO TO NO-MATCH-PASSWORD.

           IF CA-PASSWORD NOT = SPACES
              IF CA-PASSWORD = CAPASSWD-BUF
                 GO TO MATCH-PASSWORD.

           GO TO NEXT-CA-PASSWORD.

       MATCH-PASSWORD.
           IF CA-TILLNO NOT = HOLD-CA-TILLNO
              MOVE "PASSWORD ALREADY RESERVED !" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO CAPASSWD-OKAY
              GO TO EXIT-FIND-CASH-DRAWER.

           MOVE "Y" TO CAPASSWD-OKAY.
           GO TO EXIT-FIND-CASH-DRAWER.
       NO-MATCH-PASSWORD.
           MOVE "Y" TO CAPASSWD-OKAY.

       EXIT-FIND-CASH-DRAWER.
           PERFORM CLOSE-CA1-FILE.

      ******************************************************************
       BALAID-WINDOW SECTION.
      ******************************************************************
           MOVE "WI/BALAID" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.


      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
      *-----------------------------------------------------------------
       BEGIN-ENTRY.
           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.
           IF ENTER-BALBY = "Y"
              MOVE "N" TO ENTER-BALBY
              MOVE 4 TO ELE
              GO TO ENTER-ELE.

           IF ENTER-DEPOSIT = "Y"
              MOVE "N" TO ENTER-DEPOSIT
              MOVE 5 TO ELE
              GO TO ENTER-ELE.

           IF ENTER-2ND-DEPOSIT = "Y"
              MOVE "N" TO ENTER-2ND-DEPOSIT
              MOVE 6 TO ELE
              GO TO ENTER-ELE.

           IF NEW-FLAG = 0 AND TILL-CLOSED = "N"
              MOVE " " TO CHFG(3) CHFG(4)
              IF BR-AUTO-OPEN-BAL = "Y"
                 MOVE " " TO CHFG(5)
                 IF BR-ALLOW-SCANNED-CKS = "Y"
                    MOVE " " TO CHFG(6).

           MOVE "S  A000MSEL." TO VDU.
           MOVE "<ENTER>=ACCEPT, F3-REOPEN, F5-PASSWD, FIELD#" TO VDUBUF.
           IF BR-PASSWORD-DRAWER NOT = "Y"
              MOVE "<ENTER>=ACCEPT, F3-REOPEN, FIELD #" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS = "1:"
              IF BR-PASSWORD-DRAWER = "Y"
                 GO TO MODIFY-DRAWER-PASSWORD.

           IF VDUSTATUS NOT = "16"
              GO TO BYPASS-TILL-REOPEN.

           MOVE "S  A000MSEL." TO VDU.
           MOVE "F1-CANCEL, RE-OPEN CASH DRAWER? (YES/NO):"
                                                     TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TILL-REOPEN.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO TILL-REOPEN.

           IF VDUBUF = "    NO"
              GO TO MODIFY-ROUTINE.

           IF VDUBUF NOT = "   YES"
              GO TO TILL-REOPEN.

           IF CA-BALBY = " "
               MOVE REOPEN-MESS TO MESS
               PERFORM SEND-MESS
               GO TO MODIFY-ROUTINE.

           MOVE 0 TO CA-ENDBAL CA-CLOSED-HHMM CA-DEPOSIT
                               CA-CHECK-DEPOSIT.
           MOVE SPACES TO CA-BALBY.
           MOVE "RWR" TO IO-TYPE.
           PERFORM UPDATE-REOPEN-BR-FILE.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       MODIFY-DRAWER-PASSWORD.
           MOVE "M" TO CAPASSWD-STATUS.
           PERFORM ENTER-CASH-DRAWER-PASSWORD.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF CAPASSWD-OKAY = "N"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           GO TO MODIFY-ROUTINE.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       BYPASS-TILL-REOPEN.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF ELE = 5
              IF BR-AUTO-OPEN-BAL = "Y"
                 IF CA-BALBY NOT = SPACES
                    MOVE " " TO CHFG(ELE).

           IF ELE = 6
              IF (BR-AUTO-OPEN-BAL = "Y" AND BR-ALLOW-SCANNED-CKS = "Y")
                 IF CA-BALBY NOT = SPACES
                    MOVE " " TO CHFG(6).

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           IF ELE = 2
              IF BR-AUTO-OPEN-BAL = "Y"
                 IF CA-TILLNO < 21
                    IF NEW-FLAG = 1
                       GO TO CREATE-ROUTINE
                    ELSE
                       GO TO MODIFY-ROUTINE.

           IF ELE = 2
              IF CA-BALBY NOT = SPACES
                 GO TO CREATE-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F1.
      *    ----------------------------------------------
      *    DO NOT ALLOW F1 FROM FIELD 4 (BALANCED BY) IF
      *    THE HIDE BALANCES OPTION IS TURNED ON.
      *    ----------------------------------------------
           IF ELE = 4 AND F1-KEY AND BR-HIDE-END-BAL = "Y"
              GO TO GOTO-FIELD.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           MOVE 1 TO ONE.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T   E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO CA-DESC.

           IF VDUBUF = " "
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO CA-BEGBAL.
           PERFORM TIME-CALL.
           MOVE H-AND-M TO CA-OPENED-HHMM.
           PERFORM CONVERT-TIME.

           MOVE "S  A000021." TO VDU.
           MOVE TIME-EDIT TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  S072024." TO VDU.
           ADD CA-BEGBAL CA-RUNNING CA-NET-CASH-NOTES GIVING VDUNUM2.
           PERFORM SCREENIO.

           IF CA-BALBY NOT = " "
              COMPUTE CASH-WK = CA-ENDBAL - CA-BEGBAL
                                          - CA-RUNNING
                                          - CA-NET-CASH-NOTES
              MOVE "S  S072032." TO VDU
              MOVE CASH-WK TO VDUNUM2
              PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO CA-ENDBAL.
           PERFORM TIME-CALL.
           MOVE H-AND-M TO CA-CLOSED-HHMM.
           PERFORM CONVERT-TIME.

           MOVE "S  A000031." TO VDU.
           MOVE TIME-EDIT TO VDUBUF.
           PERFORM SCREENIO.

           COMPUTE CASH-WK = CA-ENDBAL - CA-BEGBAL
                                       - CA-RUNNING
                                       - CA-NET-CASH-NOTES.

           IF CASH-WK NOT = 0
              PERFORM UPDATE-TRW-FILE.

      *    -------------------------------------------------------------
      *    TD-OVERSHORT  IS ONLY PIC S9(5)V99.
      *    -------------------------------------------------------------
           IF CASH-WK < -99999.99 OR CASH-WK > 99999.99
              IF CASH-WK > 99999.99
                 MOVE "OVER/SHORT AMOUNT EXCEEDS MAXIMUM OF $99,999.99"
                                                                 TO MESS
              END-IF
              IF CASH-WK < -99999.99
                 MOVE "OVER/SHORT AMOUNT EXCEEDS MINIMUM OF $99,999.99-"
                                                                 TO MESS
              END-IF
              PERFORM SEND-MESS
              MOVE "PLEASE MAKE CORRECTIONS TO ENDING CASH BALANCE"
                                                              TO MESS
              PERFORM SEND-MESS
              MOVE "S  A000031." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE 0 TO CA-ENDBAL CA-CLOSED-HHMM
              GO ENTER-ELE.

           MOVE "S  S072032." TO VDU.
           MOVE CASH-WK TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "Y" TO ENTER-BALBY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO CA-BALBY.

           IF VDUBUF = " "
              GO TO ENTER-ELE.

           MOVE "Y" TO ENTER-DEPOSIT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE VDUNUM2 TO CA-CHECK-DEPOSIT
           ELSE
              MOVE VDUNUM2 TO CA-DEPOSIT.
 
           IF BR-ALLOW-SCANNED-CKS = "Y"
              IF CA-CHECK-DEPOSIT > CA-ENDBAL
                 MOVE "CAN NOT BE MORE THAN ENDING BALANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
           ELSE
              IF CA-DEPOSIT > CA-ENDBAL
                 MOVE "CAN NOT BE MORE THAN ENDING BALANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 COMPUTE OPEN-BAL = CA-ENDBAL - CA-DEPOSIT
                 MOVE OPEN-MSG TO MESS
                 PERFORM SEND-MESS.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE "Y" TO ENTER-2ND-DEPOSIT.
           GO TO CREATE-ROUTINE.
 
      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM2 TO CA-DEPOSIT.
 
           IF (CA-DEPOSIT + CA-CHECK-DEPOSIT) > CA-ENDBAL
              MOVE "TOTAL DEPOSITS CAN'T EXCEED ENDING BALANCE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
 
           COMPUTE OPEN-BAL = CA-ENDBAL - CA-DEPOSIT - CA-CHECK-DEPOSIT.
           MOVE OPEN-MSG TO MESS.
           PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-CA1-FILE.
           MOVE HOLD-CA1-KEY TO CA1-KEY.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CA-REC TO CACH-REC.

           IF CAOR-REC = CACH-REC
              GO TO EXIT-WRITE-MODULE.

           PERFORM READ-CA1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF CA-REC NOT = CAOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CACH-REC TO CA-REC.
           PERFORM REWRITE-CA1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-CA1-FILE.

           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           IF BR-AUTO-OPEN-BAL = "Y" AND CA-TILLNO < 21
              IF CA-BALBY NOT = SPACES
                 PERFORM UPDATE-BR-FILE.

       EXIT-WRITE-MODULE.
           PERFORM CLOSE-CA1-FILE.

      ******************************************************************
       UPDATE-REOPEN-BR-FILE SECTION.
      ******************************************************************
           PERFORM OPEN-BR-FILE.

           IF BR-AUTO-OPEN-BAL NOT = "Y"
              GO TO EXIT-UPDATE-REOPEN-BR-FILE.

           IF CA-TILLNO NOT NUMERIC
              GO TO EXIT-UPDATE-REOPEN-BR-FILE.

           MOVE DEFAULT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.

           MOVE CA-TILLNO TO SUB.
           IF BR-OPEN-BAL(SUB) = CA-BEGBAL
              GO TO EXIT-UPDATE-REOPEN-BR-FILE.

           MOVE CA-BEGBAL TO BR-OPEN-BAL(SUB).
           PERFORM REWRITE-BR-FILE.

           IF IO-FG = 0
              MOVE "C" TO ERRLOGW-ACTION
              MOVE SPACES TO ERRLOGW-KEY
              MOVE CA-BEGBAL TO HOLD-OPENBAL
              STRING DEFAULT-BRANCH," ",CA-TILLNO," ",
                      "REOPEN, OPENBAL=",HOLD-OPENBAL
                 DELIMITED BY "  " INTO ERRLOGW-KEY
              MOVE FORM-PATHNAME TO ERRLOGW-PROGX
              MOVE "CL/ERRLOG" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA
              CANCEL FORM-PROGX.

       EXIT-UPDATE-REOPEN-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CA-FILE.

           MOVE LPLOAN-USERID TO CA-OPEN-USERID.

           MOVE HOLD-CA1-KEY TO CA1-KEY.
           MOVE 0 TO ELE.

           IF BR-AUTO-OPEN-BAL = "Y" AND CA-TILLNO < 21
              MOVE "N" TO CHFG(2)
              MOVE "SN S072020." TO VDU
              MOVE CA-TILLNO TO SUB
              MOVE BR-OPEN-BAL(SUB) TO CA-BEGBAL VDUNUM2
              PERFORM SCREENIO
              PERFORM TIME-CALL
              MOVE H-AND-M TO CA-OPENED-HHMM
              PERFORM CONVERT-TIME
              MOVE "S  A000021." TO VDU
              MOVE TIME-EDIT TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************
           MOVE "SNNN040BRNO."   TO VDU.
           MOVE HOLD-CA-BRNO        TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CANO."   TO VDU.
           MOVE HOLD-CA-TILLNO   TO VDUNUM0.
           PERFORM SCREENIO.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE " " TO DISPLAY-BUF.
           MOVE CA-DESC TO D-010.
           MOVE CA-BEGBAL TO D-020.
           MOVE CA-OPENED-HHMM TO H-AND-M.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT TO D-021.
           IF CA-BALBY = " "
              IF BR-HIDE-END-BAL = "Y"
                 MOVE "*************" TO D-022A D-023A D-024A
              ELSE
                 MOVE CA-RUNNING TO D-022
                 MOVE CA-NET-CASH-NOTES TO D-023
                 ADD CA-BEGBAL CA-RUNNING CA-NET-CASH-NOTES GIVING D-024
              END-IF
           ELSE
              MOVE CA-RUNNING TO D-022
              MOVE CA-NET-CASH-NOTES TO D-023
              ADD CA-BEGBAL CA-RUNNING CA-NET-CASH-NOTES GIVING D-024.

           IF CA-CLOSED-HHMM NOT = 0
              MOVE CA-CLOSED-HHMM TO H-AND-M
              PERFORM CONVERT-TIME
              MOVE TIME-EDIT TO D-031.

           IF CA-BALBY NOT = " "
              MOVE CA-ENDBAL TO D-030
              COMPUTE CASH-WK = CA-ENDBAL - CA-BEGBAL
                                          - CA-RUNNING
                                          - CA-NET-CASH-NOTES
              MOVE CASH-WK TO D-032
              MOVE CA-BALBY TO D-040
              IF BR-AUTO-OPEN-BAL = "Y"
                 IF BR-ALLOW-SCANNED-CKS = "Y"
                    MOVE CA-CHECK-DEPOSIT TO D-050
                    MOVE CA-DEPOSIT TO D-060
                 ELSE
                    MOVE CA-DEPOSIT TO D-050.

           MOVE "Y" TO TILL-CLOSED.

           IF CA-BALBY = " "
              MOVE "N" TO TILL-CLOSED.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       CLOSE-DAY SECTION.
      ******************************************************************

           MOVE "CLOSE VERIFICATION IN PROGRESS1........" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "Y" TO VALID-CLOSE.

           MOVE "CLOSE VERIFICATION IN PROGRESS2........" TO MESS.
           PERFORM SEND-LEGEND.

      * CHECK EARLIEST CLOSE TIME

           IF GP-EARLIEST-CLOSE-TIME NOT NUMERIC
              OR GP-EARLIEST-CLOSE-TIME = 0
                 GO TO CONFIRM-CASH-DRAWERS-CLOSED.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.  
           IF DATE-YYYYMMDD NOT = TRANS-DATE
              GO TO CONFIRM-CASH-DRAWERS-CLOSED.

      * OPERATIONS IS GOING TO A FLEXIBLE WEEKEND SCHEDULE, NEED TO GIVE
      * BRANCHES THE OPTION OF CLOSING EARLY ON WEEKENDS ONLY
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           PERFORM WEEKDAY.
           IF DAY-IS-SAT OR DAY-IS-SUN
              GO TO CONFIRM-CASH-DRAWERS-CLOSED.

           MOVE "Y" TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           IF TIME-NOW < GP-EARLIEST-CLOSE-TIME
              MOVE GP-EARLIEST-CLOSE-TIME TO H-AND-M
              PERFORM CONVERT-TIME
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "N" TO VALID-CLOSE.

       CONFIRM-CASH-DRAWERS-CLOSED.

           PERFORM OPEN-CA1-FILE.

           MOVE DEFAULT-BRANCH TO CA-BRNO.
           MOVE TRANS-DATE     TO CA-DATE.
           MOVE 0              TO CA-TILLNO.

           PERFORM START-CA1-FILE.

       NEXT-CA1-CHECK.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9 OR (CA-BRNO NOT = DEFAULT-BRANCH)
              GO TO END-TILLS.

      *    UNLOCK CA-FILE.

           IF CA-DATE NOT = TRANS-DATE
              GO TO END-TILLS.

           IF CA-BALBY = " "
              MOVE CA-TILLNO TO C-TILLNO
              MOVE C-MSG TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-CLOSE.

           GO TO NEXT-CA1-CHECK.

       END-TILLS.
           PERFORM CLOSE-CA1-FILE.

       CHECK-ALL-CHECKS.

           PERFORM OPEN-CX1-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE SPACES TO CX-TYPE.
           MOVE 0      TO CX-SEQNO.

           PERFORM START-CX1-FILE.

       NEXT-CX1.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = BRANCH)
              GO TO END-CHECKS.

           IF CX-BRNO NOT = BRANCH
              GO TO NEXT-CX1.

      * IF CX-RESCISSION = "U" IT MEANS THESE CHECKS ARE READY TO PRINT
      * AND THEY NEED TO STOP YOU FROM CLOSING THE DAY, THE LOAN WAS
      * 'U'PDATED OUT OF RESCISSION, REGWAN PL# 556
      * THEIR CX-DATE IS BACK WHEN THE LOAN WAS BOOKED
      *    IF CX-DATE NOT = TRANS-DATE
      *       IF CX-RESCISSION NOT = "U"
      *           GO TO NEXT-CX1.


      * DONT LOOK AT CHECKS FROM LOANS IN RESCISSION PERIOD
           IF CX-RESCISSION = "Y"
              GO TO NEXT-CX1.

           MOVE "ALL CHECKS MUST BE PRINTED BEFORE CLOSING !!" TO MESS.
           PERFORM SEND-MESS.
           MOVE "N" TO VALID-CLOSE.

       END-CHECKS.
           PERFORM CLOSE-CX1-FILE.

       CHECK-DEALER-CHECKS.

      * ALLOW DEALER BATCH CHECKS TO BUILD UP
           IF GP-BATCH-MORE-THAN-DAY = "Y"
              GO TO CHECK-ALL-PD-RENEWAL.

           PERFORM OPEN-DT3-FILE.

           MOVE " " TO DT-STATUS.
           MOVE BRANCH TO DT-BRNO.
           MOVE 0 TO DT-NUM DT-CLASS DT-ACCTNO DT-SEQ.
           MOVE 19000101 TO DT-DATE.

           PERFORM START-DT3-FILE.

       NEXT-DT3.
           PERFORM READ-DT3-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-STATUS NOT = " ") OR
                             (DT-BRNO NOT = BRANCH)
              GO TO END-DEALER-CHECKS.

           IF DT-DATE = TRANS-DATE
              IF DT-PAID-BATCH
                 MOVE DT-PROCEEDS TO HOLD-DUE
                 IF DT-REVERSE = "Y"
                    MOVE 0 TO HOLD-DUE
                 END-IF
                 COMPUTE HOLD-DUE = HOLD-DUE - DT-PAYMENTS
                 IF HOLD-DUE NOT = 0
                    MOVE
                    "DEALER CHECKS MUST BE RELEASED BEFORE CLOSING !!"
                    TO MESS
                    PERFORM SEND-MESS
                    MOVE DT-DLNO TO D-DT-DLNUM
                    MOVE DT-ACCTNO TO D-DT-NUMBER
                    MOVE DT-MSG TO MESS
                    PERFORM SEND-MESS
                    MOVE "N" TO VALID-CLOSE
                    GO TO END-DEALER-CHECKS.

           GO TO NEXT-DT3.

       END-DEALER-CHECKS.
           PERFORM CLOSE-DT3-FILE.

       CHECK-ALL-PD-RENEWAL.

           PERFORM OPEN-PD1-FILE.

           MOVE "R"     TO PD-ID
                           QPD1-WBEG-ID
                           QPD1-WEND-ID.
           MOVE BRANCH  TO PD-BRANCH
                           QPD1-WBEG-BRANCH
                           QPD1-WEND-BRANCH.
           MOVE 0       TO PD-SOC-SEC
                           QPD1-WBEG-SOC-SEC.
           MOVE ALL "9" TO QPD1-WEND-SOC-SEC.

           PERFORM START-PD1-FILE.
       NEXT-PD1.
           PERFORM READ-PD1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-PD-RENEWAL.

           IF (PD-BRANCH NOT = BRANCH) OR (PD-ID NOT = "R")
      *       UNLOCK PD-FILE
              GO TO END-PD-RENEWAL.

           MOVE PD-SOC-SEC TO EDIT-LN-ACCTNO.
           STRING "RENEWAL LOAN FOR ACCOUNT NO: " EDIT-LN-ACCTNO
                       " NOT COMPLETED!"
                  DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.

      * LOGIC FOR REMOVAL OF PAD SCRATCH PAD RECORD:
           MOVE "RMRENEWAL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              PERFORM DELETE-RENEWAL.

           MOVE "N" TO VALID-CLOSE.

       END-PD-RENEWAL.
           PERFORM CLOSE-PD1-FILE.

       CHECK-OPEN-BATCHES.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO BPEOD-REL BPEOD-REL2.
           CALL "C$TOLOWER" USING BPEOD-EC, VALUE 3.
           CALL "C$TOLOWER" USING BPEOD-SHELL, VALUE 9.
           MOVE TRANS-DATE TO NUM-DATE.
           MOVE NUM-MO TO BPEOD-MO.
           MOVE NUM-DA TO BPEOD-DA.
           MOVE EXT-FILPATH-FIL TO BPEOD-DATA.
           MOVE BPEOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           CALL "C$TOLOWER" USING BPEOD-PATH, VALUE 14.
           MOVE BRANCH TO BPEOD-PATH-BR.
           MOVE BPEOD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "COMPLETE BATCH PAYS BEFORE CLOSING !" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-CLOSE.

       CHECK-BULK-XFERS-IN-PROGRESS.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO BULKEOD-REL BULKEOD-REL2.
           CALL "C$TOLOWER" USING BULKEOD-EC, VALUE 3.
           CALL "C$TOLOWER" USING BULKEOD-SHELL, VALUE 11.
           MOVE BRANCH TO BULKEOD-BR BULKEOD-PATH-BR.
           MOVE EXT-FILPATH-GLOBAL TO BULKEOD-DATA.
           MOVE BULKEOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           CALL "C$TOLOWER" USING BULKEOD-PATH, VALUE 16.
           MOVE BULKEOD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
             MOVE "BULK TRANSFER UPDATE IN PROGRESS: CANNOT CLOSE!"
                TO MESS
             PERFORM SEND-MESS
             MOVE "N" TO VALID-CLOSE.
      
           CALL "CBL_DELETE_FILE"  USING BULKEOD-PATH
                GIVING STAT-99.

      * VERIFY GL ACCT 1497000 NETS TO 0 BEFORE ALLOWING A BRANCH TO CLOSE

           MOVE 0              TO CK1497-AMOUNT.
           MOVE DEFAULT-BRANCH TO CK1497-BRNO.
           MOVE TRANS-DATE     TO CK1497-TRANS-DATE.
           MOVE "CL/CK1497" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME CK1497-WORKERS.
           CANCEL FORM-PROGX.
           IF CK1497-AMOUNT NOT = 0
              MOVE "GL ACCOUNT 1497000 DOES NOT NET TO ZERO!" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-CLOSE.

       RECHECK-DECISION.
           IF VALID-CLOSE = "N"
              GO TO EXIT-CLOSE.

           MOVE "COMPLETE CLOSING: 'YES' OR 'NO' ?" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO RECHECK-DECISION.

           IF VDUBUF = "    NO"
              GO TO EXIT-CLOSE.

           IF VDUBUF NOT = "   YES"
              GO TO RECHECK-DECISION.

           MOVE "TRANSACTION CLOSING IN PROGRESS........" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R0R" TO NEW-PROG-FORM.
           GO TO END-ROUTINE.

       EXIT-CLOSE.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
       DELETE-RENEWAL SECTION.
      ******************************************************************
           MOVE PD-SOC-SEC TO EDIT-LN-ACCTNO.
           MOVE SPACES TO MESS.
           STRING "REMOVE RENEWAL RECORD FOR ACCTNO: " EDIT-LN-ACCTNO
                       " TYPE----DELETE? "
                  DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-LEGEND.

           PERFORM ALARM.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "00"
              IF VDUBUF = "DELETE"
                 PERFORM DELETE-PD1-FILE
                 IF IO-FG = 0
                    MOVE "RENEWAL RECORD DELETE WAS SUCCESSFUL"
                                                       TO MESS
                    PERFORM SEND-MESS.

       DELETE-RENEWAL-EXIT.
           EXIT.

      *******************************************************************
       UPDATE-TRW-FILE SECTION.
      *******************************************************************
           IF BR-HIDE-END-BAL NOT = "Y"
              GO TO UPDATE-TRW-FILE-EXIT.

           MOVE CA-BRNO   TO TW-BRNO.
           MOVE CA-DATE   TO TW-DATE.
           MOVE CA-TILLNO TO TW-TILLNO.

           PERFORM OPEN-TRW1-FILE.
           PERFORM READ-TRW1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-TRW-FILE
              MOVE CA-BRNO   TO TW-BRNO
              MOVE CA-DATE   TO TW-DATE
              MOVE CA-TILLNO TO TW-TILLNO
              PERFORM WRITE-TRW1-FILE
              IF IO-FG NOT = 0
                 GO TO UPDATE-TRW-FILE-EXIT.

      * FIND FIRST OPEN SLOT (IF THERE IS ONE)
           PERFORM VARYING TRW-SUB FROM 1 BY 1
                   UNTIL TW-TIME(TRW-SUB) = 0 OR TRW-SUB > 20
              CONTINUE
           END-PERFORM.

           IF TRW-SUB = 20 AND TW-TIME(TRW-SUB) NOT = 0
              GO TO UPDATE-TRW-FILE-EXIT.

           MOVE CA-CLOSED-HHMM TO TW-TIME(TRW-SUB).
           ACCEPT TW-POSTNAME(TRW-SUB) FROM ENVIRONMENT "POSTNAME".
           MOVE CA-ENDBAL      TO TW-AMOUNT(TRW-SUB).
           MOVE CASH-WK        TO TW-OVERSHORT(TRW-SUB).
           MOVE CA-CLOSED-HHMM TO TW-TIME(TRW-SUB).

           PERFORM REWRITE-TRW1-FILE.

       UPDATE-TRW-FILE-EXIT.
           PERFORM CLOSE-TRW1-FILE.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPL0" TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1.
           MOVE "TO CLOSE BEFORE &&&&&&&&" TO PASSWDW-PROMPT2.
           INSPECT PASSWDW-PROMPT2 REPLACING FIRST "&&&&&&&&" BY
              TIME-EDIT.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           MOVE LPLOAN-SYSDATE TO LONPL-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPL-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPL-FORM-FIELDS TO WR-BUF.
           MOVE LONPL-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPL-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPL-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPL_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       DESTROY-LONPL-SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPL-SCREEN.
       SEND-LEGEND.
           MOVE "S  A650MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       SEND-BRANCH-NAME.
           MOVE "SN N040BRNO." TO VDU.
           MOVE DEFAULT-BRANCH TO BR-NO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "INVALID BRANCH" TO BR-NAME.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE "02. BEGINNING DRAWER BALANCE     :" TO PROMPT2
              MOVE "+   NET TRANSACTIONS (LOANS)     :" TO PROMPT2A
              MOVE "+   NET TRANSACTIONS (NOTES)     :" TO PROMPT2B
              MOVE "03. ENDING DRAWER BALANCE        :" TO PROMPT3
              MOVE "         OVER/SHORT              :" TO PROMPT3A
              MOVE "05. CHECK DEPOSIT                :" TO PROMPT5
              MOVE "06. CASH DEPOSIT                 :" TO PROMPT6
              MOVE 6                                     TO MAX
           ELSE
              MOVE "02. BEGINNING CASH   BALANCE     :" TO PROMPT2
              MOVE "+   NET CASH TRANSACTIONS (LOANS):" TO PROMPT2A
              MOVE "+   NET CASH TRANSACTIONS (NOTES):" TO PROMPT2B
              MOVE "03. ENDING DRAWER BALANCE        :" TO PROMPT3
              MOVE "         CASH OVER/SHORT         :" TO PROMPT3A
              MOVE "05. DEPOSIT                      :" TO PROMPT5
              MOVE "                                 :" TO PROMPT6
              MOVE 5                                     TO MAX
              MOVE "N"                                   TO CHFG(6).

           IF BR-AUTO-OPEN-BAL NOT= "Y"
              MOVE SPACES TO PROMPT5 PROMPT6
              MOVE "N" TO CHFG(5) CHFG(6).

           MOVE PROMPT-LIST TO WR-LIST.
           MOVE PROMPT-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "S  A400BRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

       TIME-CALL.
           ACCEPT TIME-NOW FROM TIME.

       CONVERT-TIME.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.

           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.

           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.

           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE SPACE TO T-SPACE.

      *-----------------------------------------------------------------
      * MUST REREAD BR-FILE BECAUSE THE OTHER DRAWERS COULD OF CHANGED
      *-----------------------------------------------------------------
       UPDATE-BR-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE DEFAULT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE CA-TILLNO TO SUB.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              COMPUTE BR-OPEN-BAL(SUB) = CA-ENDBAL - CA-DEPOSIT
                                       - CA-CHECK-DEPOSIT
           ELSE
              COMPUTE BR-OPEN-BAL(SUB) = CA-ENDBAL - CA-DEPOSIT.
           PERFORM REWRITE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "BRANCH OPENING BALANCE NOT UPDATED !!!" TO MESS
              PERFORM SEND-MESS
              MOVE "CALL FOR HELP." TO MESS
              PERFORM SEND-MESS.
           PERFORM CLOSE-BR-FILE.

       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCACL.CPY".
       COPY "LIBLP/LPTRWCL.CPY".
       COPY "LIBGB/DATER.CPY".       
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/ZONEDTIMEIO.CPY".

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-DT3-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPDT3RN.CPY".
       COPY "LIBLP/LPTRW1IN.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           PERFORM DESTROY-LONPL-SCREEN.
           EXIT PROGRAM.
