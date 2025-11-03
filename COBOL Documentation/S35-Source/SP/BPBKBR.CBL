       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BPBKBR.
      ******************************************************************
      *
      *                   ( SP/BPBKBR )
      *
      *     DESCRIPTION:  BREAK OUT THE CORE BATCH FILE
      *                   FROM MAG TAPE INTO EACH INDIVIDUAL
      *                   BRANCH FILE, THEN REMOVE THE BTFILE
      * REV.
      *  MJD 990309 COPIED FROM A60 TO REMOVE BANK1 LOGIC.
      *  MJD 990504 FORCE "V" FOR QUEUE WHEN PRINTING.
      *  MJD 990614 CHANGED THAE WAY WE HANDLE DUPLICATE ACCOUNTS.
      *             INSTEAD OF ADDING THGEN TO THE TRAILER RECORD I WILL
      *             BE ADDING THEM TO BP-FILE WITH INVALID SEQNO'S.
      *             TRAILER RECORD WILL CONTAIN TOTAL OF ACCOUNTS WITH
      *             ZERO ACCOUNT NUMBERS.
      *  MJD 000215 FIXED BUG, WE WERE WRITING BP-RECORDS WITH INVALID
      *             LOAN NUMBERS.
      *             EXAMPLE:
      *               ACCT FROM FT KNOX  X'3030 3030 2020 2020'
      *               THIS ACCOUNT WAS WRITTEN IN THE BP-FILE.
      *               LONPW4 STARTS THE FILE AT X'3030 3030 3030 3030'.
      *               THE INVALID RECORD WAS SKIPPED BECAUSE
      *               X'3030 3030 2020 2020" < X'3030 3030 3030 3030'
      *               THERFORE THE ACCOUNT WAS NEITHER UPDATED OF TOSSED OUT
      *               AS AN EXCEPTION BECAUSE THE RECORD WAS NEVER READ.
      *
      *  MJD 000403 ADDED LOGIC TO PROCESS ACH PAYMENTS FROM FORT KNOX
      *             ADDED GPENV LOGIC (TFC PR#78)
      *  MJD 000420 CHANGED EXIT PROGRAM TO BE BPMEN2.C (MERC PR#227)
      *  BAH 010904 PR# 1520 TFC, ALLOW BT-SIGN = 4, ACH, REF = "S####", WHERE
      *             ####=BATCH NO., AND TRCD = "PY"
      *  JTG 021104 BATCH PAYMENT REVISIONS   CLEARED BP-WK-REFCD   REGACC #0006
      *  BLF 050325 CHANGE RENAME OF BTFILE TO RENAME TO SVBT....; WHEN IT
      *             STARTED WITH B CAUSED DUPLICATE CHECK IN LONPW1 &
      *             YOU COULD NOT SELECT A BATCH, WORLD PR# 396
      *  BAH 111205 REPLACED RRFORM WITH SCREEN
      *  BLM 161024 FIX IO ON BPFILE FOR BP-SEQNO & 8 DIGIT LOAN
      *  BLM 170425 8 DIGIT DATES
      *
      * KEC 2017.0912 PR#01246 WHEN ADDING THIS PR# TO A30, I NOTICED THAT
      *               A COPYMEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *          - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *          - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *          - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      * BAH 181004 GLOBAL BWFILE
      * BAH 20191007 REMOVED ACCESS-CALL BWFILE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSBT.CPY".
       COPY "LIBLP/LPFSBP.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDBT.CPY".
       COPY "LIBLP/LPFDBP.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BPBKBR.CBL S35 02/21/24-13:19:54 >".

       COPY "LIBSP/SPWSBT.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC XX   VALUE "PR".
       01  BT-IFN               PIC XX   VALUE "BT".
       01  BP1-IFN              PIC X(3) VALUE "BP1".


      *=================================================================
      *   /USR/DATA/R#/ED/BTFILE
      *   /USR/DATA/R#/ED/SVBTFIL####    ####=MMDD
      *=================================================================
       01  BTFILE-PATH.
           03  BTFILE-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  BTFILE-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(23)   VALUE "/ED/BTFILE".

       01  SVBTFIL-PATH.
           03  SVBTFIL-PATH-BASE          PIC X(09).
           03  FILLER                     PIC X(01) VALUE "/".
           03  SVBTFIL-PATH-MACHINE       PIC X(02).
           03  FILLER                     PIC X(11) VALUE "/ED/SVBTFIL".
           03  SVBTFIL-PATH-DATE-MM       PIC 9(02) VALUE ZEROES.
           03  SVBTFIL-PATH-DATE-DD       PIC 9(02) VALUE ZEROES.

      *=================================================================
      *
      *    L I N E - W O R K E R S
      *
      *=================================================================
       01  LN-FEED         PIC 99     VALUE 0.
       01  LN-COUNT        PIC 999    VALUE 99.
       01  PAGE-NUMBER         PIC 9(4)   VALUE 0.

      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  FIRST-TIME           PIC X          VALUE "Y".

       01  MESS-LIST            PIC X(5)       VALUE "MESS.".

       01  MESS-BUF.
           03  FILLER           PIC X(39) VALUE SPACES.
           03  MESS-BR          PIC 9(6) VALUE 0.

       01  MESS-BT-BUF.
           03  FILLER           PIC X(27) VALUE
               "CAN NOT FIND TAPE BRANCH = ".
           03  MESS-BT-BR          PIC 9(6) VALUE 0.

      *-----------------------------------------------------------------
       01  DUP-FLAG             PIC X VALUE " ".
       01  TOTAL-DUPS           PIC 9(9) VALUE 0.
       01  TOTAL-AMT-DUP        PIC S9(9)V99 VALUE 0 COMP-3.
       01  TOTAL-ZERO           PIC 9(9) VALUE 0.
       01  TOTAL-AMT-ZERO       PIC S9(9)V99 VALUE 0 COMP-3.
       01  TOTAL-RECORDS        PIC 9(9) VALUE 0.
       01  TOTAL-AMOUNT         PIC S9(9)V99 VALUE 0 COMP-3.

       01  HOLD-BRANCH          PIC 9(4) VALUE 0.

       01  HOLD-BP-REC          PIC X(BP-SIZE).
       01  HOLD-BP-SEQNO        PIC 99.

       01  ERRCD            PIC X.

       01  ACCEPT-BUF        PIC X    VALUE " ".

      *=================================================================
      *
      *    P R I N T - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE             PIC X(8)    VALUE " ".
           03  FILLER             PIC XX      VALUE " ".
           03  H-HR               PIC 99      VALUE 0.
           03  FILLER             PIC X       VALUE ":".
           03  H-MIN              PIC 99      VALUE 0.
           03  FILLER             PIC X(16)   VALUE " ".
           03  H-NAME             PIC X(40)   VALUE " ".
           03  FILLER             PIC X(21)   VALUE " ".
           03  FILLER             PIC X(30)   VALUE " ".
           03  FILLER             PIC X(5)    VALUE "PAGE ".
           03  H-PAGE-NO          PIC ZZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER             PIC X(10)   VALUE "USER ID:".
           03  H-USERID           PIC X(10).
           03  FILLER             PIC X(11)   VALUE " ".
           03  FILLER             PIC X(41)   VALUE
               " - - - BATCH PROCESSING LISTING - - - - -".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER             PIC X(9)    VALUE " ".
           03  FILLER             PIC X(25)   VALUE "NAME".
           03  FILLER             PIC X(20)   VALUE
               "AMOUNT          SSNO".
           03  FILLER             PIC X(29)   VALUE
               "          ACCOUNT#       DATE".
           03  FILLER             PIC X(20)   VALUE
               "       BRNO     CODE".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-NAME                    PIC X(25).
           03  FILLER                    PIC X(5).
           03  D-AMOUNT                  PIC Z(7).99.
           03  FILLER                    PIC X(6).
           03  D-SSNO                    PIC 999/99/9999.
           03  FILLER                    PIC X(6).
           03  D-DUP                     PIC X.
           03  D-ACCTNO                  PIC Z(8).
           03  FILLER                    PIC X(5).
           03  D-DATE                    PIC 99/99/99.
           03  FILLER                    PIC X(4).
           03  D-BRANCH                  PIC Z(6).
           03  FILLER                    PIC X(4).
           03  D-CODE                    PIC X(5).
           03  FILLER                    PIC X(3).
           03  D-MESS                    PIC X(20).

      ******************************************************************
      *     C O P Y - L I B R A R Y - W O R K E R S
      ******************************************************************
       COPY "LIBLP/LPBP_CMD.CPY".
      *  "/$REL/SH/EC /$REL/SH/BPNOBR.SH ".
       01  BPNOBR-CMD.
           03  FILLER            PIC X    VALUE "/".
           03  BPNOBR-REL        PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  BPNOBR-EC         PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  BPNOBR-REL2       PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  BPNOBR-SHELL      PIC X(10) VALUE "BPNOBR.SH ".
           03  FILLER            PIC X(2)  VALUE SPACES.
           03  BPNOBR-DATA       PIC X(18).

       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  BPBKBR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BPBKBR_DEF.CPY".
       COPY "LIBSP/BPBKBR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BPBKBR_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               BT-FILE PR-FILE BP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               BT-FILE BP-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BPBKBR" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BPBKBR-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = " " )
              GO TO END-PROGRAM.

      *-----------------------------------------------------------------
       NEXT-BT.

           PERFORM READ-BT-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           MOVE BT-BRANCH TO MESS-BR.
           PERFORM SEND-LEGEND.

           PERFORM FIND-PCNI-BRANCH.
           IF ( IO-FG = 9 )
              MOVE "NO BRANCH RECORD" TO D-NAME
              PERFORM DETAIL-LINE-PROCESS
              GO TO NEXT-BT.

           IF (BT-ACCTNO < 00000000) OR (BT-ACCTNO NOT NUMERIC)
               MOVE "INVALID LOAN NUMBER"   TO D-MESS
               PERFORM DETAIL-LINE-PROCESS
               GO TO NEXT-BT.

           IF FIRST-TIME = "Y"
              MOVE BT-BRANCH TO HOLD-BRANCH
              PERFORM CREATE-BP-FILE.

           IF ( BT-BRANCH NOT = HOLD-BRANCH )
              PERFORM WRITE-BP-TRAILER
              PERFORM CLOSE-BP1-FILE
              PERFORM TOTAL-ROUTINE
              MOVE BT-BRANCH TO HOLD-BRANCH
              PERFORM FIND-PCNI-BRANCH
              PERFORM CREATE-BP-FILE.

      * IF CAN NOT MATCH PCNI BRANCH WITH INPUT FILE BRANCH
      * DO NOT CONTINUE
           IF ( BR-NO = 0 )
              MOVE BT-BRANCH TO MESS-BT-BR
              MOVE MESS-BT-BUF TO MESS
              PERFORM SEND-MESS
      * AND REMOVE ANY CREATED B????0000000 FILES
              CALL "C$TOLOWER" USING BPNOBR-EC, VALUE 3
              CALL "C$TOLOWER" USING BPNOBR-SHELL, VALUE 10
              MOVE EXT-FILPATH-FIL TO BPNOBR-DATA
              MOVE BPNOBR-CMD TO SYSTEM-BUF
              PERFORM SYSTEM-CALL
              GO TO END-PROGRAM.

           ADD 1       TO TOTAL-RECORDS.
           ADD BT-AMT TO TOTAL-AMOUNT.


           IF BT-ACCTNO NOT = 0
              PERFORM WRITE-BP-RECORDS
           ELSE
              PERFORM WRITE-DUP-RECORD
              ADD 1 TO TOTAL-ZERO
              ADD BT-AMT TO TOTAL-AMT-ZERO.

           IF DUP-FLAG = "Y"
              MOVE "DUPLICATE ACCOUNT NO" TO D-MESS
           ELSE
              MOVE "CREATED BP" TO D-MESS.
           PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.
           GO TO NEXT-BT.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF TOTAL-RECORDS NOT = 0
              PERFORM WRITE-BP-TRAILER.
           PERFORM CLOSE-BP1-FILE.

           PERFORM TOTAL-ROUTINE.

      *    -------------------------------------------------------------
      *    RENAME BTFILE TO BTFIL{SYSDATE MM}{SYSDATE DA}
      * DO NOT PUT IN END-PROGRAM BECAUSE ERRORS WILL REMOVE IT!!!
      *    -------------------------------------------------------------

           MOVE SPACES TO SYSTEM-BUF.
           STRING MV-CMD, " ",
                  BTFILE-PATH, " ",
                  SVBTFIL-PATH
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           GO TO END-PROGRAM.


      ******************************************************************
       FIND-PCNI-BRANCH SECTION.
      ******************************************************************

           PERFORM OPEN-BR-FILE.
           MOVE BT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG = 9 )
              MOVE 0 TO BR-NO
              GO TO EXIT-PCNI-BRANCH.

       EXIT-PCNI-BRANCH.
           PERFORM CLOSE-BR-FILE.

      ******************************************************************
       CREATE-BP-FILE SECTION.
      ******************************************************************

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO BP-PATH-OVERRIDE.

           MOVE BR-NO         TO BP-ALL-BRANCH.
           MOVE ZEROES        TO BP-ALL-DATE BP-ALL-BATCH-NO.
           MOVE " "           TO BP-ALL-REFCD.
           PERFORM OPEN-BP1-FILE-OUTPUT.
           PERFORM CLOSE-BP1-FILE.
           PERFORM OPEN-BP1-FILE.

      ******************************************************************
       WRITE-BP-RECORDS SECTION.
      ******************************************************************
           MOVE SPACES       TO BP-REC.
           MOVE "N"          TO DUP-FLAG.
           MOVE BT-ACCTNO    TO BP-LNNO.
           MOVE 01           TO BP-SEQNO.
           MOVE BT-SSNO      TO BP-SSNO.

           MOVE "PA"         TO BP-TRCD.
           MOVE 0            TO BP-SIGN.

      *    -------------------------------------------------------------
      *    ACH ENTRIES FROM FORT KNOX
      *       SIGN     0 = NORMAL ALLOTMENT    ( TRCD="PA" DEFAULT)
      *                1 = PRE ALLOTMENT DEBIT ( TRCD="PY" [PREEZ])
      *                2 = ONE TIME ONLY DEBIT ( TRCD="PY" [OTOEZ])
      *                3 = RECURRING DEBIT     ( TRCD="PY" [RECEZ])
      *                4 = ACH                 ( TRCD="PY" [S####])#=BATCH
      *
      *     MOVE BT-SIGN TO BP-SIGN
      *     IF BT-SIGN = 1 OR 2 OR 3 OR 4
      *        MOVE "PY" TO BP-TRCD.
      *    -------------------------------------------------------------

           MOVE BT-AMT       TO BP-TRAMT.

           PERFORM WRITE-THE-BP-RECORD.

      ******************************************************************
       WRITE-THE-BP-RECORD SECTION.
      ******************************************************************
           MOVE 1 TO HOLD-BP-SEQNO.
       TRY-TO-WRITE-BP.
           MOVE HOLD-BP-REC TO BP-REC.
           MOVE HOLD-BP-SEQNO TO BP-SEQNO.
           PERFORM WRITE-BP1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 IF HOLD-BP-SEQNO < 99
                    ADD 1 TO HOLD-BP-SEQNO
                    GO TO TRY-TO-WRITE-BP
                 ELSE
                    GO TO IO-ERROR.
           IF BP-SEQNO > 1
              MOVE "Y" TO DUP-FLAG
              ADD 1 TO TOTAL-DUPS
              ADD BT-AMT TO TOTAL-AMT-DUP.

      ******************************************************************
       WRITE-BP-TRAILER SECTION.
      ******************************************************************

           MOVE SPACES        TO BP-REC.
           MOVE 0             TO BP-LNNO.
           MOVE 1             TO BP-SEQNO.
           MOVE "M"           TO BP-TYPE.
           MOVE TOTAL-RECORDS TO BP-TOTAL-COUNT.
           MOVE TOTAL-AMOUNT  TO BP-TOTAL-AMOUNT.
           PERFORM WRITE-BP1-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

      ******************************************************************
       WRITE-DUP-RECORD SECTION.
      ******************************************************************

           MOVE 99999999 TO BP-LNNO.
           MOVE 1        TO BP-SEQNO.
           PERFORM READ-BP1-FILE.

           IF ( IO-FG = 0 )
              ADD BT-AMT TO BP-TRAMT
              PERFORM REWRITE-BP1-FILE
           ELSE
              MOVE SPACES   TO BP-REC
              MOVE 99999999 TO BP-LNNO
              MOVE 1        TO BP-SEQNO
              MOVE 0        TO BP-SSNO
              MOVE BT-AMT  TO BP-TRAMT
              PERFORM WRITE-BP1-FILE.

      ******************************************************************
       TOTAL-ROUTINE SECTION.
      ******************************************************************

           MOVE "**** T O T A L S ****" TO DETAIL-LINE.
           MOVE TOTAL-RECORDS           TO D-ACCTNO.
           MOVE TOTAL-AMOUNT            TO D-AMOUNT.
           MOVE 3                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "**** TOTAL DUPS ****" TO DETAIL-LINE.
           MOVE TOTAL-DUPS             TO D-ACCTNO.
           MOVE TOTAL-AMT-DUP          TO D-AMOUNT.
           MOVE 1                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO TOTAL-RECORDS TOTAL-AMOUNT
                     TOTAL-DUPS    TOTAL-AMT-DUP.
           MOVE 99     TO LN-COUNT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

      * CHECK THAT THE BTFILE IS THERE
           PERFORM SETUP-BTFILE-PATHS.
           MOVE BTFILE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT NOT = "00" )
              MOVE "BATCH CORE FILE DOES NOT EXIST !!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO BPNOBR-REL BPNOBR-REL2
                              LIST-REL   LIST-REL2.

      * CHECK THAT NO B????0000000 FILES EXIST
      * /$REL/SH/BPLIST.SH
      
           CALL "C$TOLOWER" USING LIST-EC, VALUE 3.
           CALL "C$TOLOWER" USING LIST-SHELL, VALUE 10.
      * USR/DATA/R1/SC0001
           MOVE EXT-FILPATH-FIL TO LIST-DATA.
           MOVE LIST-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           CALL "C$TOLOWER" USING LIST-PATH, VALUE 14.
           MOVE LIST-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT = "00" )
              MOVE "BRANCH BATCH FILES STILL EXIST !!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

       CHECK-RANGES.

           MOVE " " TO ERRCD.

      *-----------------------------------------------------------------
       SETRP.

      *    -------------------------------------------------------------
      *    FORCE FILE TO SPOOL TO DISK FOR VIEW. THIS IS ONLY REVORD
      *    OF EXCEPTIONS AND WE DONT WANT TO LOSE IF PRINTER FAILS.
      *    -------------------------------------------------------------
           MOVE "V" TO STAT.
           MOVE BPBKBR-QUEUE-POS TO Q-POS.
           MOVE BPBKBR-COPIES-POS TO C-POS.
           MOVE BPBKBR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT = "1U"
              MOVE "OUTPUT MUST BE SAVED TO DISK" TO MESS
              PERFORM SEND-MESS
              MOVE "00" TO STAT
              GO TO SETRP.
           IF STAT NOT = "00"
              GO TO ENDIT.

      *-----------------------------------------------------------------
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ( ACCEPT-BUF NOT = "Y" AND NOT = "N" )
              GO TO OK-YN.

           IF ( ACCEPT-BUF = "N" )
              GO TO ENDIT.

           MOVE "BATCH BRANCH FILE CREATE IN PROGRESS..." TO MESS-BUF.

           PERFORM OPEN-PR-FILE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

           PERFORM OPEN-BT-FILE.

           GO TO EXIT-INITIALIZE.

      *-----------------------------------------------------------------
       ENDIT.
           MOVE "X" TO ERRCD.

      *-----------------------------------------------------------------
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE BT-CODE TO D-CODE.
           MOVE BT-BRANCH      TO D-BRANCH.
           MOVE BT-ACCTNO    TO D-ACCTNO.

      * IO-FG FROM THE WRITE-BP1
      *    MJD 990614 REPLACED TEST BELOW. IF IO-FG IS SET DURING WRITE
      *      DUP-FLAG IS SET TO "Y" WHICH ALLOWS DUPLICATE TRANS ON THE
      *      BP-FILE BUT STILL FLAGS THEM (*) ON BATCH LISTING. (TFC)
      *--->IF IO-FG NOT = 0 OR BT-ACCTNO = 0

           IF DUP-FLAG = "Y" OR BT-ACCTNO = 0
              MOVE "*"       TO D-DUP.

           MOVE BT-SSNO      TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE BT-AMT       TO D-AMOUNT.
      * BT-DATE IS STILL 6 DIGIT, INPUT FILE
           MOVE BT-DATE      TO D-DATE.
           MOVE BT-NAME      TO D-NAME.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BPBKBR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BPBKBR-SYSDATE.
           DISPLAY BPBKBR-SCREEN UPON BPBKBR-WINDOW-HANDLE.
           MOVE BPBKBR-FORM-FIELDS TO WR-BUF.
           MOVE BPBKBR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BPBKBR-HELP--FILE TO HELP-LINK-FILE.
           MOVE BPBKBR-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BPBKBR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *    C O P Y - L I B R A R Y - S E C T I O N S
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------------------------------
       SETUP-BTFILE-PATHS.

           MOVE EXT-CONAME-SYSDATE     TO ALP-DATE.

           MOVE EXT-FILPATH-BASE       TO BTFILE-PATH-BASE
                                          SVBTFIL-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE    TO BTFILE-PATH-MACHINE
                                          SVBTFIL-PATH-MACHINE.

           MOVE ALP-MO                 TO SVBTFIL-PATH-DATE-MM.
           MOVE ALP-DA                 TO SVBTFIL-PATH-DATE-DD.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPBTR.CPY".
       COPY "LIBLP/LPBP1IN.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE "SP/BPMENU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BPBKBR-SCREEN.
           DESTROY BPBKBR-WINDOW-HANDLE.
           EXIT PROGRAM.
