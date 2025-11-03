       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPR0.
       DATE-WRITTEN.    112784.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   CLOSING BANK BALANCE PROCEDURES
      *
      *                  CALLS LP/LPEOD5 FOR RECAP AND GL UPDATE
      *                  CALLS SP/EORUN WHICH RUNS THE BRANCH END OF
      *                      DAY BATCH WHICH IS CURRENTLY "QED"
      *
      *
      * REV:
      *  062895 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             SET TD-USERID, POSTTIME, AND POSTDATE.
      *  063095 BLL DON'T UPDATE RC-CHECKS, CHKREC WILL DO THIS
      *  071895 BLL SUBTRACT NSF'S FROM RC-DEPOSIT
      *  072095 JTG CHANGED BANK GL REFERENCE TO DEPOSIT GL
      *  073195 BLL IMPROVE ERRCD ROUTINES, GET ERRCD BACK FROM LPEOD5
      *  082296 BLV ADDED LOGIC TO STOP REWRITING RC-STATUS AS "D1" IF
      *             CURRENT RC-STATUS > "C1".
      *  060997 BAH ADDED GP-DISPLAY-BANK-BAL FOR COLONIAL, TN PR#507
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BLV 020598 UPDATE RC-CHECKS FROM TY-CKWRITTEN
      *  BLV 020698 DON'T SUBTRACT NSF'S FROM RC-DEPOSIT IF WORLD
      *  BLV 033098 CHECK FOR ERROR WHEN RETURNING FROM EORUN SO
      *             BATCH ERROR IS DISPLAY INSTEAD OF EOD SUCCESSFUL
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  BLV 062998 IF CAN'T CLOSE DAY BECAUSE CAN'T GET OP EXCLUSIVE,
      *             GIVE VIDEO INQUIRY TO SEE WHO IS POSTING TO BRANCH
      *  BLV 080698 HANDLE MORE SPECIFIC ERRCD FROM LPEOD5; IF BAD GI
      *             MOVE CL BACK TO OP, SET RC-STATUS TO A1
      *  BLV 080798 DON'T GO TO IO-ERROR IF WRITE ERROR OF 22 ON
      *             TR DEPOSIT RECORD, JUST REWRITE
      *  BLV 980924 ADD IN CA-NET-CASH-NOTES TO CASH DRAWER ROUTINES
      *  BLV 980302 INCLUDE OCCURRENCE 8 OF TY-CKWRITTEN (NOTES PAYABLE
      *             CHECKS
      *  BLV 980302 REVISE DISPLAY-BUF & DISPLAY-LIST NAMES TO BE
      *             MEANINGFUL, NOT C1, B2, R2 D2 STUFF.
      *  MJD 000601 ADDED LOGIC TO SKIP DISPLAY OF CKWRITTEN(4) IF IT IS
      *             ZERO.
      *  BLV 020128 IN SETUP-SCREEN, CHECK CA-BALBY TO MAKE SURE ALL
      *             DRAWERS ARE CLOSED; IF NOT, ABORT CLOSE & ERROR OUT:
      *             WE DISCOVERED USER COULD GET IN & OPEN DRAWER WHILE
      *             ANOTHER USE IN LONPL0 AT 'COMPLETE CLOSING' QUESTION!
      *  BAH 110411 UNCOMP'D CKWRITTEN, MEMORY ACCESS VIOLATION IF REALLY
      *             LARGE NUMBERS, PERF EQ PL# 735
      *  BAH 130412 ADDED LPEOD5-ERRCD SINCE FLDVAL AND XFER OF ERRCD
      *             ARE GONE!
      *  BAH 130415 FIX MKDIR AND DIR ACCESS
      *   CS 140527 REPLACED MKDIR-CMD WITH C$MAKEDIR
      *   CS 150417 REMOVED MV OF OP TO CL TO UP, INSTEAD LEFT THE OP TILL 
      *             UPDATE OF RC, AND THEN IT DELETES IT, IN THE CASE YOU
      *             RETURN FROM LPEOD5 WITH AN ERROR CODE 'I', THE RC-STATUS
      *             IS RESET TO 'A1'. DON'T DELETE THE 'OP' IF ERROR CODE
      *             = 'I' GL INTERFACE
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BLM 160311 FIX FOR NEW TY-DATE FORMAT, PD0003
      * BAH 160304 CHANGED TILLNO TO NUMERIC
      * BLM 160922 IF BR-ALLOW-SCANNED-CKS IS "Y", DISPLAY SOME PROMPTS
      *            WITHOUT "CASH" AND ENTER TOTAL CHECK DEPOSIT AMT, 
      *            WORLD PR# 1188
      * BLM 161116 CHECK BR-ALLOW-SCANNED-CKS IN A COUPLE MORE SPOTS TO
      *            PREVENT 2020'S IN CASE FX1188 IS NOT RUN, PL# 954
      * BLM 161216 IF UNABLE TO CLOSE BECAUSE SOMEONE ELSE IS STILL 
      *            POSTING, DISPLAY USERID OF USER WHO HAS RESERVATION
      *            ON OPFILE
      * BAH 2017.0313 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.1031 GLOBAL GBFILE
      * BAH 2019.0624 ADDED RC-UPDATED-USERID
      * BAH 2019.1010 LPEOD5 RECAP WAS PRINTING BRANCH NAME INSTEAD OF GLOBAL
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSOP_LCK.CPY".
       COPY "LIBLP/LPFSED.CPY".
       COPY "LIBLP/LPFSER.CPY".
           SELECT POSTING-USER-FILE ASSIGN TO LONPR0-TEMPFILE
                     ORGANIZATION LINE SEQUENTIAL
                     ACCESS SEQUENTIAL
                     LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                     FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDOP.CPY".
       COPY "LIBLP/LPFDED.CPY".
       COPY "LIBLP/LPFDER.CPY".
       FD  POSTING-USER-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 8 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  POSTING-USER-REC.
           03 POSTING-USER-BYTE          PIC X  OCCURS 8 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LONPR0 S35 04/10/25-09:48:31 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LPWSER.CPY".
       01  OP-IFN          PIC X(2) VALUE "OP".

       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  NEW-FLAG        PIC 9   VALUE 0.
       01  IO-TYPE         PIC X(3).
       01  GOOD-GL         PIC X.

       01  REC-EXISTS      PIC X.

       01  HOLD-RC-SYSDATE PIC 9(8).

       01  HOLD-RC-OVER-SHORT PIC S9(9)V99.
       01  HOLD-RC-DEPOSIT    PIC S9(9)V99.
       01  HOLD-RC-CK-DEPOSIT PIC S9(9)V99.
       01  HOLD-RC-ERRCD      PIC X.

       01  C-MSG.
           03  FILLER      PIC X(9)      VALUE "DRAWER # ".
           03  C-TILLNO    PIC 99.
           03  FILLER      PIC X(17)     VALUE " WAS NOT CLOSED !".

       01  WK-GLNUMBER.
           03  WK-BR       PIC 9(4).
           03  WK-DASH     PIC X.
           03  WK-GL       PIC 9(7).

      ******************************************************************
      *  DESCRIPTION: COMMAND TO GET USERID THAT IS STOPPING CLOSE
      ******************************************************************
       01  LONPR0-POSTING-COMMAND.
           03  LONPR0-POSTING-CMD1  PIC X(6)   VALUE "PS -P ".
           03  LONPR0-POSTING-PID   PIC Z(10).
           03  LONPR0-POSTING-CMD2  PIC X(13)   VALUE " -O RUSER= > ".
           03  LONPR0-TEMPFILE      PIC X(35)   VALUE SPACES.
       01  LOCK-PID                 PIC 9(10).

       01  WK-BEGBAL         PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-RUNNING        PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-NET-CASH-NOTES PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-ENDBAL         PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-DEPOSIT        PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-CHECK-DEPOSIT  PIC S9(7)V99 COMP-3 VALUE +0.

       01  TODAYS-CKWRITTEN-TAB.
           03  CKWRITTEN   OCCURS 8 PIC S9(7)V99.

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF          VALUE SPACES.
           03  D-OPEN-PROMPT    PIC X(46).
           03  D-OPEN-BANK-BAL  PIC Z,ZZZ,ZZ9.99-.
           03  D-OPEN-CASH-BAL  PIC Z,ZZZ,ZZ9.99-.
           03  D-NET-TRANS      PIC Z,ZZZ,ZZ9.99-.
           03  D-NET-CASH-NOTES PIC Z,ZZZ,ZZ9.99-.
           03  D-GL1            PIC X(12).
           03  D-EST-CASH       PIC Z,ZZZ,ZZ9.99-.
           03  D-OVER-SHORT     PIC Z,ZZZ,ZZ9.99-.
           03  D-GL2            PIC X(12).
           03  D-ACTUAL-CASH    PIC Z,ZZZ,ZZ9.99-.
           03  D-GL3            PIC X(12).

       01  DISPLAY-BUF2    VALUE SPACES.
           03  D-CLOSE-CASH-BAL PIC Z,ZZZ,ZZ9.99-.
           03  D-CKWRITTEN-ALP  PIC X(13) OCCURS 9.
           03  D-CKWRITTEN     REDEFINES D-CKWRITTEN-ALP
                                PIC Z,ZZZ,ZZ9.99- OCCURS 9.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(47)  VALUE
           "OPENPROMPT OPENBAL OPENCASH NETTRANS NOTESCASH ".
           03  FILLER      PIC X(38)  VALUE
           "010 ESTCASH OVERSHORT 020 ACTCASH 030.".
       01  DISPLAY-LIST2.
           03  FILLER      PIC X(42)  VALUE
           "CLOSECASH CK1 CK2 CK3 CK4 CK5 CK6 CK7 CK8 ".
           03  FILLER      PIC X(21)  VALUE
           "CLOSEBAL.".

       01  PROMPT1-BUF.
           03  D-OPEN-DR-PROMPT     PIC X(27).
           03  D-LOANS-TRANS-PROMPT PIC X(27).
           03  D-NOTES-TRANS-PROMPT PIC X(27).
           03  D-EST-DR-PROMPT      PIC X(27).
           03  D-OVER-SHORT-PROMPT  PIC X(27).
           03  D-ACT-DR-PROMPT      PIC X(27).
           03  D-DEPOSIT1-PROMPT    PIC X(30).
           03  D-DEPOSIT2-PROMPT    PIC X(30).
       01  PROMPT1-LIST.
           03  FILLER      PIC X(32)  VALUE
           "OPEN_DR_PMT LOANS_PMT NOTES_PMT ".
           03  FILLER      PIC X(35)  VALUE
           "EST_DR_PMT OV_SHORT_PMT ACT_DR_PMT ".
           03  FILLER      PIC X(26)  VALUE
           "DEPOSIT1_PMT DEPOSIT2_PMT.".

       01  PROMPT-BUF           PIC X(414).
       01  PROMPT-LINE    REDEFINES PROMPT-BUF
                                PIC X(46) OCCURS 9.
      *-----------------------------------------------------------------
      * PROMPT VALUES FOR PROMPT-LINE()
      *-----------------------------------------------------------------
       01  LOAN-PMPT        PIC X(46)  VALUE
           "LOAN CHECKS...................................".
       01  DEALER-PMPT      PIC X(46)  VALUE
           "DEALER CHECKS.................................".
       01  MISC-PMPT        PIC X(46)  VALUE
           "MISC. CHECKS..................................".
       01  OVER-PMPT        PIC X(46)  VALUE
           "OVERPAYMENT CHECKS............................".
       01  NSF-PMPT         PIC X(46)  VALUE
           "NSF CHECKS....................................".
       01  BANK-PMPT        PIC X(46)  VALUE
           "BANK WITHDRAWALS..............................".
       01  INTER-PMPT       PIC X(46)  VALUE
           "INTERMEDIATE DEPOSITS.........................".
       01  NOTES-PMPT       PIC X(46)  VALUE
           "NOTES PAYABLE CHECKS..........................".
       01  END-PMPT         PIC X(46)  VALUE
           "ENDING BANK BALANCE...........................".

       01  PROMPT-LIST1.
           03  FILLER      PIC X(32)  VALUE
           "CK1_P:P CK2_P:P CK3_P:P CK4_P:P ".
           03  FILLER      PIC X(32)  VALUE
           "CK5_P:P CK6_P:P CK7_P:P CK8_P:P ".
           03  FILLER      PIC X(32)  VALUE
           "CLOSEPROMPT.".
       01  PROMPT-LIST2.
           03  FILLER      PIC X(32)  VALUE
           "CK1_P:P CK2_P:P CK3_P:P CK4_P:P ".
           03  FILLER      PIC X(32)  VALUE
           "CK5_P:P CK6_P:P CK7_P:P CK8_P   ".
           03  FILLER      PIC X(32)  VALUE
           "CLOSEPROMPT.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 5.
       01  SPEC-TABLE.
      * 1  CASH IN BANK - GL #
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 2  CASH ON HAND - GL #
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 3  CASH OVER-SHORT GL #
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 4  DEPOSIT (OR CHECK DEPOSIT IF SCANNED CKS = Y)
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 5  CASH DEPOSIT (ONLY IF CHECK DEPOSIT IF SCANNED CKS = Y)
           03  FILLER      PIC X(8) VALUE "E NS072N".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 5 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ***********************************
      *    LPEOD5 FIELDS
      ***********************************
      * MUST MATCH LPEOD5
       01  LPEOD5-BUF.
           03  LPEOD5-BRNAME     PIC X(20).
           03  LPEOD5-PCODE      PIC XXX.
           03  LPEOD5-CONAME     PIC X(20).
           03  LPEOD5-BRANCH     PIC 9(4).
           03  LPEOD5-TRANS-DATE PIC 9(8).
           03  LPEOD5-SYSDATE    PIC X(8).
           03  LPEOD5-SEL        PIC 99.
           03  LPEOD5-USERID     PIC X(10).
           03  LPEOD5-ERRCD      PIC X.

      ***********************************
      *    EORUN FIELDS
      ***********************************
       01  EORUN-BUF          VALUE ZEROS.
           03  EORUN-PTYPE          PIC XXX.
           03  EORUN-PCODE          PIC XXX.
           03  EORUN-HOUR-BUF       PIC 99.
           03  EORUN-MIN-BUF        PIC 99.
           03  EORUN-DATE1          PIC 9(8).
           03  EORUN-DATE2          PIC 9(8).
           03  EORUN-FIRST-STEP     PIC 9(4).
           03  EORUN-LAST-STEP      PIC 9(4).
           03  EORUN-ERROR.
               05  EORUN-ERR-PROG       PIC X(10).
               05  EORUN-ERR-MSG        PIC X(10).
      *01  EORUN-ERR-LIST         PIC X(9) VALUE
      *    "EORUNERR.".

       01  ER-FEED              PIC 9        VALUE 1.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
      *COPY "LIBWI/ULINQW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPR_DEF.CPY".
       COPY "LIBWI/EODUPD_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONPR_WKS.CPY".
       COPY "LIBWI/EODUPD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPR_SCN.CPY".
       COPY "LIBWI/EODUPD_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              OP-FILE ER-FILE POSTING-USER-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE ER-FILE POSTING-USER-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPR" TO VDU-FORM-NAME.

           MOVE "C" TO VDU-WIMENU-SELECTION.

           PERFORM FORM-RESET.

      * /USR/DATA/R1/DP/OPBBBBDMMDDYY
           MOVE BRANCH      TO OPEN-PATH-BRNO.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.

           PERFORM LOAD-OPEN-FILE.

      * READ GLOBAL FOR AUTO DEPOSIT FLAG
           PERFORM OPEN-GB-FILE.
           MOVE BRANCH TO GB-BRNO.
           MOVE 1      TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

      * IF FILE CAN NOT BE OPENED EXCLUSIVELY, USERS ARE STILL IN.
      * NEED TO SET ERRCD IN CASE DECLARE EXITS THE PROGRAM FROM
      * EXCLUSIVE RESERVATION
           MOVE "X" TO ERRCD.
           PERFORM OPEN-IT.
           MOVE "OP" TO E-FILE.
           OPEN OUTPUT OP-FILE.
           IF FILE-STAT = "93"
              PERFORM GET-POSTING-USER
              GO TO EXIT-PROG.

      *       MOVE BRANCH TO ULINQ-BEG-BRANCH ULINQ-END-BRANCH
      *       MOVE "N" TO ULINQ-ENTER-RANGES
      *       MOVE ULINQ-BUF TO SCAN-BUF
      *       PERFORM CALL-WINDOW-PROGRAM
      *       PERFORM CLOSE-FILES
      *       GO TO EXIT-PROG.

      * ONLY RETURNS HERE IF NO ONE ELSE SIGNED INTO DAILY PROCESSING
      * OTHERWISE, DECLARATIVES EXIT THE PROGRAM
           MOVE " " TO ERRCD.

           PERFORM SETUP-SCREEN.

      * CREATE DEPOSIT RECORD
           PERFORM OPEN-TRD1-FILE.

           PERFORM CLEAR-TRD-FILE.

           MOVE BRANCH     TO TD-BRNO.
           MOVE TRANS-DATE TO TD-DATE.

           COMPUTE TD-OVERSHORT =
              (WK-BEGBAL + WK-RUNNING + WK-NET-CASH-NOTES) - WK-ENDBAL.
           MOVE BR-GLONHAND       TO TD-CASH.
           MOVE BR-GLBANK-DEPOSIT TO TD-BANK.
           MOVE BR-GLOVERSHORT    TO TD-GLOVERSHORT.

      * DEPOSIT
           IF BR-AUTO-OPEN-BAL = "Y"
              MOVE WK-DEPOSIT TO TD-DEPOSIT HOLD-RC-DEPOSIT
           ELSE
              COMPUTE TD-DEPOSIT  
                      HOLD-RC-DEPOSIT = WK-ENDBAL - WK-BEGBAL.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE WK-CHECK-DEPOSIT TO TD-CHECK-DEPOSIT
                                       HOLD-RC-CK-DEPOSIT.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE "S  S072040." TO VDU
              COMPUTE VDUNUM2 = TD-CHECK-DEPOSIT * -1
              PERFORM SCREENIO
              MOVE "S  S072041." TO VDU
              MOVE TD-CHECK-DEPOSIT TO VDUNUM2
              PERFORM SCREENIO
              MOVE "S  S072050." TO VDU
              COMPUTE VDUNUM2 = TD-DEPOSIT * -1
              PERFORM SCREENIO
              MOVE "S  S072051." TO VDU
              MOVE TD-DEPOSIT TO VDUNUM2
              PERFORM SCREENIO
           ELSE
              MOVE "S  S072040." TO VDU
              COMPUTE VDUNUM2 = TD-DEPOSIT * -1
              PERFORM SCREENIO
              MOVE "S  S072041." TO VDU
              MOVE TD-DEPOSIT TO VDUNUM2
              PERFORM SCREENIO.

           PERFORM DISPLAY-SCREEN2.
           PERFORM ENTRY-MODULE.

      * ACTUAL WRITE OF TRD RECORD
           PERFORM WRITE-TR-DEPOSIT.
           PERFORM CLOSE-TRD1-FILE.

      *SETS STATUS TO 'B1'
           PERFORM END-OF-DAY-UPD.

           MOVE " " TO ROUTE-CD.
           MOVE "X" TO ERRCD.
           GO TO END-ROUTINE.

      ************************************
       SETUP-SCREEN SECTION.
      ************************************

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
             GO TO IO-ERROR.

           PERFORM OPEN-CA1-FILE.

           MOVE BRANCH     TO CA-BRNO
                              QCA1-WBEG-BRNO
                              QCA1-WEND-BRNO.
           MOVE TRANS-DATE TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE
                                        QCA1-WEND-DATE.
           MOVE 0          TO CA-TILLNO
                              QCA1-WBEG-TILLNO.
           MOVE ALL "9"    TO QCA1-WEND-TILLNO.


           PERFORM START-CA1-FILE.
       NEXT-CASH-TILL.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9 OR (CA-BRNO NOT = BRANCH)
              GO TO DISPLAY-SCREEN.

           IF CA-DATE NOT = TRANS-DATE
              GO TO DISPLAY-SCREEN.

           IF CA-BALBY = " "
              MOVE CA-TILLNO TO C-TILLNO
              MOVE C-MSG TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              PERFORM CLOSE-FILES
              CLOSE OP-FILE
              GO TO EXIT-PROG.

           ADD CA-BEGBAL TO WK-BEGBAL.
           ADD CA-RUNNING TO WK-RUNNING.
           ADD CA-NET-CASH-NOTES TO WK-NET-CASH-NOTES.
           ADD CA-ENDBAL TO WK-ENDBAL.
           ADD CA-DEPOSIT TO WK-DEPOSIT.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              ADD CA-CHECK-DEPOSIT TO WK-CHECK-DEPOSIT.
           GO TO NEXT-CASH-TILL.

       DISPLAY-SCREEN.
           PERFORM CLOSE-CA1-FILE.

           IF GP-DISPLAY-BANK-BAL = "Y"
              MOVE "OPENING BANK BALANCE.........................."
                   TO D-OPEN-PROMPT
              MOVE BR-BANKBAL TO D-OPEN-BANK-BAL.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE "OPENING DRAWERS............" TO D-OPEN-DR-PROMPT
              MOVE "NET TRANSACTIONS-LOANS....." TO D-LOANS-TRANS-PROMPT
              MOVE "NET TRANSACTIONS-NOTES....." TO D-NOTES-TRANS-PROMPT
              MOVE "TOTAL EST. DRAWERS........." TO D-EST-DR-PROMPT
              MOVE "OVER/SHORT................." TO D-OVER-SHORT-PROMPT
              MOVE "TOTAL ACTUAL DRAWERS......." TO D-ACT-DR-PROMPT
              MOVE "4. CHECK DEPOSIT..........." TO D-DEPOSIT1-PROMPT
              MOVE "5. CASH DEPOSIT............" TO D-DEPOSIT2-PROMPT
              MOVE 5                             TO MAX
              MOVE "Y"                           TO CHFG(5)
           ELSE
              MOVE "OPENING CASH DRAWERS......." TO D-OPEN-DR-PROMPT
              MOVE "NET CASH TRANSACTIONS-LOANS" TO D-LOANS-TRANS-PROMPT
              MOVE "NET CASH TRANSACTIONS-NOTES" TO D-NOTES-TRANS-PROMPT
              MOVE "TOTAL EST. CASH DRAWERS...." TO D-EST-DR-PROMPT
              MOVE "CASH OVER/SHORT............" TO D-OVER-SHORT-PROMPT
              MOVE "TOTAL ACTUAL CASH DRAWERS.." TO D-ACT-DR-PROMPT
              MOVE "4. DEPOSIT................." TO D-DEPOSIT1-PROMPT
              MOVE "                           " TO D-DEPOSIT2-PROMPT
              MOVE 4                             TO MAX
              MOVE "N"                           TO CHFG(5).

           MOVE PROMPT1-LIST TO WR-LIST.
           MOVE PROMPT1-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      * OPENING CASH TILLS
           MOVE WK-BEGBAL TO D-OPEN-CASH-BAL.
      * NET TRANSACTIONS
           MOVE WK-RUNNING TO D-NET-TRANS.
           MOVE WK-NET-CASH-NOTES TO D-NET-CASH-NOTES.
      * TOTAL ESTIMATED CASH TILLS
           COMPUTE D-EST-CASH = WK-BEGBAL + WK-RUNNING +
                                WK-NET-CASH-NOTES.
      * OVER/SHORT
           COMPUTE D-OVER-SHORT HOLD-RC-OVER-SHORT =
             WK-ENDBAL - (WK-BEGBAL + WK-RUNNING + WK-NET-CASH-NOTES).
      * TOTAL ACTUAL CASH TILLS
           MOVE WK-ENDBAL TO D-ACTUAL-CASH.

      * DISPLAY ALL G/L ACCOUNTS:
           MOVE BR-GLBANK-DEPOSIT TO GL-BR-ACCOUNT.
           PERFORM EDIT-GLNO.
           IF GOOD-GL = "N"
              MOVE " " TO CHFG(1).
           MOVE WK-GLNUMBER TO D-GL1.

           MOVE BR-GLONHAND TO GL-BR-ACCOUNT.
           PERFORM EDIT-GLNO.
           IF GOOD-GL = "N"
              MOVE " " TO CHFG(2).
           MOVE WK-GLNUMBER TO D-GL2.

           MOVE BR-GLOVERSHORT TO GL-BR-ACCOUNT.
           PERFORM EDIT-GLNO.
           IF GOOD-GL = "N"
              MOVE " " TO CHFG(3).
           MOVE WK-GLNUMBER TO D-GL3.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      * READ TODAYS TOTALS
           MOVE 0 TO CKWRITTEN(1) CKWRITTEN(2) CKWRITTEN(3)
                     CKWRITTEN(4) CKWRITTEN(5) CKWRITTEN(6)
                     CKWRITTEN(7) CKWRITTEN(8).

           PERFORM GET-TY-CKWRITTEN.

       END-SETUP-SCREEN.
           EXIT.

      ***********************************************
       GET-POSTING-USER SECTION.
      ***********************************************
           CALL "C$LOCKPID" GIVING LOCK-PID.
           MOVE LOCK-PID TO LONPR0-POSTING-PID.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LONPR0-TEMPFILE.
           CALL "C$TOLOWER" USING LONPR0-POSTING-CMD1 VALUE 6.
           CALL "C$TOLOWER" USING LONPR0-POSTING-CMD2 VALUE 13.
           MOVE LONPR0-POSTING-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           PERFORM OPEN-POSTING-USER-FILE.
           PERFORM READ-POSTING-USER-FILE.
           PERFORM CLOSE-POSTING-USER-FILE.
           MOVE "                 **** WARNING ****" TO ERRMSG-HEAD.
           MOVE 
           "&&&&&&&& IS STILL POSTING IN THIS BRANCH FOR THIS DATE!"
              TO ERRMSG-MSG1
           IF IO-GOOD
              INSPECT ERRMSG-MSG1 REPLACING FIRST "&&&&&&&&"
                 BY POSTING-USER-REC
           ELSE
              INSPECT ERRMSG-MSG1 REPLACING FIRST "&&&&&&&&"
                 BY "  A USER".
           MOVE SPACES TO ERRMSG-MSG2.
           MOVE "               CLOSING NOT ALLOWED." TO ERRMSG-MSG3.
           PERFORM CALL-ERRMSG.

      *********************************************
       GET-TY-CKWRITTEN SECTION.
      *********************************************
           PERFORM OPEN-TY1-FILE.

           MOVE BRANCH              TO TY-BRNO
                                       QTY1-WBEG-BRNO
                                       QTY1-WEND-BRNO.
           MOVE TRANS-DATE          TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.

           MOVE 0                   TO TY-CLASS
                                       QTY1-WBEG-CLASS.
           MOVE ALL "9"             TO QTY1-WEND-CLASS.

           PERFORM START-TY1-FILE.
       GET-TY-CKWRITTEN-NEXT.
           PERFORM READ-TY1-FILE-NEXT.
           IF IO-FG = 9 OR (TY-BRNO NOT = BRANCH)
              GO TO GET-TY-CKWRITTEN-EXIT.

           IF TRANS-DATE NOT = TY-DATE
              GO TO GET-TY-CKWRITTEN-EXIT.

           ADD TY-CKWRITTEN(1) TO CKWRITTEN(1).
           ADD TY-CKWRITTEN(2) TO CKWRITTEN(2).
           ADD TY-CKWRITTEN(3) TO CKWRITTEN(3).
           ADD TY-CKWRITTEN(4) TO CKWRITTEN(4).
           ADD TY-CKWRITTEN(5) TO CKWRITTEN(5).
           ADD TY-CKWRITTEN(6) TO CKWRITTEN(6).
           ADD TY-CKWRITTEN(7) TO CKWRITTEN(7).
           ADD TY-CKWRITTEN(8) TO CKWRITTEN(8).
           GO TO GET-TY-CKWRITTEN-NEXT.

       GET-TY-CKWRITTEN-EXIT.
           PERFORM CLOSE-TY1-FILE.

      *************************************************
      *       ENTRY-MODULE
      *       NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                  1 - INITIAL NEW RECORD ENTRY
      *                  2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
       BEGIN-ENTRY.
           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO NEW-FLAG.
           IF GB-AUTO-DEPOSIT = "Y" OR BR-AUTO-OPEN-BAL = "Y"
              MOVE 4 TO ELE
           ELSE
              MOVE 3 TO ELE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG = 2
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "S  A000MSEL." TO VDU.
           MOVE "ACCEPT THE DEPOSIT ? 'YES' OR 'NO':" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ACTION = "   YES"
              GO TO END-ENTRY-MODULE.
           IF ACTION = "    NO"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      *************************************************************
      *  PASSWORD CHECK IF ANY OF THE THREE G.L. ACCOUNT NUMBERS
      *      IS ACCESSED
      *************************************************************
           IF ELE >= 1 AND <= 3
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT= "OK"
                 GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF NEW-FLAG NOT = 1
                 GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              GO TO CREATE-ROUTINE.
           IF ELE = 4
              IF GB-AUTO-DEPOSIT = "Y" OR BR-AUTO-OPEN-BAL = "Y"
                 GO TO MODIFY-ROUTINE.
           IF ELE = 5
              IF GB-AUTO-DEPOSIT = "Y" OR BR-AUTO-OPEN-BAL = "Y"
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           GO TO F-01 F-02 F-03 F-04 F-05 DEPENDING ELE.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
           MOVE VDUBUF TO WK-GLNUMBER.
           PERFORM CHECK-GL.
           IF GOOD-GL = "N"
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TD-BANK.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO WK-GLNUMBER.
           PERFORM CHECK-GL.
           IF GOOD-GL = "N"
             GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TD-CASH.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO WK-GLNUMBER.
           PERFORM CHECK-GL.
           IF GOOD-GL = "N"
             GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TD-GLOVERSHORT.
           GO TO CREATE-ROUTINE.
       F-04.
      * CHECK IF RETURN WAS PRESSED, TO KEEP ORIGINAL
      *   BUT THE SCREEN HAS A - BECAUSE OF THE * -1
           IF BR-ALLOW-SCANNED-CKS = "Y"
              IF VDUNUM2 = (TD-CHECK-DEPOSIT * -1)
                 GO TO CREATE-ROUTINE
              END-IF
           ELSE
              IF VDUNUM2 = (TD-DEPOSIT * -1)
                 GO TO CREATE-ROUTINE.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE VDUNUM2 TO TD-CHECK-DEPOSIT
                              HOLD-RC-CK-DEPOSIT
              MOVE "S  S072040." TO VDU
              COMPUTE VDUNUM2 = TD-CHECK-DEPOSIT * -1
              PERFORM SCREENIO
              MOVE "S  S072041." TO VDU
              MOVE TD-CHECK-DEPOSIT TO VDUNUM2
              PERFORM SCREENIO
           ELSE
              MOVE VDUNUM2 TO TD-DEPOSIT
                              HOLD-RC-DEPOSIT
              MOVE "S  S072040." TO VDU
              COMPUTE VDUNUM2 = TD-DEPOSIT * -1
              PERFORM SCREENIO
              MOVE "S  S072041." TO VDU
              MOVE TD-DEPOSIT TO VDUNUM2
              PERFORM SCREENIO.
           PERFORM DISPLAY-SCREEN2.
           GO TO CREATE-ROUTINE.
       F-05.
      * CHECK IF RETURN WAS PRESSED, TO KEEP ORIGINAL
      *   BUT THE SCREEN HAS A - BECAUSE OF THE * -1
           IF VDUNUM2 = (TD-DEPOSIT * -1)
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO TD-DEPOSIT
                           HOLD-RC-DEPOSIT.
           MOVE "S  S072050." TO VDU.
           COMPUTE VDUNUM2 = TD-DEPOSIT * -1.
           PERFORM SCREENIO.
           MOVE "S  S072051." TO VDU.
           MOVE TD-DEPOSIT TO VDUNUM2.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SCREEN2.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ***********************************
       DISPLAY-SCREEN2 SECTION.
      ***********************************
           MOVE LOAN-PMPT    TO PROMPT-LINE(1)
           MOVE DEALER-PMPT  TO PROMPT-LINE(2)
           MOVE MISC-PMPT    TO PROMPT-LINE(3)
 
           IF BR-ALLOW-SCANNED-CKS = "Y"
              COMPUTE D-CLOSE-CASH-BAL = WK-ENDBAL - TD-DEPOSIT
                                       - TD-CHECK-DEPOSIT
           ELSE
              COMPUTE D-CLOSE-CASH-BAL = WK-ENDBAL - TD-DEPOSIT.
           COMPUTE D-CKWRITTEN(1) = CKWRITTEN(1) * -1.
           COMPUTE D-CKWRITTEN(2) = CKWRITTEN(2) * -1.
           COMPUTE D-CKWRITTEN(3) = CKWRITTEN(3) * -1.

           IF CKWRITTEN(4) = 0
              MOVE NSF-PMPT    TO PROMPT-LINE(4)
              MOVE BANK-PMPT   TO PROMPT-LINE(5)
              MOVE INTER-PMPT  TO PROMPT-LINE(6)
              MOVE NOTES-PMPT  TO PROMPT-LINE(7)
              MOVE SPACES      TO PROMPT-LINE(8)
                                  PROMPT-LINE(9)
              MOVE CKWRITTEN(5) TO D-CKWRITTEN(4)
              COMPUTE D-CKWRITTEN(5) = CKWRITTEN(6) * -1
              COMPUTE D-CKWRITTEN(6) = CKWRITTEN(7) * -1
              COMPUTE D-CKWRITTEN(7) = CKWRITTEN(8) * -1
              MOVE SPACES TO D-CKWRITTEN-ALP(8)
           ELSE
              MOVE OVER-PMPT   TO PROMPT-LINE(4)
              MOVE NSF-PMPT    TO PROMPT-LINE(5)
              MOVE BANK-PMPT   TO PROMPT-LINE(6)
              MOVE INTER-PMPT  TO PROMPT-LINE(7)
              MOVE NOTES-PMPT  TO PROMPT-LINE(8)
              MOVE SPACES      TO PROMPT-LINE(9)
              COMPUTE D-CKWRITTEN(4) = CKWRITTEN(4) * -1
              MOVE CKWRITTEN(5) TO D-CKWRITTEN(5)
              COMPUTE D-CKWRITTEN(6) = CKWRITTEN(6) * -1
              COMPUTE D-CKWRITTEN(7) = CKWRITTEN(7) * -1
              COMPUTE D-CKWRITTEN(8) = CKWRITTEN(8) * -1.

           IF GP-DISPLAY-BANK-BAL = "Y"
              IF CKWRITTEN(4) = 0
                 MOVE END-PMPT TO PROMPT-LINE(8)
                 COMPUTE D-CKWRITTEN(8) =
                    BR-BANKBAL + TD-DEPOSIT - CKWRITTEN(1) -
                    CKWRITTEN(2) - CKWRITTEN(3) - CKWRITTEN(4) +
                    CKWRITTEN(5) - CKWRITTEN(6) - CKWRITTEN(7) -
                    CKWRITTEN(8) + TD-CHECK-DEPOSIT
              ELSE
                 MOVE END-PMPT TO PROMPT-LINE(9)
                 COMPUTE D-CKWRITTEN(9) =
                    BR-BANKBAL + TD-DEPOSIT - CKWRITTEN(1) -
                    CKWRITTEN(2) - CKWRITTEN(3) - CKWRITTEN(4) +
                    CKWRITTEN(5) - CKWRITTEN(6) - CKWRITTEN(7) -
                    CKWRITTEN(8) + TD-CHECK-DEPOSIT.


           MOVE PROMPT-BUF TO WR-BUF.
           IF CKWRITTEN(4) = 0
              MOVE PROMPT-LIST2 TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE PROMPT-LIST1 TO WR-LIST
              PERFORM CALL-WRFORM.

      * CKWRITTEN(6) IS NEGATIVE FOR A DEPOSIT
           MOVE DISPLAY-LIST2 TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.

      **************************************
       VERIFY-PASSWORD SECTION.
      **************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPR0" TO PASSWDW-PGM.
           MOVE ELE TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ***********************************
       WRITE-TR-DEPOSIT SECTION.
      ***********************************

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TD-USERID.
           MOVE POST-TIME   TO TD-POSTTIME.
           MOVE POST-DATE   TO TD-POSTDATE.

           PERFORM WRITE-TRD1-FILE.
      *    IF IO-FG = 9
      *       MOVE "RWDEPOSIT" TO GETENV-BUF
      *       PERFORM GETENV-CALL
      *       IF STAT = "00"
      *          PERFORM REWRITE-TR1-FILE
      *       ELSE
      *          GO TO IO-ERROR.

           IF IO-FG = 9
              IF E-CODE = "22"
                 PERFORM REWRITE-TRD1-FILE
              ELSE
                 GO TO IO-ERROR.

      ***********************************
       CHECK-GL SECTION.
      ***********************************
           IF WK-BR NOT NUMERIC OR WK-GL NOT NUMERIC
              MOVE 0 TO WK-BR WK-GL.
           MOVE WK-BR TO GL-BRANCH.
           MOVE WK-GL TO GL-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           IF IO-FG = 9
             MOVE "N" TO GOOD-GL
           ELSE
             MOVE "Y" TO GOOD-GL.
           PERFORM CLOSE-GL1-FILE.

       END-CHECK-GL.
           EXIT.

      *********************************
       EDIT-GLNO SECTION.
      *********************************
           MOVE GL-ACCOUNT TO WK-GL.
           MOVE "-" TO WK-DASH.
           MOVE GL-BRANCH TO WK-BR.
           PERFORM CHECK-GL.

      *********************************
       END-OF-DAY-UPD SECTION.
      *********************************

           PERFORM OPEN-RC1-FILE.

           MOVE "N" TO REC-EXISTS.
       RC-CHECK-EXISTS.
           PERFORM SET-RC1-KEY.
           PERFORM READ-RC1-FILE.
           IF IO-FG = 0
              GO TO RC-UPDATE-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.

           PERFORM CLEAR-RC-FILE.
           PERFORM SET-RC1-KEY.
           PERFORM WRITE-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO RC-CHECK-EXISTS.

       RC-UPDATE-REC.
      *  IN PROCESS
           MOVE "B1" TO RC-STATUS.
           PERFORM GET-SYSDATE-TIME.
           MOVE HOLD-RC-SYSDATE TO RC-CLOSED-DATE.
           MOVE HH-MM-SS TO RC-CLOSED-TIME.
      * 7/18/95 SUBTRACT NSF'S(5), WITHDRAWALS(6), ADD INT DEP(7)
      * UNLESS WORLD, WORLD DOESN'T SUBTRACT NSF'S FROM DEPOSIT
      * (THIS IS DEPOSIT AMT TRANSMITTED IN CASH CONCENTRATION!)
           COMPUTE RC-DEPOSIT = HOLD-RC-DEPOSIT - CKWRITTEN(6)
                                                - CKWRITTEN(7).
      *    IF NOT GP-I-AM-WORLD
      *       ADD CKWRITTEN(5) TO RC-DEPOSIT.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE HOLD-RC-CK-DEPOSIT TO RC-CHECK-DEPOSIT.
           MOVE HOLD-RC-OVER-SHORT TO RC-OVER-SHORT.
           ADD CKWRITTEN(1) CKWRITTEN(2) CKWRITTEN(3)
               CKWRITTEN(4) CKWRITTEN(8) GIVING RC-CHECKS.
           MOVE LPLOAN-USERID TO RC-CL-USERID.

           PERFORM REWRITE-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE 12 TO WINDOW-LINES.
           MOVE 77 TO WINDOW-COLUMNS.
           MOVE 10 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "EODUPD" TO VDU-FORM-NAME.
           MOVE "WI/EODUPD" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY EODUPD-SCREEN.

           MOVE EODUPD-FORM-FIELDS TO WR-BUF.
           MOVE EODUPD-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *    MOVE PROG-BUF TO LONPR-BUF.

      *    MOVE GB-COMPANY-NAME TO LONPR-CONAME.
      *    MOVE EXT-CONAME-SYSDATE TO LONPR-SYSDATE.
      *    MOVE LPLOAN-USERID TO LONPR-USERID.
      *    MOVE BATCH TO LONPR-BATCH.
      *    MOVE TRANS-CODES TO LONPR-TRANS-CODES.
      *    MOVE NEW-SUB-MENU-FORM TO LONPR-NEW-SUB-MENU-FORM.

      *    CALL XFER USING STAT LONPR-LIST LONPR-BUF.
      *    IF STAT NOT = "00"
      *       MOVE STAT TO MESS
      *       PERFORM SEND-MESS
      *       MOVE "ERROR IN XFER" TO MESS
      *       PERFORM SEND-MESS.

      *    MOVE "RPTLNS." TO MESS.
      *    CALL XFER USING STAT MESS GB-RPT-LINES.

            MOVE "CREATING WORK FILES....................." TO MESS.
            PERFORM SEND-LEGEND.

      *  DATED EO DIR:
            MOVE " " TO EO-SPOOL-FILE.

            IF BR-EOD-PROCEDURE = " "
               MOVE "EOD" TO EO-SPOOL-PCODE
            ELSE
               MOVE BR-EOD-PROCEDURE TO EO-SPOOL-PCODE.

            MOVE TRANS-DATE TO DATE-YYYYMMDD.
            PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
            MOVE DATE-MMDDYY TO EO-SPOOL-DATE-NAME.
            PERFORM LOAD-EO-SPOOL.
            MOVE EO-SPOOL TO DIR-ACCESS-BUF.

            PERFORM DIR-ACCESS-CALL.
            IF STAT = "00"
               GO TO END-OF-DAY-UPD-CONTINUE.

            MOVE EO-SPOOL-PCODE     TO ER-ALL-SPOOL-PCODE.
            MOVE EO-SPOOL-DATE-NAME TO ER-ALL-SPOOL-DATE.
            PERFORM LOAD-ER-FILE.

            CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99.
            IF STAT NOT = "00"
               MOVE "CAN'T CREATE SPOOL DIRECTORY" TO MESS
               PERFORM SEND-MESS
               MOVE EO-SPOOL TO MESS
               PERFORM SEND-MESS
               GO TO END-OF-DAY-UPD-SKIPPED.

        END-OF-DAY-UPD-CONTINUE.

      * JOURNAL WORK FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           MOVE MKTEMP-BUF TO ED-PATH.
           OPEN OUTPUT ED-FILE.
           IF FILE-STAT NOT = "00"
              MOVE "CAN'T CREATE JOURNAL WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO END-OF-DAY-UPD-SKIPPED.
           CLOSE ED-FILE.

      *     CALL XFER USING STAT ED-PATH-LIST ED-PATH.
      *     IF STAT NOT = "00"
      *        PERFORM PROBLEMS-UPD
      *        GO TO END-OF-DAY-UPD-SKIPPED.

      *     MOVE "S  A000SELX." TO VDU.
      *     MOVE SPACES TO VDUBUF.
      *     PERFORM SCREENIO.

      * PERFORM LPEOD5 TO UPDATE GL AND PRINT RECAP:
            MOVE BR-NAME             TO LPEOD5-BRNAME.
            IF BR-EOD-PROCEDURE = " "
               MOVE "EOD"            TO LPEOD5-PCODE
            ELSE
               MOVE BR-EOD-PROCEDURE TO LPEOD5-PCODE.
      *     MOVE LPLOAN-CONAME       TO LPEOD5-CONAME.
            MOVE GB-COMPANY-NAME     TO LPEOD5-CONAME.
            MOVE BRANCH              TO LPEOD5-BRANCH.
            MOVE TRANS-DATE          TO LPEOD5-TRANS-DATE.
            MOVE LPLOAN-SYSDATE      TO LPEOD5-SYSDATE.
            MOVE 2                   TO LPEOD5-SEL.
            MOVE LPLOAN-USERID       TO LPEOD5-USERID.
            MOVE " "                 TO LPEOD5-ERRCD.

      D     STOP MESS.

            MOVE "G/L RECAP AND UPDATE IN PROGRESS........" TO MESS.
            PERFORM SEND-LEGEND.

            MOVE "LP/LPEOD5" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH LPEOD5-BUF ED-PATH
                                           EXIT-PATHNAME.
            CANCEL FORM-PROGX.

            MOVE LPEOD5-ERRCD TO ERRCD.
            MOVE ERRCD TO HOLD-RC-ERRCD.
      *  IF NO ERROR OR MISSING GL MASTER, MOVE CLOSE TO UPDATE
            IF (ERRCD = " ") OR (ERRCD = "G")
               MOVE " " TO ERRCD.

      * SET RC-RECAP-FG TO "Y" OR "ERRCD"
            PERFORM UPDATE-RC1-RECAP.

      *IF EOD OF DAY ERROR ON THE GIFILE, DONT DELETE THE 'OP'
      *WILL MATCH THE RC-STATUS
            IF ERRCD NOT = "I" 
               CALL "CBL_DELETE_FILE" USING OPEN-PATH
                GIVING STAT-99.

      *  END OF DAY PROCEDURES:
            MOVE "EOD" TO EORUN-PTYPE.
            IF BR-EOD-PROCEDURE = " "
               MOVE "EOD" TO EORUN-PCODE
            ELSE
               MOVE BR-EOD-PROCEDURE TO EORUN-PCODE.
            MOVE 99 TO EORUN-HOUR-BUF
                       EORUN-MIN-BUF.
            MOVE TRANS-DATE TO EORUN-DATE1
                               EORUN-DATE2.
            MOVE 1 TO EORUN-FIRST-STEP.
            MOVE 9999 TO EORUN-LAST-STEP.
            MOVE SPACES TO EORUN-ERR-PROG EORUN-ERR-MSG.

            MOVE "RUNNING END OF DAY BATCH............." TO MESS.
            PERFORM SEND-LEGEND.

            MOVE "SP/EORUN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EORUN-BUF EXIT-PATHNAME.
            CANCEL FORM-PROGX.

            PERFORM UPDATE-RC1-EOD-BATCH.

       END-OF-DAY-UPD-FINISH.

           PERFORM SEND-EOD-FINISHED-MESS.

       END-OF-DAY-UPD-SELX.
           MOVE "R RA060SELX." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
                 OR VDUBUF NOT = "   YES"
              GO TO END-OF-DAY-UPD-SELX.

           GO TO END-OF-DAY-UPD-EXIT.

       END-OF-DAY-UPD-SKIPPED.
           MOVE "THE EOD UPDATE WAS SKIPPED!!!!" TO MESS.
           PERFORM SEND-MESS.

       END-OF-DAY-UPD-EXIT.
           PERFORM CLOSE-WINDOW.

      *******************************************
       SEND-EOD-FINISHED-MESS SECTION.
      *******************************************
           MOVE "S  A000MESS." TO VDU.

           IF HOLD-RC-ERRCD = "I"
              MOVE "S  A000MESS2." TO VDU
              MOVE
           "** INVALID GL INTERFACE ** END OF DAY NOT UPDATED!!"
                                                  TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000MESS." TO VDU
              MOVE
           "PLEASE CALL HOME OFFICE! THEN ENTER 'YES' TO CONTINUE:"
                                                     TO VDUBUF
              PERFORM SCREENIO
              PERFORM CREATE-ERROR-TAG
              GO TO EXIT-SEND-EOD-FINISHED-MESS.

           IF HOLD-RC-ERRCD = "G"
              MOVE "S  A000MESS2." TO VDU
              MOVE
           "*** END OF DAY HAS UPDATED WITH MISSING GL MASTER(S)!!"
                                                  TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000MESS." TO VDU
              MOVE
           "PLEASE CALL HOME OFFICE! THEN ENTER 'YES' TO CONTINUE:"
                                                     TO VDUBUF
              PERFORM SCREENIO
              PERFORM CREATE-ERROR-TAG
              GO TO EXIT-SEND-EOD-FINISHED-MESS.

           IF HOLD-RC-ERRCD NOT = " "
              MOVE "S  A000MESS2." TO VDU
              MOVE
           "**ERRORS** IN THE END OF DAY UPDATE HAVE OCCURRED!"
                                                  TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000MESS." TO VDU
              MOVE
           "PLEASE CALL HOME OFFICE! THEN ENTER 'YES' TO CONTINUE:"
                                                     TO VDUBUF
              PERFORM SCREENIO
              PERFORM CREATE-ERROR-TAG
              GO TO EXIT-SEND-EOD-FINISHED-MESS.

           IF EORUN-ERROR NOT = SPACES
              MOVE "S  A000MESS2." TO VDU
              MOVE
           "**ERRORS**, UPDATE WAS OK, PROBLEM RUNNING EOD REPORTS!"
                                                  TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000MESS." TO VDU
              MOVE
           "PLEASE CALL HOME OFFICE! THEN ENTER 'YES' TO CONTINUE:"
                                                 TO VDUBUF
              PERFORM SCREENIO
              PERFORM CREATE-ERROR-TAG
              GO TO EXIT-SEND-EOD-FINISHED-MESS.

      * CLEAR THE "CREATING WORK FILES" MESSAGE
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "S  A000MESS." TO VDU.
           MOVE
           "END OF DAY UPDATE WAS SUCCESSFUL, ENTER 'YES' TO CONTINUE!"
                                                              TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-SEND-EOD-FINISHED-MESS.
           EXIT.

      ***********************************************
       UPDATE-RC1-RECAP SECTION.
      ***********************************************
           PERFORM OPEN-RC1-FILE.
           PERFORM SET-RC1-KEY.
           PERFORM READ-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
      * IF BAD GI, SET STATUS BACK TO OPEN, MUST BUILD GI & RERUN EOD:
           IF HOLD-RC-ERRCD = "I"
              MOVE "A1" TO RC-STATUS
           ELSE
              MOVE "C1" TO RC-STATUS.
           PERFORM GET-SYSDATE-TIME.
           MOVE HOLD-RC-SYSDATE TO RC-UPDATED-DATE.
           MOVE HH-MM-SS        TO RC-UPDATED-TIME.
           MOVE LPLOAN-USERID   TO RC-UPDATED-USERID.
           IF HOLD-RC-ERRCD = " "
              MOVE "Y" TO RC-RECAP-FG
           ELSE
              MOVE HOLD-RC-ERRCD  TO RC-RECAP-FG.
           PERFORM REWRITE-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      **********************************************
       UPDATE-RC1-EOD-BATCH SECTION.
      **********************************************
           PERFORM OPEN-RC1-FILE.
           PERFORM SET-RC1-KEY.
           PERFORM READ-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * IF RC-STATUS SET BACK TO A1 (BAD GI), DON'T SET TO D1:
           IF RC-STATUS = "C1"
              MOVE "D1" TO RC-STATUS.

           PERFORM GET-SYSDATE-TIME.
           MOVE HOLD-RC-SYSDATE TO RC-EOD-BATCH-DATE.
           MOVE HH-MM-SS TO RC-EOD-BATCH-TIME.
           IF EORUN-ERROR = SPACES
              MOVE "Y" TO RC-EOD-BATCH-FG
           ELSE
              MOVE "N"  TO RC-EOD-BATCH-FG.
           PERFORM REWRITE-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      *COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GBRCCL.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPTRDCL.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE LPLOAN-CONAME TO LONPR-CONAME.
           MOVE LPLOAN-SYSDATE TO LONPR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPR-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPR-FORM-FIELDS TO WR-BUF.
           MOVE LONPR-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE LONPR-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPR-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPR_EVA.CPY".
              COPY "LIBWI/EODUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SET-RC1-KEY.
           MOVE BRANCH     TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.
       GET-SYSDATE-TIME.
           ACCEPT HOLD-RC-SYSDATE FROM CENTURY-DATE.
           MOVE "Y" TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
       PROBLEMS-UPD.
           MOVE "PROBLEMS UPDATING DAY, CALL SUPPORT!" TO MESS.
           PERFORM SEND-MESS.
      * IF ERRORS, CREATE ERROR TAG IN SPOOL DIRECTORY - EOQRPT WILL
      * CHECK FOR THIS FILE & STOP USER FROM RELEASING JOURNALS
       CREATE-ERROR-TAG.
           MOVE "/0000ERFILE" TO EO-SPOOL-FILE.
           MOVE EO-SPOOL      TO ER-ALL-PATH.
           PERFORM LOAD-ER-FILE.
           MOVE ER-PATH       TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-ER-FILE-OUTPUT
           ELSE
              PERFORM OPEN-ER-FILE.

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPTY1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPTRD1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBLP/LPPRINT.CPY".
       OPEN-POSTING-USER-FILE.
           PERFORM OPEN-IT.
           MOVE LONPR0-TEMPFILE TO E-FILE.
           OPEN INPUT POSTING-USER-FILE.
           IF IO-FG = 8
              GO TO OPEN-POSTING-USER-FILE.
           IF IO-FG = 7
              CLOSE POSTING-USER-FILE
              GO TO OPEN-POSTING-USER-FILE.
       READ-POSTING-USER-FILE.
           PERFORM READ-IT.
           MOVE LONPR0-TEMPFILE TO E-FILE.
           READ POSTING-USER-FILE.
           IF IO-FG = 8
              GO TO READ-POSTING-USER-FILE.
       CLOSE-POSTING-USER-FILE.
           PERFORM CLOSE-IT.
           CLOSE POSTING-USER-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPR-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           EXIT PROGRAM.
