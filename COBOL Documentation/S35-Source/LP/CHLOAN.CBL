       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHLOAN.
       DATE-WRITTEN.    09/25/2023.
      *******************************************************
      *                 (LP)
      *
      *  DESCRIPTION:   CHANGE MAKER OR COMAKER SSNO
      *                 CAN NOT REMOVE MAKER SSNO
      *                 BORROWER MUST EXIST
      *                 CREATE LOAN MEMO
      *                 CREATE CHANGES LOG
      *                 UPDATE BWFILE NUMBERS (SUBTRACT OLD, ADD NEW)
      *                 UPDATE BIFILE (DELETE OLD, CREATE NEW)
      *
      *  REV:
      *   BAH 2023.0926 NEW #1625
      *******************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CHLOAN.CBL S35 09/26/23-09:42:14 >".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".

       01  ONE               PIC S9.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  NEW-FLAG          PIC 9       VALUE 1.
       01  IO-TYPE           PIC X(3).
       01  ELE               PIC 9(2).
       01  SUB               PIC 9(6)    COMP-3 VALUE 0.
       01  ORIGINAL-LN-REC   PIC X(LN-SIZE).
       01  CHANGED-LN-REC    PIC X(LN-SIZE).
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(12).
               05  WORK-ERRLOG-ORG-N0 REDEFINES WORK-ERRLOG-ORG-ALP
                                        PIC Z(11)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES WORK-ERRLOG-ORG-ALP
                                        PIC Z(8).99-.
               05  WORK-ERRLOG-TO-X     PIC X(3).
               05  WORK-ERRLOG-CHG-ALP  PIC X(12).
               05  WORK-ERRLOG-CHG-N0 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z(11)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z(8).99-.
       01  DIS-BRANCH.
           03  FILLER         PIC X(26)  VALUE
           "ACCOUNT OWNED BY BRANCH # ".
           03  DIS-BR         PIC ZZZ9.
       01  FIRST-SCAN        PIC 9       VALUE 0.
       01  DEFAULT1-KEY       PIC X.
       01  SCAN-TYPES.
           03  FILLER        PIC X(49) VALUE
           "1-SSNO, 2-NAME, 3-SPS SSNO, 4-SPS NAME, F1-CANCEL".
       01  SCAN-MESSAGES.
           03  TYPE-1        PIC X(55) VALUE
           "SSNO, F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
           03  TYPE-2        PIC X(55) VALUE
           "NAME (LAST,FIRST), F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
           03  TYPE-3        PIC X(55) VALUE
           "SPOUSE SSNO, F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
           03  TYPE-4        PIC X(55) VALUE
           "SPOUSE NAME, F1-CANCEL, ^, F5-SCAN LOAN, F6-SCAN".
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE SPACES.
           88  SCAN-ON       VALUE "ON".
       01  NAME-WORKER       PIC X(40) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.
       01  SAVE-MAKER-NAME   PIC X(32).
       01  SAVE-COMAKER-NAME PIC X(32).
       01  SAVE-MAKER-SSNO   PIC 9(9).
       01  SAVE-COMAKER-SSNO PIC 9(9).
       01  TYP               PIC 9     VALUE 0.
       01  BORROWER-SCAN     PIC X     VALUE SPACES.
       01  IX1               PIC 99    COMP-3.
       01  IX2               PIC 99    COMP-3.

       01  TODAYS-DATE-YYYYMMDD PIC 9(8).

       01  HOLD-LM-SEQ          PIC 999.
       01  HOLD-MEMO.
           03  FILLER           PIC X(21) VALUE
           "CHLOAN: CHANGED SSNO ".
           03  MEMO-WHICH-SSNO  PIC 9.
           03  FILLER           PIC X(6) VALUE
           " FROM ".
           03  MEMO-SSNO1       PIC 9(9).
           03  FILLER           PIC X(4) VALUE " TO ".
           03  MEMO-SSNO2       PIC 9(9).

      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  MAX               PIC 99   VALUE 2.
       01  SPEC-TABLE.
      *01. NEW MAKER SSNO
           03  FILLER        PIC X(8) VALUE "ERNX090 ".
      *02. NEW COMAKER SSNO
           03  FILLER        PIC X(8) VALUE "ERNX090 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 2 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       01  NUM-BW            PIC 9(9).
       01  NUM-BW-WK         REDEFINES NUM-BW.
           03  NUM-BW1       PIC 999.
           03  NUM-BW2       PIC 99.
           03  NUM-BW3       PIC 9999.
       01  ALP-BW.
           03  ALP-BW1       PIC 999.
           03  ALP-HYPHEN1   PIC X.
           03  ALP-BW2       PIC 99.
           03  ALP-HYPHEN2   PIC X.
           03  ALP-BW3       PIC 9999.
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL    PIC X     OCCURS 70.

       01  TEST-SSNO       PIC 9(9).
       01  PROFILE-EDIT      PIC 9(12).
       01  PROFILE-ED REDEFINES PROFILE-EDIT
                             PIC X(12).

       01  DELINQ-VALUE      PIC X(12).

       01  SCREEN-LIST.
           03  FILLER            PIC X(44) VALUE
           "010 020 MESSAGE SOC A1 A2 A3 A4 SP0 SP1 SP2 ".
           03  FILLER            PIC X(44) VALUE
           "LNNO LNSEQ MAKER PDTH OTHBAL INTBAL PRINBAL ".
           03  FILLER            PIC X(30) VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5 ".
           03  FILLER            PIC X(28) VALUE
           "LN1 GL8 F21 GD8 F22 GA8 F23 ".
           03  FILLER            PIC X(28) VALUE
           "GL9 F31 GD9 F32 GA9 F33 SEL.".
       01  D-MESSAGE         PIC X(30).

       01  MESSAGE-LIST.
           03  MESS-LIST     PIC X(7) VALUE "MESSAGE".
           03  MESS-END      PIC X(4) VALUE "    ".


       01  DISPLAY-BORR-BUF  VALUE " ".
           03  D-SOC         PIC X(11).
           03  D-NAME        PIC X(34) OCCURS 4.
           03  D-SP0         PIC X(7).
           03  D-SP1         PIC X(11).
           03  D-SP2         PIC X(25).

       01  DISPLAY-BORR-LIST.
           03  FILLER        PIC X(28) VALUE
           "SOC A1 A2 A3 A4 SP0 SP1 SP2.".

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM    PIC X(16).
           03  D-MAKER       PIC X(8).
           03  D-PDTH        PIC 9(4).
           03  D-OTHBAL      PIC Z(5).99-.
           03  D-INTBAL      PIC Z(5).99-.
           03  D-CURBAL      PIC Z(5)9.99.
           03  D-SSNO1       PIC 999/99/9999.
           03  D-NAME1       PIC X(32).
           03  D-SSNO2       PIC 999/99/9999 BLANK ZERO.
           03  D-NAME2       PIC X(32).

       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(44) VALUE
           "LNNO LNSEQ MAKER PDTH OTHBAL INTBAL PRINBAL ".
           03  FILLER        PIC X(32) VALUE
           "OSSNO OLDNAME OCOSSNO OLDCONAME.".

       01  LOAN-INFO-LIST.
           03  FILLER          PIC X(30)    VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5 ".
           03  FILLER          PIC X(28) VALUE
           "LN1 GL8 F21 GD8 F22 GA8 F23 ".
           03  FILLER          PIC X(24) VALUE
           "GL9 F31 GD9 F32 GA9 F33.".

       01  LOAN-INFO-BUF.
           03  FILLER          PIC X(40) VALUE
           "ACCOUNT #         DATE  LOAN AMT TRM  DE".
           03  FILLER          PIC X(16) VALUE
           "ALER COL PL/JD  ".
           03  FILLER          PIC X(11) VALUE "CONTRACTUAL".
           03  FILLER          PIC X(13) VALUE
           "  BAL/PDOFF M".
           03  LOAN-INFO-TABLE.
               05  LOAN-INFO-TAB OCCURS 3.
                   07  LI-LOAN-NUM PIC X(16).
                   07  FILLER      PIC X.
                   07  LI-LNDATE   PIC 9(6).
                   07  LI-LOAN-AMT PIC ZZZZZZ.ZZ.
                   07  FILLER      PIC X.
                   07  LI-TRM      PIC ZZZ.
                   07  FILLER      PIC X.
                   07  LI-DEALER   PIC ZZZZ.ZZ.
                   07  FILLER      PIC X.
                   07  LI-COLL     PIC XX.
                   07  FILLER      PIC X.
                   07  LI-PLFG     PIC X.
                   07  LI-JDPLDT   PIC 9(6)  BLANK ZERO.
                   07  FILLER      PIC X.
                   07  LI-CONTRACT PIC X(12).
                   07  FILLER      PIC X.
                   07  LI-BAL.
                       09  LI-LOAN-BAL PIC ZZZZZZ.ZZ-.
                       09  LI-MKR      PIC X.
                   07  LI-POFF REDEFINES LI-BAL.
                       09  LI-POCD     PIC XX.
                       09  FILLER      PIC X.
                       09  LI-PAIDOFF  PIC 9(6).
                       09  FILLER      PIC XX.

       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHLOAN_DEF.CPY".
       COPY "LIBLP/CHLOAN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CHLOAN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "CHLOAN" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.


           MOVE " " TO ERRCD.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE CHLOAN-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE-YYYYMMDD.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           GO TO MAIN-MODULE.

      ********************************
       ENTER-KEYS SECTION.
      ********************************
       ENTER-KEYS-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              GO TO READ-LNFILE1.

           MOVE "ENTER ACCOUNT NUMBER, F6-SCAN, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = "CLR"
                 PERFORM RESET-SCREEN
                 GO TO ENTER-KEYS-NO
              ELSE
                 MOVE LN-ACCTNO TO VDUNUM0
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
       READ-LNFILE1.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

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

      * NEED TO SAVE ORIGINAL COMAKER NAME TO DISPLAY THE NAME
           MOVE SPACES TO SAVE-COMAKER-NAME.
           PERFORM OPEN-BW1-FILE.
           IF LN-SSNO(2) NOT = 0
              MOVE LN-SSNO(2) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID" TO SAVE-COMAKER-NAME
              ELSE
               MOVE BW-LNAME TO NAME-WORKER
               MOVE BW-FNAME TO NAME
               INSPECT NAME-WORKER REPLACING FIRST "   " BY ", ?"
               INSPECT NAME-WORKER REPLACING FIRST "?               "
                       BY NAME
               MOVE NAME-WORKER TO SAVE-COMAKER-NAME.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM DISPLAY-BORROWER.

           PERFORM DISPLAY-LOAN.

           MOVE LN-REC TO ORIGINAL-LN-REC.
           MOVE LN-SSNO(1) TO SAVE-MAKER-SSNO.
           MOVE LN-SSNO(2) TO SAVE-COMAKER-SSNO.
           IF LN-OWNBR NOT = BRANCH
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

       END-ENTER-KEYS.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE "Y" TO DEFAULT1-KEY.
       ENTER-SCAN.
           MOVE 1 TO SCAN-FLAG.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           IF DEFAULT1-KEY = "N"
              GO TO ENTER-TYPE.
           MOVE "N" TO DEFAULT1-KEY.

           MOVE "SN N010TYP." TO VDU.
           MOVE 2 TO TYP VDUNUM0.
           PERFORM SCREENIO.

           MOVE SPACES TO BW-NAME.
           PERFORM START-BW2-FILE.

           GO TO DISPLAY-TYPE.
       ENTER-TYPE.
           MOVE SCAN-TYPES TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N010TYP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.
           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
           ELSE
           IF TYP = 2
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
           ELSE
           IF TYP = 3
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE
           ELSE
           IF TYP = 4
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE.
       DISPLAY-TYPE.
           MOVE TYPE-2 TO MESS.
           IF TYP = 1
              MOVE TYPE-1 TO MESS
           ELSE
              IF TYP = 3
                 MOVE TYPE-3 TO MESS
              ELSE
                 IF TYP = 4
                    MOVE TYPE-4 TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-KEY.
           MOVE "E RA160SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              PERFORM RESET-SCREEN
              GO TO ENTER-SCAN.
           IF VDUSTATUS = "1:"
              IF FIRST-SCAN = 0
                 MOVE " ENTER BORROWER # FIRST" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1:"
              PERFORM SCAN-LOANS
              IF IO-TYPE = "CLR"
                 MOVE SPACES TO IO-TYPE
                 MOVE DISPLAY-LOAN-LIST TO WR-LIST
                 MOVE SPACES            TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO ENTER-KEY
              ELSE
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF SCAN-ON
                 MOVE LOAN-INFO-LIST TO WR-LIST
                 MOVE SPACES         TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO TEST-SSNO
              IF TEST-SSNO NOT NUMERIC
              GO TO ENTER-KEY.
           IF TYP = 1 OR 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO
           ELSE
              IF TYP = 2 OR 4
                 PERFORM ARRANGE-BW-KEY
                 MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.
           IF TYP = 1
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
           MOVE 0 TO SCAN-FLAG.
       READ-FILES-NEXT.
           MOVE 0 TO FIRST-SCAN.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-BORROWER.
           PERFORM DISPLAY-LOAN-INFO.
           MOVE SPACES TO SCAN.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           MOVE SPACES            TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE 1 TO SCAN-FLAG.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "S  A010TYP." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      **************************************
       ARRANGE-BW-KEY SECTION.
      **************************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
           ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      **************************************
       SCAN-LOANS SECTION.
      **************************************
      *    GO TO START-LN4 START-LN5 START-LN6 START-LN7
           GO TO START-LN4 START-LN5 
                 DEPENDING ON SCAN-FLAG.
       START-LN4.
           IF SCAN-ON
              GO TO READ-LN4.
           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
           MOVE "ON" TO SCAN.
       READ-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-GOOD
              IF (LN-SSNO(1) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN4
                 ELSE
                    PERFORM DISPLAY-LOAN
                    GO TO END-SCAN-LOANS.
           ADD 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN.
       START-LN5.
           IF SCAN-ON
              GO TO READ-LN5.
           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
           MOVE "ON" TO SCAN.
       READ-LN5.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-GOOD
              IF (LN-SSNO(2) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN5
                 ELSE
                    PERFORM DISPLAY-LOAN
                    GO TO END-SCAN-LOANS.

      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN6.
      *    IF SCAN-ON
      *       GO TO READ-LN6.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-GOOD
      *       IF (LN-SSNO(3) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN6
      *          ELSE
      *             PERFORM DISPLAY-LOAN
      *             GO TO END-SCAN-LOANS.
      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN7.
      *    IF SCAN-ON
      *       GO TO READ-LN7.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-GOOD
      *       IF (LN-SSNO(4) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN7
      *          ELSE
      *             PERFORM DISPLAY-LOAN
      *             GO TO END-SCAN-LOANS.

           MOVE "CLR" TO IO-TYPE.
           MOVE 0 TO SCAN-FLAG.
           MOVE SPACES TO SCAN LN-REC.
       END-SCAN-LOANS.
           EXIT.

      *****************************************
       DISPLAY-BORROWER SECTION.
      *****************************************

           MOVE SPACES TO DISPLAY-BORR-BUF
                          SAVE-MAKER-NAME.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-SOC.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", ?".
           INSPECT NAME-WORKER REPLACING FIRST "?               "
             BY NAME.
           MOVE NAME-WORKER TO D-NAME(1)
                               SAVE-MAKER-NAME.
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
           IF BW-SPSSNO = 0
            IF BW-SPLNAME = SPACES
             IF BW-SPFNAME = SPACES
              GO TO END-DISPLAY-BORR.
           MOVE "SPOUSE:" TO D-SP0.
           IF BW-SPSSNO NOT = 0
              MOVE BW-SPSSNO TO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE ALP-BW TO D-SP1.
           MOVE BW-SPLNAME TO NAME-WORKER.
           MOVE BW-SPFNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-SP2.
       END-DISPLAY-BORR.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           MOVE DISPLAY-BORR-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.


      *****************************************
       DISPLAY-LOAN SECTION.
      *****************************************

           IF BORROWER-SCAN = "Y"
              GO TO DISPLAY-LOAN-EXIT.

           MOVE SPACES TO DISPLAY-LOAN-BUF.
           IF ( LN-POCD-NONLOAN )
              GO TO DISPLAY-LOAN-EXIT.

           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO MAKER-CD
              PERFORM DISPLAY-MAKER.
           MOVE LN-CURBAL TO D-CURBAL.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE TO D-PDTH.
           MOVE LN-OTHBAL TO D-OTHBAL.
           MOVE LN-INTBAL TO D-INTBAL.

           MOVE LN-SSNO(1) TO D-SSNO1.
           INSPECT D-SSNO1 REPLACING ALL "/" BY "-".
           MOVE SAVE-MAKER-NAME TO D-NAME1.
           MOVE LN-SSNO(2) TO D-SSNO2.
           IF LN-SSNO(2) NOT = 0
              INSPECT D-SSNO2 REPLACING ALL "/" BY "-".
           MOVE SAVE-COMAKER-NAME TO D-NAME2.

           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           MOVE DISPLAY-LOAN-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE LN-MESSAGE TO D-MESSAGE.
           IF LN-MESSAGE = SPACES
              MOVE "." TO MESS-END
           ELSE
              MOVE ":SR." TO MESS-END.

           IF LN-MESSAGE NOT = SPACES
              PERFORM ALARM
              PERFORM BLINK-STARS 3 TIMES
              PERFORM ALARM.

           MOVE MESSAGE-LIST TO WR-LIST.
           MOVE D-MESSAGE    TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-LOAN-EXIT.
           EXIT.

      *****************************************
       DISPLAY-LOAN-INFO SECTION.
      *****************************************
           MOVE 1 TO SCAN-FLAG.
           MOVE " " TO SCAN.
           MOVE "Y" TO BORROWER-SCAN.
       DISPLAY-NEXT-8.
           MOVE 0 TO SUB.
           MOVE " " TO LOAN-INFO-TABLE.
       DISPLAY-LN-INFO-CONT.
           PERFORM SCAN-LOANS.
           IF SCAN-FLAG = 0
              GO TO DISPLAY-LOAN-INFO-XFER.
           ADD 1 TO SUB.
           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO LI-LOAN-NUM(SUB).
           IF LN-PLCD NOT = SPACES
              MOVE LN-PLTYPE TO LI-PLFG(SUB)
              MOVE LN-PLDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-JDPLDT(SUB).
           IF LN-JDDATE NOT = 0
              MOVE "J" TO LI-PLFG(SUB)
              MOVE LN-JDDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-JDPLDT(SUB).
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LI-LNDATE(SUB).
           MOVE LN-SUM-LNAMT TO LI-LOAN-AMT(SUB).
           MOVE LN-SUM-ORGTERM TO LI-TRM(SUB).
           MOVE LN-DLNO TO LI-DEALER(SUB).
           MOVE LN-COLLCD TO LI-COLL(SUB).

           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO LI-CONTRACT(SUB).

           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO LI-MKR(SUB).
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO LI-MKR(SUB).
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO LI-MKR(SUB).
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO LI-MKR(SUB).

           IF LN-POCD NOT = SPACES
              MOVE LN-POCD TO LI-POCD(SUB)
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-PAIDOFF(SUB)
              GO TO DISPLAY-LOAN-SUB-CHECK.
           IF LN-POFFDATE NOT = 0
              MOVE LN-POCD TO LI-POCD(SUB)
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-PAIDOFF(SUB)
              GO TO DISPLAY-LOAN-SUB-CHECK.

           MOVE LN-CURBAL TO LI-LOAN-BAL(SUB).
       DISPLAY-LOAN-SUB-CHECK.
           IF SUB < 3
              GO TO DISPLAY-LN-INFO-CONT.
       DISPLAY-LOAN-INFO-XFER.
           MOVE LOAN-INFO-LIST TO WR-LIST.
           MOVE LOAN-INFO-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF SCAN-FLAG NOT = 0
              MOVE "E RA160SORTKEY." TO VDU
              PERFORM SCREENIO
              GO TO DISPLAY-NEXT-8.

       DISPLAY-LOAN-INFO-EXIT.
           MOVE "N" TO BORROWER-SCAN.
           MOVE SPACES TO IO-TYPE.

      *************************************************
      *    ENTRY-MODULE
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "ENTER FIELD #, F1-CANCEL, <ENTER>-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              PERFORM VERIFY-NOT-SAME-SSNO
              IF IO-TYPE = "BAD"
                 GO TO MODIFY-ROUTINE
              ELSE
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
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 DEPENDING ON ELE.

       F-01.
           IF VDUNUM0 = 0
              MOVE "CAN NOT CLEAR MAKER SSNO" TO MESS
              PERFORM SEND-MESS
              MOVE "S  A110010." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "BORROWER DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE
           ELSE
              MOVE VDUNUM0 TO LN-SSNO(1)
              MOVE BW-LNAME TO NAME-WORKER
              MOVE BW-FNAME TO NAME
              INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?"
              INSPECT NAME-WORKER REPLACING FIRST "                "
                                                  BY NAME
              INSPECT NAME-WORKER REPLACING FIRST "?" BY " "
              MOVE "S  A320011." TO VDU
              MOVE NAME-WORKER TO VDUBUF
              PERFORM SCREENIO
              MOVE SPACES TO MESS
              PERFORM SEND-LEGEND.

           GO TO CREATE-ROUTINE.
       F-02.

           IF LN-SSNO(2) = 0
              MOVE "CAN NOT ADD COMAKER IN THIS SCREEN" TO MESS
              PERFORM SEND-MESS
              MOVE "S  A110020." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           IF VDUNUM0 = 0
              MOVE "YOU ARE REMOVING THE COMAKER" TO MESS
              PERFORM SEND-MESS
              MOVE VDUNUM0 TO LN-SSNO(2)
              MOVE SPACES  TO LN-MAKERCD(2)
           ELSE
              MOVE VDUNUM0 TO BW-SSNO
              PERFORM OPEN-BW1-FILE
              PERFORM READ-BW1-FILE
              PERFORM CLOSE-BW1-FILE
              IF IO-FG NOT = 0
                 MOVE "BORROWER DOES NOT EXIST" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 MOVE VDUNUM0 TO LN-SSNO(2)
                 MOVE BW-LNAME TO NAME-WORKER
                 MOVE BW-FNAME TO NAME
                 INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?"
                 INSPECT NAME-WORKER REPLACING
                     FIRST "                " BY NAME
                 INSPECT NAME-WORKER REPLACING FIRST "?" BY " "
                 MOVE "S  A320021." TO VDU
                 MOVE NAME-WORKER TO VDUBUF
                 PERFORM SCREENIO
                 MOVE SPACES TO MESS
                 PERFORM SEND-LEGEND.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *************************************************
       WRITE-MODULE SECTION.
      *************************************************
           IF SAVE-MAKER-SSNO = LN-SSNO(1) AND
               SAVE-COMAKER-SSNO = LN-SSNO(2)
              GO TO END-WRITE-MODULE.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-LN-REC TO LN-REC.

           PERFORM REWRITE-LN1-FILE.

      * CHFILE (AUDIT LISTING)
           PERFORM CREATE-CHANGE-REC.
      * LG/CHG.YYMMDD
           PERFORM UPDATE-CHG-FILE.

           PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-BW1-FILE.
           IF SAVE-MAKER-SSNO NOT = LN-SSNO(1)
              MOVE 1 TO MEMO-WHICH-SSNO
              MOVE SAVE-MAKER-SSNO TO MEMO-SSNO1
              MOVE LN-SSNO(1)      TO MEMO-SSNO2
              PERFORM CREATE-LOAN-MEMO
              PERFORM UPDATE-BW1-FILE.
           IF SAVE-COMAKER-SSNO NOT = LN-SSNO(2)
              MOVE 2 TO MEMO-WHICH-SSNO
              MOVE SAVE-COMAKER-SSNO TO MEMO-SSNO1
              MOVE LN-SSNO(2)        TO MEMO-SSNO2
              PERFORM CREATE-LOAN-MEMO
              PERFORM UPDATE-BW1-FILE.

           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***************************************
       UPDATE-BW1-FILE SECTION.
      ***************************************
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BI1-FILE.

      * OLD BORROWER
           IF MEMO-WHICH-SSNO = 1
              MOVE SAVE-MAKER-SSNO TO BW-SSNO
           ELSE
              MOVE SAVE-COMAKER-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-UPDATE-BW1-FILE.
           SUBTRACT 1 FROM BW-LOANS.
           IF MEMO-WHICH-SSNO = 1
              SUBTRACT LN-YTDINTPD FROM BW-INTYTD.
           IF LN-SMALLOAN = "Y"
              SUBTRACT LN-CURBAL FROM BW-LIASMLN
            ELSE
              SUBTRACT LN-CURBAL FROM BW-LIAOTH.
           PERFORM REWRITE-BW1-FILE.
           PERFORM GET-BI-RECORD.
           IF IO-FG = 0
              PERFORM DELETE-BI1-FILE.

      * NEW BORROWER

      * IF REMOVED COMAKER, EXIT
           IF LN-SSNO(MEMO-WHICH-SSNO) = 0
              GO TO EXIT-UPDATE-BW1-FILE.

           MOVE LN-SSNO(MEMO-WHICH-SSNO) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-UPDATE-BW1-FILE.

           ADD 1 TO BW-LOANS.
           IF MEMO-WHICH-SSNO = 1
              ADD LN-YTDINTPD TO BW-INTYTD.
           IF LN-SMALLOAN = "Y"
              ADD LN-CURBAL TO BW-LIASMLN
            ELSE
              ADD LN-CURBAL TO BW-LIAOTH.
           PERFORM REWRITE-BW1-FILE.

           PERFORM GET-BI-RECORD.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-BW1-FILE.

           PERFORM UPDATE-BI1-FILE.

       EXIT-UPDATE-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BI1-FILE.

      ***********************************
       UPDATE-BI1-FILE SECTION.
      ***********************************
           PERFORM CLEAR-BI-FILE.

           MOVE LN-SSNO(MEMO-WHICH-SSNO) TO BI-SSNO.
           MOVE LN-ACCTNO    TO BI-ACCTNO.
           MOVE LN-OWNBR     TO BI-OWNBR.

           MOVE BW-LNAME        TO BI-LNAME.
           MOVE BW-FNAME        TO BI-FNAME.

           MOVE LN-MAKERCD(MEMO-WHICH-SSNO) TO BI-MAKERCD.
           MOVE LN-ORGST        TO BI-ORGST.
           MOVE LN-SALEFIN      TO BI-SALEFIN.
           MOVE LN-GIFTNET      TO BI-GIFTNET.
           MOVE LN-ENTDATE      TO BI-ENTDATE.
           MOVE LN-POFFDATE     TO BI-POFFDATE.

           MOVE LN-PLCD         TO BI-PLCD.
           MOVE LN-CONVERCD     TO BI-CONVERCD.
           MOVE LN-SMALLOAN     TO BI-SMALLOAN.
           MOVE LN-PLDATE       TO BI-PLDATE.
           PERFORM WRITE-BI1-FILE.
       EXIT-UPDATE-BI1-FILE.
           EXIT.

      ***********************************
       CREATE-LOAN-MEMO SECTION.
      ***********************************
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT TODAYS-DATE-YYYYMMDD FROM 99999999 GIVING LM-DATE.
           MOVE "S"       TO LM-MEMO-TYPE.
           MOVE 1         TO HOLD-LM-SEQ.
       WRITE-MEMO.
           MOVE HOLD-MEMO   TO LM-MEMO.
           MOVE HOLD-LM-SEQ TO LM-SEQ.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 99
                 ADD 1 TO HOLD-LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 GO TO IO-ERROR.

      **********************************
       UPDATE-CHG-FILE SECTION.
      **********************************

      * LG/CHG.YYMMDD FILE

           IF SAVE-MAKER-SSNO NOT = LN-SSNO(1)
              PERFORM CLEAR-WORK-ERRLOG
              MOVE "01"            TO WORK-ERRLOG-FIELD
              MOVE "MAKER SS"      TO WORK-ERRLOG-DESC
              MOVE SAVE-MAKER-SSNO TO WORK-ERRLOG-ORG-N0
              MOVE LN-SSNO(1)      TO WORK-ERRLOG-CHG-N0
              PERFORM CREATE-WORK-ERRLOG.

           IF SAVE-COMAKER-SSNO NOT = LN-SSNO(2)
              PERFORM CLEAR-WORK-ERRLOG
              MOVE "02"              TO WORK-ERRLOG-FIELD
              MOVE "COMKR SS"        TO WORK-ERRLOG-DESC
              MOVE SAVE-COMAKER-SSNO TO WORK-ERRLOG-ORG-N0
              MOVE LN-SSNO(2)        TO WORK-ERRLOG-CHG-N0
              PERFORM CREATE-WORK-ERRLOG.

           GO TO EXIT-UPDATE-CHG-FILE.
       CLEAR-WORK-ERRLOG.
           MOVE SPACES     TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR   TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO  TO WORK-ERRLOG-NUMBER.
           MOVE "TO "      TO WORK-ERRLOG-TO-X.
       CREATE-WORK-ERRLOG.
           MOVE "F"                TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME      TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
       EXIT-UPDATE-CHG-FILE.
           EXIT.

      *****************************************
       CREATE-CHANGE-REC SECTION.
      *****************************************
           MOVE CHLOAN-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.

           PERFORM OPEN-CH-FILE.

           IF SAVE-MAKER-SSNO NOT = LN-SSNO(1)
              PERFORM CREATE-SETUP-REC
              MOVE "CHLOAN; MAKER SSNO" TO CH-DESCRIPT
              MOVE "Q"             TO CH-CODE
              MOVE LN-ACCTNO       TO CH-ACCTNO
              MOVE SAVE-MAKER-SSNO TO CH-ORG-SSNO
              MOVE LN-SSNO(1)      TO CH-NEW-SSNO
              PERFORM CREATE-UPDATE.
           IF SAVE-COMAKER-SSNO NOT = LN-SSNO(2)
              PERFORM CREATE-SETUP-REC
              MOVE "CHLOAN; COMAKER SSNO" TO CH-DESCRIPT
              MOVE "Q"               TO CH-CODE
              MOVE LN-ACCTNO         TO CH-ACCTNO
              MOVE SAVE-COMAKER-SSNO TO CH-ORG-SSNO
              MOVE LN-SSNO(2)        TO CH-NEW-SSNO
              PERFORM CREATE-UPDATE.

           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.

           MOVE BRANCH             TO CH-BRNO.
           MOVE DATE-YYYYMMDD      TO CH-DATE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.

           MOVE LN-ACCTNO          TO CH-ACCTNO.

       CREATE-UPDATE.
           PERFORM WRITE-CH-FILE.
       EXIT-CREATE-CHANGE-REC.
           PERFORM CLOSE-CH-FILE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBLP/LPBICL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE LPLOAN-CONAME TO CHLOAN-CONAME.
           MOVE LPLOAN-SYSDATE TO CHLOAN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHLOAN-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CHLOAN-FORM-FIELDS TO WR-BUF.
           MOVE CHLOAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CHLOAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHLOAN-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHLOAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC-PARAGRAPHS SECTION.
      *************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-BI-RECORD.
           MOVE BW-SSNO                  TO BI-SSNO.
           MOVE LN-ACCTNO                TO BI-ACCTNO.
           MOVE LN-OWNBR                 TO BI-OWNBR.
           PERFORM READ-BI1-FILE.
       VERIFY-NOT-SAME-SSNO.
           MOVE SPACES TO IO-TYPE.
           IF LN-SSNO(1) = LN-SSNO(2)
              MOVE "CAN NOT HAVE MAKER AND COMAKER WITH SAME SSNO"
                     TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO IO-TYPE.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM OPEN-SP1-FILE
              PERFORM READ-SP1-FILE
              PERFORM CLOSE-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-DELINQ-VALUE.
           MOVE LN-CONPROFILE(1) TO PROFILE-EDIT.
           INSPECT PROFILE-ED REPLACING ALL "9" BY "-".
           MOVE PROFILE-ED TO DELINQ-VALUE.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

       BLINK-STARS.
           MOVE "S  A000STAR1:F." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2:F." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-MAKER.
           IF MAKER-CD = "M"
              MOVE "MAKER" TO D-MAKER
           ELSE
              IF MAKER-CD = "J"
                 MOVE "JOINT" TO D-MAKER
              ELSE
                 MOVE "ENDORSER" TO D-MAKER.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       RESET-SCREEN.
           MOVE SPACES TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************************
       IO-ROUTINE SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-BI1-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBI1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPCHIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *************************************************
       END-ROUTINE SECTION.
      *************************************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHLOAN-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
