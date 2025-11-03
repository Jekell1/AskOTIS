       IDENTIFICATION DIVISION.
       PROGRAM-ID.      REGPAY.
       DATE-WRITTEN.    081004.
      **********************************************************************
      *                 (LP)
      *  DESCRIPTION:   CHANGE REGPAY, PMTS TO DATE & NON-CONTRACTUAL
      *                 MONIES, FOR SOLDIERS & SAILORS RELIEF ACT
      *
      *  REV:
      *  090505 BLM ALLOW ZERO IN PMTS TO DATE & NON-CONTRACTUAL MONIES,
      *             THEY WERE STOPPING ENTRY OF ZERO IF ORIGINAL FIELDS
      *             WERE NOT ZERO, NOT SURE WHY??  PER JIM
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *  BAH 170712 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181004 GLOBAL BWFILE
      *  BAH 181128 GLOBAL LMFILE
      *  BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      **********************************************************************
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
           "@(#)LP/REGPAY.CBL S35 02/20/24-15:28:56 >".
      ***********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  ONE               PIC S9.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  NEW-FLAG          PIC 9       VALUE 1.
       01  IO-TYPE           PIC X(3).
       01  ELE               PIC 9(2).
       01  ORIGINAL-LN-REC   PIC X(LN-SIZE).
       01  CHANGED-LN-REC    PIC X(LN-SIZE).
       01  DIS-BRANCH.
           03  FILLER         PIC X(26)  VALUE
           "ACCOUNT OWNED BY BRANCH # ".
           03  DIS-BR         PIC ZZZ9.
       01  MAKER-CD          PIC X       VALUE SPACES.
       01  NAME-WORKER       PIC X(40) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.
       01  HOLD-TOTPAYMNTD      PIC 9(7)V99 COMP-3 VALUE 0.
       01  HOLD-TOTEXCPAYMNTD   PIC 9(7)V99 COMP-3 VALUE 0.
       01  HOLD-REGPYAMT        PIC 9(7)V99 COMP-3 VALUE 0.
       01  HOLD-SUM-REGPYAMT    PIC 9(7)V99 COMP-3 VALUE 0.
       01  HOLD-1STPYAMT        PIC 9(7)V99 COMP-3 VALUE 0.
       01  HOLD-LASTPYAMT       PIC 9(7)V99 COMP-3 VALUE 0.

       01  MEMO-SUB             PIC 9.
       01  SAVE-LM1-KEY         VALUE SPACES.
           03  SAVE-LM-BRNO     PIC 9(4).
           03  SAVE-LM-ACCTNO   PIC 9(8).
           03  SAVE-LM-DATE     PIC 9(8).
           03  SAVE-LM-SEQ      PIC 9(3).
       01  SAVE-LM-MEMO.
           03  FILLER           PIC X(4)  VALUE "OLD ".
           03  MEMO-DESC        PIC X(19) VALUE SPACES.
           03  MEMO-AMT1        PIC ZZZZZZZ.99-.
           03  FILLER           PIC X(1)  VALUE SPACES.
           03  FILLER           PIC X(4)  VALUE "NEW:".
           03  MEMO-AMT2        PIC ZZZZZZZ.99-.
           03  FILLER           PIC X(2)  VALUE SPACES.
           03  MEMO-USERID      PIC X(8).


      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  MAX               PIC 99   VALUE 3.
       01  SPEC-TABLE.
      *01. MONIES TOWARD PDTHRU
           03 FILLER         PIC X(8) VALUE "ENNN072 ".
      *02. NON-CONTRACTUAL MONIES
           03 FILLER         PIC X(8) VALUE "ENNN072 ".
      *03. REGULAR PAYMENT AMT
           03 FILLER         PIC X(8) VALUE "ENNN062 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 3 TIMES.
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

       01  PROFILE-EDIT      PIC 9(12).
       01  PROFILE-ED REDEFINES PROFILE-EDIT
                             PIC X(12).

       01  DELINQ-VALUE.
           03  DELINQ-ALP.
               05  DELINQ-NUM      PIC ZZZZ,ZZ9.99-  BLANK ZERO.

       01  D-MESSAGE         PIC X(30).

       01  MESSAGE-LIST.
           03  FILLER        PIC X(7)  VALUE "MESSAGE".
           03  MESS-END      PIC X(4)  VALUE "    ".

       01  HOLD-SYSDATE                PIC 9(8).

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
           03  D-LOAN-NUM          PIC X(16).
           03  D-MAKER             PIC X(8).
           03  D-LOANDATE          PIC 99/99/99.
           03  D-LNAMT             PIC ZZZ,ZZZ.99.
           03  D-ORGTERM           PIC ZZ9.
           03  D-PDTH              PIC 9(4).
           03  D-OTHBAL            PIC Z(5).99-.
           03  D-INTBAL            PIC Z(5).99-.
           03  D-CURBAL            PIC Z(5)9.99.
           03  D-OLD-TOTPAYMNTD    PIC ZZZZZZZ.99- BLANK ZERO.
           03  D-OLD-TOTEXCPAYMNTD PIC ZZZZZZZ.99- BLANK ZERO.
           03  D-OLD-REGPYAMT      PIC ZZZZZ9.99.
           03  D-TOTPAYMNTD        PIC ZZZZZZZ.99- BLANK ZERO.
           03  D-TOTEXCPAYMNTD     PIC ZZZZZZZ.99- BLANK ZERO.
           03  D-REGPYAMT          PIC ZZZZZ9.99.

       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(40) VALUE
           "LNNO LNSEQ MAKER LOANDATE LNAMT ORGTERM ".
           03  FILLER        PIC X(27) VALUE
           "PDTH OTHBAL INTBAL PRINBAL ".
           03  FILLER        PIC X(30) VALUE
           "OLD01 OLD02 OLD03 010 020 030.".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  REGPAY-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/REGPAY_DEF.CPY".
       COPY "LIBLP/REGPAY_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/REGPAY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "REGPAY" TO VDU-FORM-NAME.
       COPY "LIBGB/CHKSEC.CPY".

      * IF THIS WAS STANDARD, IT WOULD NOT CLEAR PREVIOUS MENU
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS REGPAY-WINDOW-HANDLE.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH-BRANCH TO BRANCH BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           MOVE " " TO ERRCD.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              PERFORM UPDATE-MEMO.
           GO TO MAIN-MODULE.

      ***********************************
       ENTER-KEYS SECTION.
      ***********************************
       ENTER-KEYS-NO.
           MOVE 1 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "ENTER ACCOUNT NUMBER, F6-SCAN, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
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
       READ-LOAN-FILE.
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

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM DISPLAY-BORROWER.

           PERFORM DISPLAY-LOAN.

           MOVE LN-REC TO ORIGINAL-LN-REC.
           IF LN-OWNBR NOT = BRANCH
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

       END-ENTER-KEYS.
           EXIT.

      *************************************************
       BWLN-SCAN SECTION.
      *************************************************
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
           MOVE 0 TO IO-FG.
       EXIT-BWLN-SCAN.
           EXIT.

      *************************************************
       DISPLAY-BORROWER SECTION.
      *************************************************

           MOVE " " TO DISPLAY-BORR-BUF.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-SOC.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", ?".
           INSPECT NAME-WORKER REPLACING FIRST "?               "
             BY NAME.
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
           MOVE DISPLAY-BORR-BUF TO WR-BUF.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************************
       DISPLAY-LOAN SECTION.
      *************************************************

           MOVE " " TO DISPLAY-LOAN-BUF.
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
           MOVE LN-LOANDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-LOANDATE.
           MOVE LN-LNAMT          TO D-LNAMT.
           MOVE LN-ORGTERM        TO D-ORGTERM.
           MOVE LN-CURBAL         TO D-CURBAL.
           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE         TO D-PDTH.
           MOVE LN-OTHBAL         TO D-OTHBAL.
           MOVE LN-INTBAL         TO D-INTBAL.
           MOVE LN-TOTPAYMNTD     TO D-TOTPAYMNTD
                                     D-OLD-TOTPAYMNTD
                                     HOLD-TOTPAYMNTD.
           MOVE LN-TOTEXCPAYMNTD  TO D-TOTEXCPAYMNTD
                                     D-OLD-TOTEXCPAYMNTD
                                     HOLD-TOTEXCPAYMNTD.
           MOVE LN-REGPYAMT       TO D-REGPYAMT
                                     D-OLD-REGPYAMT
                                     HOLD-REGPYAMT.
           MOVE LN-SUM-REGPYAMT   TO HOLD-SUM-REGPYAMT .
           MOVE LN-1STPYAMT       TO HOLD-1STPYAMT.
           MOVE LN-LASTPYAMT      TO HOLD-LASTPYAMT.
           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LN-MESSAGE TO D-MESSAGE.
           IF LN-MESSAGE = SPACES
              MOVE "." TO MESS-END
      *       MOVE SPACES TO MESSAGE-ATTR
           ELSE
              MOVE ":SR." TO MESS-END.
      *       MOVE "SR" TO MESSAGE-ATTR.
           MOVE D-MESSAGE TO WR-BUF.
           MOVE MESSAGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = SPACES
              PERFORM ALARM
              PERFORM BLINK-STARS 3 TIMES
              PERFORM ALARM.

       DISPLAY-LOAN-EXIT.
           EXIT.

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

           IF ELE = 1
              MOVE "ENTER NEW MONIES TOWARD PDTHRU, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 2
              MOVE "ENTER NEW NON-CONTRACTUAL MONIES, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 3
              MOVE "ENTER NEW REGULAR PAYMENT AMT, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.

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
           GO TO F-01 F-02 F-03 DEPENDING ON ELE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
      *    IF VDUNUM2 = 0
      *       IF HOLD-TOTPAYMNTD NOT = 0
      *          MOVE "PLEASE ENTER NON-ZERO AMOUNT" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-TOTPAYMNTD.
           GO TO CREATE-ROUTINE.
       F-02.
      *    IF VDUNUM2 = 0
      *       IF HOLD-TOTEXCPAYMNTD NOT = 0
      *          MOVE "PLEASE ENTER NON-ZERO AMOUNT" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-TOTEXCPAYMNTD.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUNUM2 = 0
              IF HOLD-REGPYAMT NOT = 0
                 MOVE "PLEASE ENTER NON-ZERO AMOUNT" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-REGPYAMT
                           LN-SUM-REGPYAMT
                           LN-1STPYAMT
                           LN-LASTPYAMT.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *************************************************
       WRITE-MODULE SECTION.
      *************************************************
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

           PERFORM UPDATE-NOTICE-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ******************************************************************
       UPDATE-MEMO SECTION.
      ******************************************************************

           PERFORM OPEN-LM1-FILE.
           IF HOLD-TOTPAYMNTD = LN-TOTPAYMNTD
              IF HOLD-TOTEXCPAYMNTD = LN-TOTEXCPAYMNTD
                 IF HOLD-REGPYAMT = LN-REGPYAMT
                    IF HOLD-SUM-REGPYAMT = LN-SUM-REGPYAMT
                       IF HOLD-1STPYAMT = LN-1STPYAMT
                          IF HOLD-LASTPYAMT = LN-LASTPYAMT
                             GO TO UPDATE-MEMO-EXIT.

           MOVE SPACES       TO LM-REC.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYSDATE.
           MOVE 0             TO SAVE-LM-SEQ.
           MOVE 1 TO MEMO-SUB.

      *-----------------------------------------------------------------
       CREATE-MEMO.
           IF (MEMO-SUB = 1 AND HOLD-TOTPAYMNTD = LN-TOTPAYMNTD)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 2 AND HOLD-TOTEXCPAYMNTD = LN-TOTEXCPAYMNTD)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 3 AND HOLD-REGPYAMT = LN-REGPYAMT)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 4 AND HOLD-SUM-REGPYAMT = LN-SUM-REGPYAMT)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 5 AND HOLD-1STPYAMT = LN-1STPYAMT)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 6 AND HOLD-LASTPYAMT = LN-LASTPYAMT)
              GO TO UPDATE-MEMO-EXIT.

           IF SAVE-LM-SEQ < 999
              ADD 1 TO SAVE-LM-SEQ
           ELSE
              MOVE "MEMO NOT CREATED" TO MESS
              PERFORM SEND-MESS
              GO TO UPDATE-MEMO-EXIT.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
              GO TO CREATE-MEMO.

           MOVE SPACES          TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           IF MEMO-SUB = 1
              MOVE "MONIES TOWARD PDTH:" TO MEMO-DESC
              MOVE HOLD-TOTPAYMNTD TO MEMO-AMT1
              MOVE LN-TOTPAYMNTD TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 2
              MOVE "NONCONTRACT MONIES:" TO MEMO-DESC
              MOVE HOLD-TOTEXCPAYMNTD TO MEMO-AMT1
              MOVE LN-TOTEXCPAYMNTD TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 3
              MOVE "REGULAR PAYMENT   :" TO MEMO-DESC
              MOVE HOLD-REGPYAMT TO MEMO-AMT1
              MOVE LN-REGPYAMT TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 4
              MOVE "SUM REGULAR PAYMNT:" TO MEMO-DESC
              MOVE HOLD-SUM-REGPYAMT TO MEMO-AMT1
              MOVE LN-SUM-REGPYAMT TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 5
              MOVE "FIRST PAYMENT AMT :" TO MEMO-DESC
              MOVE HOLD-1STPYAMT TO MEMO-AMT1
              MOVE LN-1STPYAMT TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 6
              MOVE "LAST PAYMENT AMT  :" TO MEMO-DESC
              MOVE HOLD-LASTPYAMT TO MEMO-AMT1
              MOVE LN-LASTPYAMT TO MEMO-AMT2.

           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.
           MOVE SAVE-LM-MEMO TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF MEMO-SUB < 6
              ADD 1 TO MEMO-SUB
              GO TO CREATE-MEMO.

       UPDATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE HOLD-SYSDATE    TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO REGPAY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO REGPAY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY REGPAY-SCREEN UPON REGPAY-WINDOW-HANDLE.
           MOVE REGPAY-FORM-FIELDS TO WR-BUF.
           MOVE REGPAY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/REGPAY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************
           MOVE REGPAY-HELP--FILE TO HELP-LINK-FILE.
           MOVE REGPAY-HELP--DIR TO HELP-LINK-DIR.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
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
           MOVE PROFILE-ED TO DELINQ-ALP.
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

      ********************************
       IO-ROUTINE SECTION.
      ********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ********************************
       END-ROUTINE SECTION.
      ********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "SP/UTMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY REGPAY-SCREEN.
           DESTROY REGPAY-WINDOW-HANDLE.
           EXIT PROGRAM.
