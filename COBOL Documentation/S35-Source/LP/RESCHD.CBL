       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RESCHD.
       DATE-WRITTEN.    090809.
      **********************************************************************
      *                 (LP)
      *  DESCRIPTION:   RESCHEDULE LOAN PAYMENTS
      *                 REGACC PR# 254
      *
      *  REV:
      *     100324  IF LN-ESCROW-FG = "Y", DO NOT ALLOW RESCHEDULE
      *     110117  SUBTRACT DEFERMENTS OUT WHEN COMPUTING LN-SUM-ORGTERM
      *             THE PAYMENT SCHEDULE & LN-SUM-ORGTERM DID NOT EQUAL
      *             REGACC PR#713
      *     110411 ADDED REASON CODE, MOVE 7 TO BE 8. REGACC PR# 290
      * BAH 131001 DO NOT ALLOW UPDATE IF TOTAL TERM > 360, REGACC PL# 816
      *  CS 151207 RENAMED ORIGINAL-REC TO ORIGINAL-LN-REC
      *                    CHANGED-REC   TO CHANGED-LN-REC
      *  CS 151208 SET LP-LTOUCH-DATE
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *
      * KEC 2016.0321 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED INSPECT STATEMENT WITH USE OF CD-CN-DESC.
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      *  CS 160811  CHANGED H-SP-SPRCLASS FROM 9(2) TO 9(3) H-SP1-KEY
      *
      * KEC 2016.0914 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SET LR-BRNO USING LN-OWNBR.
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *
      * BAH 170713 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181004 GLOBAL BWFILE
      * BAH 181128 GLOBAL LMFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 190103 GLOBAL LPFILE
      *  CS 190111 GLOBAL LTPFILE
      * BAH 190124 SPLIT CDFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 20190709 REMOVED LRFILE PT0151
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 20220414 ADD VALUE 0 TO HOLD-TP-LINE
      * BAH 20220425 UPDATE TP-ACTIONCD2, SECURITIZATION #1551
      * BAH 20220803 HARD CODED START
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/RESCHD.CBL S35 02/20/24-14:50:14 >".
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
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LSR.CPY".
       COPY "LIBLP/LP01LSR_SQL.CPY".
       COPY "LIBLP/LPWSLSR.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".

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

       01  DIS-BRANCH.
           03  FILLER         PIC X(26)  VALUE
           "ACCOUNT OWNED BY BRANCH # ".
           03  DIS-BR         PIC ZZZ9.

      ***********************************
      *    SAVE ORIGINAL VALUES:
      ***********************************
       01  ORIG-PAY-SCHLD-FG PIC X        VALUE " ".
       01  ORIG-SUM-ORGTERM  PIC 9(3)     VALUE 0.
       01  ORIG-SUM-REGPYAMT PIC 9(7)V99  VALUE 0.
       01  ORIG-LCPDTH-DATE  PIC 9(8)     VALUE 0.
       01  ORIG-TOTLCHG      PIC 9(6)     VALUE 0.
       01  ORIG-LC-CODE      PIC 9(6)      VALUE 0.
       01  ORIG-LCBAL        PIC S9(5)V99 VALUE 0.

      ***********************************
      *    HOLD NEW ENTERED FIELDS:
      ***********************************
       01  NEW-EFF-DATE      PIC 9(8).
       01  NEW-NXDUEDATE     PIC 9(8)    VALUE 0.
       01  FILLER REDEFINES NEW-NXDUEDATE.
           03  FILLER        PIC X(6).
           03  NEW-NEXT-DA   PIC 99.

            
       01  NEW-SUM-REGPYAMT  PIC 9(6)V99.
       01  TERM-EXTENSION    PIC 9(3)    VALUE 0.
       01  STOP-LATE-CHG-FG  PIC X VALUE SPACES.
       01  CLEAR-LATE-CHG-FG PIC X VALUE SPACES.
       01  NEW-LC-CODE       PIC 99      VALUE 0.
       01  APPROVED-BY       PIC XXX     VALUE SPACES.
       01  NEW-LCPDTH-DATE   PIC 9(8)    VALUE 0.
       01  REASON-CODE       PIC X(5)    VALUE SPACES.
       01  REASON-TDR-FLAG   PIC X       VALUE SPACES.

       01  ELAPSED-PAY-PERIODS PIC S9(3)    VALUE 0.
       01  HOLD-PAY-SCHLD-TABLE.
           03  HOLD-PAY-SCHLD-TBL    OCCURS 3 TIMES.
               05  HOLD-SCHLD-TERM   PIC 999     COMP.
               05  HOLD-SCHLD-PAYMNT PIC 9(6)V99 COMP.

       01  HOLD-TP-LINE      PIC 9(3)       VALUE 0.
       01  REMAINDER-WK      PIC 9(5)V9(12) VALUE 0.
       01  IX1               PIC 99    COMP-3.
       01  INSPECT-W         PIC X(86).
       01  ADD-ON            PIC X(25).
       01  D-DATE            PIC 99/99/99.

       01  MEMO-SUB             PIC 99.
       01  SAVE-LM1-KEY         VALUE SPACES.
           03  SAVE-LM-BRNO     PIC 9(4).
           03  SAVE-LM-ACCTNO   PIC 9(8).
           03  SAVE-LM-DATE     PIC 9(8).
           03  SAVE-LM-SEQ      PIC 9(3).
       01  SAVE-LM-MEMO.
           03  MEMO-OLD         PIC X(4)  VALUE "OLD ".
           03  MEMO-DESC        PIC X(19) VALUE SPACES.
           03  MEMO-CHAR1.
               05  MEMO-AMT1    PIC ZZZZZZZ.99-.
               05  MEMO-DEC1    REDEFINES MEMO-AMT1
                                PIC Z(9)9B.
               05  MEMO-DATE1   REDEFINES MEMO-AMT1
                                PIC 9999/99/99B.
           03  FILLER           PIC X(1)  VALUE SPACES.
           03  MEMO-NEW         PIC X(4)  VALUE "NEW ".
           03  MEMO-CHAR2.
               05  MEMO-AMT2    PIC ZZZZZZZ.99-.
               05  MEMO-DEC2    REDEFINES MEMO-AMT2
                                PIC Z(9)9B.
               05  MEMO-DATE2   REDEFINES MEMO-AMT2
                                PIC 9999/99/99B.
           03  FILLER           PIC X(2)  VALUE SPACES.
           03  MEMO-USERID      PIC X(8).
       01  SCHED-MEMO REDEFINES SAVE-LM-MEMO.
           03  MEMO-SCH-ALP     PIC X(5).
           03  MEMO-SCH         OCCURS 3.
               05  MEMO-SCH-NO  PIC ZZ9 BLANK ZERO.
               05  MEMO-SCH-AT  PIC XX.
               05  MEMO-SCH-PMT PIC ZZZZZZ.99 BLANK ZERO.
               05  MEMO-SCH-COM PIC XX.
           03  FILLER           PIC X(7).

      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  MAX               PIC 99   VALUE 8.
       01  SPEC-TABLE.
      *01. EFFECTIVE DATE
           03 FILLER         PIC X(8) VALUE "ERNM062 ".
      *02. NEXT DUE DATE
           03 FILLER         PIC X(8) VALUE "E NM062 ".
      *03. NEW REGULAR PAYMENT AMT
           03 FILLER         PIC X(8) VALUE "ENNN062 ".
      *04. NEW TERM
           03 FILLER         PIC X(8) VALUE "ENNN030 ".
      *05. STOP LATE CHARGES Y/N
           03 FILLER         PIC X(8) VALUE "E  A010 ".
      *06. CLEAR LATE CHARGES OWING  Y/N
           03 FILLER         PIC X(8) VALUE "E  A010 ".
      *07. REASON CODE
           03 FILLER         PIC X(8) VALUE "E  A050 ".
      *08. APPROVED BY
           03 FILLER         PIC X(8) VALUE "E  A030 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 8 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       01  HOLD-SYSDATE         PIC 9(8).

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM      PIC X(16).
           03  D-BORR-INFO     OCCURS 2.
               05  D-SSNO      PIC 999/99/9999.
               05  D-BORR      PIC X(33).
               05  D-MAKERCD   PIC X(1).
           03  D-ORGST         PIC XX.
           03  D-ORGBR         PIC 9999      BLANK ZERO.
           03  D-SUBCLASS      PIC Z9.
           03  D-LOANDATE      PIC 99/99/99.
           03  D-MDTE          PIC 99/99/99  BLANK ZERO.
           03  D-LNAMT         PIC ZZZ,ZZZ.99.
           03  D-OTRM          PIC ZZ9       BLANK ZERO.
           03  D-UTRM          PIC ZZ9       BLANK ZERO.
           03  D-OREGPAY       PIC ZZZZZZ.99.
           03  D-UREGPAY       PIC ZZZZZZ.99.
           03  D-DLNO          PIC ZZ99.99   BLANK ZERO.
           03  D-DLDESC        PIC X(15).
           03  D-COLLID        PIC X(20).
           03  D-PDTH          PIC 9(4).
           03  D-OTHBAL        PIC Z(5).99-.
           03  D-INTBAL        PIC Z(5).99-.
           03  D-CURBAL        PIC Z(5)9.99.
           03  D-LC-CODE       PIC 99       BLANK ZERO.

       01  DISPLAY-LOAN-LIST.
           03  FILLER          PIC X(45)  VALUE
           "LNNO SS01 NAME01 MAKER01 SS02 NAME02 MAKER02 ".
           03  FILLER          PIC X(36)  VALUE
           "ORGST ORGBR SUBCL LNDATE MDTE LNAMT ".
           03  FILLER          PIC X(45)  VALUE
           "OTRM UTRM OREGPAY UREGPAY DLNO DLDESC COLLID ".
           03  FILLER        PIC X(27) VALUE
           "PDTH OTHBAL INTBAL PRINBAL.".

       01  PMT-SCHED-BUF.
           03  FILLER               PIC X(16) VALUE
               "PAYMENT SCHEDULE".
           03  PMT-SCHED-LINE OCCURS 3.
               05  PMT-SCHED-NOPAY  PIC ZZ9.
               05  FILLER           PIC X.
               05  PMT-SCHED-PMT    PIC X(4).
               05  FILLER           PIC X.
               05  PMT-SCHED-AT     PIC X.
               05  PMT-SCHED-PAYAMT PIC ZZZZZZ.99.
       01  PMT-SCHED-LIST.
           03  FILLER               PIC X(26) VALUE
           "HEAD SCHED1 SCHED2 SCHED3.".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/ELSCANW.CPY".

       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/RESCHD_DEF.CPY".
       COPY "LIBLP/RESCHD_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/RESCHD_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
       MAIN-PROGRAM SECTION.
      ********************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "RESCHD" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH-BRANCH TO BRANCH BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE LPLOAN-SYSDATE TO ALP-DATE.
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
              PERFORM UPDATE-FILES.
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

           IF LN-UNITPER-CD NOT = "M"
              MOVE "CURRENTLY NO RESCHEDULE ON NON-MONTHLY ACCTS."
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-TOTPAYMNTD  = 0)
              MOVE "LOANS WITH NO PAYMENTS MAY NOT BE RESCHEDULED"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF LN-ESCROW-FG = "Y" OR "I"
              MOVE "LOANS WITH ESCROW MAY NOT BE RESCHEDULED"
                                                    TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.


           MOVE LN-PAY-SCHLD-FG TO  ORIG-PAY-SCHLD-FG.
           MOVE LN-SUM-ORGTERM  TO  ORIG-SUM-ORGTERM.
           MOVE LN-SUM-REGPYAMT TO  ORIG-SUM-REGPYAMT.
           MOVE LN-LCPDTH-DATE  TO  ORIG-LCPDTH-DATE.
           MOVE LN-TOTLCHG      TO  ORIG-TOTLCHG.
           MOVE LN-LC-CODE      TO  ORIG-LC-CODE.
           MOVE LN-LCBAL        TO  ORIG-LCBAL.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM DISPLAY-LOAN.

           MOVE LN-REC TO ORIGINAL-LN-REC.
           IF LN-OWNBR NOT = BRANCH
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

       END-ENTER-KEYS.
           EXIT.

      ****************************************
       BWLN-SCAN SECTION.
      ****************************************
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


      *****************************************
       DISPLAY-LOAN SECTION.
      *****************************************

           MOVE " " TO DISPLAY-LOAN-BUF.
           IF ( LN-POCD-NONLOAN )
              GO TO DISPLAY-LOAN-EXIT.
           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO IX1.
       PROCESS-NEXT-BW.
           MOVE LN-SSNO(IX1) TO BW-SSNO.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              IF IX1 < 3
                 PERFORM GET-BW1
                 MOVE BW-SSNO TO D-SSNO(IX1)
                 INSPECT D-SSNO(IX1) REPLACING ALL "/" BY "-"
                 MOVE INSPECT-W TO D-BORR(IX1)
                 MOVE LN-MAKERCD(IX1) TO D-MAKERCD(IX1).
           IF IX1 < 2
              ADD 1 TO IX1
              GO TO PROCESS-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-ORGST          TO D-ORGST.
           MOVE LN-ORGBR          TO D-ORGBR.
           MOVE SP-SUBCLASS       TO D-SUBCLASS.
           MOVE LN-LOANDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-LOANDATE.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-MDTE.
           MOVE LN-LNAMT          TO D-LNAMT.
           MOVE LN-ORGTERM        TO D-OTRM.
           IF LN-ORGTERM NOT = LN-SUM-ORGTERM
              MOVE LN-SUM-ORGTERM TO D-UTRM.

           MOVE LN-CURBAL         TO D-CURBAL.
           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE         TO D-PDTH.
           MOVE LN-OTHBAL         TO D-OTHBAL.
           MOVE LN-INTBAL         TO D-INTBAL.
           MOVE LN-REGPYAMT       TO D-OREGPAY.
           IF LN-REGPYAMT NOT = LN-SUM-REGPYAMT
              MOVE LN-SUM-REGPYAMT TO D-UREGPAY.
           MOVE LN-DLNO           TO D-DLNO DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT          TO D-DLDESC.
           MOVE LN-COLLID         TO D-COLLID.
           MOVE LN-LC-CODE        TO D-LC-CODE.

           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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
           IF ELE = 6
              IF LN-LCBAL = 0
                 GO TO CREATE-ROUTINE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           PERFORM COMPUTE-DISPLAY-PAYMENT-SCHEDULE.
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
              IF (TERM-EXTENSION + ELAPSED-PAY-PERIODS - LN-TOTNODEF)
                      > 360
                 MOVE "CAN NOT HAVE MORE THAN 360 PAYMENTS" TO MESS
                 PERFORM SEND-MESS
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

           IF ELE = 1
              MOVE "ENTER EFFECTIVE DATE, ENTER=TRANS DATE, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 2
              MOVE "ENTER NEW NEXT DUE DATE, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 3
              MOVE "ENTER NEW REGULAR PAYMENT AMT, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 4
              MOVE "ENTER TERM EXTENSION, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 5
              MOVE "ENTER 'Y' OR 'N' TO STOP LATE CHARGES, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 6
              MOVE
              "ENTER 'Y' OR 'N' TO CLEAR LATE CHARGES OWING, F1-CANCEL"
                 TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 7
              MOVE "ENTER REASON CODE , F6-SCAN, F1-CANCEL" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 8
              MOVE "ENTER INITIALS FOR 'APPROVED BY', F1-CANCEL"
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
       TEST-F6.
           IF ELE = 7 AND VDUSTATUS = "1<"
              MOVE "PS"   TO CDSCAN-TYPE
              MOVE SPACES TO CDSCAN-CODE
              MOVE "WI/CDSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
                 GO TO ENTER-ELE
              ELSE
                 MOVE "S  A050070." TO VDU
                 MOVE CDSCAN-CODE TO VDUBUF
                 PERFORM SCREENIO
                 MOVE CDSCAN-CODE TO VDUBUF
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 DEPENDING ON ELE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF VDUNUM0 = 0
              MOVE "S  8000010." TO VDU
              MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD NEW-EFF-DATE
              PERFORM SCREENIO
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO NEW-EFF-DATE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO NEW-NXDUEDATE SYS-DATE.
           IF NEW-NXDUEDATE < LN-LOANDATE
              MOVE "NEXT DUE MUST BE AFTER LOAN DATE!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NEW-NEXT-DA NOT = LN-1STPYDA
              MOVE "PLEASE POST A 'DC' TO CHANGE DUE DATE!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF NEW-NXDUEDATE < TRANS-DATE
              MOVE "WARNING: NEXT DUE PRIOR TO TODAY!" TO MESS
              PERFORM SEND-MESS.

           MOVE LN-1STPYDATE    TO NUM-DATE.
           MOVE NEW-NXDUEDATE   TO SYS-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER NOT > 1
              MOVE "INSUFFICIENT ELAPSED FROM 1ST PAY TO NEXT DUE!"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF (ELAPSED-UNITPER - LN-TOTNODEF) NOT > 1
              MOVE "INSUFFICIENT ELAPSED FROM 1ST PAY TO NEXT DUE!"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF NEW-FLAG = 2
              PERFORM COMPUTE-DISPLAY-PAYMENT-SCHEDULE.

           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUNUM2 = 0
              MOVE "PLEASE ENTER NON-ZERO AMOUNT!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO NEW-SUM-REGPYAMT.
           IF NEW-FLAG = 2
              PERFORM COMPUTE-DISPLAY-PAYMENT-SCHEDULE.
           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUNUM0 = 0
              MOVE "PLEASE DO NOT ENTER ZERO!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO TERM-EXTENSION.
           PERFORM COMPUTE-DISPLAY-PAYMENT-SCHEDULE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO STOP-LATE-CHG-FG.
           IF STOP-LATE-CHG-FG NOT = "Y"
              IF STOP-LATE-CHG-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF STOP-LATE-CHG-FG NOT = "Y"
              GO TO CREATE-ROUTINE.
       ENTER-LATE-CHG-CODE.
              MOVE "ENTER NEW LATE CHARGE CODE, F1-CANCEL, F6-SCAN"
                 TO MESS
              PERFORM SEND-LEGEND.
           MOVE "ENNN020051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-BAD
                 GO TO ENTER-LATE-CHG-CODE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-LATE-CHG-CODE.

           MOVE VDUNUM0 TO NEW-LC-CODE.
           PERFORM VERIFY-EL-CODE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LATE-CHG-CODE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO CLEAR-LATE-CHG-FG.
           IF CLEAR-LATE-CHG-FG NOT = "Y"
              IF CLEAR-LATE-CHG-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO REASON-CODE.
           PERFORM GET-REASON-DESC.
           MOVE "S  A000REASON." TO VDU.
           IF IO-GOOD
              MOVE CD-DESC TO VDUBUF
              MOVE CD-PS-TDR-FLAG TO REASON-TDR-FLAG
           ELSE
              MOVE SPACES TO REASON-TDR-FLAG
              MOVE "INVALID" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO APPROVED-BY.
           PERFORM GET-COLLECTOR-NAME.
           MOVE "S  A000NAME." TO VDU.
           IF IO-GOOD
              MOVE CDB-DESC TO VDUBUF
           ELSE
              MOVE "INVALID" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *********************************
       UPDATE-FILES SECTION.
      *********************************

           MOVE "Y"              TO LN-PAY-SCHLD-FG.
           MOVE 0                TO LN-TOTLCHG.
           MOVE NEW-SUM-REGPYAMT TO LN-SUM-REGPYAMT.
           COMPUTE LN-SUM-ORGTERM = TERM-EXTENSION
                          + ELAPSED-PAY-PERIODS - LN-TOTNODEF.

           IF STOP-LATE-CHG-FG = "Y"
              MOVE NEW-LC-CODE   TO LN-LC-CODE.
           MOVE NEW-LCPDTH-DATE  TO LN-LCPDTH-DATE.
           IF CLEAR-LATE-CHG-FG = "Y"
              MOVE 0             TO LN-LCBAL
              ADD ORIG-LCBAL     TO LN-LCWAIVERS.

       REWRITE-LOAN.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              GO TO END-UPDATE-FILES.

           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-UPDATE-FILES.

           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-UPDATE-FILES.

           MOVE CHANGED-LN-REC TO LN-REC.

           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-PAYMENT-SCHEDULE.
           PERFORM UPDATE-PAYMENT-FILE.
           PERFORM UPDATE-TRANSACTION-FILE.
           PERFORM UPDATE-LS-TRAILER.
           PERFORM UPDATE-MEMO.
           PERFORM UPDATE-NOTICE-FILE.

       END-UPDATE-FILES.
           PERFORM CLOSE-LN1-FILE.

      ******************************************
       UPDATE-PAYMENT-SCHEDULE SECTION.
      ******************************************
           PERFORM OPEN-LTP1-FILE.
           MOVE LTP-PATH-OWNBR TO LTP-BRNO
                                  QLTP1-WBEG-BRANCH
                                  QLTP1-WEND-BRANCH.
           MOVE LN-ACCTNO      TO LTP-ACCTNO
                                  QLTP1-WBEG-ACCTNO
                                  QLTP1-WEND-ACCTNO.
           MOVE 0              TO LTP-SEQNO
                                  QLTP1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTP1-WEND-SEQNO.

           PERFORM START-LTP1-FILE.
       NEXT-LT.
           PERFORM READ-LTP1-FILE-NEXT.
           IF IO-GOOD 
              IF (LTP-BRNO = LTP-PATH-OWNBR) AND
                 (LTP-ACCTNO = LN-ACCTNO)
                 PERFORM DELETE-LTP1-FILE.

           PERFORM CLEAR-LTP-FILE.

           MOVE LN-OWNBR   TO LTP-BRNO.
           MOVE LN-ACCTNO  TO LTP-ACCTNO.
           MOVE 1          TO LTP-SEQNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE HOLD-SCHLD-TERM(SUB) TO LTP-SCHLD-TERM(SUB)
              MOVE HOLD-SCHLD-PAYMNT(SUB) TO LTP-SCHLD-PAYMNT(SUB)
           END-PERFORM.

           PERFORM WRITE-LTP1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      *********************************************
       UPDATE-PAYMENT-FILE SECTION.
      *********************************************
           MOVE 0 TO SUB.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0 OR (LP-PATH-OWNBR NOT = LP-BRNO) OR
                                (LN-ACCTNO NOT = LP-ACCTNO)
              MOVE LP-PATH-OWNBR TO LP-BRNO
              MOVE LN-ACCTNO     TO LP-ACCTNO
              MOVE 0             TO LP-SEQNO.

       NEXT-AVAIL-SEQ.
           ADD 1 TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD-LP1-KEY.
           PERFORM READ-LP1-FILE.
           IF IO-FG = 0
      *       UNLOCK LP-FILE
              GO TO NEXT-AVAIL-SEQ.

           IF CLEAR-LATE-CHG-FG NOT = "Y"
              GO TO UPDATE-LP-FILE.

       CREATE-WL-LP-REC.
           PERFORM CLEAR-LP-FILE.
           MOVE HOLD-LP1-KEY     TO LP1-KEY.
           MOVE "WL"             TO LP-TRCD.
           MOVE TRANS-DATE       TO LP-TRDATE.
           MOVE NEW-EFF-DATE     TO LP-PAYDATE.
           MOVE "RESCHD"         TO LP-REFNO.
           MOVE 0                TO LP-TRAMT.
           MOVE LN-OTHBAL        TO LP-OTHBAL.
           MOVE LN-OT2BAL        TO LP-OT2BAL.
           MOVE LN-INTBAL        TO LP-INTBAL.
           MOVE LN-LCBAL         TO LP-LCBAL.
           MOVE LN-CURBAL        TO LP-CURBAL.
           MOVE NEW-LCPDTH-DATE  TO LP-LCPDTH-DATE
                                    LP-PDTH-DATE.
           MOVE ORIG-LCBAL       TO LP-APLC.
           MOVE LN-SSNO(1)       TO LP-SSNO.
           PERFORM SET-POSTDATE.
           MOVE POST-USERID      TO LP-USERID.
           MOVE POST-TIME        TO LP-POSTTIME.
           MOVE POST-DATE        TO LP-POSTDATE.
           MOVE BRANCH           TO LP-POSTING-BR.
           PERFORM WRITE-LP1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       WL-NEXT-AVAIL-SEQ.
           ADD 1 TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD-LP1-KEY.
           PERFORM READ-LP1-FILE.
           IF IO-FG = 0
      *       UNLOCK LP-FILE
              GO TO WL-NEXT-AVAIL-SEQ.

       UPDATE-LP-FILE.
           PERFORM CLEAR-LP-FILE.
           MOVE HOLD-LP1-KEY     TO LP1-KEY.
           MOVE "PS"             TO LP-TRCD.
           MOVE TRANS-DATE       TO LP-TRDATE.
           MOVE NEW-EFF-DATE     TO LP-PAYDATE.
           MOVE REASON-CODE      TO LP-REFNO.
           MOVE 0                TO LP-TRAMT.
           MOVE LN-OTHBAL        TO LP-OTHBAL.
           MOVE LN-OT2BAL        TO LP-OT2BAL.
           MOVE LN-INTBAL        TO LP-INTBAL.
           MOVE LN-LCBAL         TO LP-LCBAL.
           MOVE LN-CURBAL        TO LP-CURBAL.
           MOVE NEW-LCPDTH-DATE  TO LP-LCPDTH-DATE
                                    LP-PDTH-DATE.
           MOVE LN-SSNO(1)       TO LP-SSNO.
           PERFORM SET-POSTDATE.
           MOVE POST-USERID      TO LP-USERID.
           MOVE POST-TIME        TO LP-POSTTIME.
           MOVE POST-DATE        TO LP-POSTDATE.
           MOVE BRANCH           TO LP-POSTING-BR.
           PERFORM WRITE-LP1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      ************************************************
       UPDATE-TRANSACTION-FILE SECTION.
      ************************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BRANCH     TO TP-BRNO.
           MOVE TRANS-DATE TO TP-DATE.
           MOVE LN-CLASS   TO TP-CLASS.
           MOVE LN-ACCTNO  TO TP-ACCTNO.
       TR-CHECK-EXISTS.
           ADD 1             TO HOLD-TP-LINE.
           MOVE HOLD-TP-LINE TO TP-LINE.
           MOVE TRP1-KEY     TO LPWSTRP-TRP1-KEY.

           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
      *       UNLOCK TRP-FILE
              GO TO TR-CHECK-EXISTS.

           IF CLEAR-LATE-CHG-FG NOT = "Y"
              GO TO CREATE-TR.

       CREATE-WL-TR-REC.
           PERFORM CLEAR-TRP-FILE.

           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.

           MOVE BRANCH             TO TP-POSTING-BR.
           MOVE " "                TO TP-OTHBR-FG.

           MOVE LN-LAWCODE         TO TP-LAWCODE.
           MOVE LN-SPRCLASS        TO TP-SPRCLASS.
           MOVE LN-PLCD            TO TP-PLCD.
           MOVE LN-PLTYPE          TO TP-PLTYPE.
           IF LN-JDDATE NOT = 0
              MOVE "J"             TO TP-JDCD.
           MOVE LP-SSNO            TO TP-SSNO.
           MOVE LP-IBPC            TO TP-IBPC.
           MOVE "WL"               TO TP-TRCD.

           MOVE LP-REFNO           TO TP-REFNO.

           MOVE LP-PAYDATE         TO TP-PAYDATE.
           MOVE NEW-NXDUEDATE      TO TP-DUEDATE.

           MOVE 0                  TO TP-TRAMT.

           ADD LP-APOTH            TO TP-APOTH.
           ADD LP-APOT2            TO TP-APOT2.
           ADD LP-APINT            TO TP-APINT.
           ADD LP-APLC             TO TP-APLC.
           ADD LP-APCUR            TO TP-APCUR.

           ADD LP-OTHBAL           TO TP-OTHBAL.
           ADD LP-OT2BAL           TO TP-OT2BAL.
           ADD LP-INTBAL           TO TP-INTBAL.
           ADD LP-LCBAL            TO TP-LCBAL.
           ADD LP-CURBAL           TO TP-CURBAL.

           MOVE NEW-LCPDTH-DATE    TO TP-PDTH-DATE.
           MOVE LP-TILLNO          TO TP-TILLNO.
           MOVE NEW-LCPDTH-DATE    TO TP-LCPDTH-DATE.
           MOVE LN-DLNO            TO TP-DLNO.
           MOVE LP-DLPROC          TO TP-DLPROC.
           MOVE LP-INTPAID         TO TP-INTPAID.

           MOVE LN-SERVACCT        TO TP-SERVACCT.
           MOVE LN-SALEFIN         TO TP-SALEFIN.
           MOVE LN-LOANTYPE        TO TP-LOANTYPE.

      * MOVED THESE 4 STATEMENTS OUTSIDE OF THE IF LP-TRCD = "DC" BELOW
           COMPUTE TP-TOTNODEF = LN-TOTNODEF + LN-TOTNOALDEL.
           MOVE LN-SUM-LNAMT       TO TP-LNAMT.
           MOVE LN-ORIG-1STPYDATE  TO TP-ORIG-1STPYDATE.
           MOVE LN-SUM-ORGTERM     TO TP-ORGTERM.

           MOVE LN-REPOCD          TO TP-REPOCD.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID        TO TP-USERID.
           MOVE POST-TIME          TO TP-POSTTIME.
           MOVE POST-DATE          TO TP-POSTDATE.
           MOVE LN-UNITPER-CD      TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO TP-UNITPER-FREQ.
           MOVE LN-COLLECTOR       TO TP-COLLECTOR.
           MOVE LN-ACTIONCD2       TO TP-ACTIONCD2.

           MOVE LN-PROMDATE        TO TP-PROMDATE.
           MOVE LN-PROMAMT         TO TP-PROMAMT.
           MOVE LN-PROMDAYS-PASTDUE
                                   TO TP-PROMDAYS-PASTDUE.
           MOVE LN-PROMID          TO TP-PROMID.

           MOVE SP-DEFFRMLA        TO TP-SP-DEFFRMLA.

           PERFORM WRITE-TRP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF TP-LINE > 998
               MOVE "NO MORE POSTING TO THIS ACCOUNT TODAY !!!!" TO MESS
               PERFORM SEND-MESS.

       WL-TR-CHECK-EXISTS.
           ADD 1             TO HOLD-TP-LINE.
           MOVE HOLD-TP-LINE TO TP-LINE.

           MOVE TRP1-KEY TO LPWSTRP-TRP1-KEY.
           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
      *       UNLOCK TRP-FILE
              GO TO WL-TR-CHECK-EXISTS.

       CREATE-TR.
           PERFORM CLEAR-TRP-FILE.
           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.

           MOVE BRANCH             TO TP-POSTING-BR.
           MOVE " "                TO TP-OTHBR-FG.

           MOVE LN-LAWCODE         TO TP-LAWCODE.
           MOVE LN-SPRCLASS        TO TP-SPRCLASS.
           MOVE LN-PLCD            TO TP-PLCD.
           MOVE LN-PLTYPE          TO TP-PLTYPE.
           IF LN-JDDATE NOT = 0
              MOVE "J"             TO TP-JDCD.
           MOVE LP-SSNO            TO TP-SSNO.
           MOVE LP-IBPC            TO TP-IBPC.
           MOVE "PS"               TO TP-TRCD.

           MOVE LP-REFNO           TO TP-REFNO.

           MOVE LP-PAYDATE         TO TP-PAYDATE.
           MOVE NEW-NXDUEDATE      TO TP-DUEDATE.

           MOVE 0                  TO TP-TRAMT.

           ADD LP-APOTH            TO TP-APOTH.
           ADD LP-APOT2            TO TP-APOT2.
           ADD LP-APINT            TO TP-APINT.
           ADD ORIG-LCBAL          TO TP-APLC.
           ADD LP-APCUR            TO TP-APCUR.

           ADD LP-OTHBAL           TO TP-OTHBAL.
           ADD LP-OT2BAL           TO TP-OT2BAL.
           ADD LP-INTBAL           TO TP-INTBAL.
           ADD LP-LCBAL            TO TP-LCBAL.
           ADD LP-CURBAL           TO TP-CURBAL.

           MOVE NEW-LCPDTH-DATE    TO TP-PDTH-DATE.
           MOVE LP-TILLNO          TO TP-TILLNO.
           MOVE NEW-LCPDTH-DATE    TO TP-LCPDTH-DATE.
           MOVE LN-DLNO            TO TP-DLNO.
           MOVE LP-DLPROC          TO TP-DLPROC.
           MOVE LP-INTPAID         TO TP-INTPAID.

           MOVE LN-SERVACCT        TO TP-SERVACCT.
           MOVE LN-SALEFIN         TO TP-SALEFIN.
           MOVE LN-LOANTYPE        TO TP-LOANTYPE.

      * MOVED THESE 4 STATEMENTS OUTSIDE OF THE IF LP-TRCD = "DC" BELOW
           COMPUTE TP-TOTNODEF = LN-TOTNODEF + LN-TOTNOALDEL.
           MOVE LN-SUM-LNAMT       TO TP-LNAMT.
           MOVE LN-ORIG-1STPYDATE  TO TP-ORIG-1STPYDATE.
           MOVE LN-SUM-ORGTERM     TO TP-ORGTERM.

           MOVE LN-REPOCD          TO TP-REPOCD.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID        TO TP-USERID.
           MOVE POST-TIME          TO TP-POSTTIME.
           MOVE POST-DATE          TO TP-POSTDATE.
           MOVE LN-UNITPER-CD      TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO TP-UNITPER-FREQ.
           MOVE LN-COLLECTOR       TO TP-COLLECTOR.
           MOVE LN-ACTIONCD2       TO TP-ACTIONCD2.

           MOVE LN-PROMDATE        TO TP-PROMDATE.
           MOVE LN-PROMAMT         TO TP-PROMAMT.
           MOVE LN-PROMDAYS-PASTDUE
                                   TO TP-PROMDAYS-PASTDUE.
           MOVE LN-PROMID          TO TP-PROMID.

           MOVE SP-DEFFRMLA        TO TP-SP-DEFFRMLA.

           PERFORM WRITE-TRP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF TP-LINE > 998
               MOVE "NO MORE POSTING TO THIS ACCOUNT TODAY !!!!" TO MESS
               PERFORM SEND-MESS.

           PERFORM CLOSE-TRP1-FILE.

       UPDATE-TRANSACTION-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-LS-TRAILER SECTION.
      ******************************************************************
           PERFORM OPEN-LSR1-FILE.

           MOVE LN-OWNBR  TO LSR-BRNO.
           MOVE LN-ACCTNO TO LSR-ACCTNO.
           MOVE 1         TO LSR-SEQNO.
           PERFORM READ-LSR1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LSR-FILE
              MOVE LN-OWNBR  TO LSR-BRNO
              MOVE LN-ACCTNO TO LSR-ACCTNO
              MOVE 1         TO LSR-SEQNO.

      * THIS IS FIRST RESCHEDULE
           IF LSR-INITIAL-RESCHED-DATE = 0
              MOVE TRANS-DATE  TO LSR-INITIAL-RESCHED-DATE
              MOVE REASON-CODE TO LSR-INITIAL-REASON-CODE
              IF REASON-TDR-FLAG = "Y"
                 MOVE TRANS-DATE  TO LSR-INITIAL-TDR-DATE
                 MOVE REASON-CODE TO LSR-INITIAL-TDR-REASON.

      * ANY RESCHEDULE WILL UPDATE THE CURRENT FIELDS
           MOVE TRANS-DATE  TO LSR-CURRENT-RESCHED-DATE.
           MOVE REASON-CODE TO LSR-CURRENT-REASON-CODE.
           IF REASON-TDR-FLAG = "Y"
              MOVE TRANS-DATE  TO LSR-CURRENT-TDR-DATE
              MOVE REASON-CODE TO LSR-CURRENT-TDR-REASON.

           IF IO-FG NOT = 0
              PERFORM WRITE-LSR1-FILE
           ELSE
              PERFORM REWRITE-LSR1-FILE.

           PERFORM CLOSE-LSR1-FILE.


      *********************************************
       UPDATE-MEMO SECTION.
      *********************************************

           PERFORM OPEN-LM1-FILE.

           IF ORIG-PAY-SCHLD-FG = LN-PAY-SCHLD-FG
              IF ORIG-SUM-REGPYAMT = LN-SUM-REGPYAMT
                 IF ORIG-SUM-ORGTERM = LN-SUM-ORGTERM
                    IF ORIG-LCPDTH-DATE = LN-LCPDTH-DATE
                       IF ORIG-TOTLCHG = LN-TOTLCHG
                          IF ORIG-LC-CODE = LN-LC-CODE
                             GO TO UPDATE-MEMO-EXIT.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           MOVE HOLD-SYSDATE  TO SAVE-LM-DATE.
           COMPUTE SAVE-LM-DATE = 99999999 - SAVE-LM-DATE.
           MOVE 0             TO SAVE-LM-SEQ.
           MOVE 1             TO MEMO-SUB.

       CREATE-MEMO.
           MOVE SPACES TO SAVE-LM-MEMO.

           IF (MEMO-SUB = 4 AND ORIG-PAY-SCHLD-FG = LN-PAY-SCHLD-FG)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 5 AND ORIG-SUM-REGPYAMT = LN-SUM-REGPYAMT)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 6 AND ORIG-SUM-ORGTERM = LN-SUM-ORGTERM)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 7 AND ORIG-LCPDTH-DATE = LN-LCPDTH-DATE)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 8 AND ORIG-TOTLCHG = LN-TOTLCHG)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 9 AND ORIG-LC-CODE = LN-LC-CODE)
              ADD 1 TO MEMO-SUB.
           IF (MEMO-SUB = 10 AND ORIG-LCBAL = LN-LCBAL)
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

           MOVE SPACES       TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           IF MEMO-SUB = 1
              MOVE
              "RESCHEDULE: EFFECTIVE ########, NEW DUE %%%%%%%%"
                 TO SAVE-LM-MEMO
              MOVE NEW-EFF-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE
              INSPECT SAVE-LM-MEMO REPLACING FIRST "########" BY D-DATE
              MOVE NEW-NXDUEDATE TO D-DATE
              INSPECT SAVE-LM-MEMO REPLACING FIRST "%%%%%%%%" BY D-DATE
           ELSE
           IF MEMO-SUB = 2
              MOVE SPACES TO SAVE-LM-MEMO
              STRING "RESCHEDULE APPROVED BY ",
                     "###################################"
                     DELIMITED BY SIZE INTO SAVE-LM-MEMO
              PERFORM GET-COLLECTOR-NAME
              INSPECT SAVE-LM-MEMO REPLACING FIRST
                      "###################################"
                      BY CDB-DESC
           ELSE
           IF MEMO-SUB = 3
              MOVE "SCH: "             TO MEMO-SCH-ALP
              MOVE LTP-SCHLD-TERM(1)   TO MEMO-SCH-NO(1)
              MOVE " @"                TO MEMO-SCH-AT(1)
              MOVE LTP-SCHLD-PAYMNT(1) TO MEMO-SCH-PMT(1)
              MOVE ", "                TO MEMO-SCH-COM(1)
              MOVE LTP-SCHLD-TERM(2)   TO MEMO-SCH-NO(2)
              MOVE " @"                TO MEMO-SCH-AT(2)
              MOVE LTP-SCHLD-PAYMNT(2) TO MEMO-SCH-PMT(2)
              MOVE ", "                TO MEMO-SCH-COM(2)
              IF LTP-SCHLD-TERM(3) NOT = 0
                 MOVE "  "                TO MEMO-SCH-COM(2)
                 MOVE LTP-SCHLD-TERM(3)   TO MEMO-SCH-NO(3)
                 MOVE " @"                TO MEMO-SCH-AT(3)
                 MOVE LTP-SCHLD-PAYMNT(3) TO MEMO-SCH-PMT(3)
              END-IF
              MOVE "  "               TO MEMO-SCH-COM(3)
           ELSE
           IF MEMO-SUB = 4
              MOVE "LOAN SCHEDULE FLAG:" TO MEMO-DESC
              MOVE ORIG-PAY-SCHLD-FG     TO MEMO-CHAR1
              MOVE LN-PAY-SCHLD-FG       TO MEMO-CHAR2
           ELSE
           IF MEMO-SUB = 5
              MOVE "SUM REGULAR PAYMNT:" TO MEMO-DESC
              MOVE ORIG-SUM-REGPYAMT     TO MEMO-AMT1
              MOVE LN-SUM-REGPYAMT       TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 6
              MOVE "SUM ORIGINAL TERM :" TO MEMO-DESC
              MOVE ORIG-SUM-ORGTERM      TO MEMO-DEC1
              MOVE LN-SUM-ORGTERM        TO MEMO-DEC2
           ELSE
           IF MEMO-SUB = 7
              MOVE "LATE CHG PAID THRU:" TO MEMO-DESC
              MOVE ORIG-LCPDTH-DATE      TO MEMO-DATE1
              MOVE LN-LCPDTH-DATE        TO MEMO-DATE2
           ELSE
           IF MEMO-SUB = 8
              MOVE "TOTAL LATE CHARGES:" TO MEMO-DESC
              MOVE ORIG-TOTLCHG          TO MEMO-AMT1
              MOVE LN-TOTLCHG            TO MEMO-AMT2
           ELSE
           IF MEMO-SUB = 9
              MOVE "LOAN LATE CHG CODE:" TO MEMO-DESC
              MOVE ORIG-LC-CODE          TO MEMO-DEC1
              MOVE LN-LC-CODE            TO MEMO-DEC2
           ELSE
           IF MEMO-SUB = 10
              MOVE "LOAN LATE CHG BAL :" TO MEMO-DESC
              MOVE ORIG-LCBAL            TO MEMO-AMT1
              MOVE LN-LCBAL              TO MEMO-AMT2.

           IF MEMO-SUB > 3
              MOVE "OLD" TO MEMO-OLD
              MOVE "NEW" TO MEMO-NEW.

           MOVE POST-USERID TO MEMO-USERID.
           MOVE SAVE-LM-MEMO TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           IF MEMO-SUB < 9
              ADD 1 TO MEMO-SUB
              GO TO CREATE-MEMO.

       UPDATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TRANS-DATE     TO CRNOR2-SYSDATE.
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

      *******************************************************
       COMPUTE-DISPLAY-PAYMENT-SCHEDULE SECTION.
      *******************************************************
       COMPUTE-PAY-SCHEDULE.

      *
      * FOR EXAMPLE, 1ST PAY DATE IS 2/5/09
      *              NEW DUE DATE IS 9/5/09
      *              ELAPSED UNIT PERIODS IS 7
      *              INCREMENT 2/5/09 BY (7 - 1) IS LCPDTH OF 8/5/09
      * ???   NOTE:  TIMEUPER HAS ELAPSED-UNITPER-REM WHICH IS
      *              REMAINING DAYS,  DO WE NEED TO INCREMENT
      *              LATE CHARGE PD-THRU BY THAT # OF DAYS???
      *
           MOVE LN-1STPYDATE    TO NUM-DATE.
           MOVE NEW-NXDUEDATE    TO SYS-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.
           MOVE ELAPSED-UNITPER TO ELAPSED-PAY-PERIODS.

           MOVE LN-1STPYDATE    TO NDTE-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           COMPUTE NDTE-HOLD = ELAPSED-UNITPER - 1.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NEW-LCPDTH-DATE.

           MOVE LOW-VALUES TO HOLD-PAY-SCHLD-TABLE.
           COMPUTE HOLD-SCHLD-TERM(1) =
              ELAPSED-PAY-PERIODS - LN-TOTNODEF.
           DIVIDE LN-TOTPAYMNTD BY HOLD-SCHLD-TERM(1)
              GIVING HOLD-SCHLD-PAYMNT(1) REMAINDER REMAINDER-WK.
           IF REMAINDER-WK = 0
              MOVE 2 TO SUB
              GO TO SKIP-ODD-PAY.
           SUBTRACT 1 FROM HOLD-SCHLD-TERM(1).
           COMPUTE HOLD-SCHLD-PAYMNT(2) =
             LN-TOTPAYMNTD - (HOLD-SCHLD-TERM(1) * HOLD-SCHLD-PAYMNT(1)).
           MOVE 1 TO HOLD-SCHLD-TERM(2).
           MOVE 3 TO SUB.
       SKIP-ODD-PAY.
           MOVE TERM-EXTENSION  TO HOLD-SCHLD-TERM(SUB).
           MOVE NEW-SUM-REGPYAMT TO HOLD-SCHLD-PAYMNT(SUB).

       DISPLAY-PMT-SCHEDULE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF HOLD-SCHLD-TERM(SUB) > 0
                 MOVE HOLD-SCHLD-TERM(SUB) TO PMT-SCHED-NOPAY(SUB)
                 IF HOLD-SCHLD-TERM(SUB) > 1
                    MOVE "PMTS" TO PMT-SCHED-PMT(SUB)
                 ELSE
                    MOVE "PMT " TO PMT-SCHED-PMT(SUB)
                 END-IF
                 MOVE "@" TO PMT-SCHED-AT(SUB)
                 MOVE HOLD-SCHLD-PAYMNT(SUB) TO PMT-SCHED-PAYAMT(SUB)
              END-IF
           END-PERFORM.

           MOVE PMT-SCHED-BUF TO WR-BUF.
           MOVE PMT-SCHED-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR (ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020051." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE LPLOAN-CONAME TO RESCHD-CONAME.
           MOVE LPLOAN-SYSDATE TO RESCHD-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY RESCHD-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE RESCHD-FORM-FIELDS TO WR-BUF.
           MOVE RESCHD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE RESCHD-HELP--FILE TO HELP-LINK-FILE.
           MOVE RESCHD-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/RESCHD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPLSRCL.CPY".
       COPY "LIBLP/LPTRPCL.CPY".
       COPY "LIBLP/LPLTPCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".

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
      *-----------------------------------------------------------------
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
      *-----------------------------------------------------------------
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-BAD
              MOVE " INVALID DEALER" TO DL-SHORT.
       VERIFY-EL-CODE.
           PERFORM OPEN-CD1-FILE.
           MOVE "EL"        TO CD-TYPE.
           MOVE NEW-LC-CODE TO CD-EL-CODE.
           MOVE SPACES      TO CD-EL-FILLER.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       GET-REASON-DESC.
           PERFORM OPEN-CD1-FILE.
           MOVE "PS"        TO CD-TYPE.
           MOVE REASON-CODE TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       GET-COLLECTOR-NAME.
           PERFORM OPEN-CDB1-FILE.
           MOVE "CN"        TO CDB-TYPE.
           MOVE 0           TO CDB-BRANCH.
           MOVE APPROVED-BY TO CDB-CODE.
           PERFORM READ-CDB1-FILE.
           IF IO-BAD
              MOVE APPROVED-BY TO CDB-DESC.
           PERFORM CLOSE-CDB1-FILE.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
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
       RESET-SCREEN.
           PERFORM FORM-RESET.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLSR1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RESCHD-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
