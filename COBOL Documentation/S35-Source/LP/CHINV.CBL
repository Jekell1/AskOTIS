       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHINV.
       DATE-WRITTEN.    01/23/2020.
      *******************************************************
      *                 (LP)
      *  DESCRIPTION:   ASSIGN/UNASSIGN INVESTINET BY ACCOUNT
      *
      *  REV:
      *   01/23/2020 BAH #1415 ALLOW ASSIGN/UNASSIGN FOR INDIVIDUAL
      *                  ACCOUNT, FOLLOW SAME AS SETIMP
      *   02/11/2020 BAH #1415A CHANGE FLASHING MESSAGE TO HAVE INVESTINET
      *                  PHONE NUMBER, DONT ALLOW ASSIGN ON INACTIVE P&L
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/CHINV.CBL S34 05/24/21-11:30:30 >".
      ***********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
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
 
       01  ACTION            PIC X.
       01  IO-TYPE           PIC X(3).
       01  ORIGINAL-REC      PIC X(LN-SIZE).

       01  IX1               PIC 99    COMP-3.
       01  INSPECT-W         PIC X(86).
       01  ADD-ON            PIC X(25).

       01  HOLD-MESSAGE.
           03  FILLER           PIC X(30) VALUE
           "ASSGN INVESTINET 1800-533-8675".
       01  HOLD-OLD-MSG-MEMO.
           03  FILLER           PIC X(30) VALUE
           "SP/CHINV  OLD FLASHING MSG:  ".
           03  SAVE-MESSAGE     PIC X(30).
       01  HOLD-NEW-MSG-MEMO.
           03  FILLER           PIC X(30) VALUE
           "SP/CHINV  NEW FLASHING MSG:  ".
           03  NEW-MESSAGE      PIC X(30).
       01  HOLD-FROZEN-MEMO.
           03  FILLER           PIC X(36) VALUE
           "SP/CHINV: CHANGED FROZEN FLAG FROM ".
           03  SAVE-ACCT-FROZEN PIC X.
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-ACCT-FROZEN  PIC X.
       01  HOLD-FROZEN-REASON-MEMO.
           03  FILLER           PIC X(38) VALUE
           "SP/CHINV: CHANGED FROZEN REASON FROM ".
           03  SAVE-FROZEN-REASON PIC XX.
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-FROZEN-REASON PIC XX.
       01  HOLD-POOL-MEMO.
           03  FILLER           PIC X(34) VALUE
           "SP/CHINV: CHANGED LOAN POOL FROM ".
           03  SAVE-DLPOOL      PIC 9(5).
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-DLPOOL       PIC 9(5).
       01  HOLD-CREDIT-BUREAU-MEMO.
           03  FILLER           PIC X(45) VALUE
           "SP/CHINV: CHANGED CREDIT BUREAU STATUS FROM ".
           03  SAVE-TRW-STATUS  PIC XX.
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-TRW-STATUS   PIC XX.
       01  HOLD-LM-SEQ          PIC 999.
       01  HOLD-MEMO            PIC X(60).

       01  TODAYS-DATE-YYYYMMDD PIC 9(8).

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM      PIC X(16).
           03  D-BORR-INFO     OCCURS 2.
               05  D-SSNO      PIC 999/99/9999.
               05  D-BORR      PIC X(33).
               05  D-MAKERCD   PIC X(1).
           03  D-CURBAL        PIC Z(6).99.
           03  D-PLDATE        PIC 99/99/99 BLANK ZERO.
           03  D-BNKRPTDATE    PIC 99/99/99 BLANK ZERO.
           03  D-ACTIONCD      PIC X(4).
           03  D-MESSAGE       PIC X(30).
           03  D-POOLID        PIC Z(5).
           03  D-FREEZE        PIC X.
           03  D-FREEZE-REASON PIC XX.
           03  D-CREDIT-BUREAU PIC XX.
 
       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(45) VALUE
           "LNNO SS01 NAME01 MAKER01 SS02 NAME02 MAKER02 ".
           03  FILLER        PIC X(40) VALUE
           "CURBAL PLDATE BNKRPTDATE ACTIONCD FMESS ".
           03  FILLER        PIC X(33) VALUE
           "POOLID FREEZEFG FREASON CBSTATUS.".


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHINV_DEF.CPY".
       COPY "LIBLP/CHINV_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CHINV_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "CHINV" TO VDU-FORM-NAME.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           ACCEPT TODAYS-DATE-YYYYMMDD FROM CENTURY-DATE.

       MAIN-MODULE.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

      * ONLY HERE IF VALID ASSIGN/UNASSIGN
           PERFORM UPDATE-MODULE.

           GO TO MAIN-MODULE.

      ***********************************
       ENTER-KEYS SECTION.
      ***********************************
       ENTER-KEYS-NO.

           PERFORM FORM-RESET.

           MOVE SPACES TO IO-TYPE.

           MOVE "ENTER ACCOUNT NUMBER, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE VDUNUM0 TO LN-ACCTNO.

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

           PERFORM DISPLAY-LOAN.

           MOVE LN-REC TO ORIGINAL-REC.

      * ASSIGN/UNASSIGN (A/U)
       ENTER-ACTION.
           MOVE "E  A010ACTION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO ENTER-KEYS-NO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-ACTION.
           MOVE VDUBUF TO ACTION.
           IF ACTION NOT = "A" AND NOT = "U"
              GO TO ENTER-ACTION.

      * ASSIGN SHOULD ONLY BE ALLOWED ON ACTIVE CHARGE OFF LOANS
      *        CAN NOT BE DONE ON BANKRUPTS OR ACTION CODES OF FRAUD
      *        OR DECEASED. MUST HAVE OPEN BALANCE
           IF ACTION = "A"
              IF LN-CURBAL = 0
                 MOVE "MUST HAVE A BALANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION
              ELSE
              IF LN-PLDATE = 0
                 MOVE "MUST BE AN ACTIVE CHARGE OFF" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION
              ELSE
              IF LN-PLCD = "I"
                 MOVE "CAN'T BE AN INACTIVE CHARGE OFF" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION
              ELSE
              IF (LN-BNKRPTDATE NOT = 0) OR
                  (LN-ACTIONCD = "FRUD" OR "DCSD")
                 MOVE "ACCOUNT BANKRUPT OR FRAUD OR DECEASED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION.

      * CHECK IF ALREADY "ASSIGNED" STATUS
           IF ACTION = "A"
              IF LN-DLPOOL = 80000 AND LN-FROZEN-REASON = "AI" AND
                  LN-TRW-STATUS = "93"
                 MOVE "ACCOUNT ALREADY IN ASSIGNED STATUS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION.

      * UNASSIGN SHOULD ONLY BE ALLOWED ON ACCOUNTS WITH POOLID OF 80000
      *          AND A FROZEN REASON CODE OF "AI"
           IF ACTION = "U"
              IF LN-DLPOOL NOT = 80000
                 MOVE "MUST HAVE POOL ID 80000" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION
              ELSE
              IF LN-FROZEN-REASON NOT = "AI"
                 MOVE "MUST HAVE FROZEN REASON 'AI'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION.

      * CHECK IF ALREADY "UNASSIGNED" STATUS
           IF ACTION = "U"
              IF LN-DLPOOL = 1 AND LN-FROZEN-REASON = "  " AND
                  LN-ACCT-FROZEN = " " AND LN-TRW-STATUS = " "
                 MOVE "ACCOUNT ALREADY IN UNASSIGNED STATUS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ACTION.

       END-ENTER-KEYS.
           EXIT.
 

      *****************************************
       DISPLAY-LOAN SECTION.
      *****************************************

           MOVE SPACES TO DISPLAY-LOAN-BUF.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO IX1.
       DISPLAY-NEXT-BW.
           MOVE LN-SSNO(IX1) TO BW-SSNO.
           MOVE SPACES TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              IF IX1 < 3
                 PERFORM GET-BW1
                 MOVE BW-SSNO TO D-SSNO(IX1)
                 INSPECT D-SSNO(IX1) REPLACING ALL "/" BY "-"
                 MOVE INSPECT-W TO D-BORR(IX1)
                 MOVE LN-MAKERCD(IX1) TO D-MAKERCD(IX1).
           IF IX1 < 2
              ADD 1 TO IX1
              GO TO DISPLAY-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           IF ( LN-POCD-NONLOAN )
              GO TO DISPLAY-LOAN-EXIT.

           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           MOVE LN-CURBAL        TO D-CURBAL.

           MOVE LN-PLDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-PLDATE.
           MOVE LN-BNKRPTDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-BNKRPTDATE.
           MOVE LN-ACTIONCD      TO D-ACTIONCD.
           MOVE LN-MESSAGE       TO D-MESSAGE.
           MOVE LN-DLPOOL        TO D-POOLID.
      * FROZEN FIELDS WERE ADDED FOR ANOTHER CUSTOMER AND MAY HAVE JUNK
           MOVE LN-ACCT-FROZEN   TO D-FREEZE.
           MOVE LN-FROZEN-REASON TO D-FREEZE-REASON.
           MOVE LN-TRW-STATUS    TO D-CREDIT-BUREAU.

           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           MOVE DISPLAY-LOAN-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-LOAN-EXIT.
           EXIT.

 
      *************************************************
       UPDATE-MODULE SECTION.
      *************************************************

           MOVE SPACES TO IO-TYPE.

           PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              PERFORM RECORD-DELETED
              GO TO END-UPDATE-MODULE.

           IF LN-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-UPDATE-MODULE.

      * ASSIGN - UPDATE FLASHING MESSAGE - ASSIGNED INVESTINET MM/DD/YYYY
      *          UPDATE POOL ID TO 80000
      *          UPDATE LN-TRW-STATUS TO "93"
      *          UPDATE FROZEN FLAG TO "Y" AND REASON TO "AI"
      * UNASSIGN - CLEAR FLASHING MESSAGE 
      *            UPDATE POOL ID TO 1
      *            CLEAR LN-TRW-STATUS 
      *            CLEAR FROZEN FLAG AND REASON


      * BUILD THE CHANGE MEMOS
           MOVE LN-MESSAGE       TO SAVE-MESSAGE.
           MOVE LN-DLPOOL        TO SAVE-DLPOOL.
           MOVE LN-TRW-STATUS    TO SAVE-TRW-STATUS.
           MOVE LN-ACCT-FROZEN   TO SAVE-ACCT-FROZEN.
           MOVE LN-FROZEN-REASON TO SAVE-FROZEN-REASON.

           IF ACTION = "A"
              MOVE HOLD-MESSAGE TO LN-MESSAGE       NEW-MESSAGE
              MOVE 80000        TO LN-DLPOOL        NEW-DLPOOL
              MOVE "93"         TO LN-TRW-STATUS    NEW-TRW-STATUS
              MOVE "Y"          TO LN-ACCT-FROZEN   NEW-ACCT-FROZEN
              MOVE "AI"         TO LN-FROZEN-REASON NEW-FROZEN-REASON
           ELSE
              MOVE SPACES TO LN-MESSAGE         NEW-MESSAGE
              MOVE 1      TO LN-DLPOOL          NEW-DLPOOL
              MOVE SPACES TO LN-TRW-STATUS      NEW-TRW-STATUS
                             LN-ACCT-FROZEN     NEW-ACCT-FROZEN
                             LN-FROZEN-REASON   NEW-FROZEN-REASON.

           PERFORM REWRITE-LN1-FILE.

      * CREATE CHANGE MEMOS FOR EACH ITEM
           MOVE HOLD-OLD-MSG-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.
           MOVE HOLD-NEW-MSG-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.

           MOVE HOLD-POOL-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.

           MOVE HOLD-CREDIT-BUREAU-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.

           MOVE HOLD-FROZEN-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.
           MOVE HOLD-FROZEN-REASON-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.
           
       END-UPDATE-MODULE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
 
      ***********************************
       CREATE-MEMO SECTION.
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
              IF LM-SEQ NOT = 999
                 ADD 1 TO HOLD-LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 GO TO IO-ERROR.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE LPLOAN-CONAME TO CHINV-CONAME.
           MOVE LPLOAN-SYSDATE TO CHINV-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHINV-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CHINV-FORM-FIELDS TO WR-BUF.
           MOVE CHINV-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CHINV-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHINV-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHINV_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/ACCESS.CPY".
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
 
      ***********************************
       IO-ROUTINE SECTION.
      ***********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
      ***********************************
       END-ROUTINE SECTION.
      ***********************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHINV-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
