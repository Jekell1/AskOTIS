      *A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPN1.
       DATE-WRITTEN.    012484.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOANS (NON-SALES FINANCE)
      *                 VERIFY LOAN DELETE
      *
      *  REV:
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BLV 121697 DON'T ALLOW VOID IF REBATE CHECK DONE
      *  BAH 020698 DON'T ALLOW VOID IF A CONVERTED LOAN (WORLD #93 12/95)
      *  BLV 990514 ALLOW VOID OF CONVERTED ACCOUNT, BUT ADDED PASSWORD
      *             TO STOP
      *   CS 991119 CHANGED TEST OF LN-CONVERCD = 'Y' TO LN-CONVERCD NOT = " "
      *  MJD 001006 ADDED TEST FOR PL AND REPO STATUS ACCOUNTS. (AS PER JTG)
      *  BAH 031021 ADDED TEST OF LN-TOTPAYMNTD NOT = 0
      *             POSTED "OT" THEN "AP" FOR $15.00, BALANCE NEVER CHANGED,
      *             SO IT ALLOWED ACCOUNT TO BE VOIDED, PUT $15 IN
      *             TOTPAYMNTD, SO WE NEED TO CHECK FOR THAT ! MULL PL# 421
      *  BLM 100309 IF ACCT HAS ESCROW, DON'T ALLOW VOID UNLESS ESCROW
      *             BALANCE IS ZERO, REGWAN PR# 3506
      *  BAH 120726 PASSWORD VOID IF ORGBR NOT = OWNBR, REGMGM PR# 4082
      * BAH 170629 ACTIVATE 8 DIGIT DATES, PD0006
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPN1.CBL S34 05/21/21-13:44:22 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LP01ES_SQL.CPY".
       COPY "LIBLP/LPWSES.CPY".
       01  WK-IFN          PIC X(2) VALUE "WK".
       01  WK-KEY          PIC 9(2).

       01  ERRORS-EXIST    PIC X     VALUE "N".
       01  EXTEND-VERIFY   PIC X     VALUE "Y".
       01  MESS1           PIC X(60) VALUE " ".
       01  MSEL-LIST       PIC X(5)  VALUE "MSEL.".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CONVERTED-SEQ             VALUE 1.
       78  OWNBR-ORGBR-SEQ           VALUE 2.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-ES1-FILE.
           PERFORM CLOSE-CA1-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM GET-GPENV.

           MOVE USER-PID TO TEMP-PID.
       MAIN-MODULE.
           PERFORM VERIFY-MESSAGE.
           MOVE 7 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE WK-SAVE-REC TO LN-REC.
           PERFORM CLOSE-WK-FILE.

           IF LN-CURBAL NOT = LN-LNAMT
              MOVE "LOAN CURRENT BALANCE NOT = LOAN AMT" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-INTBAL NOT = 0
              MOVE "LOAN INTEREST BALANCE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-OTHBAL NOT = 0
              MOVE "LOAN OTHER BALANCE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-LCBAL NOT = 0
              MOVE "LOAN LATE CHARGE BALANCE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-POFFDATE NOT = 0
              MOVE "LOAN PAYOFF DATE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF NOT ( LN-POCD-OPEN )
              MOVE "LOAN PAYOFF CODE NOT = SPACES" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-PLDATE NOT = 0
              MOVE "LOAN IS IN P&&L STATUS" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-REPOCD = "X" OR "S"
              MOVE "LOAN IS IN REPO STATUS" TO MESS
              PERFORM ERROR-MESSAGE.
      * SOME TYPE OF PAYMENT WAS TAKEN
           IF LN-TOTPAYMNTD NOT = 0
              MOVE "PAYMENTS-TO-DATE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.

      * REBATES DONE WITH "CHECK" IN THE REF, ALLOW YOU TO DELETE LOAN
           IF LN-REBATE-TAB NOT = SPACES
              MOVE "LOAN HAS REBATES" TO MESS
              PERFORM ERROR-MESSAGE.

           IF LN-ORGBR NOT = LN-OWNBR
              MOVE OWNBR-ORGBR-SEQ TO PASSWDW-SEQ
              MOVE "TO VOID OWNBR NOT = ORGBR" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "INVALID PASSWORD, CAN NOT VOID" TO MESS
                 PERFORM ERROR-MESSAGE.

           IF LN-CONVERCD NOT = " "
              MOVE CONVERTED-SEQ TO PASSWDW-SEQ
              MOVE "TO VOID A CONVERTED ACCOUNT" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "INVALID PASSWORD, CAN NOT VOID" TO MESS
                 PERFORM ERROR-MESSAGE
              ELSE
                 MOVE "WARNING: ACCOUNT WAS CONVERTED!" TO MESS
                 PERFORM SEND-MESS
                 IF GP-LOAN-CONVERSION-UPD-FG = "N"
                    MOVE "WARNING: CONVERSIONS DON'T UPDATE STATS OR GL"
                          TO MESS
                    PERFORM SEND-MESS
                 ELSE IF GP-LOAN-CONVERSION-UPD-FG = "G"
                    MOVE "WARNING: CONVERSIONS DON'T UPDATE STATS"
                          TO MESS
                    PERFORM SEND-MESS
                 ELSE IF GP-LOAN-CONVERSION-UPD-FG = "S"
                    MOVE "WARNING: CONVERSIONS DON'T UPDATE GL"
                          TO MESS
                    PERFORM SEND-MESS.

           IF LN-PROCASH NOT = 0
              MOVE BRANCH TO CA-BRNO
              MOVE TRANS-DATE TO CA-DATE
              MOVE LN-TILLNO TO CA-TILLNO
              PERFORM OPEN-CA1-FILE
              PERFORM READ-CA1-FILE
              PERFORM CLOSE-CA1-FILE
              IF IO-FG NOT = 0
                 MOVE "CASH DRAWER MUST BE OPENED" TO MESS
                 PERFORM ERROR-MESSAGE.

           IF GP-ESCROW-FG = "Y"
              IF (LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I")
                 MOVE LN-ACCTNO TO ES-ACCTNO
                 PERFORM OPEN-ES1-FILE
                 PERFORM READ-ES1-FILE
                 PERFORM CLOSE-ES1-FILE
                 IF IO-GOOD
                    IF ES-BALANCE NOT = 0
                       MOVE "ACCOUNT HAS ESCROW BALANCE!" TO MESS
                       PERFORM ERROR-MESSAGE.

      *************************************************
           MOVE "N0N" TO NEW-PROG-FORM.
           MOVE "P" TO ERRCD.
           IF ERRORS-EXIST = "Y"
              MOVE "CORRECT ENTRIES AND UPDATE !" TO MESS
              PERFORM ERROR-MESSAGE
              MOVE " " TO MESS
              MOVE MESS TO TMMSG-MESS
              PERFORM CALL-TMMSG
              MOVE "1" TO ROUTE-CD
              GO TO END-ROUTINE.

           MOVE "2" TO NEW-PROG-SUB.
           GO TO END-ROUTINE.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPN1" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       FORM-RESET SECTION.
       SETUP-LINK-INFO SECTION.
       VDU-CONVERT-FIELD SECTION.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       VERIFY-MESSAGE.
           MOVE "CONTRACT VERIFICATION IN PROGRESS......." TO MESS1.
           PERFORM SEND-LEGEND.
       ERROR-MESSAGE.
           MOVE " " TO MESS1.
           PERFORM SEND-LEGEND.
           PERFORM SEND-MESS.
           MOVE "Y" TO ERRORS-EXIST.
           IF EXTEND-VERIFY = "Y"
              PERFORM VERIFY-MESSAGE.
       SEND-LEGEND.
           MOVE MESS1 TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPESGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBLP/LPWKR.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPES1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
