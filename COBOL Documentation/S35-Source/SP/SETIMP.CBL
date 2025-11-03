       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SETIMP.
      **************************************************************************
      *  DESCRIPTION:    SETTLEMENT IMPORT1712
      *
      * 02/15/2022 KM - WORLD USES THIS FREQUENTLY FOR OUTSIDE COLLECTION
      *                 ASSIGNMENTS.
      *     
      *  READ INPUT FILE WITH BR#, ACCT#, AND OPTIONALLY FLASHING
      *  MESSAGE.  OPTIONALLY UPDATE FLASHING MESSAGE AND/OR POOL ID,
      *  UPDATE OR CLEAR FROZEN FLAG.  CREATE LOAN MEMOS TO DOCUMENT
      *  WHAT WAS CHANGED.
      *    INPUT FILE: ../../SP/MESSIMP.PRN (PRN IS LOWER CASE)
      *
      * REV:
      *  BLM 170106 NEW, WORLD PR# 1217
      *  BLM 170118 ADD OPTION TO UPDATE CREDIT BUREAU FLAG WITH INPUT VALUE
      *             (LN-TRW-STATUS), WORLD PR# 1217A
      *  BLM 170123 IF OPTION TO UPDATE CREDIT BUREAU FLAG IS "Y", SKIP IF
      *             LN-TRW-STATUS = "05" OR "DA" OR "64" & WRITE EXCEPTION
      *             LINE, WORLD PR# 1217A
      *  20171015 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181130 GLOBAL LMFILE
      *  BAH 181217 GLOBAL LNFILE
      *  BAH 190307 REMOVED LN-FROZEN-REASON1, USE A WORKING STORAGE FIELD
      *             INSTEAD. THAT FIELD REPLACED A COMP-3 OLD FIELD AND
      *             NEEDS TESTED FOR A X'00'
      * BAH 20191002 REMOVED ACCESS-CALL LNFILE, LMFILE
      * BAH 20211214 CLEANSE LN-MESSAGE, BAD CHARACTERS CAUSE DATABASE TO BOMB
      * BAH 2022.0331 REMOVE POOLID FROM INPUT SCREEN, ADD IT TO THE IMPORT
      *               FILE. DO NOT UPDATE IF > 1 AND < 80000 #1550
      * BAH 2023.0509 DONT ALLOW TAB CHARACTER IN THE SETTLE MEMO
      * RMZ 2023.1007 PERFORMANCE TUNING - IMPROVED SQL CALLS.
      * BAH 2023.1207 VALIDATE NUMERIC IMPORT FIELDS
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = DATA-PATH-FIL-MACHINE S35Q-57
      * RMZ 2025.0328 ADDED LN-LTOUCH-DATE TO LNFILE REWRITE. S35Q-167
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SETTLE-FILE ASSIGN TO SETTLE-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  SETTLE-FILE
           LABEL RECORDS ARE STANDARD.
       01  SETTLE-REC.
           03  SETTLE-BRNO      PIC 9(4).
           03  SETTLE-ACCT      PIC 9(6).
           03  SETTLE-POOL      PIC 9(5).
           03  SETTLE-MESSAGE   PIC X(30).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/SETIMP S35 02/28/24-13:53:00 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  SETTLE-PATH.
           03  SETTLE-PATH-GLOBAL PIC X(13).
           03  SETTLE-PATH-FILE.
               05  FILLER           PIC X(11) VALUE
               "SP/MESSIMP.".
               05  SETTLE-PATH-SUF  PIC X(6)  VALUE "PRN".
       01  DATED-SETTLE-PATH.
           03  SETTLE-SAVE-FILE PIC X(24).
           03  SETTLE-PATH-DATE PIC 9(6).

       01  DATA-PATH.
           03  FILLER                          PIC X(9).
           03  DATA-PATH-FIL-MACHINE           PIC X(02).
           03  FILLER                          PIC X(23).

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  UPD-MSG          PIC X.
           03  UPD-POOL         PIC X.
      * LEAVE THIS HERE SO THEY DONT HAVE TO REENTER PARAMETERS
           03  DLPOOL           PIC 9(5).
           03  UPD-FROZEN-FG    PIC X.
           03  FROZEN-REASON    PIC XX.
           03  UPD-CREDIT-BUR   PIC X.
           03  TRW-STATUS       PIC XX.
           03  LEGAL            PIC X.
           03  BNKRPT           PIC X.
           03  DECEASED         PIC X.
           03  FRAUD            PIC X.
           03  PL               PIC X.
       01  VERSION-CONTROL      PIC X VALUE "A".

       01  TEST-FROZEN-REASON.
           03  TEST-FROZEN-REASON1 PIC X.
           03  FILLER              PIC X.

       01  HOLD-LN-MESSAGE      PIC X(30).
       01  HOLD-LN-MESSAGEX REDEFINES HOLD-LN-MESSAGE.
           03  HOLD-LN-MESS-BYTE PIC X OCCURS 30.
       01  HOLD-BYTE            PIC X.
           88  VALID-SPECIAL-CHARS  VALUE "(" ")"
                              "/" "\" "-" "?" "+" "%" "#" "[" "]"
                              "!" "<" ">" "$" "*" "&" "." """"  "="
                              "'" "_" ":" "@" "," ";" "~" "|" "{" "}".
           88  TAB-CHARACTER  VALUE X"09".
       01  TODAYS-DATE          PIC 9(8).
       01  HOLD-MEMO            PIC X(60).
       01  HOLD-LM-SEQ          PIC 999.
       01  HOLD-OLD-MSG-MEMO.
           03  FILLER           PIC X(30) VALUE
           "SP/SETIMP  OLD FLASHING MSG:  ".
           03  SAVE-MESSAGE     PIC X(30).
       01  HOLD-NEW-MSG-MEMO.
           03  FILLER           PIC X(30) VALUE
           "SP/SETIMP  NEW FLASHING MSG:  ".
           03  NEW-MESSAGE      PIC X(30).
       01  HOLD-FROZEN-MEMO.
           03  FILLER           PIC X(36) VALUE
           "SP/SETIMP: CHANGED FROZEN FLAG FROM ".
           03  SAVE-ACCT-FROZEN PIC X.
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-ACCT-FROZEN  PIC X.
       01  HOLD-FROZEN-REASON-MEMO.
           03  FILLER           PIC X(38) VALUE
           "SP/SETIMP: CHANGED FROZEN REASON FROM ".
           03  SAVE-FROZEN-REASON PIC XX.
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-FROZEN-REASON PIC XX.
       01  HOLD-POOL-MEMO.
           03  FILLER           PIC X(34) VALUE
           "SP/SETIMP: CHANGED LOAN POOL FROM ".
           03  SAVE-DLPOOL      PIC 9(5).
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-DLPOOL       PIC 9(5).
       01  HOLD-CREDIT-BUREAU-MEMO.
           03  FILLER           PIC X(45) VALUE
           "SP/SETIMP: CHANGED CREDIT BUREAU STATUS FROM ".
           03  SAVE-TRW-STATUS  PIC XX.
           03  FILLER           PIC X(4) VALUE " TO ".
           03  NEW-TRW-STATUS   PIC XX.
 
       01  BR-TOTAL-READ        PIC 9(10)       VALUE 0.
       01  BR-TOTAL-PROC        PIC 9(10)       VALUE 0.
       01  GR-TOTAL-READ        PIC 9(10)       VALUE 0.
       01  GR-TOTAL-PROC        PIC 9(10)       VALUE 0.
 
       01  SUB                  PIC S9(6) COMP-3.
       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.

       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(2)  VALUE SPACES.
           03  FILLER               PIC X(25) VALUE
               "SETTLEMENT IMPORT LISTING".
       01  HEAD3.
           03  FILLER               PIC X(23) VALUE
           "BR#   ACCT#   EXCEPTION".
 
       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-EXCEPTION      PIC X(40).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-TAG      PIC X(19).
           03  D-TOTAL-READ     PIC Z(8)9.
           03  D-TOTAL-PMT1     PIC X(5).
           03  D-TOTAL-PROC     PIC Z(8)9.
           03  D-TOTAL-PMT2     PIC X(10).

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(25).
           03  RANGE-SELECTION  PIC X(75).
           03  FILLER REDEFINES RANGE-SELECTION.
               05  RANGE-SELECTION-N PIC 9(5).
               05  FILLER       PIC X(70).

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY". 
       COPY "LIBGB/DACCESSW.CPY". 
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SETIMP_DEF.CPY".
       COPY "LIBSP/SETIMP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/SETIMP_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               SETTLE-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE
               SETTLE-FILE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SETIMP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM PROCESS-SETTLEMENT-FILE.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           MOVE SETTLE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
              MOVE SETTLE-PATH TO DATED-SETTLE-PATH
              MOVE DATE-YYMMDD TO SETTLE-PATH-DATE
              MOVE SPACES TO SYSTEM-BUF
              STRING MV-CMD," ",SETTLE-PATH," ", DATED-SETTLE-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

           GO TO END-PROGRAM.
 

      ***********************************************
       PROCESS-SETTLEMENT-FILE SECTION.
      ***********************************************
           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE "PROCESSING SETTLEMENT UPLOAD" TO MESS
              PERFORM SEND-LEGEND.

           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-SETTLE-FILE.
       NEXT-RECORD.
           PERFORM READ-SETTLE-FILE.
           IF IO-FG NOT = 0
              PERFORM PRINT-BR-TOTALS
              GO TO EXIT-PROCESS-SETTLEMENT-FILE.

           IF SETTLE-BRNO NOT NUMERIC
              MOVE "BRANCH NOT NUMERIC" TO D-EXCEPTION
              PERFORM EXCEPT-LINE
              MOVE 0 TO HOLD-BRNO
              ADD 1 TO GR-TOTAL-READ
              GO TO NEXT-RECORD.

           IF SETTLE-ACCT NOT NUMERIC
              MOVE "ACCT NOT NUMERIC" TO D-EXCEPTION
              PERFORM EXCEPT-LINE
              ADD 1 TO GR-TOTAL-READ
              GO TO NEXT-RECORD.

           IF SETTLE-POOL NOT NUMERIC
              MOVE "POOL NOT NUMERIC" TO D-EXCEPTION
              PERFORM EXCEPT-LINE
              ADD 1 TO GR-TOTAL-READ
              GO TO NEXT-RECORD.

           IF SETTLE-BRNO NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-LN1-FILE
                 PERFORM CLOSE-LM1-FILE
                 PERFORM PRINT-BR-TOTALS
              END-IF
              PERFORM GET-BR-SET-PATHS
              IF IO-FG = 9
                 MOVE "MISSING BRANCH" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 MOVE 0 TO HOLD-BRNO
                 ADD 1 TO GR-TOTAL-READ
                 GO TO NEXT-RECORD
              ELSE
                 IF IO-FG = 8
                    MOVE 0 TO HOLD-BRNO
                    ADD 1 TO GR-TOTAL-READ
                    GO TO NEXT-RECORD.

           ADD 1 TO BR-TOTAL-READ
                    GR-TOTAL-READ.

           MOVE SETTLE-ACCT TO LN-ACCTNO.
           PERFORM READ-LN1-FILE. *> EMBEDDED - LNFILE
           IF IO-FG NOT = 0
              MOVE "INVALID ACCOUNT NUMBER" TO D-EXCEPTION
              PERFORM EXCEPT-LINE
              GO TO NEXT-RECORD.

           IF LN-POCD-NONLOAN
              MOVE "ACCOUNT VOIDED" TO D-EXCEPTION
              PERFORM EXCEPT-LINE
              GO TO NEXT-RECORD.
 
      * SKIP PAID-OUTS
           IF LN-POFFDATE NOT = 0
              MOVE "ACCOUNT PAID OFF" TO D-EXCEPTION
              PERFORM EXCEPT-LINE
              GO TO NEXT-RECORD.
 
           IF LEGAL NOT = "Y"
              IF LN-LEGALDATE NOT = 0 OR LN-ACTIONCD(1:1) = "L"
                 MOVE "ACCOUNT IN LEGAL STATUS" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF BNKRPT NOT = "Y"
              IF LN-BNKRPTDATE NOT = 0
                 MOVE "ACCOUNT IN BANKRUPT STATUS" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF DECEASED NOT = "Y"
              IF LN-ACTIONCD = "DCSD"
                 MOVE "ACCOUNT IN DECEASED STATUS" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF FRAUD NOT = "Y"
              IF LN-ACTIONCD = "FRUD"
                 MOVE "ACCOUNT IN FRAUD STATUS" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF PL = "O"
              IF LN-PLDATE = 0
                 MOVE "ACCOUNT NOT CHARGED OFF" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 MOVE "ACCOUNT CHARGED OFF" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF UPD-CREDIT-BUR = "Y"
              IF (LN-TRW-STATUS = "05" OR "DA" OR "64")
                 MOVE "ACCOUNT HAS CREDIT BUREAU STATS OF &&"
                                                 TO D-EXCEPTION
                 INSPECT D-EXCEPTION REPLACING FIRST "&&"
                                                 BY LN-TRW-STATUS
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

           IF UPD-POOL = "Y"
              IF SETTLE-POOL > 1 AND < 80000
                 MOVE "POOLID IS < 80000; &&&&&" TO D-EXCEPTION
                 INSPECT D-EXCEPTION REPLACING FIRST "&&&&&"
                                                 BY SETTLE-POOL
                 PERFORM EXCEPT-LINE
                 GO TO NEXT-RECORD.

       UPDATE-FROZEN-FG.
           IF UPD-FROZEN-FG = "N"
              GO TO UPDATE-POOL-ID.

      * LN-FROZEN-REASON REPLACED A COMP-3 FIELD IN THE LOAN REC. CHECK FOR
      * LEFT OVER 0'S
           MOVE LN-FROZEN-REASON TO TEST-FROZEN-REASON.
           IF TEST-FROZEN-REASON1 = X"00"
              MOVE SPACES TO LN-FROZEN-REASON.
           MOVE LN-ACCT-FROZEN   TO SAVE-ACCT-FROZEN.
           MOVE LN-FROZEN-REASON TO SAVE-FROZEN-REASON.
           IF UPD-FROZEN-FG = "Y"
              MOVE "Y" TO LN-ACCT-FROZEN
              MOVE FROZEN-REASON TO LN-FROZEN-REASON.
           IF UPD-FROZEN-FG = "C"
              MOVE "N" TO LN-ACCT-FROZEN
              MOVE SPACES TO LN-FROZEN-REASON.

       UPDATE-POOL-ID.
           IF UPD-POOL = "N"
              GO TO UPDATE-FLASHING-MSG.

           MOVE LN-DLPOOL   TO SAVE-DLPOOL.
           MOVE SETTLE-POOL TO LN-DLPOOL.

       UPDATE-FLASHING-MSG.
           IF UPD-MSG = "N"
              GO TO UPDATE-CREDIT-BUREAU.

           MOVE LN-MESSAGE TO SAVE-MESSAGE.
      * CAN HAVE BAD DATA, ERRORS OUT IN SQL DATABASE
      *    MOVE SETTLE-MESSAGE TO LN-MESSAGE.
           MOVE SETTLE-MESSAGE TO HOLD-LN-MESSAGE.
           PERFORM TEST-LN-MESSAGE VARYING SUB FROM 1 BY 1 UNTIL
                    SUB > 30.
           MOVE HOLD-LN-MESSAGE TO LN-MESSAGE.

       UPDATE-CREDIT-BUREAU.
           IF UPD-CREDIT-BUR = "N"
              GO TO UPDATE-LOAN.

           MOVE LN-TRW-STATUS TO SAVE-TRW-STATUS.
           MOVE TRW-STATUS TO LN-TRW-STATUS.

       UPDATE-LOAN.
           PERFORM REWRITE-LN1-FILE. *> EMBEDDED - LNFILE
           IF UPD-MSG = "Y"
              PERFORM CREATE-MSG-MEMO.

           IF UPD-FROZEN-FG = "Y" OR "C"
              PERFORM CREATE-FROZEN-MEMO.

           IF UPD-POOL = "Y"
              PERFORM CREATE-POOL-MEMO.

           IF UPD-CREDIT-BUR = "Y"
              PERFORM CREATE-CREDIT-BUREAU-MEMO.

           ADD 1 TO BR-TOTAL-PROC
                    GR-TOTAL-PROC.

           GO TO NEXT-RECORD.

       EXIT-PROCESS-SETTLEMENT-FILE.
           EXIT.

      ***********************************
       TEST-LN-MESSAGE SECTION.
      ***********************************
           IF HOLD-LN-MESS-BYTE(SUB) NOT = SPACES
              IF HOLD-LN-MESS-BYTE(SUB) NOT ALPHABETIC
                 IF HOLD-LN-MESS-BYTE(SUB) NOT NUMERIC
                    MOVE HOLD-LN-MESS-BYTE(SUB) TO HOLD-BYTE
                    IF NOT VALID-SPECIAL-CHARS
                       IF TAB-CHARACTER
                       MOVE SPACES TO HOLD-LN-MESS-BYTE(SUB).

      ***********************************
       CREATE-MSG-MEMO SECTION.
      ***********************************
           IF SAVE-MESSAGE NOT = SPACES
              IF LN-MESSAGE NOT = SAVE-MESSAGE
                 MOVE HOLD-OLD-MSG-MEMO TO HOLD-MEMO
                 PERFORM CREATE-MEMO.
           MOVE LN-MESSAGE TO NEW-MESSAGE
           MOVE HOLD-NEW-MSG-MEMO TO HOLD-MEMO.
           PERFORM CREATE-MEMO.

      ***********************************
       CREATE-FROZEN-MEMO SECTION.
      ***********************************
           IF LN-ACCT-FROZEN NOT = SAVE-ACCT-FROZEN
              MOVE LN-ACCT-FROZEN TO NEW-ACCT-FROZEN
              MOVE HOLD-FROZEN-MEMO TO HOLD-MEMO
              PERFORM CREATE-MEMO.
           IF LN-FROZEN-REASON NOT = SAVE-FROZEN-REASON
              MOVE LN-FROZEN-REASON TO NEW-FROZEN-REASON
              MOVE HOLD-FROZEN-REASON-MEMO TO HOLD-MEMO
              PERFORM CREATE-MEMO.

      ***********************************
       CREATE-POOL-MEMO SECTION.
      ***********************************
           IF LN-DLPOOL NOT = SAVE-DLPOOL
              MOVE LN-DLPOOL TO NEW-DLPOOL
              MOVE HOLD-POOL-MEMO TO HOLD-MEMO
              PERFORM CREATE-MEMO.

      ***********************************
       CREATE-CREDIT-BUREAU-MEMO SECTION.
      ***********************************
           IF LN-TRW-STATUS NOT = SAVE-TRW-STATUS
              MOVE LN-TRW-STATUS TO NEW-TRW-STATUS
              MOVE HOLD-CREDIT-BUREAU-MEMO TO HOLD-MEMO
              PERFORM CREATE-MEMO.

      ***********************************
       CREATE-MEMO SECTION.
      ***********************************

           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO.
           SUBTRACT TODAYS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE "S"           TO LM-MEMO-TYPE.
           MOVE 1             TO HOLD-LM-SEQ.
       WRITE-MEMO.
           MOVE HOLD-MEMO   TO LM-MEMO. 
           MOVE HOLD-LM-SEQ TO LM-SEQ.
           PERFORM WRITE-LM1-FILE. *> EMBEDDED - LMFILE
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO HOLD-LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE SETIMP-QUEUE-POS TO Q-POS.
           MOVE SETIMP-COPIES-POS TO C-POS.
           MOVE SETIMP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO BR-TOTAL-READ GR-TOTAL-READ
                     BR-TOTAL-PROC GR-TOTAL-PROC.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "FIL"           TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF      TO DATA-PATH.

           MOVE EXT-FILPATH-GLOBAL TO SETTLE-PATH-GLOBAL.
           CALL "C$TOLOWER" USING SETTLE-PATH-SUF, VALUE 3.
           MOVE SETTLE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              IF EXT-ROUTE-BUF = "00"
                 MOVE "INPUT FILE NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENDIT
              ELSE
                 PERFORM OPEN-PR-FILE
                 MOVE "INPUT FILE NOT FOUND" TO D-EXCEPTION
                 PERFORM EXCEPT-LINE
                 GO TO ENDIT.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U" GO TO PL-YN.

       UPD-MSG-YN.
           MOVE "E  A010UPDMSG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UPD-MSG-YN.
           MOVE VDUBUF TO UPD-MSG.
           IF UPD-MSG NOT = "Y" AND NOT = "N"
              GO TO UPD-MSG-YN.
       UPD-POOL-YN.
           MOVE "E  A010UPDPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO UPD-MSG-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UPD-POOL-YN.
           MOVE VDUBUF TO UPD-POOL.
           IF UPD-POOL NOT = "Y" AND NOT = "N"
              GO TO UPD-POOL-YN.
       UPD-FROZEN-FG-YN.
           MOVE "E  A010UPDFROZEN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO UPD-POOL-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UPD-FROZEN-FG-YN.
           MOVE VDUBUF TO UPD-FROZEN-FG.
           IF UPD-FROZEN-FG NOT = "Y" AND NOT = "N" AND NOT = "C"
              GO TO UPD-FROZEN-FG-YN.
           IF UPD-FROZEN-FG NOT = "Y"
              GO TO UPD-CREDIT-BUR-YN.
       ENTER-FROZEN-REASON.
           MOVE "E  A020REASON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-FROZEN-REASON.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO UPD-FROZEN-FG-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FROZEN-REASON.
           MOVE VDUBUF TO FROZEN-REASON.
           IF FROZEN-REASON NOT = SPACES
              MOVE FROZEN-REASON TO CD-CODE
              PERFORM GET-REASON-DESC
              IF IO-BAD
                 GO TO ENTER-FROZEN-REASON
              ELSE
                 MOVE "S  A020REASONDESC." TO VDU
                 MOVE CD-DESC TO VDUBUF
                 PERFORM SCREENIO.
       UPD-CREDIT-BUR-YN.
           MOVE "E  A010UPDCB." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF UPD-FROZEN-FG = "N"
                 GO TO UPD-FROZEN-FG-YN
              ELSE
                 GO TO ENTER-FROZEN-REASON.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UPD-CREDIT-BUR-YN.
           MOVE VDUBUF TO UPD-CREDIT-BUR.
           IF UPD-CREDIT-BUR NOT = "Y" AND NOT = "N"
              GO TO UPD-CREDIT-BUR-YN.
       ENTER-CB-STATUS.
           IF UPD-CREDIT-BUR = "N"
              GO TO CHECK-FLAGS.
           MOVE "E  A020CBSTAT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO UPD-CREDIT-BUR-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-CB-STATUS.
           MOVE VDUBUF TO TRW-STATUS.
           IF TRW-STATUS = "0 " OR " 0"
              MOVE "USE SPACES OR 00 TO CLEAR STATUS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CB-STATUS.
           IF TRW-STATUS = "04" OR "21" OR "66" OR "67" OR "68"
                                OR "69" OR "70" OR "85" OR "86"
                                OR "87" OR "98" OR "42"
              MOVE "NOT VALID WITH METRO2; TRY A SPECIAL CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CB-STATUS.

       CHECK-FLAGS.
           IF UPD-MSG = "N"
              IF UPD-POOL = "N"
                 IF UPD-FROZEN-FG = "N"
                    MOVE "NOTHING SELECTED TO UPDATE!" TO MESS
                    PERFORM SEND-MESS
                    GO TO UPD-MSG-YN.
       LEGAL-YN.
           MOVE "E  A010LEGAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" 
              IF UPD-CREDIT-BUR = "N"
                 GO TO UPD-CREDIT-BUR-YN
              ELSE
                 GO TO ENTER-CB-STATUS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO LEGAL-YN.
           MOVE VDUBUF TO LEGAL.
           IF LEGAL NOT = "Y" AND NOT = "N"
              GO TO LEGAL-YN.
       BNKRPT-YN.
           MOVE "E  A010BNKRPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGAL-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BNKRPT-YN.
           MOVE VDUBUF TO BNKRPT.
           IF BNKRPT NOT = "Y" AND NOT = "N"
              GO TO BNKRPT-YN.
       DECEASED-YN.
           MOVE "E  A010DECEASED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BNKRPT-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO DECEASED-YN.
           MOVE VDUBUF TO DECEASED.
           IF DECEASED NOT = "Y" AND NOT = "N"
              GO TO DECEASED-YN.
       FRAUD-YN.
           MOVE "E  A010FRAUD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DECEASED-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FRAUD-YN.
           MOVE VDUBUF TO FRAUD.
           IF FRAUD NOT = "Y" AND NOT = "N"
              GO TO FRAUD-YN.
       PL-YN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FRAUD-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO PL-YN.
           MOVE VDUBUF TO PL.
           IF PL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO PL-YN.
       EXIT-PARM.
           EXIT.

      ****************************************
       CODE-SCAN SECTION.
      ****************************************
           MOVE " "  TO CDSCAN-CODE.
           MOVE "FZ" TO CDSCAN-TYPE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020REASON." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.

           MOVE CDSCAN-CODE TO VDUBUF.
       EXIT-CODE-SCAN.
           EXIT.

      ***********************************
       GET-REASON-DESC SECTION.
      ***********************************
           MOVE "FZ" TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE. *> EMBEDDED - CDFILE
           PERFORM CLOSE-CD1-FILE.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
       
           MOVE "S  A010UPDMSG." TO VDU.
           MOVE UPD-MSG          TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010UPDPOOL." TO VDU.
           MOVE UPD-POOL          TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010UPDFROZEN." TO VDU.
           MOVE UPD-FROZEN-FG       TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A020REASON." TO VDU.
           MOVE FROZEN-REASON    TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010UPDCB." TO VDU.
           MOVE UPD-CREDIT-BUR   TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A020CBSTAT." TO VDU.
           MOVE TRW-STATUS       TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010LEGAL." TO VDU.
           MOVE LEGAL        TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010BNKRPT." TO VDU.
           MOVE BNKRPT           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010DECEASED." TO VDU.
           MOVE DECEASED           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010FRAUD." TO VDU.
           MOVE FRAUD           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010PL." TO VDU.
           MOVE PL           TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           EXIT.

      *************************************
       EXCEPT-LINE SECTION.
      *************************************
           MOVE SETTLE-BRNO TO D-BRNO.
           MOVE SETTLE-ACCT TO D-NUMBER.
           PERFORM WRITE-DETAIL-LINE.

      *************************************
       PRINT-RANGE-LINES SECTION.
      *************************************
           MOVE "UPDATE FLASHING MESSAGE:" TO RANGE-LINE.
           MOVE UPD-MSG                    TO RANGE-SELECTION.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "UPDATE POOL            :" TO RANGE-LINE.
           MOVE UPD-POOL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "UPDATE FROZEN FLAG     :" TO RANGE-LINE.
           MOVE UPD-FROZEN-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           IF UPD-FROZEN-FG = "Y"
              MOVE "FROZEN REASON          :" TO RANGE-LINE
              MOVE FROZEN-REASON              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "UPDATE CRED BUR STATUS :" TO RANGE-LINE.
           MOVE UPD-CREDIT-BUR             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           IF UPD-CREDIT-BUR = "Y"
              MOVE "CREDIT BUREAU STATUS   :" TO RANGE-LINE
              MOVE TRW-STATUS                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE LEGAL ACCOUNTS :" TO RANGE-LINE.
           MOVE LEGAL                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE BANKRUPT ACCTS :" TO RANGE-LINE.
           MOVE BNKRPT                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE DECEASED ACCTS :" TO RANGE-LINE.
           MOVE DECEASED                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE FRAUD ACCOUNTS :" TO RANGE-LINE.
           MOVE FRAUD                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE P&L ACCOUNTS   :" TO RANGE-LINE.
           MOVE PL                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
      *************************************
       PRINT-BR-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "BRANCH &&&& TOTAL: " TO D-TOTAL-TAG.
           INSPECT D-TOTAL-TAG REPLACING FIRST "&&&&" BY BR-NO.
           MOVE BR-TOTAL-READ         TO D-TOTAL-READ.
           MOVE " READ"       TO D-TOTAL-PMT1.
           MOVE BR-TOTAL-PROC         TO D-TOTAL-PROC.
           MOVE " PROCESSED"  TO D-TOTAL-PMT2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE 0 TO BR-TOTAL-READ BR-TOTAL-PROC.

      *************************************
       PRINT-GRAND-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTALS     : " TO D-TOTAL-TAG.
           MOVE GR-TOTAL-READ TO D-TOTAL-READ.
           MOVE " READ"       TO D-TOTAL-PMT1.
           MOVE GR-TOTAL-PROC TO D-TOTAL-PROC.
           MOVE " PROCESSED"  TO D-TOTAL-PMT2.
           PERFORM WRITE-DETAIL-LINE.

      **************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************
       HEAD-PAGE SECTION.
      **************************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 4 TO LN-COUNT.

      **************************************
       GET-BR-SET-PATHS SECTION.
      **************************************
           MOVE SETTLE-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
      *       IF BR-MACHINE NOT = DATA-PATH-FIL-MACHINE
      *          MOVE 8 TO IO-FG
      *          ADD 1 TO GR-TOTAL-OTHER
      *          GO TO EXIT-GET-BR-SET-PATHS
      *       END-IF
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B" TO LN-PATH-OVERRIDE
                          LM-PATH-OVERRIDE
              PERFORM OPEN-LN1-FILE
              MOVE SETTLE-BRNO TO HOLD-BRNO
              PERFORM OPEN-LM1-FILE.

       EXIT-GET-BR-SET-PATHS.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO SETIMP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SETIMP-SYSDATE.
           DISPLAY SETIMP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SETIMP-FORM-FIELDS TO WR-BUF.
           MOVE SETIMP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE SETIMP-HELP--FILE TO HELP-LINK-FILE.
           MOVE SETIMP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SETIMP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
           
      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
 
      *-----------------------------------------------------------------

       OPEN-SETTLE-FILE.
           PERFORM OPEN-IT.
           MOVE SETTLE-PATH TO E-FILE.
           OPEN INPUT SETTLE-FILE.
           IF IO-FG = 8
              GO TO OPEN-SETTLE-FILE.
           IF IO-FG = 7
              CLOSE SETTLE-FILE
              GO TO OPEN-SETTLE-FILE.
       READ-SETTLE-FILE.
           PERFORM READ-IT.
           MOVE SETTLE-PATH TO E-FILE.
           READ SETTLE-FILE.
           IF IO-FG = 8
              GO TO READ-SETTLE-FILE.
       CLOSE-SETTLE-FILE.
           PERFORM CLOSE-IT.
           CLOSE SETTLE-FILE.

      * COPY "LIBLP/LPLN1IN.CPY".
      * COPYMEMBER: LIBLP/LPLN1IN
      ************************************************************
      *                (LPLN1IN)
      * REV:
      *  JTG 081497 CHANGED LN-SALEFIN TO LN-GIFTNET WORLD A90
      *  BLV 020698 ADDED LN-SALEFIN BACK TO BICREA CALL
      *  MJD 030398 ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  BAH 121023 ADDED FILPATH
      *  BAH 04/22/2016 ADDED LN-SMALLOAN TO BICREA CALL, REGENCY PR#5030
      *  BAH 20180316 ADDED LN-PLDATE TO BICREA CALL, WORLD #1308
      *  BAH 181130 GLOBAL LNFILE, L2G
      ************************************************************
       UPDATE-BI-FILE.
           IF IO-FG = 0
              MOVE "CL/BICREA" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   STAT
                                   LN-ACCTNO
                                   LN-BWTABLE
                                   LN-OWNBR
                                   LN-ORGST
                                   LN-SALEFIN
                                   LN-GIFTNET
                                   LN-ENTDATE
                                   LN-POFFDATE
                                   LN-PLCD
                                   LN-CONVERCD
                                   "000000000"
                                   LN-SMALLOAN
                                   LN-PLDATE
             CANCEL FORM-PROGX.
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LN1-FILE. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_POCD, 
                   CAST(LNFILE.LN_PLDATE     AS VARCHAR(10)), 
                   CAST(LNFILE.LN_POFFDATE   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_LEGALDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)), 
                   LNFILE.LN_DLPOOL,
                   LNFILE.LN_FROZEN_REASON, 
                   LNFILE.LN_ACTIONCD, 
                   LNFILE.LN_MESSAGE, 
                   LNFILE.LN_TRW_STATUS, 
                   LNFILE.LN_ACCT_FROZEN
              INTO  
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-POCD, 
                  :QLN-PLDATE,       
                  :QLN-POFFDATE,     
                  :QLN-LEGALDATE,    
                  :QLN-BNKRPTDATE,   
                  :QLN-DLPOOL,
                  :QLN-FROZEN-REASON, 
                  :QLN-ACTIONCD, 
                  :QLN-MESSAGE, 
                  :QLN-TRW-STATUS, 
                  :QLN-ACCT-FROZEN
              FROM DBO.LNFILE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-LN1-FILE. *> EMBEDDED - LNFILE

           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           PERFORM SET-LN-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
             SET
              LNFILE.LN_POCD          = :QLN-POCD, 
              LNFILE.LN_PLDATE        = :QLN-PLDATE,       
              LNFILE.LN_POFFDATE      = :QLN-POFFDATE,     
              LNFILE.LN_LEGALDATE     = :QLN-LEGALDATE,    
              LNFILE.LN_BNKRPTDATE    = :QLN-BNKRPTDATE,   
              LNFILE.LN_DLPOOL        = :QLN-DLPOOL,
              LNFILE.LN_FROZEN_REASON = :QLN-FROZEN-REASON, 
              LNFILE.LN_ACTIONCD      = :QLN-ACTIONCD, 
              LNFILE.LN_MESSAGE       = :QLN-MESSAGE, 
              LNFILE.LN_TRW_STATUS    = :QLN-TRW-STATUS, 
              LNFILE.LN_ACCT_FROZEN   = :QLN-ACCT-FROZEN,
              LNFILE.LN_LTOUCH_DATE   = :QLN-LTOUCH-DATE
             WHERE LNFILE.LN_OWNBR    = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO   = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LN1-FILE. *> EMBEDDED - LNFILE

           IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION
              IF LN-BI-FG = "Y"
                 MOVE "R" TO STAT
                 PERFORM UPDATE-BI-FILE.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.
           PERFORM CLOSE-IT.


      * COPY "LIBLP/LPLM1IN.CPY".
      * COPYMEMBER: LIBLP/LPLM1IN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       WRITE-LM1-FILE. *> EMBEDDED - LMFILE

           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           ACCEPT LM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.LMFILE
             (LMFILE.LM_BRNO,
              LMFILE.LM_ACCTNO,
              LMFILE.LM_DATE_REVERSED,
              LMFILE.LM_SEQ,
              LMFILE.LM_LTOUCH_DATE,
              LMFILE.LM_LTOUCH_TIME_HHMM,
              LMFILE.LM_LTOUCH_USERID,
              LMFILE.LM_MEMO,
              LMFILE.LM_MEMO_TYPE)
            VALUES
             (:QLM-BRNO,
              :QLM-ACCTNO,
              :QLM-DATE-REVERSED,
              :QLM-SEQ,
              :QLM-LTOUCH-DATE,
              :QLM-LTOUCH-TIME-HHMM,
              :QLM-LTOUCH-USERID,
              :QLM-MEMO,
              :QLM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.


      * COPY "LIBLP/LPCD1RN.CPY".
      * COPYMEMBER: LIBLP/LPCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.

      *-----------------------------------------------------------------
       OPEN-CD1-FILE.
           PERFORM LOAD-CD1-FILE.
           PERFORM OPEN-IT.
           MOVE CD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-CD1-FILE. *> EMBEDDED - CDFILE
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           MOVE CD-TYPE    TO QCD-TYPE.
           MOVE CD-CODE    TO QCD-CODE.

           EXEC SQL
            SELECT CDFILE.CD_TYPE, 
                   CDFILE.CD_CODE, 
                   CDFILE.CD_DESC 
              INTO  
                  :QCD-TYPE, 
                  :QCD-CODE, 
                  :QCD-DESC                  
              FROM DBO.CDFILE
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           PERFORM CLOSE-IT.

 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SETIMP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
