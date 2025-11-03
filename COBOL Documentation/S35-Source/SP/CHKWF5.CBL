       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHKWF5.
       DATE-WRITTEN. 06-29-2023.
      *****************************************************************
      *
      *  DESCRIPTION   :  CREATE DAILY CHECK RECONCILIATION FILE FOR
      *                   TRANSMISSION TO WELLS FARGO BANK AND JOURNAL
      *                   OF CHECKS EXTRACTED
      *                   TOOK FROM SP/CHKWF4, EXACT SAME LAYOUT
      *                   EXCEPT THIS USES GP-BANK-ACCTNO AND CHKWF4
      *                   HAS HARD CODED OLD ACCOUNT NO.
      *************************
      * ** NOTE **: THIS IS NOW THE NEW CHECK REC USING GP-BANK-ACCTNO
      *             CHKWF4 IS OLD CHECK REC WITH BANK ACCOUNT # 8018016520
      *    BRANCH WILL BE SKIPPED IN THIS PROGRAM IF
      *                  BR-USE-NEW-WFARGO-ACCT NOT = "Y"
      *************************
      *
      * SOMEONE/OR GROUP IS DUPLICATING OUR CHECKS. CASHING THEM! BANKS
      *         CANT TELL THEY ARE FRADULENT. PEOPLE CAN'T CASH AT WELLS
      *         FARGO NOW, THEY MUST DEPOSIT. 70-80% CUSTOMERS HAVE BANK
      *         ACCOUNTS. IF THEY GO TO GAS STATION, MAY GET RETURNED.
      * BUILDING A CHECK REGISTER WITH WELLS FARGO, IF CHECK TO GET CASHED
      * DOESNT MATCH EVERYTHING, IT GETS FLAGGED AND THEY HAVE TO VALIDATE
      * THAT THEY CAN PAY THE CHECK.
      *
      *
      *  OUTPUT FILE IN: /USR/EXTRACT/ACT/POSPAY/CHECKS.R?.YYMMDD.HHMM
      *  LOG FILE    IN: /USR/EXTRACT/ACT/POSPAY/LOG/LOG.NEWPOSPAY.R?.YYMMDD
      *
      * REV:
      *  BAH 2021/01/11 NEW FOR WORLD, WANT TO RUN EVERY 20 MINUTES
      *                 DONT WANT RCFILE INVOLVED AT ALL. #1486
      *  BAH 2021/01/22 CREATE NEW LOG FILE, FIX NO RECRODS FOUND TO NOT
      *                 RESET ON EACH BRANCH, IF LAST BRANCH HAD NO RECS
      *                 THE FILE WAS REMOVED, AND CHANGED FILENAME TO BE
      *                 .HHMM INSTEAD OF A SEQ. #1486A
      *  BAH 2021/03/05 CHANGED TC-FUTURE-FG(1) TO BE TC-EXTRACT-FG, MADE
      *                 IT ITS OWN FIELD, NEED TO DROP THE TRCFILE
      *  BAH 2021/02/09 CHANGE BANK ID HEADER TO BE 165, NOT 182. #1486B
      *  BAH 2021.0318 SKIP TC-TYPE "DB" DEBIT CARD UMC #1490
      *  BAH 2023.0629 COPIED FROM CHKWF4 WHICH NOW HAS HARD CODED BANK
      *                ACCOUNT # AND THIS PROGRAM WILL USE GP-BANK-ACCOUNT
      *                SKIP BRANCHES BR-USE-NEW-WFARGO-ACCT NOT = "Y"  #1616
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK-KEY
                  FILE STATUS FILE-STAT.
           SELECT LOG-FILE ASSIGN LOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *****************************************************************
      *    CHECK EXPORT FILE
      *****************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-HEADER-REC.
           03  WK-STAR-3              PIC X(3).
           03  WK-BANK-ID             PIC 9(5).
           03  WK-BANK-ACCTNO         PIC 9(15).
           03  WK-FILE-STAT           PIC 9.
           03  FILLER                 PIC X(141).
           03  WK-LF                  PIC X.
       01  WK-REC.
           03  WK-CHECK-BRNO          PIC 9(4).
           03  WK-CHECK-NO            PIC 9(6).
           03  WK-CHECK-DATE          PIC 9(6).
           03  WK-ACCT-NUMBER         PIC 9(15).
           03  WK-TRANS-TYPE          PIC 9(3).
           03  WK-CHECK-AMOUNT        PIC 9(8)V99.
           03  WK-PAYEE               PIC X(120).
           03  FILLER                 PIC XX.
       01  WK-TRAILER-REC.
           03  WK-AMPERSAND           PIC X.
           03  FILLER                 PIC X(14).
           03  WK-RECORD-COUNT        PIC 9(7).
           03  FILLER                 PIC X(3).
           03  WK-TOTAL-AMOUNT        PIC 9(11)V99.
           03  FILLER                 PIC X(128).

      **************************************************
      *  LOG FILE TO SHOW FILES CREATED
      **************************************************
       FD  LOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  LOG-REC.
           03  LOG-TIME          PIC X(8).
           03  FILLER            PIC X.
           03  LOG-FILENAME      PIC X(45).
           03  FILLER            PIC X.
           03  LOG-RECORD-COUNT  PIC 9(7).

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CHKWF5.CBL S35 06/29/23-14:17:02 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       01  WK-PATH.
           03  FILLER           PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER           PIC X(14) VALUE "ACT/NEWPOSPAY/".
           03  WK-PATH-NAME     PIC X(7)  VALUE "CHECKS.".
           03  WK-PATH-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE ".".
           03  WK-PATH-YR       PIC 99.
           03  WK-PATH-MO       PIC 99.
           03  WK-PATH-DA       PIC 99.
           03  FILLER           PIC X     VALUE ".".
           03  WK-PATH-HHMM     PIC 9(4).

       01  LOG-PATH.
           03  FILLER           PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER           PIC X(18) VALUE "ACT/NEWPOSPAY/LOG/".
           03  LOG-PATH-NAME    PIC X(14)  VALUE "LOG.NEWPOSPAY.".
           03  LOG-PATH-MACHINE PIC XX.
           03  FILLER           PIC X     VALUE ".".
           03  LOG-PATH-YR      PIC 99.
           03  LOG-PATH-MO      PIC 99.
           03  LOG-PATH-DA      PIC 99.

       01  DEFAULT-MACHINE      PIC X(2) VALUE "R1".

       01  HOLD-LOG-TIME        PIC X(8).

       01  WK-IFN               PIC XX     VALUE "WK".
       01  WK-KEY               PIC 9(9)   VALUE 0.

      *****************************************************************
      *   MISC WORKERS     *
      *****************************************************************
       01  RECORDS-FOUND        PIC X         VALUE "N".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5)      VALUE "MESS.".
       01  LEGEND               PIC X(70)     VALUE SPACES.
       01  CK-COUNT             PIC 9(6)      COMP-3 VALUE 0.
       01  CK-TOTAL             PIC S9(11)V99 COMP-3 VALUE 0.
       01  GRAND-COUNT          PIC 9(6)      COMP-3 VALUE 0.
       01  GRAND-TOTAL          PIC S9(11)V99 COMP-3 VALUE 0.


      *****************************************************************
      *   LINE WORKERS     *
      *****************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      *****************************************************************
      *     P R I N T   B U F F E R S   *
      *****************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(11)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(9)    VALUE "USER ID:".
           03  H-USERID         PIC X(8).
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(39)    VALUE
           "WELLS FARGO CHECK RECONCILIATION REPORT".
           03  FILLER           PIC X(11)    VALUE  " ".
           03  FILLER           PIC X(8)     VALUE "* = VOID".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9999.
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(20).
           03  FILLER      PIC X(16)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(29)
               VALUE "    CHECK#  VOID   --DATE--".
           03  FILLER      PIC X(22) VALUE "       AMOUNT   PAYEE".


      *****************************************************************
      *    D E T A I L    L I N E S
      *****************************************************************
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-CHECKNO   PIC Z(10).
           03  FILLER      PIC XX.
           03  D-VOID      PIC X.
           03  FILLER      PIC X(6).
           03  D-CKDATE    PIC 99/99/99.
           03  FILLER      PIC XX.
           03  D-CKAMT     PIC ZZ,ZZZ,ZZZ.99-.
           03  FILLER      PIC XX.
           03  D-PAYEE     PIC X(30).


       01  T-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(18).
           03  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.
           03  FILLER      PIC X(6).
           03  T-ITEMS     PIC ZZ,ZZ9.

       01  T-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(22).
           03  T2-BRNO     PIC 9(4).
           03  FILLER      PIC X.
           03  T2-DESC     PIC X(20).
           03  FILLER      PIC X.
           03  FILLER      PIC X.
           03  T2-ITEMS    PIC ZZ,ZZ9.
           03  FILLER      PIC X.
           03  T2-CKTOTAL  PIC ZZZ,ZZZ,ZZZ.99-.


       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(45).
           03  RANGE-SELECTION.
               04  FILLER       PIC XX.
               04  R-NUM        PIC 9(6).
           03  R-DATE REDEFINES RANGE-SELECTION
                                PIC 99/99/99.
           03  R-FLAGX REDEFINES RANGE-SELECTION.
               04  FILLER       PIC X(7).
               04  R-FLAG       PIC X.

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-DATE            PIC 9(8).
           03  END-DATE            PIC 9(8).
           03  DATE2 REDEFINES END-DATE.
               05  END-CY          PIC 99.
               05  END-YR          PIC 99.
               05  END-MO          PIC 99.
               05  END-DA          PIC 99.
           03  ALREADY-EXTRACTED   PIC X.
           03  TOT-FG              PIC X.
           03  REPORT-FG           PIC X.
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).

       01 VERSION-CONTROL      PIC X VALUE "B".

      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBCL/CKSTATW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CHKWF5_DEF.CPY".
       COPY "LIBSP/CHKWF5_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CHKWF5_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE LOG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE LOG-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *    M A I N    M O D U L E
      *
      *****************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
       MAIN-ROUTINE.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHKWF5"      TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM CREATE-HEADER-REC.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

      *    IF BR-USE-NEW-WFARGO-ACCT NOT = "Y"
      *       GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRC-PATH-OVERRIDE.

           PERFORM OPEN-TRC1-FILE.

           PERFORM TR-CHECKS.
           PERFORM BRANCH-TOTALS.
       END-OF-GROUP.
           PERFORM OPEN-TRC1-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           PERFORM CREATE-TRAILER-REC.
           PERFORM CREATE-LOG-REC.

           PERFORM GRAND-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *****************************************************************
       TR-CHECKS SECTION.
      *****************************************************************

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"           TO TRC-PATH-OVERRIDE.

           MOVE BR-NO       TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.
           MOVE BEG-DATE    TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE END-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.
 
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.
       NEXT-TR.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-TR-CHECKS.

           IF TC-KEY-DATE > END-DATE
              GO TO EXIT-TR-CHECKS.

           IF TC-TYPE = "CA" OR "BK" OR "RR" OR "RC" OR "RP" OR "RD"
                     OR "ID" OR "IW" OR "AC" OR "DB"
              GO TO NEXT-TR.

           IF ALREADY-EXTRACTED = "S"
              IF TC-EXTRACT-FG = "Y"
                 GO TO NEXT-TR.

           IF ALREADY-EXTRACTED = "O"
              IF TC-EXTRACT-FG NOT = "Y"
                 GO TO NEXT-TR.

           PERFORM CREATE-RECON-FILE.

           PERFORM DETAIL-LINE-PROCESS.

           IF REPORT-FG = "N"
              MOVE "Y" TO TC-EXTRACT-FG
              PERFORM REWRITE-TRC1-FILE.

           GO TO NEXT-TR.

       EXIT-TR-CHECKS.
           EXIT.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************

           MOVE CHKWF5-QUEUE-POS TO Q-POS.
           MOVE CHKWF5-COPIES-POS TO C-POS.
           MOVE CHKWF5-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO EXIT-INITIALIZE.

           MOVE "FILE CREATE IN PROGRESS................" TO LEGEND.

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.
           IF END-DATE = 0
              MOVE 19000101 TO END-DATE.

           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

      * IF REPORT ONLY DO NOT BOTHER WITH OUTPUT FILE
           IF REPORT-FG = "Y"
              GO TO EXIT-INITIALIZE.

           PERFORM GET-WK-AND-LOG-PATH.

           PERFORM OPEN-WK-FILE-OUTPUT.

           MOVE LOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-LOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-LOG-FILE-OUTPUT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       GET-WK-AND-LOG-PATH SECTION.
      ********************************************

           CALL "C$TOLOWER" USING DEFAULT-MACHINE, VALUE 2.
           CALL "C$TOLOWER" USING WK-PATH, VALUE 13.
           CALL "C$TOLOWER" USING WK-PATH-NAME, VALUE 7.

           CALL "C$TOLOWER" USING LOG-PATH, VALUE 13.
           CALL "C$TOLOWER" USING LOG-PATH-NAME, VALUE 14.

      * /USR/EXTRACT/ACT/POSPAY/CHECKS.R1.YYMMDD.HHMM
           MOVE END-YR  TO WK-PATH-YR LOG-PATH-YR.
           MOVE END-MO  TO WK-PATH-MO LOG-PATH-MO.
           MOVE END-DA  TO WK-PATH-DA LOG-PATH-DA.

           MOVE EXT-FILPATH-MACHINE TO WK-PATH-MACHINE LOG-PATH-MACHINE.

       GET-WK-PATH-AGAIN.
           MOVE "Y"                 TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE H-AND-M             TO WK-PATH-HHMM.

           MOVE TIME-EDIT           TO HOLD-LOG-TIME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              GO TO GET-WK-PATH-AGAIN.

       GET-WK-PATH-EXIT.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO REPORT-ONLY.

      *----------------------------------------------------------------

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO BEG-DT.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO BEG-DT.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BEG-DT.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-EXTRACTED.

           MOVE LEGEND-DATE1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEGDT."    TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEG-DT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *----------------------------------------------------------------
       END-DT.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-EXTRACTED.

           MOVE LEGEND-DATE2 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060ENDDT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEG-DT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO END-DT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DT.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-EXTRACTED.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010EXTRACTED."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO END-DT
              ELSE
                 GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-EXTRACTED.
           IF NOT (VDUBUF = "I" OR "S" OR "O")
              GO TO ENTER-EXTRACTED.
           MOVE VDUBUF TO ALREADY-EXTRACTED.

       TOT-ONLY.
           MOVE LEGEND-TOT-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010TOTONLY."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-EXTRACTED.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TOT-ONLY.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO TOT-ONLY.
           MOVE VDUBUF TO TOT-FG.


       REPORT-ONLY.
           MOVE LEGEND-REPORT-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010REPORTONLY."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TOT-ONLY.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPORT-ONLY.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO REPORT-ONLY.
           MOVE VDUBUF TO REPORT-FG.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A010EXTRACTED."    TO VDU.
           MOVE ALREADY-EXTRACTED TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010TOTONLY."    TO VDU.
           MOVE TOT-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010REPORTONLY."    TO VDU.
           MOVE REPORT-FG TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       CREATE-RECON-FILE SECTION.
      *****************************************************************
           MOVE SPACES TO WK-REC.

           MOVE TC-BRNO        TO WK-CHECK-BRNO.
           MOVE TC-CKNO        TO WK-CHECK-NO.
           MOVE TC-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO WK-CHECK-DATE.
           MOVE GP-BANK-ACCTNO TO WK-ACCT-NUMBER.

           IF TC-VOID = "Y"
              COMPUTE WK-CHECK-AMOUNT = TC-AMT * -1
           ELSE
              MOVE TC-AMT      TO WK-CHECK-AMOUNT.

           IF TC-VOID = "Y"
              MOVE 370 TO WK-TRANS-TYPE
           ELSE
              MOVE 320 TO WK-TRANS-TYPE.

           MOVE TC-PAYEE TO WK-PAYEE.

           MOVE X"0A"     TO WK-LF.

           IF REPORT-FG NOT = "Y"
              ADD 1 TO WK-KEY
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           MOVE "Y"   TO RECORDS-FOUND.
           ADD 1      TO CK-COUNT.
           ADD TC-AMT TO CK-TOTAL.

       EXIT-CREATE-RECON-FILE.
           EXIT.

      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************

           IF TC-VOID = "Y"
              MOVE "*" TO D-VOID.
           MOVE TC-CKNO          TO D-CHECKNO.
           MOVE TC-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-CKDATE.
           MOVE TC-AMT           TO D-CKAMT.
           MOVE TC-PAYEE         TO D-PAYEE.

           IF TOT-FG = "N"
              PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      *****************************************************************
       CREATE-HEADER-REC SECTION.
      *****************************************************************
           MOVE SPACES         TO WK-REC.
           MOVE "*03"          TO WK-STAR-3.
           MOVE 165            TO WK-BANK-ID.
           MOVE GP-BANK-ACCTNO TO WK-BANK-ACCTNO.
           MOVE 0              TO WK-FILE-STAT.
           MOVE X"0A"          TO WK-LF.
           IF REPORT-FG NOT = "Y"
              ADD 1 TO WK-KEY
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      *****************************************************************
       CREATE-TRAILER-REC SECTION.
      *****************************************************************
           MOVE SPACES      TO WK-REC.
           MOVE "&"         TO WK-AMPERSAND.
           MOVE GRAND-COUNT TO WK-RECORD-COUNT.
           MOVE GRAND-TOTAL TO WK-TOTAL-AMOUNT.
           MOVE X"0A"       TO WK-LF.
           IF REPORT-FG NOT = "Y"
              ADD 1 TO WK-KEY
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      *****************************************************************
       CREATE-LOG-REC SECTION.
      *****************************************************************

      * CREATE LOG FILE RECORD FOR EACH FILE CREATED FOR THE DAY
      * /USR/EXTRACT/ACT/POSPAY/LOG/LOG.POSPAY.R?.YYMMDD

           MOVE SPACES        TO LOG-REC.
           MOVE HOLD-LOG-TIME TO LOG-TIME.
           MOVE WK-PATH       TO LOG-FILENAME.
           MOVE GRAND-COUNT   TO LOG-RECORD-COUNT.

           PERFORM WRITE-LOG-FILE.

           PERFORM CLOSE-LOG-FILE.

      *****************************************************************
       BRANCH-TOTALS SECTION.
      *****************************************************************
           IF CK-TOTAL = 0
              IF CK-COUNT = 0
                 GO TO EXIT-BRANCH-TOTALS.

           MOVE "BRANCH TOTAL: BRANCH #" TO DETAIL-LINE.
           MOVE BR-NO           TO T2-BRNO.
           MOVE BR-NAME         TO T2-DESC.

           MOVE CK-COUNT        TO T2-ITEMS.
           MOVE CK-TOTAL        TO T2-CKTOTAL.
           MOVE 2               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD CK-TOTAL         TO GRAND-TOTAL.
           ADD CK-COUNT         TO GRAND-COUNT.

           IF TOT-FG NOT = "Y"
              MOVE 99           TO LN-COUNT.

       EXIT-BRANCH-TOTALS.
           MOVE 0 TO CK-TOTAL CK-COUNT.


      *****************************************************************
       GRAND-TOTALS SECTION.
      *****************************************************************

           MOVE 0                TO LN-COUNT.
           MOVE "GRAND TOTALS:"  TO DETAIL-LINE.
           MOVE GRAND-TOTAL      TO T-CKTOTAL
           MOVE GRAND-COUNT      TO T-ITEMS.
           MOVE 3                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
             PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER  TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH         TO H-HR.
           MOVE T-MM         TO H-MIN.

           MOVE HEAD1    TO PR-REC.
           MOVE 99       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2    TO PR-REC.
           MOVE 1        TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE BR-NO   TO HD-BRNO.
           MOVE BR-NAME  TO HD-BRNAME.
           MOVE HEAD3    TO PR-REC.
           MOVE 1        TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4    TO PR-REC.
           MOVE 2        TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      *****************************************************************
       PRINT-RANGE-LINES SECTION.
      *****************************************************************

           MOVE 4 TO LN-FEED.
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE" TO RANGE-LINE.
           MOVE END-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALREADY EXTRACTED" TO RANGE-LINE.
           MOVE ALREADY-EXTRACTED   TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTALS ONLY" TO RANGE-LINE.
           MOVE TOT-FG TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY" TO RANGE-LINE.
           MOVE REPORT-FG TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-PATH TO RANGE-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       END-RANGE-LINES.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CHKWF5-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHKWF5-SYSDATE.
           DISPLAY CHKWF5-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CHKWF5-FORM-FIELDS TO WR-BUF.
           MOVE CHKWF5-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CHKWF5-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHKWF5-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CHKWF5_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC SECTION.
      *****************************************************************

       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       OPEN-LOG-FILE-EXTEND.
           MOVE "LOG" TO E-FILE.
           OPEN EXTEND LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-EXTEND.
       OPEN-LOG-FILE-OUTPUT.
           MOVE "LOG" TO E-FILE.
           OPEN OUTPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-OUTPUT.
       WRITE-LOG-FILE.
           WRITE LOG-REC.
       CLOSE-LOG-FILE.
           CLOSE LOG-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

      *  IF NO RECORDS, REMOVE CHECKS FILE WITH JUST HEADER & TRAILER
           IF NO-RECORDS-FOUND
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              IF REPORT-FG NOT = "Y"
                 MOVE WK-PATH TO ACCESS-BUF
                 PERFORM REMOVE-WORKFILE.

       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/BANKMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CHKWF5-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
