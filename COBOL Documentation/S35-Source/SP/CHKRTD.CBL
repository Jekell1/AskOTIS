       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHKRTD.
       DATE-WRITTEN. 03-26-12
      *****************************************************************
      *
      *  DESCRIPTION   :  CREATE DAILY POSITIVE PAY FILE FOR
      *                   TRANSMISSION TO TD BANK
      *                   WITH JOURNAL OF CHECKS EXTRACTED
      *      NEW FOR WORLD 'TD BANK', PR # 793
      * WORLD STILL USES 4/27/2023
      *
      *  OUTPUT FILE IN: /USR/DATA/R?/SP/WACC1A03.R?.YYMMDD
      *  THIS IS JUST FOR WORLD.  
      *  IF RUNNING ON RISC1, WILL COPY FILE FROM SP TO FTP/TDBANK.
      *  IF RUNNING ON RISC2, WILL REMOTE COPY FILE TO FTP/TDBANK ON RISC1.
      *  RECORDS ARE WRITTEN TO INDEXED WORK FILE (WK2) BY BR# BY
      *  CHECK #, SO OUTPUT FILE WILL BE IN ORDER OF BRANCH. (AP
      *  CHECKS HAVE BRANCH OF 990).  HOWEVER, REPORT IS GENERATED
      *  WHEN RECORDS ARE READ IN, SO WILL BE IN ORDER BY RC STATUS
      *  BY BRANCH. LOAN CHECKS ARE INCLUDED IF RC-STATUS IS DEPOSIT (K1)
      *
      *
      *  NOTE - THIS PROGRAM HAS BATCH COPY MEMBERS BUT WILL NOT WORK
      *         IN BATCH MODE - IT SENDS ERROR MESSAGES, ETC. IF ALL
      *         BRANCHES NOT AT CORRECT STATUS.
      *
      * REV:
      * BLM 120416 CHANGED FILE NAME TO WACC1A03.R?.YYMMDD, CHANGES
      *            MADE TO SPWSTD & SHELLL CHKRTD.SH, WORLD PR# 793
      * BLM 120529 FIX TRYING TO WRITE OUTPUT FILE IF REPORT ONLY
      * BLM 120530 IF NO OUTPUT RECORDS WRITTEN, DON'T CREATE TRAILER
      *            RECORD & REMOVE EMPTY FILE
      * KEC 2015.1123 FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BAH 160606 CHANGED AH-REV-DATE TO 99999999 - YYYYMMDD
      *  BAH 170814 ACTIVATE 8 DIGIT DATES, PD0006
      *  BLM 180716 ADD A CHARACTER OF FILLER IN CHKRTD-COMMAND BETWEEN
      *             THE FILE NAME & THE DATA PATH, THERE WAS NO SPACE
      *             SO THE FILE WAS NOT GETTING COPIED.
      * BAH 2018.1217 CHANGE TD-BANK-ID FROM 0004 TO 0165 AND 
      *               TD-ACCT-NUMBERS TO 8018016512, WORLD #1356
      * KEC 2019.0128 A32 - GPFILE GP-REC MODIFICATION 
      *               REMOVED GPFILE SINCE NOT USED.
      * BAH 2019.1004 REMOVED ACCESS-CALL TRCFILE
      * BAH 2019.1105 REMOVED LIBAP ITEMS
      * BAH 2020.0424 SET NOACCEPT-BUF TO "Y" SO IT DOESN'T ENTER ACCEPT
      *               BUF IN EOINIT, IT ENTERS IT IN THIS PROGRAM, AFTER
      *               OTHER TASKS.
      * BAH 2020.0831 REMOVED RESET ON BR1 
      * BAH 2021.0318 SKIP TC-TYPE "DB" DEBIT CARD UMC #1490
      * BAH 2024.0712 WAS DISPLAYING "WARNING: AP CHECKS NOT THROUGH AP
      *               CHECK RELEASE UPDATE". AP WAS REMOVED WAY BACK BUT
      *               THE CHECK OF THE AP-STATUS-OK WAS NOT REMOVED.
      *               REMOVED AP-STATUS-OK CHECK #S35Q-85
      * BAH 2024.0715 WAS DUPLICATING RECORDS IN THE EXTRACT FILE.
      *               TRCFILE SHOULD ONLY BE GETTING 1 DAY AT A TIME AND
      *               IT WAS GETTING THE FULL DATE RANGE. SO EVERY RC
      *               DAY IT PROCESSED, IT INCLUDED ALL TRC RECORDS WITHIN
      *               THE BEG-END DATE RANGE. SHOULD ONLY GET THE TRC
      *               RECORDS THAT MATCH THE RC-TRANS-DATE! #S35Q-85
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSTD.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *****************************************************************
      *         TD BANK CHECK RECONCILLIATION FILE.
      *****************************************************************
       FD  TD-FILE
           LABEL RECORDS ARE STANDARD.
       01  TD-REC.
           03  TD-BANK-ID                  PIC 9(04).
           03  TD-APPLICATION              PIC 9(2).
           03  TD-ACCT-NUMBER              PIC 9(16).
           03  TD-ITEM-TYPE                PIC 9(2).
           03  TD-SERIAL-NUMBER.
               05  TD-BRNO                 PIC 9(4).
               05  TD-CKNO                 PIC 9(6).
           03  TD-ISSUE-AMT                PIC 9(9)V99.
           03  TD-DESC                     PIC X(30).
           03  TD-DATE-MMDDYY              PIC 9(6).
           03  TD-FILLER-ZERO              PIC 9.
           03  TD-ITEM-STATUS              PIC 9(2).
           03  TD-CR                       PIC X.
           03  TD-LF                       PIC X.

      *------------------------------------------------------------
      *    SORT CHECKS IN CHECK # ORDER
      *------------------------------------------------------------
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO            PIC 9(4).
               05  WK2-CKNO            PIC 9(6).
               05  WK2-CKSEQ           PIC 9(2).
           03  WK2-STATE               PIC XX.
           03  WK2-CHECK-AMOUNT        PIC 9(8)V99  COMP-3.
           03  WK2-ISSUE-DATE          PIC 9(8)     COMP-6.
           03  WK2-VOID-INDICATOR      PIC X.
           03  WK2-PAYEE               PIC X(30).

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/CHKRTD S35 07/15/24-08:44:09 >".

       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      ******************************************************************
      *    LIBSP/SPWSTD: TDFILE PATH WORKERS
      *    /USR/DATA/R#/SP/TDFILE
      ******************************************************************
       01  TD-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  TD-PATH                   PIC X(35)   VALUE SPACES.
       01  TD-PATH-FORMAT.
           03  TD-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  TD-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/SP/".
           03  TD-PATH-FILENAME.
               05  FILLER            PIC X(09)   VALUE "WACC1A03.".
               05  TD-PATH-MACHINE2  PIC X(02).
               05  TD-PATH-DOT       PIC X(01).
               05  TD-PATH-YR        PIC 9(02).
               05  TD-PATH-MO        PIC 9(02).
               05  TD-PATH-DA        PIC 9(02).
      *----------------------------------------------------------------
       01  PR-IFN               PIC XX     VALUE "PR".

       01  TD-KEY               PIC 9(8)   VALUE 0.
       01  WK2-PATH             PIC X(35)  VALUE SPACES.
       01  WK2-IFN              PIC X(3)   VALUE "WK2".

      *****************************************************************
       01  MESS-LIST            PIC X(5)      VALUE "MESS.".
       01  WARNING-LIST         PIC X(8)      VALUE "WARNING.".
       01  LEGEND               PIC X(70)     VALUE SPACES.
       01  BR-TOT-COUNT         PIC 9(6)      COMP-3 VALUE 0.
       01  BR-TOT-AMT           PIC S9(11)V99 COMP-3 VALUE 0.
       01  GR-TOT-COUNT         PIC 9(6)      COMP-3 VALUE 0.
       01  GR-TOT-AMT           PIC S9(11)V99 COMP-3 VALUE 0.

       01  HOLD-RC2-KEY.
           03  HOLD-RC-STATUS                  PIC X(2).
           03  HOLD-RC-BRNO                    PIC 9(4).
           03  HOLD-RC-TRANS-DATE              PIC 9(8).
           03  FILLER                REDEFINES HOLD-RC-TRANS-DATE.
               05  HOLD-RC-TRANS-DATE-CC       PIC 9(2).
               05  HOLD-RC-TRANS-DATE-YY       PIC 9(2).
               05  HOLD-RC-TRANS-DATE-MM       PIC 9(2).
               05  HOLD-RC-TRANS-DATE-DD       PIC 9(2).

       01  HOLD-TYPE            PIC XX.

       01  PROCESSING-MESS.
           03  FILLER           PIC X(22) VALUE
               "NOW PROCESSING BRANCH:".
           03  FILLER           PIC X(1)   VALUE " ".
           03  DISPLAY-BRNO     PIC Z999.
           03  FILLER           PIC X(5)   VALUE " FOR ".
           03  DISPLAY-DATE     PIC 99/99/99.


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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(8).
           03  FILLER           PIC X(28)    VALUE " ".
           03  FILLER           PIC X(40)    VALUE
           "- - - TD BANK POSITIVE PAY REPORT - - - ".
           03  FILLER           PIC X(46)     VALUE  " ".

       01  HEAD3.
           03  FILLER      PIC X(29) VALUE
               "BRNO  CHK #   VOID  CK DATE  ".
           03  FILLER      PIC X(47) VALUE
               "     CHECK AMT  PAYEE".
           03  FILLER      PIC X(30) VALUE
               "TYPE              BANK ACCOUNT".
           03  FILLER      PIC X(14) VALUE
               "  APP  BANK ID".

      *****************************************************************
      *    D E T A I L    L I N E S
      *****************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO          PIC 9(4).
           03  FILLER          PIC XX.
           03  D-CKNO          PIC 9(6).
           03  FILLER          PIC X.
           03  D-VOID          PIC X(6).
           03  FILLER          PIC X.
           03  D-DATE          PIC 99/99/99.
           03  FILLER          PIC X.
           03  D-CKAMT         PIC ZZZ,ZZZ,ZZZ.99-.
           03  FILLER REDEFINES D-CKAMT.
               05  FILLER      PIC X(14).
               05  D-SIGN      PIC X.
           03  FILLER          PIC X.
           03  D-PAYEE         PIC X(30).
           03  FILLER          PIC X.
           03  D-TYPE          PIC X(13).
           03  FILLER          PIC X.
           03  D-ACCT-NUMBER   PIC Z(15)9.
           03  FILLER          PIC XXX.
           03  D-APPLICATION   PIC 9(2).
           03  FILLER          PIC XXX.
           03  D-BANK-ID       PIC 9(4).


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
           03  T2-DATE     PIC 99/99/99.
           03  FILLER      PIC X.
           03  T2-ITEMS    PIC ZZ,ZZ9.
           03  FILLER      PIC X.
           03  T2-CKTOTAL  PIC ZZZ,ZZZ,ZZZ.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  FILLER       PIC XX.
               04  R-NUM        PIC 9(10).
           03  R-DATE REDEFINES RANGE-SELECTION
                                PIC 9999/99/99.
           03  R-FLAGX REDEFINES RANGE-SELECTION.
               04  FILLER       PIC X(9).
               04  R-FLAG       PIC X.

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-DATE            PIC 9(8).
           03  END-DATE            PIC 9(8).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).
           03  TOT-FG              PIC X.
           03  REPORT-FG           PIC X.

       01 VERSION-CONTROL      PIC X VALUE "A".

       01  AP-BEG-DATE         PIC 9(8).
       01  AP-END-DATE         PIC 9(8).

      **********************************************************
      * CHKRTD COPY COMMANDS - TD BANK POSITIVE PAY (WORLD)
      **********************************************************

      *  "/$REL/SH/EC /$REL/SH/CHKRTD.SH".
       01  CHKRTD-COMMAND.
           03  FILLER            PIC X    VALUE "/".
           03  CHKRTD-REL        PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  CHKRTD-EC         PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  CHKRTD-REL2       PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  CHKRTD-SHELL      PIC X(10) VALUE "CHKRTD.SH ".
      *  " 1 " = COPY
      *  " 2 " = RCP
      *  " 3 " = REMOVE
           03  CHKRTD-OPTION     PIC X(3)   VALUE " 1 ".
           03  CHKRTD-EXTRACT-NAME1 PIC X(18).
           03  FILLER            PIC X      VALUE " ".
           03  CHKRTD-DATA       PIC X(11).

       
      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBCL/CKSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CHKRTD_DEF.CPY".
       COPY "LIBSP/CHKRTD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CHKRTD_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE TD-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               TD-FILE WK2-FILE.
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
           MOVE "CHKRTD" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM TR-CHECKS.

           IF REPORT-FG NOT = "Y"
              PERFORM CREATE-RECON-FILE-FROM-SORT.

       END-ROUTINE.

           PERFORM GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           IF GR-TOT-COUNT NOT = 0
              PERFORM R1-CP-TO-FTP.

           GO TO END-PROGRAM.

      *****************************************************************
       TR-CHECKS SECTION.
      *****************************************************************
       NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-FG = 9 OR RC-STATUS > "K1"
              GO TO EXIT-TR-CHECKS.

           IF RC-BRNO > END-BRANCH
              GO TO EXIT-TR-CHECKS.

           IF RC-BRNO < BEG-BRANCH
              GO TO NEXT-RC.

           IF RC-TRANS-DATE < BEG-DATE OR RC-TRANS-DATE > END-DATE
              GO TO NEXT-RC.

           MOVE RC2-KEY TO HOLD-RC2-KEY.

           IF GROUP-FLAG NOT = "N"
              MOVE RC-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 GO TO NEXT-RC.

           MOVE RC-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * SET TR PATH
           IF BR-DATA-PATH = SPACES OR BR-MACHINE = SPACES
              MOVE "BRANCH &&&& HAS UNDEFINED DATA PATH" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              GO TO NEXT-RC.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"           TO TRC-PATH-OVERRIDE.

           MOVE RC-BRNO TO DISPLAY-BRNO.

           MOVE HOLD-RC-TRANS-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO DISPLAY-DATE.

           MOVE PROCESSING-MESS TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO       TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.

           MOVE RC-TRANS-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE RC-TRANS-DATE TO SQL-DATE-YYYYMMDD.
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
              PERFORM CLOSE-TRC1-FILE
              PERFORM UPDATE-RC-STATUS
              GO TO NEXT-RC.

           IF TC-KEY-DATE > RC-TRANS-DATE
              PERFORM CLOSE-TRC1-FILE
              PERFORM UPDATE-RC-STATUS
              GO TO NEXT-RC.

           IF TC-TYPE = "CA" OR "BK" OR "RR" OR "RC" OR "RP" OR "RD"
                     OR "ID" OR "IW" OR "AC" OR "DB"
              GO TO NEXT-TR.

           PERFORM CREATE-SORT-FILE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-TR.

       EXIT-TR-CHECKS.
           EXIT.


      *****************************************************************
       CREATE-SORT-FILE SECTION.
      *****************************************************************
           MOVE SPACES         TO WK2-REC.

           MOVE BR-NO          TO WK2-BRNO.
           MOVE TC-CKNO        TO WK2-CKNO.
           MOVE BR-STATE       TO WK2-STATE.
           MOVE TC-AMT         TO WK2-CHECK-AMOUNT.
           MOVE TC-DATE        TO WK2-ISSUE-DATE.
           MOVE TC-TYPE        TO HOLD-TYPE.
           IF TC-VOID = "Y"
              MOVE "Y"         TO WK2-VOID-INDICATOR
           ELSE
              MOVE " "         TO WK2-VOID-INDICATOR.
           MOVE TC-PAYEE       TO WK2-PAYEE.

           ADD 1      TO BR-TOT-COUNT GR-TOT-COUNT.
           ADD TC-AMT TO BR-TOT-AMT   GR-TOT-AMT.

           IF REPORT-FG = "Y"
              GO TO EXIT-CREATE-SORT-FILE.

           MOVE 0 TO WK2-CKSEQ.

       CREATE-SORT-RECORD.
           ADD 1 TO WK2-CKSEQ.
           IF WK2-CKSEQ > 98
              GO TO EXIT-CREATE-SORT-FILE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO CREATE-SORT-RECORD.

       EXIT-CREATE-SORT-FILE.
           EXIT.

      *****************************************************************
       UPDATE-RC-STATUS SECTION.
      *****************************************************************

           PERFORM BRANCH-TOTALS.
           IF REPORT-FG  = "Y"
              GO TO END-UPDATE-RC.

      *    PERFORM CLOSE-RC2-FILE.
           PERFORM OPEN-RC1-FILE.

           MOVE HOLD-RC-BRNO       TO RC-BRNO.
           MOVE HOLD-RC-TRANS-DATE TO RC-TRANS-DATE.
           PERFORM READ-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "M1" TO RC-STATUS.

           PERFORM SET-POSTDATE.
           MOVE POST-TIME TO RC-CHECK-TIME.
           MOVE POST-DATE TO RC-CHECK-DATE.

           PERFORM REWRITE-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.

      * DO NOT NEED TO CLOSE/REOPEN RC2 CURSOR TO UPDATE
      *    PERFORM OPEN-RC2-FILE.
      *    MOVE HOLD-RC2-KEY TO RC2-KEY.

      *    MOVE RC-STATUS    TO QRC2-WBEG-STATUS
      *                         QRC2-WEND-STATUS.
      *    MOVE RC-BRNO      TO QRC2-WBEG-BRNO.
      *    MOVE END-BRANCH   TO QRC2-WEND-BRNO.
      *    MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
      *    MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

      *    PERFORM START-RC2-FILE.

       END-UPDATE-RC.
           MOVE 0 TO BR-TOT-COUNT BR-TOT-AMT.
       EXIT-UPDATE-RC.
           EXIT.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************

           MOVE CHKRTD-QUEUE-POS TO Q-POS.
           MOVE CHKRTD-COPIES-POS TO C-POS.
           MOVE CHKRTD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

      * SET THIS SO THE ENTER OF THE ACCEPT FLAG TAKES PLACE HERE AFTER
      * CKSTAT, NOT INSIDE EOINIT. A15 HAD A FLDVAL IN EOINIT 
           MOVE "Y" TO NOACCEPT-BUF.

       COPY "LIBGB/EOINIT.CPY".
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO EXIT-INITIALIZE.

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.
           IF END-DATE = 0
              MOVE 19000101 TO END-DATE.

      * CHECK FOR AP CHECKS FOR ENTIRE MONTH:
           MOVE BEG-DATE      TO DATE-YYYYMMDD.
           MOVE 1             TO DATE-YYYYMMDD-DD.
           MOVE DATE-YYYYMMDD TO AP-BEG-DATE.

           MOVE END-DATE      TO NDTE-DATE.
           MOVE 31            TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE     TO AP-END-DATE.

           MOVE "C"          TO CKSTAT-ACTION.
           MOVE BEG-BRANCH   TO CKSTAT-BEG-BRANCH.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO CKSTAT-GROUP.
           MOVE END-BRANCH   TO CKSTAT-END-BRANCH.
           MOVE BEG-DATE     TO CKSTAT-DATE1.
           MOVE END-DATE     TO CKSTAT-DATE2.
           MOVE "K1"         TO CKSTAT-LOW-STATUS.
           MOVE "K1"         TO CKSTAT-HIGH-STATUS.
           MOVE "CL/CKSTAT"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSTAT-BUF.
           CANCEL FORM-PROGX.

           IF CKSTAT-RETURN-STAT = "N"
              MOVE "RANGE INCLUDES BRANCHES NOT AT CORRECT STATUS!"
                                   TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "ALL BRANCHES AT CORRECT STATUS" TO MESS
              PERFORM SEND-MESS.

       ACCEPT-IT.
           IF CKSTAT-RETURN-STAT = "N" MOVE
             "* WARNING: BRANCHES WITH WRONG STATUS WILL BE SKIPPED! *"
                           TO MESS
             PERFORM SEND-MESS.

           MOVE "F1-CANCEL, F6-SCAN STATUSES, F7-EXIT, 'Y' TO ACCEPT"
                           TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1U" GO TO EOINIT-ENTER-MENU.
           IF VDUSTATUS = "1<"
              GO TO ACCEPT-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N" GO TO EOINIT-ENTER-MENU.
           IF VDUBUF NOT = "Y"
              GO TO ACCEPT-IT.

       BEGIN-EXTRACT.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.

           IF REPORT-FG NOT = "Y"
              PERFORM REMOVE-PRIOR-FILES
              PERFORM OPEN-TD-FILE-OUTPUT
              PERFORM CLOSE-TD-FILE
              PERFORM OPEN-TD-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           PERFORM OPEN-RC2-FILE.

           MOVE "K1"       TO RC-STATUS
                              QRC2-WBEG-STATUS
                              QRC2-WEND-STATUS.
           MOVE BEG-BRANCH TO RC-BRNO
                              QRC2-WBEG-BRNO
           MOVE END-BRANCH TO QRC2-WEND-BRNO.

           MOVE BEG-DATE TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              PERFORM SEARCH-GR-INIT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-YY TO TD-PATH-YR.
           MOVE DATE-YYYYMMDD-MM TO TD-PATH-MO.
           MOVE DATE-YYYYMMDD-DD TO TD-PATH-DA.
           MOVE "."              TO TD-PATH-DOT.
           MOVE EXT-FILPATH-MACHINE TO TD-PATH-MACHINE2.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

           MOVE SPACES TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

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
              GO TO EXIT-PARM.

           PERFORM GET-DEFAULT-DATE.

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

       TOT-ONLY.
           MOVE LEGEND-TOT-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010TOTONLY."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO END-DT.
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
           IF REPORT-FG NOT = "Y"
              MOVE END-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-YY      TO TD-PATH-YR
              MOVE DATE-YYYYMMDD-MM      TO TD-PATH-MO
              MOVE DATE-YYYYMMDD-DD      TO TD-PATH-DA
              MOVE "."                   TO TD-PATH-DOT
              MOVE EXT-FILPATH-MACHINE   TO TD-PATH-MACHINE2
              PERFORM LOAD-TD-FILE
              MOVE TD-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 MOVE
                "&&&&&&&&&&&&&&&&&& ALREADY EXISTS; CANNOT CONTINUE!"
                    TO MESS
                 INSPECT MESS REPLACING FIRST
                        "&&&&&&&&&&&&&&&&&&" BY TD-PATH-FILENAME
                 PERFORM SEND-MESS
                 MOVE "X" TO EXT-EOBUF-ERRCD
                 GO TO EXIT-PARM
              END-IF.
           GO TO EXIT-PARM.

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

      *****************************************************************
       CREATE-RECON-FILE-FROM-SORT SECTION.
      *****************************************************************

      * READ SORT FILE WITH TR & AP CKS, CREATE OUTPUT FILE

           MOVE 0 TO WK2-BRNO WK2-CKNO WK2-CKSEQ.
           PERFORM START-WK2-FILE.
       NEXT-WK2.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO CREATE-TRAILER-REC.

           MOVE SPACES TO TD-REC.
           MOVE WK2-BRNO           TO TD-BRNO.
           MOVE WK2-CKNO           TO TD-CKNO.
           MOVE WK2-CHECK-AMOUNT   TO TD-ISSUE-AMT.
           MOVE WK2-ISSUE-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO TD-DATE-MMDDYY.
           MOVE 0                  TO TD-FILLER-ZERO.
           IF WK2-VOID-INDICATOR = "Y"
              MOVE 11              TO TD-ITEM-STATUS
           ELSE
              MOVE 10              TO TD-ITEM-STATUS.
           MOVE WK2-PAYEE          TO TD-DESC.
           MOVE 60                 TO TD-ITEM-TYPE.
           MOVE 02                 TO TD-APPLICATION.
           MOVE 0165               TO TD-BANK-ID.
           MOVE 8018016512         TO TD-ACCT-NUMBER.
           MOVE X"0D"              TO TD-CR.
           MOVE X"0A"              TO TD-LF.

           ADD 1 TO TD-KEY.
           PERFORM WRITE-TD-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO NEXT-WK2.

       CREATE-TRAILER-REC.
           IF TD-KEY = 0
              MOVE "NO RECORDS CREATED!" TO MESS
              PERFORM SEND-MESS
              MOVE TD-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
              GO TO EXIT-CREATE-FILE-FROM-SORT.

           MOVE "T"                TO TD-REC.
           MOVE X"0D"              TO TD-CR.
           MOVE X"0A"              TO TD-LF.

           ADD 1 TO TD-KEY.
           PERFORM WRITE-TD-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-CREATE-FILE-FROM-SORT.
           EXIT.

      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************

           IF WK2-VOID-INDICATOR = "Y"
              MOVE "*VOID*"      TO D-VOID.
           MOVE WK2-BRNO         TO D-BRNO.
           MOVE WK2-CKNO         TO D-CKNO.
           MOVE WK2-ISSUE-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-DATE.

           IF HOLD-TYPE = "LN" MOVE "LOAN"          TO D-TYPE
           ELSE
           IF HOLD-TYPE = "DL" MOVE "DEALER"        TO D-TYPE
           ELSE
           IF HOLD-TYPE = "MS" MOVE "MISC"          TO D-TYPE
           ELSE
           IF HOLD-TYPE = "AP" MOVE "ACCTS PAYABLE" TO D-TYPE
           ELSE
                               MOVE HOLD-TYPE       TO D-TYPE.

           MOVE WK2-CHECK-AMOUNT TO D-CKAMT.
           IF WK2-VOID-INDICATOR = "Y"
              MOVE "-" TO D-SIGN.
           MOVE 02     TO D-APPLICATION.
           MOVE 0165             TO D-BANK-ID.
           MOVE WK2-PAYEE        TO D-PAYEE.
           MOVE 8018016512    TO D-ACCT-NUMBER.

           IF TOT-FG = "N"
               PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      *****************************************************************
       BRANCH-TOTALS SECTION.
      *****************************************************************
           IF BR-TOT-COUNT = 0
              IF BR-TOT-AMT = 0
                 GO TO EXIT-BRANCH-TOTALS.

           MOVE "BRANCH TOTAL: BRANCH #" TO DETAIL-LINE.
           MOVE BR-NO           TO T2-BRNO.
           MOVE BR-NAME         TO T2-DESC.

           MOVE RC-TRANS-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO T2-DATE.

           MOVE BR-TOT-COUNT    TO T2-ITEMS.
           MOVE BR-TOT-AMT      TO T2-CKTOTAL.
           MOVE 2               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF TOT-FG NOT = "Y"
              MOVE 3 TO LN-FEED
           ELSE
              MOVE 2 TO LN-FEED.

       EXIT-BRANCH-TOTALS.
           EXIT.

      *****************************************************************
       GRAND-TOTALS SECTION.
      *****************************************************************

           MOVE 0                TO LN-COUNT.
           MOVE "GRAND TOTALS:"  TO DETAIL-LINE.
           MOVE GR-TOT-COUNT     TO T-ITEMS.
           MOVE GR-TOT-AMT       TO T-CKTOTAL.
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

           IF TOT-FG NOT = "Y"
              MOVE HEAD3    TO PR-REC
              MOVE 2        TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 5 TO LN-COUNT
           ELSE
              MOVE 3 TO LN-COUNT.

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

           MOVE "TOTALS ONLY" TO RANGE-LINE.
           MOVE TOT-FG TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY" TO RANGE-LINE.
           MOVE REPORT-FG TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE-LINES.
           EXIT.

      ******************************************************************
       REMOVE-PRIOR-FILES SECTION.
      ******************************************************************

      * REMOVE PRIOR FTP FILES 

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO CHKRTD-REL CHKRTD-REL2.
           CALL "C$TOLOWER" USING CHKRTD-EC, VALUE 3.
           CALL "C$TOLOWER" USING CHKRTD-SHELL, VALUE 10.
      * 3 = REMOVE
           MOVE " 3 " TO CHKRTD-OPTION.
           MOVE SPACES TO CHKRTD-EXTRACT-NAME1 CHKRTD-DATA.
           MOVE CHKRTD-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      ******************************************************************
       R1-CP-TO-FTP SECTION.
      ******************************************************************

      * COPY EXTRACT FILE ON RISC1 TO FTP DIRECTORY

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO CHKRTD-REL CHKRTD-REL2.
           CALL "C$TOLOWER" USING CHKRTD-EC, VALUE 3.
           CALL "C$TOLOWER" USING CHKRTD-SHELL, VALUE 10.
      * 1 = COPY
           MOVE " 1 " TO CHKRTD-OPTION.
           MOVE TD-PATH-FILENAME TO CHKRTD-EXTRACT-NAME1.
           MOVE EXT-FILPATH-FIL TO CHKRTD-DATA.
           MOVE CHKRTD-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.


      *****************************************************************
      *
      *    C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/POSTDATE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CHKRTD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHKRTD-SYSDATE.
           DISPLAY CHKRTD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CHKRTD-FORM-FIELDS TO WR-BUF.
           MOVE CHKRTD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CHKRTD-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHKRTD-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CHKRTD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-DEFAULT-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           PERFORM WEEKDAY.
           IF DAY-IS-MON
              MOVE -3 TO NDTE-HOLD
           ELSE
              MOVE -1 TO NDTE-HOLD.
           MOVE NUM-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.

           MOVE "D" TO CKSTAT-ACTION.
           MOVE BEG-BRANCH TO CKSTAT-BEG-BRANCH.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO CKSTAT-GROUP.
           MOVE END-BRANCH TO CKSTAT-END-BRANCH.
           MOVE EXT-JULIAN-BEG TO CKSTAT-DATE1.
           MOVE EXT-JULIAN-END TO CKSTAT-DATE2.
           MOVE "K1" TO CKSTAT-LOW-STATUS.
           MOVE "K1" TO CKSTAT-HIGH-STATUS.
           MOVE "CL/CKSTAT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSTAT-BUF.
           CANCEL FORM-PROGX.

           MOVE "S  8000BEGDT." TO VDU.
           IF CKSTAT-RETURN-DATE = 99999999
              MOVE NDTE-DATE TO VDU-DATE-YYYYMMDD
           ELSE
              MOVE CKSTAT-RETURN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000ENDDT." TO VDU.
           MOVE NDTE-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".
       LOAD-TD-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TD-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO TD-PATH-MACHINE
              MOVE TD-PATH-FORMAT          TO TD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TD-PATH-BASE
              MOVE FILPATH-MACHINE         TO TD-PATH-MACHINE
              MOVE TD-PATH-FORMAT          TO TD-PATH.

       OPEN-TD-FILE-OUTPUT.
           PERFORM LOAD-TD-FILE.
           PERFORM OPEN-IT.
           MOVE TD-PATH TO E-FILE.
           OPEN OUTPUT TD-FILE.
           IF IO-FG = 8
             GO TO OPEN-TD-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE TD-FILE
             GO TO OPEN-TD-FILE-OUTPUT.
           UNLOCK TD-FILE.
       OPEN-TD-FILE.
           PERFORM LOAD-TD-FILE.
           PERFORM OPEN-IT.
           MOVE TD-PATH TO E-FILE.
           OPEN I-O TD-FILE.
           IF IO-FG = 8
             GO TO OPEN-TD-FILE.
           IF IO-FG = 7
             CLOSE TD-FILE
             GO TO OPEN-TD-FILE.
           UNLOCK TD-FILE.
       WRITE-TD-FILE.
           PERFORM WRITE-IT.
           MOVE TD-PATH TO E-FILE.
           MOVE TD-KEY TO E-KEYX.
           WRITE TD-REC.
           IF IO-FG = 8
             GO TO WRITE-TD-FILE.
           UNLOCK TD-FILE.
       CLOSE-TD-FILE.
           PERFORM CLOSE-IT.
           CLOSE TD-FILE.
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/BANKMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CHKRTD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
