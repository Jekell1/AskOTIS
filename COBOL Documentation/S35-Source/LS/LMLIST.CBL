       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LMLIST.
       DATE-WRITTEN. 03-26-84
      *****************************************************
      *                      (LP)
      *    DESCRIPTION:  LOAN MEMO FILE LIST
      *
      * REV :
      *  012589 BAH FIXED INVALID DATE RANGE
      *  102095 JFF ADDED BORROWER LOAN SCAN
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE. ADDED BWFILE.
      *  GLF 110998 MADE MEMO DATE 2000 COMPATIBLE
      *  BAH 120110 REPLACED RRFORM WITH SCREEN
      * BAH 170427 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181128 GLOBAL LMFILE
      * BAH 20220803 HARD CODED START
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/LMLIST.CBL S35 02/21/24-12:03:36 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       01  PR-IFN               PIC X(11)    VALUE "0APRINT".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  LINE-SW              PIC 9        VALUE 2.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  HOLD-ACCTNO          PIC 9(8)    VALUE ZERO.
      * YYYYMMDD
       01  HOLD-DATE            PIC 9(8)    VALUE ZERO.
      * YYYYMMDD
       01  MEMO-DATE            PIC 9(8)  VALUE 0.
       01  HOLD-LN-OWNBR       PIC 9(4)    COMP-3 VALUE ZERO.
       01  LOAN-FG             PIC X(3)    VALUE "   ".
           88  LOAN-BEG                    VALUE "BEG".
           88  LOAN-END                    VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-ACCT        PIC 9(8).
           03  END-ACCT        PIC 9(8).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).

       01  ACCEPT-BUF          PIC X    VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

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
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           " - - - - LOAN MEMO FILE LISTING - - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(17)    VALUE " ".
           03  FILLER      PIC X(8)     VALUE "LOAN DTE".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(8)     VALUE "LOAN AMT".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(8)     VALUE "PYMT AMT".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC XXX      VALUE "DUE".

       01  HEAD4.
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(8)     VALUE "LOAN NO.".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC X(8)     VALUE "LST PYMT".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(8)     VALUE "CURR BAL".
           03  FILLER      PIC X        VALUE " ".
           03  FILLER      PIC X(10)    VALUE "INT/LC YTD".
           03  FILLER      PIC X        VALUE " ".
           03  FILLER      PIC XXXX     VALUE "TERM".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC XXXX     VALUE "RATE".
           03  FILLER      PIC XXX      VALUE " ".
           03  FILLER      PIC X(8)     VALUE "MEMO DTE".
           03  FILLER      PIC X        VALUE " ".
      *    03  FILLER      PIC XXX      VALUE "SEQ".
      *    03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC XXXX     VALUE "MEMO".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-DATE      PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-AMT1      PIC ZZZ,ZZZ.ZZ.
           03  FILLER      PIC X.
           03  D-AMT2      PIC ZZZ,ZZZ.ZZ.
           03  FILLER      PIC X.
           03  D-TERM      PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-RATE      PIC ZZZ.ZZZ.
           03  FILLER      PIC X.
           03  D-MEMODATE  PIC 99/99/99.
           03  FILLER      PIC X.
      *    03  D-SEQ       PIC ZZ.
      *    03  FILLER      PIC XX.
           03  D-MEMO      PIC X(60).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SELECTION  PIC X(16).

       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  LMLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/LMLIST_DEF.CPY".
       COPY "LIBLS/LMLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/LMLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LMLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LMLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-LM1.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF LM-ACCTNO > END-ACCT
              GO TO END-ROUTINE.
           IF LM-ACCTNO < BEG-ACCT
              GO TO NEXT-LM1.

      * DONT PRINT IF ALL SPACES
           IF LM-DATE = 0 AND LM-MEMO = SPACES
              GO TO NEXT-LM1.

      * SET DATE IN CORRECT ORDER (MMDDYY)
           COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE.
           MOVE DATE-YYYYMMDD TO MEMO-DATE.

      * TO GET PRIORITY MEMOS:
           IF MEMO-DATE = 99999999
      * JUST PRIORITYS
              IF BEG-DATE = 99999999 AND END-DATE = 99999999
                 GO TO PRINT-MEMO
              ELSE
      * ALL MEMOS
                 IF BEG-DATE = EXT-JULIAN-BEG AND 
                    END-DATE = EXT-JULIAN-END
                    GO TO PRINT-MEMO.

      * CHECK IF MEMO DATE IS BEFORE BEGIN RANGE DATE
           IF MEMO-DATE < BEG-DATE OR MEMO-DATE > END-DATE
              GO TO NEXT-LM1.

       PRINT-MEMO.
           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LM1.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO FIRST-TIME
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF LINE-SW NOT > 1
               PERFORM PRINT-LINE2
               PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE " " TO ERRCD.
           MOVE LMLIST-QUEUE-POS TO Q-POS.
           MOVE LMLIST-COPIES-POS TO C-POS.
           MOVE LMLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 GO TO AC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-01.
           MOVE VDUNUM0 TO BEG-ACCT.

       AC-02.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO AC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 GO TO AC-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-02.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE " INVALID ACCOUNT NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO AC-01.

           GO TO DT-01.
       ALL-AC.
           MOVE "SNNN060AC1." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.
       DT-01.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "12" GO TO JUST-PRIORITY.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO AC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
           IF VDUNUM0 = 999999
              MOVE 99999999 TO BEG-DATE
              GO TO DT-01.

       DT-02.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           GO TO OK-YN.

       JUST-PRIORITY.
           MOVE "S  8080DATE1." TO VDU.
           MOVE 99999999 TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2." TO VDU.
           MOVE 99999999 TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           GO TO OK-YN.

       ALL-DT.
           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO AC-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

      * JUST PRIORITYS
           IF BEG-DATE = 99999999 AND END-DATE = 99999999
              GO TO OPEN-FILES.

           IF BEG-DATE = 0
              MOVE EXT-JULIAN-BEG TO BEG-DATE.
           IF END-DATE = 99999999
              MOVE EXT-JULIAN-END TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

       OPEN-FILES.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE BEG-ACCT      TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO.
           MOVE END-ACCT      TO QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE LM-SEQ
                                 QLM1-WBEG-DATE-REV
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           IF LM-ACCTNO NOT = HOLD-ACCTNO
              IF LINE-SW NOT > 1
                 PERFORM PRINT-LINE2
                 PERFORM WRITE-DETAIL-LINE.

           IF LM-ACCTNO NOT = HOLD-ACCTNO
               MOVE 2 TO LN-FEED
               PERFORM FIND-LOAN
           ELSE
               IF LINE-SW NOT > 1
                   PERFORM PRINT-LINE2.
      * (MMDDYY)
           IF MEMO-DATE NOT = HOLD-DATE
              MOVE MEMO-DATE TO DATE-YYYYMMDD HOLD-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-MEMODATE.

           MOVE LM-MEMO TO D-MEMO.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ****************************
       FIND-LOAN SECTION.
      ****************************
           MOVE 1 TO LINE-SW.
           MOVE LM-ACCTNO TO HOLD-ACCTNO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
               MOVE 0 TO LN-DATE-PAID-LAST LN-CURBAL LN-YTDINTPD
                         LN-ACCERN(1) LN-ORGTERM LN-CLASS
               GO TO END-FIND-LOAN.
           IF HOLD-LN-OWNBR NOT = LN-OWNBR
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-OWNBR TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 PERFORM CLOSE-BR-FILE.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-LOANDATE NOT = 0
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE.
           MOVE LN-LNAMT TO D-AMT1.
           MOVE LN-REGPYAMT TO D-AMT2.
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE NUM-DA TO D-TERM.
           MOVE LN-APRATE TO D-RATE.
           MOVE MEMO-DATE TO HOLD-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MEMODATE.
       END-FIND-LOAN.
           MOVE 2 TO LN-FEED.

      ***********************************
       PRINT-LINE2 SECTION.
      ***********************************

      *  PRINT 2ND LINE OF LOAN DATA   *
           MOVE 2 TO LINE-SW.
           MOVE 1 TO LN-FEED.
           IF LN-DATE-PAID-LAST NOT = 0
               MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY TO D-DATE.
           MOVE LN-CURBAL TO D-AMT1.
           MOVE LN-ORGTERM TO D-TERM.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              IF LN-YTDINTPD = ZERO
                 MOVE LN-ACCERN(1) TO D-AMT2
               ELSE
                 MOVE LN-YTDINTPD TO D-AMT2
            ELSE
              IF LC-LOANTYPE = "I"
                 MOVE LN-YTDINTPD TO D-AMT2
               ELSE
                 MOVE LN-ACCERN(1) TO D-AMT2.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES AND FIRST-TIME NOT = "X"
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
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

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING LOAN #" TO RANGE-LINE.
           MOVE BEG-ACCT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING LOAN #" TO RANGE-LINE.
           MOVE END-ACCT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DATE" TO RANGE-LINE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ***********************************
       BWLN-SCAN SECTION.
      ***********************************
           MOVE EXT-CONAME-SYSDATE TO BWLNSC-SYSDATE.
           MOVE 0                  TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BWLN-SCAN.

           IF ( LOAN-FG = "BEG" )
                MOVE "SN N060AC1." TO VDU
           ELSE
                MOVE "SN N060AC2." TO VDU.

           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO LMLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LMLIST-SYSDATE.
           DISPLAY LMLIST-SCREEN UPON LMLIST-WINDOW-HANDLE.
           MOVE LMLIST-FORM-FIELDS TO WR-BUF.
           MOVE LMLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LMLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE LMLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/LMLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC SECTION.
      *************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *************************************************
       IO-ROUTINE SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************************
       END-PROGRAM SECTION.
      *************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LMLIST-SCREEN.
           DESTROY LMLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
