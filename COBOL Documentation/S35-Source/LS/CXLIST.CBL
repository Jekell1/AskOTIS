       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CXLIST.
       DATE-WRITTEN.    042800.
      ******************************************************************
      *    DIR :  LS
      *    DESC:  UNPRINTED CHECK LISTING
      *
      * CURRENT BRANCH ONLY, DOES NOT CROSS DATA PATHS
      *
      * REV:
      * MJD 1997-0103 CONVERTED TO 4 DIGIT BRANCH.
      * GLF 1998-1007 UPDATED FIELD DISPLAY/ENTRY.
      * BLV 1999-0114 RENAMED TO NPCXLS - WAS CKLIST - VERY CONFUSING!
      * GLB 2000-0428 FROM NP/NPCXLS.C
      * BAH 2018-1029 GLOBAL DTFILE
      * JKC 2022-0222 AUTO-POPULATE BEG-BRANCH & END-BRANCH WITH USER'S
      *               BRANCH SINCE THIS PROGRAM ONLY EXECUTES USING
      *               THE USER'S BRANCH.
      ******************************************************************
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
           "@(#)LS/CXLIST.CBL S35 02/21/24-12:05:16 >".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
      ****************************
      *     F I L E   I F N      *
      ****************************
       01  PR-IFN               PIC X(2)    VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99    VALUE 0.
           03  LN-COUNT         PIC 999   VALUE 99.
           03  LN-WK            PIC 999   VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X     VALUE "Y".
       01  MESS-LIST            PIC X(5)  VALUE "MESS.".
       01  REC-CT               PIC 9(6)  VALUE 0.
       01  TOTAL-AMT            PIC S9(9)V99 COMP-3 VALUE +0.

       01  DEALERA         PIC 9(6).
       01  DEALERAX       REDEFINES DEALERA.
           03  DEALERA1-4  PIC 9(4).
           03  DEALERA5-6  PIC 99.
       01  DEALERB         PIC 9(6).
       01  DEALER-BX      REDEFINES DEALERB.
           03  DEALERB1-4  PIC 9(4).
           03  DEALERB5-6  PIC 99.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD               PIC X.
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-DLNO            PIC 9(6).
           03  END-DLNO            PIC 9(6).
           03  BEG-TP              PIC X.

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
           03  FILLER           PIC X(40)    VALUE
           "- - - UNPRINTED CHECK LISTING - - - ".

       01  HEAD3.
           03  FILLER           PIC X(57)    VALUE
           "BR   TYPE  SEQ    DEALER   DATE    ACCOUNT       AMOUNT  ".
           03  FILLER           PIC X(16)    VALUE
           "PAYEE-----------".
           03  FILLER           PIC X(40)    VALUE
           "  FREE-TYPE/REASON----------------------".
           03  FILLER           PIC X(32)    VALUE
           "--------------------------------".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC Z(4).
           03  FILLER        PIC X(3).
           03  D-TYPE        PIC XX.
           03  D-TYPEX       PIC X.
           03  D-SEQNO       PIC Z(4).
           03  FILLER        PIC X(2).
           03  D-DLNO        PIC ZZZZ.99.
           03  FILLER        PIC X(2).
           03  D-DATE        PIC 99/99/99.
           03  FILLER        PIC X(2).
           03  D-ACCTNO      PIC Z(6).
           03  D-AMT         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  FILLER        PIC X.
           03  D-PAYEE       PIC X(30).
           03  D-DETAIL-AREA REDEFINES D-PAYEE.
               05  D-BATCHID     PIC Z(10).
               05  D-BATCHIDX    PIC X.
               05  D-BATCHSEQ    PIC 999.

           03  D-FREE        PIC X(70).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               05  R-BR         PIC 9(4).
               05  FILLER       PIC XX.
               05  R-DL         PIC ZZZZ.99.
               05  FILLER       PIC XX.
               05  R-TP         PIC X.

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  CXLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CXLIST_DEF.CPY".
       COPY "LIBLS/CXLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CXLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-CX1-FILE.
           CLOSE
              PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CXLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CXLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-CX.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-PATH-OWNBR NOT = CX-BRNO)
              GO TO END-ROUTINE.

           IF CX-BRNO > END-BRANCH
              GO TO END-ROUTINE.

           IF BEG-TP NOT = "~"
              IF CX-TYPE NOT = BEG-TP
                 GO TO NEXT-CX.

           IF CX-DLNO < BEG-DLNO OR CX-DLNO > END-DLNO
              GO TO NEXT-CX.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-CX.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "NO RECORDS FOR RANGE" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

       SETRP.
           MOVE " " TO ERRCD.
           MOVE CXLIST-QUEUE-POS TO Q-POS.
           MOVE CXLIST-COPIES-POS TO C-POS.
           MOVE CXLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

      * FORCE USER'S BRANCH
           MOVE "SNNN040BR1." TO VDU.         
           MOVE EXT-FILPATH-BRNO TO VDUNUM0
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU. 
           MOVE EXT-FILPATH-BRNO TO VDUNUM0. 
           PERFORM SCREENIO.
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO DL-01.

           COMPUTE BEG-DLNO = VDUNUM2 * 100.

       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO DL-02.

           COMPUTE END-DLNO = VDUNUM2 * 100.

           GO TO TP-01.

       ALL-DL.

           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DLNO VDUNUM2.
           PERFORM SCREENIO.


           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DLNO VDUNUM2.
           PERFORM SCREENIO.
       TP-01.
           MOVE "E  A010TP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-TP.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO TP-01.
           MOVE VDUBUF TO BEG-TP.
           IF BEG-TP = "~" GO TO TP-01.
           GO TO OK-YN.
       ALL-TP.
           MOVE "S  A010TP1." TO VDU.
           MOVE "~" TO BEG-TP VDUBUF.
           PERFORM SCREENIO.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO TP-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "Y" GO TO EXPR.
           IF VDUBUF NOT = "N" GO TO OK-YN.
           GO TO TP-01.

       EXPR.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS.............." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-CX1-FILE.

           MOVE BEG-BRANCH TO CX-BRNO.
           MOVE SPACES     TO CX-TYPE.
           MOVE 0          TO CX-SEQNO.

           PERFORM START-CX1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       DETAIL-LINE-PROCESS SECTION.
      ***********************************
           MOVE CX-BRNO TO D-BRNO.
           MOVE CX-TYPE TO D-TYPE.
           MOVE "-"     TO D-TYPEX.
           MOVE CX-SEQNO TO D-SEQNO.

           MOVE CX-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           COMPUTE D-DLNO = CX-DLNO / 100.
           MOVE CX-ACCTNO TO D-ACCTNO.
           MOVE CX-AMT TO D-AMT.
           MOVE CX-PAYEE TO D-PAYEE.
           IF CX-TYPE NOT = "N"
              IF CX-REASON NOT = " "
                 MOVE CX-REASON TO D-FREE
                 PERFORM WRITE-DETAIL-LINE
                 GO TO TEST-BATCH-CHECK.
           MOVE CX-FREE1 TO D-FREE.
           PERFORM WRITE-DETAIL-LINE.
           IF CX-FREE2 NOT = " "
              MOVE CX-FREE2 TO D-FREE
              PERFORM WRITE-DETAIL-LINE.
       TEST-BATCH-CHECK.
             IF CX-BATCHID NOT = 0
                PERFORM GET-CHECK-REQUEST-DETAIL.

       DETAIL-IDENTIFICATION.

       EXIT-DETAIL-LN-PROCESS.
           ADD CX-AMT TO TOTAL-AMT.
           ADD 1 TO REC-CT.

      *******************************************
       GET-CHECK-REQUEST-DETAIL SECTION.
      *******************************************
           PERFORM OPEN-DT1-FILE.

           MOVE CX-BRNO    TO DT-BRNO
                              QDT1-WBEG-BRNO
                              QDT1-WEND-BRNO.
      * ONLY WANT THE FIRST 4 OF DEALER TO MATCH
           MOVE CX-DLNO    TO DT-NUM.
           MOVE DT-NUM     TO QDT1-WBEG-NUM
                              QDT1-WEND-NUM.
           MOVE CX-CLASS   TO DT-CLASS
                              QDT1-WBEG-CLASS.
           MOVE CX-ACCTNO  TO DT-ACCTNO
                              QDT1-WBEG-ACCTNO.

           MOVE 0          TO DT-SEQ
                              QDT1-WBEG-SEQ.

           MOVE ALL "9"    TO QDT1-WEND-CLASS
                              QDT1-WEND-ACCTNO
                              QDT1-WEND-SEQ.
 
           MOVE 19000101   TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE

           PERFORM START-DT1-FILE.
       NEXT-CHECK-DETAIL.
           PERFORM READ-DT1-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = CX-BRNO)      
              GO TO EXIT-GET-REQUEST-DETAIL.

           MOVE DT-NUM  TO DEALERA.
           MOVE CX-DLNO TO DEALERB.
           IF DEALERA1-4 NOT = DEALERB1-4
              GO TO EXIT-GET-REQUEST-DETAIL.

           IF DT-BATCHID NOT = CX-BATCHID
              GO TO NEXT-CHECK-DETAIL.
      * MULTIPLE CKS MEAN WE MUST MATCH CORRECT DT TO CX BY SEQ
      *    IF DT-SEQ NOT = CX-DT-SEQ
      *    MOVE "DTSEQ" TO MESS PERFORM SEND-MESS
      *       GO TO NEXT-CHECK-DETAIL.

      * DT-STATUS = " "  -  RECORDS NOT RELEASED THRU BATCH RELEASE
      * DT-STATUS = "R"  -  RECORDS HAVE BEEN RELEASED, WAITING TO PRINT
      * DT-STATUS = "P"  -  RECORDS HAVE BEEN PRINTED

           IF DT-STATUS NOT = "R"
              GO TO NEXT-CHECK-DETAIL.

           MOVE DT-RELEASED TO D-AMT.
           MOVE DT-ACCTNO   TO D-ACCTNO.
           MOVE DT-BATCHID  TO D-BATCHID.
           MOVE "-"         TO D-BATCHIDX.
           MOVE DT-BATCHID-SEQ  TO D-BATCHSEQ.

           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-CHECK-DETAIL.
       EXIT-GET-REQUEST-DETAIL.
           PERFORM CLOSE-DT1-FILE.

      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************
       HEAD-PAGE SECTION.
      ***********************************
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

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 4 TO LN-COUNT.

      ***********************************
       PRINT-RANGE-LINES SECTION.
      ***********************************
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL AMT:" TO RANGE-LINE.
           MOVE TOTAL-AMT TO D-AMT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           MOVE "TOTAL RECORDS" TO RANGE-LINE.
           MOVE REC-CT TO R-DL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG BR, DL, TP" TO RANGE-LINE.
           MOVE EXT-FILPATH-BRNO TO R-BR.    
           MOVE BEG-DLNO TO R-DL.
           MOVE BEG-TP TO R-TP.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG BR, DL" TO RANGE-LINE.
           MOVE EXT-FILPATH-BRNO TO R-BR.  
           MOVE END-DLNO TO R-DL.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO CXLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CXLIST-SYSDATE.
           DISPLAY CXLIST-SCREEN UPON CXLIST-WINDOW-HANDLE.
           MOVE CXLIST-FORM-FIELDS TO WR-BUF.
           MOVE CXLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CXLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CXLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CXLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ****************************************************
       IO-ROUTINE SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPDT1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CXLIST-SCREEN.
           DESTROY CXLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
