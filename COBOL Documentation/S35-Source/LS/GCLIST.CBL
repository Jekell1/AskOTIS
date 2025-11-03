       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GCLIST.
       DATE-WRITTEN.  01-27-1984.
      ******************************************************************
      *
      * IDENTIFICATION:  LS/GCLIST
      *
      * DESCRIPTION   :  FEECODE FILE LISTING (CD-FILE 'GC')
      *
      *-----------------------------------------------------------------
      * REV.
      * GLF 970923 ADDED GIFTNET FIELDS FOR WORLD
      * KEC 020619 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED 'CD-GC-FEE-MAXAMT'.
      * BAH 030425 ADDED CD-GC-ALT-GLFEENO FOR LENDMARK PR# 2048
      * BLV 030701 ADDED VALIDATION OF TRAILER RECORD (CTFILE) IF
      *            WCBC FLAG IS TURNED ON;  SEND MESS IF INVALID,
      *            WORLD PR# 323
      * BLM 090519 ADDED FEE CODE RANGE, WORLD PR# 636.  TO TRY TO
      *            HELP WHEN DELETING FEE CODES WITH A RANGE.
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      *         CHANGED CD-POOL???? TO CD-PL-????
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * BAH 170223 NO RECORDS FOUND, REMOVED CDTYPE NON DISPLAY OF "GC'
      *            WAS NOT FOUND ANYMORE, HARD CODE "GC" AS TYPE.
      *
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

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GCLIST.CBL S34 06/25/21-13:34:09 >".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       01  PR-IFN               PIC X(11)    VALUE "0APRINT".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  CD1             PIC X(2).
           03  CD2             PIC X(2).
           03  GL-YN           PIC X.

       01  ACCEPT-BUF           PIC X        VALUE " ".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  FILLER           PIC X(31)    VALUE 
                                "G/L FEE FILE LISTING".
           03  FILLER           PIC X(45)    VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(18)    VALUE " ".
           03  FILLER           PIC X(4)     VALUE "CODE".
           03  FILLER           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(11)    VALUE "DESCRIPTION".
           03  FILLER           PIC X(7)     VALUE " ".
           03  HEAD3-BR         PIC X(6)     VALUE " ".
           03  FILLER           PIC X(2)     VALUE " ".
           03  HEAD3-GL         PIC X(11)    VALUE " ".
           03  FILLER           PIC X(3)     VALUE " ".
           03  FILLER           PIC X(3)     VALUE "F/C".
           03  FILLER           PIC X(3)     VALUE " ".
           03  FILLER           PIC X(4)     VALUE "WCBC".
           03  FILLER           PIC X(2)     VALUE " ".
           03  FILLER           PIC X(6)     VALUE "PRODNO".
           03  FILLER           PIC X(3)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PRICE".
           03  FILLER           PIC X(3)     VALUE " ".
           03  FILLER           PIC X(8)     VALUE "MAX AMT.".
           03  FILLER           PIC X(3)     VALUE " ".
           03  FILLER           PIC X(8)     VALUE "ALT GLNO".


      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(18).
           03  D-CODE           PIC XXX.
           03  FILLER           PIC X(9).
           03  D-GCDESC         PIC X(15).
           03  FILLER           PIC X(3).
           03  D-BRANCH         PIC 9(4).
           03  FILLER           PIC X(6).
           03  D-GLNUM          PIC 9(7).
           03  FILLER           PIC X(6).
           03  D-GC-GLFEEFINCHG PIC X.
           03  FILLER           PIC X(6).
           03  D-GC-GIFTNET     PIC X.
           03  FILLER           PIC X(3).
           03  D-GC-FEE-PRODNO  PIC Z(5).
           03  FILLER           PIC X(3).
           03  D-GC-FEE-PRICE   PIC Z(4).99 BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-GC-FEE-MAXAMT  PIC Z(4).99 BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-GC-ALT-GLFEENO  PIC 9(7) BLANK ZERO.
           03  FILLER           PIC X(15).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(22).
           03  RANGE-SELECTION      PIC X(16).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GCLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GCLIST_DEF.CPY".
       COPY "LIBLS/GCLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GCLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GCLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GCLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-ST.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF CDB-TYPE NOT = "GC"
              GO TO END-ROUTINE.

           IF CDB-BRANCH < BEG-BR OR CDB-BRANCH > END-BR
              GO TO NEXT-ST.

           IF CDB-CODE < CD1 OR CDB-CODE > CD2
              GO TO NEXT-ST.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-ST.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO ERRCD.

       SETRP.
           MOVE GCLIST-QUEUE-POS TO Q-POS.
           MOVE GCLIST-COPIES-POS TO C-POS.
           MOVE GCLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.

           IF STAT NOT = "00"
              GO TO ENDIT.

      *----------------------------------------------------------------
       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BR.

      *---------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO ENTER-BEGCD.

      *--------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.

      *--------------------------------------------------------------
       ENTER-BEGCD.
           MOVE "E  A020CD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-CODES.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-BEGCD.
           MOVE VDUBUF TO CD1.

      *-----------------------------------------------------------------
       ENTER-ENDCD.
           MOVE "E  A020CD2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ENTER-BEGCD.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ENDCD.
           MOVE VDUBUF TO CD2.
           GO TO ENTER-GL.

      *-----------------------------------------------------------------
       ALL-CODES.
           MOVE "S  A000CD1." TO VDU.
           MOVE SPACES TO CD1 VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000CD2." TO VDU.
           MOVE ALL "Z" TO CD2.
           CALL "C$TOLOWER" USING CD2, VALUE 2.
           MOVE CD2 TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-GL.
           MOVE "E  A010WCBC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ENTER-ENDCD.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-GL.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO ENTER-GL.
           MOVE VDUBUF TO GL-YN.

      *-----------------------------------------------------------------

       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1O" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ENTER-GL.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.


           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-CDB1-FILE.
           MOVE "GC"   TO CDB-TYPE
                          QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE.
           MOVE BEG-BR TO CDB-BRANCH
                          QCDB1-WBEG-BRANCH.
           MOVE END-BR TO QCDB1-WEND-BRANCH.
           MOVE CD1    TO CDB-CODE
                          QCDB1-WBEG-CODE.
           MOVE CD2    TO QCDB1-WEND-CODE.

           PERFORM START-CDB1-FILE.
           IF IO-FG = 9
              GO TO ENDIT.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           MOVE CDB-CODE           TO D-CODE.
           MOVE CDB-DESC           TO D-GCDESC.
           MOVE CDB-BRANCH         TO D-BRANCH.
           MOVE CDB-GC-GLFEENO     TO D-GLNUM.
           MOVE CDB-GC-GLFEEFINCHG TO D-GC-GLFEEFINCHG
           MOVE CDB-GC-FEE-GIFTNET TO D-GC-GIFTNET
           MOVE CDB-GC-FEE-MAXAMT  TO D-GC-FEE-MAXAMT
           IF CDB-GC-ALT-GLFEENO NOT = 2020202
              MOVE CDB-GC-ALT-GLFEENO TO D-GC-ALT-GLFEENO.
           IF CDB-GC-FEE-GIFTNET = "Y"
              MOVE CDB-GC-FEE-PRODNO TO D-GC-FEE-PRODNO
              MOVE CDB-GC-FEE-PRICE TO D-GC-FEE-PRICE.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BRANCH" TO HEAD3-BR
           MOVE "G/L NUMBER" TO HEAD3-GL.
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 2 TO LN-FEED.

           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BR             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING BRANCH   " TO RANGE-LINE.
           MOVE END-BR             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING FEE CODE" TO RANGE-LINE.
           MOVE CD1                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING FEE CODE   " TO RANGE-LINE.
           MOVE CD2                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO GCLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GCLIST-SYSDATE.
           DISPLAY GCLIST-SCREEN UPON GCLIST-WINDOW-HANDLE.
           MOVE GCLIST-FORM-FIELDS TO WR-BUF.
           MOVE GCLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GCLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GCLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GCLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

      *-----------------------------------------------------------------
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           DESTROY GCLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
