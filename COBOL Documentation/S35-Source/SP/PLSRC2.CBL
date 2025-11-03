       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PLSRC2.
       DATE-WRITTEN.    052213.
      *****************************************************************
      *  DESCRIPTION:  CHARGE OFF SOURCE CODE ANALYSIS WITH PBDS
      *
      *  READS EACH BRANCHES LNFILE
      *
      *  REPORTS CHARGE-OFFS WITH CHARGE-OFF DATE RANGE, WITH TOTALS
      *  BY SOURCE CODE, WITH CONVERTED ACCOUNTS BROKEN OUT
      *  SEPARATELY
      *
      * REV:
      *  BLV 030414 SKIP P2'S PER JIM R - WORLD USES THEM FOR NON-FILES
      *  BLF 040120 FIX DATES TO RUN IN BATCH
      *  BLF 130522 NEW REPORT, LIKE PLSRCD BUT BREAK OUT PBD'S IN
      *             TOTALS, WORLD PR# 889
      *  BAH 160203 REMOVED LTFILE, PD0003
      *  20171012 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0424 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *               LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *               WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *               LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *               PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 2018.1017 GLOBAL BWFILE
      * BAH 2018.1217 GLOBAL LNFILE
      * BAH 2019.1007 REMOVED ACCESS-CALL LNFILE
      * BAH 2020.0831 REMOVED RESET ON BR1 
      * BAH 2022.0826 HARD CODED START
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
           "@(#)SP/PLSRC2.CBL S35 02/21/24-13:18:54 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************

       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  SUB                  PIC 999.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  REC-COUNT            PIC 9(13)       VALUE 0.
       01  BR-TOTAL-RECS        PIC 9(13)       VALUE 0.
       01  GRAND-TOTAL-RECS     PIC 9(13)       VALUE 0.

       01  WORK-NO                 PIC 9(6).
       01  WORK-AMT                PIC S9(8)V99.
       01  CLASS-FG                PIC X(3)        VALUE "   ".
           88  CLASS-BEG                           VALUE "BEG".
           88  CLASS-END                           VALUE "END".

       01  TRANS-DATE              PIC 9(8).

      *****TOTALS, SUB 1 BRANCH, SUB 2 GRAND
       01  TOTAL-TABLE OCCURS 2.
           03  TOT-NO-PB                 PIC 9(7)     COMP-3.
           03  TOT-AMT-PB                PIC S9(9)V99 COMP-3.
           03  TOT-NO-PB-CNV             PIC 9(7)     COMP-3.
           03  TOT-AMT-PB-CNV            PIC S9(9)V99 COMP-3.
           03  TOT-NO-PBD                PIC 9(7)     COMP-3.
           03  TOT-AMT-PBD               PIC S9(9)V99 COMP-3.
           03  TOT-NO-PBD-CNV            PIC 9(7)     COMP-3.
           03  TOT-AMT-PBD-CNV           PIC S9(9)V99 COMP-3.
           03  TOT-NO-FB                 PIC 9(7)     COMP-3.
           03  TOT-AMT-FB                PIC S9(9)V99 COMP-3.
           03  TOT-NO-FB-CNV             PIC 9(7)     COMP-3.
           03  TOT-AMT-FB-CNV            PIC S9(9)V99 COMP-3.
           03  TOT-NO-NB                 PIC 9(7)     COMP-3.
           03  TOT-AMT-NB                PIC S9(9)V99 COMP-3.
           03  TOT-NO-NB-CNV             PIC 9(7)     COMP-3.
           03  TOT-AMT-NB-CNV            PIC S9(9)V99 COMP-3.

           03  TOT-NO-PB-ALL             PIC 9(7)     COMP-3.
           03  TOT-NO-PBD-ALL            PIC 9(7)     COMP-3.
           03  TOT-AMT-PB-ALL            PIC S9(9)V99 COMP-3.
           03  TOT-AMT-PBD-ALL            PIC S9(9)V99 COMP-3.
           03  TOT-NO-FB-ALL             PIC 9(7)     COMP-3.
           03  TOT-AMT-FB-ALL            PIC S9(9)V99 COMP-3.
           03  TOT-NO-NB-ALL             PIC 9(7)     COMP-3.
           03  TOT-AMT-NB-ALL            PIC S9(9)V99 COMP-3.
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-PLDATE      PIC 9(8).
           03  END-PLDATE      PIC 9(8).
           03  DETAIL-YN       PIC X.
               88 DETAIL-YN-VALID  VALUE "Y" "N".
               88 DETAIL-YES       VALUE "Y".
               88 DETAIL-NO        VALUE "N".
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "B".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(24) VALUE SPACES.

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(52)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(28)    VALUE " ".
           03  FILLER           PIC X(31)    VALUE
           "CHARGE OFF SOURCE CODE ANALYSIS".
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH:".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X.
           03  H-BRNAME         PIC X(30).
           03  FILLER           PIC X(87).

       01  HEAD4.
           03  FILLER           PIC X(48)
               VALUE "ACCOUNT NO.       BORROWER NAME".
           03  FILLER           PIC X(15)
               VALUE "SRCD  CONVERT".
           03  FILLER           PIC X(10)
               VALUE " BALANCE  ".
           03  FILLER           PIC X(12)
               VALUE "     PL DATE".
           03  FILLER           PIC X(18)
               VALUE "        PL AMOUNT".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-ACCTNO         PIC X(16).
           03  FILLER           PIC X(2).
           03  D-NAME           PIC X(30).
           03  FILLER           PIC X(2).
           03  D-SRCD           PIC X(3).
           03  FILLER           PIC X(4).
           03  D-CONVERCD       PIC X(1).
           03  FILLER           PIC X(5).
           03  D-BALANCE        PIC ZZZ,ZZ9.99.
           03  FILLER           PIC X(5).
           03  D-PLDATE         PIC 99/99/99.
           03  FILLER           PIC X(5).
           03  D-PLAMT          PIC ZZZ,ZZ9.99-.
       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-GRAND.
               05  D-TOTAL-BRNO PIC Z(4).
               05  FILLER       PIC X(1).
           03  D-TOT-PROMPT-CON PIC X(6).
           03  D-TOT-PROMPT-PBD PIC X(3).
           03  D-TOT-NO-PBD     PIC ZZZZZ9.
           03  D-TOT-AMT-PBD    PIC ZZZZZ,ZZ9.99-.
           03  FILLER           PIC X(1).
           03  D-TOT-PROMPT-PB  PIC X(3).
           03  D-TOT-NO-PB      PIC ZZZ,ZZ9.
           03  D-TOT-AMT-PB     PIC ZZZZZZ,ZZ9.99-.
           03  FILLER           PIC X(1).
           03  D-TOT-PROMPT-FB  PIC X(3).
           03  D-TOT-NO-FB      PIC ZZZZZ9.
           03  D-TOT-AMT-FB     PIC ZZZZZ,ZZ9.99-.
           03  FILLER           PIC X(1).
           03  D-TOT-PROMPT-NB  PIC X(3).
           03  D-TOT-NO-NB      PIC ZZZ,ZZ9.
           03  D-TOT-AMT-NB     PIC ZZZZZZ,ZZ9.99-.
           03  FILLER           PIC X(1).
           03  D-TOT-PROMPT-ALL PIC X(3).
           03  D-TOT-NO-ALL     PIC ZZZ,ZZ9.
           03  FILLER           PIC X(1).
           03  D-TOT-AMT-ALL    PIC ZZZZZZ,ZZ9.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(40).
           03  RANGE1NX REDEFINES RANGE1.
               05  RANGE1N      PIC Z(9)9.
               05  FILLER       PIC X(30).
           03  RANGE1DX REDEFINES RANGE1.
               05  RANGE1D      PIC 9999/99/99.
               05  FILLER       PIC X(30).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PLSRC2_DEF.CPY".
       COPY "LIBSP/PLSRC2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PLSRC2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ****************************************************
       MAIN-MODULE SECTION.
      ****************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PLSRC2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
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
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO BW-PATH-OVERRIDE
                              LN-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE 0 TO BR-TOTAL-RECS.

           MOVE BR-NO   TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BR
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

           ADD 1 TO TOTAL-RECORDS.


      * TEST DELETED LOAN, RESCISSION
      
           IF LN-POCD-NONLOAN
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * SKIP IF ACCOUNT IS NOT PL & SKIP P2'S (WORLD'S NON-FILES)
      
           IF LN-PLDATE = 0
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-PLTYPE = "2"
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * SKIP IF ACCOUNT WAS TRANSFERRED
      
           IF LN-POCD = "BT"
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * TEST PLDATE 
      
           IF LN-PLDATE < BEG-PLDATE OR LN-PLDATE > END-PLDATE
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           PERFORM GET-SP.

           PERFORM GET-SRCD-SUB.

           PERFORM CREATE-DETAIL.

           ADD 1 TO REC-COUNT BR-TOTAL-RECS GRAND-TOTAL-RECS.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           MOVE 1 TO SUB.
           PERFORM PRINT-TOTALS.
           PERFORM CLEAR-TOTALS.
           MOVE ZEROES TO REC-COUNT BR-TOTAL-RECS.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           MOVE 2 TO SUB.
           PERFORM PRINT-TOTALS.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE PLSRC2-QUEUE-POS TO Q-POS.
           MOVE PLSRC2-COPIES-POS TO C-POS.
           MOVE PLSRC2-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE 0 TO TOTAL-RECORDS TOTAL-BYPASSED
                     GRAND-TOTAL-RECS .

           MOVE 1 TO SUB.
           PERFORM CLEAR-TOTALS.
           MOVE 2 TO SUB.
           PERFORM CLEAR-TOTALS.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************
           MOVE EXT-EOBUF-DATE1 TO BEG-PLDATE.
           MOVE EXT-EOBUF-DATE2 TO END-PLDATE.

      ******************************************************************
       CREATE-DETAIL SECTION.
      ******************************************************************

           IF DETAIL-NO
              GO TO ADD-TOTALS.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-ACCTNO.

           MOVE LN-SRCD TO D-SRCD.

           IF SRCD-IS-PBD
              MOVE "PBD" TO D-SRCD.

           MOVE LN-CONVERCD TO D-CONVERCD.

           PERFORM PROCESS-BORROWER.

           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PLDATE.
           MOVE LN-PLAMT TO D-PLAMT.
           MOVE LN-CURBAL TO D-BALANCE.

           PERFORM WRITE-DETAIL-LINE.

       ADD-TOTALS.

           IF LN-CONVERCD NOT = " "
              IF LN-SRCD = "PB"
                 IF SRCD-IS-PBD
                   ADD 1        TO TOT-NO-PBD-CNV(1) TOT-NO-PBD-CNV(2)
                   ADD LN-PLAMT TO TOT-AMT-PBD-CNV(1) TOT-AMT-PBD-CNV(2)
                 ELSE
                   ADD 1        TO TOT-NO-PB-CNV(1) TOT-NO-PB-CNV(2)
                   ADD LN-PLAMT TO TOT-AMT-PB-CNV(1) TOT-AMT-PB-CNV(2)
                 END-IF
              ELSE IF LN-SRCD = "FB"
                 ADD 1        TO TOT-NO-FB-CNV(1) TOT-NO-FB-CNV(2)
                 ADD LN-PLAMT TO TOT-AMT-FB-CNV(1) TOT-AMT-FB-CNV(2)
              ELSE
                 ADD 1        TO TOT-NO-NB-CNV(1) TOT-NO-NB-CNV(2)
                 ADD LN-PLAMT TO TOT-AMT-NB-CNV(1) TOT-AMT-NB-CNV(2)
              END-IF
           ELSE
              IF LN-SRCD = "PB"
                 IF SRCD-IS-PBD
                    ADD 1        TO TOT-NO-PBD(1) TOT-NO-PBD(2)
                    ADD LN-PLAMT TO TOT-AMT-PBD(1) TOT-AMT-PBD(2)
                 ELSE
                    ADD 1        TO TOT-NO-PB(1) TOT-NO-PB(2)
                    ADD LN-PLAMT TO TOT-AMT-PB(1) TOT-AMT-PB(2)
                 END-IF
              ELSE IF LN-SRCD = "FB"
                 ADD 1        TO TOT-NO-FB(1) TOT-NO-FB(2)
                 ADD LN-PLAMT TO TOT-AMT-FB(1) TOT-AMT-FB(2)
              ELSE
                 ADD 1        TO TOT-NO-NB(1) TOT-NO-NB(2)
                 ADD LN-PLAMT TO TOT-AMT-NB(1) TOT-AMT-NB(2).

           IF LN-SRCD = "PB"
              IF SRCD-IS-PBD
                 ADD 1        TO TOT-NO-PBD-ALL(1) TOT-NO-PBD-ALL(2)
                 ADD LN-PLAMT TO TOT-AMT-PBD-ALL(1) TOT-AMT-PBD-ALL(2)
              ELSE
                 ADD 1        TO TOT-NO-PB-ALL(1) TOT-NO-PB-ALL(2)
                 ADD LN-PLAMT TO TOT-AMT-PB-ALL(1) TOT-AMT-PB-ALL(2)
              END-IF
           ELSE IF LN-SRCD = "FB"
              ADD 1        TO TOT-NO-FB-ALL(1) TOT-NO-FB-ALL(2)
              ADD LN-PLAMT TO TOT-AMT-FB-ALL(1) TOT-AMT-FB-ALL(2)
           ELSE
              ADD 1        TO TOT-NO-NB-ALL(1) TOT-NO-NB-ALL(2)
              ADD LN-PLAMT TO TOT-AMT-NB-ALL(1) TOT-AMT-NB-ALL(2).

       EXIT-CREATE-DETAIL.
           EXIT.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DETAIL-YN.
       BR-01.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO BEGCLS.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO BEGCLS.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *----------------------------------------------------------------
       BEGCLS.
           MOVE "ENNN020CLASS1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO BEGCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGCLS.
           MOVE VDUNUM0 TO BEG-CLASS.

           IF BEG-CLASS = 0
              GO TO BEGCLS.

      *----------------------------------------------------------------
       ENDCLS.
           MOVE "ENNN020CLASS2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BEGCLS.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO ENDCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDCLS.
           MOVE VDUNUM0 TO END-CLASS.

           IF END-CLASS = 0
              GO TO ENDCLS.

           GO TO ENTER-BEG-PLDATE.

      *-----------------------------------------------------------------
       ALLCLS.
           MOVE "SNNN020CLASS1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
      * ENTER BEGINNING PLDATE
      *-----------------------------------------------------------------
       ENTER-BEG-PLDATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-DETAIL-YN.

           MOVE "E NM060PLDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDCLS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BEG-PLDATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PLDATE.

           IF BEG-PLDATE > TRANS-DATE
              MOVE "FUTURE DATING IS NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PLDATE.

      *-----------------------------------------------------------------
      * ENTER ENDING PLDATE
      *-----------------------------------------------------------------
       ENTER-END-PLDATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-DETAIL-YN.
           MOVE "E NM060PLDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-PLDATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-END-PLDATE.
           MOVE VDU-DATE-YYYYMMDD TO END-PLDATE.

           IF END-PLDATE > TRANS-DATE
              MOVE "FUTURE DATING IS NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-PLDATE.

           IF BEG-PLDATE > END-PLDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-PLDATE.

      *-----------------------------------------------------------------
      * DETAIL Y/N
      *-----------------------------------------------------------------
       ENTER-DETAIL-YN.
           MOVE "E  A010DETAIL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-END-PLDATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-DETAIL-YN.
           MOVE VDUBUF TO DETAIL-YN.
           IF NOT DETAIL-YN-VALID
              GO TO ENTER-DETAIL-YN.
       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020CLASS1." TO VDU
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CLASS2." TO VDU
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000DETAIL." TO VDU
           MOVE DETAIL-YN TO VDUBUF.
           PERFORM SCREENIO.

      *************************************
       PRINT-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           IF SUB = 1
              MOVE BR-NO TO D-TOTAL-BRNO
           ELSE
              MOVE "TOTAL" TO D-TOTAL-GRAND.
           MOVE "NONC:" TO D-TOT-PROMPT-CON.
           MOVE "PBD" TO D-TOT-PROMPT-PBD.
           MOVE TOT-NO-PBD(SUB) TO D-TOT-NO-PBD.
           MOVE TOT-AMT-PBD(SUB) TO D-TOT-AMT-PBD.
           MOVE "PB:" TO D-TOT-PROMPT-PB.
           MOVE TOT-NO-PB(SUB) TO D-TOT-NO-PB.
           MOVE TOT-AMT-PB(SUB) TO D-TOT-AMT-PB.
           MOVE "FB:" TO D-TOT-PROMPT-FB.
           MOVE TOT-NO-FB(SUB) TO D-TOT-NO-FB.
           MOVE TOT-AMT-FB(SUB) TO D-TOT-AMT-FB.
           MOVE "NB:" TO D-TOT-PROMPT-NB.
           MOVE TOT-NO-NB(SUB) TO D-TOT-NO-NB.
           MOVE TOT-AMT-NB(SUB) TO D-TOT-AMT-NB.
           MOVE "ALL:" TO D-TOT-PROMPT-ALL.
           ADD TOT-NO-PBD(SUB)
               TOT-NO-PB(SUB)
               TOT-NO-FB(SUB)
               TOT-NO-NB(SUB) GIVING WORK-NO.
           MOVE WORK-NO TO D-TOT-NO-ALL.
           ADD TOT-AMT-PBD(SUB)
               TOT-AMT-PB(SUB)
               TOT-AMT-FB(SUB)
               TOT-AMT-NB(SUB) GIVING WORK-AMT.
           MOVE WORK-AMT TO D-TOT-AMT-ALL.
           PERFORM WRITE-DETAIL-LINE.

           ADD TOT-NO-PBD-CNV(SUB)
               TOT-NO-PB-CNV(SUB)
               TOT-NO-FB-CNV(SUB)
               TOT-NO-NB-CNV(SUB) GIVING WORK-NO.
           ADD TOT-AMT-PBD-CNV(SUB)
               TOT-AMT-PB-CNV(SUB)
               TOT-AMT-FB-CNV(SUB)
               TOT-AMT-NB-CNV(SUB) GIVING WORK-AMT.
           IF WORK-NO = 0
              IF WORK-AMT = 0
                 GO TO EXIT-PRINT-TOTALS.

           MOVE "CONV:" TO D-TOT-PROMPT-CON.
           MOVE "PBD" TO D-TOT-PROMPT-PBD.
           MOVE TOT-NO-PBD-CNV(SUB) TO D-TOT-NO-PBD.
           MOVE TOT-AMT-PBD-CNV(SUB) TO D-TOT-AMT-PBD.
           MOVE "PB:" TO D-TOT-PROMPT-PB.
           MOVE TOT-NO-PB-CNV(SUB) TO D-TOT-NO-PB.
           MOVE TOT-AMT-PB-CNV(SUB) TO D-TOT-AMT-PB.
           MOVE "FB:" TO D-TOT-PROMPT-FB.
           MOVE TOT-NO-FB-CNV(SUB) TO D-TOT-NO-FB.
           MOVE TOT-AMT-FB-CNV(SUB) TO D-TOT-AMT-FB.
           MOVE "NB:" TO D-TOT-PROMPT-NB.
           MOVE TOT-NO-NB-CNV(SUB) TO D-TOT-NO-NB.
           MOVE TOT-AMT-NB-CNV(SUB) TO D-TOT-AMT-NB.
           MOVE "ALL:" TO D-TOT-PROMPT-ALL.
           MOVE WORK-NO TO D-TOT-NO-ALL.
           MOVE WORK-AMT TO D-TOT-AMT-ALL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL :" TO D-TOT-PROMPT-CON.
           MOVE "PBD" TO D-TOT-PROMPT-PBD.
           MOVE TOT-NO-PBD-ALL(SUB) TO D-TOT-NO-PBD.
           MOVE TOT-AMT-PBD-ALL(SUB) TO D-TOT-AMT-PBD.
           MOVE "PB:" TO D-TOT-PROMPT-PB.
           MOVE TOT-NO-PB-ALL(SUB) TO D-TOT-NO-PB.
           MOVE TOT-AMT-PB-ALL(SUB) TO D-TOT-AMT-PB.
           MOVE "FB:" TO D-TOT-PROMPT-FB.
           MOVE TOT-NO-FB-ALL(SUB) TO D-TOT-NO-FB.
           MOVE TOT-AMT-FB-ALL(SUB) TO D-TOT-AMT-FB.
           MOVE "NB:" TO D-TOT-PROMPT-NB.
           MOVE TOT-NO-NB-ALL(SUB) TO D-TOT-NO-NB.
           MOVE TOT-AMT-NB-ALL(SUB) TO D-TOT-AMT-NB.
           MOVE "ALL:" TO D-TOT-PROMPT-ALL.
           ADD TOT-NO-PBD-ALL(SUB)
               TOT-NO-PB-ALL(SUB)
               TOT-NO-FB-ALL(SUB)
               TOT-NO-NB-ALL(SUB) GIVING WORK-NO.
           MOVE WORK-NO TO D-TOT-NO-ALL.
           ADD TOT-AMT-PBD-ALL(SUB)
               TOT-AMT-PB-ALL(SUB)
               TOT-AMT-FB-ALL(SUB)
               TOT-AMT-NB-ALL(SUB) GIVING WORK-AMT.
           MOVE WORK-AMT TO D-TOT-AMT-ALL.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-TOTALS.
           EXIT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP                 :" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH             :" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                :" TO RANGE-SELECTION
              MOVE END-BR TO RANGE1N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CHARGE OFF DATE    :" TO RANGE-SELECTION.
           MOVE BEG-PLDATE TO RANGE1D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING CHARGE OFF DATE       :" TO RANGE-SELECTION.
           MOVE END-PLDATE TO RANGE1D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DETAIL (Y/N)                 :" TO RANGE-SELECTION.
           MOVE DETAIL-YN TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

      *    TOTALS
      *    ------
           MOVE "TOTAL RECORDS READ           :" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE   :" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************
       HEAD-PAGE SECTION.
      ******************************************
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
           IF DETAIL-YES
              MOVE HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD4 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 6 TO LN-COUNT.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************
       CLASS-SCAN SECTION.
      ******************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
             MOVE "SNNN020CLASS1." TO VDU
           ELSE
             MOVE "SNNN020CLASS2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************
       PROCESS-BORROWER SECTION.
      ******************************************

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
               MOVE "**NOT ON FILE**" TO D-NAME
               GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      **************************************************************************
      * THIS LOGIC IS JUST LIKE LIBLP/LPSRCD, EXCEPT I STARRED OF
      * THE LINE THAT CHECKS FOR LN-PLCD = " ".  THESE ACCOUNTS MAY BE
      * CHARGED OFF, BUT WEREN'T CHARGED OFF AS OF THE BEG DATE, & WE
      * WANT TO KNOW IF THEY WERE PBD'S, DON'T CARE ABOUT PL SOURCE CODES  BARB
      **************************************************************************
       GET-SRCD-SUB SECTION.

           MOVE 0 TO SRCD.
      *    IF LN-PLCD = " "
              MOVE "N" TO SRCD-FG
              PERFORM GET-SRCD-SUB-NO-PL
              GO TO EXIT-GET-SRCD.

           MOVE 1 TO SRCD.
           IF LN-PLTYPE = "2" MOVE 3 TO SRCD
              ELSE IF LN-PLTYPE = "3" MOVE 5 TO SRCD
              ELSE IF LN-PLTYPE = "4" MOVE 7 TO SRCD
              ELSE IF LN-PLTYPE = "5" MOVE 9 TO SRCD.
           IF LN-PLCD NOT = "P"
              ADD 1 TO SRCD.
           MOVE "Y" TO SRCD-FG.

       EXIT-GET-SRCD.
           EXIT.

      *********************************************
       GET-SRCD-SUB-NO-PL SECTION.
      *********************************************

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE 2 TO SRCD
                 GO TO GET-SRCD-SUB-EXIT
              ELSE
                 IF LN-SRCD = "RF"
                    MOVE 9 TO SRCD
                    GO TO GET-SRCD-SUB-EXIT.

           IF LN-SRCD = "NB"
              MOVE 1 TO SRCD
           ELSE
           IF LN-SRCD = "RF"
              MOVE 2 TO SRCD
           ELSE
           IF LN-SRCD = "FB"
              MOVE 3 TO SRCD
           ELSE
           IF LN-SRCD = "PB"
              PERFORM GET-SRCD-PB
           ELSE
           IF LN-SRCD = "RN"
              MOVE 7 TO SRCD
           ELSE
           IF LN-SRCD = "DM" AND GP-NO-SC-USE-DM = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "SC" AND GP-NO-SC-USE-DM NOT = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "RB"
              MOVE 10 TO SRCD
           ELSE
           IF LN-SRCD = "RO"
              MOVE 11 TO SRCD
           ELSE
              MOVE 9 TO SRCD.
       GET-SRCD-SUB-EXIT.
           EXIT.

      *-------------------------------------------------------
      *    GET SRCD INDEX FOR PB
      *    DETERMINE IF ACCOUNT IS 'PB' 'PBD' OR 'PBI'
      *
      * NOTE: ACCOUNTS WILL BE CATAGORIZED AS
      *       'PB DELINQ' IF BOTH CATAGORIES APPLY.
      * 9/25/97 - FOR WORLD: IF SP-PBINCR-PERCENT = 1, JUST
      *           CHECK IF LNAMT IS > PRIOR FOR SRCD OF 6
      *-------------------------------------------------------
       GET-SRCD-PB SECTION.
           MOVE 4 TO SRCD.
           IF SP-PBDEL-DAYS NOT = 0
              IF LN-PRPDUE-DAYS > SP-PBDEL-DAYS
      * PBD
                 MOVE 5 TO SRCD
                 GO TO GET-SRCD-PB-EXIT.
           IF SP-PBINCR-PERCENT NOT = 0
              IF SP-PBINCR-PERCENT = 1
                 IF LN-LNAMT > LN-PRLNAMT
      * PBI
                    MOVE 6 TO SRCD
                 END-IF
              ELSE
                 COMPUTE SRCD-WORKER ROUNDED =
                     LN-PRLNAMT - 0.01
                         + LN-PRLNAMT * SP-PBINCR-PERCENT / 100
                 IF LN-LNAMT > SRCD-WORKER
                    MOVE 6 TO SRCD.

       GET-SRCD-PB-EXIT.
           EXIT.

       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/GPENV.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO PLSRC2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PLSRC2-SYSDATE.
           DISPLAY PLSRC2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PLSRC2-FORM-FIELDS TO WR-BUF.
           MOVE PLSRC2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE PLSRC2-HELP--FILE TO HELP-LINK-FILE.
           MOVE PLSRC2-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PLSRC2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

       CLEAR-TOTALS.
           MOVE 0 TO TOT-NO-PB(SUB)
                     TOT-AMT-PB(SUB)
                     TOT-NO-PB-CNV(SUB)
                     TOT-AMT-PB-CNV(SUB)
                     TOT-NO-PBD(SUB)
                     TOT-AMT-PBD(SUB)
                     TOT-NO-PBD-CNV(SUB)
                     TOT-AMT-PBD-CNV(SUB)
                     TOT-NO-FB(SUB)
                     TOT-AMT-FB(SUB)
                     TOT-NO-FB-CNV(SUB)
                     TOT-AMT-FB-CNV(SUB)
                     TOT-NO-NB(SUB)
                     TOT-AMT-NB(SUB)
                     TOT-NO-NB-CNV(SUB)
                     TOT-AMT-NB-CNV(SUB)
                     TOT-NO-PB-ALL(SUB)
                     TOT-AMT-PB-ALL(SUB)
                     TOT-NO-PBD-ALL(SUB)
                     TOT-AMT-PBD-ALL(SUB)
                     TOT-NO-FB-ALL(SUB)
                     TOT-AMT-FB-ALL(SUB)
                     TOT-NO-NB-ALL(SUB)
                     TOT-AMT-NB-ALL(SUB).

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PLSRC2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
