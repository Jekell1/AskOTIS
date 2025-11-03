       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CKTRLP.
       DATE-WRITTEN.    041000.
      ****************************************************************
      *
      *  DESCRIPTION:   FX
      *
      *        READ TR'S FOR A SPECIFIC DATE & VERIFY THEY HAVE
      *        A CORRESPONDING RECORD IN THE PAYMENT FILE
      *
      * TO WHOEVER:  I KIND OF GOT THIS TO WORK, DEFINITELY GAVE ME
      * SOME BAD ACCOUNTS, IT DID REPORT SOME PROBLEMS THAT WEREN'T
      * REALLY PROBLEMS...I THINK THEY WERE ALL REVERSE PL'S THAT
      * HAD A $ TRAMT IN TR BUT NOT IN LP.....    BARB, 4/18/00
      *
      * REV:
      * BAH 181219 GLOBAL LPFILE
      *
      * KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION FLR
      *               REMOVED GPFILE SINCE NOT USED
      *
      * BAH 190918 REMOVED ACCESS-CALL ON TRPFILE, LPFILE
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 2022.1004 HARD CODED STARTS
      * BAH 2024.1003 SEND-LEGEND WASNT DISPLAYING "IN PROGRESS" DUE TO NOT
      *               USING MESS-BUF S35Q-136
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./FX/CKTRLP.CBL S34 08/10/21-14:33:49 >".

       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  PR-IFN            PIC X(2) VALUE "PR".

       01  BEG-BRANCH             PIC 9999 VALUE 0.
       01  END-BRANCH             PIC 9999 VALUE 0.
       01  BR1                    PIC 9999 VALUE 0.
       01  BR2                    PIC 9999 VALUE 0.
       01  LOAN-TEST-DATE         PIC 9(8).
       01  TRANS-ENTRY-DATE       PIC 9(8).

       01  ENT-GROUP         PIC X(4).
       01  GROUP-FLAG        PIC X.
       01  ACCEPT-BUF        PIC X.

       01  ERRCD           PIC X.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED             PIC 99       VALUE 0.
           03  LN-COUNT            PIC 999      VALUE 99.
           03  LN-WK               PIC 999      VALUE 0.
       01  PAGE-NUMBER             PIC 9(4)     VALUE 0.

       01  MESS-LIST               PIC X(5) VALUE "MESS.".
       01  MESS-BUF                PIC X(40) VALUE SPACES.

       01  COMMENT-LIST            PIC X(12) VALUE "MESS2 MESS3.".
       01  COMMENT-BUF.
           03  C-BUF2              PIC X(40).
           03  C-BUF3              PIC X(40).


      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(25)     VALUE " ".
           03  H-NAME           PIC X(60)    VALUE " ".
           03  FILLER           PIC X(12)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-TITLE          PIC X(55)    VALUE " ".
           03  FILLER           PIC X(13)    VALUE "   FOR DATE: ".
           03  H-TESTDATE       PIC 99/99/99.

       01  HEAD3.
           03  FILLER      PIC X(31)  VALUE
           "BR#   BRANCH PHONE  CL ACCOUNT ".
           03  FILLER      PIC X(55)  VALUE
           "TR DATE   CD  CD  SOC-SEC-NO   NAME                    ".
           03  FILLER      PIC X(46)  VALUE
           "   TR AMT  USERID    ERROR/PROBLEM            ".

       01  DETAIL-LINE       PIC X(132) VALUE SPACES.
       01  D-LINE1          REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC ZZ99.
           03  FILLER        PIC X(2).
           03  D-BR-PHONE    PIC 999/999/9999.
           03  FILLER        PIC X(2).
           03  D-CLASS       PIC 99.
           03  FILLER        PIC X.
           03  D-LOAN-NUM    PIC ZZ9999.
           03  FILLER        PIC X(2).
           03  D-DATE1       PIC 99/99/99.
           03  FILLER        PIC X(6).
           03  D-TP-TRCD     PIC XX.
           03  FILLER        PIC X(2).
           03  D-SSNO        PIC 999/99/9999.
           03  FILLER        PIC X(26).
           03  D-TP-TRAMT    PIC ZZ,ZZZ.ZZ-.
           03  FILLER        PIC X.
           03  D-TP-USERID   PIC X(8).
           03  FILLER        PIC X(2).
           03  D-MESSAGE     PIC X(25).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       01  CKTRLP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/CKTRLP_DEF.CPY".
       COPY "LIBFX/CKTRLP_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/CKTRLP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE "CKTRLP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CKTRLP-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE "TRANSACTION WITH MISSING PAYMENTS OR   " TO C-BUF2.
           MOVE "PAYMENT RECORDS OR WRONG SSNO" TO C-BUF3.
           PERFORM SEND-COMMENT.

           MOVE " " TO ERRCD.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

       GETPR.
           MOVE CKTRLP-QUEUE-POS TO Q-POS.
           MOVE CKTRLP-COPIES-POS TO C-POS.
           MOVE CKTRLP-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-ROUTINE.

       ENTER-BR1.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS = "16" GO TO ENTER-BR-ALL.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BR1
              ELSE
                 MOVE "S  A040BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BR1.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"  TO GROUP-FLAG
              MOVE 0    TO BR1 BEG-BRANCH
              MOVE 9999 TO BR2 END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BR1 BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BR1 TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-BR1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BR2.

           MOVE VDUNUM0 TO BR2 END-BRANCH.

           IF BR1 > BR2
              PERFORM INVALID-RANGE
              GO TO ENTER-BR1.

           GO TO CHECK-BRANCH-SECURITY.

       ENTER-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BR1 BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO BR2 END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO ENTER-BR1.

           MOVE "ENNM060ENTDATE." TO VDU.
           PERFORM SCREENIO.
           MOVE VDU-DATE-YYYYMMDD TO LOAN-TEST-DATE
                                     TRANS-ENTRY-DATE.
       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-BR1.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO END-ROUTINE.
           IF NOT (ACCEPT-BUF = "Y" )
              GO TO ENTER-ACCEPT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           IF ACCEPT-BUF = "P"
              MOVE " " TO H-TITLE.
           MOVE LOAN-TEST-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-TESTDATE.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF GROUP-FLAG = "N"
              MOVE BR1 TO BR-NO
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
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > BR2
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LP-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           PERFORM PROCESS-BRANCH.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************
       PROCESS-BRANCH SECTION.
      ******************************************
           MOVE 99 TO LN-COUNT.

           PERFORM OPEN-TRP1-FILE.
           PERFORM OPEN-LP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE TRANS-ENTRY-DATE    TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE
                                       QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS-BUF.
           PERFORM SEND-LEGEND.

       PROCESS-NEXT-TR.
           PERFORM READ-TRP1-FILE-NEXT.
           IF ( IO-FG = 9 ) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO PROCESS-BRANCH-EXIT.

           IF TP-DATE NOT = TRANS-ENTRY-DATE
              GO TO PROCESS-BRANCH-EXIT.

           PERFORM SEND-PROGRESS-INDICATOR.

      *  SKIP IF TR FROM ANOTHER BR, WILL BE NO LP IN THIS BRANCH
           IF TP-POSTING-BR NOT = BR-NO
              GO TO PROCESS-NEXT-TR.

      *  FIND CORRESPONDING PAYMENT RECORD

           MOVE LP-PATH-OWNBR  TO LP-BRNO
                                  QLP1-WBEG-BRNO
                                  QLP1-WEND-BRNO.
           MOVE TP-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE ALL "9"   TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO

           PERFORM START-LP1-FILE-NOT-GREATER.
       READ-NEXT-PREV-PAYMENT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = TP-ACCTNO)
              MOVE "**LPREC MISSING**" TO D-MESSAGE
              GO TO CREATE-ERROR-LINE.

           IF LP-TRCD NOT = "RV"
              IF LP-TRCD NOT = TP-TRCD
                 GO TO READ-NEXT-PREV-PAYMENT.
           IF LP-TRCD = "RV"
              IF LP-REVTRCD NOT = TP-TRCD
                 GO TO READ-NEXT-PREV-PAYMENT.
           IF LP-TRCD = "RV"
              IF (LP-REVTRCD = "PL" OR "P2" OR "P3")
                 MOVE TP-TRAMT TO LP-TRAMT.

           IF (LP-TRCD = "PL" OR "P2" OR "P3")
              MOVE TP-TRAMT TO LP-TRAMT.

           IF LP-PAYDATE NOT = TP-PAYDATE
              OR TP-TRAMT NOT = LP-TRAMT
              OR TP-APOTH NOT = LP-APOTH
              OR TP-APINT NOT = LP-APINT
              OR TP-APLC NOT = LP-APLC
              OR TP-APCUR NOT = LP-APCUR
                 GO TO READ-NEXT-PREV-PAYMENT.
           IF TP-OTHBAL NOT = LP-OTHBAL
              OR TP-INTBAL NOT = LP-INTBAL
              OR TP-LCBAL NOT = LP-LCBAL
              OR TP-CURBAL NOT = LP-CURBAL
              OR TP-PDTH-DATE NOT = LP-PDTH-DATE
                 GO TO READ-NEXT-PREV-PAYMENT.

           GO TO PROCESS-NEXT-TR.

       CREATE-ERROR-LINE.
           MOVE TP-BRNO           TO D-BRNO.
           MOVE BR-PHONE          TO D-BR-PHONE.
           INSPECT D-BR-PHONE REPLACING ALL "/" BY "-".
           MOVE TP-CLASS          TO D-CLASS.
           MOVE TP-ACCTNO         TO D-LOAN-NUM.

           MOVE TP-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-DATE1.
           MOVE TP-TRCD           TO D-TP-TRCD.
           MOVE TP-SSNO           TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE TP-TRAMT          TO D-TP-TRAMT.
           MOVE TP-USERID         TO D-TP-USERID.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *  GET THE NEXT LOAN
           GO TO PROCESS-NEXT-TR.

       PROCESS-BRANCH-EXIT.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-LP1-FILE.

      *****************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           MOVE BR-NO                  TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE SPACES                 TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/PROGID.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO CKTRLP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CKTRLP-SYSDATE.
           DISPLAY CKTRLP-SCREEN UPON CKTRLP-WINDOW-HANDLE.
           MOVE CKTRLP-FORM-FIELDS TO WR-BUF.
           MOVE CKTRLP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CKTRLP-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKTRLP-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/CKTRLP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS-BUF  TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-COMMENT.
           MOVE COMMENT-BUF TO WR-BUF.
           MOVE COMMENT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CKTRLP-SCREEN.
           DESTROY CKTRLP-WINDOW-HANDLE.
           EXIT PROGRAM.
