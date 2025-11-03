       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LMPURG.
       DATE-WRITTEN.    08-06-97.
      ****************************************************************
      *                      (LP)
      *    DESCRIPTION:  LOAN MEMO FILE PURGE
      *                  ONLY PURGE MEMOS FOR THE PAID OUT LOANS
      *                  SELECTED
      *
      *        ACCOUNT MUST BE PAID OUT (NO BALANCE AND HAVE POCD)
      *        ACCOUNT PAID OUT DATE MUST BE WITHIN DATE RANGE
      *        ACCOUNT PAYOFF MUST BE > THAN 60 DAYS FROM TODAY
      *        ACCOUNT MUST NOT HAVE INSURANCE STILL IN EFFECT
      *
      *
      * REV:
      * GLF 981109 MADE MEMO DATE 2000 COMPATIBLE
      * BAH 170606 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181128 GLOBAL LMFILE
      * BAH 181210 GLOBAL LNFILE
      * BAH 2020.01.20  WALK ACROSS BRANCHES AND BATCHABLE, #1412
      * BAH 2020.02.12 PRINT TOTAL MEMOS PURGED BY ACCOUNT, BRANCH TOTAL
      *                AND GRAND TOTAL, REMOVE ALL OTHER DETAIL
      ****************************************************************
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
           "@(#)LP/LMPURG.CBL S35 02/20/24-14:35:06 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      * NOT READING BORROWER JUST FOR THIS FIELD FOR THE ACCT# DISPLAY
       01  BW-SOLICIT           PIC X.

       01  MESS-LOAN-NUMBER.
           03  FILLER           PIC X(29) VALUE "PROCESSING BRANCH: ".
           03  MESS-BRANCH      PIC 9(4).
      *    03  MESS-FILLER      PIC XX.
      *    03  MESS-CLASS       PIC 999.
      *    03  MESS-DASH        PIC X VALUE "-".
      *    03  MESS-NUMBER      PIC 9(8).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-MEMO           PIC X        VALUE "N".
       01  PURGE-LM-COUNT       PIC 9(15)   COMP-3    VALUE 0.
       01  BRANCH-COUNT         PIC 9(15)   COMP-3    VALUE 0.
       01  GRAND-COUNT          PIC 9(15)   COMP-3    VALUE 0.
       01  PURGE-FLAG           PIC X.
           88  PURGE-OK         VALUE "Y".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  HOLD-SYSDATE        PIC 9(8).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  ACTION          PIC X.
           03  BEG-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  BEG-ACCT        PIC 9(8).
           03  END-BRANCH      PIC 9(4).
           03  END-CLASS       PIC 9(3).
           03  END-ACCT        PIC 9(8).
           03  BEG-PO-DATE     PIC 9(8).
           03  END-PO-DATE     PIC 9(8).
           03  GROUP-FLAG      PIC X(1).
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL      PIC X        VALUE "A".

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
           "- - - LOAN MEMO PURGE REPORT - - -".
           03  FILLER           PIC X(45)    VALUE " ".


       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO                 PIC Z(4).
           03  FILLER                 PIC XX.
           03  D-LOAN-NUM             PIC X(16).
           03  FILLER                 PIC X(5).
           03  D-TOTAL-PURGED         PIC ZZZ,ZZZ,ZZZ,ZZ9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(50).
           03  RANGE-SELECTION.
               04  R-BRANCH     PIC 9(4).
               04  R-GROUP REDEFINES R-BRANCH PIC X(4).
               04  FILLER       PIC XX.
               04  R-CLASS      PIC 99.
               04  FILLER       PIC XX.
               04  R-ACCT       PIC ZZZZZZZZ9.
               04  FILLER       PIC XX.
               04  R-DATE       PIC X(10).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LMPURG_DEF.CPY".
       COPY "LIBLP/LMPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/LMPURG_SCN.CPY".

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
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LMPURG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOCALITY.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO END-MAIN-MODULE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9  OR BR-NO > END-BRANCH
              GO TO END-MAIN-MODULE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LM1-FILE.

           MOVE BR-NO            TO MESS-BRANCH.
           MOVE MESS-LOAN-NUMBER TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MEMO-PURGE-MODULE.

      * BRANCH TOTAL
           IF ACTION NOT = "P"
              IF BRANCH-COUNT NOT = 0
                 MOVE BR-NO             TO D-BRNO
                 MOVE "** BRANCH TOTAL" TO D-LOAN-NUM
                 MOVE BRANCH-COUNT      TO D-TOTAL-PURGED
                 MOVE 3 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO BRANCH-COUNT.

           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

       END-OF-GROUP.

           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LN3-FILE.

           IF ( FIRST-TIME = "N" )
              MOVE "Y" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE ZEROES TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOCALITY.

       END-MAIN-MODULE.
           IF ( NO-RECORDS-FOUND )
              GO TO EXIT-MAIN-MODULE.

           PERFORM PRINT-RANGE-LINES.

       EXIT-MAIN-MODULE.
           GO TO END-PROGRAM.


      *****************************************************
       MEMO-PURGE-MODULE SECTION.
      *****************************************************

           MOVE 0 TO PURGE-LM-COUNT BRANCH-COUNT.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE BEG-ACCT  TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE END-ACCT  TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-MEMO-PURGE-MODULE.

           IF (LN-CLASS < BEG-CLASS) OR (LN-CLASS > END-CLASS)
              GO TO NEXT-LN.

           IF (LN-ACCTNO < BEG-ACCT) OR (LN-ACCTNO > END-ACCT)
              GO TO NEXT-LN.

           PERFORM CHECK-PURGABILITY.
           IF NOT PURGE-OK
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

      * IF PURGE OR BOTH
           PERFORM PURGE-MEMO-PROCESS.

      * PRINT TOTAL MEMOS PURGED PER ACCOUNT
           IF ACTION NOT = "P"
              IF PURGE-LM-COUNT NOT = 0
                 PERFORM LPSTAT-DISPLAY
                 MOVE LN-OWNBR         TO D-BRNO
                 MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM
                 MOVE PURGE-LM-COUNT   TO D-TOTAL-PURGED
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE.


           MOVE "N" TO FIRST-TIME.

           ADD PURGE-LM-COUNT TO BRANCH-COUNT GRAND-COUNT.

           MOVE 0 TO PURGE-LM-COUNT.

           GO TO NEXT-LN.

       EXIT-MEMO-PURGE-MODULE.
           EXIT.

      ******************************************
       CHECK-PURGABILITY SECTION.
      ******************************************
           MOVE "Y" TO PURGE-FLAG.

           IF LN-CURBAL NOT = 0 OR
               LN-INTBAL NOT = 0 OR
                LN-LCBAL NOT = 0 OR
                 LN-OTHBAL NOT = 0
              MOVE "N" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

      * ONLY DELETE PAID OUT ACCOUNTS (OR DELETES-**)
           IF LN-POCD = SPACES
              MOVE "N" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

      * VERIFY BETWEEN BEGINNING AND ENDING DATE
           IF (LN-POFFDATE < BEG-PO-DATE) OR
                  (LN-POFFDATE > END-PO-DATE)
              MOVE "N" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

      * VERIFY PAYOFF IS GREATER THAN 2 MONTHS OLD FROM SYSTEM DATE
           MOVE LN-POFFDATE  TO NUM-DATE.
           MOVE HOLD-SYSDATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < 61
              MOVE "N" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

      * TEST FOR INSURANCE STILL IN EFFECT:
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 MOVE "N" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.

           IF LN-INSURED = "N"
              GO TO EXIT-CHECK-PURGABILITY.

      * CREDIT LIFE:
           IF LN-INSPREM(1) NOT = 0
              MOVE 1 TO ITRM-SUB
              MOVE HOLD-SYSDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "N" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.
      * ACCIDENT-HEALTH:
           IF LN-INSPREM(2) NOT = 0
              MOVE 2 TO ITRM-SUB
              MOVE HOLD-SYSDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "N" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.
      * PP INSURANCE:
           IF LN-INSPREM(3) NOT = 0
              MOVE 3 TO ITRM-SUB
              MOVE HOLD-SYSDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "N" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.
      * OTHER INSURANCE:
           IF LN-INSPREM(4) NOT = 0
              MOVE 4 TO ITRM-SUB
              MOVE HOLD-SYSDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "N" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.
      * OTHER INSURANCE:
           IF LN-INSPREM(5) NOT = 0
              MOVE 5 TO ITRM-SUB
              MOVE HOLD-SYSDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "N" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.
       EXIT-CHECK-PURGABILITY.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE LMPURG-QUEUE-POS TO Q-POS.
           MOVE LMPURG-COPIES-POS TO C-POS.
           MOVE LMPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-ERRCD = "E" GO TO ENDIT.

           IF ACTION = "R"
              MOVE "REPORT IN PROGRESS.................." TO WR-BUF.
           IF ACTION = "P"
              MOVE "PURGE IN PROGRESS..................." TO WR-BUF.
           IF ACTION = "B"
              MOVE "PURGE AND REPORT IN PROGRESS........" TO WR-BUF.
           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE "N" TO RECORDS-FOUND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ************************************************
       EO-EXEC-AUDIT SECTION.
      ************************************************
           MOVE EXT-JULIAN-BEG  TO BEG-PO-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-PO-DATE.

      ************************************************
       ENTER-PARAMETERS SECTION.
      ************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DT-02.

       RRACT.
           MOVE "E  A010ACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRACT.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "R" GO TO BR-01.
           IF ACTION = "P" GO TO BR-01.
           IF ACTION = "B" GO TO BR-01.
           GO TO RRACT.
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCHS.

           IF ( F6-KEY )
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

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF (                    UP-KEY ) GO TO RRACT.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"           TO GROUP-FLAG
              MOVE 0             TO BEG-BRANCH
              MOVE 9999          TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
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
           GO TO CL-01.
       ENTER-ALL-BRANCHS.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO BR-01.

       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO AC-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-01.
           MOVE VDUNUM0 TO BEG-ACCT.
       AC-02.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AC-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-02.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE "INVALID ACCOUNT RANGE" TO MESS
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
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO AC-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PO-DATE.
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-PO-DATE.
           GO TO OK-YN.
       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-PO-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-PO-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       OK-YN.
           MOVE 70 TO LN-COUNT.

           IF END-PO-DATE < BEG-PO-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.
       EXIT-PARM.
           EXIT.

      ******************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************

           MOVE "S  A010ACT." TO VDU.
           MOVE ACTION TO VDUBUF.
           PERFORM SCREENIO.

           IF ( GROUP-FLAG = "Y" )
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

           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC1." TO VDU.
           MOVE BEG-ACCT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE END-ACCT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-PO-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE END-PO-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *******************************************
       PURGE-MEMO-PROCESS SECTION.
      *******************************************
           MOVE "Y" TO FIRST-MEMO.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ 
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.
           COMPUTE QLM1-WSBEG-DATE-REV = LM-DATE-REVERSED + 1.
           PERFORM START-LM1-FILE.

       NEXT-MEMO.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO PURGE-MEMOS-EXIT.

           IF LM-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LM-FILE
              GO TO PURGE-MEMOS-EXIT.

           IF ACTION NOT = "R"
              PERFORM DELETE-LM1-FILE.

           ADD 1 TO PURGE-LM-COUNT.

      *    UNLOCK LM-FILE.
           GO TO NEXT-MEMO.


       PURGE-MEMOS-EXIT.
           EXIT.

      *********************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *********************************************
       HEAD-PAGE SECTION.
      *********************************************
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

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 2 TO LN-COUNT.


      ***********************************************
       PRINT-RANGE-LINES SECTION.
      ***********************************************
           MOVE "** GRAND TOTAL"  TO D-LOAN-NUM
           MOVE GRAND-COUNT       TO D-TOTAL-PURGED
           MOVE 3 TO LN-FEED
           PERFORM WRITE-DETAIL-LINE.


           MOVE "BEGINNING BRANCH, CLASS, ACCOUNT #, PAY-OFF DATE:"
              TO RANGE-LINE.
           IF ( GROUP-FLAG = "Y" )
              MOVE ENT-GROUP  TO R-GROUP
           ELSE
              MOVE BEG-BRANCH TO R-BRANCH.
           MOVE BEG-CLASS   TO R-CLASS.
           MOVE BEG-ACCT    TO R-ACCT.
           MOVE BEG-PO-DATE TO R-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH,    CLASS, ACCOUNT #, PAY-OFF DATE:"
              TO RANGE-LINE.
           IF ( GROUP-FLAG = "Y" )
              MOVE SPACES     TO R-GROUP
           ELSE
              MOVE END-BRANCH TO R-BRANCH.
           MOVE END-CLASS   TO R-CLASS.
           MOVE END-ACCT    TO R-ACCT.
           MOVE END-PO-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
       END-RANGE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO LMPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LMPURG-SYSDATE.
           DISPLAY LMPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LMPURG-FORM-FIELDS TO WR-BUF.
           MOVE LMPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LMPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE LMPURG-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LMPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC-PARAGRAPHS SECTION.
      *************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/EMPUMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LMPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
