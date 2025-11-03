       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWLNRP.
       DATE-WRITTEN. 08-06-84
      *******************************************************************
      *                      (LP)
      *  DESCRIPTION:  BORROWER LOAN SUMMARY REPORT
      *  REV:
      *   JFF 100295 ADDED BORROWER SCAN
      *   KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *   BAH 042098 ADDED OPTION FOR NO LOANS ONLY PRINT
      *   BLM 091015 ADDED CLASS RANGE PARAMETERS & OPTION FOR PL'S
      *              (SKIP, INC, ONLY).  TRASHED BRANCHNO PARAMETER,
      *              WAS CONFUSING SINCE PROGRAM DOESN'T CROSS PATHS.
      *              WORLD PR# 656
      *   BLM 091021 ADDED OPTION TO MASK 1ST 5 DIGITS OF SSNO,
      *              WORLD PR# 656
      *   BAH 151130 ADDED BRANCH RANGE AND CROSS DATA PATHS, PACESETTER 4904
      *   BAH 151209 LT-ALL-PATH NEEDED TO BE SET, 4904
      *   BAH 160125 SPLIT OUT LTFILE, PD0003
      *    CS 170317 FIXED 8 DIGIT DATES (LN-LOANDATE, LN-DATE-PAID-LAST)
      *   BAH 181002 GLOBAL BWFILE
      *   BAH 181204 GLOBAL LNFILE
      * BAH 20190919 REMOVED ACCESS-CALL ON BWFILE
      * BAH 20200710 MOVED OPENS TO INITIALIZE AND USE LOADS WHERE OPENS WERE
      *              AND DONT CLOSE. #TST001
      * BAH 20230712 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * RMZ 20230824 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *******************************************************************
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
           "@(#)LP/BWLNRP.CBL S35 11/21/23-15:39:31 >".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      **********************
      *   LINE WORKERS     *
      **********************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01 QWS-WBEG-CLASS        PIC 9(3)     VALUE 0. 
       01 QWS-WEND-CLASS        PIC 9(3)     VALUE 0.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BW1-CONN-OPEN-SW     PIC X     VALUE "N".
       01  BW2-CONN-OPEN-SW     PIC X     VALUE "N".

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  BEG-SS          PIC 9(9).
       01  END-SS          PIC 9(9).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).
       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  SS-FG           PIC X(3)         VALUE "   ".
           88  SS-BEG                       VALUE "BEG".
           88  SS-END                       VALUE "END".
       01  WK-SSNO         PIC 9(9).
       01  WK-SSX REDEFINES WK-SSNO.
           03  WK-SS1      PIC 999.
           03  WK-SS2      PIC 99.
           03  WK-SS3      PIC 9999.

       01  A-LOAN-FOUND            PIC X.
       01  A-DELETED-LOAN-FOUND    PIC X.
       01  NAME-WORKER     PIC X(24).
       01  FIRST-NAME      PIC X(6).
       01  LAST-NAME       PIC X(16).

       01  BAL-WORK        PIC S9(6)V99 COMP-3 VALUE 0.


      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SEQ            PIC 9.
           03  W-BEG-SS       PIC X(11).
           03  W-END-SS       PIC X(11).
           03  BRANCHNO       PIC 9(4).
           03  BALYN          PIC X.
           03  MAKER-ONLY     PIC X.
           03  NOLOANS        PIC X.
           03  BEG-CLASS      PIC 999.
           03  END-CLASS      PIC 999.
           03  PL-FG          PIC X.
           03  MASK-SSNO      PIC X.
           03  BEG-BRANCH     PIC 9(4).
           03  END-BRANCH     PIC 9(4).
           03  GROUP-FLAG     PIC X.
           03  ENT-GROUP      PIC X(4).

       01  VERSION-CONTROL     PIC X  VALUE "D".
       01  SV-BEG-BR           PIC 9(4)  VALUE 9999.
       01  SV-END-BR           PIC 9(4)  VALUE 9999.
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
           " - - - BORROWER/LOAN SUMMARY REPORT - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  FILLER           PIC XX VALUE " ".
           03  H-LOC            PIC 9(4).
           03  FILLER           PIC X      VALUE " ".
           03  H-LOC-NAME       PIC X(20).
           03  FILLER           PIC X(97)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(38)    VALUE " ".
           03  FILLER      PIC X(53)    VALUE
           "M                   LOAN".
           03  FILLER      PIC X(41)    VALUE
           "LOAN                    LAST".

       01  HEAD5.
           03  FILLER      PIC X(38)    VALUE
           "SOC-SEC-NO   BORROWER".
           03  FILLER      PIC X(51)    VALUE
           "K ACCOUNT NUMBER    DATE   DEALER SC CC PC  TRM".
           03  FILLER      PIC X(43)    VALUE
           "AMOUNT PAYMENTS DUE PDTH PAYMNT    BALANCE".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-SSNO      PIC 999/99/9999.
           03  FILLER REDEFINES D-SSNO.
               05  D-MASK  PIC X(7).
               05  D-SS3   PIC 9999.

           03  FILLER      PIC XX.
           03  D-NAME      PIC X(24).
           03  FILLER      PIC X.
           03  D-MAKERCD   PIC X.
           03  FILLER      PIC X.
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-LOANDATE  PIC 9(6).
           03  FILLER      PIC X.
           03  D-DLNO      PIC Z(3)9.99.
           03  FILLER      PIC X.
           03  D-SRCD      PIC XX.
           03  FILLER      PIC X.
           03  D-COLLCD    PIC XX.
           03  FILLER      PIC X.
           03  D-PURCD     PIC X(3).
           03  FILLER      PIC X.
           03  D-ORGTERM   PIC ZZ9.
           03  FILLER      PIC X.
           03  D-LNAMT     PIC Z(5)9.99.
           03  D-REGPYAMT  PIC Z(5)9.99.
           03  FILLER      PIC XX.
           03  D-1STPYDATE PIC 99.
           03  FILLER      PIC X.
           03  D-PDTH      PIC 9(4)     BLANK ZERO.
           03  FILLER      PIC X.
           03  D-LSTPAYMNT PIC 9(6)     BLANK ZERO.
           03  D-OTHINT    REDEFINES D-LSTPAYMNT
                           PIC X(6).
           03  FILLER      PIC XX.
           03  D-BALANCE   PIC Z(5)9.99-.


       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  RANGE-SELECTION  PIC X(16).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWLNRP_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/BWLNRP_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BWLNRP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW2-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWLNRP" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

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

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS................", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"   TO BW-PATH-OVERRIDE
                         LN-PATH-OVERRIDE
                         LTP-PATH-OVERRIDE.

           MOVE 99 TO LN-COUNT.

           PERFORM LOAD-LN4-FILE.
           PERFORM LOAD-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR
                                 QBW1-WBEG-OWNBR
                                 QBW1-WEND-OWNBR.

           IF SEQ = 1
              MOVE BEG-SS TO BW-SSNO
                             QBW1-WBEG-SSNO
              MOVE END-SS TO QBW1-WEND-SSNO
              PERFORM START-BW1-FILE *> - SSP - BWFILE
           ELSE
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE. *> - SSP - BWFILE
       NEXT-BW.
           IF SEQ = 1
              PERFORM READ-BW1-FILE-NEXT *> EMBEDDED - BWFILE
              IF IO-FG = 0
                 IF (BW-SSNO > END-SS) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
                    GO TO END-OF-GROUP.
           IF SEQ = 2
              PERFORM READ-BW2-FILE-NEXT. *> EMBEDDED - BWFILE
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
              GO TO END-OF-GROUP.

           IF MASK-SSNO = "Y"
              MOVE BW-SSNO TO WK-SSNO
              MOVE "XXX-XX-" TO D-MASK
              MOVE WK-SS3 TO D-SS3
           ELSE
              MOVE BW-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE BW-LNAME TO LAST-NAME.
           MOVE BW-FNAME TO FIRST-NAME.
           PERFORM SET-NAME.

           PERFORM DETAIL-LINE-PROCESS-READ.

           IF NOLOANS = "I"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF NOLOANS = "O"
                 IF A-LOAN-FOUND = " "
                    IF A-DELETED-LOAN-FOUND = "Y"
                       MOVE "VOIDED LOAN FOUND" TO D-LOAN-NUM
                       PERFORM WRITE-DETAIL-LINE
                    ELSE
                       PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-BW.
       END-OF-GROUP.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE BWLNRP-QUEUE-POS  TO Q-POS.
           MOVE BWLNRP-COPIES-POS TO C-POS.
           MOVE BWLNRP-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-LN4-FILE.

           IF SEQ = 1
              PERFORM OPEN-BW1-FILE
           ELSE
              PERFORM OPEN-BW2-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **************************************
       EO-EXEC-AUDIT SECTION.
      **************************************
           MOVE W-BEG-SS TO ALP-BW.
           PERFORM ALPBW-TO-NUMBW.
           MOVE NUM-BW TO BEG-SS.
           MOVE W-END-SS TO ALP-BW.
           PERFORM ALPBW-TO-NUMBW.
           MOVE NUM-BW TO END-SS.

      **************************************
       ENTER-PARAMETERS SECTION.
      **************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO MASK-SSNO-YN.

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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       RRSEQ.
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRSEQ.
           MOVE VDUNUM0 TO SEQ.
           IF SEQ = 1 GO TO SS-01.
           IF SEQ < 1 OR SEQ > 2
              GO TO RRSEQ.
           GO TO CLASS-01.
       SS-01.
           MOVE "ERNX110SS1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-SS.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO SS-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
                 GO TO SS-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SS-01.
           MOVE VDUNUM0 TO BEG-SS NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO W-BEG-SS.

       SS-02.
           MOVE "ERNX110SS2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SS-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO SS-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
                 GO TO SS-02.
           MOVE VDUNUM0 TO END-SS NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO W-END-SS.

           IF BEG-SS > END-SS
              MOVE " INVALID BORROWER NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO SS-01.

           GO TO CLASS-01.
       ALL-SS.
           MOVE "S  A110SS1." TO VDU.
           MOVE "000-00-0000" TO W-BEG-SS VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A110SS2." TO VDU.
           MOVE "999-99-9999" TO W-END-SS VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO BEG-SS.
           MOVE 999999999 TO END-SS.
       CLASS-01.
           MOVE "ENNN020BEGCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CLASSES.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CLASS-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CLASS-02.
           MOVE "ENNN020ENDCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CLASS-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CLASS-02.
           MOVE VDUNUM0 TO END-CLASS.
           GO TO RRNOLOANS.
       ALL-CLASSES.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       RRNOLOANS.
           MOVE "E  A010NOLOANS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CLASS-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRNOLOANS.
           MOVE VDUBUF TO NOLOANS.
           IF NOT (NOLOANS = "I" OR "S" OR "O") GO TO RRNOLOANS.
           IF NOLOANS = "O"
              GO TO OK-YN.
       BAL.
           MOVE "E  A010BALYN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RRNOLOANS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BAL.
           MOVE VDUBUF TO BALYN.
           IF BALYN = "Y" GO TO INC-PLS.
           IF BALYN NOT = "N" GO TO BAL.
       INC-PLS.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO INC-PLS.
           MOVE VDUBUF TO PL-FG.
           IF NOT (PL-FG = "I" OR "S" OR "O")
              MOVE "PLEASE ENTER 'I'-INCLUDE, 'S'-SKIP OR 'O'-ONLY"
                TO MESS
              PERFORM SEND-MESS
              GO TO INC-PLS.
       ONLYBW.
           MOVE "E  A010BWONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INC-PLS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ONLYBW.
           MOVE VDUBUF TO MAKER-ONLY.
           IF MAKER-ONLY = "Y"
              GO TO MASK-SSNO-YN.
           IF MAKER-ONLY NOT = "N" GO TO ONLYBW.
       MASK-SSNO-YN.
           MOVE "E  A010MASK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ONLYBW.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO MASK-SSNO-YN.
           MOVE VDUBUF TO MASK-SSNO.
           IF MASK-SSNO = "Y"
              GO TO OK-YN.
           IF MASK-SSNO NOT = "N" GO TO MASK-SSNO-YN.
       OK-YN.

       EXIT-PARM.
           EXIT.

      **************************************
       DISPLAY-PARAMETERS SECTION.
      **************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN010SEQ." TO VDU.
           MOVE SEQ TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S RA110SS1." TO VDU.
           MOVE W-BEG-SS TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S RA110SS2." TO VDU.
           MOVE W-END-SS TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BALYN TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S RA010PL." TO VDU.
           MOVE PL-FG TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S RA010BWONLY." TO VDU.
           MOVE MAKER-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S RA010MASK." TO VDU.
           MOVE MASK-SSNO TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
       DETAIL-LINE-PROCESS-READ SECTION.
      ***************************************
           MOVE " " TO A-LOAN-FOUND
                       A-DELETED-LOAN-FOUND.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE. *> EMBEDDED - LNFILE
       READ-NEXT-SSNO4.
           PERFORM READ-LN4-FILE-NEXT. *> EMBEDDED - LNFILE
           IF (IO-FG = 9) OR (LN-SSNO(1) NOT = BW-SSNO)
                         OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO LN-FILE5.
      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              MOVE "Y" TO A-DELETED-LOAN-FOUND
              GO TO READ-NEXT-SSNO4.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO READ-NEXT-SSNO4.

           IF PL-FG = "O"
              IF LN-PLDATE = 0
                 GO TO READ-NEXT-SSNO4.

           IF PL-FG = "S"
              IF LN-PLDATE NOT = 0
                 GO TO READ-NEXT-SSNO4.

           MOVE 1 TO SUB.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO READ-NEXT-SSNO4.

       LN-FILE5.

           IF MAKER-ONLY = "Y"
              GO TO END-DETAIL-LN-PROCESS-READ.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE. *> EMBEDDED - LNFILE
       READ-NEXT-SSNO5.
           PERFORM READ-LN5-FILE-NEXT. *> EMBEDDED - LNFILE
           IF (IO-FG = 9) OR (LN-SSNO(2) NOT = BW-SSNO)
                         OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       GO TO LN-FILE6.
              GO TO END-DETAIL-LN-PROCESS-READ.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              MOVE "Y" TO A-DELETED-LOAN-FOUND
              GO TO READ-NEXT-SSNO5.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO READ-NEXT-SSNO5.

           IF PL-FG = "O"
              IF LN-PLDATE = 0
                 GO TO READ-NEXT-SSNO5.

           IF PL-FG = "S"
              IF LN-PLDATE NOT = 0
                 GO TO READ-NEXT-SSNO5.

           MOVE 2 TO SUB.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO READ-NEXT-SSNO5.

      *LN-FILE6.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *READ-NEXT-SSNO6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF (IO-FG = 9) OR (LN-SSNO(3) NOT = BW-SSNO)
      *                  OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       GO TO LN-FILE7.

      *    IF ( LN-POCD-NONLOAN )
      *       MOVE "Y" TO A-DELETED-LOAN-FOUND
      *       GO TO READ-NEXT-SSNO6.

      *    IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
      *       GO TO READ-NEXT-SSNO6.

      *    IF PL-FG = "O"
      *       IF LN-PLDATE = 0
      *          GO TO READ-NEXT-SSNO6.

      *    IF PL-FG = "S"
      *       IF LN-PLDATE NOT = 0
      *          GO TO READ-NEXT-SSNO6.

      *    MOVE 3 TO SUB.

      *    PERFORM DETAIL-LINE-PROCESS.

      *    GO TO READ-NEXT-SSNO6.
      *LN-FILE7.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *READ-NEXT-SSNO7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF (IO-FG = 9) OR (LN-SSNO(4) NOT = BW-SSNO)
      *                  OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       GO TO END-DETAIL-LN-PROCESS-READ.

      *    IF ( LN-POCD-NONLOAN )
      *       MOVE "Y" TO A-DELETED-LOAN-FOUND
      *       GO TO READ-NEXT-SSNO7.

      *    IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
      *       GO TO READ-NEXT-SSNO7.

      *    IF PL-FG = "O"
      *       IF LN-PLDATE = 0
      *          GO TO READ-NEXT-SSNO7.

      *    IF PL-FG = "S"
      *       IF LN-PLDATE NOT = 0
      *          GO TO READ-NEXT-SSNO7.

      *    MOVE 4 TO SUB.

      *    PERFORM DETAIL-LINE-PROCESS.

      *    GO TO READ-NEXT-SSNO7.

       END-DETAIL-LN-PROCESS-READ.
           EXIT.

      ************************************
       DETAIL-LINE-PROCESS SECTION.
      ************************************
           MOVE "Y" TO RECORDS-FOUND.
           MOVE "Y" TO A-LOAN-FOUND.
           IF NOLOANS = "O"
              GO TO END-DETAIL-LN-PROCESS.

           COMPUTE BAL-WORK ROUNDED = LN-CURBAL + LN-INTBAL +
                                      LN-LCBAL + LN-OTHBAL.
           IF BALYN = "N"
              IF BAL-WORK = 0
                 GO TO END-DETAIL-LN-PROCESS.

           MOVE LN-MAKERCD(SUB) TO D-MAKERCD.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-LOANDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-LOANDATE.
           MOVE LN-DLNO         TO D-DLNO.
           MOVE LN-SRCD         TO D-SRCD.
           MOVE LN-COLLCD       TO D-COLLCD.
           MOVE LN-PURCD        TO D-PURCD.
           MOVE LN-SUM-ORGTERM  TO D-ORGTERM.
           MOVE LN-SUM-LNAMT    TO D-LNAMT.
           MOVE LN-REGPYAMT     TO D-REGPYAMT.
           MOVE LN-1STPYDA      TO D-1STPYDATE.    

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
      *  PDTH-DATE IS IN MMDD FORMAT IF WEEKLY UNIT PERIODS
           MOVE PDTH-DATE TO D-PDTH.
          
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-LSTPAYMNT.

           MOVE LN-CURBAL       TO D-BALANCE.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-INTBAL NOT = 0
              MOVE "INTBAL"     TO D-OTHINT
              MOVE LN-INTBAL    TO D-BALANCE
              PERFORM WRITE-DETAIL-LINE.
           IF LN-LCBAL NOT = 0
              MOVE "LCBAL"     TO D-OTHINT
              MOVE LN-LCBAL    TO D-BALANCE
              PERFORM WRITE-DETAIL-LINE.
           IF LN-OTHBAL NOT = 0
              MOVE "OTHBAL"     TO D-OTHINT
              MOVE LN-OTHBAL    TO D-BALANCE
              PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
       SET-NAME SECTION.
      ****************************

           MOVE SPACES TO NAME-WORKER.
           MOVE LAST-NAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "      "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
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

           MOVE BR-NO TO H-LOC.
           MOVE BR-NAME TO H-LOC-NAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           IF SEQ = 1
              MOVE "BEGINNING BORROWER" TO RANGE-LINE
              MOVE W-BEG-SS TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BORROWER" TO RANGE-LINE
              MOVE W-END-SS TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BORR'S WITH NO LOANS" TO RANGE-LINE.
           MOVE NOLOANS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ZERO BALANCES" TO RANGE-LINE.
           MOVE BALYN TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PRINT CHARGE-OFFS" TO RANGE-LINE.
           MOVE PL-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "MAKER ONLY" TO RANGE-LINE.
           MOVE MAKER-ONLY TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "MASK 1ST 5 OF SSNO" TO RANGE-LINE.
           MOVE MASK-SSNO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      **************************************
       BORROWER-SCAN SECTION.
      **************************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.

           MOVE BWSCAN-NO TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           IF ( SS-FG = "BEG" )
               MOVE "S  A110SS1." TO VDU
           ELSE
               MOVE "S  A110SS2." TO VDU.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           MOVE ALP-BW TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           MOVE EXT-CONAME-CONAME TO BWLNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWLNRP-SYSDATE.
           DISPLAY BWLNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWLNRP-FORM-FIELDS TO WR-BUF.
           MOVE BWLNRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE BWLNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWLNRP-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWLNRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      **************************************
       MISC SECTION.
      **************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           MOVE NUM-BW123 TO ALP-BW123.
           MOVE NUM-BW45 TO ALP-BW45.
           MOVE NUM-BW6789 TO ALP-BW6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBW-TO-NUMBW.
           MOVE ALP-BW123 TO NUM-BW123.
           MOVE ALP-BW45 TO NUM-BW45.
           MOVE ALP-BW6789 TO NUM-BW6789.

      **************************************
       IO-ROUTINE SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

           IF BW1-CONN-OPEN-SW = "N"
              MOVE "Y" TO BW1-CONN-OPEN-SW 
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.


      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW1-FILE. *> SSP - BWFILE
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

      *  STORED PROCEDURE
      *
      *   SELECT BWFILE.BW_OWNBR,
      *          BWFILE.BW_SSNO,             
      *          BWFILE.BW_LNAME, 
      *          BWFILE.BW_FNAME,
      *          BWFILE.BW_SOLICIT
      *     FROM DBO.BWFILE
      *    WHERE BWFILE.BW_OWNBR       = @QBW1_WBEG_OWNBR
      *      AND BWFILE.BW_SSNO  BETWEEN @QBW1_WBEG_SSNO
      *                              AND @QBW1_WEND_SSNO
      *    ORDER BY BWFILE.BW_OWNBR,
      *             BWFILE.BW_SSNO

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
                :BW1-RETURN-CODE = EXEC SSP_BWLNRP_BW1FILE_SELECT
              ( :QBW1-WBEG-OWNBR,                    
                :QBW1-WBEG-SSNO,  
                :QBW1-WEND-SSNO )
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT. *> EMBEDDED - BWFILE
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO 
                     :QBW-OWNBR,
                     :QBW-SSNO,
                     :QBW-LNAME,
                     :QBW-FNAME,
                     :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.


      D    STOP MESS.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
   
      *-----------------------------------------------------------------
       CLOSE-BW1-CONNECTION.

           IF BW1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW1-CONN-OPEN-SW.


      * COPY "LIBLP/LPBW2RN.CPY".
      * COPYMEMBER: LIBLP/LPBW2RN
       LOAD-BW2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW2-FILE.
           PERFORM LOAD-BW2-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

           IF BW2-CONN-OPEN-SW = "N"
              MOVE "Y" TO BW2-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW2_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW2-FILE. *> SSP - BWFILE
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-LNAME, BW-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( BW2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW2-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QBW2-WEND-OWNBR = ZEROES    ) OR
              ( QBW2-WEND-OWNBR NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW2-WBEG-OWNBR
                                           QBW2-WEND-OWNBR
              MOVE BW-LNAME             TO QBW2-WBEG-LNAME
              MOVE ALL "Z"              TO QBW2-WEND-LNAME
              CALL "C$TOLOWER" USING QBW2-WEND-LNAME, VALUE 35
              MOVE BW-FNAME             TO QBW2-WBEG-FNAME
              MOVE ALL "Z"              TO QBW2-WEND-FNAME
              CALL "C$TOLOWER" USING QBW2-WEND-FNAME, VALUE 35
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              MOVE 35                   TO SQL-TEXT-SIZE
              MOVE BW-LNAME             TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QBW2-WSBEG-LNAME.

      *  STORED PROCEDURE
      *
      *   SELECT BWFILE.BW_OWNBR,
      *          BWFILE.BW_SSNO,             
      *          BWFILE.BW_LNAME, 
      *          BWFILE.BW_FNAME,
      *          BWFILE.BW_SOLICIT
      *     FROM DBO.BWFILE
      *    WHERE BWFILE.BW_OWNBR       = @QBW2_WBEG_OWNBR
      *    ORDER BY BWFILE.BW_OWNBR,
      *             BWFILE.BW_LNAME,
      *             BWFILE.BW_FNAME,
      *             BWFILE.BW_SSNO

           EXEC SQL SET CONNECTION BW2_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW2_CURSOR CURSOR FOR
                :BW2-RETURN-CODE = EXEC SSP_BWLNRP_BW2FILE_SELECT
              ( :QBW2-WBEG-OWNBR )                    
           END-EXEC.

           EXEC SQL
               OPEN BW2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW2-WBEG-OWNBR 
                          QBW2-WEND-OWNBR.
           MOVE SPACES TO QBW2-WBEG-LNAME
                          QBW2-WBEG-FNAME
                          QBW2-WEND-LNAME
                          QBW2-WEND-FNAME.

           MOVE STAT---GOOD TO BW2-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BW2-FILE-NEXT. *> EMBEDDED - BWFILE
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-LNAME, BW-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION BW2_CONNECT END-EXEC.

           IF ( BW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW2_CURSOR INTO  
                     :QBW-OWNBR,
                     :QBW-SSNO,
                     :QBW-LNAME,
                     :QBW-FNAME,
                     :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW2-FILE.

           PERFORM CLOSE-IT.
           IF ( BW2-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW2_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW2-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW2-CONNECTION.

           IF BW2-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW2_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW2_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW2-CONN-OPEN-SW.


      * COPY "LIBLP/LPLN4RN.CPY".
      * COPYMEMBER: LIBLP/LPLN4RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN4-FILE.
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
       OPEN-LN4-FILE.
           PERFORM LOAD-LN4-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN4-FILE. *> EMBEDDED - LNFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO1 DELIMITED BY " " INTO E-KEYX.

           IF ( LN4-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN4-FILE.

           IF ( QLN4-WEND-SSNO1 = ZEROES    ) OR
              ( QLN4-WEND-SSNO1 NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN4-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN4-WEND-OWNBR
              MOVE LN-SSNO1             TO QLN4-WBEG-SSNO1
              MOVE ALL "9"              TO QLN4-WEND-SSNO1.

           MOVE BEG-CLASS               TO QWS-WBEG-CLASS.
           MOVE END-CLASS               TO QWS-WEND-CLASS.  

           EXEC SQL
            DECLARE LN4_CURSOR CURSOR FOR 
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1, 
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2, 
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4,
                   LNFILE.LN_ORGST,
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_PURCD, 
                   LNFILE.LN_SRCD, 
                   LNFILE.LN_COLLCD, 
                   LNFILE.LN_POCD, 
                   LNFILE.LN_PAY_SCHLD_FG, 
                   LNFILE.LN_STATFLAGS, 
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_ORIG_1STPYDATE
                                           AS VARCHAR(10)),
                   CAST(LNFILE.LN_1STPYDATE
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_PLDATE   AS VARCHAR(10)), 
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_DLNO,
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_TOTNOALDEL,
                   CAST(LNFILE.LN_DATE_PAID_LAST
                                          AS VARCHAR(10)),
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ 
            FROM DBO.LNFILE  LNFILE
            WHERE (  LNFILE.LN_OWNBR       = :QLN4-WBEG-OWNBR
              AND    LNFILE.LN_SSNO1 BETWEEN :QLN4-WBEG-SSNO1
                                         AND :QLN4-WEND-SSNO1
              AND    LNFILE.LN_CLASS BETWEEN :QWS-WBEG-CLASS
                                         AND :QWS-WEND-CLASS )
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_SSNO1,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN4_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN4-WBEG-OWNBR
                          QLN4-WEND-OWNBR
                          QLN4-WBEG-SSNO1
                          QLN4-WEND-SSNO1.

           MOVE STAT---GOOD TO LN4-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN4-FILE-NEXT. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO1 DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QSPA-REC.
           INITIALIZE QSPC-REC.

           IF ( LN4-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN4_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-PURCD, 
                   :QLN-SRCD, 
                   :QLN-COLLCD, 
                   :QLN-POCD, 
                   :QLN-PAY-SCHLD-FG, 
                   :QLN-STATFLAGS, 
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-PLDATE, 
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-DLNO,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-DATE-PAID-LAST,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN4-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN4-FILE.

           PERFORM CLOSE-IT.
           IF ( LN4-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN4_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN4-CURSOR-STAT.


      *COPY "LIBLP/LPLN5RN.CPY".
      * COPYMEMBER: LIBLP/LPLN5RN
       LOAD-LN5-FILE.
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
       OPEN-LN5-FILE.
           PERFORM LOAD-LN5-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN5-FILE. *> EMBEDDED - LNFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO2 DELIMITED BY " " INTO E-KEYX.

           IF ( LN5-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN5-FILE.

           IF ( QLN5-WEND-SSNO2 = ZEROES    ) OR
              ( QLN5-WEND-SSNO2 NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN5-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN5-WEND-OWNBR
              MOVE LN-SSNO2             TO QLN5-WBEG-SSNO2
              MOVE ALL "9"              TO QLN5-WEND-SSNO2.

           MOVE BEG-CLASS               TO QWS-WBEG-CLASS.
           MOVE END-CLASS               TO QWS-WEND-CLASS.  

           EXEC SQL
            DECLARE LN5_CURSOR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1, 
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2, 
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4,
                   LNFILE.LN_ORGST,
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_PURCD, 
                   LNFILE.LN_SRCD, 
                   LNFILE.LN_COLLCD, 
                   LNFILE.LN_POCD, 
                   LNFILE.LN_PAY_SCHLD_FG, 
                   LNFILE.LN_STATFLAGS, 
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_ORIG_1STPYDATE
                                           AS VARCHAR(10)),
                   CAST(LNFILE.LN_1STPYDATE
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_PLDATE   AS VARCHAR(10)), 
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_DLNO,
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_TOTNOALDEL,
                   CAST(LNFILE.LN_DATE_PAID_LAST
                                          AS VARCHAR(10)),
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ 
            FROM DBO.LNFILE  LNFILE
            WHERE ( LNFILE.LN_OWNBR       = :QLN5-WBEG-OWNBR
              AND   LNFILE.LN_SSNO2 BETWEEN :QLN5-WBEG-SSNO2
                                        AND :QLN5-WEND-SSNO2
              AND   LNFILE.LN_CLASS BETWEEN :QWS-WBEG-CLASS
                                        AND :QWS-WEND-CLASS )
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_SSNO2,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN5_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN5-WBEG-OWNBR
                          QLN5-WEND-OWNBR
                          QLN5-WBEG-SSNO2
                          QLN5-WEND-SSNO2.

           MOVE STAT---GOOD TO LN5-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN5-FILE-NEXT. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO2 DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN5-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN5_CURSOR INTO  
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-PURCD, 
                   :QLN-SRCD, 
                   :QLN-COLLCD, 
                   :QLN-POCD, 
                   :QLN-PAY-SCHLD-FG, 
                   :QLN-STATFLAGS, 
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-PLDATE, 
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-DLNO,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-DATE-PAID-LAST,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN5-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN5-FILE.

           PERFORM CLOSE-IT.
           IF ( LN5-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN5_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN5-CURSOR-STAT.

  
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
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

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPRPMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BWLNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
