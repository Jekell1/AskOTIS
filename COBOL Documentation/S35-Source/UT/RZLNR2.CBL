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
      *   BAH 181002 GLOBAL BWFILE, L2G
      *   BAH 181204 GLOBAL LNFILE, L2G
      * BAH 20190919 REMOVED ACCESS-CALL ON BWFILE
      * BAH 20200710 MOVED OPENS TO INITIALIZE AND USE LOADS WHERE OPENS WERE
      *              AND DONT CLOSE. #TST001
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
           "@(#)LP/BWLNRP.CBL S35 04/11/22-10:49:48 >".
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
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

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

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWLNRP_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/BWLNRP_WKS.CPY".
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
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
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
              PERFORM START-BW1-FILE
           ELSE
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE.
       NEXT-BW.
           IF SEQ = 1
              PERFORM READ-BW1-FILE-NEXT
              IF IO-FG = 0
                 IF (BW-SSNO > END-SS) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
                    GO TO END-OF-GROUP.
           IF SEQ = 2
              PERFORM READ-BW2-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

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
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
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
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-BW1-FILE.

           PERFORM ENABLE-INTERRUPT.
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
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
           PERFORM START-LN4-FILE.
       READ-NEXT-SSNO4.
           PERFORM READ-LN4-FILE-NEXT.
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
           PERFORM START-LN5-FILE.
       READ-NEXT-SSNO5.
           PERFORM READ-LN5-FILE-NEXT.
           IF (IO-FG = 9) OR (LN-SSNO(2) NOT = BW-SSNO)
                         OR (LN-OWNBR NOT = LN-PATH-OWNBR)
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
      *       IF IO-FG NOT = 0
              IF SP-ORGST  = '  '
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
           MOVE 0 TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE BWSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO BWSCAN-BUF.
           IF SCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
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
       COPY "LIBWI/PFSWINDOWS.CPY".
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME,
                   BWFILE.BW_FNAME,
                   BWFILE.BW_SOLICIT
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
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

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            INTO :QBW-REC
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

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

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.


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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW2-FILE.
           PERFORM LOAD-BW2-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW2-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE BW2_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME, 
                   BWFILE.BW_FNAME, 
                   BWFILE.BW_SOLICIT 
            FROM DBO.BWFILE
            WHERE (( BWFILE.BW_OWNBR >= :QBW2-WBEG-OWNBR
              AND    BWFILE.BW_LNAME >= :QBW2-WBEG-LNAME
              AND    BWFILE.BW_FNAME >= :QBW2-WBEG-FNAME )
               OR  ( BWFILE.BW_OWNBR >= :QBW2-WBEG-OWNBR
              AND    BWFILE.BW_LNAME >= :QBW2-WSBEG-LNAME
              AND    BWFILE.BW_FNAME >= '' ))
              AND (  BWFILE.BW_OWNBR <= :QBW2-WEND-OWNBR
              AND    BWFILE.BW_LNAME <= :QBW2-WEND-LNAME
              AND    BWFILE.BW_FNAME <= :QBW2-WEND-FNAME )
            ORDER BY BWFILE.BW_OWNBR,
                     BWFILE.BW_LNAME,
                     BWFILE.BW_FNAME,
                     BWFILE.BW_SSNO
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

      *-----------------------------------------------------------------
       READ-BW2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-LNAME, BW-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBW-REC.

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

      *-----------------------------------------------------------------
       CLOSE-BW2-FILE.

           PERFORM CLOSE-IT.
           IF ( BW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW2-CURSOR-STAT.

  
  
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN4-FILE.
           PERFORM LOAD-LN4-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN4-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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
                   LNFILE.LN_UNITPER_FREQ,
                   ISNULL(SPAFILE.SPA_ORGST,'  '),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPCFILE.SPC_BANKRULE_RATE
            FROM DBO.LNFILE  LNFILE
              LEFT OUTER JOIN
                 DBO.SPAFILE SPAFILE
                  ON (LN_ORGST     = SPA_ORGST
                 AND  LN_SPRCLASS  = SPA_SPRCLASS
                 AND  LN_SUBCLASS  = SPA_SUBCLASS 
                 AND  LN_LAWCODE   = SPA_LAWCODE ) 
              LEFT OUTER JOIN
                 DBO.SPCFILE SPCFILE
                  ON (LN_ORGST     = SPC_ORGST
                 AND  LN_SPRCLASS  = SPC_SPRCLASS
                 AND  LN_SUBCLASS  = SPC_SUBCLASS 
                 AND  LN_LAWCODE   = SPC_LAWCODE )
            WHERE LNFILE.LN_OWNBR >= :QLN4-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN4-WEND-OWNBR
              AND LNFILE.LN_SSNO1 >= :QLN4-WBEG-SSNO1
              AND LNFILE.LN_SSNO1 <= :QLN4-WEND-SSNO1
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
       START-LN4-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO1 DELIMITED BY " " INTO E-KEYX.

           IF ( LN4-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LN4-FILE.

           IF ( QLN4-WEND-OWNBR = ZEROES    ) OR
              ( QLN4-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN4-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN4-WEND-OWNBR
              MOVE ALL "0"              TO QLN4-WBEG-SSNO1 
              MOVE LN-SSNO1             TO QLN4-WEND-SSNO1.

           EXEC SQL
            DECLARE LN4_CURSOR_NG CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR >= :QLN4-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN4-WEND-OWNBR
              AND LNFILE.LN_SSNO1 >= :QLN4-WBEG-SSNO1
              AND LNFILE.LN_SSNO1 <= :QLN4-WEND-SSNO1
            ORDER BY LNFILE.LN_OWNBR DESC,
                     LNFILE.LN_SSNO1 DESC,
                     LNFILE.LN_ACCTNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LN4_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN4-WBEG-OWNBR
                          QLN4-WEND-OWNBR
                          QLN4-WBEG-SSNO1
                          QLN4-WEND-SSNO1.

           MOVE STAT---GOOD TO LN4-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LN4-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO1 DELIMITED BY " " INTO E-KEYX.

           MOVE LN-OWNBR TO QLN-OWNBR.
           MOVE LN-SSNO1 TO QLN-SSNO1.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            INTO :QLN-REC
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR = :QLN-OWNBR
              AND LNFILE.LN_SSNO1 = :QLN-SSNO1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN4-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN4-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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
                   :QLN-UNITPER-FREQ,
                   :QSPA-ORGST,
                   :QSPA-SPRCLASS,
                   :QSPA-SUBCLASS,
                   :QSPA-LAWCODE,
                   :QSPA-EXPD-FRMLA,
                   :QSPA-CONTPDTH,
                   :QSPC-BANKRULE-RATE
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
       READ-LN4-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO1 DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN4-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LN4_CURSOR_NG INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN4-FILE-PREVIOUS.

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

           IF ( LN4-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LN4_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LN4-CURSOR-NG-STAT.  
  

      * COPY "LIBLP/LPLN5RN.CPY".
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN5-FILE.
           PERFORM LOAD-LN5-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN5-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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
                   LNFILE.LN_UNITPER_FREQ,
                   ISNULL(SPAFILE.SPA_ORGST,'  '),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPCFILE.SPC_BANKRULE_RATE
            FROM DBO.LNFILE  LNFILE
              LEFT OUTER JOIN
                 DBO.SPAFILE SPAFILE
                  ON (LN_ORGST     = SPA_ORGST
                 AND  LN_SPRCLASS  = SPA_SPRCLASS
                 AND  LN_SUBCLASS  = SPA_SUBCLASS 
                 AND  LN_LAWCODE   = SPA_LAWCODE ) 
              LEFT OUTER JOIN
                 DBO.SPCFILE SPCFILE
                  ON (LN_ORGST     = SPC_ORGST
                 AND  LN_SPRCLASS  = SPC_SPRCLASS
                 AND  LN_SUBCLASS  = SPC_SUBCLASS 
                 AND  LN_LAWCODE   = SPC_LAWCODE )
            WHERE LNFILE.LN_OWNBR >= :QLN5-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN5-WEND-OWNBR
              AND LNFILE.LN_SSNO2 >= :QLN5-WBEG-SSNO2
              AND LNFILE.LN_SSNO2 <= :QLN5-WEND-SSNO2
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
       START-LN5-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO2 DELIMITED BY " " INTO E-KEYX.

           IF ( LN5-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LN5-FILE.

           IF ( QLN5-WEND-OWNBR = ZEROES    ) OR
              ( QLN5-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN5-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN5-WEND-OWNBR
              MOVE ALL "0"              TO QLN5-WBEG-SSNO2
              MOVE LN-SSNO2             TO QLN5-WEND-SSNO2.

           EXEC SQL
            DECLARE LN5_CURSOR_NG CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR >= :QLN5-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN5-WEND-OWNBR
              AND LNFILE.LN_SSNO2 >= :QLN5-WBEG-SSNO2
              AND LNFILE.LN_SSNO2 <= :QLN5-WEND-SSNO2
            ORDER BY LNFILE.LN_OWNBR DESC,
                     LNFILE.LN_SSNO2 DESC,
                     LNFILE.LN_ACCTNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LN5_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN5-WBEG-OWNBR
                          QLN5-WEND-OWNBR
                          QLN5-WBEG-SSNO2
                          QLN5-WEND-SSNO2.

           MOVE STAT---GOOD TO LN5-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LN5-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO2 DELIMITED BY " " INTO E-KEYX.

           MOVE LN-OWNBR TO QLN-OWNBR.
           MOVE LN-SSNO2 TO QLN-SSNO2.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            INTO :QLN-REC
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR = :QLN-OWNBR
              AND LNFILE.LN_SSNO2 = :QLN-SSNO2
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN5-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN5-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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
                   :QLN-UNITPER-FREQ,
                   :QSPA-ORGST,
                   :QSPA-SPRCLASS,
                   :QSPA-SUBCLASS,
                   :QSPA-LAWCODE,
                   :QSPA-EXPD-FRMLA,
                   :QSPA-CONTPDTH,
                   :QSPC-BANKRULE-RATE
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
       READ-LN5-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO2 DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN5-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LN5_CURSOR_NG INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN5-FILE-PREVIOUS.

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

           IF ( LN5-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LN5_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LN5-CURSOR-NG-STAT.



       COPY "LIBLP/LPLTP1RN.CPY".

      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE
              INTO  
                  :QBR-NO,
                  :QBR-NAME,
                  :QBR-LOAN-NUM-FLAG,
                  :QBR-DATA-PATH,
                  :QBR-MACHINE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BR_CURSOR INTO  
                  :QBR-NO,
                  :QBR-NAME,
                  :QBR-LOAN-NUM-FLAG,
                  :QBR-DATA-PATH,
                  :QBR-MACHINE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 
  

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
