       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GAGILA.
       DATE-WRITTEN.   10-23-89.
      ******************************************************************
      *
      *    DESCRIPTION:  GEORGIA 'GILA' REPORT
      *
      *=================================================================
      * REV:
      *  JFF 101195 ADDED CLASS SCAN
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *   CS 130820 CHANGED SUB PIC 9(2) SUBSCRIPT OUT OF RANGE IF MORE THAN
      *             10 LOANS VIEWED FOR RESTRICTIONS, ALSO ON 10TH LOAN
      *             GAVE ERROR NOT FOUND ON THE BORROWER
      * CS 160510 REMOVED REDEF OF LE-FLAG/LE-SSNO AND MADE SEPARATE FIELDS
      *           CHANGED LE-FLAG FROM PIC X(4) TO PIC X.
      *  CS 160410 ACTIVATE 8 DIGIT DATES BEG-DATE, END-DATE
      *            REMOVED  DIVIDE LE-OSACCTNO BY 100 GIVING D-OSACCTNO.
      *
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED 'LE' FILE TO HAVE SUB-PREFIX OF 'E' OR 'R'.
      *
      * KEC 2018.0424 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181003 GLOBAL BWFILE
      * BAH 181030 GLOBAL LEFILE
      * BAH 181207 GLOBAL LNFILE
      * BAH 190528 CHANGED SUB FROM 99 TO 9(4), IN AIX ONLY IT WAS FALLING
      *            TO A $ SIGN FROM OVER 100 LE RECORDS. QA0005
      * BAH 20190926 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
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
      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/GAGILA.CBL J35 12/21/23-09:04:56 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
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
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(4).
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  NAME-WORKER          PIC X(40).
       01  FIRST-NAME           PIC X(16).
       01  HOLD-BR              PIC 9(4)     VALUE 0.
       01  ENDORSER-PRINTED     PIC X        VALUE "N".
       01  CLASS-FG            PIC X(3)      VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  END-BRANCH      PIC 9(4).
           03  BEG-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-ACCT        PIC 9(8).
           03  END-ACCT        PIC 9(8).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  LINE-SP         PIC 9.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

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
           "- - - GILA RESTRICTED MONEY REPORT - - - ".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "BRANCH :".
           03  D-BRANCH    PIC 9(4)     VALUE 0.
           03  FILLER      PIC X        VALUE " ".
           03  D-BRDESC    PIC X(20)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(18)    VALUE " ".
           03  FILLER      PIC X(55)    VALUE
               "LOAN        LOAN                 ".
           03  FILLER      PIC X(58)    VALUE
           "------------- LOANS CONSIDERED FOR RESTRICTION -----------".

       01  HEAD5.
           03  FILLER      PIC X(43)    VALUE
               "ACCOUNT NO.       DATE      AMOUNT  BORROWE".
           03  FILLER      PIC X(34)    VALUE
               "RS SIGNED ON THE LOAN         TYPE".
           03  FILLER      PIC X(55)    VALUE
           " ACCTNO LOANDATE  POFFDATE    LNAMT   BALANCE  RESTRICT".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(16).
           03  D-LOANDATE  PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-LNAMT     PIC ZZZZZZ.ZZ.
           03  FILLER      PIC XX.
           03  D-SSNO      PIC 999/99/9999.
           03  FILLER      PIC X.
           03  D-MAKER     PIC X(4).
           03  D-BW-NAME   PIC X(20).
           03  FILLER      PIC X.
           03  D-TYPE      PIC X(4).
           03  FILLER      PIC X.
           03  D-OSACCTNO  PIC 999999.
           03  FILLER      PIC X.
           03  D-OSLDATE   PIC 99/99/99 BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-OSPDATE   PIC 99/99/99 BLANK ZERO.
           03  D-OSLAMT    PIC ZZZZZZ.ZZ.
           03  D-BALANCE   PIC ZZZZZZZ.ZZ.
           03  D-OSRAMT    PIC ZZZZZZZ.ZZ.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/GAGILA_DEF.CPY".
       COPY "LIBLP/GAGILA_WKS.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/GAGILA_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LE1-FILE.
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
           MOVE "GAGILA" TO FORM-PROG VDU-FORM-NAME.
           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.


      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LE-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LE1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS     TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CLASS     TO QLN3-WEND-CLASS.
           MOVE BEG-ACCT      TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-OWNBR < BEG-BRANCH
              GO TO NEXT-LN.
           IF LN-CLASS < BEG-CLASS OR
               LN-CLASS > END-CLASS
                  GO TO NEXT-LN.
           IF LN-ACCTNO < BEG-ACCT OR
               LN-ACCTNO > END-ACCT
                  GO TO NEXT-LN.
           IF LN-SMALLOAN NOT = "Y"
              GO TO NEXT-LN.
           IF LN-OWNST NOT = "GA"
              GO TO NEXT-LN.

      * VERIFY BETWEEN BEGINNING AND ENDING DATE
           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              GO TO NEXT-LN.

           IF LN-OWNBR = HOLD-BR
              GO TO REPORT-DETAILS.

           MOVE LN-OWNBR TO HOLD-BR D-BRANCH.
           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.
           MOVE BR-NAME TO D-BRDESC.

       REPORT-DETAILS.
           MOVE "N" TO FIRST-TIME.

           PERFORM DETAIL-LINE-PROCESS.
           GO TO NEXT-LN.
      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LE1-FILE.

      *    MOVE "Y" TO FIRST-TIME.
      *    MOVE "N" TO MORE-THAN-1-CLASS.

           GO TO NEXT-GROUP-LOC.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE GAGILA-QUEUE-POS TO Q-POS.
           MOVE GAGILA-COPIES-POS TO C-POS.
           MOVE GAGILA-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           ADD EXT-CONAME-RPT-LINES 1 GIVING LN-COUNT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS....................." TO WR-BUF
              MOVE  MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO LN-SP.
      *----------------------------------------------------
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

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *-------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO AC-01.
      *----------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-01.
           MOVE VDUNUM0 TO BEG-ACCT.
      *-------------------------------------------------------
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
      *-----------------------------------------------------
       ALL-AC.
           MOVE "SNNN060AC1." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.
      *------------------------------------------------------
       DT-01.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO AC-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-------------------------------------------------------
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO LN-SP.
      *-------------------------------------------------------
       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *------------------------------------------------------
       LN-SP.
           MOVE "ENNN010LNSP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LN-SP.
           MOVE VDUNUM0 TO LINE-SP.
           IF LINE-SP = 1 GO TO EXIT-PARM.
           IF LINE-SP NOT = 2 GO TO LN-SP.

       EXIT-PARM.
           EXIT.

      *******************************************************
       DISPLAY-PARAMETERS SECTION.
      *******************************************************
           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
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
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "SNNN010LNSP." TO VDU.
           MOVE LINE-SP TO VDUNUM0.
           PERFORM SCREENIO.

      *******************************************************
       DETAIL-LINE-PROCESS SECTION.
      *******************************************************

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

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-LNAMT TO D-LNAMT.
           MOVE LINE-SP TO LN-FEED.

           PERFORM PRINT-LOAN-ENDORSERS.
           IF ENDORSER-PRINTED = "Y"
              GO TO END-DETAIL-LN-PROCESS.

           PERFORM WRITE-DETAIL-LINE.
       CHECK-ALL-SSNO.
           ADD 1 TO SUB.
           IF SUB NOT > 4
              IF LN-SSNO(SUB) NOT = 0
                 PERFORM GET-BORROWER
                 PERFORM WRITE-DETAIL-LINE
                 GO TO CHECK-ALL-SSNO.

       END-DETAIL-LN-PROCESS.
           EXIT.

      *******************************************************
       GET-BORROWER SECTION.
      *******************************************************

           IF LN-MAKERCD(SUB) = "M"
              MOVE SPACES TO D-MAKER
           ELSE
              IF LN-MAKERCD(SUB) = "J"
                 MOVE "(J)" TO D-MAKER
              ELSE
                 IF LN-MAKERCD(SUB) = "E"
                    MOVE "(E)" TO D-MAKER.

           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "**NOT ON FILE**" TO D-BW-NAME
              GO TO END-GET-BORROWER.

           MOVE LN-SSNO(SUB) TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", +".
           INSPECT NAME-WORKER REPLACING FIRST "+               "
              BY FIRST-NAME.
           MOVE NAME-WORKER TO D-BW-NAME.
       END-GET-BORROWER.
           EXIT.

      *********************************************
       PRINT-LOAN-ENDORSERS SECTION.
      *********************************************
           MOVE 1 TO SUB.
           PERFORM GET-BORROWER.
           MOVE "N" TO ENDORSER-PRINTED.

           MOVE LN-OWNBR  TO LE-BRNO
                             QLE1-WBEG-BRNO
                             QLE1-WEND-BRNO.
           MOVE LN-ACCTNO TO LE-ACCTNO
                             QLE1-WBEG-ACCTNO
                             QLE1-WEND-ACCTNO.
           MOVE "R"       TO LE-TYPE
                             QLE1-WBEG-TYPE
                             QLE1-WEND-TYPE.
           MOVE "D"       TO LE-FLAG
                             QLE1-WBEG-FLAG.
           MOVE ALL "Z"   TO QLE1-WEND-FLAG.
           MOVE 0         TO LE-OSACCTNO LE-SSNO
                             QLE1-WBEG-OSACCTNO
                             QLE1-WBEG-SSNO.
           MOVE ALL "9"   TO QLE1-WEND-OSACCTNO
                             QLE1-WEND-SSNO.

           PERFORM START-LE1-FILE.

       NEXT-ENDORSER.
           PERFORM READ-LE1-FILE-NEXT.
           IF (IO-FG = 9) OR (LE-BRNO NOT = LE-PATH-OWNBR)
                           OR (LE-ACCTNO NOT = LN-ACCTNO)
              GO TO PRINT-LN-ENDORSERS-EXIT.

           IF LE-TYPE NOT = "R"
              GO TO NEXT-ENDORSER.

           IF LE-FLAG = "P"
              MOVE "PAID" TO D-TYPE
           ELSE
              IF LE-FLAG = "R"
                 MOVE "RNWL" TO D-TYPE
              ELSE
                 IF LE-FLAG = "D"
                    IF LE-R-RESTRICTED-MONEY = 0
                       MOVE " " TO D-TYPE
                    ELSE
                       MOVE "DUAL" TO D-TYPE.
      *    DIVIDE LE-OSACCTNO BY 100 GIVING D-OSACCTNO.
           MOVE LE-OSACCTNO TO D-OSACCTNO.
           MOVE LE-R-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-OSLDATE.

           MOVE LE-R-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-OSPDATE.

           MOVE LE-R-LNAMT TO D-OSLAMT.
           MOVE LE-R-RESTRICTED-MONEY TO D-OSRAMT.
           MOVE LE-R-BALANCE TO D-BALANCE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE "Y" TO ENDORSER-PRINTED.
           ADD 1 TO SUB.
           IF SUB NOT > 4
              IF LN-SSNO(SUB) NOT = 0
                 PERFORM GET-BORROWER
                 MOVE "N" TO ENDORSER-PRINTED.
           GO TO NEXT-ENDORSER.
       PRINT-LN-ENDORSERS-EXIT.
           EXIT.

      *******************************************************
       WRITE-DETAIL-LINE SECTION.
      *******************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************************************
       HEAD-PAGE SECTION.
      *******************************************************

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
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE LINE-SP TO LN-FEED.
           MOVE 6 TO LN-COUNT.

      *******************************************************
       PRINT-RANGE-LINES SECTION.
      *******************************************************

           MOVE 4 TO LN-FEED.
           MOVE "BEGINNING BRANCH   :" TO RANGE-LINE.
           MOVE BEG-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    BRANCH   :" TO RANGE-LINE.
           MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    CLASS    :" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT  :" TO RANGE-LINE.
           MOVE BEG-ACCT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    ACCOUNT  :" TO RANGE-LINE.
           MOVE END-ACCT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LOANDATE :" TO RANGE-LINE.
           MOVE BEG-DATE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    LOANDATE :" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      *******************************************************
       CLASS-SCAN SECTION.
      *******************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
               MOVE "SNNN020CL1." TO VDU
           ELSE
               MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO GAGILA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GAGILA-SYSDATE.
           DISPLAY GAGILA-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GAGILA-FORM-FIELDS TO WR-BUF.
           MOVE GAGILA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE GAGILA-HELP--FILE TO HELP-LINK-FILE.
           MOVE GAGILA-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GAGILA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *************************************
       MISC-PARA SECTION.
      *************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ***********************************************************
       IO-ROUTINE SECTION.
      ***********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLE1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ********************************************************
       END-PROGRAM SECTION.
      ********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GAGILA-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
