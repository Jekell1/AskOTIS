       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ADDEXT.
       DATE-WRITTEN.    02/17/2025.
      ***************************************************************
      *
      *    DESC: COMPLIANCE AND MONITORING CONTROL FOR OVERLAPPING
      *          AD&D POLICIES TO ENSURE THE SYSTEM CONTROLS AROUND
      *          THAT ARE WORKING AS EXPECTED.
      *
      *    CREATES TAB DELIMITED /USR/EXTRACT/OPS/ADDEXP
      *
      *    ONLY INCLUDES LOANS THAT HAVE SOLD AD&D INSURANCE
      *    WITHIN THE DATE RANGE ENTERED. SKIPS VOIDS AND
      *    ONLY INCLUDES ACCOUNTS WITH ORIG BRANCH = OWNING
      *    BRANCH. FILE INCLUDES THE MOST RECENT PRIOR
      *    AD&D SALE DATE TO MONITOR OVERLAPPING POLICIES.
      *
      * SSP_ADDEXT_LNFILE_SELECT 
      *
      *  REV:
      * BAH 2025.0217 NEW TAB DELIMITED ASCII FILE FOR COMPLIANCE #1701
      *                                                        S35Q-165
      * RMZ 2025.0218 PERFORMANCE TUNING - ADDED STORED PROCEDURE.
      *                                    #1701 (S35Q-165)
      * 
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT ADDEXT-FILE ASSIGN OUTPUT ADDEXT-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *   LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  ADDEXT-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  ADDEXT-REC.
           03  ADDEXT-BYTE                PIC X OCCURS 200 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/ADDEXT S35 02/20/25-08:44:47 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *-----------------------------------------------------------------
      *             SQL WORKING STORAGE
      *-----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.           
       01  QWS-BEG-DATE         PIC X(10)    VALUE SPACES.
       01  QWS-END-DATE         PIC X(10)    VALUE SPACES. 
       01  QWS-LAST-ADD-DATE    PIC X(10)    VALUE SPACES.   
       01  QWS-HOLD-LN-LOANDATE PIC X(10)    VALUE SPACES.   
       01  QWS-HOLD-LN-ACCTNO   PIC 9(8)     VALUE ZEROES.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN1-CONN-OPEN-SW     PIC X        VALUE "N". 

      *-----------------------------------------------------------------

      *01  WS-LAST-ADD-DATE      PIC X(10)   VALUE SPACES. 
       01  HOLD-LN-SSNO          PIC 9(9).
       01  HOLD-LN-ACCTNO        PIC 9(8).
       01  ADDEXT-PATH.
           03  FILLER            PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER            PIC X(10) VALUE  "OPS/ADDEXT".

       01  HOLD-ADDEXT-REC.
           03  ADDEXT-OWNBR          PIC 9(4).
           03  ADDEXT-NUMBER         PIC 9(6).
           03  ADDEXT-LNAME          PIC X(16).
           03  ADDEXT-ORIG-ST        PIC XX.
           03  ADDEXT-LOANDATE       PIC 99/99/99.
           03  ADDEXT-ORGTERM        PIC 9(3).
           03  ADDEXT-PREMIUM        PIC 9(6).99.
           03  ADDEXT-PRIOR-SALEDATE PIC 99/99/99.

       01  ADDEXT-IFN        PIC X(4)     VALUE "WKLN".

       01  SUB              PIC 9(3).
       01  FEE-SUB          PIC 9(3).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TAB-1                PIC X VALUE X"09".

       01  FOUND-LAST-ADD-DATE  PIC 9(8).
       01  FILLER REDEFINES FOUND-LAST-ADD-DATE.
           03 FOUND-LAST-ADD-YYYY PIC 9(4).
           03 FOUND-LAST-ADD-MM   PIC 9(2).
           03 FOUND-LAST-ADD-DD   PIC 9(2).    
 
       01  WS-LAST-ADD-DATE       PIC X(10).
       01  FILLER REDEFINES WS-LAST-ADD-DATE.
           03 WS-LAST-ADD-YYYY    PIC X(4).
           03 FILLER              PIC X.
           03 WS-LAST-ADD-MM      PIC X(2).
           03 FILLER              PIC X.
           03 WS-LAST-ADD-DD      PIC X(2). 
 
       01  CR-1          PIC X VALUE X"0D".

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

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
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "--- AD&D PREMIUM MONITOR EXTRACT ---".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  RANGE1D REDEFINES RANGE1 PIC 9999/99/99.
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).
           03  RANGE2D REDEFINES RANGE2 PIC 9999/99/99.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ADDEXT_DEF.CPY".
       COPY "LIBSP/ADDEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ADDEXT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE ADDEXT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LN1-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE ADDEXT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ADDEXT" TO VDU-FORM-NAME.

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
           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           PERFORM GLOBAL-UPDATE.

           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      **************************
       GLOBAL-UPDATE SECTION.
      **************************
           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST   TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE BR-NO     TO LN-OWNBR.
           MOVE 0         TO LN-ACCTNO.

           MOVE LN-PATH-OWNBR        TO LN-OWNBR
                                        QLN1-WBEG-OWNBR
                                        QLN1-WEND-OWNBR.
           MOVE 0                    TO LN-ACCTNO
                                        QLN1-WBEG-ACCTNO.
           MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           PERFORM START-LN1-FILE. *> SSP - LNFILE, BRFILE, BWFILE
       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT. *> EMBEDDED - LN, BR,BWFILE
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-GLOBAL-UPDATE.

      *    ADD 1 TO TOTAL-RECORDS.

           IF LN-OWNBR > END-BR
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-POCD-NONLOAN
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-OWNBR NOT = LN-ORGBR
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           PERFORM VARYING FEE-SUB FROM 1 BY 1 UNTIL FEE-SUB > 10
              IF LN-FEECODE(FEE-SUB) = BR-ADD-INS-FEECODE AND
                     BR-ADD-INS-FEECODE NOT = SPACES
                  PERFORM CREATE-ADDEXT-WORK
              END-IF
           END-PERFORM.

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE ADDEXT-QUEUE-POS TO Q-POS.
           MOVE ADDEXT-COPIES-POS TO C-POS.
           MOVE ADDEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

      *     MOVE 0 TO TOTAL-RECORDS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.


           CALL "C$TOLOWER" USING ADDEXT-PATH,VALUE 13.
           PERFORM OPEN-ADDEXT-FILE-OUTPUT.

      * CREATE HEADER
           STRING "BRNO",           TAB-1,
                  "ACCTNO",         TAB-1,
                  "LNAME",          TAB-1,
                  "ORGST",          TAB-1,
                  "LOANDATE",       TAB-1,
                  "TERM",           TAB-1,
                  "AD&D PREMIUM",   TAB-1,
                  "PRIOR DATE",   CR-1
              DELIMITED BY SIZE INTO ADDEXT-REC.
           PERFORM WRITE-ADDEXT-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

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
              GO TO BEGDATE-01.
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
           GO TO BEGDATE-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              IF BEG-DATE > END-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO EXIT-PARM.

       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.


      ****************************
       CREATE-ADDEXT-WORK SECTION.
      ****************************

           MOVE SPACES TO HOLD-ADDEXT-REC ADDEXT-REC.

           MOVE LN-SSNO(1) TO HOLD-LN-SSNO.
           MOVE LN-ACCTNO  TO HOLD-LN-ACCTNO.

      * READS LN4 FILE LOOKING FOR PAST AD&D SOLD
           PERFORM FIND-LAST-ADD-SOLD.

      * MUST GET ORIGINAL LOAN BACK IN THE BUFFER
           MOVE LN-PATH-OWNBR        TO LN-OWNBR
                                        QLN1-WBEG-OWNBR
                                        QLN1-WEND-OWNBR.
           MOVE HOLD-LN-ACCTNO       TO LN-ACCTNO
                                        QLN1-WBEG-ACCTNO.
           MOVE ALL "9"              TO QLN1-WEND-ACCTNO.
      *    PERFORM START-LN1-FILE.
      *    PERFORM READ-LN1-FILE-NEXT.

           MOVE LN-OWNBR           TO ADDEXT-OWNBR.
           MOVE LN-ACCTNO          TO ADDEXT-NUMBER.
           MOVE LN-ORGST           TO ADDEXT-ORIG-ST.
      * MM/DD/YY
           MOVE LN-LOANDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO ADDEXT-LOANDATE.
           MOVE LN-ORGTERM         TO ADDEXT-ORGTERM.
           MOVE LN-FEEAMT(FEE-SUB) TO ADDEXT-PREMIUM.
      * MM/DD/YY
           MOVE FOUND-LAST-ADD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ADDEXT-PRIOR-SALEDATE.

      *    MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-BAD
      *       MOVE "INVALID" TO BW-LNAME.
           MOVE BW-LNAME    TO ADDEXT-LNAME.

           STRING
              ADDEXT-OWNBR          ,TAB-1,
              ADDEXT-NUMBER         ,TAB-1,
              ADDEXT-LNAME          ,TAB-1,
              ADDEXT-ORIG-ST        ,TAB-1,
              ADDEXT-LOANDATE       ,TAB-1,
              ADDEXT-ORGTERM        ,TAB-1,
              ADDEXT-PREMIUM        ,TAB-1,
              ADDEXT-PRIOR-SALEDATE ,CR-1
              DELIMITED BY SIZE INTO ADDEXT-REC.

           PERFORM WRITE-ADDEXT-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           ADD 1 TO TOTAL-INCLUDED.

       CREATE-ADDEXT-WORK-EXIT.
           EXIT.

      ****************************
        FIND-LAST-ADD-SOLD SECTION.
      ****************************

           MOVE 19000101 TO FOUND-LAST-ADD-DATE.
 
      *     MOVE HOLD-LN-SSNO TO LN-SSNO(1).
      *     PERFORM START-LN4-FILE.
      *FIND-NEXT.
      *    PERFORM READ-LN4-FILE-NEXT.
           PERFORM READ-LN4-FILE. *> EMBEDDED - LNFILE, BRFILE

           IF IO-BAD
              MOVE 19000101 TO FOUND-LAST-ADD-DATE.

      * NO LOANS OR AT THE END OF THEIR LOANS
      *   IF LN-SSNO(1) NOT = HOLD-LN-SSNO
      *      GO TO EXIT-FIND-LAST-ADD-SOLD.

      * SKIP ACCTNO YOU ARE LOOKING FOR PRIORS FOR
      *    IF LN-ACCTNO = HOLD-LN-ACCTNO
      *       GO TO FIND-NEXT.
      *
      *    IF ( LN-POCD-NONLOAN )
      *       GO TO FIND-NEXT.

      * DOES THIS ACCOUNT HAVE AD&D

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *       IF LN-FEECODE(SUB) = BR-ADD-INS-FEECODE AND
      *              BR-ADD-INS-FEECODE NOT = SPACES
      *          IF FOUND-LAST-ADD-DATE = 0
      *             MOVE LN-LOANDATE       TO FOUND-LAST-ADD-DATE
      *          ELSE
      *             MOVE FOUND-LAST-ADD-DATE    TO NUM-DATE
      *             MOVE LN-LOANDATE            TO SYS-DATE
      *             PERFORM CMPDAT
      *             IF SYS-IS-GT
      *                MOVE LN-LOANDATE TO FOUND-LAST-ADD-DATE
      *             END-IF
      *          END-IF
      *       END-IF
      *    END-PERFORM.
      *
      *    GO TO FIND-NEXT.

           IF FOUND-LAST-ADD-DATE = 19000101
              MOVE ZEROES        TO FOUND-LAST-ADD-DATE.

       EXIT-FIND-LAST-ADD-SOLD.
           EXIT.

      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.


           MOVE "BEG LOANDATE - END LOANDATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

      *    MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
      *    MOVE TOTAL-RECORDS TO RANGE1N.
      *    MOVE 2 TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
      *    MOVE TOTAL-BYPASSED TO RANGE1N.
      *    PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ADDEXT-PATH TO RANGE-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO ADDEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ADDEXT-SYSDATE.
           DISPLAY ADDEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ADDEXT-FORM-FIELDS TO WR-BUF.
           MOVE ADDEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ADDEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE ADDEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ADDEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      **********************
       IO-ROUTINE SECTION.
      **********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
      * REV:
      * JKC 2024-0404 ADDED LN-LOS-APP-ID
      ******************************************************************
       LOAD-LN1-FILE.
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
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER
                               AS LN1_CONNECT 
                   USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE.  *> SSP - LNFILE, BRFILE, BWFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           MOVE BEG-DATE                TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-BEG-DATE.

           MOVE END-DATE                TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-END-DATE.

      *  STORED PROCEDURE:
      *
      *  SELECT LNFILE.LN_OWNBR,
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_SSNO1,
      *         ISNULL(LNFILE.LN_MAKERCD1,'')     AS LN_MAKERCD1,
      *         LNFILE.LN_SSNO2, 
      *         ISNULL(LNFILE.LN_MAKERCD2,'')     AS LN_MAKERCD2,
      *         LNFILE.LN_SSNO3, 
      *         ISNULL(LNFILE.LN_MAKERCD3,'')     AS LN_MAKERCD3,
      *         LNFILE.LN_SSNO4,  
      *         ISNULL(LNFILE.LN_MAKERCD4,'')     AS LN_MAKERCD4,
      *         ISNULL(LNFILE.LN_ORGST,'')        AS LN_ORGST, 
      *         LNFILE.LN_ORGBR, 
      *         ISNULL(LNFILE.LN_POCD,'')         AS LN_POCD,
      *         ISNULL(CAST(LNFILE.LN_LOANDATE    AS VARCHAR(10)),'0') 
      *                                           AS LN_LOANDATE,
      *         LNFILE.LN_ORGTERM, 
      *         ISNULL(LNFILE.LN_FEECODE_1,'')    AS LN_FEEDCODE_1,
      *         ISNULL(LNFILE.LN_FEECODE_2,'')    AS LN_FEEDCODE_2,
      *         ISNULL(LNFILE.LN_FEECODE_3,'')    AS LN_FEEDCODE_3,
      *         ISNULL(LNFILE.LN_FEECODE_4,'')    AS LN_FEEDCODE_4,
      *         ISNULL(LNFILE.LN_FEECODE_5,'')    AS LN_FEEDCODE_5,
      *         ISNULL(LNFILE.LN_FEECODE_6,'')    AS LN_FEEDCODE_6,
      *         ISNULL(LNFILE.LN_FEECODE_7,'')    AS LN_FEEDCODE_7,
      *         ISNULL(LNFILE.LN_FEECODE_8,'')    AS LN_FEEDCODE_8,
      *         ISNULL(LNFILE.LN_FEECODE_9,'')    AS LN_FEEDCODE_9,
      *         ISNULL(LNFILE.LN_FEECODE_10,'')   AS LN_FEEDCODE_10,
      *         ISNULL(LNFILE.LN_FEECODE_11,'')   AS LN_FEEDCODE_11,
      *         ISNULL(LNFILE.LN_FEECODE_12,'')   AS LN_FEEDCODE_12,
      *         ISNULL(LNFILE.LN_FEECODE_13,'')   AS LN_FEEDCODE_13,
      *         ISNULL(LNFILE.LN_FEECODE_14,'')   AS LN_FEEDCODE_14,
      *         ISNULL(LNFILE.LN_FEECODE_15,'')   AS LN_FEEDCODE_15,
      *         ISNULL(LNFILE.LN_FEECODE_16,'')   AS LN_FEEDCODE_16,
      *         ISNULL(LNFILE.LN_FEECODE_17,'')   AS LN_FEEDCODE_17,
      *         ISNULL(LNFILE.LN_FEECODE_18,'')   AS LN_FEEDCODE_18,
      *         ISNULL(LNFILE.LN_FEECODE_19,'')   AS LN_FEEDCODE_19,
      *         ISNULL(LNFILE.LN_FEECODE_20,'')   AS LN_FEEDCODE_20,
      *         LNFILE.LN_FEEAMT_1,  
      *         LNFILE.LN_FEEAMT_2,  
      *         LNFILE.LN_FEEAMT_3,  
      *         LNFILE.LN_FEEAMT_4,  
      *         LNFILE.LN_FEEAMT_5,  
      *         LNFILE.LN_FEEAMT_6,  
      *         LNFILE.LN_FEEAMT_7,  
      *         LNFILE.LN_FEEAMT_8,  
      *         LNFILE.LN_FEEAMT_9,  
      *         LNFILE.LN_FEEAMT_10, 
      *         LNFILE.LN_FEEAMT_11, 
      *         LNFILE.LN_FEEAMT_12, 
      *         LNFILE.LN_FEEAMT_13, 
      *         LNFILE.LN_FEEAMT_14, 
      *         LNFILE.LN_FEEAMT_15, 
      *         LNFILE.LN_FEEAMT_16, 
      *         LNFILE.LN_FEEAMT_17, 
      *         LNFILE.LN_FEEAMT_18, 
      *         LNFILE.LN_FEEAMT_19, 
      *         LNFILE.LN_FEEAMT_20, 
      *         ISNULL(BWFILE.BW_OWNBR,0)         AS BW_OWNBR,
      *         ISNULL(BWFILE.BW_SSNO,0)          AS BW_SSNO,
      *         ISNULL(BWFILE.BW_LNAME,'INVALID') AS BW_LNAME
      *  FROM DBO.LNFILE LNFILE
      *
      *  INNER JOIN
      *       DBO.BRFILE BRFILE
      *     ON  ( LNFILE.LN_OWNBR            = BRFILE.BR_NO 
      *    AND    BRFILE.BR_ADD_INS_FEECODE != '  ')
      *
      *  LEFT JOIN 
      *      DBO.BWFILE BWFILE 
      *     ON   (LNFILE.LN_OWNBR            = BWFILE.BW_OWNBR     
      *    AND    LNFILE.LN_SSNO1            = BWFILE.BW_SSNO)
      *
      *  WHERE (  LNFILE.LN_OWNBR            = @QLN1_WBEG_OWNBR  
      *    AND    LNFILE.LN_POCD        NOT IN ('**', '*I', '*R', '*U')
      *    AND    LNFILE.LN_OWNBR            = LNFILE.LN_ORGBR 
      *    AND  ( LNFILE.LN_LOANDATE   BETWEEN @QWS_BEG_DATE
      *                                    AND @QWS_END_DATE )
      *    AND  ( LNFILE.LN_FEECODE_1        = BRFILE.BR_ADD_INS_FEECODE 
      *     OR    LNFILE.LN_FEECODE_2        = BRFILE.BR_ADD_INS_FEECODE 
      *     OR    LNFILE.LN_FEECODE_3        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_4        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_5        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_6        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_7        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_8        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_9        = BRFILE.BR_ADD_INS_FEECODE
      *     OR    LNFILE.LN_FEECODE_10       = BRFILE.BR_ADD_INS_FEECODE ))   
      *  ORDER BY LNFILE.LN_OWNBR,
      *           LNFILE.LN_ACCTNO;

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = EXEC SSP_ADDEXT_LNFILE_SELECT 
              ( :QLN1-WBEG-OWNBR, 
                :QWS-BEG-DATE, 
                :QWS-END-DATE )
           END-EXEC. 

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT. *> EMBEDDED - LNFILE, BRFILE, BWFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN1_CURSOR 
                  INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,
                   :QLN-MAKERCD4, 
                   :QLN-ORGST, 
                   :QLN-ORGBR,
                   :QLN-POCD, 
                   :QLN-LOANDATE,
                   :QLN-ORGTERM,
                   :QLN-FEECODE-1, 
                   :QLN-FEECODE-2, 
                   :QLN-FEECODE-3, 
                   :QLN-FEECODE-4, 
                   :QLN-FEECODE-5, 
                   :QLN-FEECODE-6, 
                   :QLN-FEECODE-7, 
                   :QLN-FEECODE-8, 
                   :QLN-FEECODE-9, 
                   :QLN-FEECODE-10, 
                   :QLN-FEECODE-11, 
                   :QLN-FEECODE-12, 
                   :QLN-FEECODE-13, 
                   :QLN-FEECODE-14, 
                   :QLN-FEECODE-15, 
                   :QLN-FEECODE-16, 
                   :QLN-FEECODE-17, 
                   :QLN-FEECODE-18, 
                   :QLN-FEECODE-19, 
                   :QLN-FEECODE-20, 
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QBW-OWNBR, 
                   :QBW-SSNO, 
                   :QBW-LNAME  
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN1-CONNECTION.

           IF LN1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN1-CONN-OPEN-SW.

      *-----------------------------------------------------------------


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
       READ-LN4-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-SSNO1 DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           MOVE "1900-01-01"         TO QWS-LAST-ADD-DATE.
           MOVE LN-OWNBR             TO QLN-OWNBR.
           MOVE LN-SSNO1             TO QLN-SSNO1.
           MOVE LN-ACCTNO            TO QWS-HOLD-LN-ACCTNO.
           MOVE LN-LOANDATE          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD  TO QWS-HOLD-LN-LOANDATE.

           EXEC SQL
            SELECT CAST(MAX(LNFILE.LN_LOANDATE) AS VARCHAR(10))  
              INTO :QWS-LAST-ADD-DATE 
              FROM DBO.LNFILE
             INNER JOIN
                   DBO.BRFILE BRFILE
                ON ( BRFILE.BR_NO               
                                          = LNFILE.LN_OWNBR
               AND   BRFILE.BR_ADD_INS_FEECODE 
                                         <> '  ' )
             WHERE ( LNFILE.LN_OWNBR      = :QLN-OWNBR
               AND   LNFILE.LN_SSNO1      = :QLN-SSNO1
               AND   LNFILE.LN_ACCTNO    <> :QWS-HOLD-LN-ACCTNO
               AND   LNFILE.LN_LOANDATE  <= :QWS-HOLD-LN-LOANDATE
               AND   LNFILE.LN_POCD  NOT IN ('**', '*I', '*R', '*U')
               AND ( LNFILE.LN_FEECODE_1  = BRFILE.BR_ADD_INS_FEECODE 
                OR   LNFILE.LN_FEECODE_2  = BRFILE.BR_ADD_INS_FEECODE 
                OR   LNFILE.LN_FEECODE_3  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_4  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_5  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_6  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_7  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_8  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_9  = BRFILE.BR_ADD_INS_FEECODE
                OR   LNFILE.LN_FEECODE_10 = BRFILE.BR_ADD_INS_FEECODE ))
           END-EXEC.   

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN4-FILE.

      *    IF ( IO-FG = 0 )
      *       PERFORM GET-LN-FIELDS.

           IF QWS-LAST-ADD-DATE           = "1900-01-01"
              MOVE 19000101              TO FOUND-LAST-ADD-DATE
           ELSE
              MOVE QWS-LAST-ADD-DATE     TO WS-LAST-ADD-DATE
              MOVE WS-LAST-ADD-YYYY      TO FOUND-LAST-ADD-YYYY
              MOVE WS-LAST-ADD-MM        TO FOUND-LAST-ADD-MM
              MOVE WS-LAST-ADD-DD        TO FOUND-LAST-ADD-DD.   

      *-----------------------------------------------------------------
       CLOSE-LN4-FILE.

           PERFORM CLOSE-IT.
 
      *-----------------------------------------------------------------

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       OPEN-ADDEXT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE ADDEXT-IFN TO E-FILE.
           OPEN OUTPUT ADDEXT-FILE.
           IF IO-FG = 8
              GO TO OPEN-ADDEXT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE ADDEXT-FILE
              GO TO OPEN-ADDEXT-FILE-OUTPUT.
       WRITE-ADDEXT-FILE.
           PERFORM WRITE-IT.
           MOVE ADDEXT-IFN TO E-FILE.
           WRITE ADDEXT-REC.
           IF IO-FG = 8
              GO TO WRITE-ADDEXT-FILE.
       CLOSE-ADDEXT-FILE.
           PERFORM CLOSE-IT.
           CLOSE ADDEXT-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **********************
       END-PROGRAM SECTION.
      **********************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ADDEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
