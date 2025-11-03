       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RTCEXT.
       DATE-WRITTEN.    03/31/2023.
      ***************************************************************
      *
      *    DESC: RECOVERY NEEDS TO PROVIDE A LIST OF ACCOUNTS THAT
      *          HAVE RECEIVED A RIGHT TO CURE IN "MO" AS PART OF THE SALES
      *          AGREEMENT WITH A BUYER. NEED EXTRACT TO PROVIDE
      *          NECESSARY INFO
      *
      *          POOLID RANGE
      *          CHARGE OFF DATE RANGE
      *          SKIP 0 BAL AND BRANCH TRANSFERS
      *
      *    CREATES TAB DELIMITED R1/FTP/RECOVERY/RTCDATE.YYMMDD
      *
      *  REV:
      * BAH 20230403 #1599
      * BAH 2025.0217 NOTICED IF WASNT SETTING THE QLN-WBEG-OWNBR S35Q-166
      * RMZ 2025.0418 PERFORMANCE IMPROVEMENT - ADDED STORED PROCEDURE
      *               TO EXTRACT ALL DATA NEEDED FOR OUTPUT FILE, FOR
      *               EACH BRANCH PROCESSED. 
      *             * BECAUSE A15 ALLOWS A LN_PLDATE OF '00/00/00' TO BE
      *               PART OF THE EXPORT FILE, S35'S SSP ALSO ALLOWS FOR 
      *               '1900-01-01' TO BE PART OF THE EXPORT FILE.
      *             * S35 SOMETIMES PLACES 'INVALID' IN RTC-BUYER-NAME
      *               WHILE A15 PLACES SPACES. THERE IS NOTHING THAT CAN    
      *               BE DONE ABOUT THIS BECAUSE A15 USES BMFILE TO 
      *               GET THIS VALUE WHILE S35 USES BWFILE. (S35Q-166)   
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT RTC-FILE ASSIGN OUTPUT RTC-PATH
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
       FD  RTC-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RTC-REC.
           03  RTC-BYTE                PIC X OCCURS 2000 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/RTCEXT S35 04/18/25-11:17:40 >".

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
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-BEG-DLPOOL         PIC 9(5).             
       01  QWS-END-DLPOOL         PIC 9(5).                   
       01  QWS-BEG-DATE           PIC X(10).
       01  QWS-END-DATE           PIC X(10). 
       01  QWS-TOTAL-RECORDS      PIC 9(13). 
       01  QWS-TOTAL-BYPASSED     PIC 9(13). 
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN1-CONN-OPEN-SW       PIC X        VALUE "N".
       01  LN1-CONN-OPEN-TOTAL-SW PIC X        VALUE "N".

       01  RTC-PATH.
           03  RTC-PATH-BASE      PIC X(9).
           03  FILLER             PIC X  VALUE "/".
           03  RTC-PATH-MACHINE   PIC X(2).
           03  RTC-PATH-DIR-FTP   PIC X(14) 
                                  VALUE "/FTP/RECOVERY/".
           03  FILLER             PIC X(8) 
                                  VALUE "RTCDATE.".
           03  RTC-PATH-YR        PIC 99.
           03  RTC-PATH-MO        PIC 99.
           03  RTC-PATH-DA        PIC 99.

       01  HOLD-RTC-REC.
           03  RTC-LOANID.
               05  RTC-OWNBR      PIC 9(4).
               05  RTC-NUMBER     PIC 9(6).
           03  RTC-OWNBR2         PIC 9(4).
           03  RTC-NUMBER2        PIC 9(6).
           03  RTC-CLASS          PIC 9(2).
           03  RTC-FNAME          PIC X(16).
           03  RTC-LNAME          PIC X(16).
           03  RTC-LOANDATE       PIC 99/99/99.
           03  RTC-PLDATE         PIC 99/99/99.
           03  RTC-POOLID         PIC 9(5).
           03  RTC-COLLCD         PIC XX.
           03  RTC-LAST-RTCDATE   PIC 99/99/99.
           03  RTC-BUYER-NAME     PIC X(45).


       01  RTC-IFN        PIC X(4)     VALUE "WKLN".


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
       01  FIRST-TIME           PIC X        VALUE "Y".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TAB-1                PIC X VALUE X"09".
       01  FOUND-LAST-RTCDATE   PIC 9(8).
 
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
           03  BEG-DLPOOL      PIC 9(5).
           03  END-DLPOOL      PIC 9(5).
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
           " ---- RECOVERY RTC DATE EXTRACT ----".

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
       COPY "LIBSP/RTCEXT_DEF.CPY".
       COPY "LIBSP/RTCEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/RTCEXT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE RTC-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LN1-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-CJ2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE RTC-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RTCEXT" TO VDU-FORM-NAME.

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
                              BW-PATH-OVERRIDE
                              CJ-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-CJ2-FILE.

           PERFORM GLOBAL-UPDATE.

           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-CJ2-FILE.

           PERFORM OPEN-LN1-TOTAL-FILE.
           PERFORM START-LN1-TOTAL-FILE.      *> SSP - LN,CJ
           PERFORM READ-LN1-TOTAL-FILE-NEXT.  *> EMBEDDED - LNFILE
           PERFORM CLOSE-LN1-TOTAL-FILE.

           ADD QWS-TOTAL-RECORDS  TO TOTAL-RECORDS.
           ADD QWS-TOTAL-BYPASSED TO TOTAL-BYPASSED.

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

           MOVE BR-NO     TO LN-OWNBR
                             QLN1-WBEG-OWNBR
                             QLN1-WEND-OWNBR.
           MOVE 0         TO LN-ACCTNO
                             QLN1-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN1-WEND-ACCTNO.

           MOVE BEG-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QWS-BEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QWS-END-DATE.
           MOVE BEG-DLPOOL          TO QWS-BEG-DLPOOL.
           MOVE END-DLPOOL          TO QWS-END-DLPOOL.

           PERFORM START-LN1-FILE.  *> SSP - LN,BW,CJ

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT. *> EMBEDDED - LN,BW
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-GLOBAL-UPDATE.

      *    ADD 1 TO TOTAL-RECORDS.

           IF LN-OWNBR > END-BR
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-DLPOOL < BEG-DLPOOL OR LN-DLPOOL > END-DLPOOL
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-POCD-NONLOAN
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * SKIP ZERO BALANCES
           IF LN-CURBAL = 0 AND LN-INTBAL = 0 AND LN-LCBAL = 0
              AND LN-OTHBAL = 0
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.


      *** COMMENTED OUT BELOW BECAUSE DATES OF 1900-01-01 ARE ALLOWED
      *** TO BE PASSED, BECAUSE A15 DOES IT. THE STORED PROCEDURE  
      *** DOES THIS WORK. (RMZ - 04-22-2025 - (S35Q-166))
      *
      *    IF LN-PLDATE < BEG-DATE OR LN-PLDATE > END-DATE
      *       ADD 1 TO TOTAL-BYPASSED
      *       GO TO NEXT-LN.

           PERFORM CREATE-RTC-WORK.

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE RTCEXT-QUEUE-POS TO Q-POS.
           MOVE RTCEXT-COPIES-POS TO C-POS.
           MOVE RTCEXT-PRU-POS   TO P-POS.
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

           MOVE 0 TO TOTAL-RECORDS 
                     TOTAL-BYPASSED
                     TOTAL-INCLUDED
                     QWS-TOTAL-RECORDS 
                     QWS-TOTAL-BYPASSED.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.

           MOVE EXT-FILPATH-BASE    TO RTC-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO RTC-PATH-MACHINE.

           MOVE DATE-YYYYMMDD-DD TO RTC-PATH-DA.
           MOVE DATE-YYYYMMDD-MM TO RTC-PATH-MO.
           MOVE DATE-YYYYMMDD-YY TO RTC-PATH-YR.

           CALL "C$TOLOWER" USING RTC-PATH-DIR-FTP,VALUE 14.
           PERFORM OPEN-RTC-FILE-OUTPUT.

      * CREATE HEADER
           STRING "LOANID",   TAB-1,
                  "OWNBR",    TAB-1,
                  "ACCTNO",   TAB-1,
                  "CLASS",    TAB-1,
                  "FIRST",    TAB-1,
                  "LAST",     TAB-1,
                  "LOANDATE", TAB-1,
                  "P&L DATE", TAB-1,
                  "POOLID",   TAB-1,
                  "COLLCD",   TAB-1,
                  "LASTRTC",  TAB-1,
                  "BUYER",    CR-1
              DELIMITED BY SIZE INTO RTC-REC.

           PERFORM WRITE-RTC-FILE.

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
              GO TO POOL-01.
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
           GO TO POOL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       POOL-01.
           MOVE "ENNN050POOL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-POOL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO POOL-01.
           MOVE VDUNUM0 TO BEG-DLPOOL.
       POOL-02.
           MOVE "ENNN050POOL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO POOL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO POOL-02.
           MOVE VDUNUM0 TO END-DLPOOL.
           IF BEG-DLPOOL > END-DLPOOL
              PERFORM INVALID-RANGE
              GO TO POOL-01.
           GO TO BEGDATE-01.
       ALL-POOL.
           MOVE "SNNN050POOL1." TO VDU.
           MOVE 0 TO BEG-DLPOOL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN050POOL2." TO VDU.
           MOVE 99999 TO END-DLPOOL VDUNUM0.
           PERFORM SCREENIO.
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO POOL-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
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

      *----------------------------------------------------------
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

           MOVE "SN N050POOL1." TO VDU.
           MOVE BEG-DLPOOL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N050POOL2." TO VDU.
           MOVE END-DLPOOL TO VDUNUM0.
           PERFORM SCREENIO.

      ****************************
       CREATE-RTC-WORK SECTION.
      ****************************

           MOVE SPACES TO HOLD-RTC-REC RTC-REC.

      *    PERFORM FIND-LAST-RTCDATE.

           MOVE CJ-DATE TO FOUND-LAST-RTCDATE.

           IF FOUND-LAST-RTCDATE = 0
              GO TO CREATE-RTC-WORK-EXIT.

           MOVE FOUND-LAST-RTCDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RTC-LAST-RTCDATE.

           MOVE LN-OWNBR   TO RTC-OWNBR RTC-OWNBR2.
           MOVE LN-ACCTNO  TO RTC-NUMBER RTC-NUMBER2.
           MOVE LN-CLASS   TO RTC-CLASS.

      *    MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-BAD
           IF BW-SSNO = 0
              MOVE "INVALID" TO BW-LNAME BW-FNAME.
           MOVE BW-FNAME    TO RTC-FNAME.
           MOVE BW-LNAME    TO RTC-LNAME.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RTC-LOANDATE.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO RTC-PLDATE.

           MOVE LN-DLPOOL TO RTC-POOLID.
           MOVE LN-COLLCD TO RTC-COLLCD.

           MOVE BW-CONTACT-DESC(10) TO RTC-BUYER-NAME.

           IF RTC-BUYER-NAME = SPACES
              MOVE "INVALID" TO RTC-BUYER-NAME.

           STRING
              RTC-LOANID           ,TAB-1,
              RTC-OWNBR2           ,TAB-1,
              RTC-NUMBER2          ,TAB-1,
              RTC-CLASS            ,TAB-1,
              RTC-FNAME            ,TAB-1,
              RTC-LNAME            ,TAB-1,
              RTC-LOANDATE         ,TAB-1,
              RTC-PLDATE           ,TAB-1,
              RTC-POOLID           ,TAB-1,
              RTC-COLLCD           ,TAB-1,
              RTC-LAST-RTCDATE     ,TAB-1,
              RTC-BUYER-NAME       ,CR-1
              DELIMITED BY SIZE INTO RTC-REC.

           PERFORM WRITE-RTC-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "N" TO FIRST-TIME.
           ADD 1 TO TOTAL-INCLUDED.

       CREATE-RTC-WORK-EXIT.
           EXIT.

      ****************************
        FIND-LAST-RTCDATE SECTION.
      ****************************

      * READ THROUGH THE CJFILE AND LOOK FOR ANY RECORDS WITH 610, 611 OR
      * 612 IN CJ-FORM-NO

           MOVE 0 TO FOUND-LAST-RTCDATE.

           MOVE LN-OWNBR               TO CJ-BRNO
                                          QCJ2-WBEG-BRNO
                                          QCJ2-WEND-BRNO.
           MOVE LN-ACCTNO              TO CJ-ACCTNO
                                          QCJ2-WBEG-ACCTNO
                                          QCJ2-WEND-ACCTNO.

           MOVE 19000101               TO CJ-DATE
                                          SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD    TO QCJ2-WBEG-DATE.
           MOVE 99991231               TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD    TO QCJ2-WEND-DATE.
           MOVE 0                      TO CJ-START-TIME
                                          QCJ2-WBEG-START-TIME.
           MOVE ALL "9"                TO QCJ2-WEND-START-TIME.

           PERFORM START-CJ2-FILE.

       FIND-NEXT-CJ2.
           PERFORM READ-CJ2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-FIND-LAST-RTCDATE.

           IF CJ-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-FIND-LAST-RTCDATE.

           IF CJ-FORM-NO = 610 OR 611 OR 612
              MOVE CJ-DATE TO FOUND-LAST-RTCDATE.

           GO TO FIND-NEXT-CJ2.

       EXIT-FIND-LAST-RTCDATE.
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


           MOVE "BEG POOL - END POOL" TO RANGE-SELECTION.
           MOVE BEG-DLPOOL TO RANGE1N.
           MOVE END-DLPOOL TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG PLDATE - END PLDATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE RTC-PATH TO RANGE-LINE.
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
           MOVE EXT-CONAME-CONAME TO RTCEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RTCEXT-SYSDATE.
           DISPLAY RTCEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RTCEXT-FORM-FIELDS TO WR-BUF.
           MOVE RTCEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE RTCEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE RTCEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/RTCEXT_EVA.CPY".
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
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      *COPY "LIBLP/LPLN1RN.CPY".
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
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       OPEN-LN1-TOTAL-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN1-CONN-OPEN-TOTAL-SW = "N" 
              MOVE "Y"         TO LN1-CONN-OPEN-TOTAL-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER 
			                   AS LN1_TOTAL_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE.
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

      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SSNO1,
      *          LNFILE.LN_MAKERCD1, 
      *          LNFILE.LN_SSNO2,
      *          LNFILE.LN_MAKERCD2, 
      *          LNFILE.LN_SSNO3,
      *          LNFILE.LN_MAKERCD3, 
      *          LNFILE.LN_SSNO4,
      *          LNFILE.LN_MAKERCD4, 
      *          LNFILE.LN_COLLCD, 
      *          LNFILE.LN_POCD, 
      *          CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)) AS LN_LOANDATE, 
      *          CAST(LNFILE.LN_PLDATE   AS VARCHAR(10)) AS LN_PLDATE, 
      *          LNFILE.LN_DLPOOL,
      *          LNFILE.LN_CURBAL,
      *          LNFILE.LN_INTBAL,
      *          LNFILE.LN_LCBAL,
      *          LNFILE.LN_OTHBAL,
      *          ISNULL(BWFILE.BW_OWNBR, 0)              AS BW_OWNBR,  
      *          ISNULL(BWFILE.BW_SSNO, 0)               AS BW_SSNO,
      *          ISNULL(BWFILE.BW_LNAME,'')              AS BW_LNAME, 
      *          ISNULL(BWFILE.BW_FNAME,'')              AS BW_FNAME, 
      *          ISNULL(BWFILE.BW_CONTACT_DESC_10,'')    AS BW_CONTACT_DESC_10,
      *          CJFILE.CJ_BRNO,
      *          CJFILE.CJ_ACCTNO,
      *          CAST(CJFILE.CJ_DATE AS VARCHAR(10))     AS CJ_DATE 
      * 
      *     FROM DBO.LNFILE      LNFILE
      * 
      *     LEFT JOIN DBO.BWFILE BWFILE 
      *       ON  ( LNFILE.LN_OWNBR  = BWFILE.BW_OWNBR     
      *      AND    LNFILE.LN_SSNO1  = BWFILE.BW_SSNO )
      * 
      *    INNER JOIN 
      * 
      * ( SELECT CJFILE1.CJ_BRNO,
      *          CJFILE1.CJ_ACCTNO,
      *          MAX(CJFILE1.CJ_DATE) AS CJ_DATE
      *     FROM DBO.CJFILE CJFILE1
      *    WHERE CJFILE1.CJ_BRNO     = @QLN1_WBEG_OWNBR
      *      AND CJFILE1.CJ_FORM_NO IN (610, 611, 612)
      *    GROUP BY 
      *          CJFILE1.CJ_BRNO,
      *          CJFILE1.CJ_ACCTNO )  CJFILE  
      * 
      *       ON  ( LNFILE.LN_OWNBR      = CJFILE.CJ_BRNO             
      *      AND    LNFILE.LN_ACCTNO     = CJFILE.CJ_ACCTNO )
      * 
      *    WHERE (( LNFILE.LN_OWNBR     = @QLN1_WBEG_OWNBR ) 
      *      AND  ( LNFILE.LN_POCD    NOT IN ('**', '*I', '*R', '*U' ))
      *      AND  ( LNFILE.LN_DLPOOL BETWEEN @QWS_BEG_DLPOOL
      *                                  AND @QWS_END_DLPOOL )
      *      AND  ( LNFILE.LN_PLDATE BETWEEN @QWS_BEG_DATE
      *                                  AND @QWS_END_DATE )
      *      AND  ( LNFILE.LN_CURBAL       > 0 
      *       OR    LNFILE.LN_INTBAL       > 0 
      *       OR    LNFILE.LN_LCBAL        > 0
      *       OR    LNFILE.LN_OTHBAL       > 0 ))
      * 
      *    ORDER BY LNFILE.LN_OWNBR,
      *             LNFILE.LN_ACCTNO; 

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = EXEC SSP_RTCEXT_LNFILE_SELECT 
              ( :QLN1-WBEG-OWNBR, 
                :QWS-BEG-DLPOOL,
                :QWS-END-DLPOOL,
                :QWS-BEG-DATE,
                :QWS-END-DATE )
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN1-WBEG-OWNBR
      *                   QLN1-WEND-OWNBR
      *                   QLN1-WBEG-ACCTNO
      *                   QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       START-LN1-TOTAL-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-TOTAL-FILE.

      *    IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
      *       ( QLN1-WEND-OWNBR NOT NUMERIC )
      *       MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
      *       MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
      *       MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          SUM(1)                       AS QWS_TOTAL_RECORDS,
      *          SUM(CASE WHEN LNFILE.LN_POCD IN ('**', '*I', '*R', '*U' )
      *                   THEN 1
      *                   WHEN LNFILE.LN_DLPOOL < @QWS_BEG_DLPOOL
      *                     OR LNFILE.LN_DLPOOL > @QWS_END_DLPOOL 
      *                   THEN 1
      *                   WHEN LNFILE.LN_PLDATE < @QWS_BEG_DATE
      *                     OR LNFILE.LN_PLDATE > @QWS_END_DATE 
      *                   THEN 1
      *                   WHEN LNFILE.LN_CURBAL = 0
      *                    AND LNFILE.LN_INTBAL = 0
      *                    AND LNFILE.LN_LCBAL  = 0
      *                    AND LNFILE.LN_OTHBAL = 0
      *                   THEN 1
      *                   ELSE 0
      *               END)                     AS QWS_TOTAL_BYPASSED
      * 
      *     FROM DBO.LNFILE      LNFILE
      *
      *    INNER JOIN 
      * ( SELECT CJFILE3.CJ_BRNO,
      *          CJFILE3.CJ_ACCTNO,
      *          CJFILE3.CJ_SEQNO,
      *          CJFILE3.CJ_COLLID,  
      *          CJFILE3.CJ_DATE,
      *          CJFILE3.CJ_START_TIME,
      *          CJFILE3.CJ_FORM_NO
      *     FROM DBO.CJFILE CJFILE3
      *
      *    INNER JOIN
      *
      * ( SELECT TEMP1.CJ_BRNO,
      *          TEMP1.CJ_ACCTNO,
      *          TEMP1.CJ_DATE,
      *          MAX(CJFILE2.CJ_START_TIME) AS CJ_START_TIME
      *     FROM DBO.CJFILE CJFILE2
      *
      *    INNER JOIN
      *
      * ( SELECT CJFILE1.CJ_BRNO,
      *          CJFILE1.CJ_ACCTNO,
      *          MAX(CJFILE1.CJ_DATE)       AS CJ_DATE
      *     FROM DBO.CJFILE CJFILE1
      *    WHERE CJFILE1.CJ_BRNO           = @QLN1_WBEG_OWNBR
      *      AND CJFILE1.CJ_FORM_NO       IN (610, 611, 612)
      *    GROUP BY 
      *          CJFILE1.CJ_BRNO,
      *          CJFILE1.CJ_ACCTNO ) TEMP1
      *
      *       ON ( CJFILE2.CJ_BRNO         = TEMP1.CJ_BRNO
      *      AND   CJFILE2.CJ_ACCTNO       = TEMP1.CJ_ACCTNO
      *      AND   CJFILE2.CJ_DATE         = TEMP1.CJ_DATE )
      *    GROUP BY TEMP1.CJ_BRNO,
      *             TEMP1.CJ_ACCTNO,
      *             TEMP1.CJ_DATE ) TEMP2
      *
      *       ON ( CJFILE3.CJ_BRNO         = TEMP2.CJ_BRNO
      *      AND   CJFILE3.CJ_ACCTNO       = TEMP2.CJ_ACCTNO
      *      AND   CJFILE3.CJ_DATE         = TEMP2.CJ_DATE 
      *      AND   CJFILE3.CJ_START_TIME   = TEMP2.CJ_START_TIME )) CJFILE  
      *
      *       ON  ( LNFILE.LN_OWNBR        = CJFILE.CJ_BRNO             
      *      AND    LNFILE.LN_ACCTNO       = CJFILE.CJ_ACCTNO )
      *
      *    WHERE (  LNFILE.LN_OWNBR        = @QLN1_WBEG_OWNBR ) 
      *    GROUP BY LNFILE.LN_OWNBR;

           EXEC SQL SET CONNECTION LN1_TOTAL_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_TOTAL_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = EXEC SSP_RTCEXT_LNFILE_TOTAL_SELECT 
              ( :QLN1-WBEG-OWNBR, 
                :QWS-BEG-DLPOOL,
                :QWS-END-DLPOOL,
                :QWS-BEG-DATE,
                :QWS-END-DATE )
           END-EXEC.

           EXEC SQL
               OPEN LN1_TOTAL_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN1-WBEG-OWNBR
      *                   QLN1-WEND-OWNBR
      *                   QLN1-WBEG-ACCTNO
      *                   QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QCJ-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN1_CURSOR 
                 INTO 
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SSNO1,
                  :QLN-MAKERCD1, 
                  :QLN-SSNO2,
                  :QLN-MAKERCD2, 
                  :QLN-SSNO3,
                  :QLN-MAKERCD3, 
                  :QLN-SSNO4,
                  :QLN-MAKERCD4, 
                  :QLN-COLLCD, 
                  :QLN-POCD, 
                  :QLN-LOANDATE,
                  :QLN-PLDATE,
                  :QLN-DLPOOL,
                  :QLN-CURBAL,
                  :QLN-INTBAL,
                  :QLN-LCBAL,
                  :QLN-OTHBAL,
                  :QBW-OWNBR, 
                  :QBW-SSNO,  
                  :QBW-LNAME, 
                  :QBW-FNAME, 
                  :QBW-CONTACT-DESC-10, 
                  :QCJ-BRNO,
                  :QCJ-ACCTNO,
                  :QCJ-DATE 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS
              PERFORM GET-CJ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN1-TOTAL-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
 
           EXEC SQL SET CONNECTION LN1_TOTAL_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN1_TOTAL_CURSOR 
                 INTO 
                  :QLN-OWNBR,
                  :QWS-TOTAL-RECORDS,
                  :QWS-TOTAL-BYPASSED
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-TOTAL-FILE-NEXT.

      *    IF ( IO-FG = 0 )
      *       PERFORM GET-LN-FIELDS
      *       PERFORM GET-BW-FIELDS
      *       PERFORM GET-CJ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC. 
      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-TOTAL-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN1_TOTAL_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN1_TOTAL_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-CONNECTION.

           IF LN1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN1-CONN-OPEN-SW.

           IF LN1-CONN-OPEN-TOTAL-SW = "Y"
              EXEC SQL SET CONNECTION LN1_TOTAL_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN1_TOTAL_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN1-CONN-OPEN-TOTAL-SW.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBSP/SPCJ2RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       OPEN-RTC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE RTC-IFN TO E-FILE.
           OPEN OUTPUT RTC-FILE.
           IF IO-FG = 8
              GO TO OPEN-RTC-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE RTC-FILE
              GO TO OPEN-RTC-FILE-OUTPUT.
       WRITE-RTC-FILE.
           PERFORM WRITE-IT.
           MOVE RTC-IFN TO E-FILE.
           WRITE RTC-REC.
           IF IO-FG = 8
              GO TO WRITE-RTC-FILE.
       CLOSE-RTC-FILE.
           PERFORM CLOSE-IT.
           CLOSE RTC-FILE.

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
              DESTROY RTCEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
