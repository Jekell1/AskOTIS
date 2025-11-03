       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SETNON.
      ************************************************************************
      *  SET LOAN STATUS FLAG "N" FOR ALL ACCOUNTS IN NON-ACCRUAL STATUS
      *          (LN-ERNDELQ-STOP = C) 
      *  SEARCHES FOR ACCOUNTS WITH LN-ERNDELQ-STOP = "C", SETS THESE 
      *  ACCOUNTS STATUS FLAG TO "N" TO SHOW NON-ACCRUAL STATUS
      *
      *  RUNS FROM BATCH OR EOCRON
      *
      *  PRINT 1 PAGE OF TOTAL UPDATED RECORDS
      *   
      * REV:
      *  20180816 BAH WORLD PR#1329
      *  BAH 181217 GLOBAL LNFILE
      *  20181016 BAH DO NOT SET LN-STATUSFG; ACCEL, REDU, AND JUDGEMENTS
      *               WONT WORK IF THIS FLAG GETS CHANGED WHEN IT WAS ONE
      *               OF THOSE STATUS'S, WORLD #1343
      *  BAH 20190930 REMOVED ACCESS-CALL LNFILE
      *  MB  20231103 PERFORMANCE TUNING - CREATED STORED PROCEDURE TO RETRIEVE
      *                 LNFILE ROWS. USES BRANCHLIST PROCESSING (RETRIEVES 
      *                 LNFILE FOR MULTIPLE BRANCHES IN 1 CURSOR)
      *  MB  20250328 ADD MOVE 'Y' TO LN-PATH-OVERRIDE     S35Q-167
      *               WAS DEFAULTED TO SPACE SO OPEN LNFILE WAS NOT CORRECT
      ************************************************************************
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
      *
      *      W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/SETNON S35 03/28/25-10:27:53 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  LOOP-PROC-SUB               PIC 9(9)        VALUE 0.
       01  WS-PROC-BRANCH              PIC 9(4)        VALUE 0.

       01  LN1-CONN-OPEN-SW            PIC X        VALUE "N".

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(5)    VALUE " ".
           03  FILLER           PIC X(42)    VALUE
           "- - - UPDATE STATUS FLAG NON-ACCRUAL - - -".

       01  DETAIL-LINE      PIC X(80) VALUE SPACES.
       01  DETAIL-L1        REDEFINES DETAIL-LINE.
           03  D-TEXT       PIC X(25).
           03  D-COUNT      PIC ZZZ,ZZZ,ZZ9.
           03  D-BRANCH REDEFINES D-COUNT PIC Z(10)9.

      *-----------------------------------------------------------------
       01  LN-FEED           PIC 9(2) VALUE ZERO.
       01  N-COUNT           PIC 9(2) VALUE ZERO.
       01  UPDATED-COUNT     PIC 9(9) COMP-3 VALUE ZERO.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9(4)    VALUE 9999.
       01  SV-END-BR           PIC 9(4)    VALUE 9999.

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

      *----------------------------------------------------------------
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SETNON_DEF.CPY".
       COPY "LIBSP/SETNON_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/SETNON_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-LN1-CONNECTION.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SETNON" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      *     IF GROUP-FLAG = "N"
      *        MOVE BEG-BR TO BR-NO
      *        PERFORM START-BR-FILE
      *        GO TO NEXT-BRANCH-NO.

      *     MOVE ENT-GROUP TO GR-ID.
      *     PERFORM SEARCH-GR-INIT.

      D    STOP MESS.

           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BR                    TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-BEG-BRGRP
               MOVE END-BR                    TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

      * SET BRANCHLIST-PGM-PROC-MAX TO NUMBER OF BRANCHES TO PROCESS PER STORED PROCEDURE CURSOR
           MOVE 500 TO BRANCHLIST-PGM-PROC-MAX.

      * THIS NEEDED TO OPEN LN1 CONNECTION
           PERFORM OPEN-LN1-FILE.

      * RETRIEVE BRANCHES WE WANT TO PROCESS FROM BGLS-BRANCH-TBL
      * WILL PROCESS UNTIL END OF BGLS-BRANCH-TBL OR BRANCHLIST-PGM-PROC-MAX
           PERFORM VARYING LOOP-PROC-SUB 
             FROM 1 BY 1 UNTIL LOOP-PROC-SUB > BGLS-BRANCHES-RETR-CTR
              ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR
                       BRANCHLIST-SUB
              MOVE BGLS-BRANCH-BRNO(LOOP-PROC-SUB) TO 
                                 BRANCHLIST-BRNO(BRANCHLIST-SUB)
              IF BRANCHLIST-SUB > 1
                 MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB)
              END-IF 
              IF BRANCHLIST-PROC-BRANCH-CTR = BRANCHLIST-PGM-PROC-MAX 
                   OR LOOP-PROC-SUB = BGLS-BRANCHES-RETR-CTR
                 MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST
                 PERFORM START-LN1-FILE
                 PERFORM PROCESS-LN-FILE-NEXT
                 PERFORM CLOSE-LN1-FILE
                 INITIALIZE BRANCHLIST-TBL BRANCHLIST-SUB
                            BRANCHLIST-PROC-BRANCH-CTR
                            PASS-BRANCHLIST
              END-IF
           END-PERFORM.

           GO TO END-ROUTINE.

      * NEXT-GROUP-LOC.
      *     IF GROUP-FLAG NOT = "Y"
      *        GO TO NEXT-BRANCH-NO.
      *
      *     PERFORM SEARCH-GR-NEXT.
      *
      *     IF WGR-SRESULT = 0
      *        GO TO END-ROUTINE.
      *
      *     MOVE WGR-SRESULT TO BR-NO.
      *     PERFORM READ-BR-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO IO-ERROR.
      *
      *     GO TO MOVE-BRANCH-PATH-INFO.
      *
      * NEXT-BRANCH-NO.
      *     PERFORM READ-BR-FILE-NEXT.
      *     IF IO-FG = 9 OR BR-NO > END-BR
      *        GO TO END-ROUTINE.
      *
      *     STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
      *                 DELIMITED BY SIZE INTO WR-BUF.
      *     MOVE MESS-LIST TO WR-LIST.
      *     PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
      *     MOVE-BRANCH-PATH-INFO.
      *     MOVE BR-MACHINE   TO LN-ALL-MACHINE.
      *     MOVE BR-DATA-PATH TO LN-ALL-DATA.
      *     MOVE LN-ALL-PATH  TO LN-PATH.
      *
      *     PERFORM OPEN-LN1-FILE.
      *
      *     MOVE LN-PATH-OWNBR TO LN-OWNBR.
      *     MOVE 0             TO LN-ACCTNO.
      *
      *     PERFORM START-LN1-FILE.

       PROCESS-LN-FILE-NEXT.
      D    STOP MESS.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD                      *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *        GO TO END-OF-GROUP.
               EXIT PARAGRAPH.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-POFFDATE NOT = 0 OR LN-PLDATE NOT = 0
      *       UNLOCK LN-FILE
              GO TO PROCESS-LN-FILE-NEXT.

           IF LN-OWNBR NOT = WS-PROC-BRANCH
              MOVE BR-MACHINE   TO LN-ALL-MACHINE
              MOVE BR-DATA-PATH TO LN-ALL-DATA
              MOVE LN-ALL-PATH  TO LN-PATH
              MOVE "Y"          TO LN-PATH-OVERRIDE
              PERFORM OPEN-LN1-FILE
              STRING "PROGRAM IN PROGRESS......BRNO: " LN-OWNBR
                       DELIMITED BY SIZE INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE LN-OWNBR TO WS-PROC-BRANCH.

      * THIS ALSO CHECKED IN STORED PROCEDURE
      * SKIP ACCOUNTS THAT ARE ALREADY "N"
           MOVE 0 TO N-COUNT.
           INSPECT LN-STATFLAGS TALLYING N-COUNT FOR ALL "N".
           IF N-COUNT > 0
      *       UNLOCK LN-FILE
              GO TO PROCESS-LN-FILE-NEXT.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-ERNDELQ-STOP = "C"
              PERFORM UPDATE-STATUSFG
      * DO NOT SET LN-STATUSFG; ACCEL, REDU, AND JUDGEMENTS WONT WOK IF THIS
      * FLAG GETS CHANGED WHEN IT WAS ONE OF THOSE STATUS'S
      *       MOVE "N" TO LN-STATUSFG
              PERFORM REWRITE-LN1-FILE
              ADD 1 TO UPDATED-COUNT.
      *    ELSE
      *       UNLOCK LN-FILE.

           GO TO PROCESS-LN-FILE-NEXT.

      * END-OF-GROUP.
      *     PERFORM CLOSE-LN1-FILE.
      *     GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM HEAD-PAGE.
           MOVE "TOTAL UPDATED RECORDS:" TO D-TEXT.
           MOVE UPDATED-COUNT TO D-COUNT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING BRANCH     :" TO D-TEXT.
           MOVE BEG-BR TO D-BRANCH.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    BRANCH     :" TO D-TEXT.
           MOVE END-BR TO D-BRANCH.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE SETNON-QUEUE-POS TO Q-POS.
           MOVE SETNON-COPIES-POS TO C-POS.
           MOVE SETNON-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           MOVE 0 TO UPDATED-COUNT.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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
              GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR NOT > END-BR
              GO TO INIT-ACCEPT-ACCEPT.
           PERFORM INVALID-RANGE.
           GO TO BR-01.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       INIT-ACCEPT-ACCEPT.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
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

      ****************************
       HEAD-PAGE SECTION.
      ****************************

           MOVE 1           TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.


      **************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      *****************************************
       UPDATE-STATUSFG SECTION.
      *****************************************
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
             AT END PERFORM PREPARE-TO-SHIFT
             WHEN (LN-STATUSFLAGS(STFG) = " ")
                MOVE "N" TO LN-STATUSFLAGS(STFG)
                GO TO END-UPDATE-STATUSFG.

       SHIFT-STATUS.

           IF STFG < 10
              IF LN-STATUSFLAGS(STFG + 1) = " "
                 GO END-UPDATE-STATUSFG
              ELSE
                 MOVE LN-STATUSFLAGS(STFG + 1) TO LN-STATUSFLAGS(STFG)
                 MOVE SPACE TO LN-STATUSFLAGS(STFG + 1)
                 SET STFG UP BY 1
                 GO TO SHIFT-STATUS.

       END-UPDATE-STATUSFG.
           EXIT.

      ***************************************
       PREPARE-TO-SHIFT SECTION.
      ***************************************

      * IF YOU DONT FIND ANY OPEN SPOTS IN THE 10 SLOTS, FORCES LAST
      * ONE TO BE WHAT YOU NEED

           SET STFG TO 1.
       SHIFT-LOOP.
           MOVE LN-STATUSFLAGS(STFG + 1) TO LN-STATUSFLAGS(STFG).
           SET STFG UP BY 1.
           IF STFG < 10
              GO TO SHIFT-LOOP.
           MOVE "N" TO LN-STATUSFLAGS(STFG).

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SETNON-CONAME.                             
           MOVE EXT-CONAME-SYSDATE TO SETNON-SYSDATE.                           
           DISPLAY SETNON-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.                  
           MOVE SETNON-FORM-FIELDS TO WR-BUF.
           MOVE SETNON-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
                                                                                
       FORM-RESET-EXIT.                                                         
           EXIT.                                                                
                                                                                
      ************************************************                          
       SETUP-LINK-INFO SECTION.                                                 
      ************************************************                          
           MOVE SETNON-HELP--FILE TO HELP-LINK-FILE.                            
           MOVE SETNON-HELP--DIR TO HELP-LINK-DIR.                              
                                                                                
      ************************************************                          
       VDU-CONVERT-FIELD SECTION.                                               
      ************************************************                          
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SETNON_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GRBRLS_SQL.CPY".

      * COPY "LIBLP/LPLN1IN.CPY".
      * COPYMEMBER: LIBLP/LPLN1IN
      ************************************************************
      *                (LPLN1IN)
      * REV:
      *  JTG 081497 CHANGED LN-SALEFIN TO LN-GIFTNET WORLD A90
      *  BLV 020698 ADDED LN-SALEFIN BACK TO BICREA CALL
      *  MJD 030398 ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  BAH 121023 ADDED FILPATH
      *  BAH 04/22/2016 ADDED LN-SMALLOAN TO BICREA CALL, REGENCY PR#5030
      *  BAH 20180316 ADDED LN-PLDATE TO BICREA CALL, WORLD #1308
      *  BAH 181130 GLOBAL LNFILE
      ************************************************************
       UPDATE-BI-FILE.
           IF IO-FG = 0
              MOVE "CL/BICREA" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   STAT
                                   LN-ACCTNO
                                   LN-BWTABLE
                                   LN-OWNBR
                                   LN-ORGST
                                   LN-SALEFIN
                                   LN-GIFTNET
                                   LN-ENTDATE
                                   LN-POFFDATE
                                   LN-PLCD
                                   LN-CONVERCD
                                   "000000000"
                                   LN-SMALLOAN
                                   LN-PLDATE
             CANCEL FORM-PROGX.
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
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE.   *> SSP - LN BR
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = 
                 EXEC SSP_SETNON_LNFILE_SELECT
                  ( :PASS-BRANCHLIST)
                END-EXEC.

            EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.   *> SSP - LN BR
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC QBR-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH LN1_CURSOR INTO 
                    :QBR-NO,
                    :QBR-DATA-PATH,
                    :QBR-MACHINE,
                    :QLN-OWNBR,
                    :QLN-ACCTNO,
                    :QLN-STATUSFG, 
                    :QLN-STATFLAGS, 
                    :QLN-PLDATE,
                    :QLN-POFFDATE,
                    :QLN-ERNDELQ-STOP
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       REWRITE-LN1-FILE. *> EMBEDDED LN

           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           PERFORM SET-LN-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
             SET LNFILE.LN_STATUSFG    = :QLN-STATUSFG,
                 LNFILE.LN_STATFLAGS   = :QLN-STATFLAGS,
                 LNFILE.LN_LTOUCH_DATE = :QLN-LTOUCH-DATE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LN1-FILE.

           IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION
              IF LN-BI-FG = "Y"
                 MOVE "R" TO STAT
                 PERFORM UPDATE-BI-FILE.

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

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SETNON-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
