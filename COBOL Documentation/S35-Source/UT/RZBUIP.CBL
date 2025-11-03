       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BIBUID.
       DATE-WRITTEN.    062001.
      ********************************************************************
      *  DESCRIPTION:  REBUILD BI-FILE -
      *    11/14/06 PROGRAM COPIED FROM BIBLD2
      *    WHICH IS ALREADY BATCHABLE AND CONTAINS THE GROUP LOGIC
      *
      *    THIS PROGRAM INCLUDES THE LOGIC THAT IF RUN F3-ALL BRANCHES
      *    OPEN THE BI-FILE TO OUTPUT(THIS COME FROM THE ORIG BIBUID).
      *
      *    NEITHER PROGRAM RE-WRITES THE BI
      *    MUST RUN THE DELETE BI FIRST IF REBUILDING
      *
      * REV:
      *  JTG 081694 ADDED LOGIC FOR BI-ENTDATE
      *  JTG 111095 CHANGED TO DELETE OLD BI-RECORD
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BLV 070298 BROUGHT FROM A60; ADDED A10 FIELDS, REMOVED
      *             ACCT# DISPLAYS
      *   CS 061114 COPIED BIBLD2 TO BIBUID. BIBLD2 INCLUDED THE BATCH JUNK
      *             & THE PROCESS IS THE SAME AS BIBUID OTHER THAN THE
      *             GROUP LOGIC PR#153 REGIONAL ACCEPT
      * BAH 160422 ADDED BI-LN-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      * BAH 180316 ADDED BI-PLDATE, WORLD PR#1308
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181203 GLOBAL LNFILE, L2G
      *
      * KEC 2019-0107 G2G; COMMENTED OUT THE OPEN OUTPUT ON THE BIFILE
      *               FOR ALL BRANCHES (F3-ALL; NOT GROUP).  
      *               NOW USING THE NEW SECTION PURGE-OPEN-OUTPUT-BI
      *               TO CLEAR THE BIFILE; CANNOT DO OPEN OUTPUT
      *               IN A34/ACU4GL.
      *
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      ******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZBUIP S35 05/20/23-14:27:25 >".
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
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".


       01  SUB               PIC 99.
       01  BRANCH            PIC 9999.
       01  BI-WRITE-CTR      PIC 9(09) VALUE ZEROES.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".

       01  LN-FEED          PIC 99       VALUE 0.

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/BIBUID_DEF.CPY".
       COPY "LIBFX/BIBUID_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/BIBUID_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BIBUID" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF EXT-ROUTE-BUF = "00"
              STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
                          DELIMITED BY SIZE INTO MESS
              PERFORM SEND-LEGEND.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM GLOBAL-UPDATE.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ***************************************************
       INITIALIZATION SECTION.
      ***************************************************

           MOVE "R"    TO P-TIMER-TYPE-SET.
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.
           MOVE ZEROES TO BI-WRITE-CTR.

           IF ( P-TIMER-TYPE-SET-VALID )
               MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
           ELSE
               MOVE "R" TO P-TIMER-TYPE.     *> REPORT OPTION

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

           DISPLAY "RUN START: " P-TIMER-BEG-EDIT.

           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       COPY "LIBGB/EOINIT.CPY".


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


           IF ( BEG-BRANCH = 0 AND END-BRANCH = 9999) AND
              ( GROUP-FLAG NOT = "Y"                )
              PERFORM PURGE-OPEN-OUTPUT-BI.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-BI1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      ***************************************************
       EO-EXEC-AUDIT SECTION.
      ***************************************************

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO BR-02.

      * DEFAULT TO USER BRANCH
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE 0 TO BEG-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE 0 TO END-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.

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
              GO TO END-PARM.

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
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO END-PARM.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

       END-PARM.

       EXIT-PARM.
           EXIT.

      ***************************************************
       DISPLAY-PARAMETERS SECTION.
      ***************************************************

           IF GROUP-FLAG = "Y"
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    P U R G E - O P E N - O U T P U T - B I
      *
      *=================================================================
      *    CANNOT DO AN OPEN OUTPUT.
      *    NOW DOING THIS STANDARD ROUTINE FOR PURGE RECORDS FOR
      *    ONLY WHEN ALL BRANCHES IS SELECTED.
      ******************************************************************
       PURGE-OPEN-OUTPUT-BI SECTION.

           MOVE ZEROES                     TO BI-SSNO.
           MOVE ZEROES                     TO BI-ACCTNO.
           MOVE ZEROES                     TO BI-OWNBR.

           PERFORM OPEN-BI1-FILE.

           PERFORM START-BI1-FILE.

       PURGE-OPEN-OUTPUT-BI-NEXT.

           PERFORM READ-BI1-FILE-NEXT.
           IF ( IO-BAD )
              PERFORM CLOSE-BI1-FILE
              GO TO PURGE-OPEN-OUTPUT-BI-EXIT.

           PERFORM DELETE-BI1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-BI-NEXT.

       PURGE-OPEN-OUTPUT-BI-EXIT.
           EXIT.

      ******************************************
       GLOBAL-UPDATE SECTION.
      ******************************************

           MOVE BR-NO TO LN-OWNBR.
           MOVE 0     TO LN-ACCTNO.


           PERFORM START-LN1-FILE.

       GLOBAL-UPDATE-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO GLOBAL-UPDATE-EXIT.

           IF ( LN-POCD-NONLOAN )
      *       UNLOCK LN-FILE
              GO TO GLOBAL-UPDATE-NEXT.

           PERFORM CREATE-BI-RECORD
               VARYING SUB FROM 1 BY 1 UNTIL SUB > 4.

           GO TO GLOBAL-UPDATE-NEXT.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      *************************************
       CREATE-BI-RECORD SECTION.
      *************************************
           IF LN-SSNO(SUB) = 0
              GO TO CREATE-BI-RECORD-EXIT.

           PERFORM CLEAR-BI-FILE.

           MOVE LN-SSNO(SUB) TO BI-SSNO.
           MOVE LN-ACCTNO    TO BI-ACCTNO.
           MOVE LN-OWNBR     TO BI-OWNBR.

           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM GETBW.
           MOVE BW-LNAME     TO BI-LNAME.
           MOVE BW-FNAME     TO BI-FNAME.

           MOVE LN-MAKERCD(SUB) TO BI-MAKERCD.
           MOVE LN-ORGST        TO BI-ORGST.
           MOVE LN-GIFTNET      TO BI-GIFTNET.
           MOVE LN-POFFDATE     TO BI-POFFDATE.
           MOVE LN-ENTDATE      TO BI-ENTDATE.
           MOVE LN-SALEFIN      TO BI-SALEFIN.

           MOVE LN-CONVERCD     TO BI-CONVERCD.
           MOVE LN-PLCD         TO BI-PLCD.
           MOVE LN-SMALLOAN     TO BI-SMALLOAN.
           MOVE LN-PLDATE       TO BI-PLDATE.

      *    PERFORM WRITE-BI1-FILE.
           IF IO-FG = 9
              IF E-CODE NOT = "22"
                STRING
                   "AN ERROR OCCURED CREATING BI-INDEX FOR SSNO: "
                      LN-SSNO(SUB)
                           DELIMITED BY SIZE
                              INTO MESS
              PERFORM SEND-MESS.

           ADD 1 TO BI-WRITE-CTR.


       CREATE-BI-RECORD-EXIT.
           EXIT.

       COPY "LIBWI/PFSWINDOWS.CPY".

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBICL.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO BIBUID-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BIBUID-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BIBUID-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BIBUID-FORM-FIELDS TO WR-BUF.
           MOVE BIBUID-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE BIBUID-HELP--FILE TO HELP-LINK-FILE.
           MOVE BIBUID-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/BIBUID_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GETBW.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "NOT FOUND" TO BW-LNAME
              MOVE SPACES TO BW-FNAME.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBI1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *
      D    STOP MESS.

      *    PERFORM TIMER-STOP.
      *    MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           DISPLAY "RUN STOP: " P-TIMER-END-EDIT.
           INSPECT P-TIMER-ELAPSED-EDIT REPLACING ALL "/" BY "-".
           DISPLAY "RUN TIME: " P-TIMER-ELAPSED-EDIT.

           STRING "BI ROWS INSERTED: " BI-WRITE-CTR
                          DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-LEGEND.

      *-----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BIBUID-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
