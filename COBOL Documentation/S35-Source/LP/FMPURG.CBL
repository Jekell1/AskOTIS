       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FMPURG.
       DATE-WRITTEN.    10/04/2015.
      *****************************************************
      *   DIR : LP
      *   DESC: FORMS REQUEST PURGE
      *         VERSION THAT CROSSES DATA PATHS
      *         NOT ON A MENU, RUNS IN BATCH
      *         WILL NOT PURGE UNPRINTED CHECKS, EVER
      *  REV:
      *   151005 BAH NEW FOR REGACC TO CROSS DATA PATHS, PR# 375
      *   BAH 181114 GLOBAL FMFILE
      *   BAH 20190930 REMOVED ACCESS-CALL FMFILE
      *****************************************************
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
           "@(#)LP/FMPURG.CBL S35 02/20/24-15:04:02 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       01  PR-IFN               PIC XX      VALUE "PR".

       01  LN-FEED          PIC 99       VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  FORM-NO         PIC 999.
           03  PURGE-TYPE      PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".
       01  FORM-CLASS          PIC X.

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  PURG-SEQ            VALUE 1.
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/FMPURG_DEF.CPY".
       COPY "LIBLP/FMPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/FMPURG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **********************************
       MAIN-MODULE SECTION.
      **********************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FMPURG" TO FORM-PROG VDU-FORM-NAME.

           MOVE "1" TO FORM-CLASS.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF ( GROUP-FLAG = "N" )
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
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9  OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE     TO FILPATH-BASE.
           MOVE BR-MACHINE           TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH         TO FILPATH-DATA.

           MOVE "B"                  TO FM-PATH-OVERRIDE
                                        GB-PATH-OVERRIDE.

      * READ GLOBAL TO HAVE CHECK ID
           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           PERFORM OPEN-FM1-FILE.
           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE SPACES        TO FM-INITKEY.
           MOVE 0             TO FM-FMNO.

           PERFORM START-FM1-FILE.

       NEXT-FM.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-OF-GROUP.

      *    UNLOCK FM-FILE.
           IF FM-BRNO < BEG-BRANCH OR > END-BRANCH  
              GO TO NEXT-FM.                        

           IF FORM-NO NOT = 0
              IF FM-FMNO NOT = FORM-NO
                 GO TO NEXT-FM.

           IF PURGE-TYPE = "P"
              IF FM-PRINT NOT = "Y"
                 GO TO NEXT-FM.

           IF PURGE-TYPE = "N"
              IF FM-PRINT NOT = " "
                 GO TO NEXT-FM.

           IF FORM-NO NOT = 0
              GO TO FORM-PURGE-CONT.

           MOVE FM-FMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              GO TO NEXT-FM.

      *    LOAN FORMS
           IF FORM-CLASS = "1"
              IF FD-TYPE = "N"
                 GO TO NEXT-FM.
      *    NOTE FORMS
           IF FORM-CLASS NOT = "1"
              IF FD-TYPE NOT = "N"
                 GO TO NEXT-FM.

       FORM-PURGE-CONT.
           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF LPWSFM-EDIT-FM-TYPE NOT NUMERIC
              MOVE FM-FMNO TO FD-FMNO
              PERFORM READ-FD1-FILE
              IF IO-FG = 0
                 IF FD-PROG = GB-CHECK-PROG
                    IF FM-PRINT NOT = "Y"
                       GO TO NEXT-FM.

           PERFORM READ-FM1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM DELETE-FM1-FILE.

           GO TO NEXT-FM.

       END-OF-GROUP.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-GB-FILE.


           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE FMPURG-QUEUE-POS TO Q-POS.
           MOVE FMPURG-COPIES-POS TO C-POS.
           MOVE FMPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-FD1-FILE.

           IF FORM-NO NOT = 0
              MOVE FORM-NO TO FD-FMNO
              PERFORM READ-FD1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************
           PERFORM OPEN-FD1-FILE.

           IF FORM-NO = 0
              GO TO EXIT-AUDIT.

           MOVE FORM-NO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              MOVE "FORM# NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO EXT-EOBUF-ERRCD.

           IF FORM-CLASS = "1"
              IF FD-TYPE = "N"
                 MOVE 9 TO IO-FG.

           IF FORM-CLASS NOT = "1"
              IF FD-TYPE NOT = "N"
                 MOVE 9 TO IO-FG.

           IF IO-FG = 9
              MOVE "INVALID FORM# FOR THIS PURGE" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO EXT-EOBUF-ERRCD.

       EXIT-AUDIT.
           EXIT.

      ***************************
       ENTER-PARAMETERS SECTION.
      ***************************
       ENTER-PARMS.

           IF VDUSTATUS = "1U"
              GO TO PRGTYP.

           IF EXT-ROUTE-BUF = "00"
              MOVE PURG-SEQ TO PASSWDW-SEQ
              MOVE "TO RUN FORM PURGE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "E" TO EXT-EOBUF-ERRCD
                 MOVE "1>" TO VDUSTATUS
                 GO TO EXIT-PARM.
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16" GO TO ALLBRN.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.


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

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLBRN.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           GO TO RRFMNO.
       ALLBRN.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO BR-01.
       RRFMNO.
           MOVE "ENNN030FM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-FM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRFMNO.
           MOVE VDUNUM0 TO FORM-NO.

           IF FORM-NO = 0
              GO TO RRFMNO.

           GO TO PRGTYP.
       ALL-FM.
           MOVE "SNNN030FM." TO VDU.
           MOVE "000" TO FORM-NO VDUNUM0.
           PERFORM SCREENIO.
       PRGTYP.
           MOVE "E  A010PA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO RRFMNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRGTYP.
           MOVE VDUBUF TO PURGE-TYPE.

           IF PURGE-TYPE = "P" GO TO OK-YN.
           IF PURGE-TYPE = "N" GO TO OK-YN.
           IF PURGE-TYPE NOT = "A" GO TO PRGTYP.

       OK-YN.
       EXIT-PARM.
           EXIT.

      *****************************
       DISPLAY-PARAMETERS SECTION.
      *****************************

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

           MOVE "SNNN030FM." TO VDU.
           MOVE FORM-NO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010PA." TO VDU.
           MOVE PURGE-TYPE TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/DATER.CPY".


      *******************************
       VERIFY-PASSWORD SECTION.
      *******************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "FMPURG" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO FMPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FMPURG-SYSDATE.
           DISPLAY FMPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FMPURG-FORM-FIELDS TO WR-BUF.
           MOVE FMPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE FMPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE FMPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/FMPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SPACES TO ERRLOGW-KEY.
           STRING BEG-BRANCH,"-",END-BRANCH,",",FORM-NO,
                PURGE-TYPE DELIMITED BY "  "
                      INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY FMPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
