       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DELFEE.
       AUTHOR.             BLM.
       DATE-WRITTEN.  05/20/09.
      *********************************************************************
      *                        (SP)
      * DESC: DELETE FEE CODES BY BRANCH
      *       THIS IS A VARIATION OF THE REGULAR FEE DELETE UTILITY FOR
      *       FOR WORLD.  NEED ABILITY TO DELETE A RANGE OF FEE CODES TO
      *       CLEAN UP WCBC FEE CODES.  WANTED SEPARATE UTILITY SINCE
      *       ALPHA RANGE IS SCARY!  ALSO NEED SOME CUSTOM WCBC CHECKS &
      *       ABILITY TO DELETED FEE CODES FOR A DELETED BRANCH.
      *       WORLD PR# 636
      *
      * REV:
      *
      * KEC 2016.0318 CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0325 REMOVED THE USE OF CDSCAN; NOT BEING USED.
      * BAH 190130 CHANGED CDFILE TO CDBFILE
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/DELFEE.CBL S35 02/23/24-11:30:12 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  PR-IFN        PIC X(2)    VALUE "PR".

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(25)    VALUE
           "DELETE IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).

       01  LN-FEED              PIC 9.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  DEL-CD-COUNT         PIC 9(6)     VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  CD1             PIC XX.
           03  CD2             PIC XX.
           03  WCBC-ONLY-FG    PIC X.
           03  DEL-BRANCH-FG   PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/DELFEE_DEF.CPY".
       COPY "LIBSP/DELFEE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/DELFEE_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DELFEE" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "Y"
              MOVE  ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           MOVE "GC"       TO CDB-TYPE
                              QCDB1-WBEG-TYPE
                              QCDB1-WEND-TYPE.
           MOVE BEG-BRANCH TO CDB-BRANCH
                              QCDB1-WBEG-BRANCH.
           IF GROUP-FLAG = "Y"
              MOVE BEG-BRANCH TO QCDB1-WEND-BRANCH
           ELSE
              MOVE END-BRANCH TO QCDB1-WEND-BRANCH.
           MOVE SPACES        TO CDB-CODE
                                 QCDB1-WBEG-CODE.
           MOVE ALL "Z"       TO QCDB1-WEND-CODE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           PERFORM START-CDB1-FILE.
       NEXT-CD.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF CDB-TYPE NOT = "GC"
              GO TO END-ROUTINE.

           IF GROUP-FLAG = "Y"
              MOVE CDB-BRANCH TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 GO TO NEXT-CD
              END-IF
           ELSE
              IF CDB-BRANCH < BEG-BRANCH OR CDB-BRANCH > END-BRANCH
                 GO TO NEXT-CD.

           IF (CDB-CODE < CD1) OR (CDB-CODE > CD2)
              GO TO NEXT-CD.

           IF WCBC-ONLY-FG = "Y"
              IF CDB-GC-FEE-GIFTNET NOT = "Y"
                 GO TO NEXT-CD.

           MOVE "Y" TO RECORDS-FOUND.
           MOVE "N" TO FIRST-TIME.

           PERFORM DELETE-CDB1-FILE.
           ADD 1 TO DEL-CD-COUNT.

           GO TO NEXT-CD.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "&&&&&& FEE CODES DELETED." TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&&&" BY DEL-CD-COUNT.
           PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE 0 TO Q-POS C-POS P-POS.
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

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-CDB1-FILE.

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
              GO TO ENTER-ENDCD.
       ENTER-DELETED-BRANCH-FG.
           MOVE "E  A010DELBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ENTER-DELETED-BRANCH-FG.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ENTER-DELETED-BRANCH-FG.
           MOVE VDUBUF TO DEL-BRANCH-FG.

      *-----------------------------------------------------------------
           IF VDUSTATUS = "1U"
              GO TO ENTER-WCBC-FG.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DELETED-BRANCH-FG.
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

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF DEL-BRANCH-FG = "Y"
              IF WGR-TYPE = "A"
                 MOVE
                 "PLEASE ENTER BRANCH RANGE OF 1 DELETED BRANCH"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO BR-01
              ELSE
                 IF WGR-ERR = 0
                 MOVE
                 "BRANCH NOT DELETED; PLEASE SELECTED DELETED BRANCH"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO BR-01.

           IF DEL-BRANCH-FG = "N"
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
              MOVE 999 TO END-BRANCH
              GO TO ENTER-WCBC-FG.
      *----------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF (BEG-BRANCH = 0 AND END-BRANCH = 0)
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           IF DEL-BRANCH-FG = "Y"
              IF BEG-BRANCH NOT = END-BRANCH
                 MOVE
                 "PLEASE ENTER BRANCH RANGE OF 1 DELETED BRANCH"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO BR-01
              ELSE
                 MOVE "S  A020CD1." TO VDU
                 MOVE "  " TO CD1 VDUBUF
                 PERFORM SCREENIO
                 MOVE "S  A020CD2." TO VDU
                 MOVE ALL "Z" TO CD2 
                 CALL "C$TOLOWER" USING CD2, VALUE 2
                 MOVE CD2 TO VDUBUF
                 PERFORM SCREENIO
                 MOVE "S  A010WCBC." TO VDU
                 MOVE "N" TO WCBC-ONLY-FG VDUBUF
                 PERFORM SCREENIO
                 GO TO EXIT-PARM.

           GO TO ENTER-WCBC-FG.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *--------------------------------------------------------------
       ENTER-WCBC-FG.
           MOVE "E  A010WCBC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ENDCD.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ENTER-WCBC-FG.
           MOVE VDUBUF TO WCBC-ONLY-FG.
      *-----------------------------------------------------------------

       ENTER-BEGCD.
           MOVE "E  A020CD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              IF (DEL-BRANCH-FG = "Y" OR WCBC-ONLY-FG = "Y")
                 GO TO ALL-CODES
              ELSE
                 MOVE
                "ALL CODES NOT ALLOWED UNLESS DEL BRANCH OR WCBC ONLY"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-BEGCD.
           IF VDUSTATUS = "1U" GO TO ENTER-WCBC-FG.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ENTER-BEGCD.
           MOVE VDUBUF TO CD1.

      *-----------------------------------------------------------------
       ENTER-ENDCD.
           MOVE "E  A020CD2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEGCD.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ENDCD.
           MOVE VDUBUF TO CD2.
           GO TO EXIT-PARM.

       ALL-CODES.
           MOVE "S  A000CD1." TO VDU.
           MOVE SPACES TO CD1 VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000CD2." TO VDU.
           MOVE ALL "Z" TO CD2.
           CALL "C$TOLOWER" USING CD2, VALUE 2.
           MOVE CD2 TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ********************************************
       DISPLAY-PARAMETERS SECTION.
      ********************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A020CD1." TO VDU.
           MOVE CD1 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CD2." TO VDU.
           MOVE CD2 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010WCBC." TO VDU.
           MOVE WCBC-ONLY-FG TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO DELFEE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DELFEE-SYSDATE.
           DISPLAY DELFEE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DELFEE-FORM-FIELDS TO WR-BUF.
           MOVE DELFEE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE DELFEE-HELP--FILE TO HELP-LINK-FILE.
           MOVE DELFEE-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/DELFEE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY DELFEE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
