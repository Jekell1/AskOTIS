      *A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EODEL.
       DATE-WRITTEN.    01/30/96
      ******************************************************************
      *
      *   DELETE REPORT BATCHES BY PROCEDURE CODE
      *                         BY BRANCH RANGE
      *                         BY DATE
      *
      * REV:
      * KEC 072997 CHANGED 'LIBGB/SFTDATE' TO 'LIBGB/EDITOR'.
      * GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  KEC 040998 REVISED THE COPY MEMBER 'LIBGB/EDITOR(W)' SO FOR
      *              MORE EFFICENT SYSTEM OF EXECUTING.
      *  KEC 052798 REVISED EDITOR (AGAIN) FOR A 'LIBGB/EDITOR' PROBLEM.
      *  CS  010601 PASSWORD ON DELETE OF BATCH [SP/BATCHID/3 BFMOTOR PR#1494
      *  20170905 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2021.1011 ALLOW TO RUN IN BATCH, PUT XFERS IN END-ROUTINE #1519
      ***********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT LS-FILE ASSIGN TO MKTEMP-BUF
              ORGANIZATION LINE SEQUENTIAL
              FILE STATUS IS STAT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    L S - F I L E
      *
      ******************************************************************
       FD  LS-FILE   LABEL RECORDS ARE STANDARD.
       01  LS-REC.
           03  LS-FILPATH-BASE            PIC X(9).
           03  LS-SLASH1                  PIC X(1).
           03  LS-MACHINE                 PIC X(2).
           03  LS-SLASH4                  PIC X(1).
           03  LS-STATE                   PIC X(2).
           03  LS-BRANCH                  PIC 9(4).
           03  LS-SLASH5                  PIC X(1).
           03  LS-EO                      PIC X(2).
           03  LS-SLASH6                  PIC X(1).
           03  LS-PCODE                   PIC X(3).
           03  LS-MMDDYY                  PIC 9(6).

      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/EODEL.CBL J35 12/21/23-09:40:01 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  LS-IFN                         PIC X(2)    VALUE "LS".

       01  RECORDS-FOUND                  PIC X(1)    VALUE "N".
           88  NO-RECORDS-FOUND           VALUE "N".

      ******************************************************************
      *
      *    H O L D
      *
      ******************************************************************
       01  HOLD-FIELDS.
           03  HOLD-DELETE-PATH                        PIC X(32).
           03  HOLD-DELETE-PATH-RDF          REDEFINES HOLD-DELETE-PATH.
               05  HOLD-DELETE-FILPATH-BASE            PIC X(9).
               05  HOLD-DELETE-SLASH1                  PIC X(1).
               05  HOLD-DELETE-MACHINE                 PIC X(2).
               05  HOLD-DELETE-SLASH4                  PIC X(1).
               05  HOLD-DELETE-STATE                   PIC X(2).
               05  HOLD-DELETE-BRANCH                  PIC 9(4).
               05  HOLD-DELETE-SLASH5                  PIC X(1).
               05  HOLD-DELETE-EO                      PIC X(2).
               05  HOLD-DELETE-SLASH6                  PIC X(1).
               05  HOLD-DELETE-PCODE                   PIC X(3).
               05  HOLD-DELETE-MMDDYY                  PIC 9(6).

       01  HOLD-PATH.
           03  HOLD-BR-MACHINE                         PIC X(2).
           03  HOLD-BR-DATA-PATH                       PIC X(6).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  PCODE                 PIC X(3).
           03  BEG-BRANCH            PIC 9(4).
           03  END-BRANCH            PIC 9(4).
           03  BEG-DATE              PIC 9(8).
           03  END-DATE              PIC 9(8).
           03  GROUP-FLAG            PIC X.
           03  ENT-GROUP             PIC X(4).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL           PIC X          VALUE "A".
       01  SV-BEG-BR                 PIC 9999       VALUE 9999.
       01  SV-END-BR                 PIC 9999       VALUE 9999.

       01  LS-DIRECTORY-CMD.
           03  FILLER            PIC X(06)  VALUE "LS -D ".
           03  LL-FILPATH-BASE   PIC X(09).
           03  FILLER            PIC X(1)   VALUE "/".
           03  LL-BR-MACHINE     PIC X(2).
           03  FILLER            PIC X(1)   VALUE "/".
           03  LL-BR-DATA-PATH   PIC X(6).
           03  FILLER            PIC X(4)   VALUE "/EO/".
           03  LL-PCODE          PIC X(3).
           03  FILLER            PIC X(3)   VALUE "* >".
           03  LL-TEMPFILE       PIC X(35).
      *-----------------------------------------------------------------
       01  REMOVE-ALL-REPORT-CMD.
           03  FILLER            PIC X(3)   VALUE "RM ".
           03  RA-FILE           PIC X(32).
           03  FILLER            PIC X(2)   VALUE "/*".
      *-----------------------------------------------------------------
       01  REMOVE-DIRECTORY-CMD.
           03  FILLER            PIC X(6)   VALUE "RMDIR ".
           03  RD-DIRECTORY      PIC X(32).
      *-----------------------------------------------------------------
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/EOSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78    PURGE-BATCH-SEQ     VALUE 3.
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EODEL_DEF.CPY".
       COPY "LIBSP/EODEL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/EODEL_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               LS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-EO1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               LS-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "EODEL"     TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = " " )
              GO TO END-PROGRAM.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = 0 )
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( BR-NO > END-BRANCH )
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE BR-MACHINE   TO HOLD-BR-MACHINE.
           MOVE BR-DATA-PATH TO HOLD-BR-DATA-PATH.

           PERFORM DELETE-REPORTS.

       END-OF-GROUP.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO REPORTS WERE FOUND; NOTHING WAS PURGED!!!" TO
                    MESS
              PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE 0 TO Q-POS C-POS P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LL-TEMPFILE.

           IF ( BEG-BRANCH = 0 )
              IF (END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF ( VDUSTATUS = "1U" )
              GO TO DATE-02.

      *-----------------------------------------------------------------
       ENTER-PCODE.
           MOVE "E  A030PCODE." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              PERFORM EO-SCAN.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-PCODE.

           MOVE VDUBUF TO PCODE EO-PCODE.

           IF ( PCODE < "A  " ) OR ( PCODE > "ZZZ" )
              GO TO ENTER-PCODE.

           INSPECT PCODE REPLACING ALL " " BY "X".

           IF ( PCODE NOT = EO-PCODE )
              MOVE "SPACES NOT ALLOWED IN PROCEDURE CODE!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PCODE.

           MOVE 0 TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.

           IF ( IO-FG = 0 )
              MOVE "S  A000PDESC." TO VDU
              MOVE EO-COMMENT      TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "PROCEDURE NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PCODE.

           MOVE PCODE TO PASSWDW-PGM.
           MOVE PURGE-BATCH-SEQ TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
              IF PASSWDW-STAT NOT = "OK"
                 MOVE
                "PURGE OF THIS BATCH NOT ALLOWED WITHOUT PASSWORD!"
                   TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-PCODE.

      *-----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO ENTER-PCODE.
           IF ( VDUSTATUS = "16" ) GO TO ALL-BR.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-PCODE.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
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

           IF ( WGR-WORD-ZERO )
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO BR-01.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" ) GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
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

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N"                    TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES                 TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO BR-01.

      *-----------------------------------------------------------------
       DATE-01.
           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "10" ) GO TO ENTER-PCODE.
           IF ( VDUSTATUS = "16" ) GO TO ALL-DATE.
           IF ( VDUSTATUS = "1U" ) GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.
           IF ( VDUNUM0 = 0 ) GO TO DATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       DATE-02.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "10" ) GO TO ENTER-PCODE.
           IF ( VDUSTATUS = "1U" ) GO TO DATE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-02.
           IF ( VDUNUM0 = 0 ) GO TO DATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DATE-01.

           GO TO EXIT-PARM.

      *----------------------------------------------------------------
       ALL-DATE.
           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A030PCODE." TO VDU.
           MOVE PCODE           TO VDUBUF.
           PERFORM SCREENIO.
      
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
      
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

       EXIT-DISPLAY-PARAMETERS.
           EXIT.

      ******************************************************************
       EO-SCAN SECTION.
      ******************************************************************
           MOVE SPACES        TO EOSCAN-KEY.
           MOVE "WI/EOSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EOSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF (EOSCAN-STATUS NOT = "00") OR (EOSCAN-KEY = SPACES)
              MOVE "X" TO VDUSTATUS
              GO TO EXIT-EO-SCAN.

           MOVE "S  A030PCODE." TO VDU.
           MOVE EOSCAN-KEY      TO VDUBUF.
           PERFORM SCREENIO.
           MOVE EOSCAN-KEY      TO VDUBUF.
           MOVE "00"            TO VDUSTATUS.

       EXIT-EO-SCAN.
           EXIT.

      **************************************
       VERIFY-PASSWORD SECTION.
      **************************************
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.

           MOVE "TO PURGE THIS BATCH:"       TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                  TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
       DELETE-REPORTS SECTION.
      ******************************************************************

           CALL "C$TOLOWER" USING LS-DIRECTORY-CMD, VALUE 6.
           MOVE EXT-FILPATH-BASE  TO LL-FILPATH-BASE.
           MOVE HOLD-BR-MACHINE   TO LL-BR-MACHINE.
           MOVE HOLD-BR-DATA-PATH TO LL-BR-DATA-PATH.
           MOVE PCODE             TO LL-PCODE.
           MOVE LS-DIRECTORY-CMD  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           PERFORM OPEN-LS-FILE.

       NEXT-LS.

           PERFORM READ-LS-FILE-NEXT.
           IF ( IO-FG = 9 )
              PERFORM CLOSE-LS-FILE
              GO TO EXIT-DELETE-REPORTS.

           MOVE LS-REC             TO HOLD-DELETE-PATH.

           MOVE HOLD-DELETE-MMDDYY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           IF DATE-YYYYMMDD < BEG-DATE OR DATE-YYYYMMDD > END-DATE
              GO TO NEXT-LS.

           MOVE "Y"                TO RECORDS-FOUND.
           MOVE HOLD-DELETE-MMDDYY TO ALP-DATE-EDIT.

           STRING "PURGE IN PROGRESS..... ",HOLD-DELETE-PCODE," ",
                  HOLD-DELETE-BRANCH," ",ALP-DATE-EDIT
                  DELIMITED BY "  " INTO MESS.

           MOVE "S  A600MESS:B." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      * DELETE DIRECTORY AND FILES  

           PERFORM RELEASE-REPORT.

           GO TO NEXT-LS.

       RELEASE-REPORT.

           MOVE HOLD-DELETE-PATH TO RA-FILE RD-DIRECTORY.

      * REMOVE ALL FILES 
           CALL "C$TOLOWER" USING REMOVE-ALL-REPORT-CMD, VALUE 3.
           MOVE REMOVE-ALL-REPORT-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      * REMOVE DIRECTORY 
           CALL "C$TOLOWER" USING REMOVE-DIRECTORY-CMD, VALUE 6.
           MOVE REMOVE-DIRECTORY-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

       EXIT-DELETE-REPORTS.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO EODEL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EODEL-SYSDATE.
           DISPLAY EODEL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EODEL-FORM-FIELDS TO WR-BUF.
           MOVE EODEL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE EODEL-HELP--FILE TO HELP-LINK-FILE.
           MOVE EODEL-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EODEL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBEO1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      *-----------------------------------------------------------------
       OPEN-LS-FILE.
           PERFORM OPEN-IT.
           MOVE LS-IFN TO E-FILE.
           OPEN INPUT LS-FILE.

           IF IO-FG = 8
              GO TO OPEN-LS-FILE.

           IF IO-FG = 7
              CLOSE LS-FILE
              GO TO OPEN-LS-FILE.

      *-----------------------------------------------------------------
       READ-LS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LS-IFN TO E-FILE.
           MOVE SPACES TO E-KEYX.
           MOVE SPACES TO LS-REC.
           READ LS-FILE AT END MOVE 9 TO IO-FG.

           IF IO-FG = 0 AND LS-REC = SPACES
              GO TO READ-LS-FILE-NEXT.

      *-----------------------------------------------------------------
       CLOSE-LS-FILE.
           PERFORM CLOSE-IT.
           CLOSE LS-FILE.

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE MKTEMP-BUF  TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/UTMENU" TO FORM-NAM
              MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EODEL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
