       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCOPY.
      ******************************************************************
      *                   (SP)
      *
      *       COPY FEE CODES FROM ONE BRANCH TO ANOTHER
      *
      * REV:
      *  081195 BAH ADDED SOURCE AND TARGET FILE NAME
      *  071197 BAH ADDED WK-GLFEEFINCHG FROM A90 FOR WORLD
      *  072898 BLV WAS SKIPPING BRANCH IF NEWBR = OLDBR; CHANGED TO SKIP
      *             IN THIS SITUATION ONLY IF SOURCE-BUF = TARGET-BUF
      *             SO WE WOULDN'T MISS A BRANCH!
      *  981213 BLV ADDED 14 BYTES TO WK-REC BECAUSE CD RECORD SIZE
      *             WAS EXPANDED FROM 56 TO 70 BYES
      *  JTG 030530 ADDED WCBC FEE GL ENTRY FILE              WORLD #323
      *  BAH 100824 DO NOT RESET PATHS AFTER EACH RUN. GO BACK TO PATH ENTRY
      *             NOT MAIN-ROUTINE. JIMR WORLD.
      *   CS 111108 FEE CODE COPY WASNT REPLACING THE BRANCH # IN THE PAYABLE
      *             GL ACCTNO WITH THE TARGET BRANCH NUMBER. WE WERE JUST
      *             CHANGING KEY TO TARGET BRANCH & WRITING RECORD. I ADDED
      *             IF OLDBR = BRANCH PORTION OF GL-COMM-GLFEENO, MOVE IN
      *             NEWBR TO BRANCH PORTION.  IF THEY ARE DIFFERENT LEAVE ALONE
      *             FOR EX: WORLD 9990-????                    SUPPORT PR#756
      * BAH 111205 REPLACED RRFORM WITH SCREEN
      * KEC 2016.0318 CHANGED CD-FEE???? TO CD-GC-????.
      *               ALSO CHANGED TO USE "REPLACING LEADING" FOR:
      *               CD-REC - LIBLP/LP01CD.CPY - WK-REC
      *               CT-REC - LIBLP/LP01CT.CPY - WKCT-REC
      * KEC 2018.0404 ADDED SETTING CD-LTOUCH-DATE (WK-LTOUCH-DATE) WITHIN THE
      *               HARDCODED WRITE-WK-FILE AND REWRITE-WK-FILE.
      * BAH 190130 SPLIT CDFILE, REMOVED CTFILE
      * BAH 20191004 REMOVED ACCESS-CALL SOURCE & TARGET
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.


      *********************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CDCOPY.CBL S35 02/23/24-10:25:34 >".
      *********************************
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)SP/CDCOPY A15 11/08/11-12:34:31 11/30/11-15:44:43 >".

       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".


       COPY "LIBLP/LP01CDB.CPY" REPLACING LEADING "CDB" BY "CDBOR".

       01  ERRCD          PIC X      VALUE SPACES.

       01  NEWBR          PIC 9(5)    VALUE 0.
       01  NEWBR1         PIC 9(4)    VALUE 0.
       01  NEWBR2         PIC 9(4)    VALUE 0.
       01  OLDBR          PIC 9(4)    VALUE 0.
       01  CD1            PIC XX      VALUE SPACES.
       01  CD2            PIC XX      VALUE SPACES.
       01  OBNAME-LIST    PIC X(7)    VALUE "OBNAME.".
       01  OBNAME         PIC X(25)   VALUE SPACES.
       01  NBNAME-LIST1   PIC X(8)    VALUE "NBNAME1.".
       01  NBNAME-LIST2   PIC X(8)    VALUE "NBNAME2.".
       01  NBNAME         PIC X(25)   VALUE SPACES.

       01  MESS-LIST      PIC X(5)    VALUE "MESS.".
       01  IN-PROG-MESS   PIC X(40)   VALUE
           "    *** FEE CODE COPY IN PROGRESS ***   ".

       01  ACCEPT-BUF     PIC X       VALUE SPACES.

      *-----------------------------------------------------------------
       01  WK-GL-ACCOUNT   PIC 9(11).
       01  WK-GL REDEFINES WK-GL-ACCOUNT.
           03  WK-GL-BR    PIC 9(4).
           03  WK-GL-ACCT  PIC 9(7).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  CDCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CDCOPY_DEF.CPY".
       COPY "LIBSP/CDCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CDCOPY_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CDCOPY"      TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CDCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "Y"                 TO CDB-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE    TO CDB-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO CDB-ALL-MACHINE.
           MOVE CDB-ALL-PATH        TO CDB-PATH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-CDB1-FILE.

      *-----------------------------------------------------------------
       ENTER-OLDBR.
           MOVE "EN N040OLDBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-OLDBR.
           MOVE VDUNUM0 TO OLDBR BR-NO.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OLDBR.

           MOVE BR-NAME TO OBNAME.
           MOVE OBNAME TO WR-BUF.
           MOVE OBNAME-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       ENTER-NEWBR1.
           MOVE "EN N040NEWBR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1U" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR1.
           MOVE VDUNUM0 TO NEWBR1 BR-NO.
           IF NEWBR1 = OLDBR
              GO TO ENTER-NEWBR1.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEWBR1.

           MOVE BR-NAME TO NBNAME.
           MOVE NBNAME TO WR-BUF.
           MOVE NBNAME-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       ENTER-NEWBR2.
           MOVE "EN N040NEWBR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1U" GO TO ENTER-NEWBR1.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR2.
           MOVE VDUNUM0 TO NEWBR2 BR-NO.

           IF NEWBR2 = OLDBR OR NEWBR2 < NEWBR1
              GO TO ENTER-NEWBR2.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEWBR2.

           MOVE BR-NAME TO NBNAME.
           MOVE NBNAME       TO WR-BUF.
           MOVE NBNAME-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       ENTER-BEGCD.
           MOVE "E  A020CD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "16" GO TO ALL-CODES.
           IF VDUSTATUS = "1U" GO TO ENTER-NEWBR2.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-BEGCD.
           MOVE VDUBUF TO CD1.

      *-----------------------------------------------------------------
       ENTER-ENDCD.
           MOVE "E  A020CD2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1U" GO TO ENTER-BEGCD.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ENDCD.
           MOVE VDUBUF TO CD2.
           GO TO ENTER-ACCEPT.

      *-----------------------------------------------------------------
       ALL-CODES.
           MOVE "S  A000CD1." TO VDU.
           MOVE SPACES TO CD1 VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000CD2." TO VDU.
           MOVE ALL "Z" TO CD2.
           CALL "C$TOLOWER" USING CD2, VALUE 2.
           MOVE CD2 TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N"
              GO TO ENTER-OLDBR.

           IF ACCEPT-BUF NOT = "Y"
              GO TO ENTER-ACCEPT.

      *-----------------------------------------------------------------
       SET-INPROGRESS.
           MOVE IN-PROG-MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "GC"   TO CDB-TYPE
                          QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE.
           MOVE OLDBR  TO CDB-BRANCH
                          QCDB1-WBEG-BRANCH
                          QCDB1-WEND-BRANCH.
           MOVE CD1    TO CDB-CODE
                          QCDB1-WBEG-CODE.
           MOVE CD2    TO QCDB1-WEND-CODE.

           PERFORM START-CDB1-FILE.
       NEXT-FEE.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG NOT = 0
              PERFORM RESET-SCREEN
              GO TO ENTER-OLDBR.

           IF ( CDB-BRANCH > OLDBR     ) OR
              ( CDB-TYPE NOT = "GC"    ) OR
              ( CDB-CODE > CD2         )
              PERFORM RESET-SCREEN
              GO TO ENTER-OLDBR.

           MOVE NEWBR1  TO NEWBR.

      * SAVE ORIGINAL
           MOVE CDB-REC TO CDBOR-REC.

       SETUP-NEW-CD.

           MOVE NEWBR TO CDB-BRANCH.
           PERFORM READ-CDB1-FILE.
           IF IO-FG = 0
              PERFORM FIX-GLNO
              MOVE NEWBR TO CDB-BRANCH
              PERFORM REWRITE-CDB1-FILE
           ELSE
              MOVE CDBOR-REC TO CDB-REC
              MOVE NEWBR     TO CDB-BRANCH
              PERFORM FIX-GLNO
              PERFORM WRITE-CDB1-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

      *-----------------------------------------------------------------
       INCREMENT-BRANCH.
           ADD 1 TO NEWBR.

           IF NEWBR > NEWBR2
              GO TO EXIT-SETUP-NEW-CD.

           IF NEWBR = OLDBR
              GO TO INCREMENT-BRANCH.

           MOVE NEWBR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO INCREMENT-BRANCH.

           GO TO SETUP-NEW-CD.

       EXIT-SETUP-NEW-CD.
      * GET ORIGINAL BACK IN THE BUFFER
      *    MOVE CDBOR-REC TO CDB-REC.
      *    PERFORM START-CDB1-FILE-GREATER.

           GO TO NEXT-FEE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO CDCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CDCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CDCOPY-SCREEN UPON CDCOPY-WINDOW-HANDLE.
           MOVE CDCOPY-FORM-FIELDS TO WR-BUF.
           MOVE CDCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE CDCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CDCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC-ROUTINES SECTION.
      *************************************************
       FIX-GLNO.
           MOVE CDB-GC-COMM-GLFEENO TO WK-GL-ACCOUNT.
           IF WK-GL-BR = OLDBR
              MOVE NEWBR TO WK-GL-BR.
           MOVE WK-GL-ACCOUNT TO CDB-GC-COMM-GLFEENO.
       RESET-SCREEN.
           MOVE SPACES TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCDB1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CDCOPY-SCREEN.
           DESTROY CDCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
