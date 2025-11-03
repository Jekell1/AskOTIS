       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GBCOPY.
       DATE-WRITTEN.    11/11/2019.
      ******************************************************************
      *
      *                     SP/GBCOPY
      *
      *  DESCRIPTION:   COPY GLOBAL RECORDS FROM A SOURCE BRANCH TO A
      *                 RANGE OF TARGET BRANCHES
      *
      * REV:
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 

 
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/GBCOPY.CBL S34 06/19/21-12:21:44 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  ACCEPT-LIST     PIC X(20) VALUE "ACCEPT-YN KEY2 KEY5.".
       01  MESS-LIST       PIC X(5)  VALUE "MESS.".
 
       01  ERRCD           PIC X     VALUE " ".
 
      * HOLD ORIGINAL GLOBAL RECORD (SOURCE)
       COPY "LIBGB/GB01GB.CPY" REPLACING LEADING "GB" BY "GBOR".

       01  NEWBR1         PIC 9(4)    VALUE 0.
       01  NEWBR2         PIC 9(4)    VALUE 0.
       01  ORIG-BRANCH    PIC 9(4)    VALUE 0.
       01  OBNAME-LIST    PIC X(7)    VALUE "OBNAME.".
       01  OBNAME         PIC X(40)   VALUE SPACES.
 

       01  CURRENT-PATH.
           03  CUR-PATH-BASE       PIC X(9).
           03  FILLER              PIC X      VALUE "/".
           03  CUR-PATH-MACHINE    PIC XX.
           03  FILLER              PIC X      VALUE "/".
           03  CUR-PATH-DATA       PIC X(6).

 
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  GBCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/GBCOPY_DEF.CPY".
       COPY "LIBSP/GBCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

 
      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/GBCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GBCOPY" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GBCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.

      * USR/DATA/R1
           MOVE EXT-FILPATH-BASE    TO CUR-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO CUR-PATH-MACHINE.
           MOVE "S  A000CURFIL." TO VDU.
           MOVE CURRENT-PATH TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-BR-FILE.

       ENTER-ORIG-BRANCH.
           MOVE "ENNN040OLDBR." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ORIG-BRANCH.
           MOVE VDUNUM0 TO ORIG-BRANCH BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ORIG-BRANCH.

           MOVE BR-NAME TO OBNAME.

           MOVE OBNAME      TO WR-BUF.
           MOVE OBNAME-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


           MOVE "Y" TO GB-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO GB-ALL-BASE.
           MOVE BR-MACHINE       TO GB-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO GB-ALL-DATA.
           MOVE GB-ALL-PATH      TO GB-PATH.

      * GB-PATH-OWNBR NEEDS TO BE SET BECAUSE I/O ROUTINES FORCE IT TO KEY
           MOVE ORIG-BRANCH      TO GB-PATH-OWNBR.

           PERFORM OPEN-GB-FILE.

      * READ SOURCE GB
           MOVE ORIG-BRANCH TO GB-BRNO.
           MOVE 1           TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID GLOBAL" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ORIG-BRANCH.

      *    UNLOCK GB-FILE.

      * SAVE ORIGINAL BRANCHES GLOBAL RECORD
           MOVE GB-REC TO GBOR-REC.

       ENTER-NEWBR1.
           MOVE "ENNN040NEWBR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "16" GO TO ALL-NEWBR.
           IF VDUSTATUS = "1U" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR1.
           MOVE VDUNUM0 TO NEWBR1.
       
       ENTER-NEWBR2.
           MOVE "ENNN040NEWBR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "1U" GO TO ENTER-NEWBR1.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR2.
           MOVE VDUNUM0 TO NEWBR2.

           IF NEWBR2 < NEWBR1
              GO TO ENTER-NEWBR2.

           GO TO ENTER-ACCEPT-YN.

       ALL-NEWBR.
           MOVE "SN N040NEWBR1." TO VDU.
           MOVE ZEROES           TO VDUNUM0 NEWBR1.
           PERFORM SCREENIO.

           MOVE "SN N040NEWBR2." TO VDU.
           MOVE 9999             TO VDUNUM0 NEWBR2.
           PERFORM SCREENIO.

       ENTER-ACCEPT-YN.
           MOVE SPACES      TO WR-BUF.
           MOVE ACCEPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010ACCEPT-YN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "1U" GO TO ENTER-NEWBR1.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF = "N" )
              GO TO ENTER-ORIG-BRANCH.

           PERFORM UPDATE-FILES.

           GO TO ENTER-ORIG-BRANCH.


      ******************************************************************
       UPDATE-FILES SECTION.
      ******************************************************************

           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE NEWBR1 TO BR-NO.
           PERFORM START-BR-FILE.
       NEXT-TARGET-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              GO TO END-UPDATE-FILES.

           IF BR-NO > NEWBR2
              GO TO END-UPDATE-FILES.

      * SKIP ORIGINATING BRANCH OR YOU WILL GET RECORD LOCKING PROBLEM:
           IF BR-NO = ORIG-BRANCH
              GO TO NEXT-TARGET-BRANCH.

      * MUST MAKE SURE GB-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE GB-PATH-OWNBR TO THE KEY.

           MOVE BR-NO TO GB-PATH-OWNBR.

      * TARGET BRANCH GLOBAL
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE GBOR-REC TO GB-REC
              PERFORM WRITE-GB-FILE
           ELSE
              MOVE GBOR-REC TO GB-REC
              PERFORM REWRITE-GB-FILE.

           GO TO NEXT-TARGET-BRANCH.

       END-UPDATE-FILES.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO GBCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GBCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GBCOPY-SCREEN UPON GBCOPY-WINDOW-HANDLE.
           MOVE GBCOPY-FORM-FIELDS TO WR-BUF.
           MOVE GBCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE GBCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE GBCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/GBCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       ENDIT.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "GB/INSTMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GBCOPY-SCREEN.
           DESTROY GBCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.

