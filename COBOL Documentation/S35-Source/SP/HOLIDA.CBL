       IDENTIFICATION DIVISION.
       PROGRAM-ID.      HOLIDA.
       DATE-WRITTEN.    12-15-2015.
      ******************************************************************
      *
      *  DESCRIPTION:   COPY THE BRANCH GLOBAL HOLIDAY SCHEDULE FROM
      *                 ONE BRANCH TO A RANGE OF BRANCHES
      *
      * REV:
      *  151215 BAH REGENCY PR#4908
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
           "@(#)SP/HOLIDA.CBL S34 06/19/21-12:28:14 >".

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
 
       01  IO-TYPE         PIC X(3).

       01  HOLD-HOLIDAY-TABLE.
           03  HOLD-HOLIDAY-TAB.
               05  HOLD-HOLIDAY-TABLE    OCCURS 16 TIMES
                                       INDEXED BY HOLD-GBHOIX.
                   07  HOLD-HOLIDAY      PIC 9999    COMP-3.

      *-----------------------------------------------------------------
       01  NEWBR1         PIC 9(4)    VALUE 0.
       01  NEWBR2         PIC 9(4)    VALUE 0.
       01  OLDBR          PIC 9(4)    VALUE 0.
       01  OBNAME-LIST    PIC X(7)    VALUE "OBNAME.".
       01  OBNAME         PIC X(40)   VALUE SPACES.
       01  NBNAME-LIST1   PIC X(8)    VALUE "NBNAME1.".
       01  NBNAME-LIST2   PIC X(8)    VALUE "NBNAME2.".
       01  NBNAME         PIC X(40)   VALUE SPACES.
 
      *-----------------------------------------------------------------


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
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  HOLIDA-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/HOLIDA_DEF.CPY".
       COPY "LIBSP/HOLIDA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY "LIBSP/HOLIDA_SCN.CPY".

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
           MOVE "HOLIDA" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS HOLIDA-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.

      * USR/DATA/R1
           MOVE EXT-FILPATH-BASE    TO CUR-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO CUR-PATH-MACHINE.
           MOVE "S  A000CURFIL." TO VDU.
           MOVE CURRENT-PATH TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-BR-FILE.

      *-----------------------------------------------------------------
       ENTER-OLDBR.
           MOVE "ENNN040OLDBR." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO ENDIT.
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

      * READ SOURCE GB
           PERFORM OPEN-GB-FILE.
           MOVE OLDBR TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID GLOBAL" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OLDBR.

      *    UNLOCK GB-FILE.

      * SAVE ORIGINAL BRANCHES HOLIDAY TAB
           MOVE GB-HOLIDAY-TAB TO HOLD-HOLIDAY-TAB.

      *-----------------------------------------------------------------
       ENTER-NEWBR1.
           MOVE "ENNN040NEWBR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "16" GO TO ALL-NEWBR.
           IF VDUSTATUS = "1U" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR1.
           MOVE VDUNUM0 TO NEWBR1 BR-NO.
       
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
           MOVE "ENNN040NEWBR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-OLDBR.
           IF VDUSTATUS = "1U" GO TO ENTER-NEWBR1.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-NEWBR2.
           MOVE VDUNUM0 TO NEWBR2 BR-NO.

           IF NEWBR2 < NEWBR1
              GO TO ENTER-NEWBR2.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE " BRANCH NOT ON FILE" TO MESS BR-NAME
              PERFORM SEND-MESS.

           MOVE BR-NAME TO NBNAME.
           MOVE NBNAME TO WR-BUF.
           MOVE NBNAME-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO ENTER-KEYS.
 

      *-----------------------------------------------------------------
       ALL-NEWBR.
           MOVE "SN N040NEWBR1." TO VDU.
           MOVE ZEROES           TO VDUNUM0 NEWBR1.
           PERFORM SCREENIO.
           MOVE "*** ALL BRANCHES ***" TO NBNAME.
           MOVE NBNAME TO WR-BUF.
           MOVE NBNAME-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "SN N040NEWBR2." TO VDU.
           MOVE 9999             TO VDUNUM0 NEWBR2.
           PERFORM SCREENIO.
           MOVE SPACES TO NBNAME.
           MOVE NBNAME TO WR-BUF.
           MOVE NBNAME-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       ENTER-KEYS.
           PERFORM UPDATE-FILES.

           IF ( IO-TYPE = "CLR" )
              GO TO ENTER-OLDBR.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           GO TO ENTER-KEYS.

      ******************************************************************
       UPDATE-FILES SECTION.
      ******************************************************************

           MOVE SPACES TO WR-BUF.
           MOVE ACCEPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


       ENTER-ACCEPT-YN.
           MOVE "E  A010ACCEPT-YN." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FILES.
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FILES.
           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-ACCEPT-YN.
           IF ( VDUBUF = "N" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FILES
           ELSE
           IF ( VDUBUF NOT = "Y" )
              GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE NEWBR1 TO BR-NO.
           PERFORM START-BR-FILE.
       NEXT-TARGET-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FILES.

           IF BR-NO > NEWBR2
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FILES.

      * SKIP ORIGINATING BRANCH OR YOU WILL GET RECORD LOCKING PROBLEM:
           IF BR-NO = OLDBR
              GO TO NEXT-TARGET-BRANCH.

      * MUST MAKE SURE GB-PATH-OWNBR IS CORRECT BECAUSE THE PATH
      * STILL HAS BRANCH 1 IN IT FROM THE OPEN, THE COPY MEMBERS HAVE
      * THE SAFE GUARD IN IT TO MOVE GB-PATH-OWNBR TO THE KEY.
           MOVE BR-NO TO GB-PATH-OWNBR.

      * TARGET BRANCH GLOBAL
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO NEXT-TARGET-BRANCH.

           MOVE HOLD-HOLIDAY-TAB TO GB-HOLIDAY-TAB.
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
           MOVE EXT-CONAME-CONAME TO HOLIDA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HOLIDA-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY HOLIDA-SCREEN UPON HOLIDA-WINDOW-HANDLE.
           MOVE HOLIDA-FORM-FIELDS TO WR-BUF.
           MOVE HOLIDA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE HOLIDA-HELP--FILE TO HELP-LINK-FILE.
           MOVE HOLIDA-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/HOLIDA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
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
           DESTROY HOLIDA-SCREEN.
           DESTROY HOLIDA-WINDOW-HANDLE.
           EXIT PROGRAM.
