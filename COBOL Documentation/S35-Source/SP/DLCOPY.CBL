       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLCOPY.
       DATE-WRITTEN.    061104.
      ******************************************************************
      *
      *                 (SP)
      *  DESCRIPTION:   DEALER MASTER FILE COPY (LOCAL FILE)
      *
      * REV:
      *  140310 BAH SET DL-TRACK-CHANGE-FLAG TO "Y" ON A NEW DEALER,
      *                                                 REGACC PR# 350
      *  BAH 20191007 REMOVED ACCESS-CALL DLFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * SORT-FILE WILL BE THE NEW RECORDS TO WRITE
           SELECT SORT-FILE ASSIGN SORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY SORT-DL1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       COPY "LIBSP/SPFDSORT.CPY".
       COPY "LIBLP/LP01DL.CPY" REPLACING LEADING "DL" BY "SORT-DL".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/DLCOPY.CBL S35 09/20/22-14:26:12 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBSP/SPWSSORT.CPY".


      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  ACCEPT-LIST     PIC X(20) VALUE "ACCEPT-YN KEY2 KEY5.".
       01  MESS-LIST       PIC X(5)  VALUE "MESS.".

       01  ERRCD           PIC X     VALUE " ".

       01  IO-TYPE         PIC X(3).

      *-----------------------------------------------------------------
       01  NEWBR1         PIC 9(4)    VALUE 0.
       01  NEWBR2         PIC 9(4)    VALUE 0.
       01  OLDBR          PIC 9(4)    VALUE 0.
       01  OBNAME-LIST    PIC X(7)    VALUE "OBNAME.".
       01  OBNAME         PIC X(40)   VALUE SPACES.
       01  NBNAME-LIST1   PIC X(8)    VALUE "NBNAME1.".
       01  NBNAME-LIST2   PIC X(8)    VALUE "NBNAME2.".
       01  NBNAME         PIC X(40)   VALUE SPACES.

       01  DEALER1         PIC 9(4)V99.
       01  DEALER2         PIC 9(4)V99.
       01  NEWDEALER       PIC 9(4)V99.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY". 
       COPY "LIBGB/ACCESSW.CPY".       

       COPY "LIBGB/DATERW.CPY".
       01  DLCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/DLCOPY_DEF.CPY".
       COPY "LIBSP/DLCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/DLCOPY_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              SORT-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
              SORT-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLCOPY"      TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DLCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

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

           MOVE "Y"               TO DL-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO DL-ALL-BASE.
           MOVE BR-MACHINE       TO DL-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO DL-ALL-DATA.
           MOVE DL-ALL-PATH      TO DL-PATH.
      * MUST MAKE SURE IC-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE GB-PATH-OWNBR TO THE KEY.
           MOVE BR-NO            TO DL-PATH-OWNBR.

           PERFORM OPEN-DL1-FILE.

           MOVE BR-NAME TO OBNAME.
           MOVE OBNAME TO WR-BUF.
           MOVE OBNAME-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

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
           MOVE NBNAME       TO WR-BUF.
           MOVE NBNAME-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

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
           MOVE NBNAME       TO WR-BUF.
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
           PERFORM UPDATE-FORM.

           IF ( IO-TYPE = "CLR" )
              GO TO ENTER-OLDBR.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           GO TO ENTER-KEYS.

      **********************
       UPDATE-FORM SECTION.
      **********************

           MOVE SPACES      TO WR-BUF.
           MOVE ACCEPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-DEALER1.
           MOVE SPACES TO IO-TYPE.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF F3-KEY
      * DONT ALLOW F3 ALL DEALERS IF TRYING TO COPY WITHIN A BRANCH
              IF OLDBR = NEWBR1 AND NEWBR1 = NEWBR2
                 GO TO ENTER-DEALER1
              ELSE
                 GO TO ALL-DEALERS.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-DEALER1.

           MOVE VDUNUM2 TO DEALER1.
           IF VDUNUM2 = 0
              GO TO ENTER-DEALER1.

      *-----------------------------------------------------------------
       ENTER-DEALER2.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF UP-KEY
              GO TO ENTER-DEALER1.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DEALER2.

           MOVE VDUNUM2 TO DEALER2.
           IF VDUNUM2 = 0
              GO TO ENTER-DEALER2.

           GO TO TEST-NEW-DEALER.
      *-----------------------------------------------------------------
       ALL-DEALERS.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0000.00 TO DEALER1 VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO DEALER2 VDUNUM2.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       TEST-NEW-DEALER.

           MOVE 0 TO NEWDEALER.
      * IF TRYING TO COPY 1 DEALER TO ANOTHER DEALER WITHIN SAME BRANCH
           IF OLDBR NOT = NEWBR1
              MOVE "SNNN042NEW." TO VDU
              MOVE 0000.00 TO NEWDEALER VDUNUM2
              PERFORM SCREENIO
              GO TO ENTER-ACCEPT-YN.

      * ENTER A NEW DEALER NUMBER TO COPY TO WITHIN SAME BRANCH
           MOVE "ENNN042NEW." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF UP-KEY
              GO TO ENTER-DEALER2.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO TEST-NEW-DEALER.

           MOVE VDUNUM2 TO NEWDEALER.
           IF VDUNUM2 = 0
              GO TO TEST-NEW-DEALER.

       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT-YN." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-DEALER2.
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-ACCEPT-YN.
           IF ( VDUBUF = "N" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM
           ELSE
           IF ( VDUBUF NOT = "Y" )
              GO TO ENTER-ACCEPT-YN.

           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM SOURCE-FILE-MODULE.
           PERFORM TARGET-FILE-MODULE.

       END-UPDATE-FORM.
           PERFORM COPY-END.

      ******************************************************************
      *
      *    S O U R C E - F I L E - M O D U L E
      *
      *=================================================================
      * IN  : DEALER1
      *       DEALER2
      *       SOURCE-DL-PATH
      * OUT : SORT-FILE
      * DESC: CREATE SORT-FILE BY GOING THROUGH THE SOURCE FILE FOR
      *       GIVEN BRANCH NUMBER (OLDBR).
      ******************************************************************
       SOURCE-FILE-MODULE SECTION.
    
      * CREATE SORT FILE MKTEMP-BUF
           PERFORM LOAD-SORT-FILE.
           PERFORM OPEN-SORT-FILE.
    
           MOVE DEALER1        TO DL-DLNO.
           PERFORM OPEN-DL1-FILE.

           MOVE DL-PATH-OWNBR TO DL-BRNO.


           PERFORM START-DL1-FILE.
    
       SOURCE-FILE-READ-NEXT.
    
           PERFORM READ-DL1-FILE-NEXT.
           IF ( IO-BAD ) OR ( DL-BRNO NOT = DL-PATH-OWNBR )
              GO TO SOURCE-FILE-MODULE-EXIT.
    
           IF ( DL-DLNO > DEALER2 )
              GO TO SOURCE-FILE-MODULE-EXIT.
    
      * VALID DL-REC 
           MOVE DL-BRNO TO SORT-DL-BRNO.
           MOVE DL-DLNO TO SORT-DL-DLNO.
           PERFORM READ-SORT-FILE.
           IF ( IO-GOOD )
              MOVE DL-REC TO SORT-DL-REC
              PERFORM REWRITE-SORT-FILE
              GO TO SOURCE-FILE-READ-NEXT.

           MOVE DL-REC TO SORT-DL-REC.
           PERFORM WRITE-SORT-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.
    
           GO TO SOURCE-FILE-READ-NEXT.
    
       SOURCE-FILE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T A R G E T - F I L E - M O D U L E
      *
      *=================================================================
      * IN  : NEWBR1
      *       NEWBR2
      *       SORT-FILE
      * OUT : DL-FILE
      * DESC: UPDATE DL-FILE (TARGET) FROM RECORDS FROM
      *       THE SORT-FILE (SOURCE) FOR GIVEN BRANCH RANGE
      *       (NEWBR1 TO NEWBR2).
      ******************************************************************
       TARGET-FILE-MODULE SECTION.
    
           MOVE NEWBR1 TO BR-NO.
           PERFORM START-BR-FILE.
    
       TARGET-FILE-NEXT-BR.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD )
              GO TO TARGET-FILE-MODULE-END.
    
           IF ( BR-NO > NEWBR2 )
              GO TO TARGET-FILE-MODULE-END.
    
      * SKIP ORIGINATING BRANCH OR YOU WILL GET RECORD LOCKING PROBLEM:
           IF ( BR-NO = OLDBR )
              GO TO TARGET-FILE-NEXT-BR.
    
      * NEED TO CHANGE THE DL-PATH FOR THE TARGET PATH 
      * NEED THE DL-PATH SET FOR DL-PATH-OWNBR BEING CORRECT FOR DOING A
      * REWRITE/WRITE ON THE DLFILE.
      
           MOVE "Y"              TO DL-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO DL-ALL-BASE.
           MOVE BR-MACHINE       TO DL-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO DL-ALL-DATA.
           MOVE DL-ALL-PATH      TO DL-PATH.

      * MUST MAKE SURE DL-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE GB-PATH-OWNBR TO THE KEY.
           MOVE BR-NO            TO DL-PATH-OWNBR.

      * VALID BR-REC; 
      * FILES LEFT OPEN FROM PREVIOUS SECTION (SOURCE-FILE-MODULE).
      * JUST START THE SORT-FILE AT THE BEGINNING.

           MOVE 0 TO SORT-DL-BRNO.
           MOVE 0 TO SORT-DL-DLNO.
           PERFORM START-SORT-FILE.
    
       TARGET-FILE-NEXT-SORT.
           PERFORM READ-SORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO TARGET-FILE-NEXT-BR.
    
           MOVE SORT-DL-REC TO DL-REC.
           MOVE BR-NO       TO DL-BRNO.
           PERFORM READ-DL1-FILE.
           IF ( IO-GOOD )
              MOVE SORT-DL-REC TO DL-REC
              MOVE BR-NO       TO DL-BRNO
              PERFORM REWRITE-DL1-FILE
              GO TO TARGET-FILE-NEXT-SORT.
    
           PERFORM WRITE-DL1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.
    
           GO TO TARGET-FILE-NEXT-SORT.
    
       TARGET-FILE-MODULE-END.
    
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-SORT-FILE.
    
       TARGET-FILE-MODULE-EXIT.
           EXIT.


      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO DLCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DLCOPY-SCREEN UPON DLCOPY-WINDOW-HANDLE.
           MOVE DLCOPY-FORM-FIELDS TO WR-BUF.
           MOVE DLCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE DLCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/DLCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       COPY-END.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1IN.CPY".

       LOAD-SORT-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO SORT-PATH.
           PERFORM OPEN-SORT-FILE-OUTPUT.
           PERFORM CLOSE-SORT-FILE.
    
      *-----------------------------------------------------------------
       OPEN-SORT-FILE-OUTPUT.
    
           PERFORM OPEN-IT.
           MOVE SORT-IFN TO E-FILE.
           OPEN OUTPUT SORT-FILE.
           IF ( IO-LOCKED )
              GO TO OPEN-SORT-FILE-OUTPUT.
           IF ( IO-ALREADY-OPEN )
              CLOSE SORT-FILE
              GO TO OPEN-SORT-FILE-OUTPUT.
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       OPEN-SORT-FILE.
    
           PERFORM OPEN-IT.
           MOVE SORT-IFN TO E-FILE.
           OPEN I-O SORT-FILE.
           IF ( IO-LOCKED )
              GO TO OPEN-SORT-FILE.
           IF ( IO-ALREADY-OPEN )
              CLOSE SORT-FILE
              GO TO OPEN-SORT-FILE.
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       READ-SORT-FILE.

           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-DL1-KEY TO E-KEYX.
           READ SORT-FILE.
           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE.
    
      *-----------------------------------------------------------------
       READ-SORT-FILE-NEXT.
    
           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-DL1-KEY TO E-KEYX.
           READ SORT-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE-NEXT.
    
      *-----------------------------------------------------------------
       START-SORT-FILE.
    
           PERFORM START-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-DL1-KEY TO E-KEYX.
           START SORT-FILE KEY NOT < SORT-DL1-KEY.
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       WRITE-SORT-FILE.
    
           PERFORM WRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-DL1-KEY TO E-KEYX.
           WRITE SORT-DL-REC.
           IF ( IO-LOCKED )
             GO TO WRITE-SORT-FILE.
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       REWRITE-SORT-FILE.
    
           PERFORM REWRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-DL1-KEY TO E-KEYX.
           REWRITE SORT-DL-REC.
           IF ( IO-LOCKED )
              GO TO REWRITE-SORT-FILE.
    
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       CLOSE-SORT-FILE.
    
           PERFORM CLOSE-IT.
           CLOSE SORT-FILE.
    
      *-----------------------------------------------------------------
       REMOVE-SORT-FILE.
    
           MOVE SORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       ENDIT.
           PERFORM CLOSE-FILES.
           PERFORM REMOVE-SORT-FILE.

       EXIT-PROG.
           MOVE "GB/INSTMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLCOPY-SCREEN.
           DESTROY DLCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
