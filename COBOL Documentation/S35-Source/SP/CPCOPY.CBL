       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPCOPY.
       DATE-WRITTEN.    010401.
      ******************************************************************
      *
      *                 (SP)
      *  DESCRIPTION:   COLLECTOR POOL COPY
      *
      * REV:
      * BLV 021120 ORIGINALLY HAD ENTRY OF SOURCE & TARGET FILE PATHS,
      *            BUT STARRED OFF; THIS REALLY DIDN'T SEEM RIGHT FOR
      *            SETTING UP LOCAL FILE PATHS FOR CPFILE...FELT
      *            CONFUSED BECAUSE IT WAS ALMOST LIKE I SHOULD BE
      *            READING THE TARGET BRANCH FILE TO SET UP THE TARGET
      *            FILE PATHS....SO REMOVED THIS...
      * KEC 2018-1029 CONVERTED CPFILE FOR A32 & A34
      * KEC 2019-0405 FIXED HOW TO CHECK AFTER READ-CP1-FILE-NEXT
      *               FOR GLOBAL AND LOCAL USAGE.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE ASSIGN SORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY SORT-CP1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       COPY "LIBSP/SPFDSORT.CPY".
       COPY "LIBSP/SP01CP.CPY" REPLACING LEADING "CP" BY "SORT-CP".


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CPCOPY.CBL S35 02/22/24-07:36:47 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".
       COPY "LIBSP/SPWSSORT.CPY".

       01  ACCEPT-LIST     PIC X(20) VALUE "ACCEPT-YN KEY2 KEY5.".
       01  MESS-LIST       PIC X(5)  VALUE "MESS.".

       01  ERRCD           PIC X     VALUE " ".

       01  IO-TYPE         PIC X(3).

      *-----------------------------------------------------------------
       01  NEWBR1         PIC 9(4)    VALUE 0.
       01  NEWBR2         PIC 9(4)    VALUE 0.
       01  OLDBR          PIC 9(4)    VALUE 0.
       01  OBNAME-LIST    PIC X(7)    VALUE "OBNAME.".
       01  NBNAME-LIST1   PIC X(8)    VALUE "NBNAME1.".
       01  NBNAME-LIST2   PIC X(8)    VALUE "NBNAME2.".
       01  NBNAME         PIC X(40)   VALUE SPACES.

      *-----------------------------------------------------------------

       01  CURRENT-PATH.
           03  CUR-PATH-BASE       PIC X(9).
           03  FILLER              PIC X         VALUE "/".
           03  CUR-PATH-MACHINE    PIC X(2).
           03  FILLER              PIC X         VALUE "/".
           03  CUR-PATH-DATA       PIC X(6).
           03  FILLER              PIC X(10)  VALUE
               "/SP/CPFILE".

       01  POOL1           PIC X(3).
       01  POOL2           PIC X(3).


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  CPCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CPCOPY_DEF.CPY".
       COPY "LIBSP/CPCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CPCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              SORT-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CP1-FILE.
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
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPCOPY" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CPCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      *-----------------------------------------------------------------

           MOVE EXT-FILPATH-BASE    TO CUR-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO CUR-PATH-MACHINE.
           MOVE EXT-FILPATH-DATA    TO CUR-PATH-DATA.
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

           MOVE "Y"            TO CP-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO CP-ALL-BASE.
           MOVE BR-MACHINE       TO CP-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO CP-ALL-DATA.
           MOVE CP-ALL-PATH      TO CP-PATH.
      * MUST MAKE SURE IC-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE GB-PATH-OWNBR TO THE KEY.
           MOVE BR-NO            TO CP-PATH-OWNBR.

           PERFORM OPEN-CP1-FILE.

           MOVE BR-NAME     TO WR-BUF.
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
              MOVE " BRANCH NOT ON FILE" TO MESS
                                            BR-NAME
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
           PERFORM UPDATE-FORM.

           IF ( IO-TYPE = "CLR" )
              GO TO ENTER-OLDBR.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           GO TO ENTER-KEYS.

      ******************************************************************
       UPDATE-FORM SECTION.
      ******************************************************************

           MOVE SPACES TO WR-BUF.
           MOVE ACCEPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-POOL1.
           MOVE SPACES TO IO-TYPE.
           MOVE "E  A030POOL1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF F3-KEY
              GO TO ALL-POOLS.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-POOL1.
           MOVE VDUBUF TO POOL1.

      *-----------------------------------------------------------------
       ENTER-POOL2.
           MOVE "E  A030POOL2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF UP-KEY
              GO TO ENTER-POOL1.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-POOL2.
           MOVE VDUBUF TO POOL2.

           GO TO ENTER-ACCEPT-YN.
      *-----------------------------------------------------------------
       ALL-POOLS.
           MOVE "S  A030POOL1." TO VDU.
           MOVE SPACES TO POOL1 VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030POOL2." TO VDU.
           MOVE ALL "Z" TO POOL2.
           CALL "C$TOLOWER" USING POOL2, VALUE 3.
           MOVE POOL2 TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-ACCEPT-YN.
           MOVE "E  A010ACCEPT-YN." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-POOL2.
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
      * IN  : POOL1
      *       POOL2
      *       SOURCE-CP-PATH
      * OUT : SORT-FILE
      * DESC: CREATE SORT-FILE BY GOING THROUGH THE SOURCE FILE FOR
      *       GIVEN BRANCH NUMBER (OLDBR).
      ******************************************************************
       SOURCE-FILE-MODULE SECTION.
    
      * CREATE SORT-FILE MKTEMP-BUF
           PERFORM LOAD-SORT-FILE.
           PERFORM OPEN-SORT-FILE.
    
           MOVE POOL1          TO CP-ID.

           PERFORM START-CP1-FILE.
    
       SOURCE-FILE-READ-NEXT.
    
           PERFORM READ-CP1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO SOURCE-FILE-MODULE-EXIT.

           IF ( CP-PATH-OVERRIDE-GLOBAL ) AND
              ( CP-BRNO NOT = ZEROES    )
              MOVE 9 TO IO-FG
              GO TO SOURCE-FILE-MODULE-EXIT
           ELSE
           IF ( NOT CP-PATH-OVERRIDE-GLOBAL ) AND
              ( CP-BRNO NOT = CP-PATH-OWNBR )
              MOVE 9 TO IO-FG
              GO TO SOURCE-FILE-MODULE-EXIT.

       
           IF ( CP-ID > POOL2 )
              GO TO SOURCE-FILE-MODULE-EXIT.
    
      * VALID CP-REC 

           MOVE CP-BRNO TO SORT-CP-BRNO.
           MOVE CP-ID   TO SORT-CP-ID.
           PERFORM READ-SORT-FILE.
           IF ( IO-GOOD )
              MOVE CP-REC TO SORT-CP-REC
              PERFORM REWRITE-SORT-FILE
              GO TO SOURCE-FILE-READ-NEXT.
    
           MOVE CP-REC TO SORT-CP-REC.
           PERFORM WRITE-SORT-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.
    
           GO TO SOURCE-FILE-READ-NEXT.
    
       SOURCE-FILE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      * IN  : NEWBR1
      *       NEWBR2
      *       SORT-FILE
      * OUT : CP-FILE
      * DESC: UPDATE CP-FILE (TARGET) FROM RECORDS FROM
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
    
      * NEED TO CHANGE THE CP-PATH FOR THE TARGET PATH 
      * NEED THE CP-PATH SET FOR CP-PATH-OWNBR BEING CORRECT FOR DOING A
      * REWRITE/WRITE ON THE CPFILE.
      
           MOVE "Y"              TO CP-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO CP-ALL-BASE.
           MOVE BR-MACHINE       TO CP-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO CP-ALL-DATA.
           MOVE CP-ALL-PATH      TO CP-PATH.
    
      * MUST MAKE SURE CP-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE CP-PATH-OWNBR TO THE KEY.
           MOVE BR-NO            TO CP-PATH-OWNBR.

      * VALID BR-REC; 
      * FILES LEFT OPEN FROM PREVIOUS SECTION (SOURCE-FILE-MODULE).
      * JUST START THE SORT-FILE AT THE BEGINNING.

           MOVE 0      TO SORT-CP-BRNO.
           MOVE SPACES TO SORT-CP-ID.
           PERFORM START-SORT-FILE.
    
       TARGET-FILE-NEXT-SORT.
    
           PERFORM READ-SORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO TARGET-FILE-NEXT-BR.
    
           MOVE SORT-CP-REC TO CP-REC.
           MOVE BR-NO       TO CP-BRNO.
           PERFORM READ-CP1-FILE.
    
           IF ( IO-GOOD )
              MOVE SORT-CP-REC TO CP-REC
              MOVE BR-NO       TO CP-BRNO
              PERFORM REWRITE-CP1-FILE
              GO TO TARGET-FILE-NEXT-SORT.
       
           PERFORM WRITE-CP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.
       
           GO TO TARGET-FILE-NEXT-SORT.
    
       TARGET-FILE-MODULE-END.
    
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-SORT-FILE.
    
       TARGET-FILE-MODULE-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO CPCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CPCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CPCOPY-SCREEN UPON CPCOPY-WINDOW-HANDLE.
           MOVE CPCOPY-FORM-FIELDS TO WR-BUF.
           MOVE CPCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE CPCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CPCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
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
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPCP1IN.CPY".
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
           MOVE SORT-CP1-KEY TO E-KEYX.
           READ SORT-FILE.
           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE.
    
      *-----------------------------------------------------------------
       READ-SORT-FILE-NEXT.
    
           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-CP1-KEY TO E-KEYX.
           READ SORT-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE-NEXT.
    
      *-----------------------------------------------------------------
       START-SORT-FILE.
    
           PERFORM START-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-CP1-KEY TO E-KEYX.
           START SORT-FILE KEY NOT < SORT-CP1-KEY.
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       WRITE-SORT-FILE.
    
           PERFORM WRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-CP1-KEY TO E-KEYX.
           WRITE SORT-CP-REC.
           IF ( IO-LOCKED )
             GO TO WRITE-SORT-FILE.
      *    UNLOCK SORT-FILE.
    
      *-----------------------------------------------------------------
       REWRITE-SORT-FILE.
    
           PERFORM REWRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-CP1-KEY TO E-KEYX.
           REWRITE SORT-CP-REC.
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
           DESTROY CPCOPY-SCREEN.
           DESTROY CPCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
