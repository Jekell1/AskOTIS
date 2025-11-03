       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ICCOPY.
       DATE-WRITTEN.    011504.
      ******************************************************************
      *
      * WARNING - FORMS IS DIFFERENT FROM A15, INCREASED CURFIL, SOURCE
      *                                  AND TARGET BY 1 FOR LEADING "/"
      *                 (SP)
      *  DESCRIPTION:   INSURANCE FILE COPY (LOCAL FILE)
      *
      * REV:
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED FOR CHANGE TO IC-REC.
      * KEC 2018-1001 CONVERTED ICFILE FOR LOCAL-TO-GLOBAL
      *               CREATED LOGIC TO COPY IC-REC TO NEW BRNO   
      * BAH 20191003 REMOVED ACCESS-CALL ICFILE
      * BAH 20191112 REMOVED SOURCE AND TARGET
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
                  RECORD KEY SORT-IC1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       COPY "LIBSP/SPFDSORT.CPY".
       COPY "LIBLP/LP01IC.CPY" REPLACING LEADING "IC" BY "SORT-IC".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ICCOPY.CBL S35 02/22/24-07:38:04 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
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
           03  FILLER              PIC X          VALUE "/".
           03  CUR-PATH-MACHINE    PIC XX.
           03  FILLER              PIC X(10) VALUE "/LP/ICFILE".

       01  COMPANY1        PIC 9(2).
       01  COMPANY2        PIC 9(2).


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".

       01  ICCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ICCOPY_DEF.CPY".
       COPY "LIBSP/ICCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ICCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              SORT-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-IC1-FILE.
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
           MOVE "ICCOPY" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ICCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.

       SEND-DEFAULT-SOURCE-PATHS.
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

           MOVE "Y"            TO IC-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO IC-ALL-BASE.
           MOVE BR-MACHINE       TO IC-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO IC-ALL-DATA.
           MOVE IC-ALL-PATH      TO IC-PATH.
      * MUST MAKE SURE IC-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE IC-PATH-OWNBR TO THE KEY.
           MOVE BR-NO            TO IC-PATH-OWNBR.

           MOVE BR-NAME TO OBNAME.
           MOVE OBNAME      TO WR-BUF.
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

      ******************************************************************
       UPDATE-FORM SECTION.
      ******************************************************************

           MOVE SPACES      TO WR-BUF.
           MOVE ACCEPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-COMPANY1.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N020COMP1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF F3-KEY
              GO TO ALL-COMPANIES.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-COMPANY1.
           MOVE VDUNUM0 TO COMPANY1.
           IF VDUNUM0 = 0
              GO TO ENTER-COMPANY1.

      *-----------------------------------------------------------------
       ENTER-COMPANY2.
           MOVE "EN N020COMP2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF UP-KEY
              GO TO ENTER-COMPANY1.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-COMPANY2.
           MOVE VDUNUM0 TO COMPANY2.
           IF VDUNUM0 = 0
              GO TO ENTER-COMPANY2.
           IF COMPANY2 < COMPANY1
              GO TO ENTER-COMPANY2.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ALL-COMPANIES.
           MOVE "SN N020COMP1." TO VDU.
           MOVE 00 TO COMPANY1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020COMP2." TO VDU.
           MOVE 99 TO COMPANY2 VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------

       ENTER-ACCEPT-YN.
           MOVE "E  A010ACCEPT-YN." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-COMPANY2.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM SOURCE-FILE-MODULE.
           PERFORM TARGET-FILE-MODULE.

       END-UPDATE-FORM.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       EXIT-UPDATE-FORM.
           EXIT.

      ******************************************************************
      * IN  : COMPANY1
      *       COMPANY2
      *       SOURCE-IC-PATH
      * OUT : SORT-FILE
      * DESC: CREATE SORT-FILE BY GOING THROUGH THE SOURCE FILE FOR
      *       GIVEN BRANCH NUMBER (OLDBR).
      ******************************************************************
       SOURCE-FILE-MODULE SECTION.
    
           PERFORM LOAD-SORT-FILE. 
           PERFORM OPEN-SORT-FILE.
    
           MOVE COMPANY1       TO IC-CODE.

      * PATH IS SETUP WHEN ORIGINAL BRANCH WAS ENTERED.

           PERFORM OPEN-IC1-FILE.
           PERFORM START-IC1-FILE.
    
       SOURCE-FILE-READ-NEXT.
    
           PERFORM READ-IC1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IC-BRNO NOT = IC-PATH-OWNBR )
              GO TO SOURCE-FILE-MODULE-EXIT.
       
           IF ( IC-CODE > COMPANY2 )
              GO TO SOURCE-FILE-MODULE-EXIT.
    
      * VALID IC-REC 
           MOVE IC-BRNO TO SORT-IC-BRNO.
           MOVE IC-CODE TO SORT-IC-CODE.
           PERFORM READ-SORT-FILE.
           IF ( IO-GOOD )
              MOVE IC-REC TO SORT-IC-REC
              PERFORM REWRITE-SORT-FILE
              GO TO SOURCE-FILE-READ-NEXT.
    
           MOVE IC-REC TO SORT-IC-REC.
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
      * OUT : IC-FILE
      * DESC: UPDATE IC-FILE (TARGET) FROM RECORDS FROM
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
    
      * NEED TO CHANGE THE IC-PATH FOR THE TARGET PATH 
      * NEED THE IC-PATH SET FOR IC-PATH-OWNBR BEING CORRECT FOR DOING A
      * REWRITE/WRITE ON THE ICFILE.

           MOVE "Y"            TO IC-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE TO IC-ALL-BASE.
           MOVE BR-MACHINE       TO IC-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO IC-ALL-DATA.
           MOVE IC-ALL-PATH      TO IC-PATH.

      * MUST MAKE SURE IC-PATH-OWNBR IS CORRECT , THE I/O COPY MEMBERS
      * HAVE THE SAFE GUARD IN IT TO MOVE IC-PATH-OWNBR TO THE KEY.
           MOVE BR-NO            TO IC-PATH-OWNBR.

      * VALID BR-REC; 
      * FILES LEFT OPEN FROM PREVIOUS SECTION (SOURCE-FILE-MODULE).
      * JUST START THE SORT-FILE AT THE BEGINNING.

           MOVE 0 TO SORT-IC-BRNO
                     SORT-IC-CODE.
           PERFORM START-SORT-FILE.
    
       TARGET-FILE-NEXT-SORT.
    
           PERFORM READ-SORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO TARGET-FILE-NEXT-BR.
    
           MOVE SORT-IC-REC TO IC-REC.
           MOVE BR-NO       TO IC-BRNO.
           PERFORM READ-IC1-FILE.
           IF ( IO-GOOD )
              MOVE SORT-IC-REC TO IC-REC
              MOVE BR-NO       TO IC-BRNO
              PERFORM REWRITE-IC1-FILE
              GO TO TARGET-FILE-NEXT-SORT.
       
           PERFORM WRITE-IC1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.
       
           GO TO TARGET-FILE-NEXT-SORT.
    
       TARGET-FILE-MODULE-END.
    
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-SORT-FILE.
    
       TARGET-FILE-MODULE-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO ICCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ICCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ICCOPY-SCREEN UPON ICCOPY-WINDOW-HANDLE.
           MOVE ICCOPY-FORM-FIELDS TO WR-BUF.
           MOVE ICCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE ICCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ICCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIC1IN.CPY".
       LOAD-SORT-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO SORT-PATH.
           PERFORM OPEN-SORT-FILE-OUTPUT.
           PERFORM CLOSE-SORT-FILE.
       OPEN-SORT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE SORT-IFN TO E-FILE.
           OPEN OUTPUT SORT-FILE.
           IF ( IO-LOCKED )
              GO TO OPEN-SORT-FILE-OUTPUT.
           IF ( IO-ALREADY-OPEN )
              CLOSE SORT-FILE
              GO TO OPEN-SORT-FILE-OUTPUT.
           UNLOCK SORT-FILE.
       OPEN-SORT-FILE.
           PERFORM OPEN-IT.
           MOVE SORT-IFN TO E-FILE.
           OPEN I-O SORT-FILE.
           IF ( IO-LOCKED )
              GO TO OPEN-SORT-FILE.
           IF ( IO-ALREADY-OPEN )
              CLOSE SORT-FILE
              GO TO OPEN-SORT-FILE.
           UNLOCK SORT-FILE.
       READ-SORT-FILE.
           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-IC1-KEY TO E-KEYX.
           READ SORT-FILE.
           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE.
       READ-SORT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-IC1-KEY TO E-KEYX.
           READ SORT-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-SORT-FILE-NEXT.
       START-SORT-FILE.
           PERFORM START-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-IC1-KEY TO E-KEYX.
           START SORT-FILE KEY NOT < SORT-IC1-KEY.
           UNLOCK SORT-FILE.
       WRITE-SORT-FILE.
           PERFORM WRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-IC1-KEY TO E-KEYX.
           WRITE SORT-IC-REC.
           IF ( IO-LOCKED )
             GO TO WRITE-SORT-FILE.
           UNLOCK SORT-FILE.
       REWRITE-SORT-FILE.
           PERFORM REWRITE-IT.
           MOVE SORT-IFN TO E-FILE.
           MOVE SORT-IC1-KEY TO E-KEYX.
           REWRITE SORT-IC-REC.
           IF ( IO-LOCKED )
              GO TO REWRITE-SORT-FILE.
           UNLOCK SORT-FILE.
       CLOSE-SORT-FILE.
           PERFORM CLOSE-IT.
           CLOSE SORT-FILE.
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
           DESTROY ICCOPY-SCREEN.
           DESTROY ICCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
