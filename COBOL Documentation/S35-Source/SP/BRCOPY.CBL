       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRCOPY.
       DATE-WRITTEN.    072188.
      *********************************************************
      *                 (SP)
      *  DESCRIPTION:   BRANCH RECORD COPY UTILITY
      *
      * 081595 BAH ADDED BANK-CLEARING-GL AND GLBANK-DEPOSIT TO
      *             GET CORRECT BRANCH NO ASSIGNED
      * 011298 BLV UPDATED WORK RECORD
      * 041498 BLV FIX GL BR#, 9990 NOT 990, SO ONLY USE TARGET
      *            BR# FOR GL IF IT MATCHES SOURCE GL BR#
      * 090298 BLV REMOVED BR-EOM1-PROCEDURE, BR-EOM2-PROCEDURE
      * 040119 BAH ADDED NEW BRANCH TRAILER, B2FILE
      * 040119 BLV IF I-AM-WORLD, CLEAR BR-INS-PRODUCER NUMBER
      * 040218 BLV IF I-AM-WORLD, SET BR-JE-ALLOCATION-SKIP TO
      *        "Y", DE-NOVO BRANCHES DON'T GET ALLOCATIONS
      * 040303 BLV UNDO 2/18 CHANGE ABOVE, AT LEAST FOR NOW,
      *            DE-NOVOS DO GET ALLOCATIONS
      * 111206 BAH REPLACED RRFORM WITH SCREEN
      * 120928 BAH FIX GL BR# FOR BR-GL-ESCROW-BALANCE AND
      *                           BR-GL-ESCROW-PAYABLE, REGWAN PL# 781
      * 150930 BAH CHANGED BRANCH FROM RELATIVE TO INDEXED, PD0005
      * 151021 BAH REMOVED B2FILE
      * 161201 BLM FIX GL BR# FOR BR-CHECK-DEPOSIT-GL, WORLD PR# 1209
      * KEC 2018-1004 CONVERTED BRFILE FOR A32 & A34
      * BAH 20191002 REMOVED ACCESS-CALL SOURCE & TARGET FILE
      * BAH 20191112 REMOVED SOURCE AND TARGET
      ****************************************************************
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
           "@(#)SP/BRCOPY.CBL S35 02/23/24-11:28:28 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  MESS-LIST       PIC X(5)  VALUE "MESS.".

       01  ERRCD           PIC X     VALUE " ".

       01  CURRENT-PATH.
           03  CUR-PATH-BASE       PIC X(9).
           03  FILLER              PIC X      VALUE "/".
           03  CUR-PATH-MACHINE    PIC XX.
           03  FILLER              PIC X      VALUE "/".
           03  CUR-PATH-DATA       PIC X(6).

       01  WK-DATA-PATH             PIC X(6).
       01  WK-DATA-PATHX REDEFINES WK-DATA-PATH.
           03  FILLER               PIC XX.
           03  WK-DATA-PATH-BRNO    PIC 9(4).

       01  W-GL            PIC 9(11).
       01  W-GLX REDEFINES W-GL.
           03  W-GLBR      PIC 9(4).
           03  W-GLNO      PIC 9(7).

       01  IO-TYPE         PIC X(3).

      * HOLD ORIGINAL BRANCH RECORD (SOURCE)
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".


       01  TARGET-KEY        PIC 9(4).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  BRCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BRCOPY_DEF.CPY".
       COPY "LIBSP/BRCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BRCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRCOPY" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BRCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      * USR/DATA/R1
           MOVE EXT-FILPATH-BASE    TO CUR-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO CUR-PATH-MACHINE.
           MOVE "S  A000CURFIL." TO VDU.
           MOVE CURRENT-PATH TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-KEYS.
      D    STOP MESS.
           PERFORM UPDATE-FORM.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM CLOSE-BR-FILE.
           GO TO ENTER-KEYS.

      ************************************
       UPDATE-FORM SECTION.
      ************************************
       ENTER-ORIG-BRANCH.
           MOVE SPACES TO IO-TYPE.

           MOVE "ENNN040KEY1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ORIG-BRANCH.

           MOVE VDUNUM0 TO BR-NO.

           MOVE EXT-FILPATH-BASE    TO BR-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO BR-ALL-MACHINE.
           MOVE BR-ALL-PATH         TO BR-PATH.

           MOVE "Y"                 TO BR-PATH-OVERRIDE.

           PERFORM OPEN-BR-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ORIG-BRANCH.

      * SAVE ORIGINAL BRANCH RECORD
           MOVE BR-REC TO BROR-REC.

           MOVE "S  A400DISP1." TO VDU.
           MOVE BR-NAME         TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-TO-BRANCH.
           MOVE "S  A400DISP2." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENNN040KEY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-ORIG-BRANCH.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TO-BRANCH.

           MOVE VDUNUM0 TO TARGET-KEY.

           IF TARGET-KEY = BR-NO
              MOVE "CANT BE SAME BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-BRANCH.
           IF TARGET-KEY = 0 
              MOVE "CANT BE ZERO BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-BRANCH.

           MOVE TARGET-KEY TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
      * ALREADY EXIST
              MOVE "S  A400DISP2." TO VDU
              MOVE BROR-NAME       TO VDUBUF
              PERFORM SCREENIO
      * OVERWRITE QUESTION ??
              PERFORM COPY-IT
              IF IO-FG = 9
                 GO TO ENTER-TO-BRANCH
              ELSE
                 MOVE BROR-REC   TO BR-REC
                 MOVE TARGET-KEY TO BR-NO
                 PERFORM SETUP-GL
                 PERFORM REWRITE-BR-FILE
           ELSE
              MOVE BROR-REC   TO BR-REC
              MOVE TARGET-KEY TO BR-NO
              PERFORM SETUP-GL
              PERFORM WRITE-BR-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

       END-UPDATE-FORM.
           PERFORM COPY-END.

      ****************
       COPY-IT SECTION.
      ****************
           MOVE "OVERWRITE BRANCH RECORD? (Y/N):" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM ALARM.
       COPY-IT-AGAIN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = "Y"
              MOVE 0 TO IO-FG
           ELSE
              IF VDUBUF = "N"
                 MOVE 9 TO IO-FG
              ELSE
                 GO TO COPY-IT-AGAIN.

           MOVE "S  A010SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO BRCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BRCOPY-SCREEN UPON BRCOPY-WINDOW-HANDLE.
           MOVE BRCOPY-FORM-FIELDS TO WR-BUF.
           MOVE BRCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE BRCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BRCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       SETUP-GL.

           MOVE 0      TO BR-BANK-ROUTING-NO.
           MOVE SPACES TO BR-BANK-ACCTNO.
           MOVE 1      TO BR-NEXT-SOLICIT-NO.

      * IF GL BR# = BR# YOU ARE COPYING FROM, USE TARGET BR# FOR GL BR#;
      * OTHERWISE LEAVE IT ALONE, IT'S A CORPORATE BR# SUCH AS 9990 OR 1

           MOVE BROR-RETAIN TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-RETAIN.

           MOVE BROR-GLOVERSHORT TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-GLOVERSHORT.

           MOVE BROR-GLBANK TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-GLBANK.

           MOVE BROR-GLONHAND TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-GLONHAND.

           MOVE BROR-BANK-CLEARING-GL TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-BANK-CLEARING-GL.

           MOVE BROR-GLBANK-DEPOSIT TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-GLBANK-DEPOSIT.

      * IF GL BR# = BR# YOU ARE COPYING FROM, USE TARGET BR# FOR GL BR#;
      * OTHERWISE LEAVE IT ALONE, IT'S A CORPORATE BR# SUCH AS 9990 OR 1
           MOVE BROR-GL-ESCROW-BALANCE TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-GL-ESCROW-BALANCE.

           MOVE BROR-GL-ESCROW-PAYABLE TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-GL-ESCROW-PAYABLE.

           MOVE BROR-CHECK-DEPOSIT-GL TO W-GL.
           IF W-GLBR = BROR-NO
              MOVE TARGET-KEY TO W-GLBR.
           MOVE W-GL TO BR-CHECK-DEPOSIT-GL.

           MOVE TARGET-KEY TO BR-FEE-GL-BRNO.

      * CLEAR OUT MANAGER NAME
           MOVE SPACES     TO BR-MANAGER.

      * SET BRNO ON END OF THE DATA PATH
           MOVE BR-DATA-PATH TO WK-DATA-PATH.
           MOVE TARGET-KEY   TO WK-DATA-PATH-BRNO.
           MOVE WK-DATA-PATH TO BR-DATA-PATH.

       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       COPY-BEBRN.
           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.
       COPY-END.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.


      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GB/INSTMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRCOPY-SCREEN.
           DESTROY BRCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
