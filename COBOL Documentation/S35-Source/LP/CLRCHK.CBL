       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLRCHK.
       DATE-WRITTEN.    041602.
      ******************************************************************
      *      CHECK RECONCILIATION CLEAR
      *
      * SET CK-CLEARED-FG AND CK-CLEARED-DATE
      * ALSO CLEARS VOIDS SO THE DELETE WILL WORK
      *
      * NOT BATCHABLE !!
      *
      * REV:
      *  020416 BAH NEW FOR REGMGM CHECK RECONCILIATION, PR# 1726
      *  BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181008 GLOBAL CKFILE
      *  BAH 20190926 REMOVED ACCESS-CALL CKFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CLRCHK.CBL S35 02/20/24-13:07:46 >".
      ******************************************
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CLRCHK.CBL S34 05/24/21-15:38:59 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".

       01  STMT-DATE       PIC 9(8).
       01  BRANCH          PIC 9(4).
       01  BEG-CHECK       PIC 9(6).
       01  END-CHECK       PIC 9(6).
       01  ERRCD           PIC X.

       01  FIRST-TIME          PIC X     VALUE "Y".

       01  MSEL-LIST         PIC X(5)  VALUE "MSEL.".
       01  LEGEND            PIC X(70).
       01  RECORD-COUNT      PIC 9(6)  VALUE 0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  CLRCHK-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CLRCHK_DEF.CPY".
       COPY "LIBLP/CLRCHK_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CLRCHK_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLRCHK" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS CLRCHK-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "Y" TO FIRST-TIME.

       MAIN-ROUTINE.

           PERFORM INITIAL-ROUTINE.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO FIRST-TIME.


           PERFORM OPEN-CK1-FILE.

           MOVE BRANCH               TO CK-BRNO
                                        QCK1-WBEG-BRNO
                                        QCK1-WEND-BRNO.
           MOVE BEG-CHECK            TO CK-NO
                                        QCK1-WBEG-CKNO
           MOVE END-CHECK            TO QCK1-WEND-CKNO.

           PERFORM START-CK1-FILE.

       NEXT-CK.
           PERFORM READ-CK1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           IF CK-BRNO NOT = BRANCH
      *       UNLOCK CK-FILE
              GO TO NEXT-CK.

      * SHOULD NEVER BE ANOTHER BRANCHES CHECKS IN THIS BRANCHES FILE !!
           IF CK-NO > END-CHECK
              GO TO END-ROUTINE.

           IF CK-CLEARED-DATE = 0 OR 00020202
              MOVE STMT-DATE TO CK-CLEARED-DATE
              MOVE "Y" TO CK-CLEARED-FG
              PERFORM REWRITE-CK1-FILE
              ADD 1 TO RECORD-COUNT.
      *    ELSE
      *       UNLOCK CK-FILE.

           GO TO NEXT-CK.

       END-ROUTINE.
           PERFORM CLOSE-CK1-FILE.

           IF FIRST-TIME = "Y"
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE !" TO LEGEND
              PERFORM SEND-MSEL
           ELSE
              MOVE "TOTAL CLEARED : " TO MESS
              MOVE RECORD-COUNT TO MESS-X-NUM-3
              MOVE MESS TO LEGEND
              PERFORM SEND-MSEL.

           MOVE 0 TO RECORD-COUNT.

           GO TO MAIN-ROUTINE.

      ***********************************
       INITIAL-ROUTINE SECTION.
      ***********************************

           MOVE " " TO ERRCD.

           IF FIRST-TIME = "N"
              GO TO BEG-CK.

       STMT-01.
           MOVE "ERNM080STMTDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO STMT-01.
           MOVE VDU-DATE-YYYYMMDD TO STMT-DATE.
           IF STMT-DATE = 0
              GO TO STMT-01.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           IF STMT-DATE NOT < DATE-YYYYMMDD
              MOVE "** WARNING; STATEMENT DATE IS IN THE FUTURE"
                TO MESS
              PERFORM SEND-MESS
              GO TO STMT-01.

       BR-01.
           MOVE "ENNN040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO STMT-01.
           IF VDUSTATUS = "1U" GO TO STMT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           MOVE VDUNUM0 TO BRANCH BR-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "S  A200BRNAME." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01
           ELSE
              MOVE "S  A200BRNAME." TO VDU
              MOVE BR-NAME TO VDUBUF
              PERFORM SCREENIO.

           PERFORM CLOSE-BR-FILE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CK-PATH-OVERRIDE.

       BEG-CK.
           MOVE "ENNN060BEGCK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO STMT-01.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEG-CK.
           MOVE VDUNUM0 TO BEG-CHECK.
       END-CK.
           MOVE "ENNN060ENDCK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO STMT-01.
           IF VDUSTATUS = "1U" GO TO BEG-CK.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO END-CK.
           MOVE VDUNUM0 TO END-CHECK.
           IF BEG-CHECK > END-CHECK
              MOVE "INVALID CHECK RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-CK.


       ACCPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO STMT-01.
           IF VDUSTATUS = "1U" GO TO END-CK.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ACCPT.
           IF VDUBUF = "N" GO TO BEG-CK.
           IF VDUBUF NOT = "Y" GO TO ACCPT.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO CLRCHK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLRCHK-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CLRCHK-SCREEN UPON CLRCHK-WINDOW-HANDLE.
           MOVE CLRCHK-FORM-FIELDS TO WR-BUF.
           MOVE CLRCHK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE CLRCHK-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLRCHK-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CLRCHK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-MSEL.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLRCHK-SCREEN.
           DESTROY CLRCHK-WINDOW-HANDLE.
           EXIT PROGRAM.
