       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FTENT.
       DATE-WRITTEN.    051295.
      *************************************************************
      *          CHECK FREE TYPING INPUT
      *            CALLED FROM LONPB0, LONPD0, SFDISB
      *
      * REV:
      *  JTG 082896 REVISED TO ALLOW EXIT BACK WITH OUT UPDATE
      *  BAH 170120 PASSED "S" IN PASS-RTNCD FROM LONPD0 IF SPANISH
      *             PROMPT IS NEEDED, WORLD PR#1218
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)WI/FTENT A30 01/20/17-11:50:34 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  SPANISH-FLAG          PIC X       VALUE SPACES.
       01  LEGEND                PIC X(60).
       01  LEGEND-LIST           PIC X(5)    VALUE "WSEL.".
      * DUMMY FIELD NEEDED FOR LIBGB/SENDMESS
       01  EXT-EOBUF-MESS PIC X(1).

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 FTENT-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/FTENT_DEF.CPY".
       COPY "LIBWI/FTENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  LINK-BUF.
           03  PASS-RTNCD         PIC X.
           03  PASS-FREE-TYPE1    PIC X(70).
           03  PASS-FREE-TYPE2    PIC X(70).

       SCREEN SECTION.
       COPY "LIBWI/FTENT_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME LINK-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
           MOVE "FTENT" TO VDU-FORM-NAME.

            DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 16 * VDU-WIN-L-OFFSET
                      AT SCREEN COL  2 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    78 + VDU-WIN-W-MOD
                      LINES   07 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS FTENT-WINDOW-HANDLE.

      D    STOP MESS.
       MAIN-MODULE.
           PERFORM FORM-RESET.

      * LONPD0 PASSES AN "S" IF SPANISH (BR-STATE "MX")
           IF PASS-RTNCD = "S"
              MOVE "Y" TO SPANISH-FLAG
              MOVE "S  A250TITLE." TO VDU
              MOVE "CAPTURA TIPO DE CREDITO" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE SPACES TO SPANISH-FLAG.

           MOVE " " TO PASS-RTNCD
                       PASS-FREE-TYPE1
                       PASS-FREE-TYPE2.


       ENTER-FREE-TYPE1.
           MOVE "F7-EXIT, F1-RESET, ENTER FREE TYPE LINE 1" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R  A700010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO MAIN-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "X" TO PASS-RTNCD
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPE1.
           MOVE VDUBUF TO PASS-FREE-TYPE1.

       ENTER-FREE-TYPE2.
           MOVE "F7-EXIT, F1-RESET, ^-BACK, ENTER FREE TYPE LINE 2"
                                                           TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A700020." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO MAIN-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FREE-TYPE1.
           IF VDUSTATUS = "1>"
              MOVE "X" TO PASS-RTNCD
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPE2.
           MOVE VDUBUF TO PASS-FREE-TYPE2.

       ENTER-ACCEPT.
           MOVE "S  A010ACCEPT." TO VDU.
           MOVE "A" TO VDUBUF.
           PERFORM SCREENIO.
           IF SPANISH-FLAG = "Y"
              MOVE "F1-RESET, ^-REGRESAR, 'A'-ACEPTAR:" TO LEGEND
           ELSE
              MOVE "F1-RESET, ^-BACK, 'A'-ACCEPT:" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO MAIN-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FREE-TYPE2.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ACCEPT.
           IF VDUBUF NOT = "A"
              GO TO ENTER-ACCEPT.
           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           DISPLAY FTENT-SCREEN UPON FTENT-WINDOW-HANDLE.
           MOVE FTENT-FORM-FIELDS TO WR-BUF.
           MOVE FTENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE FTENT-HELP--FILE TO HELP-LINK-FILE.
           MOVE FTENT-HELP--DIR TO HELP-LINK-DIR.

      ****************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/FTENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       END-ROUTINE.
           EXIT.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FTENT-SCREEN.
           DESTROY FTENT-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
