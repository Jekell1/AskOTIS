       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IRCOPY.
       DATE-WRITTEN.    042403.
      ******************************************************************
      *
      *                 (SP)
      *  DESCRIPTION:   INSURANCE RATE CODE RECORD COPY UTILITY
      *
      * REV:
      * BAH 20190606 QA0016 DUPLICATE RATE CODES EXIST BETWEEN R1 AND R2
      *              MADE THIS UTILITY TO COPY TO A NEW RATE CODE
      *  BAH 20190724 INCREASED IR-KEY TO 9(3), PT0147
      *  BAH 20190930 REMOVED ACCESS-CALL SOURCE & TARGET PATH
      *  BAH 20191113 REMOVED SOURCE AND TARGET
      *
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
           "@(#)SP/IRCOPY.CBL S34 06/19/21-15:20:56 >".

       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  MESS-LIST       PIC X(7) VALUE "MESS:S.".

       01  ERRCD           PIC X    VALUE " ".

       01  IO-TYPE         PIC X(3).

       COPY "LIBLP/LP01IR.CPY" REPLACING LEADING "IR" BY "IROR".

       01  TARGET-KEY      PIC 9(3).

       COPY "LIBWI/IRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  IRCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/IRCOPY_DEF.CPY".
       COPY "LIBSP/IRCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/IRCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      ***********************************
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "IRCOPY" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS IRCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.


       ENTER-KEYS.
           PERFORM UPDATE-FORM.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM CLOSE-IR-FILE.

           GO TO ENTER-KEYS.

      ************************************
       UPDATE-FORM SECTION.
      ************************************
       ENTER-RATE.
           MOVE SPACES TO IO-TYPE.
           MOVE "S  A400DISP1." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A400DISP2." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENNN030KEY1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS = "1<"
              PERFORM RATE-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-RATE
              ELSE
                 MOVE "SN N030KEY1." TO VDU
                 COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70
                 PERFORM SCREENIO
                 COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RATE.

           IF VDUNUM0 < 71
              MOVE "INVALID, MUST BE 71-99" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RATE.

           SUBTRACT 70 FROM VDUNUM0 GIVING IR-KEY.

           MOVE EXT-FILPATH-BASE    TO IR-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO IR-ALL-MACHINE.
           MOVE IR-ALL-PATH         TO IR-PATH.

           MOVE "Y"                 TO IR-PATH-OVERRIDE.

           PERFORM OPEN-IR-FILE.

           PERFORM READ-IR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID RATE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RATE.

      * SAVE ORIGINAL INSURANCE RATE RECORD
           MOVE IR-REC TO IROR-REC.

           MOVE "S  A400DISP1." TO VDU.
           MOVE IR-DESC TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-TO-RATE.
           MOVE "S  A400DISP2." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENNN030KEY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-RATE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TO-RATE.

           IF VDUNUM0 < 71
              MOVE "INVALID, MUST BE 71-99" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-RATE.

           SUBTRACT 70 FROM VDUNUM0 GIVING TARGET-KEY.
           IF TARGET-KEY = 0 OR TARGET-KEY = IR-KEY
              GO TO ENTER-TO-RATE.

           MOVE TARGET-KEY TO IR-KEY.
           PERFORM READ-IR-FILE.
           IF IO-FG = 0
      * ALREADY EXIST
              MOVE "S  A400DISP2." TO VDU
              MOVE IROR-DESC       TO VDUBUF
              PERFORM SCREENIO
      * OVERWRITE QUESTION ?
              PERFORM COPY-IT
              IF IO-FG = 9
                 GO TO ENTER-TO-RATE
              ELSE
                 MOVE IROR-REC   TO IR-REC
                 MOVE TARGET-KEY TO IR-KEY
                 PERFORM REWRITE-IR-FILE
           ELSE
              MOVE IROR-REC   TO IR-REC
              MOVE TARGET-KEY TO IR-KEY
              PERFORM WRITE-IR-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

       END-UPDATE-FORM.
           PERFORM COPY-END.

      ******************************
       RATE-SCAN SECTION.
      ******************************

           MOVE 0           TO IRSCAN-IR-KEY.
           MOVE "WI/IRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH IRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF (IRSCAN-STATUS NOT = "00") OR (IRSCAN-IR-KEY =  0)
              MOVE "XX" TO VDUSTATUS
              GO TO EXIT-RATE-SCAN.

           MOVE "00" TO VDUSTATUS.

           MOVE "SNNN030KEY1." TO VDU.
           COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.
           PERFORM SCREENIO.
           COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.

       EXIT-RATE-SCAN.
           EXIT.

      ******************************
       COPY-IT SECTION.
      ******************************
           MOVE "OVERWRITE RATE RECORD? (Y/N):" TO MESS.
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
           MOVE EXT-CONAME-CONAME TO IRCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IRCOPY-SYSDATE.
           DISPLAY SPACES UPON IRCOPY-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY IRCOPY-SCREEN UPON IRCOPY-WINDOW-HANDLE.
           MOVE IRCOPY-FORM-FIELDS TO WR-BUF.
           MOVE IRCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE IRCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/IRCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       COPY-END.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.


      ***************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-IR-FILE.
       COPY "LIBLP/LPIRGS_SQL.CPY".
      ***************************************
       COPY "LIBLP/LPIRIN.CPY".

      *=======================================[ NOTE: NEEDS TO BE LAST ]
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
           DESTROY IRCOPY-SCREEN.
           DESTROY IRCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
