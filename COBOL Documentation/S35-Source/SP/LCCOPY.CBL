       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LCCOPY.
       DATE-WRITTEN.    042403.
      ******************************************************************
      *
      *                 (SP)
      *  DESCRIPTION:   LOAN CLASS RECORD COPY UTILITY
      *
      *=================================================================
      * BAH 030424 NEW (FOR JUDY CAUSE SHE WHINED LONG ENOUGH)
      * BAH 111207 REPLACED RRFORM WITH SCREEN
      * KEC 2017.0221 FIXED THE WK FILE TO MATCH LC FILE (REPLACING LEADING).
      * KEC 2018-1004 CONVERTED LCFILE FOR A32 & A34 
      * BAH 20191007 REMOVED ACCESS-CALL SOURCE, TARGET
      * BAH 20191112 REMOVED SOURCE AND TARGET
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
           "@(#)SP/LCCOPY.CBL S35 02/23/24-10:25:50 >".

       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01  MESS-LIST       PIC X(7) VALUE "MESS:S.".

       01  ERRCD           PIC X    VALUE " ".

       01  CURRENT-PATH.
           03  CUR-PATH-BASE       PIC X(9).
           03  FILLER              PIC X      VALUE "/".
           03  CUR-PATH-MACHINE    PIC XX.
           03  FILLER              PIC X      VALUE "/".
           03  CUR-PATH-DATA       PIC X(6).

       01  IO-TYPE         PIC X(3).

       COPY "LIBLP/LP01LC.CPY" REPLACING LEADING "LC" BY "LCOR".

       01  TARGET-KEY        PIC 9(3).


       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  LCCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LCCOPY_DEF.CPY".
       COPY "LIBSP/LCCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LCCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      ***********************************
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LCCOPY" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LCCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      * USR/DATA/R1
           MOVE EXT-FILPATH-BASE    TO CUR-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO CUR-PATH-MACHINE.
           MOVE "S  A000CURFIL." TO VDU.
           MOVE CURRENT-PATH TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-KEYS.
           PERFORM UPDATE-FORM.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM CLOSE-LC1-FILE.

           GO TO ENTER-KEYS.

      ************************************
       UPDATE-FORM SECTION.
      ************************************
       ENTER-CLASS.
           MOVE SPACES TO IO-TYPE.

           MOVE "ENNN030KEY1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-CLASS
              ELSE
                 MOVE "SN N030KEY1." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY TO VDUNUM0.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CLASS.

           MOVE VDUNUM0 TO LC-CODE.


           MOVE EXT-FILPATH-BASE    TO LC-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO LC-ALL-MACHINE.
           MOVE LC-ALL-PATH         TO LC-PATH.

           MOVE "Y"                 TO LC-PATH-OVERRIDE.

           PERFORM OPEN-LC1-FILE.

           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLASS.

      * SAVE ORIGINAL LOAN CLASS RECORD
           MOVE LC-REC TO LCOR-REC.

           MOVE "S  A400DISP1." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-TO-CLASS.
           MOVE "S  A400DISP2." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENNN030KEY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-CLASS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TO-CLASS.

           MOVE VDUNUM0 TO TARGET-KEY.

           IF (TARGET-KEY = 0) OR (TARGET-KEY = LC-CODE) OR
               (TARGET-KEY > 99)
              MOVE "INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-CLASS.

           MOVE TARGET-KEY TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 0
      * ALREADY EXIST
              MOVE "S  A400DISP2." TO VDU
              MOVE LCOR-DESC       TO VDUBUF
              PERFORM SCREENIO
      * OVERWRITE QUESTION ?
              PERFORM COPY-IT
              IF IO-FG = 9
                 GO TO ENTER-TO-CLASS
              ELSE
                 MOVE LCOR-REC   TO LC-REC
                 MOVE TARGET-KEY TO LC-CODE
                 PERFORM REWRITE-LC1-FILE
           ELSE
              MOVE LCOR-REC   TO LC-REC
              MOVE TARGET-KEY TO LC-CODE
              PERFORM WRITE-LC1-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

       END-UPDATE-FORM.
           PERFORM COPY-END.

      ******************************
       CLASS-SCAN SECTION.
      ******************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE 0               TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************
       COPY-IT SECTION.
      ******************************
           MOVE "OVERWRITE CLASS RECORD? (Y/N):" TO MESS.
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
           MOVE EXT-CONAME-CONAME TO LCCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LCCOPY-SYSDATE.
           DISPLAY SPACES UPON LCCOPY-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LCCOPY-SCREEN UPON LCCOPY-WINDOW-HANDLE.
           MOVE LCCOPY-FORM-FIELDS TO WR-BUF.
           MOVE LCCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE LCCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE LCCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LCCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       COPY-END.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.


      ***************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
      ***************************************
       COPY "LIBLP/LPLC1IN.CPY".

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
           DESTROY LCCOPY-SCREEN.
           DESTROY LCCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
