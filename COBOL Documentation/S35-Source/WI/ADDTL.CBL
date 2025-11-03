       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ADDTL.
       DATE-WRITTEN.    04/21/2022.
      ******************************************************************
      *
      *                      ( WI )
      *              ADDITIONAL LOAN INFO (SECURITIZATION)
      *                 CALLED FROM F9 CLMENU
      * REV:
      *  BAH 2022.0421 NEW #1551
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
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)WI/ADDTL S35 04/16/24-17:28:32 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  ERRCD             PIC X     VALUE SPACES.

       01  DISPLAY-LIST.
           03  FILLER             PIC X(50)  VALUE
           "POOLID POOLID-D ACTION2 ACTION2-D LPO LPO-D.".
       01  DISPLAY-BUF.
           03  D-POOLID           PIC Z(5).
           03  D-POOLID-DESC      PIC X(35).
           03  D-ACTIONCD2        PIC X(4).
           03  D-ACTIONCD2-DESC   PIC X(35).
           03  D-LPO              PIC X(4).
           03  D-LPO-DESC         PIC X(35).

      ******************************************************************
      *     C O P Y    L I B R A R Y    W O R K E R S
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ADDTL_DEF.CPY".
       COPY "LIBWI/ADDTL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/ADDTL_SCN.CPY".

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE 18 TO WINDOW-LINES.
           MOVE 44 TO WINDOW-COLUMNS.
           MOVE 6  TO WINDOW-BEGIN-Y.
           MOVE 19 TO WINDOW-BEGIN-X.
           MOVE "WI/ADDTL" TO WINDOW-FORM-NAM.
           MOVE "ADDTL" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY ADDTL-SCREEN.

           MOVE ADDTL-FORM-FIELDS TO WR-BUF.
           MOVE ADDTL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.

           PERFORM DISPLAY-SCREEN.

       ENTER-FUNCTION-KEY.
           MOVE "F7-EXIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           GO TO ENTER-FUNCTION-KEY.

      ***********************************
       DISPLAY-SCREEN SECTION.
      ***********************************

           PERFORM OPEN-CD1-FILE.

      * POOLID
           MOVE "PL"        TO CD-TYPE.
           MOVE LN-DLPOOL   TO CD-PL-POOLID D-POOLID.
           MOVE SPACES      TO CD-PL-FILLER.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO      D-POOLID-DESC
           ELSE
              MOVE CD-DESC        TO      D-POOLID-DESC.
            

           MOVE "A2"         TO CD-TYPE.
           MOVE LN-ACTIONCD2 TO CD-CODE   D-ACTIONCD2.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO        D-ACTIONCD2-DESC
           ELSE
              MOVE CD-DESC        TO        D-ACTIONCD2-DESC.

           MOVE "LO"         TO CD-TYPE.
           MOVE LN-LPO       TO CD-CODE  D-LPO.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO        D-LPO-DESC
           ELSE
              MOVE CD-DESC        TO        D-LPO-DESC.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-SCREEN-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ADDTL-HELP--FILE TO HELP-LINK-FILE.
           MOVE ADDTL-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ADDTL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ADDTL-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
