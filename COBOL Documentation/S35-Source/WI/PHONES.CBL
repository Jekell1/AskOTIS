       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PHONES.
       DATE-WRITTEN.    021698.
      ********************************************************************
      *          WINDOW PHONE NO. DISPLAY
      *
      * REV:
      *  JTG 021698 CHANGED TO ALLOW CTRL-N TO MAKERS WINDOW MAKERS.C
      *  BAH 190128 SPLIT CDFILE
      ********************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/PHONES.CBL S35 02/23/24-11:34:59 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".


       01  ERRCD                 PIC X.
       01  SUB                   PIC 9(3) COMP-3  VALUE 0.

      **************************************************
      *    DISPLAY BUFFER
      **************************************************
       01  DISPLAY-BUF        VALUE " ".
           03  DISPLAY-BUFX OCCURS 6 TIMES.
               05  D-PHONE         PIC X(12).
               05  D-EXT           PIC Z(4).
               05  D-PHONE-CD      PIC XX.
               05  D-PHONE-DESC    PIC X(20).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 011 012 013 020 021 022 023 ".
           03  FILLER      PIC X(32) VALUE
           "030 031 032 033 040 041 042 043 ".
           03  FILLER      PIC X(32) VALUE
           "050 051 052 053 060 061 062 063.".

       01  LEGEND                  PIC X(39).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".

       01  PHONES-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PHONES_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/PHONES_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PHONES_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-CLMENU-SELECTION.
           MOVE "PHONES" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 09 * VDU-WIN-L-OFFSET
               AT SCREEN COL 01 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 80 + VDU-WIN-W-MOD
               LINES 11 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS PHONES-WINDOW-HANDLE.

            PERFORM FORM-RESET.

       MAIN-MODULE.
           MOVE " " TO ERRCD.

           MOVE "S  A110TITLE." TO VDU.
           MOVE "MISC PHONE" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EXT-WI-CL-SSNO(1) TO BW-SSNO.
           PERFORM GET-BW.

           PERFORM DISPLAY-FIELDS.

       ACCEPT-IT.
           MOVE " CTRL N MAKERS, EXIT-ANYKEY" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE " " TO VDU-CTRL-N-PROG.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           GO TO END-ROUTINE.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BUF.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF BW-MISC-PHONE(SUB) NOT = 0
                 MOVE BW-MISC-PHONE(SUB) TO TEL-BUF
                 PERFORM SEND-PHONE
                 MOVE TEL-EDIT TO D-PHONE(SUB)
                 MOVE BW-MISC-EXT(SUB) TO D-EXT(SUB)
                 MOVE "PH"                  TO CD-TYPE
                 MOVE BW-MISC-PHONE-CD(SUB) TO D-PHONE-CD(SUB)
                                               CD-CODE
                 PERFORM GET-CD
                 MOVE CD-DESC TO D-PHONE-DESC(SUB)
              END-IF
           END-PERFORM.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SPACES UPON PHONES-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PHONES-SCREEN UPON PHONES-WINDOW-HANDLE.
           MOVE PHONES-FORM-FIELDS TO WR-BUF.
           MOVE PHONES-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE PHONES-HELP--FILE TO HELP-LINK-FILE.
           MOVE PHONES-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PHONES_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO CD-DESC.
       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              MOVE "**MISSING**" TO BW-LNAME.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PHONES-SCREEN.
           DESTROY PHONES-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
