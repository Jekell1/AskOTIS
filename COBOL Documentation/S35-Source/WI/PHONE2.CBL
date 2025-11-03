       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PHONE2.
       AUTHOR.             MJD.
       DATE-WRITTEN. 2000/05/02.
      ********************************************************************
      *          WINDOW CONTACT PHONE NO. DISPLAY
      *             CALLED FROM WI/MAKERS IF CONTACT INFO EXISTS
      *                         (CTRL N)
      *
      * REV:
      *  MJD 000502 NEW PROGRAM - DISPLAYS CONTACT INFO FROM
      *             BM-REC (TFC PR#37)
      *  BAH 160824 BM CONTACT FIELDS GOT MOVED TO BW REC
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
           "@(#)WI/PHONE2.CBL S34 05/24/21-08:57:26 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  ERRCD                 PIC X.
       01  SUB                   PIC 9(3) COMP-3  VALUE 0.

      **************************************************
      *    DISPLAY BUFFER
      **************************************************
       01  DISPLAY-BUF        VALUE " ".
           03  CONTACT-BUF OCCURS 10 TIMES.
               05  D-CONTACT-DESC  PIC X(46).
               05  D-CONTACT-PHONE PIC X(12).
               05  D-CONTACT-EXT   PIC Z(04).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(36) VALUE
           "010 011 012 020 021 022 030 031 032 ".
           03  FILLER      PIC X(36) VALUE
           "040 041 042 050 051 052 060 061 062 ".
           03  FILLER      PIC X(36) VALUE
           "070 071 072 080 081 082 090 091 092 ".
           03  FILLER      PIC X(12) VALUE
           "100 101 102.".

       01  LEGEND                  PIC X(39).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".

       01  PHONE2-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PHONE2_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/PHONE2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PHONE2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-CLMENU-SELECTION.
      D    STOP MESS.
           MOVE "PHONE2" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 09 * VDU-WIN-L-OFFSET
               AT SCREEN COL 01 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 80 + VDU-WIN-W-MOD
               LINES 13 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS PHONE2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.
           MOVE " " TO ERRCD.
           MOVE "S  A110TITLE." TO VDU.
           MOVE "  CONTACTS " TO VDUBUF.
           PERFORM SCREENIO.

           
           PERFORM OPEN-BW1-FILE.
           MOVE EXT-WI-CL-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-REC.
           PERFORM CLOSE-BW1-FILE.

           PERFORM DISPLAY-FIELDS.

       ACCEPT-IT.
           MOVE " CTRL N MAKERS, EXIT-ANYKEY" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "WI/PHONES" TO VDU-CTRL-N-PROG.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           GO TO END-ROUTINE.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BUF.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF BW-CONTACT-DESC(SUB) NOT = SPACES
                 IF BW-CONTACT-PHONE(SUB) = 0
                    MOVE SPACES TO D-CONTACT-PHONE(SUB)
                 ELSE
                    MOVE BW-CONTACT-PHONE(SUB) TO TEL-BUF
                    PERFORM SEND-PHONE
                    MOVE TEL-EDIT TO D-CONTACT-PHONE(SUB)
                 END-IF
                 MOVE BW-CONTACT-EXT(SUB) TO D-CONTACT-EXT(SUB)
                 MOVE BW-CONTACT-DESC(SUB) TO D-CONTACT-DESC(SUB)
              END-IF
           END-PERFORM.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SPACES UPON PHONE2-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PHONE2-SCREEN UPON PHONE2-WINDOW-HANDLE.
           MOVE PHONE2-FORM-FIELDS TO WR-BUF.
           MOVE PHONE2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE PHONE2-HELP--FILE TO HELP-LINK-FILE.
           MOVE PHONE2-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PHONE2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PHONE2-SCREEN.
           DESTROY PHONE2-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
