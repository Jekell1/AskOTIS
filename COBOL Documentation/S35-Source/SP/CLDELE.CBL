       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLDELE.
       DATE-WRITTEN.    061297.
      ******************************************************************
      *                 (SP)
      *  DESCRIPTION:   CAN LOAN DELETE UTILITY
      *
      * REV:
      *  MJD 061397 NEW PROGRAM BASED ON SP/PDDEL.C
      *  JTG 040210 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      * KEC 20160309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-CL???? TO BE CD-CL-????.
      * KEC 20160325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BAH 20200624 ADDED OPEN-CD1-FILE, REMOVED FROM TSTCANID COPY
      *                                                  #TST001
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/CLDELE.CBL S35 09/20/22-14:07:50 >".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      **********************************
      *         MISC WORKERS           *
      **********************************
       01  MESS-LIST       PIC X(5) VALUE "MESS.".

       01  ERRCD           PIC X    VALUE " ".

       01  IO-TYPE         PIC X(3).

      *-----------------------------------------------------------------
      * SCREEN ENTRY FIELDS
      *-----------------------------------------------------------------
       01  GROUP-ID        PIC 9(4).
       01  BEG-CAN-NO      PIC 9(2).
       01  END-CAN-NO      PIC 9(2).

      *-----------------------------------------------------------------
      * PROCESSING MESSAGE
      *-----------------------------------------------------------------
       01  PROG-MSG.
           03  FILLER      PIC X(24) VALUE
               "NOW DELETING CAN LOAN # ".
           03  D-GROUP-ID  PIC X(4).
           03  FILLER      PIC X     VALUE "-".
           03  D-CAN       PIC XX.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/CLSCANW.CPY".
       COPY "LIBGB/TSTCANIDW.CPY".
       01  CLDELE-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CLDELE_DEF.CPY".
       COPY "LIBSP/CLDELE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLDELE_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE "CLDELE" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CLDELE-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-CD1-FILE.
       ENTER-KEYS.

           PERFORM ENTER-SCREEN-FIELDS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-ALL.

           GO TO ENTER-KEYS.

      ***************************************
       ENTER-SCREEN-FIELDS SECTION.
      ***************************************
       ENTER-GROUP-ID.
           MOVE "E  A040GR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF CDSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUP-ID
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-GROUP-ID.

           MOVE VDUBUF TO TCAN-WORD.
           MOVE "N" TO TCAN-SKIP-CD-VERIFY.
           PERFORM TEST-CAN-GROUP.
           IF TCAN-ERR = 1
              MOVE "S  A020GRDESC." TO VDU
              MOVE "INVALID GROUP" TO VDUBUF
              PERFORM SCREENIO
              MOVE "CAN GROUP ID NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP-ID.

           MOVE "S  A040GR1." TO VDU.
           MOVE TCAN-WORD TO VDUBUF GROUP-ID.
           PERFORM SCREENIO.
           MOVE "S  A020GRDESC." TO VDU.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-BEG-CAN-LOAN.
           MOVE "ENNA020BEGCAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              MOVE GROUP-ID TO CLSCAN-GROUP-ID
              PERFORM CAN-LOAN-SCAN
              IF CLSCAN-STATUS NOT = "00"
                 GO TO ENTER-BEG-CAN-LOAN
              ELSE
                 MOVE "S  A020BEGCAN." TO VDU
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 PERFORM SCREENIO
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "16"
              MOVE "S  A020BEGCAN." TO VDU
              MOVE "  " TO BEG-CAN-NO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020ENDCAN." TO VDU
              MOVE ALL "Z" TO END-CAN-NO
              CALL "C$TOLOWER" USING END-CAN-NO, VALUE 2
              MOVE END-CAN-NO TO VDUBUF
              PERFORM SCREENIO
              GO TO ACCEPT-IT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-GROUP-ID.
           IF VDUSTATUS = "10"
              GO TO ENTER-GROUP-ID.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-CAN-LOAN.
           IF VDUBUF = SPACES
              GO TO ENTER-BEG-CAN-LOAN.
           MOVE VDUBUF TO BEG-CAN-NO.

       ENTER-END-CAN-LOAN.
           MOVE "ENNA020ENDCAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              MOVE GROUP-ID TO CLSCAN-GROUP-ID
              PERFORM CAN-LOAN-SCAN
              IF CLSCAN-STATUS NOT = "00"
                 GO TO ENTER-END-CAN-LOAN
              ELSE
                 MOVE "S  A020ENDCAN." TO VDU
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 PERFORM SCREENIO
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-CAN-LOAN.
           IF VDUSTATUS = "10"
              GO TO ENTER-GROUP-ID.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-CAN-LOAN.
           IF VDUBUF = SPACES
              GO TO ENTER-END-CAN-LOAN.
           MOVE VDUBUF TO END-CAN-NO.
           IF END-CAN-NO < BEG-CAN-NO
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-CAN-LOAN.

       ACCEPT-IT.
           MOVE "E RA000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "10" GO TO ENTER-GROUP-ID.
           IF VDUSTATUS = "1U" GO TO ENTER-END-CAN-LOAN.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N" GO TO ENTER-GROUP-ID.
           IF VDUBUF NOT = "Y"
              GO TO ACCEPT-IT.

       END-ENTER-SCREEN-FIELDS.
           EXIT.

      ***************************************
       UPDATE-ALL SECTION.
      ***************************************
           PERFORM OPEN-CL1-FILE.
           MOVE GROUP-ID TO CL-GROUP-ID
                            QCL1-WBEG-GROUP
                            QCL1-WEND-GROUP.
           MOVE SPACES   TO CL-CAN-NO
                            QCL1-WBEG-CAN-NO.
           MOVE ALL "Z"  TO QCL1-WEND-CAN-NO.

           PERFORM START-CL1-FILE.

       UPDATE-ALL-NEXT.
           PERFORM READ-CL1-FILE-NEXT.
           IF IO-FG = 9
              GO TO UPDATE-ALL-EXIT.

           IF CL-GROUP-ID NOT = GROUP-ID
              GO TO UPDATE-ALL-EXIT.

           IF CL-CAN-NO < BEG-CAN-NO OR
               CL-CAN-NO > END-CAN-NO
             GO TO UPDATE-ALL-NEXT.

           PERFORM COPY-BEGIN.

           PERFORM DELETE-CL1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO UPDATE-ALL-NEXT.

       UPDATE-ALL-EXIT.
           PERFORM RESET-LEGEND.
           PERFORM COPY-END.

      **********************************************
       CODE-SCAN SECTION.
      **********************************************
           MOVE "CL"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-CODE-SCAN.
           EXIT.

      **********************************************************************
       CAN-LOAN-SCAN SECTION.
      **********************************************************************

      * SET CLSCAN-NON-MONTHLY - 'A' TO DISPLAY ALL CAN LOANS WORLD J043

           MOVE "A" TO CLSCAN-NON-MONTHLY.
           MOVE 1   TO CLSCAN-SCAN-TYPE.
           MOVE "WI/CLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-CAN-LOAN-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TSTCANID.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO CLDELE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLDELE-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CLDELE-SCREEN UPON CLDELE-WINDOW-HANDLE.
           MOVE CLDELE-FORM-FIELDS TO WR-BUF.
           MOVE CLDELE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE CLDELE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLDELE-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLDELE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       COPY-BEGIN.
           MOVE GROUP-ID TO D-GROUP-ID.
           MOVE CL-CAN-NO TO D-CAN.
           MOVE PROG-MSG TO MESS.
           PERFORM SEND-LEGEND.
       COPY-END.
           MOVE "CANNED LOAN DELETE COMPLETE." TO MESS.
           PERFORM SEND-MESS.

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCL1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
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
           DESTROY CLDELE-SCREEN.
           DESTROY CLDELE-WINDOW-HANDLE.
           EXIT PROGRAM.
