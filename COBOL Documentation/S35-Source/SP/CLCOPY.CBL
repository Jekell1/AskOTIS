       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLCOPY.
       DATE-WRITTEN.    061297.
      ******************************************************************
      *                 (SP)
      *  DESCRIPTION:   CAN LOAN COPY UTILITY
      *
      * REV:
      *  MJD NEW PROGRAM, BASED ON PDTFR.C TO COPY CAN LOANS.
      *  MJD 112197 EXPANDED PD-REC TO 2259 BYTES
      *  MJD 042798 EXPANDES WK-PD-REC BY 30 BYTES. USED 78 LEVEL.
      *             WK-REC DID NOT MATCH CL-REC
      *  JTG 040213 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C)       WORLD J043
      *             SET CLSCAN-NON-MONTHLY - 'A' TO DISPLAY ALL CAN LOANS
      * KEC 2016.0309 CHANGED CD-CL???? TO BE CD-CL-????.
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2016.1004 USED 'REPLACING LEADING' OF "CL" BY "WK" ON FS & FD.
      * BAH 181024 GLOBAL CLFILE
      * BAH 20191004 REMOVED ACCESS-CALL CLFILE SOURCE & TARGET
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CLCOPY.CBL S35 02/23/24-11:29:48 >".

       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".

       01  MESS-LIST       PIC X(5) VALUE "MESS.".

       01  ERRCD           PIC X    VALUE " ".

       COPY "LIBLP/LP01CL.CPY" REPLACING LEADING "CL" BY "CLOR".


       01  FIRST-TIME         PIC X    VALUE "Y".

      *-------------------------------------
      * USER INPUT FIELDS
      *-------------------------------------
       01  FROM-GROUP-ID   PIC X(4).
       01  FROM-CAN-NO     PIC XX.
       01  TO-GROUP-ID     PIC X(4).
       01  TO-CAN-NO       PIC XX.
       01  OVERWRITE-FG    PIC X VALUE "N".

       01  ALL-LOANS           PIC X VALUE "N".

       01  IO-TYPE         PIC X(3).

       01  RECORDS-UPDATED PIC X  VALUE "N".
           88 RECORDS-WRITTEN     VALUE "Y".

      *-------------------------------------
      * PROCESSING MESSAGE
      *-------------------------------------
       01  PROG-MSG.
           03  FILLER      PIC X(23) VALUE
               "NOW COPYING CAN LOAN # ".
           03  D-FROM-GR   PIC X(4).
           03  FILLER      PIC X     VALUE "-".
           03  D-FROM-CAN  PIC XX.
           03  FILLER      PIC X(15) VALUE
               " TO CAN LOAN # ".
           03  D-TO-GR     PIC X(4).
           03  FILLER      PIC X     VALUE "-".
           03  D-TO-CAN    PIC XX.

      * PD-REC NEEDED FOR FIELD REDEFINITION OF CL-PD-REC.
       COPY "LIBLP/LP01PD.CPY".

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TSTCANIDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/CLSCANW.CPY".
       01  CLCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CLCOPY_DEF.CPY".
       COPY "LIBSP/CLCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLCOPY_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *******************************************************
       MAIN-PROGRAM SECTION.
      *******************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLCOPY"      TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CLCOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "Y" TO FIRST-TIME.

           MOVE "Y"                 TO CL-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE    TO CL-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO CL-ALL-MACHINE.
           MOVE CL-ALL-PATH         TO CL-PATH.

           PERFORM OPEN-CL1-FILE.
           PERFORM OPEN-CD1-FILE.

       ENTER-KEYS.

           PERFORM ENTER-SCREEN-FIELDS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-ALL.

           GO TO ENTER-KEYS.

      ************************************
       ENTER-SCREEN-FIELDS SECTION.
      ************************************

           MOVE "N" TO FIRST-TIME.
           MOVE "N" TO OVERWRITE-FG.
       
       ENTER-FROM-GROUP.
           MOVE SPACES TO IO-TYPE.

           MOVE "E  A040GR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF CDSCAN-STATUS NOT = "00"
                 GO TO ENTER-FROM-GROUP
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FROM-GROUP.
           IF VDUBUF = SPACES
              GO TO ENTER-FROM-GROUP.

           MOVE VDUBUF TO TCAN-WORD.
           MOVE "N" TO TCAN-SKIP-CD-VERIFY.
           PERFORM TEST-CAN-GROUP.

      * REDISPLAY CORRECTED GROUP NAME
           MOVE "S  A040GR1." TO VDU.
           MOVE TCAN-WORD     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020GRDESC." TO VDU.
           MOVE CD-DESC          TO VDUBUF.
           PERFORM SCREENIO.
           IF TCAN-ERR = 1
              MOVE "CAN GROUP ID NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FROM-GROUP.
           MOVE TCAN-WORD TO FROM-GROUP-ID.

       ENTER-FROM-CAN-NO.
           MOVE "E  A020CAN1." TO VDU.
           PERFORM SCREENIO.

           MOVE "N" TO ALL-LOANS.

           IF VDUSTATUS = "16"
              MOVE "S  A020CAN1." TO VDU
              MOVE "~~"           TO FROM-CAN-NO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"            TO ALL-LOANS
              GO TO ENTER-TO-GROUP.

           IF VDUSTATUS = "1<"
              MOVE FROM-GROUP-ID TO CLSCAN-GROUP-ID
              PERFORM CAN-LOAN-SCAN
              IF CLSCAN-STATUS NOT = "00"
                 GO TO ENTER-FROM-CAN-NO
              ELSE
                 MOVE "S  A020CAN1." TO VDU
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 PERFORM SCREENIO
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS = "10" GO TO ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1U" GO TO ENTER-FROM-GROUP.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FROM-CAN-NO.
           IF VDUBUF = "  "
              GO TO ENTER-FROM-CAN-NO.
           MOVE VDUBUF TO FROM-CAN-NO.

           PERFORM FIND-SOURCE-RECORD.
           IF IO-FG NOT = 0
              GO TO ENTER-FROM-CAN-NO.

       ENTER-TO-GROUP.
           MOVE SPACES TO IO-TYPE.
           MOVE "E  A040GR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1U" GO TO ENTER-FROM-CAN-NO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF CDSCAN-STATUS NOT = "00"
                 GO TO ENTER-FROM-GROUP
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-TO-GROUP.
           IF VDUBUF = SPACES
              GO TO ENTER-TO-GROUP.
           MOVE VDUBUF TO TCAN-WORD.
           MOVE "N" TO TCAN-SKIP-CD-VERIFY.
           PERFORM TEST-CAN-GROUP.
      * REDISPLAY CORRECTED GROUP NAME
           MOVE "S  A040GR2." TO VDU.
           MOVE TCAN-WORD TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020GR2DESC." TO VDU.
           MOVE CD-DESC           TO VDUBUF.
           PERFORM SCREENIO.

           IF TCAN-ERR = 1
              MOVE "CAN GROUP ID NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-GROUP.

           MOVE TCAN-WORD TO TO-GROUP-ID.
           IF ALL-LOANS = "Y"
              IF FROM-GROUP-ID = TO-GROUP-ID
                 MOVE "SOURCE KEY AND TARGET KEY MUST BE DIFFERENT"
                                                             TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-TO-GROUP.

       ENTER-TO-CAN-NO.
           IF ALL-LOANS = "Y"
              MOVE "S  A020CAN2." TO VDU
              MOVE "~~"           TO VDUBUF
              PERFORM SCREENIO
              GO TO OVERWRITE-IT.

           MOVE "E  A020CAN2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              MOVE FROM-GROUP-ID TO CLSCAN-GROUP-ID
              PERFORM CAN-LOAN-SCAN
              IF CLSCAN-STATUS NOT = "00"
                 GO TO ENTER-TO-CAN-NO
              ELSE
                 MOVE "S  A020CAN2." TO VDU
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 PERFORM SCREENIO
                 MOVE CLSCAN-CAN-NO TO VDUBUF
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS = "10" GO TO ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1U" GO TO ENTER-TO-GROUP.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-TO-CAN-NO.
           IF VDUBUF = "  "
              GO TO ENTER-TO-CAN-NO.
           MOVE VDUBUF TO TO-CAN-NO.
           IF FROM-GROUP-ID = TO-GROUP-ID
              IF TO-CAN-NO = FROM-CAN-NO
                 MOVE "SOURCE KEY AND TARGET KEY MUST BE DIFFERENT"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-TO-CAN-NO.

       OVERWRITE-IT.
           MOVE "E  A000OVERW." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "10" GO TO ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1U"
              IF ALL-LOANS = "Y"
                 GO TO ENTER-TO-GROUP
              ELSE
                 GO TO ENTER-TO-CAN-NO.
           IF VDUSTATUS NOT = "00" GO TO OVERWRITE-IT.
           IF VDUBUF = "N" GO TO ACCEPT-IT.
           IF VDUBUF NOT = "Y"
              GO TO OVERWRITE-IT.
           MOVE VDUBUF TO OVERWRITE-FG.

       ACCEPT-IT.
           MOVE "E RA000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "10" GO TO ENTER-SCREEN-FIELDS.
           IF VDUSTATUS = "1U" GO TO OVERWRITE-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N" GO TO ENTER-FROM-GROUP.
           IF VDUBUF NOT = "Y"
              GO TO ACCEPT-IT.

       END-ENTER-SCREEN-FIELDS.
           EXIT.

      ******************************************************************
       UPDATE-ALL SECTION.
      ******************************************************************
           MOVE "N" TO RECORDS-UPDATED.

           IF ALL-LOANS = "N"
              GO TO MOVE-RECORD.

           MOVE FROM-GROUP-ID TO CL-GROUP-ID.
           MOVE SPACES        TO CL-CAN-NO.
           PERFORM START-CL1-FILE.

       UPDATE-ALL-NEXT.
           PERFORM READ-CL1-FILE-NEXT.
           IF IO-FG = 9
              GO TO UPDATE-ALL-EXIT.

           IF CL-GROUP-ID NOT = FROM-GROUP-ID
              GO TO UPDATE-ALL-EXIT.

       MOVE-RECORD.
      * SAVE ORIGINAL
           MOVE CL-REC       TO CLOR-REC.

           IF ALL-LOANS = "N"
              MOVE TO-CAN-NO TO CL-CAN-NO.
           MOVE TO-GROUP-ID  TO CL-GROUP-ID.
           PERFORM READ-CL1-FILE.
           IF IO-FG = 9
              GO TO UPDATE-ALL-WRITE-CL1.

      * RECORD ALREADY EXIST

           IF OVERWRITE-FG = "N"
              GO TO CHECK-IF-ALL-LOANS.

           PERFORM COPY-BEGIN.

           MOVE CLOR-REC    TO CL-REC.
           MOVE TO-GROUP-ID TO CL-GROUP-ID.
           IF ALL-LOANS = "N"
              MOVE CL-PD-REC TO PD-REC
              MOVE TO-CAN-NO TO PD-CAN-LOAN-NO
              MOVE PD-REC TO CL-PD-REC
              MOVE TO-CAN-NO TO CL-CAN-NO.

           PERFORM REWRITE-CL1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO RECORDS-UPDATED.
           GO TO CHECK-IF-ALL-LOANS.

       UPDATE-ALL-WRITE-CL1.
           PERFORM COPY-BEGIN.

           MOVE CLOR-REC    TO CL-REC.
           MOVE TO-GROUP-ID TO CL-GROUP-ID.
           IF ALL-LOANS = "N"
              MOVE CL-PD-REC TO PD-REC
              MOVE TO-CAN-NO TO PD-CAN-LOAN-NO
              MOVE PD-REC TO CL-PD-REC
              MOVE TO-CAN-NO TO CL-CAN-NO.

           PERFORM WRITE-CL1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO RECORDS-UPDATED.

       CHECK-IF-ALL-LOANS.
           IF ALL-LOANS = "Y"
              MOVE CLOR-REC TO CL-REC
      * GET ORIGINAL BACK IN THE BUFFER
              PERFORM START-CL1-FILE
              PERFORM READ-CL1-FILE-NEXT
              GO TO UPDATE-ALL-NEXT.

       UPDATE-ALL-EXIT.
           PERFORM RESET-LEGEND.
           IF RECORDS-WRITTEN
              PERFORM COPY-END
           ELSE
              PERFORM COPY-END2.

      ************************************
       FIND-SOURCE-RECORD SECTION.
      ************************************
           MOVE FROM-GROUP-ID TO CL-GROUP-ID.
           MOVE FROM-CAN-NO   TO CL-CAN-NO.
           PERFORM READ-CL1-FILE.
           IF IO-FG = 9
              MOVE "RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

       FIND-SOURCE-RECORD-EXIT.
           EXIT.

      ********************************************
       CODE-SCAN SECTION.
      ********************************************
           MOVE "CL"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-CODE-SCAN.
           EXIT.

      ************************************
       CAN-LOAN-SCAN SECTION.
      ************************************

      * SET CLSCAN-NON-MONTHLY - 'A' TO DISPLAY ALL CAN LOANS

           MOVE "A"        TO CLSCAN-NON-MONTHLY.
           MOVE 1          TO CLSCAN-SCAN-TYPE.
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
           MOVE EXT-CONAME-CONAME TO CLCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CLCOPY-SCREEN UPON CLCOPY-WINDOW-HANDLE.
           MOVE CLCOPY-FORM-FIELDS TO WR-BUF.
           MOVE CLCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE CLCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       COPY-BEGIN.
           MOVE FROM-GROUP-ID TO D-FROM-GR.
           MOVE TO-GROUP-ID   TO D-TO-GR.
           MOVE CL-CAN-NO     TO D-FROM-CAN.
           IF ALL-LOANS = "Y"
              MOVE CL-CAN-NO  TO D-TO-CAN
           ELSE
              MOVE TO-CAN-NO  TO D-TO-CAN.
           MOVE PROG-MSG TO MESS.
           PERFORM SEND-LEGEND.
       RESET-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       COPY-END.
           MOVE "CANNED LOAN TRANSFER COMPLETE." TO MESS.
           PERFORM SEND-MESS.
       COPY-END2.
           MOVE "NO RECORDS COPIED FOR SELECTED RANGE." TO MESS.
           PERFORM SEND-MESS.
       COPY "LIBGB/ACCESS.CPY".

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCL1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GB/INSTMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLCOPY-SCREEN.
           DESTROY CLCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
