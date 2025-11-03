       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TFCCDS.
       DATE-WRITTEN.    030899.
      *******************************************************
      *  DESC:  LPO, REP, AND PROCESS CODE ENTRY FOR TFC
      *
      * REV:
      *  051399 GLF DO NOT ALLOW EXITING UNLESS ALL FIELDS
      *             ARE FILLED IN WITH CORRECT DATA, PR# 27
      *  MJD 990702 IF LPO CODE COMES IN AS NON SPACES THEN
      *             DISPLAY AND SKIP TO REP CODE ENTRY (TFC #31)
      *  BAH 990804 LPO CODE WAS ONLY TESTING 3 CHARACTERS
      *             BECAUSE IT WAS MOVING IT TO CD-CODE INSTEAD
      *             OF CD-BW-CODE....TFC
      *  BAH 130131 A30 CONVERSION
      *  BAH 130213 PASSWORD CHANGING LPO FROM ANYTHING OTHER THAN SPACES
      *                                                EXCEL PR# 4213
      * KEC 2016.0309 CHANGED CD-TYPE "LO" TO USE NEW LAYOUT (CD-LO-REC).
      * KEC 2016.0311 CHANGED CD-RC-CODE TO NO LONGER USE HASH.
      *               REMOVED CD-RC-CODE-ALPHA; USING CD-RC-CODE NOW.
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * BAH 190128 SPLIT CDFILE
      *******************************************************
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
           "@(#)WI/TFCCDS.CBL S35 02/23/24-11:38:12 >".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ERRCD                   PIC X VALUE " ".
       01  ROUTE-BUF               PIC XX  VALUE SPACES.
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       01  FIRST-TIME      PIC X            VALUE "Y".
       01  HOLD-IO-FG      PIC 99.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  WK-WINDOW-WORKERS.
           03  WK-WINDOW-LPO       PIC X(4).
           03  WK-WINDOW-REP       PIC X(8).
           03  WK-WINDOW-PROCESS   PIC X(1).

       01 TFCCDS-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TFCCDS_DEF.CPY".
       COPY "LIBWI/TFCCDS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  LPO-SEQ                 VALUE 0.
       COPY "LIBGB/GETFMW.CPY".
       01  WINDOW-WORKERS.
           03  WINDOW-LPO          PIC X(4).
           03  WINDOW-REP          PIC X(8).
           03  WINDOW-PROCESS      PIC X(1).

       SCREEN SECTION.
       COPY "LIBWI/TFCCDS_SCN.CPY".


       PROCEDURE DIVISION
            USING FORM-PATHNAME WINDOW-WORKERS EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TFCCDS" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 14 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 09 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
            COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 40 + VDU-WIN-W-MOD
                      LINES 09 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS TFCCDS-WINDOW-HANDLE.

           MOVE "Y" TO FIRST-TIME.
       WINDOW-RESET.

           PERFORM FORM-RESET.

           MOVE WINDOW-WORKERS TO WK-WINDOW-WORKERS.
           IF WINDOW-LPO NOT = SPACES
              MOVE "S  A040010." TO VDU
              MOVE WINDOW-LPO TO VDUBUF
              PERFORM SCREENIO.

           IF WINDOW-REP NOT = SPACES
              MOVE "S  A080020." TO VDU
              MOVE WINDOW-REP TO VDUBUF
              PERFORM SCREENIO.

           IF WINDOW-PROCESS NOT = SPACES
              MOVE "S  A010030." TO VDU
              MOVE WINDOW-PROCESS TO VDUBUF
              PERFORM SCREENIO.

      * THIS COULD BE THE DEFAULT DEALER OR A NEW ONE THEY ENTERED LAST TIME
           IF WK-WINDOW-LPO NOT = SPACES
              GO TO WINDOW-ENTER-REP.

       WINDOW-ENTER-LPO.
           IF WINDOW-LPO NOT = SPACES
              MOVE LPO-SEQ TO PASSWDW-SEQ
              MOVE "TO OVERRIDE DEFAULT LPO" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO WINDOW-ENTER-REP.

           MOVE " F6-SCAN, ENTER LPO CD:" TO MESS
           PERFORM SEND-LEGEND.

           MOVE "E  A040010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO WINDOW-RESET.
           IF VDUSTATUS = "1<"
              MOVE "LO" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO WINDOW-ENTER-LPO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO WINDOW-ENTER-LPO.

           MOVE "LO" TO CD-TYPE.
           MOVE VDUBUF TO WK-WINDOW-LPO CD-CODE.
           PERFORM GET-CD.
           IF IO-FG = 9
              MOVE " INVALID CODE, F6 SCAN CODES" TO MESS
              PERFORM SEND-LEGEND
              GO TO WINDOW-ENTER-LPO.

       WINDOW-ENTER-REP.
      D    STOP MESS.
           IF IO-FG = 0
              MOVE " F6-SCAN, ENTER REP CODE:" TO MESS
              PERFORM SEND-LEGEND.
           MOVE "E  A080020." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO WINDOW-RESET.
           IF VDUSTATUS = "1U"
              GO TO WINDOW-ENTER-LPO.
           IF VDUSTATUS = "1<"
              MOVE "RC" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO WINDOW-ENTER-REP.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO WINDOW-ENTER-REP.

           MOVE "RC"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE  WK-WINDOW-REP.
           PERFORM GET-CD.
           IF IO-FG = 9
              MOVE " INVALID CODE, F6 SCAN CODES" TO MESS
              PERFORM SEND-LEGEND
              GO TO WINDOW-ENTER-REP.

       WINDOW-ENTER-PROCESS.
           IF IO-FG = 0
              MOVE " F6-SCAN,ENTER PROCESS CODE:" TO MESS
              PERFORM SEND-LEGEND.
           MOVE "E  A010030." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO WINDOW-RESET.
           IF VDUSTATUS = "1U"
              GO TO WINDOW-ENTER-REP.
           IF VDUSTATUS = "1<"
              MOVE "PC" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO WINDOW-ENTER-PROCESS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO WINDOW-ENTER-PROCESS.

           MOVE "PC" TO CD-TYPE.
           MOVE VDUBUF TO WK-WINDOW-PROCESS CD-CODE.
           PERFORM GET-CD.
           IF IO-FG = 9
              MOVE " INVALID CODE, F6 SCAN CODES" TO MESS
              PERFORM SEND-LEGEND
              GO TO WINDOW-ENTER-PROCESS.

       END-WINDOW.

           MOVE WK-WINDOW-WORKERS TO WINDOW-WORKERS.
           GO TO END-ROUTINE.

      *****************************************************************
       CODE-SCAN SECTION.
      *****************************************************************

           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS NOT = "00"    ) OR
              (( CDSCAN-TYPE = "LO") AND ( CDSCAN-CODE = SPACES )) OR
              (( CDSCAN-TYPE = "PC") AND ( CDSCAN-CODE = SPACES )) OR
              (( CDSCAN-TYPE = "RC") AND ( CDSCAN-CODE = SPACES ))
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           IF CDSCAN-TYPE = "LO"
              MOVE "S  A040010." TO VDU
           ELSE
           IF CDSCAN-TYPE = "RC"
              MOVE "S  A080020." TO VDU
           ELSE
           IF CDSCAN-TYPE = "PC"
              MOVE "S  A010030." TO VDU.

           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.

           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON TFCCDS-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY TFCCDS-SCREEN UPON TFCCDS-WINDOW-HANDLE.
           MOVE TFCCDS-FORM-FIELDS TO WR-BUF.
           MOVE TFCCDS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE TFCCDS-HELP--FILE TO HELP-LINK-FILE.
           MOVE TFCCDS-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TFCCDS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "TFCCDS"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           MOVE IO-FG TO HOLD-IO-FG.
           PERFORM CLOSE-CD1-FILE.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TFCCDS-SCREEN.
           DESTROY TFCCDS-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
