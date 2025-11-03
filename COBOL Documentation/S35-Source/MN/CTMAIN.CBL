       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CTMAIN.
      **************************************************************************
      *                 (MN)
      *  DESCRIPTION:   COUNTY TAX PERCENT FILE MAINTENANCE
      *
      * REV:
      * JTG 050797 ADDED CD-ELUNITPER-CD
      * KEC 022599 ADDED 'EXITPROG' LOGIC.
      * BAH 990720 ALLOW ELFRMLA-VALID TO INCLUDE "J" AND "K", JUDY
      * JTG 020206 **NEW
      *            ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      * JTG 061208 ALLOWED CHARGING OF INSURANCE SALES TAX FOR "FL" FLORIDA
      *                                                               NEWS #2875
      * KEC 2016.0321 CHANGED HOLD/SAVE KEYS & RECS TO MATCH CD-REC.
      * KEC 2016.0328 CHANGED CTSCAN WORKERS TO MATCH CD1-KEY FOR CT LAYOUT.
      * BAH 190129 SPLIT CDFILE
      **************************************************************************
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
           "@(#)MN/CTMAIN.CBL S35 02/23/24-10:18:09 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

       01  ERRCD           PIC X     VALUE SPACES.
       01  CHAN            PIC X(4).
       01  SUB             PIC 999   VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.

       01  HOLD-CD1-KEY                PIC X(10)     VALUE SPACES.

       01  ORIGINAL-CD-REC    PIC X(CD-SIZE).
       01  CHANGED-CD-REC     PIC X(CD-SIZE).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE " ".
           03  F01           PIC X(18).
           03  F02           PIC X.
           03  F021          PIC Z9.99 BLANK ZERO.
           03  F03           PIC X.
           03  F031          PIC Z9.99 BLANK ZERO.
           03  F04           PIC X.
           03  F041          PIC Z9.99 BLANK ZERO.
           03  F05           PIC X.
           03  F051          PIC Z9.99 BLANK ZERO.
           03  F06           PIC X.
           03  F061          PIC Z9.99 BLANK ZERO.
           03  F07           PIC X.
           03  F071          PIC Z9.99 BLANK ZERO.
           03  F08           PIC X.
           03  F081          PIC Z9.99 BLANK ZERO.
           03  F09           PIC X.
           03  F091          PIC Z9.99 BLANK ZERO.
           03  F10           PIC X.
           03  F101          PIC Z9.99 BLANK ZERO.
           03  F11           PIC X.
           03  F111          PIC Z9.99 BLANK ZERO.
           03  F12           PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(44) VALUE
           "010 020 021 030 031 040 041 050 051 060 061 ".
           03  FILLER      PIC X(44) VALUE
           "070 071 080 081 090 091 100 101 110 111 120.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 11.
       01  SPEC-TABLE.
      * 1. CD-CT-COUNTY
           03  FILLER      PIC X(8)   VALUE "E  A180 ".
      * 2. CD-CT-INSTAXFRMLA(1)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 3. CD-CT-INSTAXFRMLA(2)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 4. CD-CT-INSTAXFRMLA(3)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 5. CD-CT-INSTAXFRMLA(4)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 6. CD-CT-INSTAXFRMLA(5)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 7. CD-CT-INSTAXFRMLA(6)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 8. CD-CT-INSTAXFRMLA(7)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      * 9. CD-CT-INSTAXFRMLA(8)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      *10. CD-CT-INSTAXFRMLA(9)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".
      *11. CD-CT-INSTAXFRMLA(10)
           03  FILLER      PIC X(8)   VALUE "E  A010 ".

       01  SPEC-TABLE2    REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 11.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/CTSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  CTMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CTMAIN_DEF.CPY".
       COPY "LIBMN/CTMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CTMAIN_SCN.CPY".


       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
      *    MAIN PROGRAM MODULE
      ******************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "CTMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CTMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
              MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           PERFORM REC-CD-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.
           GO TO MAIN-MODULE.

      **************************************
       REC-CD-MODULE SECTION.
      **************************************
           MOVE 0 TO NEW-FLAG.
           MOVE " " TO IO-TYPE.

       REC-CD-MODULE-STATE.
           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-CD-MODULE-STATE.
           IF VDUBUF = " "
              GO TO REC-CD-MODULE-STATE.

           MOVE VDUBUF TO CD-CT-STATE
                          ST-CODE.
           PERFORM GET-ST.
           IF IO-BAD
              MOVE " STATE CODE IS NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CD-MODULE-STATE.

           PERFORM SEND-STATE-DESC.

       REC-CD-MODULE-COUNTY.
           MOVE "E  A030COUNTY." TO VDU.
           PERFORM SCREENIO.
           IF F6-KEY
              PERFORM CTSCAN-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-CD-MODULE-COUNTY.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-CD-MODULE-COUNTY.
           IF VDUBUF = " "
              GO TO REC-CD-MODULE-COUNTY.

           MOVE VDUBUF  TO CD-CT-COUNTY.

           MOVE "CT"    TO CD-TYPE.
           MOVE " "     TO CD-CT-FILLER.
           MOVE CD1-KEY TO HOLD-CD1-KEY.

           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 0
              MOVE CD-REC TO ORIGINAL-CD-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-CD-MODULE.
           EXIT.

      *************************************************
       CTSCAN-SCAN SECTION.
      *************************************************
           MOVE SPACES      TO CTSCAN-CD1-KEY.
           MOVE "CT"        TO CTSCAN-KEY-TYPE.
           MOVE CD-CT-STATE TO CTSCAN-KEY-STATE.
           MOVE ST-DESC     TO CTSCAN-ST-DESC.

           MOVE "WI/CTSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CTSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF (CTSCAN-STATUS NOT = "00") OR (CTSCAN-KEY-COUNTY = SPACES)
              MOVE 9 TO IO-FG
              GO TO CTSCAN-SCAN-EXIT.

           MOVE "S  A030COUNTY."  TO VDU.
           MOVE CTSCAN-KEY-COUNTY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE "00" TO VDUSTATUS.
           MOVE CTSCAN-KEY-COUNTY TO VDUBUF.

       CTSCAN-SCAN-EXIT.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
             MOVE "DEL" TO IO-TYPE
             GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL " "
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0   TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01
                 F-FRMLA F-FRMLA F-FRMLA F-FRMLA F-FRMLA
                 F-FRMLA F-FRMLA F-FRMLA F-FRMLA F-FRMLA
                 F-TAX-BOTH DEPENDING ON ELE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO CD-DESC.
           GO TO CREATE-ROUTINE.
       F-FRMLA.
           MOVE VDUBUF TO CD-CT-INSTAXFRMLA(ELE - 1).
           IF NOT (CD-CT-INSTAXFRMLA(ELE - 1) = " " OR "F")
              MOVE "ENTER SPACE-PERCENT OR F-FIXED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-PERCENT.
           MOVE "ENNN022" TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 1         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-PERCENT.
           MOVE VDUNUM2 TO CD-CT-INSTAXAMT(ELE - 1).
           GO TO CREATE-ROUTINE.
       F-TAX-BOTH.
           MOVE VDUBUF  TO CD-CT-BOTH-TAX-FG.
           IF NOT(CD-CT-BOTH-TAX-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ********************************************
       WRITE-MODULE SECTION.
      ********************************************
           PERFORM OPEN-CD1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CD-REC TO CHANGED-CD-REC.
           IF ORIGINAL-CD-REC = CHANGED-CD-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF CD-REC NOT = ORIGINAL-CD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-CD-REC TO CD-REC.
           PERFORM REWRITE-CD1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-CD1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-CD1-FILE.

      ***************************************
       DELETE-MODULE SECTION.
      ***************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF CD-REC NOT = ORIGINAL-CD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CD1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-CD1-FILE.

      ***************************************
       NEW-RECORD-SETUP SECTION.
      ***************************************
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-CD-FILE.

           MOVE HOLD-CD1-KEY TO CD1-KEY.

           MOVE 0 TO ELE.

      ***************************************
       DISPLAY-FIELDS SECTION.
      ***************************************
           MOVE " " TO DISPLAY-BUF.
           MOVE CD-DESC               TO F01.
           MOVE CD-CT-INSTAXFRMLA(1)  TO F02.
           MOVE CD-CT-INSTAXAMT(1)    TO F021.
           MOVE CD-CT-INSTAXFRMLA(2)  TO F03.
           MOVE CD-CT-INSTAXAMT(2)    TO F031.
           MOVE CD-CT-INSTAXFRMLA(3)  TO F04.
           MOVE CD-CT-INSTAXAMT(3)    TO F041.
           MOVE CD-CT-INSTAXFRMLA(4)  TO F05.
           MOVE CD-CT-INSTAXAMT(4)    TO F051.
           MOVE CD-CT-INSTAXFRMLA(5)  TO F06.
           MOVE CD-CT-INSTAXAMT(5)    TO F061.
           MOVE CD-CT-INSTAXFRMLA(6)  TO F07.
           MOVE CD-CT-INSTAXAMT(6)    TO F071.
           MOVE CD-CT-INSTAXFRMLA(7)  TO F08.
           MOVE CD-CT-INSTAXAMT(7)    TO F081.
           MOVE CD-CT-INSTAXFRMLA(8)  TO F09.
           MOVE CD-CT-INSTAXAMT(8)    TO F091.
           MOVE CD-CT-INSTAXFRMLA(9)  TO F10.
           MOVE CD-CT-INSTAXAMT(9)    TO F101.
           MOVE CD-CT-INSTAXFRMLA(10) TO F11.
           MOVE CD-CT-INSTAXAMT(10)   TO F111.
           MOVE CD-CT-BOTH-TAX-FG     TO F12.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCDCL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO CTMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO CTMAIN-SYSDATE.
           DISPLAY CTMAIN-SCREEN UPON CTMAIN-WINDOW-HANDLE.
           MOVE CTMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CTMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE CTMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CTMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/CTMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-ST.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
       SEND-STATE-DESC.
           MOVE "S  A160STATEDESC." TO VDU.
           MOVE ST-DESC             TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CTMAIN-SCREEN.
           DESTROY CTMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
