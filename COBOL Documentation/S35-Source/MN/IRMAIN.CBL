       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IRMAIN.
      ******************************************************************
      *
      *   DESC:  INSURANCE RATE TABLE FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * JTG 051596 CHANGED SP-INS-RATE-FILE VALUES TO 71 THRU 99
      * JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *            ADDED NEW FIELD '1210' TO HOLD A TABLE DESCRIPTION
      *            ADDED NEW FIELD '1220' TO HOLD PER 100 OR PER 1000
      * MJD 131120 A30 CONVERSION COMPLETED
      * BAH 151006 MADE FILE INDEXED, PD0005
      *
      * KEC 2017.0213 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'IR' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 20190724 INCREASED IR-KEY TO 9(3), PT0147
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/IRMAIN.CBL S34 06/04/21-10:20:03 >".
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  HOLD-IR-KEY                 PIC 9(3)      VALUE ZEROES.

      *=================================================================
      *   IRCH-REC FROM 'REPLACING LEADING' ON IR-REC. (CHANGED-REC)
      *   IROR-REC FROM 'REPLACING LEADING' ON IR-REC. (ORIGINAL-REC)
      *=================================================================
       COPY "LIBLP/LP01IR.CPY" REPLACING LEADING "IR" BY "IROR".
       COPY "LIBLP/LP01IR.CPY" REPLACING LEADING "IR" BY "IRCH".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-IR-KEY       PIC 9(3).
           03  FILLER                   PIC X(11).
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(16).
               05  WORK-ERRLOG-ORG-N0 REDEFINES WORK-ERRLOG-ORG-ALP 
                                        PIC ZZZZ9-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(16).
               05  WORK-ERRLOG-CHG-N0 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC ZZZZ9-.

      ******************************************************************
       01 IRMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       01  ERRCD           PIC X       VALUE SPACES.
       01  ONE               PIC S9.
       01  S                 PIC 999.
       01  ELE               PIC 999    VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(3).
           03  CHNO          PIC 9(3).
           03  CHNO-X REDEFINES CHNO
                             PIC X(3).
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9      VALUE 0.

       01  VDUFIELD-R.
           03  VDUROW-R      PIC 999.
           03  VDUCOL-R      PIC 9.
           03  VDUDECIMAL-R  PIC X.
           03  FILLER        PIC X(8).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF.
           03  DISP-BUF           OCCURS 120 TIMES.
               05  FLD             PIC Z9.999 BLANK ZERO.
           03  D-1210              PIC X(40).
      * IR-PER-FG
           03  D-1220              PIC X(4).

       01  DISPLAY-LIST.
           03  FILLER        PIC X(50)  VALUE
           "0010 0020 0030 0040 0050 0060 0070 0080 0090 0100 ".
           03  FILLER        PIC X(50)  VALUE
           "0110 0120 0130 0140 0150 0160 0170 0180 0190 0200 ".
           03  FILLER        PIC X(50)  VALUE
           "0210 0220 0230 0240 0250 0260 0270 0280 0290 0300 ".
           03  FILLER        PIC X(50)  VALUE
           "0310 0320 0330 0340 0350 0360 0370 0380 0390 0400 ".
           03  FILLER        PIC X(50)  VALUE
           "0410 0420 0430 0440 0450 0460 0470 0480 0490 0500 ".
           03  FILLER        PIC X(50)  VALUE
           "0510 0520 0530 0540 0550 0560 0570 0580 0590 0600 ".
           03  FILLER        PIC X(50)  VALUE
           "0610 0620 0630 0640 0650 0660 0670 0680 0690 0700 ".
           03  FILLER        PIC X(50)  VALUE
           "0710 0720 0730 0740 0750 0760 0770 0780 0790 0800 ".
           03  FILLER        PIC X(50)  VALUE
           "0810 0820 0830 0840 0850 0860 0870 0880 0890 0900 ".
           03  FILLER        PIC X(50)  VALUE
           "0910 0920 0930 0940 0950 0960 0970 0980 0990 1000 ".
           03  FILLER        PIC X(50)  VALUE
           "1010 1020 1030 1040 1050 1060 1070 1080 1090 1100 ".
           03  FILLER        PIC X(50)  VALUE
           "1110 1120 1130 1140 1150 1160 1170 1180 1190 1200 ".
           03  FILLER        PIC X(10)  VALUE
           "1210 1220.".

      ***********************************
      *   ELEMENT SPECIFICATIONS
      ***********************************
       01  MAX               PIC 999 VALUE 122.
       01  SPEC-TABLE.
      * FIELDS 1-120
           03  FILLER        PIC X(8) VALUE "ENNN023".
      * 121 IR-DESC
           03  FILLER        PIC X(8) VALUE "E  A400".
      * 122 IR-PER-FG
           03  FILLER        PIC X(8) VALUE "ENNN040".
       01  SPEC-TAB         REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 3 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.
       COPY "LIBWI/IRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/IRMAIN_DEF.CPY".
       COPY "LIBMN/IRMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/IRMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "IRMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS IRMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.
           PERFORM FORM-RESET.

           PERFORM REC-IR-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************
       REC-IR-MODULE SECTION.
      ******************************************
       REC-IR-MODULE-NO.

           MOVE ZEROES TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "EN N030RATECD." TO VDU.
           PERFORM SCREENIO.

           IF (        F5-KEY ) PERFORM ENTER-SCANKEY
                                GO TO END-REC-IR-MODULE.

           IF (        F6-KEY ) PERFORM SCAN-IR-FILE.

           IF (        F7-KEY ) MOVE "END" TO IO-TYPE
                                GO TO END-REC-IR-MODULE.

           IF ( NOT ENTER-KEY ) GO TO REC-IR-MODULE-NO.

           IF VDUNUM0 < 71
              MOVE "INVALID RATE CODE, ENTER 71-99" TO MESS
              PERFORM SEND-MESS
              GO TO REC-IR-MODULE-NO.

           SUBTRACT 70 FROM VDUNUM0 GIVING IR-KEY HOLD-IR-KEY.
       READ-IRFILE.
           PERFORM OPEN-IR-FILE.
           PERFORM READ-IR-FILE.
           PERFORM CLOSE-IR-FILE.
           IF IO-FG = 0
              MOVE IR-REC TO IROR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-IR-MODULE.
           EXIT.

      ********************************************
      *   ENTRY-MODULE
      *   NEW-FLAG ).0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ********************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
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
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ELE < 121
              MOVE 1 TO CHNO
           ELSE
           IF ELE = 121
              MOVE 2 TO CHNO
           ELSE
           IF ELE = 122
              MOVE 3 TO CHNO.

           IF CHKEY NOT = ZEROS
              IF CHFG(CHNO) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(CHNO) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.
           IF ELE < 121
              MOVE SPEC(1) TO VDU
           ELSE
           IF ELE = 121
              MOVE SPEC(2) TO VDU
           ELSE
           IF ELE = 122
              MOVE SPEC(3) TO VDU.
           MOVE ELE        TO VDUROW-R.
           MOVE 0          TO VDUCOL-R.
           MOVE "."        TO VDUDECIMAL-R.
           MOVE VDUFIELD-R TO VDUFIELD.
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
           IF ELE < 121
              GO TO A-TABLE.
           IF ELE = 121
              GO TO A-121.
           IF ELE = 122
              GO TO A-122.
           GO TO END-ENTRY-MODULE.


       A-TABLE.
           SET IRIX TO ELE.
           MOVE VDUNUM3 TO IR-INSRATE(IRIX).
           GO TO CREATE-ROUTINE.
       A-121.
           MOVE VDUBUF TO IR-DESC.
           GO TO CREATE-ROUTINE.
       A-122.
           IF NOT (VDUNUM0 = 100 OR 1000)
              MOVE "ENTER 100 OR 1000 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF VDUNUM0 = 100
              MOVE "H" TO IR-PER-FG.
           IF VDUNUM0 = 1000
              MOVE "T" TO IR-PER-FG.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *************************************
       WRITE-MODULE SECTION.
      *************************************
           PERFORM OPEN-IR-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE IR-REC TO IRCH-REC.

           IF IROR-REC = IRCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-IR-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF IR-REC NOT = IROR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE IRCH-REC TO IR-REC.
           PERFORM REWRITE-IR-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-IR-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-IR-FILE.

      **************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           COMPUTE WORK-ERRLOG-IR-KEY = IR-KEY + 70.
           MOVE IR-KEY    TO WORK-ERRLOG-IR-KEY.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           PERFORM VARYING S FROM 1 BY 1 UNTIL S > 120
              IF IROR-INSRATE(S) NOT = IRCH-INSRATE(S)
                 MOVE S              TO WORK-ERRLOG-FIELD
                 MOVE S              TO WORK-ERRLOG-DESC
                 MOVE IROR-INSRATE(S)  TO WORK-ERRLOG-ORG-N0
                 MOVE IRCH-INSRATE(S)  TO WORK-ERRLOG-CHG-N0
                 PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

           IF IROR-DESC NOT = IRCH-DESC
              MOVE ELE        TO WORK-ERRLOG-FIELD
              MOVE "DESC"     TO WORK-ERRLOG-DESC
              MOVE IROR-DESC  TO WORK-ERRLOG-ORG-ALP
              MOVE IRCH-DESC  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF IROR-PER-FG NOT = IRCH-PER-FG
              MOVE ELE              TO WORK-ERRLOG-FIELD
              MOVE "PER"            TO WORK-ERRLOG-DESC
              MOVE IROR-PER-FG  TO WORK-ERRLOG-ORG-ALP
              MOVE IRCH-PER-FG  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

      **********************************************
       LOG-FIELD-CHANGE SECTION.
      **********************************************
           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *********************************
       DELETE-MODULE SECTION.
      *********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-IR-FILE.
           PERFORM READ-IR-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF IR-REC NOT = IROR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-IR-FILE.

      * CREATE ERROR LOG OF DELETED RECORD
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING IR-KEY, " ", "DELETED" DELIMITED BY SIZE
                         INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-IR-FILE.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE "F5-SEARCH, ENTER RATE CODE" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-IR-FILE.
           MOVE 0 TO IR-KEY.

           PERFORM START-IR-FILE.
       ENTER-KEY.
           MOVE "EN N030SCAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           IF ( F5-KEY ) GO TO READ-FILES-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              MOVE IR-REC TO IROR-REC
              GO TO CLOSE-SCAN.

           MOVE VDUNUM0 TO IR-KEY.
           IF IR-KEY > 70
              SUBTRACT 70 FROM IR-KEY.

           PERFORM START-IR-FILE.
       READ-FILES-NEXT.
           PERFORM READ-IR-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N030RATECD." TO VDU.
           COMPUTE VDUNUM0 = IR-KEY + 70.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.
       CLOSE-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM CLOSE-IR-FILE.
       END-SCAN.
           EXIT.

      ******************************************************************
       SCAN-IR-FILE SECTION.
      ******************************************************************

           MOVE ZEROES      TO IRSCAN-IR-KEY.
           MOVE "WI/IRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH IRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF (IRSCAN-STATUS NOT = "00") OR (IRSCAN-IR-KEY =  0)
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-IR-FILE-EXIT.

           MOVE "SNNN030RATECD." TO VDU.
           COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.
           PERFORM SCREENIO.

           COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.
           MOVE "00" TO VDUSTATUS.

       SCAN-IR-FILE-EXIT.
           EXIT.

      *******************************************
       NEW-RECORD-SETUP SECTION.
      *******************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           MOVE SPACES TO IR-REC.

           PERFORM VARYING IRIX FROM 1 BY 1 UNTIL IRIX > 120
                MOVE 0 TO IR-INSRATE(IRIX)
           END-PERFORM.

           MOVE HOLD-IR-KEY TO IR-KEY.

           MOVE 0 TO ELE.

      *******************************************
       DISPLAY-FIELDS SECTION.
      *******************************************
           MOVE ZEROS TO DISPLAY-BUF.
           PERFORM VARYING IRIX FROM 1 BY 1 UNTIL IRIX > 120
              MOVE IR-INSRATE(IRIX) TO FLD(IRIX)
           END-PERFORM.
           MOVE IR-DESC TO D-1210.
           IF IR-PER-FG = "T"
              MOVE "1000" TO D-1220
           ELSE
              MOVE "100 " TO D-1220.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************************
       FORM-RESET SECTION.
      ********************************************************************
           MOVE EXT-CONAME-CONAME TO IRMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IRMAIN-SYSDATE.
           DISPLAY SPACES UPON IRMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY IRMAIN-SCREEN UPON IRMAIN-WINDOW-HANDLE.
           MOVE IRMAIN-FORM-FIELDS TO WR-BUF
           MOVE IRMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************************
           MOVE IRMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRMAIN-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/IRMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ********************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
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
           MOVE " RECORD HAS ALREADY BEEN CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-IR-FILE.
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBLP/LPIRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY IRMAIN-SCREEN.
           DESTROY IRMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
