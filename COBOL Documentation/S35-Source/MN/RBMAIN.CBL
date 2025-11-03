       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RBMAIN.
      *****************************************************
      *   DIR :  MN
      *   DESC:  REBATE /CANCELLATION TABLE FILE MAINTENANCE
      *   REV:
      *        180531 BAH ADDED ERRLOG
      *****************************************************
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
           "@(#)MN/RBMAIN.CBL S34 09/20/21-14:17:31 >".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01RB.CPY".
       COPY "LIBLP/LP01RB_SQL.CPY".
       COPY "LIBLP/LPWSRB.CPY".

       01  ONE               PIC S9.
       01  SUB               PIC 99      VALUE 0.
       01  ELE               PIC 999    VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(3).
           03  CHNO          PIC 9(3).
           03  CHNO-X REDEFINES CHNO
                             PIC X(3).
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  MONDAY-LIST       PIC X(30)  VALUE
           "MODA1 MODA2 MODA3 MODA4 MODA5.".
       01  MONDAY-BUF.
           03  MODA          OCCURS 5 TIMES
                             PIC X(3).

       01  HOLD-KEY.
           03  HOLD-RB-TBL-NO PIC 99.
           03  HOLD-RB-TERM   PIC 9(3).

      *=================================================================
      *   RBCH-REC FROM 'REPLACING LEADING' ON RB-REC. (CHANGED-REC)
      *   RBOR-REC FROM 'REPLACING LEADING' ON RB-REC. (ORIGINAL-REC)
      *=================================================================
       COPY "LIBLP/LP01RB.CPY" REPLACING LEADING "RB" BY "RBOR".
       COPY "LIBLP/LP01RB.CPY" REPLACING LEADING "RB" BY "RBCH".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-RB-KEY       PIC 9(5).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(18).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(16).
               05  WORK-ERRLOG-ORG-N0 REDEFINES WORK-ERRLOG-ORG-ALP
                                        PIC ZZZZ9-.
               05  WORK-ERRLOG-ORG-N1 REDEFINES WORK-ERRLOG-ORG-ALP
                                        PIC Z9.9.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(16).
               05  WORK-ERRLOG-CHG-N0 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC ZZZZ9-.
               05  WORK-ERRLOG-CHG-N1 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z9.9.

       01  VDUFIELD-R.
           03  VDUROW-R      PIC 999.
           03  VDUCOL-R      PIC 9.
           03  VDUDECIMAL-R  PIC X.
           03  FILLER        PIC X(8).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF.
           03  MONDAY              PIC X.
           03  DISP-BUF            OCCURS 75 TIMES.
               05  FLD-AMT         PIC ZZZZ.
               05  FLD-RATE        PIC Z9.9  BLANK ZERO.

       01  DISPLAY-LIST.
           03 FILLER         PIC X(7)   VALUE
           "MONDAY ".
           03  FILLER        PIC X(50)  VALUE
           "0010 0011 0020 0021 0030 0031 0040 0041 0050 0051 ".
           03  FILLER        PIC X(50)  VALUE
           "0060 0061 0070 0071 0080 0081 0090 0091 0100 0101".
           03  FILLER        PIC X(50)  VALUE
           "0110 0111 0120 0121 0130 0131 0140 0141 0150 0151 ".
           03  FILLER        PIC X(50)  VALUE
           "0160 0161 0170 0171 0180 0181 0190 0191 0200 0201".
           03  FILLER        PIC X(50)  VALUE
           "0210 0211 0220 0221 0230 0231 0240 0241 0250 0251 ".
           03  FILLER        PIC X(50)  VALUE
           "0260 0261 0270 0271 0280 0281 0290 0291 0300 0301 ".
           03  FILLER        PIC X(50)  VALUE
           "0310 0311 0320 0321 0330 0331 0340 0341 0350 0351 ".
           03  FILLER        PIC X(50)  VALUE
           "0360 0361 0370 0371 0380 0381 0390 0391 0400 0401".
           03  FILLER        PIC X(50)  VALUE
           "0410 0411 0420 0421 0430 0431 0440 0441 0450 0451 ".
           03  FILLER        PIC X(50)  VALUE
           "0460 0461 0470 0471 0480 0481 0490 0491 0500 0501".
           03  FILLER        PIC X(50)  VALUE
           "0510 0511 0520 0521 0530 0531 0540 0541 0550 0551 ".
           03  FILLER        PIC X(50)  VALUE
           "0560 0561 0570 0571 0580 0581 0590 0591 0600 0601".
           03  FILLER        PIC X(50)  VALUE
           "0610 0611 0620 0621 0630 0631 0640 0641 0650 0651 ".
           03  FILLER        PIC X(50)  VALUE
           "0660 0661 0670 0671 0680 0681 0690 0691 0700 0701".
           03  FILLER        PIC X(50)  VALUE
           "0710 0711 0720 0721 0730 0731 0740 0741 0750 0751.".

      ***********************************
      *   ELEMENT SPECIFICATIONS
      ***********************************
       01  MAX               PIC 999 VALUE 75.
       01  SPEC-TABLE.
           03  SPEC      PIC X(7) VALUE "ENNN040".
           03  CHFG      PIC X    VALUE " ".
       01  LEGEND-01.
           03  FILLER         PIC X(40) VALUE
           "F1, F5-CHG MONTH/DAY, 'DELETE', RTN-ACCE".
           03  FILLER         PIC X(20) VALUE
           "PT, ENTER FIELD NO:".
       01  MSEL-LIST         PIC X(5)   VALUE "MSEL.".
       01  ERRCD               PIC X VALUE " ".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  LEGEND            REDEFINES MESS
                             PIC X(60).
       01 RBMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/RBMAIN_DEF.CPY".
       COPY "LIBMN/RBMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/RBMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************************
      *         MAIN PROGRAM MODULE
      ***********************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RBMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS RBMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-RB-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      *****************************
       REC-RB-MODULE SECTION.
      *****************************
       REC-RB-TABLE.
           MOVE 0 TO RB-TBL-NO RB-TERM.
           MOVE 0 TO NEW-FLAG HOLD-RB-TBL-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE "F7-EXIT, F6-SCAN, ENTER REFUND TABLE NO. (80 TO 99):"
                                                    TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N020TABLE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-RB-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RB-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-RB-TABLE.
           IF VDUNUM0 = 0
              GO TO REC-RB-TABLE.
           IF VDUNUM0 < 80
              GO TO REC-RB-TABLE.

           MOVE VDUNUM0 TO RB-TBL-NO HOLD-RB-TBL-NO.
       REC-RB-TERM.
           MOVE 0 TO HOLD-RB-TERM.
           MOVE "F7-EXIT, ^, F6-SCAN, ENTER A TERM BREAK:" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N030TERM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-RB-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RB-MODULE.
           IF VDUSTATUS = "1U"
              GO TO REC-RB-TABLE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-RB-TERM.
           IF VDUNUM0 = 0
              GO TO REC-RB-TERM.

           MOVE VDUNUM0 TO RB-TERM HOLD-RB-TERM.
       READ-RBFILE.
           PERFORM OPEN-RB1-FILE.
           PERFORM READ-RB1-FILE.
           PERFORM CLOSE-RB1-FILE.
           IF IO-FG = 0
              MOVE RB-REC TO RBOR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP
              PERFORM ENTER-MONDAY
              IF IO-FG = 9
                 MOVE "CLR" TO IO-TYPE.
       END-REC-RB-MODULE.
           EXIT.

      ***************************
       ENTER-MONDAY SECTION.
      ***************************
           MOVE 0 TO IO-FG.
           IF NEW-FLAG = 1
              MOVE "F1-CLEAR, ENTER BREAK TYPE; M-MONTHS OR D-DAYS:"
                                                        TO LEGEND
           ELSE
              MOVE "ENTER BREAK TYPE; M-MONTHS OR D-DAYS:" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010MONDAY." TO VDU.
           PERFORM SCREENIO.
           IF NEW-FLAG = 1
              IF VDUSTATUS = "10"
                 MOVE 9 TO IO-FG
                 GO TO END-ENTER-MONDAY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONDAY.
           IF NOT (VDUBUF = "M" OR "D")
              GO TO ENTER-MONDAY.

           MOVE VDUBUF TO RB-MON-DAY-CD.
           PERFORM SEND-MONDAY.

       END-ENTER-MONDAY.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE "F1-RESET SCAN, F6-SCAN" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-RB1-FILE.
       ENTER-KEY.
           MOVE "E RA050SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-RBFILE-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE RB1-KEY TO HOLD-KEY
                 MOVE RB-REC TO RBOR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
       READ-RBFILE-NEXT.
           IF FIRST-SCAN = 0
              PERFORM START-RB1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-RB1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N020TABLE." TO VDU.
           MOVE RB-TBL-NO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N030TERM." TO VDU.
           MOVE RB-TERM TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      ********************************************
      *   ENTRY-MODULE
      *   NEW-FLAG   0 - MODIFY EXISTING RECORD
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
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1:"
              PERFORM ENTER-MONDAY
              GO TO MODIFY-ROUTINE.
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
           IF NEW-FLAG = 1
              MOVE "F1-CLEAR, ^, ENTER THE BREAK AMOUNT:"
                                                       TO LEGEND
           ELSE
              MOVE "F1-CLEAR, ENTER THE BREAK AMOUNT:" TO LEGEND.
           PERFORM SEND-LEGEND.
           IF CHKEY NOT = ZEROS
              IF CHFG = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.
           MOVE SPEC TO VDU.
           MOVE ELE TO VDUROW-R.
           MOVE 0 TO VDUCOL-R.
           MOVE "." TO VDUDECIMAL-R.
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
           GO TO F-00.

      ************************************************
       F-00.
           IF VDUNUM0 = 0
              IF ELE = 1
                 GO TO ENTER-ELE
              ELSE
                 MOVE "S  A040" TO VDU
                 MOVE "." TO VDUDECIMAL-R
                 PERFORM CLEAR-REMAINING-TABLE
                         VARYING ELE FROM ELE BY 1
                         UNTIL ELE > 75
                 GO TO CREATE-ROUTINE.
           MOVE VDUNUM0 TO RB-AMT(ELE).
           IF ELE NOT = 1
              IF NOT (RB-AMT(ELE) > RB-AMT(ELE - 1))
                 GO TO ENTER-ELE.
       F-001.
           MOVE "F1-CLEAR, ^, ENTER REFUND (XX.X%) PERCENTAGE:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPEC TO VDU.
           MOVE 2 TO VDUNUM.
           MOVE 1 TO VDUDEC.
           MOVE ELE TO VDUROW-R.
           MOVE 1 TO VDUCOL-R.
           MOVE "." TO VDUDECIMAL-R.
           MOVE VDUFIELD-R TO VDUFIELD.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-001.
           MOVE VDUNUM1 TO RB-RATE(ELE).
           IF ELE NOT = 1
              IF NOT (RB-RATE(ELE) < RB-RATE(ELE - 1))
                 GO TO F-001.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ********************************************
       CLEAR-REMAINING-TABLE SECTION.
      ********************************************
           MOVE 0 TO RB-AMT(ELE) RB-RATE(ELE).
           MOVE ELE TO VDUROW-R.
           MOVE 0 TO VDUCOL-R.
           MOVE VDUFIELD-R TO VDUFIELD.
           PERFORM SCREENIO.

           MOVE 1 TO VDUCOL-R.
           MOVE VDUFIELD-R TO VDUFIELD.
           PERFORM SCREENIO.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
           PERFORM OPEN-RB1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE RB-REC TO RBCH-REC.

           IF RBOR-REC = RBCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-RB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF RB-REC NOT = RBOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE RBCH-REC TO RB-REC.

           PERFORM REWRITE-RB1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-RB1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-RB1-FILE.

      *********************************
       DELETE-MODULE SECTION.
      *********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-RB1-FILE.
           PERFORM READ-RB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF RB-REC NOT = RBOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-RB1-FILE.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING RB1-KEY, " ", "DELETED" DELIMITED BY SIZE INTO
                                           ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
       END-DELETE-MODULE.
           PERFORM CLOSE-RB1-FILE.

      **************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE RB1-KEY   TO WORK-ERRLOG-RB-KEY.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 75
              IF RBOR-AMT(SUB) NOT = RBCH-AMT(SUB)
                 MOVE SUB                 TO WORK-ERRLOG-FIELD
                 MOVE "DAY "              TO WORK-ERRLOG-DESC
                 MOVE RBOR-AMT(SUB)       TO WORK-ERRLOG-ORG-N0
                 MOVE RBCH-AMT(SUB)       TO WORK-ERRLOG-CHG-N0
                 PERFORM LOG-FIELD-CHANGE
              END-IF
              IF RBOR-RATE(SUB) NOT = RBCH-RATE(SUB)
                 MOVE SUB                 TO WORK-ERRLOG-FIELD
                 MOVE "% "                TO WORK-ERRLOG-DESC
                 MOVE RBOR-RATE(SUB)       TO WORK-ERRLOG-ORG-N1
                 MOVE RBCH-RATE(SUB)       TO WORK-ERRLOG-CHG-N1
                 PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

      **********************************************
       LOG-FIELD-CHANGE SECTION.
      **********************************************
           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ******************************
       NEW-RECORD-SETUP SECTION.
      ******************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO RB-REC.
           PERFORM VARYING RBIX FROM 1 BY 1 UNTIL RBIX > 75
              MOVE 0 TO RB-AMT(RBIX)
                        RB-RATE(RBIX)
           END-PERFORM.

           MOVE HOLD-KEY TO RB1-KEY.
         
           MOVE 0 TO ELE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE RB-MON-DAY-CD TO MONDAY.
           PERFORM SEND-MONDAY.
           PERFORM DISP-FLDS VARYING RBIX FROM 1 BY 1
              UNTIL RBIX > 75.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ***************************
       SEND-MONDAY SECTION.
      ***************************
           IF RB-MON-DAY-CD = "M"
              MOVE "MON" TO MODA(1)
                            MODA(2)
                            MODA(3)
                            MODA(4)
                            MODA(5)
           ELSE
              MOVE "DAY" TO MODA(1)
                            MODA(2)
                            MODA(3)
                            MODA(4)
                            MODA(5).
           MOVE MONDAY-BUF TO WR-BUF.
           MOVE MONDAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       DISP-FLDS SECTION.
      *********************************
           MOVE RB-AMT(RBIX) TO FLD-AMT(RBIX).
           MOVE RB-RATE(RBIX) TO FLD-RATE(RBIX).

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           MOVE EXT-CONAME-CONAME TO RBMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RBMAIN-SYSDATE.
           DISPLAY SPACES UPON RBMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY RBMAIN-SCREEN UPON RBMAIN-WINDOW-HANDLE.
           MOVE RBMAIN-FORM-FIELDS TO WR-BUF.
           MOVE RBMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE RBMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE RBMAIN-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/RBMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                   TO MESS.
           PERFORM SEND-MESS.


      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RB1-FILE.
       COPY "LIBLP/LPRBGS_SQL.CPY".
       COPY "LIBLP/LPRB1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY RBMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
