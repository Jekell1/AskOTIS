       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LTCREA.
      **********************************************************
      *   DIR :  WI
      *   DESC: LOAN TRAILER RECORD CREATE/CHANGE
      *
      * REV:
      *  JTG 032593 REVISED TO PASS BR-REC AND GB-REC
      *  JTG 030495 ADDED SET "00" TO LINK-STATUS
      *  060195 BAH CHANGED WK-COLL-AMT1 TO WK-MILEAGE
      *                     WK-COLL-AMT2 TO WK-VALUE
      *                     WK-COLL-AMT3 TO WK-SELLPR
      *                     WK-COLL-AMT4 TO WK-MISC-AMT
      *  MJD 022796 ADDED TEST OF BA-FILE. IF A BORROWER HAS A BA-REC
      *             AND THE BA-REC HAS COLLATERAL INFORMATION,
      *             THEN THIS BA-REC COLLATERAL INFORMATION IS DISPLAYED
      *             INSTEAD OF THE WK-REC INFO. IF INFO IS SAVED, THE
      *             INFO IN THE BA-REC IS SAVED IN THE WK-REC.
      *             ANY CHANGES SAVED IN WK-REC ARE UPDATED IN THE
      *             BA-REC IF IT EXISTS.
      *             IF BA-REC EXISTS AND THERE IS COLLATERAL
      *              THEN DISPLAY " APPLICATION " AT TOP OF WINDOW.
      *  KEC 040797 REVISED TO PROPERLY ACCESS THE APPLICATION MODULE.
      *  BAH 000131 VERIFY THE VIN YOU ENTERED DOES NOT ALREADY EXIST
      *             IN THE IXFILE, JUST DISPLAYS A MESSAGE, TENFIN PR#1064
      *  BAH 100128 ADDED ANOTHER LINE OF HHGS, SEQ 5, UNITED ONLY PR# 3524
      *  BAH 130923 ALLOW GP-I-AM-WDT TO USE SEQ 5 ALSO, WDT PR# 4095
      *  BAH 150929 PASSWORD HHGS2, FIELD 26, REGMGM PR# 4856
      *  BAH 151013 PASSWORD HHGS2 EVEN ON NEW ENTRY, REGMGM PR#4856
      *  BAH 160114 CHANGED WK-LT-RECORD TO MATCH NEW LTFILE PD0003
      *
      * KEC 2016.0517 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      *
      * KEC 2016.0819 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF THE BA-FILE [LIBLA/LAFDMX].
      *         REVISED LA-REC LAYOUT.
      *
      * KEC 180911 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      *  CS 181218 GLOBAL LTCFILE
      *  CS 181220 GLOBAL IXFILE
      * BAH 20190523 INCREASED WK-SAVE-RECORD-X FROM 1606 TO 4800, MATCH
      *              WK-SAVE-RECORD! WAS CAUSING 20202 IN SAVED COLLATERAL
      *              AUTO FIELDS. QA0029
      * BAH 2024.0418 CALL LTCREA USING LTCREA-BUF, NOT USING SCAN-BUF WINDOW
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/LTCREA.CBL S34 05/25/21-10:37:13 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       01  WK-IFN            PIC X(2)   VALUE "WK".
       01  WK-KEY            PIC 999.

       01  LTCREA-LINES              PIC 99  VALUE 22.
       01  LTCREA-COLUMNS            PIC 99  VALUE 80.
       01  LTCREA-BEGIN-Y            PIC 99  VALUE 1.
       01  LTCREA-BEGIN-X            PIC 99  VALUE 1.
       01  ERRCD             PIC X VALUE " ".
       01  WK-SAVE-RECORD-X  PIC X(4800).

       01  WS-ACCTNO         PIC 9(8).
       01  WS-ACCTNOX REDEFINES WS-ACCTNO.
           03  WS-NUMBER     PIC 9(6).
           03  FILLER        PIC 99.
       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  OUT-SUB           PIC 99     COMP-3 VALUE 0.
       01  OUT-VIN           PIC X(6) VALUE SPACES.
       01  OUT-VINX REDEFINES OUT-VIN.
           03  OUT-VIN-BYTE  PIC X OCCURS 6.
       01  ELE               PIC 99     VALUE 0.
       01  CHAN              PIC X(4).

       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC X(10).
           03  F02           PIC X(16).
           03  F03           PIC X(16).
           03  F04           PIC X(4).
           03  F05           PIC X(8).
           03  F06           PIC X(8).
           03  F07           PIC X(20).

           03  F08           PIC X(8).
      * MILEAGE
           03  F09           PIC Z(7)-     BLANK ZERO.
      * VALUE
           03  F10           PIC Z(5).99   BLANK ZERO.
      * SELL PRICE
           03  F11           PIC Z(5).99   BLANK ZERO.
      * MISC AMT
           03  F12           PIC Z(5).99-  BLANK ZERO.

           03  F13           PIC X(10).
           03  F14           PIC X(16).
           03  F15           PIC X(16).
           03  F16           PIC X(4).
           03  F17           PIC X(8).
           03  F18           PIC X(8).
           03  F19           PIC X(20).

           03  F20           PIC X(8).
      * MILEAGE
           03  F21           PIC Z(7)-     BLANK ZERO.
      * VALUE
           03  F22           PIC Z(5).99   BLANK ZERO.
      * SELL PRICE
           03  F23           PIC Z(5).99   BLANK ZERO.
      * MISC AMT
           03  F24           PIC Z(5).99-  BLANK ZERO.

           03  F25           PIC X(78).

           03  F26           PIC X(78).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(24) VALUE
           "210 220 230 240 250 260.".

       01  DISPLAY-BUF2 VALUE SPACES.
           03  F261          PIC X(78).
       01  DISPLAY-LIST2.
           03  FILLER        PIC X(4) VALUE "261.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX             PIC 99   VALUE 26.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
      * MILEAGE
           03  FILLER      PIC X(8) VALUE "ENNS070 ".
      * VALUE
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * SELL PRICE
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * MISC AMT
           03  FILLER      PIC X(8) VALUE "ENNS052 ".

           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
      * MILEAGE
           03  FILLER      PIC X(8) VALUE "ENNS070 ".
      * VALUE
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * SELL PRICE
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * MISC AMT
           03  FILLER      PIC X(8) VALUE "ENNS052 ".

           03  FILLER      PIC X(8) VALUE "E  A780 ".
           03  FILLER      PIC X(8) VALUE "E  A780 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 26 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      * KEEP SAME PASSWORDS AS LTSCAN
       01  MODIFY-SEQ              PIC 99 VALUE 01.
       01  TITLE-SEQ               PIC 99 VALUE 02.
       01  HHGS2-SEQ               PIC 99 VALUE 03.

       01  LTCREA-WINDOW-HANDLE        PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LTCREA_DEF.CPY".
       COPY "LIBWI/LTCREA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LTCREAW.CPY".
      
       SCREEN SECTION.
       COPY "LIBWI/LTCREA_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME LTCREA-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-IX2-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LTCREA" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           DISPLAY FLOATING GRAPHICAL WINDOW
              AT  SCREEN LINE LTCREA-BEGIN-X * VDU-WIN-L-OFFSET
              AT  SCREEN COL  LTCREA-BEGIN-Y * VDU-WIN-C-OFFSET
              CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
              SIZE    LTCREA-COLUMNS + VDU-WIN-W-MOD
              LINES   LTCREA-LINES + VDU-WIN-H-MOD
              BLANK SCREEN
              HANDLE IS LTCREA-WINDOW-HANDLE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE "N" TO BR-COLL-ID-SCAN.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE SPACE TO IO-TYPE.

           PERFORM RESTORE-TRAILER.

           IF WK-HHGS-1 = " " AND
               WK-HHGS-2 = " " AND
                WK-DESC-1 = " " AND
                 WK-DESC-2 = " "
                 MOVE 1 TO NEW-FLAG
                 MOVE 0 TO ELE
           ELSE
              PERFORM DISPLAY-FIELDS.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              PERFORM RESTORE-TRAILER
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              PERFORM SAVE-TRAILER
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
           MOVE "F1-RESET, F7-EXIT WITH UPDATE, ENTER FIELD#:"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
              GO TO MODIFY-ROUTINE.

           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ELE = 26
              MOVE HHGS2-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE HHGS 2:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

           IF ELE = 1 AND NEW-FLAG = 1
              MOVE " F7-EXIT, ENTER DESCRIPTION:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 1 OR 13
              MOVE " F1-RESET, ENTER DESCRIPTION:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 2 OR 14
              MOVE " F1-RESET, ENTER MAKE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 3 OR 15
              MOVE " F1-RESET, ENTER MODEL:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 4 OR 16
              MOVE " F1-RESET, ENTER YEAR:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 5 OR 17
              MOVE " F1-RESET, ENTER STYLE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 6 OR 18
              MOVE " F1-RESET, ENTER CLASS:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 7 OR 19
              MOVE " F1-RESET, ENTER ID #:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 8 OR 20
              MOVE " F1-RESET, ENTER TITLE #:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 9 OR 21
              MOVE " F1-RESET, ENTER COLLATERAL VALUE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 10 OR 22
              MOVE " F1-RESET, ENTER SELLING PRICE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 11 OR 23
              MOVE " F1-RESET, ENTER DEALER COST:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 12 OR 24
              MOVE " F1-RESET, ENTER DEALER PROFIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 25
              MOVE " F1-RESET, ENTER GOODS AND APPLIANCES 1:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 26
              MOVE " F1-RESET, ENTER GOODS AND APPLIANCES 2:" TO MESS
              PERFORM SEND-LEGEND.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
      *    IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
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
       TEST-F7.
           IF VDUSTATUS NOT = "1>"
              GO TO TEST-F1.
           IF NEW-FLAG = 1
              IF ELE = 1
                 MOVE "END" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08
                 A-09 A-10 A-11 A-12 A-13 A-14 A-15 A-16
                 A-17 A-18 A-19 A-20 A-21 A-22 A-23 A-24
                 A-25 A-26
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       A-01.
           IF NEW-FLAG = 1
                     AND VDUBUF = SPACE
              MOVE SPACES TO WK-AUTO-ALP-1
              MOVE 0 TO WK-MILEAGE-1
                        WK-VALUE-1
                        WK-SELLPR-1
                        WK-MISC-AMT-1
                        WK-VIN-CHG-DATE-1
              MOVE 12 TO ELE
              PERFORM PROTECT-AUTO-1
              GO TO CREATE-ROUTINE.

           MOVE VDUBUF TO WK-DESC-1.
           IF VDUBUF = SPACE
              MOVE SPACES TO WK-AUTO-ALP-1
              MOVE 0 TO WK-MILEAGE-1
                        WK-VALUE-1
                        WK-SELLPR-1
                        WK-MISC-AMT-1
                        WK-VIN-CHG-DATE-1
              PERFORM PROTECT-AUTO-1
              PERFORM SET-AUTO1-FIELDS
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO CREATE-ROUTINE.

           PERFORM UNPROTECT-AUTO-1.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO WK-MAKE-1.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO WK-MODEL-1.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO WK-YEAR-1.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO WK-STYLE-1.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO WK-CLASS-1.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO WK-VIN-1.
           IF VDUBUF NOT = SPACES
              IF BR-COLL-ID-SCAN = "Y" OR "S"
                 MOVE WK-VIN-1 TO IX-VIN
                 PERFORM VERIFY-VIN
                 IF IO-FG = 0
                    MOVE IX-ACCTNO TO WS-ACCTNO
                    STRING "VIN ALREADY EXIST, ACCOUNT: ",
                       WS-NUMBER DELIMITED BY "  " INTO MESS
                    PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO WK-TITLE-NO-1.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO WK-MILEAGE-1.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO WK-VALUE-1.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM2 TO WK-SELLPR-1.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO WK-MISC-AMT-1.
           GO TO CREATE-ROUTINE.
       A-13.
           IF NEW-FLAG = 1
                     AND VDUBUF = SPACE
              MOVE SPACES TO WK-AUTO-ALP-2
              MOVE 0 TO WK-MILEAGE-2
                        WK-VALUE-2
                        WK-SELLPR-2
                        WK-MISC-AMT-2
                        WK-VIN-CHG-DATE-2
              MOVE 24 TO ELE
              PERFORM PROTECT-AUTO-2
              GO TO CREATE-ROUTINE.

           MOVE VDUBUF TO WK-DESC-2.
           IF VDUBUF = SPACE
              MOVE SPACES TO WK-AUTO-ALP-2
              MOVE 0 TO WK-MILEAGE-2
                        WK-VALUE-2
                        WK-SELLPR-2
                        WK-MISC-AMT-2
                        WK-VIN-CHG-DATE-2
              PERFORM PROTECT-AUTO-2
              PERFORM SET-AUTO2-FIELDS
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO CREATE-ROUTINE.
           PERFORM UNPROTECT-AUTO-2.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO WK-MAKE-2.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUBUF TO WK-MODEL-2.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUBUF TO WK-YEAR-2.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUBUF TO WK-STYLE-2.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUBUF TO WK-CLASS-2.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO WK-VIN-2.
           IF VDUBUF NOT = SPACES
              IF BR-COLL-ID-SCAN = "Y" OR "S"
                 MOVE WK-VIN-2 TO IX-VIN
                 PERFORM VERIFY-VIN
                 IF IO-FG = 0
                    MOVE IX-ACCTNO TO WS-ACCTNO
                    STRING "VIN ALREADY EXIST, ACCOUNT: ",
                       WS-NUMBER DELIMITED BY "  " INTO MESS
                    PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO WK-TITLE-NO-2.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM0 TO WK-MILEAGE-2.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO WK-VALUE-2.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO WK-SELLPR-2.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM2 TO WK-MISC-AMT-2.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUBUF TO WK-HHGS-1.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUBUF TO WK-HHGS-2.
      *    IF NOT (GP-I-AM-UNITED OR GP-I-AM-WDT)
           GO TO CREATE-ROUTINE.

      * THIRD LINE OF HHGS
       A-261.
           MOVE "E  A780261." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-261.
           MOVE VDUBUF TO WK-HHGS-3.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      ***************************
       VERIFY-VIN SECTION.
      ***************************
           IF BR-COLL-ID-SCAN NOT = "S"
              GO TO VERIFY-START-VIN.
           MOVE SPACES TO OUT-VIN.
           MOVE 20 TO SUB.
           MOVE 6 TO OUT-SUB.
       VERIFY-NEXT-VIN.
           IF VDUTAB(SUB) NOT = SPACES
              MOVE VDUTAB(SUB) TO OUT-VIN-BYTE(OUT-SUB)
              SUBTRACT 1 FROM OUT-SUB.
           IF OUT-SUB > 0
              IF SUB > 1
                 SUBTRACT 1 FROM SUB
                 GO TO VERIFY-NEXT-VIN.

           MOVE OUT-VIN TO IX-VIN.
       VERIFY-START-VIN.
           PERFORM OPEN-IX2-FILE.

           PERFORM START-IX2-FILE.
           PERFORM READ-IX2-FILE-NEXT.
           IF IO-FG = 0
              IF BR-COLL-ID-SCAN  = "S"
                 IF IX-VIN NOT = OUT-VIN OR    
                     (IX-BRNO NOT = IX-PATH-OWNBR)
                    MOVE 9 TO IO-FG
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF IX-VIN NOT = VDUBUF OR    
                     (IX-BRNO NOT = IX-PATH-OWNBR)
                    MOVE 9 TO IO-FG.

           PERFORM CLOSE-IX2-FILE.
       EXIT-VERIFY-VIN.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************
           MOVE " " TO DISPLAY-BUF.

           PERFORM SET-AUTO1-FIELDS.

           PERFORM SET-AUTO2-FIELDS.

           MOVE WK-HHGS-1 TO F25.
           MOVE WK-HHGS-2 TO F26.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * THIRD LINE OF HHGS
      *    IF GP-I-AM-UNITED OR GP-I-AM-WDT
      *       MOVE WK-HHGS-3 TO F261
      *       MOVE DISPLAY-BUF2 TO WR-BUF
      *       MOVE DISPLAY-LIST2 TO WR-LIST
      *       PERFORM CALL-WRFORM.

      *****************************
       SET-AUTO1-FIELDS SECTION.
      *****************************
           MOVE WK-DESC-1 TO F01.
           MOVE WK-MAKE-1 TO F02.
           MOVE WK-MODEL-1 TO F03.
           MOVE WK-YEAR-1 TO F04.
           MOVE WK-STYLE-1 TO F05.
           MOVE WK-CLASS-1 TO F06.
           MOVE WK-VIN-1 TO F07.
           MOVE WK-TITLE-NO-1 TO F08.
           MOVE WK-MILEAGE-1 TO F09.
           MOVE WK-VALUE-1 TO F10.
           MOVE WK-SELLPR-1 TO F11.
           MOVE WK-MISC-AMT-1 TO F12.

      *****************************
       SET-AUTO2-FIELDS SECTION.
      *****************************
           MOVE WK-DESC-2 TO F13.
           MOVE WK-MAKE-2 TO F14.
           MOVE WK-MODEL-2 TO F15.
           MOVE WK-YEAR-2 TO F16.
           MOVE WK-STYLE-2 TO F17.
           MOVE WK-CLASS-2 TO F18.
           MOVE WK-VIN-2 TO F19.
           MOVE WK-TITLE-NO-2 TO F20.
           MOVE WK-MILEAGE-2 TO F21.
           MOVE WK-VALUE-2 TO F22.
           MOVE WK-SELLPR-2 TO F23.
           MOVE WK-MISC-AMT-2 TO F24.

      *****************************
       RESTORE-TRAILER SECTION.
      *****************************
           MOVE LTCREA-PATH TO TEMP-PATH.
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.

      *****************************
       SAVE-TRAILER SECTION.
      *****************************
           MOVE WK-SAVE-RECORD TO WK-SAVE-RECORD-X.
           MOVE LTCREA-PATH TO TEMP-PATH.
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           MOVE WK-SAVE-RECORD-X TO WK-SAVE-RECORD.
           PERFORM REWRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           DISPLAY LTCREA-SCREEN UPON LTCREA-WINDOW-HANDLE.
           MOVE LTCREA-FORM-FIELDS TO WR-BUF.
           MOVE LTCREA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE LTCREA-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTCREA-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LTCREA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-PARAGRAPHS SECTION.
      *****************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       PROTECT-AUTO-1.
           MOVE "N" TO STAT.
           PERFORM DO-AUTO-1.
       UNPROTECT-AUTO-1.
           MOVE " " TO STAT.
           PERFORM DO-AUTO-1.
       DO-AUTO-1.
           MOVE STAT TO CHFG(2) CHFG(3) CHFG(4) CHFG(5) CHFG(6)
                        CHFG(7) CHFG(8) CHFG(9) CHFG(10) CHFG(11)
                        CHFG(12).
       PROTECT-AUTO-2.
           MOVE "N" TO STAT.
           PERFORM DO-AUTO-2.
       UNPROTECT-AUTO-2.
           MOVE " " TO STAT.
           PERFORM DO-AUTO-2.
       DO-AUTO-2.
           MOVE STAT TO CHFG(14) CHFG(15) CHFG(16) CHFG(17) CHFG(18)
                        CHFG(19) CHFG(20) CHFG(21) CHFG(21) CHFG(22)
                        CHFG(23) CHFG(24).

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LTCREA" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***************************
       IO-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIX2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LTCREA-SCREEN.
           DESTROY LTCREA-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
