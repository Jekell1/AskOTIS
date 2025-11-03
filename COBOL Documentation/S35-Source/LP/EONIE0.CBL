       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EONIE0.
       DATE-WRITTEN.    112990.
      *****************************************************
      *                 (LP)
      *
      *  DESCRIPTION:   BORROWER CONVERSION FILE ENTRY
      *                 PAGE 2
      *
      * REV:
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 120497 EXPANDED BW-LOANS TO PIC 99
      *  MJD 021998 ADDED BW-SIZE (78 LEVEL FOR REC SIZE)
      *  GLF 990909 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *   CS 991018 ADDED TEST OF BULK-CONVERSION-FG TO DETERMINE IF
      *      SCREEN SHOULD DISPLAY BULK PURCHASE OR CONVERSION
      *  BAH 170531 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181002 GLOBAL BWFILE
      *  BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      *****************************************************
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
           "@(#)./LP/EONIE0.CBL S34 09/24/21-13:10:06 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  TYP             PIC 9      COMP  VALUE 0.
       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  JUSTIFY-CONAME  PIC X(40).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9      COMP.

       01  ORIGINAL-BW-REC PIC X(BW-SIZE).
       01  CHANGED-BW-REC  PIC X(BW-SIZE).

       01  FIRST-SCAN      PIC 9      COMP  VALUE 0.

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.
       01  TEST-SSNO       PIC 9(9).

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-NAME      PIC X(34).
           03  F01         PIC Z9.
           03  F02         PIC Z9.
           03  F03         PIC Z,ZZZ,ZZZ.99-.
           03  F04         PIC Z,ZZZ,ZZZ.99-.
           03  F05         PIC ZZ,ZZZ.99-.
           03  F06         PIC ZZ,ZZZ.99-.
           03  F07         PIC ZZZ,ZZZ.99.
           03  F08         PIC ZZZ,ZZZ.99.
           03  F09         PIC 99/99/99.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(41) VALUE
           "NAME 010 020 030 040 050 060 070 080 090.".

       01  MAX             PIC 99   VALUE 9.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      *01. NUMBER OF PRIOR LOANS
           03  FILLER      PIC X(8) VALUE "EN N020N".
      *02. NUMBER OF CURRENT LOANS
           03  FILLER      PIC X(8) VALUE "EN N020N".
      *03. Y-T-D INTEREST
           03  FILLER      PIC X(8) VALUE "E  S072N".
      *04. L-Y-R INTEREST
           03  FILLER      PIC X(8) VALUE "E  S072N".
      *05. REAL ESTATE INTEREST YTD
           03  FILLER      PIC X(8) VALUE "E  S052N".
      *06. REAL ESTATE INTEREST LYR
           03  FILLER      PIC X(8) VALUE "E  S052N".
      *07. SMALL LOAN LIABILITY
           03  FILLER      PIC X(8) VALUE "E  N062N".
      *08. OTHER LOAN LIABILITY
           03  FILLER      PIC X(8) VALUE "E  N062N".
      *09. LAST PAYOFF DATE
           03  FILLER      PIC X(8) VALUE "ER M080N".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 9 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBLP/EONIE_DEF.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBLP/EONIE_WKS.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LPCMNU_SCN.CPY".
       COPY "LIBLP/EONIE_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EONIE" TO VDU-FORM-NAME.

       MAIN-MODULE.
           MOVE SPACES TO ERRCD.
           PERFORM FORM-RESET.
           PERFORM DISPLAY-BULK-CONVERSION.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              MOVE SPACES TO SAVEKEY
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE BW-SSNO TO SAVEKEY
                 MOVE MENU-PROG-FORM(1) TO NEW-PROG-FORM
                 MOVE 0 TO NEW-PROG-SUB
                 MOVE "1" TO ERRCD
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      **************************
       REC-BW-MODULE SECTION.
      **************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE SPACES TO SAVEKEY
              MOVE "S  A110BWNO." TO VDU
              MOVE ALP-BW TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.

           MOVE "F7-MENU, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.

       REC-BW-SSNO.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DESTROY ALL CONTROLS
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO REC-BW-MODULE-NO
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-SSNO.
           MOVE VDUNUM0 TO BW-SSNO.
           IF BW-SSNO = 0
              GO TO REC-BW-SSNO.
       READ-BWFILE.
           MOVE NUM-BW TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-BW-SSNO.
           MOVE BW-REC TO ORIGINAL-BW-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
       END-REC-BW-MODULE.
           EXIT.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-SCANKEY SECTION.
       ENTER-TYPE.
           MOVE "1-SS#,2-NAME,3-SP SS#,4-SP NAME,F6-SCAN:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              MOVE "SN N010TYPE." TO VDU
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM OPEN-BW1-FILE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.

           PERFORM OPEN-BW1-FILE.
           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 MOVE SPACES TO BW-NAME
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    MOVE 0 TO BW-SPSSNO
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       MOVE SPACES TO BW-SPNAME
                       PERFORM START-BW4-FILE.

       ENTER-KEY.
           MOVE "ENTER KEY, ENTER=ACCEPT, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           IF TYP = 2 OR TYP = 4
              MOVE "E RA240SORTKEY." TO VDU
           ELSE
              MOVE "E RA090SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE BW-REC TO ORIGINAL-BW-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO TEST-SSNO
              IF TEST-SSNO NOT NUMERIC
              GO TO ENTER-KEY.
           IF TYP = 1 OR 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO
           ELSE
              IF TYP = 2 OR 4
                 PERFORM ARRANGE-BW-KEY
                 MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.

           IF TYP = 1
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
       READ-FILES-NEXT.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
      *                       UNLOCK BW-FILE.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-BW1-FILE.
       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE "S  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO VDU-I VDU-J.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF VDU-I NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(VDU-I) NOT = ","
              MOVE VDUTAB(VDU-I) TO BW-ALP-TBL(VDU-J)
            ELSE
              MOVE 35 TO VDU-J.
           ADD 1 TO VDU-I.
           ADD 1 TO VDU-J.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO IO-TYPE.
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

           MOVE "F1-CLEAR, <RET>-ACCEPT, FIELD #, P1 :" TO MESS.
           PERFORM SEND-LEGEND.
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
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR > 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.
           MOVE SPEC (ELE) TO VDU.
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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO BW-PRLOANS.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO BW-LOANS.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO BW-INTYTD.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO BW-INTLYR.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO BW-RE-INTYTD.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO BW-RE-INTLYR.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO BW-LIASMLN.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO BW-LIAOTH.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDU-DATE-YYYYMMDD TO BW-LASTPODATE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
           PERFORM OPEN-BW1-FILE.
           MOVE BW-REC TO CHANGED-BW-REC.
           IF ORIGINAL-BW-REC = CHANGED-BW-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BW-REC NOT = ORIGINAL-BW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-BW-REC TO BW-REC.
           PERFORM REWRITE-BW1-FILE.
       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************

           MOVE SPACES TO DISPLAY-BUF.
          
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO D-NAME.

           MOVE BW-PRLOANS   TO F01.
           MOVE BW-LOANS     TO F02.
           MOVE BW-INTYTD    TO F03.
           MOVE BW-INTLYR    TO F04.
           MOVE BW-RE-INTYTD TO F05.
           MOVE BW-RE-INTLYR TO F06.
           MOVE BW-LIASMLN   TO F07.
           MOVE BW-LIAOTH    TO F08.
           MOVE BW-LASTPODATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F09.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBLP/LPMNUI.CPY".
           IF BW-SSNO NUMERIC
              IF MENU-FG NOT = 99
                 MOVE BW-SSNO TO BORROWER-KEY.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
      * CONAME IS CENTERED IN SETENV AND DOESN'T HAVE ENOUGH ROOM IN THIS
      * FORM
           MOVE EXT-CONAME-CONAME TO JUSTIFY-CONAME.
           CALL "C$JUSTIFY" USING JUSTIFY-CONAME  "L".
           MOVE JUSTIFY-CONAME TO EONIE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EONIE-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EONIE-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE EONIE-FORM-FIELDS TO WR-BUF.
           MOVE EONIE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE EONIE-HELP--FILE TO HELP-LINK-FILE.
           MOVE EONIE-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EONIE_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-PARAGRAPHS SECTION.
      *****************************
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
       DISPLAY-BULK-CONVERSION.
      *--{CS}---
           MOVE "S  A240TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BORROWER (BULK PURCHASE)" TO VDUBUF
           ELSE
              MOVE "BORROWER (CONVERSION)" TO VDUBUF.
           PERFORM SCREENIO.

       NUMBW-TO-ALPBW.
           MOVE NUM-BW123 TO ALP-BW123.
           MOVE NUM-BW45 TO ALP-BW45.
           MOVE NUM-BW6789 TO ALP-BW6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBW-TO-NUMBW.
           MOVE ALP-BW123 TO NUM-BW123.
           MOVE ALP-BW45 TO NUM-BW45.
           MOVE ALP-BW6789 TO NUM-BW6789.
           IF NUM-BW NOT NUMERIC
              MOVE 0 TO NUM-BW.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *****************************
       END-ROUTINE SECTION.
      *****************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DESTROY ALL CONTROLS.
       STOP-RUN.
           EXIT PROGRAM.
