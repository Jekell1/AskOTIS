       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWINQ2.
       DATE-WRITTEN.
      *****************************************************
      *  DESCRIPTION:   BORROWER MASTER FILE INQUIRY
      *            PAGE 2
      *  REV:
      *  MJD 120497 ENLARGED BW-LOANS TO PIC 99
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 170510 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180926 GLOBAL FILES
      *  BLM 210921 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/BWINQ2.CBL S34 09/21/21-14:54:02 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  IX1             PIC 99      VALUE 0.
       01  IX2             PIC 99      VALUE 0.

       01  TYP             PIC 9       VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION          PIC X(6).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.

       01  BW-SSNO-EDIT    PIC X(11).
       01  WORK-KEY.
           03  WK-1        PIC 999.
           03  DASH-1      PIC X.
           03  WK-2        PIC 99.
           03  DASH-2      PIC X.
           03  WK-3        PIC 9(4).
       01  SOCSEC-KEY      PIC 9(9).
       01  SS-KEY REDEFINES SOCSEC-KEY.
           03  SS-1        PIC 999.
           03  SS-2        PIC 99.
           03  SS-3        PIC 9(4).

       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.

       01  BWINQ-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  BWINQ-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
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
           03  F09         PIC 99/99/99 BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(41) VALUE
           "NAME 010 020 030 040 050 060 070 080 090.".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWINQ2_DEF.CPY".
       COPY "LIBLP/BWINQ2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBLP/BWINQ2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************

           PERFORM SQL-CONNECT.

           MOVE "BWINQ2" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BWINQ-LINK.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE BW-SSNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ***********************************
       REC-BW-MODULE SECTION.
      ***********************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO SS-KEY
              MOVE SPACES TO SAVEKEY
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDU-SSNO-EDIT
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.
       REC-BW-SSNO.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-TYPEKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-SSNO.
           MOVE VDUNUM0 TO BW-SSNO.
           IF BW-SSNO = ZERO
              GO TO REC-BW-SSNO.
       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-GOOD
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-BW-MODULE.
           GO TO REC-BW-MODULE-NO.
       END-REC-BW-MODULE.
           EXIT.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-TYPEKEY SECTION.
       ENTER-TYPE.
           MOVE " 1=SS#, 2=NAME, 3=SPOUSE SS#, 4=SPOUSE NAME, F6=SCAN"
              TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1<"
              MOVE "SN N010TYPE." TO VDU
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-SCAN-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
             GO TO ENTER-TYPE.
       DISPLAY-SCAN-TYPE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           IF TYP = 1
              MOVE " ENTER SS#, OR F6 TO SCAN" TO MESS.
           IF TYP = 2
              MOVE " ENTER NAME (LAST), OR F6 TO SCAN" TO MESS.
           IF TYP = 3
              MOVE " ENTER SPOUSE SS#, OR F6 TO SCAN" TO MESS.
           IF TYP = 4
              MOVE " ENTER SPOUSE NAME (LAST), OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.
       ENTER-KEY.
           IF TYP = 2 OR 4
              MOVE "E RA240SORTKEY." TO VDU
           ELSE
              MOVE "E RA110SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SORTKEY
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 GO TO CLOSE-SORTKEY
              ELSE
                 GO TO ENTER-KEY.

           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO SOCSEC-KEY
              IF SOCSEC-KEY NOT NUMERIC
              GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO.
           IF TYP = 2 OR TYP = 4
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
           IF TYP = 1
              PERFORM READ-BW1-FILE-NEXT
      *       UNLOCK BW-FILE
           ELSE
           IF TYP = 2
              PERFORM READ-BW2-FILE-NEXT
           ELSE
           IF TYP = 3
              PERFORM READ-BW3-FILE-NEXT
           ELSE
           IF TYP = 4
              PERFORM READ-BW4-FILE-NEXT.
           IF IO-BAD OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SORTKEY.

           MOVE "S  A110BWNO." TO VDU.
           MOVE BW-SSNO TO SOCSEC-KEY.
           MOVE SS-1 TO WK-1.
           MOVE SS-2 TO WK-2.
           MOVE SS-3 TO WK-3.
           MOVE "-" TO DASH-1 DASH-2.
           MOVE WORK-KEY TO BW-SSNO-EDIT.
           MOVE BW-SSNO-EDIT TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SORTKEY.
           PERFORM CLOSE-BW1-FILE.
       END-SORTKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "S  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
            ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
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
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 2
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1 AND NOT = 3
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO EXIT-ENTRY-MODULE.
           GO TO MODIFY-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ****************************************
       DISPLAY-FIELDS SECTION.
      ****************************************

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
           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO BWINQ2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWINQ2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BWINQ2-SCREEN UPON BWINQ-WINDOW-HANDLE.
           MOVE BWINQ2-FORM-FIELDS TO WR-BUF.
           MOVE BWINQ2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE BWINQ2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWINQ2-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWINQ2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************
       COPY "LIBGB/ACCESS.CPY".

      *********************************************
       IO-ROUTINE SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWINQ2-SCREEN.
           MOVE BWINQ-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
