       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWINQ1.
      *****************************************************
      *                 (LP)
      *  DESCRIPTION:   BORROWER MASTER FILE INQUIRY
      *
      * REV:
      * GLF 990908 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * BAH 170510 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180926 GLOBAL FILES
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BWINQ1.CBL S35 02/21/24-11:31:39 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION          PIC X(6).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

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

       01  BWINQ-LINK      VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  BWINQ-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01         PIC X(16).
           03  F02         PIC X(16).
           03  F03         PIC X(24).
           03  F04         PIC X(24).
           03  F05         PIC X(15).
           03  F06         PIC XX.
           03  F07         PIC X(10).
           03  F08         PIC X(12).
           03  F09         PIC 99/99/99 BLANK ZERO.
           03  F10         PIC 99/99/9999 BLANK ZERO.
           03  F11         PIC X(4).
           03  F12         PIC ZZ.
           03  F13         PIC X(4).
           03  F14         PIC X(4).
           03  F15         PIC XX.
           03  F16         PIC X(4).
           03  F17         PIC X(6).
           03  F18         PIC X(10).

           03  F19         PIC 9(4).
           03  F20         PIC XXX.
           03  F21         PIC ZZZZ.
           03  F22         PIC X(4).
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X(24).
           03  F26         PIC X(24).
           03  F27         PIC X(12).
           03  F271        PIC ZZZZ.
           03  F28         PIC ZZ.
           03  F281        PIC X(9).
           03  F29         PIC ZZZ,ZZZ.ZZ.
           03  F30         PIC ZZZ,ZZZ.ZZ.
           03  F31         PIC X(8).

           03  F32         PIC X(16).
           03  F33         PIC X(16).
           03  F34         PIC 999/99/9999.
           03  F35         PIC 99/99/99.
           03  F35-AGE     REDEFINES F35
                           PIC X(8).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(44) VALUE
           "210 220 230 240 250 260 270 271 280 281 290 ".
           03  FILLER      PIC X(24) VALUE
           "300 310 320 330 340 350.".

       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWINQ1_DEF.CPY".
       COPY "LIBLP/BWINQ1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBLP/BWINQ1_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************

           PERFORM SQL-CONNECT.

           MOVE "BWINQ1" TO VDU-FORM-NAME.
       COPY "LIBGB/CHKSEC.CPY".

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

      *****************************************
       REC-BW-MODULE SECTION.
      *****************************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO
              MOVE SPACES TO SAVEKEY
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDU-SSNO-EDIT
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM BORROWER-SCAN
              IF IO-GOOD
                 GO TO READ-BWFILE
              ELSE
                 GO TO REC-BW-MODULE-NO.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-MODULE-NO.
           MOVE VDUNUM0 TO BW-SSNO.
           IF BW-SSNO = ZERO
               GO TO REC-BW-MODULE-NO.
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
       BORROWER-SCAN SECTION.
           MOVE 0   TO BWSCAN-NO.
           MOVE " " TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BORROWER-SCAN.
           MOVE BWSCAN-NO TO BW-SSNO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110BWNO." TO VDU.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           MOVE ALP-BW TO VDUBUF.
           MOVE 0 TO IO-FG.
       EXIT-BORROWER-SCAN.
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
              IF FORM-SEL1 < 0 OR = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 > 3
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO EXIT-ENTRY-MODULE.
           GO TO MODIFY-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE BW-LNAME    TO F01.
           MOVE BW-FNAME    TO F02.
           MOVE BW-ADR1     TO F03.
           MOVE BW-ADR2     TO F04.
           MOVE BW-CITY     TO F05.
           MOVE BW-STATE    TO F06.
           MOVE BW-ZIP      TO F07.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F08.
           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F09.
           IF (BW-BDATE NOT = 0)
              MOVE BW-BDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
              MOVE DATE-MMDDYYYY TO F10.
           MOVE BW-ORIGIN   TO F11.
           MOVE BW-DEPEND   TO F12.
           MOVE BW-RENTCD   TO F13.
           MOVE BW-TIMEADDR TO F14.
           MOVE BW-DWELLCD  TO F15.
           MOVE BW-CREDITCD TO F16.
           MOVE BW-STRTGCD  TO F17.
           MOVE BW-CENSUSCD TO F18.
           MOVE BW-ORGBR    TO F19.
           MOVE BW-COUNTY   TO F20.
           MOVE BW-ORGSLSMN TO F21.
           MOVE BW-REPCODE  TO F22.
           MOVE BW-SOLICIT  TO F23.
           MOVE BW-CONFIDENTIAL TO F24.
           MOVE BW-EMPLOYER TO F25.
           MOVE BW-EMPLPOS  TO F26.
           IF BW-EMPPHONE NOT = 0
              MOVE BW-EMPPHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F27.
           MOVE BW-EMPEXT   TO F271.
           MOVE BW-EMPYEARS TO F28.
           MOVE BW-WORKHRS  TO F281.
           MOVE BW-EMPPAY   TO F29.
           MOVE BW-EMPPAYOT TO F30.
           MOVE BW-EMPPAYSR TO F31.
           MOVE BW-SPLNAME  TO F32.
           MOVE BW-SPFNAME  TO F33.
           IF BW-SPSSNO > ZERO
               MOVE BW-SPSSNO TO F34
               INSPECT F34 REPLACING ALL "/" BY "-".
           IF (BW-SP-BDATE NOT = 0)
              MOVE BW-SP-BDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
              MOVE DATE-MMDDYYYY TO F35.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO BWINQ1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWINQ1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BWINQ1-SCREEN UPON BWINQ-WINDOW-HANDLE.
           MOVE BWINQ1-FORM-FIELDS TO WR-BUF.
           MOVE BWINQ1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE BWINQ1-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWINQ1-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWINQ1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
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

      *********************************************
       IO-ROUTINE SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWINQ1-SCREEN.
           MOVE BWINQ-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
