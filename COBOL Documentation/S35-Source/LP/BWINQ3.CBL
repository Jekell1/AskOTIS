      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWINQ3.
      *****************************************************
      *                 (LP)
      *  DESCRIPTION:   BORROWER MASTER FILE INQUIRY
      *                       PAGE 3 MEMO
      *  REV:
      *  GLF 111098 MADE BORROWER MEMO DATE 2000 COMPATIBLE
      *  GLF 990908 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *
      * KEC 2016.0926 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED BM-MEMO-HOLD-KEY FOR NEW SIZE OF BM1-KEY.
      * BAH 180926 GLOBAL BWFILE
      * BAH 181005 GLOBAL BMFILE
      * BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      * BAH 20220803 HARD CODED START
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
           "@(#)LP/BWINQ3.CBL S35 12/13/22-07:34:47 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".

       01  IX1             PIC 99 COMP-3.
       01  IX2             PIC 99 COMP-3.
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
       01  NAME            PIC X(16).
       01  HOLD-BW-SSNO    PIC 9(9).
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

       01  BWINQ-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  BWINQ-WINDOW-HANDLE PIC X(10).

      *--------------------------------------------
       01  HOLD-BM1-KEY.
           03  BM-MEMO-HOLD-OWNBR PIC 9(4).
           03  BM-MEMO-HOLD-SSNO  PIC 9(9).
           03  BM-MEMO-HOLD-DATE  PIC 9(8).
           03  BM-MEMO-HOLD-SEQ   PIC 9(3).
       01  MEMO-SUB            PIC 99     VALUE 0.
       01  MEMO-MAX            PIC 99     COMP-3 VALUE 13.

       01  DISPLAY-MEMO-LIST          PIC X(52)  VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08 L09 L10 L11 L12 L13.".
       01  DISPLAY-MEMO-BUF        VALUE SPACES.
           03  D-MEMO-TAB        OCCURS 13.
               05  D-MEMO-DATE    PIC 99/99/99.
               05  FILLER         PIC X(3).
               05  D-MEMO-SEQ     PIC ZZ.
               05  FILLER         PIC X(3).
               05  D-MEMO         PIC X(60).
               05  FILLER         PIC X(4).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWINQ3_DEF.CPY".
       COPY "LIBLP/BWINQ3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBLP/BWINQ3_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ***************************************
       MAIN-PROGRAM SECTION.
      ***************************************

           PERFORM SQL-CONNECT.

           MOVE "BWINQ3" TO VDU-FORM-NAME.
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

       RETURN-FROM-MEMO.
           PERFORM ENTRY-MODULE.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE BW-SSNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ****************************
      *    RECEIVE BWFILE MODULE *
      ****************************
       REC-BW-MODULE SECTION.
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = SPACES
              MOVE SAVEKEY TO BW-SSNO SS-KEY
              MOVE SPACES TO SAVEKEY
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDUBUF
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
           MOVE VDUNUM0 TO BW-SSNO HOLD-BW-SSNO.
           IF BW-SSNO = ZERO
               GO TO REC-BW-MODULE-NO.

      * WE DO NOT HAVE BM-PATH-OWNBR SET HERE, FILE ISNT OPEN
           MOVE BW-OWNBR TO BM-BRNO.
           MOVE BW-SSNO  TO BM-SSNO.
           MOVE 0        TO BM-DATE BM-SEQ.
           MOVE BM1-KEY TO HOLD-BM1-KEY.
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
           MOVE 0 TO FORM-SEL1.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-TYPEKEY SECTION.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
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
           IF TYP < 0 OR TYP > 4
             GO TO ENTER-TYPE.
       DISPLAY-SCAN-TYPE.
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
           IF TYP = 2 OR TYP = 4
              MOVE "E RA240SORTKEY." TO VDU
           ELSE
              MOVE "E RA110SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1U"
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
           MOVE BW-SSNO TO HOLD-BW-SSNO SOCSEC-KEY.
           MOVE SS-1 TO WK-1.
           MOVE SS-2 TO WK-2.
           MOVE SS-3 TO WK-3.
           MOVE "-" TO DASH-1 DASH-2.
           MOVE WORK-KEY TO BW-SSNO-EDIT.
           MOVE BW-SSNO-EDIT TO VDUBUF.
           PERFORM SCREENIO.

      * WE DO NOT HAVE BM-PATH-OWNBR SET HERE, FILE ISNT OPEN
           MOVE BW-OWNBR TO BM-BRNO.
           MOVE BW-SSNO  TO BM-SSNO.
           MOVE 0        TO BM-DATE BM-SEQ.
           MOVE BM1-KEY  TO HOLD-BM1-KEY.

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

      *******************************************
      *    ARRANGE BORROWER KEY
      *******************************************
       ARRANGE-BW-KEY SECTION.
       ARR-BW-KEY.
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
           ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1 IX2.
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

           IF VDUSTATUS = "1U"
              PERFORM BM-MEMO-PRIORITY-SCAN
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1D"
              PERFORM BM-MEM-SCAN
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 3
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1 AND NOT = 2
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO EXIT-ENTRY-MODULE.

           GO TO MODIFY-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***********************************
       DISPLAY-FIELDS SECTION.
      ***********************************
           MOVE "S  A340BWNAME." TO VDU.
           MOVE SPACES TO VDUBUF.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO VDUBUF.
           PERFORM SCREENIO.
           PERFORM BM-MEMO-RESET-HOLD-KEY.
           PERFORM BM-MEM-SCAN.

      ***********************************
       BM-MEM-SCAN SECTION.
      ***********************************
           MOVE 0 TO MEMO-SUB.

           MOVE SPACES TO DISPLAY-MEMO-BUF.

           MOVE HOLD-BM1-KEY TO BM1-KEY.
           ADD 1             TO BM-SEQ.

           PERFORM OPEN-BM1-FILE.

           MOVE BM-BRNO          TO QBM1-WBEG-BRNO
                                    QBM1-WEND-BRNO.
           MOVE BM-SSNO          TO QBM1-WBEG-SSNO
                                    QBM1-WEND-SSNO.
           MOVE BM-DATE-REVERSED TO QBM1-WBEG-DATE-REV.
           MOVE BM-SEQ           TO QBM1-WBEG-SEQ.
           MOVE ALL "9"          TO QBM1-WEND-DATE-REV
                                    QBM1-WEND-SEQ.
           PERFORM START-BM1-FILE.

       BM-MEM-SCAN-NEXT-SEQ.
           PERFORM READ-BM1-FILE-NEXT.
           IF IO-GOOD
              IF (BM-SSNO NOT = BW-SSNO) OR
                   (BM-BRNO NOT = BM-PATH-OWNBR)
                 MOVE 9 TO IO-FG.
           IF IO-BAD
              PERFORM BM-MEMO-RESET-HOLD-KEY
              GO TO BM-MEM-SCAN-EXIT.

           MOVE BM1-KEY TO HOLD-BM1-KEY.
           ADD 1 TO MEMO-SUB.
           IF BM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - BM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-MEMO-DATE(MEMO-SUB)
           ELSE
              MOVE "PRIORITY" TO D-MEMO-TAB(MEMO-SUB).
           MOVE BM-SEQ  TO D-MEMO-SEQ(MEMO-SUB).
           MOVE BM-MEMO TO D-MEMO(MEMO-SUB).
           IF MEMO-SUB < MEMO-MAX
              GO TO BM-MEM-SCAN-NEXT-SEQ.
       BM-MEM-SCAN-EXIT.
           MOVE DISPLAY-MEMO-BUF TO WR-BUF.
           MOVE DISPLAY-MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-BM1-FILE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO BWINQ3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWINQ3-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BWINQ3-SCREEN UPON BWINQ-WINDOW-HANDLE.
           MOVE BWINQ3-FORM-FIELDS TO WR-BUF.
           MOVE BWINQ3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE BWINQ3-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWINQ3-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWINQ3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       BM-MEMO-PRIORITY-SCAN.
           PERFORM BM-MEMO-RESET-HOLD-KEY.
           PERFORM BM-MEM-SCAN.
       BM-MEMO-RESET-HOLD-KEY.
           MOVE BW-OWNBR TO BM-BRNO.
           MOVE BW-SSNO  TO BM-SSNO.
           MOVE 0        TO BM-DATE BM-SEQ.
           MOVE BM1-KEY  TO HOLD-BM1-KEY.

      *****************************************
       IO-ROUTINE SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPBM1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************
       END-ROUTINE SECTION.
      *****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWINQ3-SCREEN.
           MOVE BWINQ-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
