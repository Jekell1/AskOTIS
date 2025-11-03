       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHSSNO.
       DATE-WRITTEN.    081588.
      *******************************************************************
      *                 (LP)
      *  DESCRIPTION:   CHANGE BORROWER SOC.SEC.NO.
      *                     FROM LONPX0 (CHANGES MENU)
      *
      *                 THIS PROGRAM ALLOW THE ENTRY OF A
      *                 NEW SOCIAL SECURITY NO. AND CHANGES,
      *                 DELETES AND CREATES ALL RECORDS.
      *
      * REV:
      *  MJD 061495 SET UP LP-USERID, LP-POSTDATE, AND LP-POSTTIME.
      *  MJD 062995 REMOVED LP-USERID, ETC. NOT NEEDED WITH REWRITE.
      *  BAH 091295 ADDED LOGIC FOR BI-FILE  UPDATE
      *   CS 041697 STOPPED CHANGE OF SS TO 999999999
      *  JTG 081497 CHANGED LN-SALEFIN TO LN-GIFTNET IN BICREA CALL
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 120497 EXPANDED BW-LOANS TO PIC 99
      *  BLV 020698 ADDED LN-SALEFIN BACK TO BICREA CALL
      *  MJD 021998 CHANGED BW-REC TO 546 BYTES, ADDED BW-SIZE (78)
      *  MJD 030398 ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  GLF 090199 ADDED A MEMO TO THE NEW SSNO TO LOG THE CHANGE PR#884
      *  GLF 990908 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BLV 991201 FIX ERRORS IN NEW SSNO ENTRY ROUTINES
      *  BLV 991203 FIX ERRORS FROM 90199 CHANGE, WAS READING BMFILE
      *             WITH WRONG SSNO TO GET SEQ, CAUSED WRITE ERRORS 22
      *  CS  010417 ADD APP FILES , PR# 1465 SUPPORT
      *  BAH 030618 ADD USERID OF WHO CHANGED THE SSNO TO THE BORROWER
      *             MEMO, LENDMARK PL# 409
      *  BAH 061116 FORGOT TO CHANGE SSNO IN BX TRAILER, LENDMARK PR# 2044
      *  BLM 090227 CHECK IF ARCHIVE LOANS EXIST AND UPDATE CHANGED SSNO
      *             IN ARCHIVED LOANS & PAYMENTS, WORLD PR# 547
      *             NOTE - WILL SET LN-PATH & LP-PATH TO ARCHIVED PATHS,
      *             THEN PERFORM SAME UPDATE ROUTINES (WILL SKIP OUT OF
      *             UPDATING BI-FILE), THEN RESTORES PATHS TO DEFAULT
      *  BLM 090304 ADD UPDATE OF HISTORY FILE (LHFILE) - PRIMARY KEY IS
      *             SSNO SO HAVE TO DELETE & WRITE WITH NEW SSNO,
      *             WORLD PR# 547
      *  BLM 100914 DON'T SEND MESSAGE "INVALID ARCHIVE PATH" IF
      *             ARCHIVE PATH DOES NOT EXIST, BRANCH MAY NOT HAVE
      *             PURGED TO ARCHIVE, WORLD PR# 547
      *  BLM 110801 UPDATE CUSTOMER INFORMATION FILE (CIFILE) WITH NEW
      *             SSNO FOR CI-SSNO (KEY2) & CI-SSNO-2 (KEY3) IF
      *             I-AM-WORLD, WORLD PR# 739
      * BAH 160422 ADDED BI-LN-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      * BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180316 ADDED LN-PLDATE TO BICREA CALL, WORLD #1308
      * KEC 180912 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181001 GLOBAL BWFILE
      * BAH 181005 GLOBAL BMFILE
      * BAH 181009 GLOBAL CIFILE
      * BAH 181127 GLOBAL LHFILE
      * BAH 181205 GLOBAL LNFILE
      * BAH 181221 GLOBAL LPFILE
      * BAH 190513 NEW CHFILE
      * BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      * BAH 211021 CHANGE REWRITE OF SECONDARY KEYS CI2 AND CI3 TO CI1
      * BAH 20220803 HARD CODED START
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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
           "@(#)LP/CHSSNO.CBL S35 10/17/24-07:40:46 >".
      /**********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".


       01  IX1               PIC 99      COMP-3.
       01  IX2               PIC 99      COMP-3.
       01  TYP               PIC 9       VALUE 0.
       01  ONE               PIC S9.
       01  ELE               PIC 9(2).
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  RTNCD             PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-SCAN        PIC 9       VALUE 0.
       01  NEW-SSNO-EXISTS   PIC X       VALUE SPACES.
       01  NAME-WORKER       PIC X(34).

       01  ALREADY-EXISTS-MESS.
           03  FILLER         PIC X(44) VALUE
           "SSNO # ALREADY EXISTS !!".
       01  ALREADY-EXISTS-MESS2.
           03  FILLER         PIC X(43) VALUE
           "ENTER 'YES' TO OVERWRITE, OR PRESS <ENTER> ".
           03  FILLER         PIC X(17) VALUE
           "TO CANCEL ".
       01  HOLD-CH-SEQ         PIC 9(3).
       01  SAVE-OLD-FNAME      PIC X(16).
       01  SAVE-OLD-LNAME      PIC X(16).
       01  SAVE-OLD-BW-SSNO     PIC 9(9) VALUE 0.
       01  SAVE-OLD-LOANS      PIC 99.
       01  SAVE-OLD-PRLOANS    PIC 99.
       01  SAVE-OLD-INTYTD     PIC S9(7)V99 COMP-3 VALUE 0.
       01  SAVE-OLD-INTLYR     PIC S9(7)V99 COMP-3 VALUE 0.
       01  SAVE-OLD-RE-INTYTD  PIC S9(7)V99 COMP-3 VALUE 0.
       01  SAVE-OLD-RE-INTLYR  PIC S9(7)V99 COMP-3 VALUE 0.
       01  SAVE-OLD-LIASMLN    PIC 9(6)V99  COMP-3 VALUE 0.
       01  SAVE-OLD-LIAOTH     PIC 9(6)V99  COMP-3 VALUE 0.
       01  SAVE-OLD-LASTPODATE PIC 9(8).

       01  SAVE-NEW-BW-SSNO    PIC 9(9) VALUE 0.

       01  HOLD-LN-BWTABLE.
           03  HOLD-LN-BORRTBL      OCCURS 4 TIMES.
               05  HOLD-LN-SSNO     PIC 9(9).
               05  HOLD-LN-MAKERCD  PIC X.

       01  BW-ALP-TABLE.
           03  BW-ALP-TBL    OCCURS 70 TIMES
                             PIC X.

       01  ORIGINAL-BW-REC   PIC X(BW-SIZE).
           COPY "LIBLP/LP01BM.CPY" REPLACING LEADING "BM" BY "HOLD-BM".
           COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "HOLD-BX".
           COPY "LIBLP/LP01LH.CPY" REPLACING LEADING "LH" BY "HOLD-LH".

       01  NEW-SSNO-MEMO     PIC 999/99/9999.
       01  OLD-SSNO-MEMO     PIC 999/99/9999.
       01  TEST-SSNO         PIC 9(9).

      **********************************
      *    SCREEN DISPLAY BUFFER
      **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  FBWNO         PIC 999/99/9999.
           03  FNAME         PIC X(32).
           03  FADDR1        PIC X(30).
           03  FADDR2        PIC X(30).
           03  FADDR3        PIC X(30).
           03  FSPSSNO       PIC 999/99/9999.
           03  FSPNAME       PIC X(32).
       01  DISPLAY-LIST.
           03  FILLER        PIC X(44) VALUE
           "BWNO NAME ADDR1 ADDR2 ADDR3 SPSSNO SPNAME.".

       01  MAX               PIC 99   VALUE 1.

      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  SPEC-TABLE.
      *01. NEW SOCIAL SECURITY NUMBER
           03  FILLER        PIC X(8) VALUE "ERNX090 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 1 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHSSNO_DEF.CPY".
       COPY "LIBLP/CHSSNO_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CHSSNO_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHSSNO" TO VDU-FORM-NAME.

           MOVE " " TO ERRCD.
       MAIN-MODULE.
           MOVE 0 TO SAVE-OLD-BW-SSNO SAVE-NEW-BW-SSNO.

           PERFORM FORM-RESET.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "UPD"
              PERFORM UPDATE-MODULE.
           GO TO MAIN-MODULE.

      ***********************************
       REC-BW-MODULE SECTION.
      ***********************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE 0 TO SAVE-OLD-BW-SSNO.
           MOVE "ENTER ORIGINAL SSNO #, F6-SCAN, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-TYPEKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-MODULE-NO.

           MOVE VDUNUM0 TO SAVE-OLD-BW-SSNO BW-SSNO.
           IF SAVE-OLD-BW-SSNO = 0
              GO TO REC-BW-MODULE-NO.
       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF NOT IO-GOOD
              GO TO REC-BW-MODULE-NO.

           MOVE BW-REC TO ORIGINAL-BW-REC.
           PERFORM DISPLAY-FIELDS.
       END-REC-BW-MODULE.
           EXIT.

      *************************************************
      *      ENTER SORTKEY ROUTINES
      *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *             2 - BORROWER NAME
      *             3 - SPOUSE SOCIAL SEC. #
      *             4 - SPOUSE NAME
      *************************************************
       ENTER-TYPEKEY SECTION.
       ENTER-TYPE.
           MOVE " 1=SS#, 2=NAME, 3=SPOUSE SS#, 4=SPOUSE NAME, F6=SCAN"
              TO MESS.
           PERFORM SEND-LEGEND.

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
              MOVE " ENTER SS#, OR F6-SCAN" TO MESS
              PERFORM SEND-LEGEND
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE.
           IF TYP = 2
              MOVE " ENTER NAME (LAST), OR F6-SCAN" TO MESS
              PERFORM SEND-LEGEND
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE.
           IF TYP = 3 
              MOVE " ENTER SPOUSE SS#, OR F6-SCAN" TO MESS
              PERFORM SEND-LEGEND
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE.
           IF TYP = 4 
              MOVE " ENTER SPOUSE NAME (LAST), OR F6-SCAN" TO MESS
              PERFORM SEND-LEGEND
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE.
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
              PERFORM CLOSE-SORTKEY
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE BW-REC TO ORIGINAL-BW-REC
                 MOVE BW-SSNO TO SAVE-OLD-BW-SSNO
                 GO TO CLOSE-SORTKEY
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
      *               UNLOCK BW-FILE.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SORTKEY.
           PERFORM CLOSE-BW1-FILE.
       END-SORTKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
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

      /************************************************
      *    ENTRY-MODULE
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
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
      D    STOP MESS.
           MOVE "ENTER FIELD #, F1-CANCEL, <ENTER> TO UPDATE" TO MESS.
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
              IF SAVE-NEW-BW-SSNO = 0
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "UPD" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ELE = 1
              MOVE "F1-CLEAR, ENTER NEW SOCIAL SECURITY # " TO MESS
              PERFORM SEND-LEGEND
              MOVE SPACES TO WR-BUF
              MOVE "OLDNAME OLDACCEPT." TO WR-LIST
              PERFORM CALL-WRFORM.

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
           GO TO F-01.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO SAVE-NEW-BW-SSNO.
           IF SAVE-NEW-BW-SSNO = 999999999
              GO TO ENTER-ELE.
           IF SAVE-NEW-BW-SSNO = SAVE-OLD-BW-SSNO
              MOVE "CANT CHANGE TO ORIGINAL NUMBER!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE SAVE-NEW-BW-SSNO TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           MOVE " " TO NEW-SSNO-EXISTS.
           IF IO-GOOD
              MOVE "S  A320OLDNAME." TO VDU
              MOVE SPACES TO VDUBUF
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                    INTO VDUBUF
              PERFORM SCREENIO

              MOVE "S  A030OLDACCEPT." TO VDU
              MOVE "___" TO VDUBUF
              PERFORM SCREENIO
              MOVE SPACES TO MESS
              PERFORM SEND-LEGEND
              MOVE ALREADY-EXISTS-MESS TO MESS
              PERFORM SEND-MESS
              MOVE ALREADY-EXISTS-MESS2 TO MESS
              PERFORM SEND-LEGEND
              MOVE "E  A030OLDACCEPT." TO VDU
              PERFORM SCREENIO
              IF VDUBUF = "YES"
                 MOVE "Y" TO NEW-SSNO-EXISTS
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *************************************
       UPDATE-MODULE SECTION.
      *************************************
           PERFORM UPDATE-DELETE-OLD-BW.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-BW3-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           IF NEW-SSNO-EXISTS NOT = "Y"
              PERFORM UPDATE-BW1-FILE
              IF RTNCD = "END"
                 GO TO END-UPDATE-MODULE.

           IF NEW-SSNO-EXISTS = "Y"
              PERFORM UPDATE-BW1-FILE-EXISTS
              IF RTNCD = "END"
                 GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-BM1-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LN4-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LN5-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

      *    PERFORM UPDATE-LN6-FILE.
      *    IF RTNCD = "END"
      *       GO TO END-UPDATE-MODULE.

      *    PERFORM UPDATE-LN7-FILE.
      *    IF RTNCD = "END"
      *       GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LH1-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-CI-FILE
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

      * CREATE A CHANGE-RECORD
           PERFORM CREATE-CHANGE-REC.

           PERFORM CREATE-NEW-BM1-RECORD.

       END-UPDATE-MODULE.
           EXIT.

      *************************************
       UPDATE-DELETE-OLD-BW SECTION.
      *************************************
           MOVE SPACES TO RTNCD.
           MOVE "DELETING OLD BORROWER RECORD!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-BW1-FILE.

           MOVE ORIGINAL-BW-REC TO BW-REC.

           MOVE BW-SSNO       TO SAVE-OLD-BW-SSNO.
           MOVE BW-LNAME      TO SAVE-OLD-LNAME.
           MOVE BW-FNAME      TO SAVE-OLD-FNAME.
           MOVE BW-LOANS      TO SAVE-OLD-LOANS.
           MOVE BW-PRLOANS    TO SAVE-OLD-PRLOANS.

           MOVE BW-RE-INTYTD  TO SAVE-OLD-RE-INTYTD.
           MOVE BW-RE-INTLYR  TO SAVE-OLD-RE-INTLYR.

           MOVE BW-INTYTD     TO SAVE-OLD-INTYTD.
           MOVE BW-INTLYR     TO SAVE-OLD-INTLYR.

           MOVE BW-LIASMLN    TO SAVE-OLD-LIASMLN.
           MOVE BW-LIAOTH     TO SAVE-OLD-LIAOTH.
           MOVE BW-LASTPODATE TO SAVE-OLD-LASTPODATE.
           PERFORM READ-BW1-FILE.
           IF NOT IO-GOOD
              PERFORM RECORD-DELETED
              MOVE "END" TO RTNCD
              GO TO END-UPDATE-DELETE-OLD-BW.
           IF BW-REC NOT = ORIGINAL-BW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              MOVE "END" TO RTNCD
              GO TO END-UPDATE-DELETE-OLD-BW.
           PERFORM DELETE-BW1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
       END-UPDATE-DELETE-OLD-BW.
           PERFORM CLOSE-BW1-FILE.

           PERFORM OPEN-BX1-FILE.
           MOVE SAVE-OLD-BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG = 0
              MOVE BX-REC TO HOLD-BX-REC
              PERFORM DELETE-BX1-FILE
              MOVE HOLD-BX-REC TO BX-REC
              MOVE SAVE-NEW-BW-SSNO TO BX-SSNO
              PERFORM WRITE-BX1-FILE.

           PERFORM CLOSE-BX1-FILE.


      ****************************************************
      *    CHANGE ALL BORROWER RECORDS WHERE THIS BORROWER
      *    APPEARS AS A SPOUSE:
      ****************************************************
       UPDATE-BW3-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "CHANGING ALL BORROWER SPOUSE FIELDS!" TO MESS.
           PERFORM SEND-LEGEND.
       UPDATE-BW3-NEXT.
           PERFORM OPEN-BW3-FILE.

           MOVE BW-PATH-OWNBR    TO BW-OWNBR
                                    QBW3-WBEG-OWNBR
                                    QBW3-WEND-OWNBR.
           MOVE SAVE-OLD-BW-SSNO TO BW-SPSSNO
                                    QBW3-WBEG-SPSSNO
                                    QBW3-WEND-SPSSNO.

           PERFORM START-BW3-FILE.
           IF (IO-FG = 9)
              PERFORM CLOSE-BW3-FILE
              GO TO END-UPDATE-BW3-FILE.

           PERFORM READ-BW3-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
              PERFORM CLOSE-BW3-FILE
              GO TO END-UPDATE-BW3-FILE.

           IF BW-SPSSNO NOT = SAVE-OLD-BW-SSNO
              PERFORM CLOSE-BW3-FILE
              GO TO END-UPDATE-BW3-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE SAVE-NEW-BW-SSNO TO BW-SPSSNO.
           MOVE SAVE-OLD-LNAME TO BW-SPLNAME.
           MOVE SAVE-OLD-FNAME TO BW-SPFNAME.
           PERFORM REWRITE-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           GO TO UPDATE-BW3-NEXT.
       END-UPDATE-BW3-FILE.
           EXIT.

      *******************************************
      *    CREATE BORROWER RECORD WITH NEW SSNO:
      *******************************************
       UPDATE-BW1-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "CREATING NEW BORROWER RECORD!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-BW1-FILE.
           MOVE ORIGINAL-BW-REC TO BW-REC.
           MOVE SAVE-NEW-BW-SSNO TO BW-SSNO.
           PERFORM WRITE-BW1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           PERFORM CLOSE-BW1-FILE.

      ********************************************
      *    UPDATE BORROWER RECORD WITH OLD STATS:
      ********************************************
       UPDATE-BW1-FILE-EXISTS SECTION.

           MOVE SPACES TO RTNCD.

           MOVE "UPDATING OLD BORROWER STATS!" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-BW1-FILE.
           MOVE SAVE-NEW-BW-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           ADD SAVE-OLD-LOANS     TO BW-LOANS.
           ADD SAVE-OLD-PRLOANS   TO BW-PRLOANS.

           ADD SAVE-OLD-RE-INTYTD TO BW-RE-INTYTD.
           ADD SAVE-OLD-RE-INTLYR TO BW-RE-INTLYR.

           ADD SAVE-OLD-INTYTD    TO BW-INTYTD.
           ADD SAVE-OLD-INTLYR    TO BW-INTLYR.
           ADD SAVE-OLD-LIASMLN   TO BW-LIASMLN.
           ADD SAVE-OLD-LIAOTH    TO BW-LIAOTH.

           IF SAVE-OLD-LASTPODATE > BW-LASTPODATE
              MOVE SAVE-OLD-LASTPODATE TO BW-LASTPODATE.

           PERFORM REWRITE-BW1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           PERFORM CLOSE-BW1-FILE.

      ****************************************************
      *    DELETE ALL BORROWER MEMO RECORDS WITH THE OLD
      *    KEY AND RE-CREATE WITH NEW KEY:
      *****************************************************
       UPDATE-BM1-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "UPDATING ALL MEMO RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

       UPDATE-BM1-NEXT.
           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR    TO BM-BRNO
                                    QBM1-WBEG-BRNO
                                    QBM1-WEND-BRNO.
           MOVE SAVE-OLD-BW-SSNO TO BM-SSNO
                                    QBM1-WBEG-SSNO
                                    QBM1-WEND-SSNO.
           MOVE 0                TO BM-DATE
                                    QBM1-WBEG-DATE-REV
                                    BM-SEQ
                                    QBM1-WBEG-SEQ.
           MOVE ALL "9"       TO QBM1-WEND-DATE-REV
                                 QBM1-WEND-SEQ.
           PERFORM START-BM1-FILE.

           IF (IO-FG = 9) 
              GO TO END-UPDATE-BM1-FILE.

           PERFORM READ-BM1-FILE-NEXT.
           IF (IO-FG = 9) OR (BM-BRNO NOT = BM-PATH-OWNBR) 
              GO TO END-UPDATE-BM1-FILE.

           IF BM-SSNO NOT = SAVE-OLD-BW-SSNO
              GO TO END-UPDATE-BM1-FILE.

           MOVE BM-REC TO HOLD-BM-REC.
           PERFORM DELETE-BM1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE HOLD-BM-REC TO BM-REC.
           MOVE SAVE-NEW-BW-SSNO TO BM-SSNO.
           PERFORM WRITE-BM1-FILE.

           GO TO UPDATE-BM1-NEXT.
       END-UPDATE-BM1-FILE.
           PERFORM CLOSE-BM1-FILE.

      ***************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN4-KEY):
      ****************************************************
       UPDATE-LN4-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "UPDATING ALL LOAN (MAKER 1) RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

       UPDATE-LN4-NEXT.
           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR.
           MOVE SAVE-OLD-BW-SSNO TO LN-SSNO(1)
                                    QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
           IF NOT IO-GOOD
              GO TO END-UPDATE-LN4-FILE.

           PERFORM READ-LN4-FILE-NEXT.
           IF (IO-BAD) OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                        OR (LN-SSNO(1) NOT = SAVE-OLD-BW-SSNO)
              GO TO END-UPDATE-LN4-FILE.

           PERFORM UPDATE-LP1-FILE.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE LN-BWTABLE TO HOLD-LN-BWTABLE.
           MOVE SAVE-NEW-BW-SSNO TO LN-SSNO1.
           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           MOVE 0 TO HOLD-LN-SSNO(2)
                     HOLD-LN-SSNO(3)
                     HOLD-LN-SSNO(4).

           PERFORM UPDATE-BI-FILE-SWITCH-SSNO.

           GO TO UPDATE-LN4-NEXT.
       END-UPDATE-LN4-FILE.
           PERFORM CLOSE-LN4-FILE.

      ****************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN5-KEY):
      ****************************************************
       UPDATE-LN5-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "UPDATING ALL LOAN (MAKER 2) RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

       UPDATE-LN5-NEXT.
           PERFORM OPEN-LN5-FILE.

           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR.
           MOVE SAVE-OLD-BW-SSNO TO LN-SSNO(2)
                                    QLN5-WBEG-SSNO2
                                    QLN5-WEND-SSNO2.

           PERFORM START-LN5-FILE.
           IF NOT IO-GOOD
              GO TO END-UPDATE-LN5-FILE.

           PERFORM READ-LN5-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(2) NOT = SAVE-OLD-BW-SSNO)
              GO TO END-UPDATE-LN5-FILE.

           PERFORM UPDATE-LP1-FILE.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           MOVE LN-BWTABLE TO HOLD-LN-BWTABLE.
           MOVE SAVE-NEW-BW-SSNO TO LN-SSNO2.
           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           MOVE 0 TO HOLD-LN-SSNO(1)
                     HOLD-LN-SSNO(3)
                     HOLD-LN-SSNO(4).
           PERFORM UPDATE-BI-FILE-SWITCH-SSNO.

           GO TO UPDATE-LN5-NEXT.
       END-UPDATE-LN5-FILE.
           PERFORM CLOSE-LN5-FILE.

      ****************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN6-KEY):
      ****************************************************
      *UPDATE-LN6-FILE SECTION.
      *    MOVE SPACES TO RTNCD.
      *    MOVE "UPDATING ALL LOAN (MAKER 3) RECORDS!" TO MESS.
      *    PERFORM SEND-LEGEND.

      *UPDATE-LN6-NEXT.
      *    PERFORM OPEN-LN6-FILE.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE SAVE-OLD-BW-SSNO TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.

      *    PERFORM START-LN6-FILE.
      *    IF NOT IO-GOOD
      *       GO TO END-UPDATE-LN6-FILE.

      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = SAVE-OLD-BW-SSNO)
      *       GO TO END-UPDATE-LN6-FILE.

      *    PERFORM UPDATE-LP1-FILE.

      *    PERFORM OPEN-LN1-FILE.
      *    PERFORM READ-LN1-FILE.
      *    IF NOT IO-GOOD
      *       GO TO IO-ERROR.
      *    MOVE LN-BWTABLE TO HOLD-LN-BWTABLE.
      *    MOVE SAVE-NEW-BW-SSNO TO LN-SSNO3.
      *    PERFORM REWRITE-LN1-FILE.
      *    PERFORM CLOSE-LN1-FILE.

      *    MOVE 0 TO HOLD-LN-SSNO(1)
      *              HOLD-LN-SSNO(2)
      *              HOLD-LN-SSNO(4).
      *    PERFORM UPDATE-BI-FILE-SWITCH-SSNO.

      *    GO TO UPDATE-LN6-NEXT.
      *END-UPDATE-LN6-FILE.
      *    PERFORM CLOSE-LN6-FILE.

      ****************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN7-KEY):
      ****************************************************
      *UPDATE-LN7-FILE SECTION.
      *    MOVE SPACES TO RTNCD.
      *    MOVE "UPDATING ALL LOAN (MAKER 4) RECORDS!" TO MESS.
      *    PERFORM SEND-LEGEND.

      *UPDATE-LN7-NEXT.
      *    PERFORM OPEN-LN7-FILE.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE SAVE-OLD-BW-SSNO TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.

      *    PERFORM START-LN7-FILE.
      *    IF NOT IO-GOOD
      *       GO TO END-UPDATE-LN7-FILE.

      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO4 NOT = SAVE-OLD-BW-SSNO)
      *       GO TO END-UPDATE-LN7-FILE.

      *    PERFORM UPDATE-LP1-FILE.

      *    PERFORM OPEN-LN1-FILE.
      *    PERFORM READ-LN1-FILE.
      *    IF NOT IO-GOOD
      *       GO TO IO-ERROR.
      *    MOVE LN-BWTABLE TO HOLD-LN-BWTABLE.
      *    MOVE SAVE-NEW-BW-SSNO TO LN-SSNO4.
      *    PERFORM REWRITE-LN1-FILE.
      *    PERFORM CLOSE-LN1-FILE.

      *    MOVE 0 TO HOLD-LN-SSNO(1)
      *              HOLD-LN-SSNO(2)
      *              HOLD-LN-SSNO(3).
      *    PERFORM UPDATE-BI-FILE-SWITCH-SSNO.

      *    GO TO UPDATE-LN7-NEXT.
      *END-UPDATE-LN7-FILE.
      *    PERFORM CLOSE-LN7-FILE.

      *****************************************
      *    UPDATE BI FILE FOR SWITCH IN SSNO
      *****************************************
       UPDATE-BI-FILE-SWITCH-SSNO SECTION.

           MOVE "CL/BICREA" TO FORM-NAM.
           MOVE "SS" TO STAT.
           CALL FORM-PROGX USING FORM-PATH
                                 STAT
                                 LN-ACCTNO
                                 HOLD-LN-BWTABLE
                                 LN-OWNBR
                                 LN-ORGST
                                 LN-SALEFIN
                                 LN-GIFTNET
                                 LN-ENTDATE
                                 LN-POFFDATE
                                 LN-PLCD
                                 LN-CONVERCD
                                 SAVE-NEW-BW-SSNO
                                 LN-SMALLOAN
                                 LN-PLDATE.

           CANCEL FORM-PROGX.

       EXIT-UPDATE-BI.
           EXIT.

      ****************************************************
      *    CHANGE ALL LOAN PAYMENT RECORDS WHERE THIS
      *    BORROWER APPEARS AS A MAKER (LP-SSNO):
      ****************************************************
       UPDATE-LP1-FILE SECTION.
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.
       UPDATE-LP1-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LN-ACCTNO NOT = LP-ACCTNO)
              GO TO END-UPDATE-LP1-FILE.

           IF LP-SSNO = SAVE-OLD-BW-SSNO
              MOVE SAVE-NEW-BW-SSNO TO LP-SSNO
              PERFORM REWRITE-LP1-FILE.
      *    ELSE
      *       UNLOCK LP-FILE.

           GO TO UPDATE-LP1-NEXT.

       END-UPDATE-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.

      ****************************************************
      *    DELETE ALL LOAN HISTORY RECORDS WITH OLD SSNO
      *    KEY AND RE-CREATE WITH NEW KEY:
      *****************************************************
       UPDATE-LH1-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "UPDATING ALL HISTORY RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

       UPDATE-LH1-NEXT.
           PERFORM OPEN-LH1-FILE.

           MOVE LH-PATH-OWNBR    TO LH-BRNO
                                    QLH1-WBEG-BRNO
                                    QLH1-WEND-BRNO.
           MOVE SAVE-OLD-BW-SSNO TO LH-SSNO
                                    QLH1-WBEG-SSNO
                                    QLH1-WEND-SSNO.
           MOVE 0                TO LH-ACCTNO
                                    QLH1-WBEG-ACCTNO.
           MOVE ALL "9"          TO QLH1-WEND-ACCTNO.

           PERFORM START-LH1-FILE.
           PERFORM READ-LH1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LH-BRNO NOT = LH-PATH-OWNBR)
              GO TO END-UPDATE-LH1-FILE.

           IF LH-SSNO NOT = SAVE-OLD-BW-SSNO
              GO TO END-UPDATE-LH1-FILE.

           MOVE LH-REC TO HOLD-LH-REC.
           PERFORM DELETE-LH1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE HOLD-LH-REC TO LH-REC.
           MOVE SAVE-NEW-BW-SSNO TO LH-SSNO.
           PERFORM WRITE-LH1-FILE.

           GO TO UPDATE-LH1-NEXT.

       END-UPDATE-LH1-FILE.
           PERFORM CLOSE-LH1-FILE.

      ***********************************************************
      * CHANGE ALL CUST INFO RECORDS WHERE THIS BORROWER APPEARS
      *************************************************************
       UPDATE-CI-FILE SECTION.
           MOVE SPACES TO RTNCD.
           MOVE "UPDATING ALL CUST INFO RECORDS!" TO MESS.
           PERFORM SEND-LEGEND.

       UPDATE-CI2-FILE.
           PERFORM OPEN-CI2-FILE.

           MOVE CI-PATH-OWNBR    TO CI-BRNO
                                    QCI2-WBEG-BRNO
                                    QCI2-WEND-BRNO.

           MOVE SAVE-OLD-BW-SSNO TO CI-SSNO
                                    QCI2-WBEG-SSNO1
                                    QCI2-WEND-SSNO1.

           PERFORM START-CI2-FILE.
       UPDATE-CI2-NEXT.
           PERFORM READ-CI2-FILE-NEXT.
           IF IO-BAD OR (CI-BRNO NOT = CI-PATH-OWNBR)
              GO TO UPDATE-CI3-FILE.

           IF CI-SSNO NOT = SAVE-OLD-BW-SSNO
              GO TO UPDATE-CI3-FILE.

           MOVE SAVE-NEW-BW-SSNO TO CI-SSNO.
           PERFORM REWRITE-CI1-FILE.

           GO TO UPDATE-CI2-NEXT.
       UPDATE-CI3-FILE.
           PERFORM CLOSE-CI2-FILE.
           PERFORM OPEN-CI3-FILE.

           MOVE CI-PATH-OWNBR    TO CI-BRNO
                                    QCI3-WBEG-BRNO
                                    QCI3-WEND-BRNO.

           MOVE SAVE-OLD-BW-SSNO TO CI-SSNO-2
                                    QCI3-WBEG-SSNO2
                                    QCI3-WEND-SSNO2.

           PERFORM START-CI3-FILE.
       UPDATE-CI3-NEXT.
           PERFORM READ-CI3-FILE-NEXT.
           IF IO-BAD OR (CI-BRNO NOT = CI-PATH-OWNBR)
              GO TO END-UPDATE-CI-FILE.

           IF CI-SSNO-2 NOT = SAVE-OLD-BW-SSNO
              GO TO END-UPDATE-CI-FILE.

           MOVE SAVE-NEW-BW-SSNO TO CI-SSNO-2.
           PERFORM REWRITE-CI1-FILE.

           GO TO UPDATE-CI3-NEXT.

       END-UPDATE-CI-FILE.
           PERFORM CLOSE-CI3-FILE.

      *********************************
       DISPLAY-FIELDS SECTION.
      *********************************
           MOVE SPACES TO DISPLAY-BUF.
      * SETUP BORROWER:
           MOVE BW-SSNO TO FBWNO.
           INSPECT FBWNO REPLACING ALL "/" BY "-".
           MOVE SPACES TO FNAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO FNAME.

           MOVE BW-ADR1 TO FADDR1.
           PERFORM SETUP-CITY-STATE-ZIP.
           IF BW-ADR2 = SPACES
              MOVE NAME-WORKER TO FADDR2
           ELSE
              MOVE BW-ADR2 TO FADDR2
              MOVE NAME-WORKER TO FADDR3.

      * SETUP BORROWER SPOUSE:
           IF BW-SPSSNO NOT = 0
              MOVE BW-SPSSNO TO FSPSSNO
              INSPECT FSPSSNO REPLACING ALL "/" BY "-".
           IF BW-SPLNAME NOT = SPACES AND
                    BW-SPFNAME NOT = SPACES
              MOVE SPACES TO FSPNAME
              STRING BW-SPLNAME,", ",BW-SPFNAME DELIMITED BY "   "
                   INTO FSPNAME.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ***************************************
       CREATE-NEW-BM1-RECORD SECTION.
      ***************************************
           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR    TO BM-BRNO.
           MOVE SAVE-NEW-BW-SSNO TO BM-SSNO.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           COMPUTE BM-DATE = 99999999 - DATE-YYYYMMDD.
           MOVE 0                TO BM-SEQ.
       CREATE-NEW-BM1-RECORD-NEXT.
           IF BM-SEQ = 999
              GO TO END-CREATE-NEW-BM1-RECORD.

           ADD 1 TO BM-SEQ.
           PERFORM READ-BM1-FILE.
           IF IO-GOOD
      *       UNLOCK BM-FILE
              GO TO CREATE-NEW-BM1-RECORD-NEXT.

           MOVE BM-PATH-OWNBR    TO BM-BRNO.
           MOVE SAVE-NEW-BW-SSNO TO BM-SSNO.
           COMPUTE BM-DATE = 99999999 - DATE-YYYYMMDD.
           MOVE "9" TO BM-MEMO-TYPE.
           MOVE SAVE-OLD-BW-SSNO TO OLD-SSNO-MEMO.
           INSPECT OLD-SSNO-MEMO REPLACING ALL "/" BY "-".
           MOVE BM-SSNO TO NEW-SSNO-MEMO.
           INSPECT NEW-SSNO-MEMO REPLACING ALL "/" BY "-".
           MOVE " " TO MESS.
           STRING
            EXT-CONAME-USER-ID," CHANGED OLD SSNO FROM ",
            OLD-SSNO-MEMO, " TO ",NEW-SSNO-MEMO
                  DELIMITED BY "  " INTO MESS.

           MOVE MESS TO BM-MEMO.
           PERFORM WRITE-BM1-FILE.
           IF IO-BAD
              PERFORM IO-ERROR.

       END-CREATE-NEW-BM1-RECORD.
           PERFORM CLOSE-BM1-FILE.

      ********************************
       CREATE-CHANGE-REC SECTION.
      ********************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.
           MOVE 1                  TO HOLD-CH-SEQ.
       CREATE-CHANGE-REC-AGAIN.

           PERFORM CLEAR-CH-FILE.

           MOVE BRANCH             TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "S"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE HOLD-CH-SEQ        TO CH-SEQ.
              
           MOVE "SOCIAL SECURITY # CHANGE" TO CH-DESCRIPT.
           MOVE SAVE-OLD-BW-SSNO   TO CH-ORG-SSNO.
           MOVE SAVE-NEW-BW-SSNO   TO CH-NEW-SSNO.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF HOLD-CH-SEQ < 999
                 ADD 1 TO HOLD-CH-SEQ
                 GO TO CREATE-CHANGE-REC-AGAIN.

           PERFORM CLOSE-CH-FILE.
       EXIT-CREATE-CHANGE-REC.
           EXIT.


      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE LPLOAN-CONAME TO CHSSNO-CONAME.
           MOVE LPLOAN-SYSDATE TO CHSSNO-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHSSNO-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CHSSNO-FORM-FIELDS TO WR-BUF.
           MOVE CHSSNO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE CHSSNO-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHSSNO-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHSSNO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SETUP-CITY-STATE-ZIP.
           MOVE BW-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ",? ".
           INSPECT NAME-WORKER REPLACING FIRST "  " BY
                                            BW-STATE.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY "?  ".
           INSPECT NAME-WORKER REPLACING FIRST "          "
                                            BY BW-ZIP.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCHCL.CPY".

      ********************************
       IO-ROUTINE SECTION.
      ********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-CI3-FILE.
           PERFORM CLOSE-CI2-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPCI1IN.CPY".
       COPY "LIBLP/LPCI2IN.CPY".
       COPY "LIBLP/LPCI3IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ********************************
       END-ROUTINE SECTION.
      ********************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHSSNO-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
