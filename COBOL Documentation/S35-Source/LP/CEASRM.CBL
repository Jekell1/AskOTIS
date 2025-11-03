       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CEASRM.
       DATE-WRITTEN.    081588.
      *******************************************************************
      *                 (LP)
      *  DESCRIPTION:   REMOVE CEASE AND DESIST 
      *
      *  BORROWER:
      *    IF VERBAL - CLEAR TYPE AND DATE, JUST CREATE A REMOVED MEMO
      *       
      *    IF WRITTEN - REMOVE "CEASE & DESIST" FROM BW-EMPLOYER AND
      *                 CREATE A REMOVED MEMO, CLEAR TYPE AND DATE
      *
      *  EMPLOYER:
      *    REMOVE "CEASE & DESIST" FROM BW-EMPLOYER AND CREATE A REMOVED
      *      MEMO CLEAR TYPE AND DATE
      *
      *  REFERENCE:
      *    REMOVE "CEASE & DESIST" FROM FIRST BORROWER REFERENCE AND
      *      CREATE A REMOVED MEMO CLEAR TYPE AND DATE
      *
      * PASSWORD MUST EXIST IN THE PWFILE LP/CEASRM SEQ 1
      *
      * REV:
      *  2024.1121 BAH NEW CEASE & DESIST REMOVE #1686
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/CEASRM S35 12/03/24-13:08:15 >".

      ***********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".

       01  SUB               PIC 999.
       01  IX1               PIC 99      COMP-3.
       01  IX2               PIC 99      COMP-3.
       01  TYP               PIC 9       VALUE 0.
       01  ONE               PIC S9.
       01  ELE               PIC 9(2).
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9       VALUE 0.
       01  FIRST-SCAN        PIC 9       VALUE 0.
       01  NAME-WORKER       PIC X(34).
       01  SAVE-BM-MEMO.
           03  MEMO-USERID      PIC X(10) VALUE SPACES.
           03  FILLER           PIC X(2)  VALUE "; ".
           03  MEMO-MSG         PIC X(32) VALUE SPACES.
           03  MEMO-TYPE        PIC X(16) VALUE SPACES.

       01  TEST-SSNO           PIC 9(9).
       01  TEST-CEASE-DESIST   PIC X(14).
       01  HOLD-CEASE-MSG     PIC X(30).
       01  HOLD-SSNO          PIC 9(9) VALUE 0.
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL    OCCURS 32 TIMES
                             PIC X.
 
      **********************************
      *    SCREEN DISPLAY BUFFER
      **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  FBWNO         PIC 999/99/9999.
           03  FNAME         PIC X(32).
           03  FADDR1        PIC X(30).
           03  FADDR2        PIC X(30).
           03  FADDR3        PIC X(30).
           03  EMP           PIC X(24).
           03  EMP1          PIC X(24).
           03  EMP2          PIC X(24).
           03  SOLICIT       PIC X.
           03  CONF          PIC X.
           03  TEXTOPT       PIC X.
           03  CEASE         PIC X.
       01  DISPLAY-LIST.
           03  FILLER        PIC X(42) VALUE
           "BWNO NAME ADDR1 ADDR2 ADDR3 EMP EMP1 EMP2 ".
           03  FILLER        PIC X(24) VALUE
           "SOLICIT CONF TEXT CEASE.".
 
       01  MAX               PIC 99   VALUE 1.
       01  REMOVE-CEASE      PIC X    VALUE SPACES.
      
 
      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  SPEC-TABLE.
      * 01. REMOVE CEASE & DESIST Y/N
           03  FILLER        PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 1 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBGB/PASSWDW.CPY".
       78  SEQ-01                        VALUE 1.
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CEASRM_DEF.CPY".
       COPY "LIBLP/CEASRM_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CEASRM_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CEASRM"      TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE " " TO ERRCD.

           MOVE SEQ-01                TO PASSWDW-SEQ.
           MOVE "TO RUN THIS PROGRAM" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD-MUST-EXIST.
           IF PASSWDW-STAT NOT = "OK"
              GO TO END-ROUTINE.

       MAIN-MODULE.

           PERFORM FORM-RESET.


           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

      * "C" OR "W" OR "V" OR "E" OR "R"
           IF BW-CEASE-DESIST-TYPE = "C"
             MOVE "CUSTOMER IS COMBINATION CEASE && DESIST" TO MESS
             PERFORM SEND-MESS
           ELSE
             IF BW-CEASE-DESIST-TYPE = "W"
                MOVE "CUSTOMER IS WRITTEN CEASE && DESIST" TO MESS
                PERFORM SEND-MESS
             ELSE
             IF BW-CEASE-DESIST-TYPE = "V"
                MOVE "CUSTOMER IS VERBAL CEASE && DESIST" TO MESS
                PERFORM SEND-MESS
             ELSE
             IF BW-CEASE-DESIST-TYPE = "E"
                MOVE "CUSTOMER IS EMPLOYER CEASE && DESIST" TO MESS
                PERFORM SEND-MESS
              ELSE
             IF BW-CEASE-DESIST-TYPE = "R"
                MOVE "CUSTOMER IS REFERENCE CEASE && DESIST" TO MESS
                PERFORM SEND-MESS
             ELSE
                MOVE "CUSTOMER DOES NOT HAVE CEASE && DESIST" TO MESS
                PERFORM SEND-MESS
                GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "UPD"
              IF REMOVE-CEASE = "Y"
                 PERFORM REMOVE-CEASE-AND-DESIST.

           GO TO MAIN-MODULE.

      ***********************************
      *    RECEIVE BWFILE MODULE
      ***********************************
       REC-BW-MODULE SECTION.
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE "ENTER SSNO #, F6-SCAN, F7-EXIT" TO MESS.
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

           MOVE VDUNUM0 TO BW-SSNO.

       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF NOT IO-GOOD
              GO TO REC-BW-MODULE-NO.

           MOVE BW-SSNO TO HOLD-SSNO.

           PERFORM DISPLAY-FIELDS.

       END-REC-BW-MODULE.
           EXIT.

      /************************************************
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
                 MOVE BW-SSNO TO HOLD-SSNO
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

           IF TYP = 1 PERFORM START-BW1-FILE.
           IF TYP = 2 PERFORM START-BW2-FILE.
           IF TYP = 3 PERFORM START-BW3-FILE.
           IF TYP = 4 PERFORM START-BW4-FILE.
       READ-FILES-NEXT.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF IO-BAD
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
      *    ARRANGE BORROWER KEY
      ***********************************
       ARRANGE-BW-KEY SECTION.
       ARR-BW-KEY.
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 16
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
            ELSE
              MOVE 16 TO IX2.
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
           MOVE "ENTER FIELD #, F1-CANCEL, 'U' TO UPDATE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ACTION = "     U"
              IF NOT (BW-IS-CEASE-DESIST) 
                 MOVE "CLR" TO IO-TYPE 
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "UPD" TO IO-TYPE
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
           IF ELE = 1
              MOVE "F1-CLEAR, ENTER 'Y' OR 'N' TO REMOVE " TO MESS
              PERFORM SEND-LEGEND.

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
           MOVE VDUBUF TO REMOVE-CEASE.
           IF REMOVE-CEASE NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ********************************************
       REMOVE-CEASE-AND-DESIST SECTION.
      ********************************************
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BM1-FILE.

           MOVE HOLD-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-REMOVE-CEASE-AND-DESIST.

           IF BW-CEASE-DESIST-TYPE = "C"
              PERFORM UPDATE-COMBINATION
           ELSE
           IF BW-CEASE-DESIST-TYPE = "E"
              MOVE SPACES  TO BW-EMPLOYER
           ELSE
           IF BW-CEASE-DESIST-TYPE = "R"
              PERFORM UPDATE-BORROWER-REFERENCES
           ELSE
           IF BW-CEASE-DESIST-TYPE = "W"
              MOVE SPACES TO BW-EMPLOYER
              PERFORM UPDATE-BORROWER-REFERENCES
           ELSE
      * NOTHING TO CLEAR FOR A VERBAL BUT THE TYPE
           IF BW-CEASE-DESIST-TYPE = "V"
              NEXT SENTENCE
           ELSE
              GO TO EXIT-REMOVE-CEASE-AND-DESIST.

           PERFORM CREATE-BORROWER-MEMO.

           MOVE 0 TO BW-CEASE-DESIST-DATE.
           MOVE SPACES TO BW-CEASE-DESIST-TYPE.
           PERFORM REWRITE-BW1-FILE.

       EXIT-REMOVE-CEASE-AND-DESIST.
           EXIT.

      *************************************
       UPDATE-COMBINATION SECTION.
      *************************************
      * THE COMBO SHOULD NOT CLEAR THE FIELDS IF THEY DIDNT SAY
      * CEASE & DESIST AS YOU DONT KNOW WHAT COMBO THEY SET.
           MOVE BW-EMPLOYER TO TEST-CEASE-DESIST.
           IF TEST-CEASE-DESIST = "CEASE & DESIST"
              MOVE SPACES TO BW-EMPLOYER.

           PERFORM UPDATE-BORROWER-REFERENCES.


       EXIT-UPDATE-COMBINATION.
           EXIT.

     
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
           MOVE BW-EMPLOYER TO EMP.
           MOVE BW-EMPADDR1 TO EMP1.
           MOVE BW-EMPADDR2 TO EMP2.
           MOVE BW-SOLICIT  TO SOLICIT.
           MOVE BW-CONFIDENTIAL TO CONF.
           MOVE BW-PRIVACY-OPT-OUT TO TEXTOPT.
           MOVE BW-CEASE-DESIST-TYPE TO CEASE.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.
 
      *************************************
       UPDATE-BORROWER-REFERENCES SECTION.
      *************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE BW-CONTACT-DESC(SUB) TO TEST-CEASE-DESIST
              IF TEST-CEASE-DESIST = "CEASE & DESIST"
                 MOVE SPACES TO BW-CONTACT-DESC(SUB)
                 MOVE 0      TO BW-CONTACT-PHONE(SUB)
                                BW-CONTACT-EXT(SUB)
              END-IF
           END-PERFORM.

       EXIT-UPDATE-NEXT-REFERENCE.
           EXIT.

      ***********************************
       CREATE-BORROWER-MEMO SECTION.
      ***********************************

           MOVE EXT-CONAME-USER-ID  TO MEMO-USERID.
           MOVE SPACES              TO MEMO-TYPE.
           IF BW-CEASE-DESIST-TYPE = "C"
              MOVE "REMOVE COMBO CEASE & DESIST " TO MEMO-MSG
           ELSE
           IF BW-CEASE-DESIST-TYPE = "E"
              MOVE "REMOVE EMPLOYER CEASE & DESIST " TO MEMO-MSG
           ELSE
           IF BW-CEASE-DESIST-TYPE = "R"
              MOVE "REMOVE REFERENCE CEASE & DESIST " TO MEMO-MSG
           ELSE
           IF BW-CEASE-DESIST-TYPE = "W"
              MOVE "REMOVE BORROWER CEASE & DESIST " TO MEMO-MSG
              MOVE "(WRITTEN)" TO MEMO-TYPE
           ELSE
           IF BW-CEASE-DESIST-TYPE = "V"
              MOVE "REMOVE BORROWER CEASE & DESIST " TO MEMO-MSG
              MOVE "(VERBAL)" TO MEMO-TYPE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.

           MOVE SPACES TO BM-REC.

           MOVE BW-SSNO  TO BM-SSNO.
           COMPUTE BM-DATE = 99999999 - DATE-YYYYMMDD.

           MOVE SAVE-BM-MEMO TO BM-MEMO.
           MOVE "9"      TO BM-MEMO-TYPE.
 
           MOVE 1 TO BM-SEQ.
       CREATE-BM-NEXT.
           PERFORM WRITE-BM1-FILE.
           IF IO-FG NOT = 0
              IF BM-SEQ NOT = 99
                 ADD 1 TO BM-SEQ
                 GO TO CREATE-BM-NEXT.

       EXIT-CREATE-BORROWER-MEMO.
           EXIT.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE LPLOAN-CONAME TO CEASRM-CONAME.
           MOVE LPLOAN-SYSDATE TO CEASRM-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CEASRM-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CEASRM-FORM-FIELDS TO WR-BUF.
           MOVE CEASRM-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE CEASRM-HELP--FILE TO HELP-LINK-FILE.
           MOVE CEASRM-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CEASRM_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       VERIFY-PASSWORD-MUST-EXIST SECTION.
      **************************************
           MOVE "EX"     TO PASSWDW-STAT.
           MOVE "CEASRM" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/ACCESS.CPY".
       SETUP-CITY-STATE-ZIP.
           MOVE BW-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ",? ".
           INSPECT NAME-WORKER REPLACING FIRST "  " BY
                                            BW-STATE.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY "?  ".
           INSPECT NAME-WORKER REPLACING FIRST "          "
                                            BY BW-ZIP.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
 
      *********************************
      *   FILE I-O ROUTINES
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BM1-FILE.
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CEASRM-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
