       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CEASUP.
       DATE-WRITTEN.    081588.
      *******************************************************************
      *                 (LP)
      *  DESCRIPTION:   UPDATE CEASE AND DESIST 
      *                  CALLED FROM CEASMU, CALLED FROM LONPX0 (CHANGES)
      *
      *  BORROWER:
      *    IF VERBAL - ONLY CLEARS CELL PHONE AND HOME PHONE, 
      *                 CEASE TYPE 'V'
      *    IF WRITTEN - CLEARS CELL PHONE, HOME PHONE, SOLICIT 'N',
      *                 CONFIDENT 'Y', TEXTOPT 'S', CEASE TYPE 'W'
      *
      *  EMPLOYER:
      *    REMOVE EMPLOYER NAME, ADR, PHONE, EXT, POS AND REPLACE EMPLOYER NAME
      *    WITH "CEASE & DESIST", CEASE TYPE 'E'
      *
      *  REFERENCE:
      *    REMOVE BORROWER REFERENCES AND REPLACE FIRST ONE WITH
      *      "CEASE & DESIST", CEASE TYPE 'R'
      *
      *  COMBINATION: IF THEY SET MORE THAN ONE TYPE (BESIDES WRITTEN),
      *               CEASE TYPE 'C'
      *
      *  IF WRITTEN, YOU CANT ADD ANYMORE, THAT IS THE HIGHEST
      *  IF CHANGING TO WRITTEN FROM ANYTHING, LEAVE THE TYPE WRITTEN!
      *
      * REV:
      *  2024.1121 BAH NEW CEASE & DESIST UPDATE #1686
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
           "@(#)LP/CEASUP A15 11/12/24-09:48:22 >".

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
       01  NAME-WORKER         PIC X(34).
       01  TODAYS-DATE-YYYYMMDD  PIC 9(8).
       01  SAVE-BM-MEMO.
           03  MEMO-USERID      PIC X(10) VALUE SPACES.
           03  FILLER           PIC X(2)  VALUE "; ".
           03  MEMO-MSG         PIC X(29) VALUE SPACES.
           03  MEMO-TYPE        PIC X(19) VALUE SPACES.


       01  TEST-SSNO           PIC 9(9).
       01  TEST-CEASE-DESIST   PIC X(14).
       01  HOLD-CEASE-MSG      PIC X(30).
       01  HOLD-SSNO           PIC 9(9) VALUE 0.
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL     OCCURS 32 TIMES
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
           03  REFERENCES OCCURS 6 TIMES.
               05  REF       PIC X(40).
               05  PH        PIC X(12).
       01  DISPLAY-LIST.
           03  FILLER        PIC X(42) VALUE
           "BWNO NAME ADDR1 ADDR2 ADDR3 EMP EMP1 EMP2 ".
           03  FILLER        PIC X(42) VALUE
           "SOLICIT CONF TEXT CEASE REF1 PH1 REF2 PH2 ".
           03  FILLER        PIC X(36) VALUE
           "REF3 PH3 REF4 PH4 REF5 PH5 REF6 PH6.".
 
       01  MAX               PIC 99   VALUE 1.
       01  TYPE-OF-CEASE     PIC X    VALUE SPACES.
       01  VERBAL-WRITTEN    PIC X    VALUE SPACES.
       01  WHICH-REFERENCE   PIC 9.
 
      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  SPEC-TABLE.
      *01. VERBAL OR WRITTEN
           03  FILLER        PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 1 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CEASUP_DEF.CPY".
       COPY "LIBLP/CEASUP_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CEASUP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CEASUP"      TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE-YYYYMMDD.

           MOVE " " TO ERRCD.
       MAIN-MODULE.

           MOVE SPACES TO TYPE-OF-CEASE.

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
                PERFORM SEND-MESS.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "UPD"
              IF TYPE-OF-CEASE = "B"
                 PERFORM UPDATE-BORROWER-MODULE
              ELSE
              IF TYPE-OF-CEASE = "E"
                 PERFORM UPDATE-EMPLOYER-MODULE
              ELSE
              IF TYPE-OF-CEASE = "R"
                 PERFORM UPDATE-REFERENCE-MODULE.

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
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF IO-BAD OR (BW-OWNBR NOT = BW-PATH-OWNBR)
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

      D    STOP MESS.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     U"
              IF NOT (TYPE-OF-CEASE = "B" OR "E" OR  "R")
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
              IF BW-CEASE-DESIST-TYPE = "W"
                 MOVE "CUSTOMER IS WRITTEN, CANNOT CHANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE.
           IF ELE = 1
              MOVE "F1-CLEAR, ENTER TYPE " TO MESS
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
           MOVE VDUBUF TO TYPE-OF-CEASE.
           IF TYPE-OF-CEASE NOT = "B" AND 
                             NOT = "E" AND
                              NOT = "R"
              MOVE "ENTER 'B', 'E', OR 'R' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF TYPE-OF-CEASE = "E" AND BW-CEASE-DESIST-TYPE = "E"
              MOVE "ALREADY AN 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
              

      * REFERENCE, NOW ENTER WHICH ONE
           IF TYPE-OF-CEASE = "R"
              GO TO F-012.

      * ONLY ENTER VERBAL OR WRITTEN IF TYPE IS BORROWER
           IF TYPE-OF-CEASE NOT = "B"
              GO TO CREATE-ROUTINE.

      * VERBAL OR WRITTEN
       F-011.
           MOVE "E  A010011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.
           
           MOVE VDUBUF TO VERBAL-WRITTEN.
           IF VERBAL-WRITTEN NOT = "V" AND NOT = "W"
              GO TO F-011.

           IF VERBAL-WRITTEN = "V" AND BW-CEASE-DESIST-TYPE = "V"
              MOVE "ALREADY A 'V'" TO MESS
              PERFORM SEND-MESS
              GO TO F-011.
           IF VERBAL-WRITTEN = "W" AND BW-CEASE-DESIST-TYPE = "W"
              MOVE "ALREADY A 'W'" TO MESS
              PERFORM SEND-MESS
              GO TO F-011.
 
           IF TYPE-OF-CEASE = "B" AND VERBAL-WRITTEN = "V"
              IF BW-CEASE-DESIST-TYPE = "W"
                 MOVE "CANNOT MAKE VERBAL AFTER BEING WRITTEN" TO MESS
                 PERFORM SEND-MESS
                 MOVE "MUST GO REMOVE WRITTEN CEASE FIRST" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-011.
                 
           GO TO CREATE-ROUTINE.

      * WHICH REFERENCE
       F-012.
           MOVE "EN N010012." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-012.
           
           MOVE VDUNUM0 TO WHICH-REFERENCE.
           IF WHICH-REFERENCE < 0 AND > 5
              GO TO F-011.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *************************************
       UPDATE-BORROWER-MODULE SECTION.
      *************************************

           MOVE SPACES TO HOLD-CEASE-MSG.
           STRING "CEASE & DESIST " EXT-CONAME-SYSDATE DELIMITED BY "  " 
                INTO HOLD-CEASE-MSG.

           PERFORM OPEN-BW1-FILE.

           MOVE HOLD-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO END-UPDATE-BORROWER-MODULE.

           MOVE TODAYS-DATE-YYYYMMDD TO BW-CEASE-DESIST-DATE.
           MOVE 0 TO BW-CELL-PHONE BW-PHONE.
           IF VERBAL-WRITTEN = "V" 
              IF BW-CEASE-DESIST-TYPE NOT = SPACES AND
                 BW-CEASE-DESIST-TYPE NOT = "V"
                 MOVE "C"  TO BW-CEASE-DESIST-TYPE
              ELSE
                 MOVE "V"  TO BW-CEASE-DESIST-TYPE
           ELSE
           IF VERBAL-WRITTEN = "W"
              MOVE HOLD-CEASE-MSG TO BW-EMPLOYER
              MOVE SPACES         TO BW-EMPADDR1 BW-EMPADDR2 BW-EMPLPOS
              MOVE 0              TO BW-EMPPHONE BW-EMPEXT
              MOVE "N"            TO BW-SOLICIT
              MOVE "Y"            TO BW-CONFIDENTIAL
              MOVE "S"            TO BW-PRIVACY-OPT-OUT
      * WHEN CHANGING TO WRITTEN, EITHER FIRST TIME OR AFTER ANY OTHER 
      * TYPE, THEY WANT THE FLAG TO BE WRITTEN, NOT A COMBO
              MOVE "W"            TO BW-CEASE-DESIST-TYPE.

           IF VERBAL-WRITTEN = "W"
              PERFORM UPDATE-ALL-BORROWER-REFERENCES.

           PERFORM REWRITE-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM CREATE-BORROWER-MEMO.

       END-UPDATE-BORROWER-MODULE.
           EXIT.

      *************************************
       UPDATE-EMPLOYER-MODULE SECTION.
      *************************************

           MOVE SPACES TO HOLD-CEASE-MSG.
           STRING "CEASE & DESIST " EXT-CONAME-SYSDATE DELIMITED BY "  " 
                INTO HOLD-CEASE-MSG.

           PERFORM OPEN-BW1-FILE.

           MOVE HOLD-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO END-UPDATE-EMPLOYER-MODULE.

           MOVE HOLD-CEASE-MSG TO BW-EMPLOYER.
           MOVE SPACES TO BW-EMPADDR1 BW-EMPADDR2 BW-EMPLPOS.
           MOVE 0 TO BW-EMPPHONE BW-EMPEXT.
           MOVE TODAYS-DATE-YYYYMMDD TO BW-CEASE-DESIST-DATE.
           IF BW-CEASE-DESIST-TYPE NOT = SPACES AND
               BW-CEASE-DESIST-TYPE NOT = "E"
              MOVE "C"         TO BW-CEASE-DESIST-TYPE
           ELSE
              MOVE "E"         TO BW-CEASE-DESIST-TYPE.

           PERFORM REWRITE-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM CREATE-BORROWER-MEMO.

       END-UPDATE-EMPLOYER-MODULE.
           EXIT.

      *************************************
       UPDATE-REFERENCE-MODULE SECTION.
      *************************************

           MOVE SPACES TO HOLD-CEASE-MSG.
           STRING "CEASE & DESIST " EXT-CONAME-SYSDATE DELIMITED BY "  " 
                INTO HOLD-CEASE-MSG.

           PERFORM OPEN-BW1-FILE.
           MOVE HOLD-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO END-UPDATE-REFERENCE-MODULE.

           MOVE TODAYS-DATE-YYYYMMDD TO BW-CEASE-DESIST-DATE.
           IF BW-CEASE-DESIST-TYPE NOT = SPACES AND
               BW-CEASE-DESIST-TYPE NOT = "R"
              MOVE "C"         TO BW-CEASE-DESIST-TYPE
           ELSE
              MOVE "R"         TO BW-CEASE-DESIST-TYPE.

           MOVE HOLD-CEASE-MSG TO BW-CONTACT-DESC(WHICH-REFERENCE).
           MOVE 0      TO BW-CONTACT-PHONE(WHICH-REFERENCE)
                          BW-CONTACT-EXT(WHICH-REFERENCE).
          
           PERFORM REWRITE-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM CREATE-BORROWER-MEMO.

       END-UPDATE-REFERENCE-MODULE.
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

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE BW-CONTACT-DESC(SUB) TO REF(SUB)
              IF BW-CONTACT-PHONE(SUB) = 0
                 MOVE SPACES TO PH(SUB)
              ELSE
                 MOVE BW-CONTACT-PHONE(SUB) TO PH(SUB)
              END-IF
           END-PERFORM.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.
 
      *************************************
       UPDATE-ALL-BORROWER-REFERENCES SECTION.
      *************************************

      * ONLY PUT CEASE & DESIST IN FIRST REFERENCE AND CLEAR
      * THE OTHER 5

           MOVE HOLD-CEASE-MSG TO BW-CONTACT-DESC(1).
           MOVE 0      TO BW-CONTACT-PHONE(1)
                          BW-CONTACT-EXT(1).

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > 6
              MOVE SPACES TO BW-CONTACT-DESC(SUB)
              MOVE 0      TO BW-CONTACT-PHONE(SUB)
                             BW-CONTACT-EXT(SUB)
           END-PERFORM.
          
       EXIT-UPDATE-ALL-REFERENCE.
           EXIT.


      ***********************************
       CREATE-BORROWER-MEMO SECTION.
      ***********************************
           PERFORM OPEN-BM1-FILE.

           MOVE EXT-CONAME-USER-ID   TO MEMO-USERID.
           MOVE SPACES   TO MEMO-TYPE.
           IF TYPE-OF-CEASE = "E"
              MOVE "SET EMPLOYER CEASE & DESIST " TO MEMO-MSG
           ELSE
           IF TYPE-OF-CEASE = "R"
              MOVE "SET REFERENCE CEASE & DESIST " TO MEMO-MSG
           ELSE
              MOVE "SET BORROWER CEASE & DESIST " TO MEMO-MSG
              IF VERBAL-WRITTEN = "W"
                  MOVE "(WRITTEN)" TO MEMO-TYPE
              ELSE
                  MOVE "(VERBAL)" TO MEMO-TYPE.

           MOVE SPACES TO BM-REC.

           MOVE BW-SSNO  TO BM-SSNO.
           COMPUTE BM-DATE = 99999999 - TODAYS-DATE-YYYYMMDD.

           MOVE SAVE-BM-MEMO TO BM-MEMO.
           MOVE "9"      TO BM-MEMO-TYPE.
 
           MOVE 1 TO BM-SEQ.
       CREATE-BM-NEXT.
           PERFORM WRITE-BM1-FILE.
           IF IO-FG NOT = 0
              IF BM-SEQ NOT = 99
                 ADD 1 TO BM-SEQ
                 GO TO CREATE-BM-NEXT.

           PERFORM CLOSE-BM1-FILE.

       EXIT-CREATE-BORROWER-MEMO.
           EXIT.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE LPLOAN-CONAME TO CEASUP-CONAME.
           MOVE LPLOAN-SYSDATE TO CEASUP-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CEASUP-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CEASUP-FORM-FIELDS TO WR-BUF.
           MOVE CEASUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE CEASUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE CEASUP-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CEASUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

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
       IO-ROUTINE SECTION.
      *********************************
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

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CEASUP-SCREEN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
