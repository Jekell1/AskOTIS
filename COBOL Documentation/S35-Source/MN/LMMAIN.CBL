       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LMMAIN.
      ********************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   LOAN MEMO FILE MAINTENANCE
      * NEW:  06/08/04
      *       NEW MEMO'S CAN NOT BE CREATED FROM HERE, YOU MAY CHANGE
      *       OR DELETE EXISTING MEMO'S ONLY
      * REV:
      * BLF 050906 FIX TO EXIT TO LPM2MU INSTEAD OF LPMNMU
      * MJD 131209 A30 CONVERSION COMPLETED
      * BAH 181128 GLOBAL LMFILE
      *  CS 200110 FIXED SCAN KEY (ACCT = 8, DATE = YYYYMMDD, SEQ 99
      ********************************************************************
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
           "@(#)MN/LMMAIN.CBL S35 02/21/24-12:31:23 >".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD           PIC X        VALUE " ".
       01  MEMO-DESC       PIC X(30)    VALUE SPACES.

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.
    
       01  HOLD-WORK-KEY.
           03  HOLD-WORK-ACCTNO       PIC 9(8).
           03  HOLD-WORK-DATE         PIC 9(8).
           03  HOLD-WORK-SEQ          PIC 9(2).


      * ORIGINAL RECORD - LMOR
      * CHANGED  RECORD - LMCH
           COPY "LIBLP/LP01LM.CPY" REPLACING LEADING "LM" BY "LMOR".
           COPY "LIBLP/LP01LM.CPY" REPLACING LEADING "LM" BY "LMCH".

      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DATE         PIC 9(6).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-SEQ          PIC 9(2).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(7).

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(7).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(1).
           03  F011        PIC X(30).
           03  F02         PIC X(60).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(12) VALUE
           "010 011 020.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 9   VALUE 2.
       01  SPEC-TABLE.
      *1.  MEMO TYPE
           03  FILLER      PIC X(8) VALUE "E  A010 ".

      *2.  MEMO DESC
           03  FILLER      PIC X(8) VALUE "E  A600 ".
       01  SPEC-TABLE2    REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 2 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01 LMMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LMMAIN_DEF.CPY".
       COPY "LIBMN/LMMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/LMMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "LMMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LMMAIN-WINDOW-HANDLE.

           PERFORM INITIALIZATION.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LM-MODULE.
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

      ************************************
       INITIALIZATION SECTION.
      ************************************

           PERFORM OPEN-BR-FILE.
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

      ************************************
       REC-LM-MODULE SECTION.
      ************************************
       REC-LM-ACCTNO.
           MOVE 0 TO NEW-FLAG.
          
           MOVE ZEROS TO   LM-ACCTNO
                           LM-DATE
                           LM-SEQ
           MOVE SPACES TO IO-TYPE.

           MOVE "ENTER ACCOUNT NUMBER" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N080ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LM-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LM-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-LM-ACCTNO.
           MOVE VDUNUM0 TO LM-ACCTNO LN-ACCTNO.
           IF LM-ACCTNO = 0
              GO TO REC-LM-ACCTNO.
       REC-LM-DATE.
           MOVE "ERNM060MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO REC-LM-ACCTNO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LM-MODULE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LM-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-LM-DATE.

           IF VDUNUM0 = ZEROS
              MOVE ZEROS TO LM-DATE
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO DATE-YYYYMMDD
              SUBTRACT DATE-YYYYMMDD FROM 99999999 GIVING LM-DATE.

       REC-LM-SEQ.
           MOVE "EN N020SEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" OR "10"
              GO TO REC-LM-DATE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LM-MODULE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LM-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-LM-SEQ.

           MOVE VDUNUM0 TO LM-SEQ.

       READ-LMFILE.
           PERFORM OPEN-LM1-FILE.
           PERFORM READ-LM1-FILE.
           PERFORM CLOSE-LM1-FILE.
           IF IO-FG = 0
              MOVE LM-REC TO LMOR-REC
              PERFORM GET-LN
              IF IO-FG = 0
                 PERFORM DISPLAY-FIELDS
              ELSE
                 MOVE "LOAN MASTER RECORD NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE
           ELSE
              MOVE "CAN NOT CREATE NEW MEMO'S" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE.

      * DONT ALLOW THE CREATE OF A NEW MEMO FROM THIS MAINTENANCE.

       END-REC-LM-MODULE.
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE "ENTER ACCOUNT #, OR F6 TO SCAN" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO.

           PERFORM START-LM1-FILE.
       ENTER-KEY.
           MOVE "EN N180SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-LMFILE-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LM-REC TO LMOR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0       TO HOLD-WORK-KEY.

           MOVE LM-PATH-OWNBR     TO LM-BRNO.
           MOVE HOLD-WORK-ACCTNO  TO LM-ACCTNO.
           MOVE HOLD-WORK-DATE          TO DATE-YYYYMMDD.
           SUBTRACT DATE-YYYYMMDD FROM 99999999 GIVING LM-DATE
           MOVE HOLD-WORK-SEQ     TO LM-SEQ.

           PERFORM START-LM1-FILE.

       READ-LMFILE-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK LM-FILE.

           MOVE "SN N080ACCTNO." TO VDU.
           MOVE LM-ACCTNO  TO VDUNUM0 LN-ACCTNO.
           PERFORM SCREENIO.

           MOVE "S  A080MEMODATE." TO VDU.

           IF LM-DATE = 0
              MOVE 0 TO VDU-DATE-EDIT
           ELSE
              COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY    TO VDU-DATE-EDIT.

           PERFORM SCREENIO.

           MOVE "SN N020SEQ." TO VDU.
           MOVE LM-SEQ TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-LM1-FILE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.


      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

       CREATE-ROUTINE.

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
           IF CHFG(ELE) = "N"
              GO TO CREATE-ROUTINE.

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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF TO LM-MEMO-TYPE.

           IF NOT ((LM-MEMO-TYPE-AUTOINS)      OR
                  (LM-MEMO-TYPE-BANKRPT)       OR
                  (LM-MEMO-TYPE-COLLECT)       OR
                  (LM-MEMO-TYPE-DEBTAID)       OR
                  (LM-MEMO-TYPE-JD)            OR
                  (LM-MEMO-TYPE-LEGAL)         OR
                  (LM-MEMO-TYPE-NOTES)         OR
                  (LM-MEMO-TYPE-BACKDATE-POFF) OR
                  (LM-MEMO-TYPE-REVERSAL)      OR
                  (LM-MEMO-TYPE-SYSTEM)        OR
                  (LM-MEMO-TYPE-TITLE)         OR
                  (LM-MEMO-TYPE-REPO)          OR
                  (LM-MEMO-TYPE-UPLOAD)        OR
                  (LM-MEMO-TYPE-LOAN))
                  MOVE "INVALID MEMO TYPE" TO MESS
                  PERFORM SEND-MESS
                  GO TO ENTER-ELE.

           PERFORM GET-MEMO-TYPE-DESC.
           MOVE MEMO-DESC TO  F011.
           MOVE "S  A030011." TO VDU.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUBUF TO LM-MEMO.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ************************************************
       GET-MEMO-TYPE-DESC SECTION.
      ************************************************
           IF LM-MEMO-TYPE = "A"
              MOVE "AUTO INSURANCE" TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "B"
              MOVE "BANKRUPT"       TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "C"
              MOVE "COLLECTION"     TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "D"
              MOVE "DEBT AID"       TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "J"
              MOVE "JUDGEMENT"      TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "L"
              MOVE "LEGAL"          TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "N"
              MOVE "NOTES"          TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "P"
              MOVE "BACKDATE PAYOFF" TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "R"
              MOVE "REVERSAL"       TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "S"
              MOVE "SYSTEM"         TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "9"
              MOVE "SYSTEM"         TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "T"
              MOVE "TITLE"          TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "X"
              MOVE "REPO"           TO MEMO-DESC
           ELSE
           IF LM-MEMO-TYPE = "U"
              MOVE "UPLOAD"         TO MEMO-DESC
           ELSE
             MOVE SPACES            TO MEMO-DESC.
       GET-MEMO-TYPE-DESC-EXIT.
           EXIT.

      ********************************************
       WRITE-MODULE SECTION.
      ********************************************
           PERFORM OPEN-LM1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LM-REC TO LMCH-REC.

           IF LMOR-REC = LMCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LM1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LM-REC NOT = LMOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LMCH-REC TO LM-REC.
           PERFORM REWRITE-LM1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-LM1-FILE.

      **********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LM-BRNO   TO WORK-ERRLOG-BR.
           MOVE LM-ACCTNO TO WORK-ERRLOG-NUMBER.
           COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO WORK-ERRLOG-DATE.
           MOVE LM-SEQ      TO WORK-ERRLOG-SEQ.
           MOVE "TO "       TO  WORK-ERRLOG-TO-X.

           IF LMOR-MEMO-TYPE NOT = LMCH-MEMO-TYPE
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE "MEMOTYPE"            TO WORK-ERRLOG-DESC
             MOVE LMOR-MEMO-TYPE        TO WORK-ERRLOG-ORG-ALP
             MOVE LMCH-MEMO-TYPE        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LMOR-MEMO NOT = LMCH-MEMO
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE "MEMO    "            TO WORK-ERRLOG-DESC
             MOVE LMOR-MEMO        TO WORK-ERRLOG-ORG-ALP
             MOVE LMCH-MEMO        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      ************************************
       LOG-FIELD-CHANGE SECTION.
      ************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      **********************************************
       DELETE-MODULE SECTION.
      **********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LM1-FILE.
           PERFORM READ-LM1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LM-REC NOT = LMOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-LM1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING LM1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-LM1-FILE.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE LM-MEMO-TYPE TO F01.
           PERFORM GET-MEMO-TYPE-DESC.
           MOVE MEMO-DESC    TO F011.
           MOVE LM-MEMO      TO F02.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           MOVE EXT-CONAME-CONAME TO LMMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LMMAIN-SYSDATE.
           DISPLAY SPACES UPON LMMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LMMAIN-SCREEN UPON LMMAIN-WINDOW-HANDLE.
           MOVE LMMAIN-FORM-FIELDS TO WR-BUF.
           MOVE LMMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE LMMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LMMAIN-HELP--DIR TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LMMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       INVALID-STATE.
           MOVE " INVALID STATE CODE - PLEASE RE-ENTER!" TO MESS.
           PERFORM SEND-MESS.
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
       SEND-LEGEND.
           PERFORM CALL-TMMSG.

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY LMMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
