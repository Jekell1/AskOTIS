       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DBMAIN.
      ******************************************************************
      *
      *   DESC:  DEBIT CARD PROCEEDS TRAILER MAINTENANCE
      *          LTFILE "D"
      *
      * REV:
      *  BAH 20180221 NEW, LT-D DEBIT CARD PROCEEDS NETSPEND WORLD #1306
      *  BAH 181128 GLOBAL LMFILE
      *   CS 181221 GLOBAL LTDFILE
      *  BAH 20190516 D-LOANDATE WAS NOT DISPLAYING, LIST WAS MISSING
      *               FIELD NAME "LOANDATE". QA0022
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
           "@(#)MN/DBMAIN.CBL S35 02/21/24-12:29:31 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  HOLD-LTD1-KEY          VALUE SPACES.
           03  HOLD-LTD-BRNO     PIC 9(4).
           03  HOLD-LTD-ACCTNO   PIC 9(8).
           03  HOLD-LTD-SEQNO    PIC 999.
       01  ERRCD         PIC X.

      * SCAN-KEY WILL NOT INCLUDE BRANCH, DIDNT WANT TO CHANGE ENTIRE
      * MAINTENANCE
       01  SCAN-KEY            PIC 9(11).
       01  SCAN-KEYX REDEFINES SCAN-KEY.
           03  SCAN-LTD-ACCTNO  PIC 9(8).
           03  SCAN-LTD-SEQNO   PIC 9(3).

       01  CHANGES-LTD1-KEY          VALUE SPACES.
           03  CHANGES-LTD-BRNO     PIC 9(4).
           03  CHANGES-LTD-ACCTNO   PIC 9(8).
           03  CHANGES-LTD-SEQNO    PIC 999.

      * ORIGINAL REC
       COPY "LIBLP/LP01LTD.CPY" REPLACING LEADING "LTD" BY "OLTD".
      * CHANGED REC
       COPY "LIBLP/LP01LTD.CPY" REPLACING LEADING "LTD" BY "CLTD".

       01  SAVE-LM1-KEY.
           03  SAVE-LM-BRNO            PIC 9(4).
           03  SAVE-LM-ACCTNO          PIC 9(8).
           03  SAVE-LM-DATE            PIC 9(8).
           03  SAVE-LM-SEQ             PIC 9(3).

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(4).
           03  FILLER                  PIC X(1).
           03  LMC-NUMBER              PIC Z(6).
           03  FILLER                  PIC X(1).
           03  LMC-FIELD               PIC X(3).
           03  FILLER                  PIC X(1).
           03  LMC-DESC                PIC X(8).
           03  FILLER                  PIC X(1).

           03  LMC-ORG-TEXT            PIC X(13).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC Z,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(13).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC Z,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.

       01  MEMO-AREA.
           03  FILLER             PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESC          PIC X(20).
           03  FILLER             PIC X     VALUE " ".
           03  MEMO-ORG-CHAR      PIC X(24).
           03  FILLER             REDEFINES MEMO-ORG-CHAR.
               05  MEMO-ORG-2DEC  PIC Z,ZZZ,ZZZ,ZZ9.99.
               05  FILLER         PIC X(08).
           03  FILLER             REDEFINES MEMO-ORG-CHAR.
               05  MEMO-ORG-DATE  PIC 99/99/99.
               05  FILLER         PIC X(16).
           03  FILLER             REDEFINES MEMO-ORG-CHAR.
               05  MEMO-ORG-NODEC PIC ZZZZZZZZZZZZ9.
               05  FILLER         PIC X(11).
           03  FILLER             PIC X     VALUE " ".
           03  MEMO-USERID        PIC X(10).


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC Z(10).
           03  F02        PIC Z(7).99-.
           03  F03        PIC ZZ/ZZ/ZZ.
           03  F04        PIC ZZ/ZZ/ZZ.
           03  F05        PIC Z(7).99-.
           03  D-LOANDATE PIC 99/99/99.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 LOANDATE.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 05.
       01  SPEC-TABLE.
      * 1. LTD-DEBIT-CARD-NUMBER
           03  FILLER     PIC X(8) VALUE "ENNN100N".
      * 2. LTD-DEBIT-CARD-AMT
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 3. LTD-VOID-DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 4. LTD-CLEARED-DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 5. LTD-CLEARED-AMT
           03  FILLER     PIC X(8) VALUE "ENNS072N".


       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 05 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       01 DBMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DBMAIN_DEF.CPY".
       COPY "LIBMN/DBMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/DBMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************************
       MAIN-PROGRAM SECTION.
      ************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "DBMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DBMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LT-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************
       REC-LT-MODULE SECTION.
      ******************************************
       REC-LT-MODULE-NO.

           MOVE 0 TO LTD-BRNO LTD-ACCTNO LTD-SEQNO FIRST-SCAN.

           MOVE SPACES TO IO-TYPE.

           MOVE 0 TO NEW-FLAG.
       REC-LT-NUMBER.
           MOVE "EN N080LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LT-NUMBER.

           MOVE VDUNUM0 TO HOLD-LTD-ACCTNO LTD-ACCTNO.

       REC-LT-INSEQ.
           MOVE "EN N030INSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-LT-NUMBER.
           IF VDUSTATUS = "1U"
              GO TO REC-LT-NUMBER.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-LT-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-LT-INSEQ.
           IF VDUNUM0 = 0
              GO TO REC-LT-INSEQ.

           MOVE VDUNUM0 TO LTD-SEQNO HOLD-LTD-SEQNO.

       READ-LTFILE.
           MOVE LTD-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* LOAN NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LT-MODULE-NO.

           MOVE LN-OWNBR TO LTD-BRNO HOLD-LTD-BRNO.

           PERFORM OPEN-LTD1-FILE.
           PERFORM READ-LTD1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           IF IO-FG = 0
              MOVE LTD-REC TO OLTD-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       END-REC-LT-MODULE.
           EXIT.

      *****************************************
       ENTER-SCANKEY SECTION.
      *****************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LTD1-FILE.

           MOVE LTD-PATH-OWNBR TO LTD-BRNO.


           PERFORM START-LTD1-FILE.
       ENTER-KEY.
           MOVE "EN N110SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LTD1-KEY TO HOLD-LTD1-KEY
                 MOVE LTD-REC TO OLTD-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO SCAN-KEY.

           MOVE LTD-PATH-OWNBR     TO LTD-BRNO.
           MOVE SCAN-LTD-ACCTNO    TO LTD-ACCTNO.
           MOVE SCAN-LTD-SEQNO     TO LTD-SEQNO.


           PERFORM START-LTD1-FILE.

       READ-FILES-NEXT.
           PERFORM READ-LTD1-FILE-NEXT.
      *    UNLOCK LTD-FILE.
           IF IO-FG = 9 OR (LTD-BRNO NOT = LTD-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-SCAN-KEY.

           MOVE LTD-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N080LNNO." TO VDU.
           MOVE LTD-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N030INSEQ." TO VDU.
           MOVE LTD-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LTD1-FILE.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
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
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
                   IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.


      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO LTD-DEBIT-CARD-NUMBER.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO LTD-DEBIT-CARD-AMT.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDU-DATE-YYYYMMDD TO LTD-VOID-DATE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDU-DATE-YYYYMMDD TO LTD-CLEARED-DATE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LTD-CLEARED-AMT.
           GO TO CREATE-ROUTINE.
       EXIT-ENTRY-MODULE.
           EXIT.

      *********************************************
       WRITE-MODULE SECTION.
      *********************************************
           PERFORM OPEN-LTD1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LTD-REC TO CLTD-REC.
           IF OLTD-REC = CLTD-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LTD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LTD-REC NOT = OLTD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CLTD-REC TO LTD-REC.
           PERFORM REWRITE-LTD1-FILE.

      * CODE TO WRITE CHANGE FILE.
           MOVE LTD-BRNO   TO CHANGES-LTD-BRNO.
           MOVE LTD-ACCTNO TO CHANGES-LTD-ACCTNO.
           MOVE LTD-SEQNO  TO CHANGES-LTD-SEQNO.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE CHANGES-LTD1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTD1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LTD1-FILE.

      *****************************************
       DELETE-MODULE SECTION.
      *****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LTD1-FILE.
           PERFORM READ-LTD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LTD-REC NOT = OLTD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LTD1-FILE.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE LN-ACCTNO                  TO LMC-NUMBER.
           MOVE "DELETED"                  TO LMC-DESC.
           MOVE "DEBIT CARD LT"            TO LMC-ORG-TEXT.
           MOVE "DELETED DEBIT CARD"       TO MEMO-DESC.

           PERFORM OPEN-LM1-FILE.
           PERFORM LOG-FIELD-CHANGE.
           PERFORM CLOSE-LM1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LTD1-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************
           PERFORM OPEN-LM1-FILE.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE LTD-BRNO                   TO LMC-BRNO.
           MOVE LTD-ACCTNO                 TO LMC-NUMBER.
           MOVE " TO "                     TO LMC-TO-X.

           IF OLTD-DEBIT-CARD-NUMBER NOT = CLTD-DEBIT-CARD-NUMBER
             MOVE "01 "                   TO LMC-FIELD
             MOVE "DEBIT CARD NUMBER   "  TO MEMO-DESC
             MOVE "DCARD # "              TO LMC-DESC
             MOVE OLTD-DEBIT-CARD-NUMBER  TO LMC-ORG-NUM
                                             MEMO-ORG-NODEC
             MOVE CLTD-DEBIT-CARD-NUMBER  TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF OLTD-DEBIT-CARD-AMT NOT = CLTD-DEBIT-CARD-AMT
             MOVE "02 "                   TO LMC-FIELD
             MOVE "DEBIT CARD AMOUNT   "  TO MEMO-DESC
             MOVE "DCARDAMT"              TO LMC-DESC
             MOVE OLTD-DEBIT-CARD-AMT     TO LMC-ORG-AMT
                                             MEMO-ORG-2DEC  
             MOVE CLTD-DEBIT-CARD-AMT     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.
           IF OLTD-VOID-DATE NOT = CLTD-VOID-DATE
             MOVE "03 "                   TO LMC-FIELD
             MOVE "DEBIT CARD VOID DATE"  TO MEMO-DESC
             MOVE "VOIDDATE"              TO LMC-DESC
             MOVE OLTD-VOID-DATE          TO LMC-ORG-DATE
                                             MEMO-ORG-DATE 
             MOVE CLTD-VOID-DATE          TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.
           IF OLTD-CLEARED-DATE NOT = CLTD-CLEARED-DATE
             MOVE "04 "                   TO LMC-FIELD
             MOVE "DEBIT CLEARED DATE  "  TO MEMO-DESC
             MOVE "CLEARED "              TO LMC-DESC
             MOVE OLTD-CLEARED-DATE       TO LMC-ORG-DATE
                                             MEMO-ORG-DATE 
             MOVE CLTD-CLEARED-DATE       TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.
           IF OLTD-CLEARED-AMT NOT = CLTD-CLEARED-AMT
             MOVE "05 "                   TO LMC-FIELD
             MOVE "DEBIT CLEARED AMOUNT"  TO MEMO-DESC
             MOVE "CLEARAMT"              TO LMC-DESC
             MOVE OLTD-CLEARED-AMT        TO LMC-ORG-AMT
                                             MEMO-ORG-2DEC  
             MOVE CLTD-CLEARED-AMT        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           PERFORM CLOSE-LM1-FILE.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           PERFORM UPDATE-LM-FILE.

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
 
       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ****************************************
       UPDATE-LM-FILE SECTION.
      ****************************************

           MOVE LM-PATH-OWNBR      TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO          TO SAVE-LM-ACCTNO.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           COMPUTE SAVE-LM-DATE = 99999999 - DATE-YYYYMMDD.
           MOVE 1                  TO SAVE-LM-SEQ.
       UPDATE-LM-FILE-AGAIN.
           MOVE SPACES       TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.

           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.
           MOVE MEMO-DESC          TO MEMO-DESC.
           MOVE MEMO-AREA          TO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-LM-FILE-AGAIN.

       UPDATE-LM-FILE-EXIT.
           EXIT.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
           MOVE LTD-DEBIT-CARD-NUMBER TO F01.
           MOVE LTD-DEBIT-CARD-AMT    TO F02.
           MOVE LTD-VOID-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F03.
           MOVE LTD-CLEARED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F04.
           MOVE LTD-CLEARED-AMT       TO F05.
           MOVE LTD-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-LOANDATE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *****************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000DISP." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTD-FILE.

           MOVE HOLD-LTD-BRNO   TO LTD-BRNO.
           MOVE HOLD-LTD-ACCTNO TO LTD-ACCTNO.
           MOVE HOLD-LTD-SEQNO  TO LTD-SEQNO.

           MOVE 0 TO ELE.
       NEW-RECORD-EXIT.
           EXIT.

      ************************************************************
       FORM-RESET SECTION.
      ************************************************************
           MOVE EXT-CONAME-CONAME TO DBMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DBMAIN-SYSDATE.
           DISPLAY SPACES UPON DBMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DBMAIN-SCREEN UPON DBMAIN-WINDOW-HANDLE.
           MOVE DBMAIN-FORM-FIELDS TO WR-BUF. 
           MOVE DBMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE DBMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE DBMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DBMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLTDCL.CPY".

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLTD1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM3MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY DBMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
