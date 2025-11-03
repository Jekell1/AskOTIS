       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FPMAIN.
      ******************************************************************
      *
      *   DESC: FEE PAYABLE MAINTENANCE
      *          LTF RECORD 
      *
      ***** THIS IS A WINDOW MAINTENANCE CALLED FROM THE FIELD FEECODE
      *     IN LOAN FILE MAINTENANCE PAGE 4(LNMAN4) FIELD 14-23. (F3)
      *
      *=================================================================
      * REV:
      * 040108 BAH ADDED FOR WORLD FEE COMMISSIONS ON NON-FILE
      * 141125  CS ADDED EXT-CONAME-SYSDATE TO FPMAIN-SYSDATE IN FORM-RESET
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2017.0519 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LT' (F) FILE PREFIX FOR WORKING STORAGE FIELDS.
      *  CS 181231 REMOVED ENTERING LN-SEQNO, REMOVED SCAN-SEQ
      *
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
           "@(#)MN/FPMAIN.CBL S34 06/03/21-20:54:14 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTF.CPY".
       COPY "LIBLP/LP01LTF_SQL.CPY".
       COPY "LIBLP/LPWSLTF.CPY".
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
       01  HOLD-LTF1-KEY         VALUE SPACES.
           03  HOLD-LTF-BRNO     PIC 9(4).
           03  HOLD-LTF-ACCTNO   PIC 9(8).
           03  HOLD-LTF-SEQNO    PIC 999.

      * SCAN-KEY WILL NOT INCLUDE BRANCH, DIDNT WANT TO CHANGE ENTIRE
      * MAINTENANCE
       01  SCAN-KEY            PIC 9(11).
       01  SCAN-KEYX REDEFINES SCAN-KEY.
           03  SCAN-LTF-ACCTNO   PIC 9(8).
           03  SCAN-LTF-SEQNO    PIC 9(3).

       01  WS-ACCTNO         PIC 9(8).

       01  CHANGES-LTF1-KEY         VALUE SPACES.
           03  CHANGES-LTF-BRNO     PIC 9(4).
           03  CHANGES-LTF-ACCTNO   PIC 9(8).
           03  CHANGES-LTF-SEQNO    PIC 999.

       01  ORIGINAL-LTF-REC  PIC X(LTF-SIZE) VALUE SPACES.
       01  CHANGED-LTF-REC   PIC X(LTF-SIZE) VALUE SPACES.

       01  ERRCD         PIC X.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F010       PIC XX.
           03  F011       PIC ZZZZZ.ZZ-.
           03  F020       PIC XX.
           03  F021       PIC ZZZZZ.ZZ-.
           03  F030       PIC XX.
           03  F031       PIC ZZZZZ.ZZ-.
           03  F040       PIC XX.
           03  F041       PIC ZZZZZ.ZZ-.
           03  F050       PIC XX.
           03  F051       PIC ZZZZZ.ZZ-.
           03  F060       PIC XX.
           03  F061       PIC ZZZZZ.ZZ-.
           03  F070       PIC XX.
           03  F071       PIC ZZZZZ.ZZ-.
           03  F080       PIC XX.
           03  F081       PIC ZZZZZ.ZZ-.
           03  F090       PIC XX.
           03  F091       PIC ZZZZZ.ZZ-.
           03  F100       PIC XX.
           03  F101       PIC ZZZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(32) VALUE
               "010 011 020 021 030 031 040 041 ".
           03  FILLER     PIC X(32) VALUE
               "050 051 060 061 070 071 080 081 ".
           03  FILLER     PIC X(32) VALUE
               "090 091 100 101.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 10.
       01  SPEC-TABLE.
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 10 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/FPMAIN_DEF.CPY".
       COPY "LIBMN/FPMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/FPMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************
           PERFORM SQL-CONNECT.

           MOVE 13 TO WINDOW-LINES.
           MOVE 77 TO WINDOW-COLUMNS.
           MOVE 02 TO WINDOW-BEGIN-Y.
           MOVE 3 TO WINDOW-BEGIN-X.
           MOVE "MN/FPMAIN" TO WINDOW-FORM-NAM.
           MOVE "FPMAIN" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.

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

      ***************************************
       REC-LT-MODULE SECTION.
      ***************************************
       REC-LT-MODULE-NO.
           MOVE 0 TO LTF-BRNO LTF-ACCTNO LTF-SEQNO FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.

           MOVE 0 TO NEW-FLAG.
       REC-LT-NUMBER.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-LT-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-LT-NUMBER.
           MOVE VDUNUM0 TO WS-ACCTNO.

           MOVE WS-ACCTNO TO HOLD-LTF-ACCTNO LTF-ACCTNO.
           IF LTF-ACCTNO = 0
              GO TO REC-LT-NUMBER.
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
           MOVE VDUNUM0 TO LTF-SEQNO HOLD-LTF-SEQNO.

       READ-LTFILE.
           MOVE LTF-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* LOAN NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LT-MODULE-NO.

           MOVE LN-OWNBR TO LTF-BRNO HOLD-LTF-BRNO.

           PERFORM OPEN-LTF1-FILE.
           PERFORM READ-LTF1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

           MOVE LTF-REC TO ORIGINAL-LTF-REC.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-BR-FILE.

       END-REC-LT-MODULE.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LTF1-FILE.
           MOVE LTF-PATH-OWNBR TO LTF-BRNO.

           PERFORM START-LTF1-FILE.
       ENTER-KEY.
           MOVE "EN N120SCANKEY." TO VDU.
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
                 MOVE LTF1-KEY TO HOLD-LTF1-KEY
                 MOVE LTF-REC TO ORIGINAL-LTF-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO SCAN-KEY.

           MOVE LTF-PATH-OWNBR     TO LTF-BRNO.
           MOVE SCAN-LTF-ACCTNO    TO LTF-ACCTNO.
           MOVE SCAN-LTF-SEQNO     TO LTF-SEQNO.

           PERFORM START-LTF1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LTF1-FILE-NEXT.
      *    UNLOCK LTF-FILE.
           IF IO-FG = 9 OR (LTF-BRNO NOT = LTF-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-SCAN-KEY.

           MOVE LTF-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE LTF-ACCTNO TO SCAN-LTF-ACCTNO.
           MOVE "SN N060LNNO." TO VDU.
           MOVE SCAN-LTF-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N030INSEQ." TO VDU.
           MOVE LTF-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LTF1-FILE.

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
           GO TO A-01 A-01 A-01 A-01 A-01
                 A-01 A-01 A-01 A-01 A-01
                 A-01 A-01 A-01 A-01 A-01
                 A-01 A-01 A-01 A-01 A-01
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.


      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO LTF-FEECODE(ELE).
       A-011.
           MOVE "ENNS052  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-011.
           MOVE VDUNUM2 TO LTF-FEECODE-PAYABLE(ELE).
           IF LTF-FEECODE(ELE) = SPACES
              IF LTF-FEECODE-PAYABLE(ELE) NOT = 0
                MOVE "MUST HAVE A FEECODE" TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-LTF1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LTF-REC TO CHANGED-LTF-REC.
           IF ORIGINAL-LTF-REC = CHANGED-LTF-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LTF1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LTF-REC NOT = ORIGINAL-LTF-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-LTF-REC TO LTF-REC.
           PERFORM REWRITE-LTF1-FILE.

      * CODE TO WRITE CHANGE FILE.
           MOVE LTF-BRNO   TO CHANGES-LTF-BRNO.
           MOVE LTF-ACCTNO TO CHANGES-LTF-ACCTNO.
           MOVE LTF-SEQNO  TO CHANGES-LTF-SEQNO.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE CHANGES-LTF1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTF1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LTF1-FILE.

      *************************************
       DELETE-MODULE SECTION.
      *************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LTF1-FILE.
           PERFORM READ-LTF1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LTF-REC NOT = ORIGINAL-LTF-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LTF1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LTF1-FILE.

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE LTF-FEECODE(1) TO F010.
           MOVE LTF-FEECODE-PAYABLE(1) TO F011.
           MOVE LTF-FEECODE(2) TO F020.
           MOVE LTF-FEECODE-PAYABLE(2) TO F021.
           MOVE LTF-FEECODE(3) TO F030.
           MOVE LTF-FEECODE-PAYABLE(3) TO F031.
           MOVE LTF-FEECODE(4) TO F040.
           MOVE LTF-FEECODE-PAYABLE(4) TO F041.
           MOVE LTF-FEECODE(5) TO F050.
           MOVE LTF-FEECODE-PAYABLE(5) TO F051.
           MOVE LTF-FEECODE(6) TO F060.
           MOVE LTF-FEECODE-PAYABLE(6) TO F061.
           MOVE LTF-FEECODE(7) TO F070.
           MOVE LTF-FEECODE-PAYABLE(7) TO F071.
           MOVE LTF-FEECODE(8) TO F080.
           MOVE LTF-FEECODE-PAYABLE(8) TO F081.
           MOVE LTF-FEECODE(9) TO F090.
           MOVE LTF-FEECODE-PAYABLE(9) TO F091.
           MOVE LTF-FEECODE(10) TO F100.
           MOVE LTF-FEECODE-PAYABLE(10) TO F101.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *************************************
       NEW-RECORD-SETUP SECTION.
      *************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000DISP." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTF-FILE.

           MOVE HOLD-LTF-BRNO   TO LTF-BRNO.
           MOVE HOLD-LTF-ACCTNO TO LTF-ACCTNO.
           MOVE HOLD-LTF-SEQNO  TO LTF-SEQNO.

           MOVE 0 TO ELE.
       NEW-RECORD-EXIT.
           EXIT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPLTFCL.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           MOVE EXT-CONAME-SYSDATE TO FPMAIN-SYSDATE.
           DISPLAY FPMAIN-SCREEN.

           MOVE FPMAIN-FORM-FIELDS TO WR-BUF.
           MOVE FPMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE FPMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FPMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/FPMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
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
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTF1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FPMAIN-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
