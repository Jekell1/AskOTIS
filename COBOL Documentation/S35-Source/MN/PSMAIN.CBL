       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PSMAIN.
      ******************************************************************
      *
      *   DESC:  PAYMENT SCHEDULE MAINTENANCE
      *
      *=================================================================
      * REV:
      * JTG 061997 PUT UNLOCK LT-FILE AFTER READ-NEXT IN SCAN
      * MJD 112597 EXPANDED LT-REC TO 144 BYTES (ADDED 30)
      * BAH 030127 ADDED CHANGES LOG FOR ANY CHANGES...
      * MJD 131120 A30 CONVERSION COMPLETED
      * BAH 160115 CHANGED LT TO LTP, PD0003
      *  CS 161216 REMOVED ENTRY OF LN-SEQ
      * KEC 2017.0522 ADDED 'LT' (P) FILE PREFIX FOR WORKING STORAGE FIELDS.
      *  CS 190111 GLOBAL LTP 
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
           "@(#)MN/PSMAIN.CBL S35 02/21/24-12:55:15 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  ERRCD           PIC X VALUE " ".
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  HOLD-LTP1-KEY          VALUE SPACES.
           03  HOLD-LTP-BRNO     PIC 9(4).
           03  HOLD-LTP-ACCTNO   PIC 9(8).
           03  HOLD-LTP-SEQNO    PIC 999.

      * SCAN-KEY WILL NOT INCLUDE BRANCH, DIDNT WANT TO CHANGE ENTIRE
      * MAINTENANCE
       01  SCAN-KEY                PIC 9(11).
       01  SCAN-KEYX REDEFINES SCAN-KEY.
           03  SCAN-LTP-ACCTNO     PIC 9(8).
           03  SCAN-LTP-ACCTNOX REDEFINES SCAN-LTP-ACCTNO.
               05  SCAN-NUMBER     PIC 9(6).
               05  SCAN-SEQNO      PIC 99.
           03  SCAN-LTP-SEQNO      PIC 9(3).

      *=================================================================
      *   LTPCH-REC  [CHANGED-REC]
      *   LTPOR-REC  [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01LTP.CPY" REPLACING LEADING "LTP" BY "LTPCH".
       COPY "LIBLP/LP01LTP.CPY" REPLACING LEADING "LTP" BY "LTPOR".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-SEQNO        PIC 99.
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(11).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(10).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(6).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(10).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(6).99-.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F010       PIC ZZZ.
           03  F011       PIC ZZZZZZ.ZZ.
           03  F020       PIC ZZZ.
           03  F021       PIC ZZZZZZ.ZZ.
           03  F030       PIC ZZZ.
           03  F031       PIC ZZZZZZ.ZZ.
           03  F040       PIC ZZZ.
           03  F041       PIC ZZZZZZ.ZZ.
           03  F050       PIC ZZZ.
           03  F051       PIC ZZZZZZ.ZZ.
           03  F060       PIC ZZZ.
           03  F061       PIC ZZZZZZ.ZZ.
           03  F070       PIC ZZZ.
           03  F071       PIC ZZZZZZ.ZZ.
           03  F080       PIC ZZZ.
           03  F081       PIC ZZZZZZ.ZZ.
           03  F090       PIC ZZZ.
           03  F091       PIC ZZZZZZ.ZZ.
           03  F100       PIC ZZZ.
           03  F101       PIC ZZZZZZ.ZZ.
           03  F110       PIC ZZZ.
           03  F111       PIC ZZZZZZ.ZZ.
           03  F120       PIC ZZZ.
           03  F121       PIC ZZZZZZ.ZZ.
           03  F130       PIC ZZZ.
           03  F131       PIC ZZZZZZ.ZZ.
           03  F140       PIC ZZZ.
           03  F141       PIC ZZZZZZ.ZZ.
           03  F150       PIC ZZZ.
           03  F151       PIC ZZZZZZ.ZZ.
           03  F160       PIC ZZZ.
           03  F161       PIC ZZZZZZ.ZZ.
           03  F170       PIC ZZZ.
           03  F171       PIC ZZZZZZ.ZZ.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(32) VALUE
               "010 011 020 021 030 031 040 041 ".
           03  FILLER     PIC X(32) VALUE
               "050 051 060 061 070 071 080 081 ".
           03  FILLER     PIC X(32) VALUE
               "090 091 100 101 110 111 120 121 ".
           03  FILLER     PIC X(32) VALUE
               "130 131 140 141 150 151 160 161 ".
           03  FILLER     PIC X(8) VALUE
               "170 171.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 17.
       01  SPEC-TABLE.
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 17 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 PSMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PSMAIN_DEF.CPY".
       COPY "LIBMN/PSMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/PSMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
      *          MAIN PROGRAM MODULE
      ******************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "PSMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS PSMAIN-WINDOW-HANDLE.

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

           MOVE 0 TO LTP-BRNO LTP-ACCTNO LTP-SEQNO FIRST-SCAN.
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
           MOVE VDUNUM0 TO HOLD-LTP-ACCTNO LTP-ACCTNO.

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
           MOVE VDUNUM0 TO LTP-SEQNO HOLD-LTP-SEQNO.

       READ-LTFILE.
           MOVE LTP-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* LOAN NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LT-MODULE-NO.

           MOVE LN-OWNBR TO LTP-BRNO HOLD-LTP-BRNO.

           PERFORM OPEN-LTP1-FILE.
           PERFORM READ-LTP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM SEARCH-FOR-ACTIVE-INS
              IF ERRCD = "X"
                 MOVE "PAYMENT SCHEDULE ALREADY EXISTS FOR THIS LOAN"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-LT-MODULE-NO
              ELSE
                 PERFORM NEW-RECORD-SETUP.

           MOVE LTP-REC TO LTPOR-REC.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       END-REC-LT-MODULE.
           EXIT.

      **********************************************
       ENTER-SCANKEY SECTION.
      **********************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LTP1-FILE.
           MOVE LTP-PATH-OWNBR TO LTP-BRNO.


           PERFORM START-LTP1-FILE.

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
                 MOVE LTP1-KEY TO HOLD-LTP1-KEY
                 MOVE LTP-REC TO LTPOR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO SCAN-KEY.

           MOVE SCAN-LTP-ACCTNO TO LTP-ACCTNO.
           MOVE SCAN-LTP-SEQNO  TO LTP-SEQNO.

           PERFORM START-LTP1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LTP1-FILE-NEXT.
      *    UNLOCK LTP-FILE.
           IF IO-FG = 9 OR (LTP-BRNO NOT = LTP-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-SCAN-KEY.

           MOVE LTP-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE LTP-ACCTNO TO SCAN-NUMBER.   
           MOVE "SN N060LNNO." TO VDU.
           MOVE SCAN-NUMBER TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N030INSEQ." TO VDU.
           MOVE LTP-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LTP1-FILE.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
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
           IF ELE > 1 AND LTP-SCHLD-TERM(ELE - 1) = 0
              SUBTRACT 1 FROM ELE
              GO TO A-01.
           MOVE VDUNUM0 TO LTP-SCHLD-TERM(ELE).
           PERFORM DISPLAY-FIELDS.
           IF (VDUBUF = "0" OR SPACE) OR VDUNUM0 = 0
              MOVE "SNNN062  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTP-SCHLD-PAYMNT(ELE) VDUNUM2
              PERFORM SCREENIO
              PERFORM CONSOLIDATE-SCHEDULE
              MOVE 17 TO ELE
              GO TO CREATE-ROUTINE.
       A-011.
           MOVE "ENNN062  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-011.
           MOVE VDUNUM2 TO LTP-SCHLD-PAYMNT(ELE).
           GO TO CREATE-ROUTINE.
       EXIT-ENTRY-MODULE.
           EXIT.

      *********************************************
       WRITE-MODULE SECTION.
      *********************************************
           PERFORM OPEN-LTP1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LTP-REC TO LTPCH-REC.
           IF LTPOR-REC = LTPCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LTP-REC NOT = LTPOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LTPCH-REC TO LTP-REC.
           PERFORM REWRITE-LTP1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LTP1-FILE.

      **********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **********************************************

           MOVE SPACES     TO WORK-ERRLOG-DETAIL.
           MOVE LTP-BRNO   TO WORK-ERRLOG-BR.
           MOVE LTP-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE LTP-SEQNO  TO WORK-ERRLOG-SEQNO.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 17
              IF LTPOR-SCHLD-TERM(SUB) NOT = LTPCH-SCHLD-TERM(SUB)
                MOVE SUB                   TO WORK-ERRLOG-FIELD
                MOVE "TERM"                TO WORK-ERRLOG-DESC
                MOVE LTPOR-SCHLD-TERM(SUB) TO WORK-ERRLOG-ORG-N0
                MOVE LTPCH-SCHLD-TERM(SUB) TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE
              END-IF
              IF LTPOR-SCHLD-PAYMNT(SUB) NOT = LTPCH-SCHLD-PAYMNT(SUB)
                MOVE SUB                   TO WORK-ERRLOG-FIELD
                MOVE "PAYMNT"               TO WORK-ERRLOG-DESC
                MOVE LTPOR-SCHLD-PAYMNT(SUB) TO WORK-ERRLOG-ORG-N2
                MOVE LTPCH-SCHLD-PAYMNT(SUB) TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.


      *******************************************
       LOG-FIELD-CHANGE SECTION.
      *******************************************
      * CODE TO WRITE CHANGE FILE.
           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ****************************************
       DELETE-MODULE SECTION.
      ****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LTP1-FILE.
           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LTP-REC NOT = LTPOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LTP1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING LTP1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-LTP1-FILE.

      **********************************************
       DISPLAY-FIELDS SECTION.
      **********************************************
           MOVE LTP-SCHLD-TERM(1) TO F010.
           MOVE LTP-SCHLD-PAYMNT(1) TO F011.
           MOVE LTP-SCHLD-TERM(2) TO F020.
           MOVE LTP-SCHLD-PAYMNT(2) TO F021.
           MOVE LTP-SCHLD-TERM(3) TO F030.
           MOVE LTP-SCHLD-PAYMNT(3) TO F031.
           MOVE LTP-SCHLD-TERM(4) TO F040.
           MOVE LTP-SCHLD-PAYMNT(4) TO F041.
           MOVE LTP-SCHLD-TERM(5) TO F050.
           MOVE LTP-SCHLD-PAYMNT(5) TO F051.
           MOVE LTP-SCHLD-TERM(6) TO F060.
           MOVE LTP-SCHLD-PAYMNT(6) TO F061.
           MOVE LTP-SCHLD-TERM(7) TO F070.
           MOVE LTP-SCHLD-PAYMNT(7) TO F071.
           MOVE LTP-SCHLD-TERM(8) TO F080.
           MOVE LTP-SCHLD-PAYMNT(8) TO F081.
           MOVE LTP-SCHLD-TERM(9) TO F090.
           MOVE LTP-SCHLD-PAYMNT(9) TO F091.
           MOVE LTP-SCHLD-TERM(10) TO F100.
           MOVE LTP-SCHLD-PAYMNT(10) TO F101.
           MOVE LTP-SCHLD-TERM(11) TO F110.
           MOVE LTP-SCHLD-PAYMNT(11) TO F111.
           MOVE LTP-SCHLD-TERM(12) TO F120.
           MOVE LTP-SCHLD-PAYMNT(12) TO F121.
           MOVE LTP-SCHLD-TERM(13) TO F130.
           MOVE LTP-SCHLD-PAYMNT(13) TO F131.
           MOVE LTP-SCHLD-TERM(14) TO F140.
           MOVE LTP-SCHLD-PAYMNT(14) TO F141.
           MOVE LTP-SCHLD-TERM(15) TO F150.
           MOVE LTP-SCHLD-PAYMNT(15) TO F151.
           MOVE LTP-SCHLD-TERM(16) TO F160.
           MOVE LTP-SCHLD-PAYMNT(16) TO F161.
           MOVE LTP-SCHLD-TERM(17) TO F170.
           MOVE LTP-SCHLD-PAYMNT(17) TO F171.

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

           PERFORM CLEAR-LTP-FILE.

           MOVE HOLD-LTP-BRNO   TO LTP-BRNO.
           MOVE HOLD-LTP-ACCTNO TO LTP-ACCTNO.
           MOVE HOLD-LTP-SEQNO  TO LTP-SEQNO.

           MOVE 0 TO ELE.
       NEW-RECORD-EXIT.
           EXIT.

      *******************************************
       SEARCH-FOR-ACTIVE-INS SECTION.
      *******************************************
           MOVE LTP-REC TO LTPCH-REC.
           MOVE SPACE TO ERRCD.

           MOVE 0 TO LTP-SEQNO.
           PERFORM OPEN-LTP1-FILE.
           PERFORM START-LTP1-FILE.
       NEXT-LT.
           PERFORM READ-LTP1-FILE-NEXT.
      *    UNLOCK LTP-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-SEARCH.

           IF (LTP-BRNO NOT = LTP-PATH-OWNBR) OR
               (LTP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-SEARCH.

           IF HOLD-LTP-BRNO = LTP-BRNO AND
                HOLD-LTP-ACCTNO = LTP-ACCTNO AND
               HOLD-LTP-SEQNO = LTP-SEQNO
              GO TO NEXT-LT.

           MOVE "X" TO ERRCD.
           GO TO EXIT-SEARCH.

       EXIT-SEARCH.
           PERFORM CLOSE-LTP1-FILE.

      ******************************************
       CONSOLIDATE-SCHEDULE SECTION.
      ******************************************

           IF ELE = 17
              GO TO EXIT-CONSOLIDATE-SCHEDULE.
       NEXT-SCHED.
           MOVE LTP-SCHLD-TERM(ELE + 1) TO LTP-SCHLD-TERM(ELE).
           MOVE LTP-SCHLD-PAYMNT(ELE + 1) TO LTP-SCHLD-PAYMNT(ELE).
           ADD 1 TO ELE.
           IF ELE < 17
              GO TO NEXT-SCHED.

       EXIT-CONSOLIDATE-SCHEDULE.
           MOVE 0 TO LTP-SCHLD-TERM(ELE) LTP-SCHLD-PAYMNT(ELE).
           PERFORM DISPLAY-FIELDS.

      ************************************************************
       FORM-RESET SECTION.
      ************************************************************
           MOVE EXT-CONAME-CONAME TO PSMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PSMAIN-SYSDATE.
           DISPLAY SPACES UPON PSMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PSMAIN-SCREEN UPON PSMAIN-WINDOW-HANDLE.
           MOVE PSMAIN-FORM-FIELDS TO WR-BUF.
           MOVE PSMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PSMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE PSMAIN-HELP--DIR TO HELP-LINK-DIR.

      ************************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PSMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLTPCL.CPY".

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
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
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
           DESTROY PSMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
