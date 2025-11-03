       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PAYSCH.
      ******************************************************************
      *
      *   DIR :  WI
      *   DESC:  LOAN PAYMENT SCHEDULE ENTRY
      * CALLED:  FROM XONIN0 TO PUT A SHEDULE ON A COVERTED LOAN
      *
      *=================================================================
      * REV:
      * MJD 112597 EXPANDED LT-REC TO 144 BYTES (ADDED 30)
      * BAH 010698 DISPLAY OF LN-LASTPYAMT WAS DISPLAYING IN
      *            THE FIRST SLOT, WRONG FIELD NAME !!
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2017.0522 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LT' (P) FILE PREFIX FOR WORKING STORAGE FIELDS.
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
           "@(#)WI/PAYSCH.CBL S35 02/22/24-12:51:55 >".
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

       01  PAYSCH-LINES           PIC 99   VALUE 18.
       01  PAYSCH-COLUMNS         PIC 99   VALUE 65.
       01  PAYSCH-BEGIN-Y         PIC 99   VALUE 6.
       01  PAYSCH-BEGIN-X         PIC 99   VALUE 3.

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  LEGEND            PIC X(55).
       01  LEGEND-LIST       PIC X(8)   VALUE "WSEL:D.".
       01  HOLD-LTP1-KEY          VALUE SPACES.
           03  HOLD-LTP-BRNO     PIC 9(4).
           03  HOLD-LTP-ACCTNO   PIC 9(8).
           03  HOLD-LTP-SEQNO    PIC 999.


       01  ORIGINAL-LTP-REC      PIC X(LTP-SIZE) VALUE SPACES.
       01  CHANGED-LTP-REC       PIC X(LTP-SIZE) VALUE SPACES.

       01  CHAN              PIC X(4).

       01  ERRCD             PIC X.

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
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
           03  FILLER     PIC X(8) VALUE "ENNN030 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 17 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  PAYSCH-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PAYSCH_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/PAYSCH_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/PAYSCHW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PAYSCH_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME PAYSCH-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "PAYSCH" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE PAYSCH-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  PAYSCH-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    PAYSCH-COLUMNS + VDU-WIN-W-MOD
                      LINES   PAYSCH-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS PAYSCH-WINDOW-HANDLE.

           MOVE PAYSCH-ACCTNO TO LN-ACCTNO.
           IF PAYSCH-NEW-FLAG NOT = 0
              MOVE PAYSCH-PLCD TO LN-PLCD
              MOVE PAYSCH-SERVACCT TO LN-SERVACCT
              MOVE PAYSCH-SALEFIN TO LN-SALEFIN
              MOVE PAYSCH-LOANTYPE TO LN-LOANTYPE
              MOVE PAYSCH-1STPYAMT TO LN-1STPYAMT
              MOVE PAYSCH-NOREGPY TO LN-NOREGPY
              MOVE PAYSCH-REGPYAMT TO LN-REGPYAMT
              MOVE PAYSCH-LASTPYAMT TO LN-LASTPYAMT
           ELSE
              PERFORM OPEN-LN1-FILE
              PERFORM READ-LN1-FILE
              PERFORM CLOSE-LN1-FILE
              IF IO-FG NOT = 0
                 MOVE "LOAN RECORD NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
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
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ***************************
       REC-LT-MODULE SECTION.
      ***************************

           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE PAYSCH-BRANCH TO HOLD-LTP-BRNO   LTP-BRNO.
           MOVE PAYSCH-ACCTNO TO HOLD-LTP-ACCTNO LTP-ACCTNO.
           MOVE 1             TO HOLD-LTP-SEQNO  LTP-SEQNO.

       READ-LTFILE.
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
                 GO TO IO-ERROR
              ELSE
                 IF PAYSCH-TEST-BAL = 9
                    MOVE
                     "ACCOUNT HAS BALANCES, NEW RECORD NOT ALLOWED"
                                                       TO MESS
                    PERFORM SEND-MESS
                    MOVE "END" TO IO-TYPE
                    GO TO END-REC-LT-MODULE
                 ELSE
                    PERFORM NEW-RECORD-SETUP.

           MOVE LTP-REC TO ORIGINAL-LTP-REC.

           MOVE PAYSCH-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR
           ELSE
              PERFORM CLOSE-BR-FILE.

       END-REC-LT-MODULE.
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
           MOVE " F1-CLR, 'DELETE', ENTER FIELD#, F7-EXIT & UPD:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
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
           IF PAYSCH-TEST-BAL = 9
              MOVE "ACCOUNT HAS BALANCES, NO CHANGES ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
           IF ELE = 1
              IF NEW-FLAG = 1
                 MOVE " F7-EXIT, ENTER NO. OF PAYMENTS:" TO LEGEND
              ELSE
                 MOVE " F1-CLR, ENTER NO. OF PAYMENTS:" TO LEGEND.
           PERFORM SEND-LEGEND.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = CHAN
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
           IF VDUSTATUS = "1:"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F7.
           IF VDUSTATUS = "1>"
              IF ELE = 1
                 IF NEW-FLAG = 1
                    MOVE "END" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
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
           MOVE " F1-CLR, ^, ENTER AMOUNT OF PAYMENT:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN062  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1:")
              GO TO A-011.

           MOVE VDUNUM2 TO LTP-SCHLD-PAYMNT(ELE).
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      ***************************************
       WRITE-MODULE SECTION.
      ***************************************
           PERFORM OPEN-LTP1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LTP-REC TO CHANGED-LTP-REC.
           IF ORIGINAL-LTP-REC = CHANGED-LTP-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LTP-REC NOT = ORIGINAL-LTP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-LTP-REC TO LTP-REC.
           PERFORM REWRITE-LTP1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LTP1-FILE.

      **************************************
       DELETE-MODULE SECTION.
      **************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LTP1-FILE.
           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LTP-REC NOT = ORIGINAL-LTP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LTP1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-LTP1-FILE.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
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

      ****************************************
       NEW-RECORD-SETUP SECTION.
      ****************************************
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000DISP." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTP-FILE.

           MOVE HOLD-LTP-BRNO   TO LTP-BRNO.
           MOVE HOLD-LTP-ACCTNO TO LTP-ACCTNO.
           MOVE HOLD-LTP-SEQNO  TO LTP-SEQNO.

           MOVE 0 TO ELE.

           MOVE "SN N030010." TO VDU.
           MOVE 1 TO VDUNUM0 LTP-SCHLD-TERM(1).
           PERFORM SCREENIO.
           MOVE "SN N062011." TO VDU.
           MOVE LN-1STPYAMT TO VDUNUM2 LTP-SCHLD-PAYMNT(1).
           PERFORM SCREENIO.

           MOVE "SN N030020." TO VDU.
           MOVE LN-NOREGPY TO VDUNUM0 LTP-SCHLD-TERM(2).
           PERFORM SCREENIO.
           MOVE "SN N062021." TO VDU.
           MOVE LN-REGPYAMT TO VDUNUM2 LTP-SCHLD-PAYMNT(2).
           PERFORM SCREENIO.

           MOVE "SN N030030." TO VDU.
           MOVE 1 TO VDUNUM0 LTP-SCHLD-TERM(3).
           PERFORM SCREENIO.
           MOVE "SN N062031." TO VDU.
           MOVE LN-LASTPYAMT TO VDUNUM2 LTP-SCHLD-PAYMNT(3).
           PERFORM SCREENIO.

       NEW-RECORD-EXIT.
           EXIT.

      *********************************************
       SEARCH-FOR-ACTIVE-INS SECTION.
      *********************************************
           MOVE LTP-REC TO CHANGED-LTP-REC.

           MOVE SPACES TO ERRCD.


           PERFORM OPEN-LTP1-FILE.

           MOVE 0 TO LTP-SEQNO.

           PERFORM START-LTP1-FILE.
       NEXT-LT.
           PERFORM READ-LTP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LTP-BRNO NOT = LTP-PATH-OWNBR) OR
               (LTP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-SEARCH.

           IF HOLD-LTP-ACCTNO = LTP-ACCTNO AND
               HOLD-LTP-BRNO = LTP-BRNO AND
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
           PERFORM UNPACK-PAYMENT-SCHEDULE.
           PERFORM PACK-PAYMENT-SCHEDULE.
           PERFORM DISPLAY-FIELDS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLTPCL.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY SPACES UPON PAYSCH-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PAYSCH-SCREEN UPON PAYSCH-WINDOW-HANDLE.
           MOVE PAYSCH-FORM-FIELDS TO WR-BUF.
           MOVE PAYSCH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE PAYSCH-HELP--FILE TO HELP-LINK-FILE.
           MOVE PAYSCH-HELP--DIR  TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PAYSCH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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

      ************************************
       IO-ROUTINES SECTION.
      ************************************
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

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PAYSCH-SCREEN.
           DESTROY PAYSCH-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
