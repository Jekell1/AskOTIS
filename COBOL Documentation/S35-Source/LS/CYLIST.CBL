       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CYLIST.
       DATE-WRITTEN. 020221
      ****************************************************************
      *                      (LS)
      *    DESCRIPTION:  CITY CODE TAX LISTING
      *
      * REV:
      *  CS        *NEW***  [WORLD PR#288]
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT WK-CITY-FILE    ASSIGN WK-CITY-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-CITY-KEY
                  FILE STATUS FILE-STAT.

      *---------------------------------------------------

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-CITY-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-CITY-REC.
           03  WK-CITY-KEY.
               05  WK-STATE                 PIC X(2).
               05  WK-CITY-DESC             PIC X(18).
           03  WK-CITY                      PIC X(3).
           03  WK-TAX-TABLE OCCURS 10 TIMES.
               05  WK-INSTAXFRMLA           PIC X.
               05  WK-INSTAXAMT             PIC 99.99.
           03  WK-TAX-FLAG                  PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LS/CYLIST.CBL S35 09/20/22-14:21:36 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK-CITY-IFN          PIC X(9)    VALUE "WK-CITY".
       01  WK-CITY-PATH         PIC X(35)   VALUE SPACES.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME      PIC X        VALUE "Y".
       01  MESS-LIST       PIC X(5)     VALUE "MESS.".

       01  TAX-SUB         PIC 9(2)     VALUE ZEROS.
       01  HOLD-CY-STATE   PIC X(2)     VALUE SPACES.
       01  PRINT-HEADER-TYPE PIC 9      VALUE 1.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  SEQUENCE-PRINT  PIC 9.
           03  BEG-STATE       PIC X(2).
           03  END-STATE       PIC X(2).
           03  BEG-CODE        PIC X(3).
           03  END-CODE        PIC X(3).
           03  INCLUDE-TAX     PIC X.


      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(20)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(05)    VALUE " ".
           03  FILLER           PIC X(30)    VALUE SPACES.
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(20)     VALUE " ".
           03  H-TITLE            PIC X(55)    VALUE  SPACES.

       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "STATE     ".
           03  H-STATE     PIC 99.

       01  HEAD4.
           03  FILLER      PIC X(22)    VALUE SPACES.
           03  FILLER      PIC X(5)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE " DCL(S)".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE " DCL(J)".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE " LCL(S)".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE " LCL(J)".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE "  AH(S)".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE "     PP".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE "    PP2".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE "     O1".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE "     O2".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE  "    O3".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(4)     VALUE " TAX".

       01  HEAD5-CODE.
           03  FILLER      PIC X(4)     VALUE "CODE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(15)    VALUE "DESCRIPTION    ".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(4)     VALUE "FLAG".

       01  HEAD5-DESC.
           03  FILLER      PIC X(15)    VALUE "DESCRIPTION    ".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(4)     VALUE "CODE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE "CD".
           03  FILLER      PIC X(2)     VALUE "--".
           03  FILLER      PIC X(4)     VALUE "RATE".


       01  HEAD5-CODE-SHORT.
           03  FILLER      PIC X(40)    VALUE SPACES.
           03  FILLER      PIC X(4)     VALUE "CODE".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(15)    VALUE "DESCRIPTION    ".

       01  HEAD5-DESC-SHORT.
           03  FILLER      PIC X(40)    VALUE SPACES.
           03  FILLER      PIC X(15)    VALUE "DESCRIPTION    ".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(4)     VALUE "CODE".


       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DLINE1   REDEFINES DETAIL-LINE.
           03  D1-PRINT-BY-CODE.
               05  D1-CY-CODE                  PIC X(3).
               05  FILLER                      PIC X(3).
               05  D1-CY-DESCRIPTION           PIC X(18).
               05  FILLER                      PIC X(2).
            03 D1-PRINT-BY-DESC REDEFINES D1-PRINT-BY-CODE.
               05  D1-CY-CITY-DESCRIPTION      PIC X(18).
               05  FILLER                      PIC X(3).
               05  D1-CY-CITY-CODE             PIC X(3).
               05  FILLER                      PIC X(2).

           03  D1-TAX-TABLE OCCURS 10 TIMES.
               05  D1-CY-INSTAXFRMLA       PIC X.
               05  FILLER                  PIC X(2).
               05  D1-CY-INSTAXAMT         PIC 99.99.
               05  FILLER                  PIC X(2).

           03  FILLER                      PIC X(4).
           03  D1-TAX-FLAG                 PIC X.


       01  DLINE2   REDEFINES DETAIL-LINE.
           03  D2-PRINT-BY-CODE.
               05  FILLER                      PIC X(40).
               05  D2-CY-CODE                  PIC X(3).
               05  FILLER                      PIC X(3).
               05  D2-CY-DESCRIPTION           PIC X(18).
               05  FILLER                      PIC X(2).
           03 D2-PRINT-BY-DESC REDEFINES D2-PRINT-BY-CODE.
               05  FILLER                      PIC X(40).
               05  D2-CY-CITY-DESCRIPTION      PIC X(18).
               05  FILLER                      PIC X(3).
               05  D2-CY-CITY-CODE             PIC X(3).
               05  FILLER                      PIC X(2).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SELECTION  PIC X(16).


       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  CYLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CYLIST_DEF.CPY".
       COPY "LIBLS/CYLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CYLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE WK-CITY-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-CITY-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CYLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CYLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-CY.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-FG = 9
               GO TO END-ROUTINE.

           IF CD-TYPE NOT = "CY"
              GO TO END-ROUTINE.

           IF CD-CY-STATE < BEG-STATE OR CD-CY-STATE > END-STATE
              GO TO NEXT-CY.

           IF CD-CY-CITY   < BEG-CODE OR CD-CY-CITY   > END-CODE
              GO TO NEXT-CY.

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.

           IF SEQUENCE-PRINT = 2
              PERFORM UPDATE-WK-CITY
              GO TO NEXT-CY.

           PERFORM PRINT-DETAIL-LINE-PROCESS.

           GO TO NEXT-CY.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

           IF SEQUENCE-PRINT = 2
              PERFORM PRINT-WK-CITY-RECORDS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE " " TO ERRCD.
       SETRP.
           MOVE CYLIST-QUEUE-POS TO Q-POS.
           MOVE CYLIST-COPIES-POS TO C-POS.
           MOVE CYLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       SEQ.
           MOVE "E  N010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
               GO TO SEQ.
           MOVE VDUNUM0 TO SEQUENCE-PRINT.
           IF NOT (SEQUENCE-PRINT = 1 OR  2)
               MOVE "MUST ENTER 1 OR 2" TO MESS
               PERFORM SEND-MESS
               GO TO SEQ.

       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO SEQ.
           IF F3-KEY GO TO ALL-ST.

           IF NOT (ENTER-KEY OR DOWN-KEY)
               GO TO ST-01.

           MOVE VDUBUF  TO  BEG-STATE.
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO ST-01.

           IF NOT (ENTER-KEY OR DOWN-KEY)
               GO TO ST-02.

           MOVE VDUBUF  TO  END-STATE.
           GO TO CD-01.

       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE SPACES TO BEG-STATE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020ST2."   TO VDU.
           MOVE "ZZ"           TO END-STATE VDUBUF.
           PERFORM SCREENIO.

       CD-01.
           MOVE "E  A030CODE1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F3-KEY GO TO ALL-CD.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO SEQ.

           MOVE VDUBUF TO BEG-CODE.

       CD-02.
           MOVE "E  A030CODE2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO CD-01.

           MOVE VDUBUF TO END-CODE.

           IF BEG-CODE > END-CODE
               MOVE "INVALID CODE RANGE" TO MESS
               PERFORM SEND-MESS
               GO TO CD-02.

           GO TO TAX-RATE.

       ALL-CD.
           MOVE "S  A030CODE1." TO VDU.
           MOVE SPACES TO BEG-CODE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030CODE2." TO VDU.
           MOVE "ZZZ"           TO END-CODE VDUBUF.
           PERFORM SCREENIO.

       TAX-RATE.
           MOVE "E  A010TAXRT." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO CD-02.

           MOVE VDUBUF          TO INCLUDE-TAX.
           IF NOT (INCLUDE-TAX = "Y" OR "N")
               MOVE "ENTER 'Y' OR 'N' " TO MESS
               PERFORM SEND-MESS
               GO TO TAX-RATE.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO TAX-RATE.

           IF VDUBUF = "N" GO TO ST-01.
           IF VDUBUF NOT = "Y" GO TO OK-YN.


           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 1     TO  PRINT-HEADER-TYPE.

           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK-CITY-PATH.
           PERFORM OPEN-WK-CITY-FILE-OUTPUT.
           PERFORM CLOSE-WK-CITY-FILE.
           PERFORM OPEN-WK-CITY-FILE.

           PERFORM OPEN-CD1-FILE.

           MOVE "CY"      TO CD-TYPE
                             QCD1-WBEG-TYPE
                             QCD1-WEND-TYPE.
           MOVE BEG-STATE TO CD-CY-STATE.
           MOVE BEG-CODE  TO CD-CY-CITY.
           MOVE SPACES    TO CD-CY-FILLER.
           MOVE CD-CODE   TO QCD1-WBEG-CODE.
           MOVE ALL "Z"   TO QCD1-WEND-CODE.

           PERFORM START-CD1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       UPDATE-WK-CITY   SECTION.
      ***********************************
           MOVE SPACES TO WK-CITY-REC.
           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE ZEROS TO CD-CY-INSTAXAMT(TAX-SUB)
           END-PERFORM.

           MOVE CD-CY-STATE TO WK-STATE.
           MOVE CD-DESC     TO WK-CITY-DESC.
           MOVE CD-CY-CITY  TO  WK-CITY.

           IF NOT (INCLUDE-TAX = "Y")
               GO TO UPDATE-WK-CITY-END.
           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE CD-CY-INSTAXFRMLA(TAX-SUB) TO
                      WK-INSTAXFRMLA(TAX-SUB)
                MOVE CD-CY-INSTAXAMT (TAX-SUB) TO
                       WK-INSTAXAMT(TAX-SUB)
           END-PERFORM.

           MOVE CD-CY-BOTH-TAX-FG TO WK-TAX-FLAG.

       UPDATE-WK-CITY-END.
           PERFORM WRITE-WK-CITY-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-WK-CITY-EXIT.
           EXIT.

      ***********************************
       PRINT-DETAIL-LINE-PROCESS SECTION.
      ***********************************

           IF CD-CY-STATE NOT = HOLD-CY-STATE
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE CD-CY-STATE TO HOLD-CY-STATE H-STATE.

           IF NOT (INCLUDE-TAX = "Y")
              MOVE CD-CY-CITY   TO D2-CY-CODE
              MOVE CD-DESC      TO D2-CY-DESCRIPTION
              GO TO PRINT-DETAIL-LINE-PROCESS-END.

           MOVE CD-CY-CITY      TO D1-CY-CODE.
           MOVE CD-DESC         TO D1-CY-DESCRIPTION.

           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE CD-CY-INSTAXFRMLA(TAX-SUB) TO
                      D1-CY-INSTAXFRMLA(TAX-SUB)
                MOVE CD-CY-INSTAXAMT (TAX-SUB) TO
                       D1-CY-INSTAXAMT(TAX-SUB)
           END-PERFORM.

           MOVE CD-CY-BOTH-TAX-FG TO D1-TAX-FLAG.

       PRINT-DETAIL-LINE-PROCESS-END.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-DETAIL-LINE-PROCESS-EXIT.
           EXIT.


      *******************************************
       PRINT-WK-CITY-RECORDS SECTION.
      *******************************************
           MOVE 2 TO PRINT-HEADER-TYPE.
           MOVE SPACES TO WK-STATE.
           MOVE SPACES TO WK-CITY-DESC.
           PERFORM START-WK-CITY-FILE.

       NEXT-WK-CITY.
           PERFORM READ-WK-CITY-FILE-NEXT.
           IF IO-BAD
              GO TO PRINT-WK-CITY-RECORDS-EXIT.

           IF WK-STATE   NOT = HOLD-CY-STATE
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE WK-STATE  TO HOLD-CY-STATE H-STATE.

           IF NOT (INCLUDE-TAX = "Y")
               MOVE WK-CITY-DESC   TO D2-CY-CITY-DESCRIPTION
               MOVE WK-CITY        TO D2-CY-CITY-CODE
               GO TO PRINT-WK-CITY-RECORDS-END.

           MOVE WK-CITY-DESC   TO D1-CY-CITY-DESCRIPTION.
           MOVE WK-CITY        TO D1-CY-CITY-CODE.

           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE WK-INSTAXFRMLA(TAX-SUB) TO
                      D1-CY-INSTAXFRMLA(TAX-SUB)
                MOVE WK-INSTAXAMT (TAX-SUB) TO
                       D1-CY-INSTAXAMT(TAX-SUB)
            END-PERFORM.

           MOVE WK-TAX-FLAG TO D1-TAX-FLAG.

       PRINT-WK-CITY-RECORDS-END.

           PERFORM WRITE-DETAIL-LINE.
           GO TO NEXT-WK-CITY.

       PRINT-WK-CITY-RECORDS-EXIT.
           EXIT.


      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD 6 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************
       HEAD-PAGE SECTION.
      ***********************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF PRINT-HEADER-TYPE = 2
              MOVE "CITY TAX RATE FOR INSURANCE BY CITY DESCRIPTION"
                    TO H-TITLE
           ELSE
              MOVE "CITY TAX RATE FOR INSURANCE BY CITY CODE"
                    TO H-TITLE.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF INCLUDE-TAX NOT = "Y"
              GO TO NO-TAX-HEADING.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF SEQUENCE-PRINT = 2
              MOVE HEAD5-DESC TO PR-REC
           ELSE
              MOVE HEAD5-CODE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           GO TO END-HEAD-PAGE.

       NO-TAX-HEADING.
           IF SEQUENCE-PRINT = 2
               MOVE HEAD5-DESC-SHORT TO PR-REC
           ELSE
               MOVE HEAD5-CODE-SHORT TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ***********************************
       PRINT-RANGE-LINES SECTION.
      ***********************************
           MOVE 2 TO LN-FEED.

           MOVE "BEGINNING STATE" TO RANGE-LINE.
           MOVE BEG-STATE         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING STATE"    TO RANGE-LINE.
           MOVE END-STATE         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CODE" TO RANGE-LINE.
           MOVE BEG-CODE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CODE" TO RANGE-LINE.
           MOVE END-CODE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE TAX RATES" TO RANGE-LINE.
           MOVE  INCLUDE-TAX        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO CYLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CYLIST-SYSDATE.
           DISPLAY CYLIST-SCREEN UPON CYLIST-WINDOW-HANDLE.
           MOVE CYLIST-FORM-FIELDS TO WR-BUF.
           MOVE CYLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CYLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CYLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CYLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ****************************************************
       IO-ROUTINE SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       OPEN-WK-CITY-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-CITY-IFN TO E-FILE.
           OPEN OUTPUT WK-CITY-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-CITY-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-CITY-FILE
              GO TO OPEN-WK-CITY-FILE-OUTPUT.
           UNLOCK WK-CITY-FILE.
       OPEN-WK-CITY-FILE.
           PERFORM OPEN-IT.
           MOVE WK-CITY-IFN TO E-FILE.
           OPEN I-O WK-CITY-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-CITY-FILE.
           IF IO-FG = 7
              CLOSE WK-CITY-FILE
              GO TO OPEN-WK-CITY-FILE.
           UNLOCK WK-CITY-FILE.
       READ-WK-CITY-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK-CITY-IFN TO E-FILE.
           MOVE WK-CITY-KEY TO E-KEYX.
           READ WK-CITY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK-CITY-FILE-NEXT.
       START-WK-CITY-FILE.
           PERFORM START-IT.
           MOVE WK-CITY-IFN TO E-FILE.
           MOVE WK-CITY-KEY TO E-KEYX.
           UNLOCK WK-CITY-FILE.
       WRITE-WK-CITY-FILE.
           PERFORM WRITE-IT.
           MOVE WK-CITY-IFN TO E-FILE.
           MOVE WK-CITY-KEY TO E-KEYX.
           WRITE WK-CITY-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-CITY-FILE.
           UNLOCK WK-CITY-FILE.
       CLOSE-WK-CITY-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-CITY-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           MOVE WK-CITY-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY CYLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
