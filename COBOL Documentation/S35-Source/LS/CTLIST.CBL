       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CTLIST.
       DATE-WRITTEN. 020221
      ****************************************************************
      *                      (LS)
      *    DESCRIPTION:  COUNTY CODE TAX LISTING
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

           SELECT WK-COUNTY-FILE  ASSIGN WK-COUNTY-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-COUNTY-KEY
                  FILE STATUS FILE-STAT.

      *---------------------------------------------------

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-COUNTY-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-COUNTY-REC.
           03  WK-COUNTY-KEY.
               05  WK-STATE                 PIC X(2).
               05  WK-COUNTY-DESC           PIC X(18).
           03  WK-COUNTY                    PIC X(3).
           03  WK-TAX-TABLE OCCURS 10 TIMES.
               05  WK-INSTAXFRMLA           PIC X.
               05  WK-INSTAXAMT             PIC 99.99.


      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/CTLIST.CBL S35 10/03/22-12:09:32 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK-COUNTY-IFN        PIC X(9)    VALUE "WK-COUNTY".
       01  WK-COUNTY-PATH       PIC X(35)   VALUE SPACES.

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
       01  HOLD-CT-STATE   PIC X(2)     VALUE SPACES.
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

       01  HEAD5.
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


       01  HEAD5-2.
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
               05  D1-CT-CODE                  PIC X(3).
               05  FILLER                      PIC X(3).
               05  D1-CT-DESCRIPTION           PIC X(18).
               05  FILLER                      PIC X(2).
            03 D1-PRINT-BY-DESC REDEFINES D1-PRINT-BY-CODE.
               05  D1-CT-COUNTY-DESCRIPTION    PIC X(18).
               05  FILLER                      PIC X(3).
               05  D1-CT-COUNTY-CODE           PIC X(3).
               05  FILLER                      PIC X(2).

           03  D1-TAX-TABLE OCCURS 10 TIMES.
               05  D1-CT-INSTAXFRMLA       PIC X.
               05  FILLER                  PIC X(2).
               05  D1-CT-INSTAXAMT         PIC 99.99.
               05  FILLER                  PIC X(2).

       01  DLINE2   REDEFINES DETAIL-LINE.
           03  D2-PRINT-BY-CODE.
               05  FILLER                      PIC X(40).
               05  D2-CT-CODE                  PIC X(3).
               05  FILLER                      PIC X(3).
               05  D2-CT-DESCRIPTION           PIC X(18).
               05  FILLER                      PIC X(2).
           03 D2-PRINT-BY-DESC REDEFINES D2-PRINT-BY-CODE.
               05  FILLER                      PIC X(40).
               05  D2-CT-COUNTY-DESCRIPTION    PIC X(18).
               05  FILLER                      PIC X(3).
               05  D2-CT-COUNTY-CODE           PIC X(3).
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
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  CTLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CTLIST_DEF.CPY".
       COPY "LIBLS/CTLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CTLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE WK-COUNTY-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-COUNTY-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CTLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CTLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-CT.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-FG = 9
               GO TO END-ROUTINE.

           IF CD-TYPE NOT = "CT"
               GO TO END-ROUTINE.

           IF CD-CT-STATE < BEG-STATE OR CD-CT-STATE > END-STATE
              GO TO NEXT-CT.

           IF CD-CT-COUNTY < BEG-CODE OR CD-CT-COUNTY > END-CODE
              GO TO NEXT-CT.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME.

           IF SEQUENCE-PRINT = 2
              PERFORM UPDATE-WK-COUNTY
              GO TO NEXT-CT.

           PERFORM PRINT-DETAIL-LINE-PROCESS.

           GO TO NEXT-CT.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

           IF SEQUENCE-PRINT = 2
              PERFORM PRINT-WK-COUNTY-RECORDS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE " " TO ERRCD.
       SETRP.
           MOVE CTLIST-QUEUE-POS TO Q-POS.
           MOVE CTLIST-COPIES-POS TO C-POS.
           MOVE CTLIST-PRU-POS TO P-POS.
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

           MOVE VDUBUF TO BEG-STATE.
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
           PERFORM OPEN-CD1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK-COUNTY-PATH.
           PERFORM OPEN-WK-COUNTY-FILE-OUTPUT.
           PERFORM CLOSE-WK-COUNTY-FILE.
           PERFORM OPEN-WK-COUNTY-FILE.

           MOVE "CT"      TO CD-TYPE
                             QCD1-WBEG-TYPE
                             QCD1-WEND-TYPE.
           MOVE BEG-STATE TO CD-CT-STATE
           MOVE BEG-CODE  TO CD-CT-COUNTY.
           MOVE SPACES    TO CD-CT-FILLER.
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

      ****************************
       UPDATE-WK-COUNTY SECTION.
      ****************************
           MOVE SPACES       TO WK-COUNTY-REC.

           MOVE CD-CT-STATE  TO WK-STATE.
           MOVE CD-DESC      TO WK-COUNTY-DESC.
           MOVE CD-CT-COUNTY TO WK-COUNTY.

           IF NOT (INCLUDE-TAX = "Y")
              GO TO UPDATE-WK-COUNTY-END.

           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE CD-CT-INSTAXFRMLA(TAX-SUB) TO
                      WK-INSTAXFRMLA(TAX-SUB)
                MOVE CD-CT-INSTAXAMT (TAX-SUB) TO
                       WK-INSTAXAMT(TAX-SUB)
           END-PERFORM.

       UPDATE-WK-COUNTY-END.
           PERFORM WRITE-WK-COUNTY-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-WK-COUNTY-EXIT.
           EXIT.

      ************************************************
       PRINT-DETAIL-LINE-PROCESS SECTION.
      ************************************************

           IF CD-CT-STATE NOT = HOLD-CT-STATE
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE CD-CT-STATE TO HOLD-CT-STATE H-STATE.

           IF NOT (INCLUDE-TAX = "Y")
               MOVE CD-CT-COUNTY TO D2-CT-CODE
               MOVE CD-DESC      TO D2-CT-DESCRIPTION
               GO TO PRINT-DETAIL-LINE-PROCESS-END.

           MOVE CD-CT-COUNTY  TO D1-CT-CODE.
           MOVE CD-DESC       TO D1-CT-DESCRIPTION.

           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE CD-CT-INSTAXFRMLA(TAX-SUB) TO
                      D1-CT-INSTAXFRMLA(TAX-SUB)
                MOVE CD-CT-INSTAXAMT (TAX-SUB) TO
                       D1-CT-INSTAXAMT(TAX-SUB)
           END-PERFORM.

       PRINT-DETAIL-LINE-PROCESS-END.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-DETAIL-LINE-PROCESS-EXIT.
           EXIT.


      ************************************************
       PRINT-WK-COUNTY-RECORDS SECTION.
      ************************************************
           MOVE 2 TO PRINT-HEADER-TYPE.
           MOVE SPACES TO WK-STATE.
           MOVE SPACES TO WK-COUNTY-DESC.
           PERFORM START-WK-COUNTY-FILE.

       NEXT-WK-COUNTY.
           PERFORM READ-WK-COUNTY-FILE-NEXT.
           IF IO-BAD
              GO TO PRINT-WK-COUNTY-RECORDS-EXIT.

           IF WK-STATE    NOT = HOLD-CT-STATE
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE WK-STATE  TO HOLD-CT-STATE H-STATE.

           IF NOT (INCLUDE-TAX = "Y")
                MOVE WK-COUNTY-DESC TO D2-CT-COUNTY-DESCRIPTION
                MOVE WK-COUNTY      TO D2-CT-COUNTY-CODE
                GO TO  PRINT-WK-COUNTY-RECORDS-END.

           MOVE WK-COUNTY-DESC TO D1-CT-COUNTY-DESCRIPTION.
           MOVE WK-COUNTY      TO D1-CT-COUNTY-CODE.

           PERFORM VARYING TAX-SUB FROM 1 BY 1
              UNTIL TAX-SUB > 10
                MOVE WK-INSTAXFRMLA(TAX-SUB) TO
                      D1-CT-INSTAXFRMLA(TAX-SUB)
                MOVE WK-INSTAXAMT (TAX-SUB) TO
                       D1-CT-INSTAXAMT(TAX-SUB)
            END-PERFORM.

       PRINT-WK-COUNTY-RECORDS-END.

            PERFORM WRITE-DETAIL-LINE.

            GO TO NEXT-WK-COUNTY.

       PRINT-WK-COUNTY-RECORDS-EXIT.
           EXIT.

      ************************************************
       WRITE-DETAIL-LINE SECTION.
      ************************************************
           ADD 6 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ************************************************
       HEAD-PAGE SECTION.
      ************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF PRINT-HEADER-TYPE = 2
              MOVE "COUNTY TAX RATE FOR INSURANCE BY COUNTY DESCRIPTION"
                    TO H-TITLE
           ELSE
              MOVE "COUNTY TAX RATE FOR INSURANCE BY COUNTY CODE"
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
              MOVE HEAD5-2 TO PR-REC
           ELSE
              MOVE HEAD5 TO PR-REC.

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

      ************************************************
       PRINT-RANGE-LINES SECTION.
      ************************************************
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
           MOVE EXT-CONAME-CONAME TO CTLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CTLIST-SYSDATE.
           DISPLAY CTLIST-SCREEN UPON CTLIST-WINDOW-HANDLE.
           MOVE CTLIST-FORM-FIELDS TO WR-BUF.
           MOVE CTLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CTLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CTLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CTLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       OPEN-WK-COUNTY-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-COUNTY-IFN TO E-FILE.
           OPEN OUTPUT WK-COUNTY-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-COUNTY-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-COUNTY-FILE
              GO TO OPEN-WK-COUNTY-FILE-OUTPUT.
           UNLOCK WK-COUNTY-FILE.
       OPEN-WK-COUNTY-FILE.
           PERFORM OPEN-IT.
           MOVE WK-COUNTY-IFN TO E-FILE.
           OPEN I-O WK-COUNTY-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-COUNTY-FILE.
           IF IO-FG = 7
              CLOSE WK-COUNTY-FILE
              GO TO OPEN-WK-COUNTY-FILE.
           UNLOCK WK-COUNTY-FILE.
       READ-WK-COUNTY-FILE.
           PERFORM READ-IT.
           MOVE WK-COUNTY-IFN TO E-FILE.
           MOVE WK-COUNTY-KEY TO E-KEYX.
           READ WK-COUNTY-FILE.
           IF IO-FG = 8
              GO TO READ-WK-COUNTY-FILE.
       READ-WK-COUNTY-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK-COUNTY-IFN TO E-FILE.
           MOVE WK-COUNTY-KEY TO E-KEYX.
           READ WK-COUNTY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK-COUNTY-FILE-NEXT.
       START-WK-COUNTY-FILE.
           PERFORM START-IT.
           MOVE WK-COUNTY-IFN TO E-FILE.
           MOVE WK-COUNTY-KEY TO E-KEYX.
           START WK-COUNTY-FILE KEY NOT < WK-COUNTY-KEY.
           UNLOCK WK-COUNTY-FILE.
       WRITE-WK-COUNTY-FILE.
           PERFORM WRITE-IT.
           MOVE WK-COUNTY-IFN TO E-FILE.
           MOVE WK-COUNTY-KEY TO E-KEYX.
           WRITE WK-COUNTY-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-COUNTY-FILE.
           UNLOCK WK-COUNTY-FILE.
       CLOSE-WK-COUNTY-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-COUNTY-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           MOVE WK-COUNTY-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY CTLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
