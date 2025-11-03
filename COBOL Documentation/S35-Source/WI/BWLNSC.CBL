       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWLNSC.
      *******************************************************************
      *  BORROWER / LOANS FILE WINDOW SCAN
      * CALLED FROM:
      *              LP/LONPF0 (F6-SCAN)
      *              LP/LONPN0 (F6-SCAN)
      *              LP/XONPN0 (F6-SCAN)
      *              LP/LONPJ0 (F6-SCAN)
      *              SP/CPINQ0 (F6-SCAN) F10-INTERRUPT
      *              WI/LNQUOT (F6-SCAN)
      * REV:
      *  JTG 082494 ADDED READ PREVIOUS LOGIC
      *  JTG 082994 FORCED TO SEE ALL LOANS IF BORROWER HAS NONE ACTIVE
      *  BAH 111194 WAS NOT PASSING BACK BWLNSC-SSNO
      *  MJD 042396 ADDED BWLNSC-ACCTNO: REDEFINED BY BWLNSC-NO AND
      *              BWLNSC-SEQNO. NOW PASSES BACK 8 DIGIT ACCOUNT NUMBER
      *  KEC 112696 CHANGED LN-POCD FOR RESCISSION.
      *  BLF 070801 CHANGED BWLNSC-CUR-ONLY TO BWLNSC-ARCHIVE-FG, TO
      *             INDICATE RETURNING ACCT # WHICH IS IN ARCHIVE
      *             DIRECTORY, WORLD PR# 547
      *  BLM 100920 WHEN NO-LOANS = 1, SCAN LOOKS FOR 1 LOAN TO DISPLAY,
      *             SKIPPING PAID-OUTS & ZERO BALANCE.  FIX TO INCLUDE
      *             REVOLVING ACCOUNTS EVEN IF ZERO BALANCE, REGWAN PL# 704
      *   CS 141114 CHANGED VDU-WCOLOR-TCLP/VDU-WCTRL-TCLP TO VDU-WCOLOR-SCAN/
      *             VDU-WCTRL-SCAN  
      *  BLM 161221 FIX SCAN FOR LONGER BORROWER NAMES
      *  BAH 180926 GLOBAL BWFILE
      *  BAH 181218 GLOBAL LNFILE
      *  BAH 20190627 REMOVED LN-REVOLVING, PT0153
      *  BAH 20200626 MOVED OPEN-BW1 TO TOP OF PROGRAM #TST001
      *  BAH 20230714 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      *******************************************************************
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
           "@(#)WI/BWLNSC.CBL S35 07/01/24-12:21:36 >".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  BWLNSC-LINES          PIC 99  VALUE 17.
       01  BWLNSC-COLUMNS        PIC 99  VALUE 61.
       01  BWLNSC-BEGIN-Y        PIC 99  VALUE 4.
       01  BWLNSC-BEGIN-X        PIC 99  VALUE 10.

       01  ERRCD                    PIC X      VALUE SPACES.
       01  ONE                      PIC S9     VALUE 1.
       01  IX1                      PIC 99.
       01  IX2                      PIC 99.
       01  SUB                      PIC 99.
       01  SCAN-TYPE                PIC 9      VALUE 2.
       01  NO-LOANS                 PIC 99.
       01  LNSUB                    PIC 9.
       01  CURBAL                   PIC 9(6)V99.

       01  ALP-BW.
           03  ALP-BW123            PIC X(3).
           03  ALP-HYPHEN1          PIC X.
           03  ALP-BW45             PIC X(2).
           03  ALP-HYPHEN2          PIC X.
           03  ALP-BW6789           PIC X(4).

       01  NUM-BW                   PIC 9(9).
       01  FILLER                   REDEFINES NUM-BW.
           03  NUM-BW123            PIC X(3).
           03  NUM-BW45             PIC X(2).
           03  NUM-BW6789           PIC X(4).

       01  A-NAME                   PIC X(35).

       01  BW-ALP-TABLE.
           03  BW-ALP-TBL           OCCURS 70 TIMES
                                    PIC X.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-S-SSNO    PIC X(11).
               05  FILLER           PIC XX.
               05  FLDSEL-S-NAME    PIC X(22).
               05  FILLER           PIC X.
               05  FLDSEL-S-ADR1    PIC X(18).
               05  FILLER           PIC X.
               05  FLDSEL-S-OPEN    PIC 99.
           03  FLDSEL-N             REDEFINES FLDSEL-S.
               05  FLDSEL-N-NAME    PIC X(22).
               05  FILLER           PIC X.
               05  FLDSEL-N-ADR1    PIC X(18).
               05  FILLER           PIC X.
               05  FLDSEL-N-SSNO    PIC X(11).
               05  FILLER           PIC XX.
               05  FLDSEL-N-OPEN    PIC 99.

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  PROMPT-LIST              PIC X(6)  VALUE "000:S.".

       01  BWLNSC-WINDOW-HANDLE    PIC X(10).
       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-S.
                   07  D-S-KEY      PIC X(11).
                   07  FILLER       PIC XX.
                   07  D-S-NAME     PIC X(22).
                   07  FILLER       PIC X.
                   07  D-S-ADR1     PIC X(18).
                   07  FILLER       PIC X.
                   07  D-S-OPEN     PIC ZZ.
               05  D-N              REDEFINES D-S.
                   07  D-N-NAME     PIC X(22).
                   07  FILLER       PIC X.
                   07  D-N-ADR1     PIC X(18).
                   07  FILLER       PIC X.
                   07  D-N-KEY      PIC X(11).
                   07  FILLER       PIC XX.
                   07  D-N-OPEN     PIC ZZ.
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 50.
       01  PAGE-KEY.
           03  FILLER               OCCURS 50 TIMES.
               05  PAGE-KEY-N-X.
                   07  PAGE-KEY-N       PIC X(41).
                   07  PAGE-KEY-N-S     PIC 9(9).
               05  FILLER           REDEFINES PAGE-KEY-N-X.
                   07  PAGE-KEY-S   PIC 9(9).
                   07  FILLER       PIC X(41).

       01  MORE-LIST.
           03  FILLER               PIC X(39)  VALUE
           "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
           03  FILLER               PIC X(16)  VALUE
           "LIA OPEN CLOSED.".
       01  MORE-BUF.
           03  M-NAME               PIC X(30).
           03  M-ADR1               PIC X(30).
           03  M-ADR2               PIC X(30).
           03  M-ADR3               PIC X(30).
           03  M-PHONE              PIC X(14).
           03  M-EMPL               PIC X(20).
           03  M-SPOUSE             PIC X(28).
           03  M-LIA                PIC Z,ZZZ,ZZ9.99-.
           03  M-OPEN               PIC ZZ.
           03  M-CLOSED             PIC ZZ.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/BWLNSC_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BWLNSC_DEF.CPY".
       COPY "LIBWI/BWSCN2_DEF.CPY".
       COPY "LIBWI/BWLNSC_WKS.CPY".
       COPY "LIBWI/BWSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BWLNSCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BWLNSC_SCN.CPY".
       COPY "LIBWI/BWSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME BWLNSC-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO BWLNSC-STATUS.

           MOVE "BWLNSC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE BWLNSC-BEGIN-Y
                   AT COL  BWLNSC-BEGIN-X
                   SIZE    BWLNSC-COLUMNS + VDU-WIN-W-MOD
                   LINES   BWLNSC-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS BWLNSC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE 2 TO SCAN-TYPE.
           GO TO SEND-SORT-PROMPT.

       ENTER-TYPE.
           MOVE "S  A050ATYPE:S." TO VDU.
           MOVE LEGEND-05 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010TYPE." TO VDU.
           MOVE "_" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           IF NOT (VDUNUM0 = 1 OR 2)
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO SCAN-TYPE.

       SEND-SORT-PROMPT.
           IF SCAN-TYPE = 1 OR 3
              MOVE SSNO-PROMPT TO WR-BUF
           ELSE
              MOVE NAME-PROMPT TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-SCANKEY.
           IF SCAN-TYPE = 1
              MOVE LEGEND-02 TO MESS
           ELSE
              MOVE LEGEND-03 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA200SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
      D    STOP MESS.
           IF SCAN-TYPE = 1 OR 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO PAGE-KEY-N(PAGE-COUNT)
              IF PAGE-KEY-S(PAGE-COUNT) NOT NUMERIC
                 GO TO ENTER-SCANKEY.
           IF SCAN-TYPE = 2 OR 4
              PERFORM ARRANGE-BW-KEY
              MOVE BW-ALP-TABLE TO PAGE-KEY-N(PAGE-COUNT)
              MOVE 0 TO PAGE-KEY-N-S(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           IF SCAN-TYPE = 1
              MOVE PAGE-KEY-S(CURRENT-PAGE) TO BW-SSNO
           ELSE
           IF SCAN-TYPE = 2
              MOVE PAGE-KEY-N(CURRENT-PAGE) TO BW-NAME
           ELSE
           IF SCAN-TYPE = 3
              MOVE PAGE-KEY-S(CURRENT-PAGE) TO BW-SPSSNO
           ELSE
              MOVE PAGE-KEY-N(CURRENT-PAGE) TO BW-SPNAME.

           PERFORM START-BORROWER.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-BORROWER.
           IF (IO-FG NOT = 0) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
                               OR (SUB > 9)
              GO TO DISPLAY-PAGE.

           IF SCAN-TYPE = 1 OR 2
              MOVE BW-SSNO TO NUM-BW
           ELSE
              MOVE BW-SPSSNO TO NUM-BW.
           IF NUM-BW = 0
              GO TO BUILD-PAGENO.
           PERFORM NUMBW-TO-ALPBW.
           IF SCAN-TYPE = 1 OR 3
              MOVE ALP-BW TO D-S-KEY(SUB)
           ELSE
              MOVE ALP-BW TO D-N-KEY(SUB).

           IF SCAN-TYPE = 1 OR 2
              MOVE BW-LNAME TO MESS
              MOVE BW-FNAME TO A-NAME
           ELSE
              MOVE BW-SPLNAME TO MESS
              MOVE BW-SPFNAME TO A-NAME.
           PERFORM SET-BW-LNAME.
           IF SCAN-TYPE = 1 OR 3
              MOVE MESS TO D-S-NAME(SUB)
              MOVE BW-ADR1 TO D-S-ADR1(SUB)
              MOVE BW-LOANS TO D-S-OPEN(SUB)
           ELSE
              MOVE MESS TO D-N-NAME(SUB)
              MOVE BW-ADR1 TO D-N-ADR1(SUB)
              MOVE BW-LOANS TO D-N-OPEN(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-SCANKEY.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    IF SCAN-TYPE = 1
                       MOVE BW-SSNO TO PAGE-KEY-S(PAGE-COUNT) 
                    ELSE
                    IF SCAN-TYPE = 2
                       MOVE BW-NAME TO PAGE-KEY-N(PAGE-COUNT)      
                       MOVE BW-SSNO TO PAGE-KEY-N-S(PAGE-COUNT)      
                    ELSE
                    IF SCAN-TYPE = 3
                       MOVE BW-SPSSNO TO PAGE-KEY-S(PAGE-COUNT)      
                    ELSE
                       MOVE BW-SPNAME TO PAGE-KEY-N(PAGE-COUNT)      
                       MOVE BW-SPSSNO TO PAGE-KEY-N-S(PAGE-COUNT)      
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-04 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANKEY.
           IF NOT (FLDSEL-STAT = "00" OR "18" OR "1<")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "18"
              MOVE "00" TO FLDSEL-STAT
              MOVE "Y" TO BWLNS2-ALL-LOANS
           ELSE
              MOVE "N" TO BWLNS2-ALL-LOANS.

           IF SCAN-TYPE = 1 OR 3
              MOVE FLDSEL-S-SSNO TO ALP-BW
              INSPECT FLDSEL-S-OPEN REPLACING ALL " " BY "0"
              MOVE FLDSEL-S-OPEN TO NO-LOANS
           ELSE
              MOVE FLDSEL-N-SSNO TO ALP-BW
              INSPECT FLDSEL-N-OPEN REPLACING ALL " " BY "0"
              MOVE FLDSEL-N-OPEN TO NO-LOANS.

           IF NO-LOANS = 0
              MOVE "Y" TO BWLNS2-ALL-LOANS.

           PERFORM ALPBW-TO-NUMBW.
           MOVE NUM-BW TO BWLNS2-KEY.

           IF BWLNS2-ALL-LOANS = "N"
              IF NO-LOANS = 1
                 MOVE 0 TO LNSUB
                           CURBAL
                 PERFORM OPEN-LN4-FILE
                 PERFORM READ-LOANS
      *              UNTIL LNSUB = 4 OR CURBAL NOT = 0
                     UNTIL LNSUB = 2 OR CURBAL NOT = 0
                 PERFORM CLOSE-LN4-FILE
                 IF (CURBAL NOT = 0)
                    MOVE LN-ACCTNO TO BWLNSC-ACCTNO
                    MOVE NUM-BW    TO BWLNSC-SSNO
                    MOVE "00"      TO BWLNSC-STATUS
                    GO TO END-ROUTINE.

           MOVE BWLNSC-SYSDATE TO BWLNS2-SYSDATE.
           MOVE "WI/BWLNS2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNS2-STATUS NOT = "00" OR BWLNS2-ACCTNO = 0
              GO TO SELECT-FIELD-FROM-DISPLAY
           ELSE
              MOVE BWLNS2-ACCTNO TO BWLNSC-ACCTNO
              MOVE NUM-BW        TO BWLNSC-SSNO
              MOVE "00"          TO BWLNSC-STATUS
              GO TO END-ROUTINE.

           GO TO END-ROUTINE.


      *********************************************
       READ-LOANS SECTION.
      *********************************************
           ADD 1 TO LNSUB.
           IF LNSUB = 1
              PERFORM READ-LOANS-4
           ELSE
              IF LNSUB = 2
                 PERFORM READ-LOANS-5.
      *       ELSE
      *          IF LNSUB = 3
      *             PERFORM READ-LOANS-6
      *          ELSE
      *             IF LNSUB = 4
      *                PERFORM READ-LOANS-7.

       READ-LOANS-EXIT.
           EXIT.

      *******************************************
       READ-LOANS-4 SECTION.
      *******************************************
           MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BWLNS2-KEY TO LN-SSNO(1)
                              QLN4-WBEG-SSNO1
                              QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE-NOT-GREATER.
       READ-LOANS-4-NEXT.
           PERFORM READ-LN4-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LN-SSNO(1) = BWLNS2-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LOANS-4-NEXT
                 ELSE
                    IF LN-CURBAL = 0
                       GO TO READ-LOANS-4-NEXT
                    ELSE
                       MOVE LN-CURBAL TO CURBAL.

      *******************************************
       READ-LOANS-5 SECTION.
      *******************************************
           MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE BWLNS2-KEY TO LN-SSNO(2)
                              QLN5-WBEG-SSNO2
                              QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE-NOT-GREATER.
       READ-LOANS-5-NEXT.
           PERFORM READ-LN5-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LN-SSNO(2) = BWLNS2-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LOANS-5-NEXT
                 ELSE
                    IF LN-CURBAL = 0
                       GO TO READ-LOANS-5-NEXT
                    ELSE
                       MOVE LN-CURBAL TO CURBAL.

      *******************************************
      *READ-LOANS-6 SECTION.
      *******************************************
      *    MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE BWLNS2-KEY TO LN-SSNO(3)
      *                       QLN6-WBEG-SSNO3
      *                       QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE-NOT-GREATER.
      *READ-LOANS-6-NEXT.
      *    PERFORM READ-LN6-FILE-PREVIOUS.
      *    IF IO-FG = 0
      *       IF LN-SSNO(3) = BWLNS2-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LOANS-6-NEXT
      *          ELSE
      *             IF LN-CURBAL = 0
      *                GO TO READ-LOANS-6-NEXT
      *             ELSE
      *                MOVE LN-CURBAL TO CURBAL.

      *******************************************
      *READ-LOANS-7 SECTION.
      *******************************************
      *    MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE BWLNS2-KEY TO LN-SSNO(4)
      *                       QLN7-WBEG-SSNO4
      *                       QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE-NOT-GREATER.
      *READ-LOANS-7-NEXT.
      *    PERFORM READ-LN7-FILE-PREVIOUS.
      *    IF IO-FG = 0
      *       IF LN-SSNO(4) = BWLNS2-KEY AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LOANS-7-NEXT
      *          ELSE
      *             IF LN-CURBAL = 0
      *                GO TO READ-LOANS-7-NEXT
      *             ELSE
      *                MOVE LN-CURBAL TO CURBAL.

      *************************************
       MORE-INFO SECTION.
      *************************************

           IF SCAN-TYPE = 1 OR 3
              MOVE FLDSEL-S-SSNO TO ALP-BW
           ELSE
              MOVE FLDSEL-N-SSNO TO ALP-BW.
           PERFORM ALPBW-TO-NUMBW.

           MOVE NUM-BW TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ BORROWER RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           MOVE 17 TO WINDOW-LINES.
           MOVE 50 TO WINDOW-COLUMNS.
           MOVE 7 TO WINDOW-BEGIN-Y.
           MOVE 12 TO WINDOW-BEGIN-X.
           MOVE FLDSEL-FIELD TO WINDOW-FIT-TO.
           MOVE "WI/BWSCN2" TO WINDOW-FORM-NAM.
           MOVE "BWSCN2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY BWSCN2-SCREEN.

           MOVE BWSCN2-FORM-FIELDS TO WR-BUF.
           MOVE BWSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE BW-FNAME TO MESS.
           MOVE BW-LNAME TO A-NAME.
           PERFORM SET-BW-FNAME.
           MOVE MESS TO M-NAME.
           PERFORM SET-CITY-STATE-ZIP.
           IF BW-ADR2 = " "
              MOVE BW-ADR1 TO M-ADR1
              MOVE MESS TO M-ADR2
              MOVE " " TO M-ADR3
           ELSE
              MOVE BW-ADR1 TO M-ADR1
              MOVE BW-ADR2 TO M-ADR2
              MOVE MESS TO M-ADR3.
           MOVE BW-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO M-PHONE.
           MOVE BW-EMPLOYER TO M-EMPL.
           MOVE BW-SPFNAME TO MESS.
           MOVE BW-SPLNAME TO A-NAME.
           PERFORM SET-BW-FNAME.
           MOVE MESS TO M-SPOUSE.
           ADD BW-LIASMLN BW-LIAOTH GIVING M-LIA.
           MOVE BW-LOANS TO M-OPEN.
           MOVE BW-PRLOANS TO M-CLOSED.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "BWLNSC" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.

      ***************************************
       BORROWER-IO-ROUTINES SECTION.
      ***************************************
       START-BORROWER.
      D   STOP MESS.
           IF SCAN-TYPE = 1
              PERFORM START-BW1-FILE
           ELSE
           IF SCAN-TYPE = 2
              PERFORM START-BW2-FILE
           ELSE
           IF SCAN-TYPE = 3
              PERFORM START-BW3-FILE
           ELSE
              PERFORM START-BW4-FILE.
       READ-BORROWER.
           IF SCAN-TYPE = 1
              PERFORM READ-BW1-FILE-NEXT
           ELSE
           IF SCAN-TYPE = 2
              PERFORM READ-BW2-FILE-NEXT
              IF (IO-FG = 0) AND (BW-OWNBR = BW-PATH-OWNBR)
                    AND (SUB = 1) AND (CURRENT-PAGE NOT = 1)
                 PERFORM READ-BW2-FILE-NEXT
                      UNTIL (BW-SSNO = PAGE-KEY-N-S(CURRENT-PAGE)) OR
                       (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              ELSE
                 NEXT SENTENCE
           ELSE
           IF SCAN-TYPE = 3
              PERFORM READ-BW3-FILE-NEXT
           ELSE
              PERFORM READ-BW4-FILE-NEXT
              IF (IO-FG = 0) AND (BW-OWNBR = BW-PATH-OWNBR)
                    AND (SUB = 1) AND (CURRENT-PAGE NOT = 1)
                 PERFORM READ-BW4-FILE-NEXT
                    UNTIL (BW-SSNO = PAGE-KEY-N-S(CURRENT-PAGE)) OR
                    (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR).

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

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY BWLNSC-SCREEN UPON BWLNSC-WINDOW-HANDLE.
           MOVE BWLNSC-FORM-FIELDS TO WR-BUF.
           MOVE BWLNSC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           IF VDU-FORM-NAME = "BWLNSC"
              MOVE BWLNSC-HELP--FILE TO HELP-LINK-FILE 
              MOVE BWLNSC-HELP--DIR  TO HELP-LINK-DIR 
           ELSE
              MOVE BWSCN2-HELP--FILE TO HELP-LINK-FILE
              MOVE BWSCN2-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BWLNSC_EVA.CPY".
              COPY "LIBWI/BWSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/CHKSEC.CPY".
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.
       SET-BW-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST
             "                                   "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-BW-FNAME.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST
             "                                   "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-CITY-STATE-ZIP.
           MOVE BW-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "  "
               BY BW-STATE.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "          "
               BY BW-ZIP.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW123 TO ALP-BW123.
           MOVE NUM-BW45 TO ALP-BW45.
           MOVE NUM-BW6789 TO ALP-BW6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBW-TO-NUMBW.
           MOVE ALP-BW123 TO NUM-BW123.
           MOVE ALP-BW45 TO NUM-BW45.
           MOVE ALP-BW6789 TO NUM-BW6789.
           IF NUM-BW NOT NUMERIC
              MOVE 0 TO NUM-BW.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************************
       END-ROUTINE SECTION.
      ****************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DESTROY BWLNSC-SCREEN.
           DESTROY BWLNSC-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
