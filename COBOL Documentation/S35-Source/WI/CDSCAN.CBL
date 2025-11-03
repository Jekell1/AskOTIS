       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDSCAN.
      ******************************************************************
      *
      *                   CODE FILE WINDOW SCAN
      *                    ALL CODES EXCEPT "GC" AND "CN" (CDBSCN)
      *
      * REV.
      * KEC 021296 ADDED LOGIC TO ACCESS SCAN FOR "CN" ON CD-CN-BRANCH.
      * KEC 021396 FOR TYPE OF "CN", DISPLAY ONLY USER-BRANCH RECORDS.
      * KEC 032296 FIXED THE SCAN FOR A CODE OF "CN".
      * KEC 032296 FOR TYPE OF "CN", DISPLAY ONLY RECORDS FOR THE BRANCH
      *            THAT IS IN CDSCAN-CN-BRANCH.
      * BLL 051096 FOR TYPE OF "EA", PASS WHETHER SCANNING FOR NORMAL
      *            EXTRACTION GROUPS OR COLLECTOR QUEUE GROUPS
      * JFF 081396 FIXED SCAN FOR "EA".
      * JFF 093096 ADDED CDSCAN-TYPE "BW".
      * JTG 110896 CHANGED TITLE DISPLAY FROM REFUSAL TO TURNDOWN
      * JTG 021797 ADDED CDSCAN-TYPE "FC"
      * BAH 031397 EXPANDED "BW" TYPE TO 4 CHARACTER
      * JTG 032797 ADDED LOGIC FOR CD-RF-STATCD
      * MJD 070997 ADDED LOGIC TO SCAN BOTH EQ AND CQ GROUPS (EA-SCAN)
      *            IF CDSCAN-EA-CQ-FG = " ".
      * MJD 071497 ADDED "CL" REDEF OF CDFILE FOR CAN GOUP ID
      * MJD 071897 FIXED BUG WITH ACTION CODE SCAN. IT ONLY DISPLAYS THE 1ST
      *            PAGE OF CODES. NEED TO TEST ACTION CODE SEPARATELY
      *            BECAUSE IT ONLY TAKE UP THE 1ST BYTE OF THE 2 BYTE CD-TYPE.
      * JTG 081397 ADDED LOGIC FOR CDSCAN-FEE-GIFTNET-ISO
      * GLF 091897 REPLACED ANY PROMPT THAT DISPLAYS WORD 'GIFTNET' W/'WCBC'
      * MJD 012898 ADDED "ML" MAIL LIST SOURCE CODE LOGIC
      * MJD 021798 ADDED "PH" PHONE NUMBER TYPE CODE LOGIC.
      * KEC 092198 ADDED "PR" PRODUCT CODE MAINTENANCE LOGIC.
      * JTG 120798 ADDED "RO" OTHER CHARGES CODE MAINTENANCE LOGIC.
      * MJD 990303 ADDED "BS" CODE FOR REFNO
      * GLF 990308 ADDED LOGIC FOR "RC" CODE
      * JTG 990729 ADDED CD-RO-NUM TO PROVIDE AN ALPHA KEY RE: CD-RO-CODE
      * BAH 990804 "LO" LPO CODES WERE ONLY DISPLAYING 3 CHARACTERS, NEEDED
      *            TO USE CD-BW-CODE
      * GLF 990830 ADD "TS" CODE FOR TRW SPECIAL CODE FOR TFC PR#51
      * KEC 2000.0428 REVISED THE ENTIRE 'BUILD-PAGENO.' PARAGRAPH
      *               BECAUSE OF THE PROBLEMS IN THE PREVIOUS COMMENT.
      * BAH 000901 ACTIONCD STILL HAD A BUG IN THE SCAN, AFTER THE READ-NEXT
      *            IT HAS TO BE TESTED SEPARATELY BECAUSE OF THE KEY..JUDY
      * MJD 001207 ADDED LOGIC TO START CODE FILE AT THE PROPER BRANCH
      *            IF SCANNING FOR CODE "CN" (TFC PR#60)
      * BLV 001207 TRASHED HOLD-EA-CQ-FG - NOT NEEDED & IT CONFUSED ME;
      *            ADDED CQSCAN-EA-GQ-FG TO SCAN ONLY FOR GLOBAL QUEUING
      *            GROUPS (OR NONE) BASED ON BRANCH QUEUING FLAG, TFC PR #60
      * BLV 010924 ADDED LOGIC FOR CD-FU-REC, FOLLOW-UP CODE, PR# 1487
      * BAH 020626 ADDED "LS" REPO REASONS AND "CH" CHARGE OFF REASON
      *             REGACC PR# 1775
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * BAH 050915 ADDED "DF" DEFERMENT (PAID THRU) REASON CODE LENDMARK PR#2568
      * JTG 050927 ADDED "UW" 32. SF VOID (UNWIND) CODES          REGACC PR#105
      * BAH 051027 ADDED "FZ" FROZEN REASON CODES, REGACC PR# 114
      *  CS 060531 ADDED "EV" EVENT CODES SECURITY PR# 2503
      *  CS 060607 REMOVED "EV" CODES WILL USE REFNO CODES
      * BLF 060908 ADDED "PA" PAYROLL TIMESHEET NOT WORKED CODES, WORLD PR# 488
      * MJD 070827 ADDED "ID" KYC IDTYPE CODES                   REGENCY PR#3032
      * BLM 110128 ADDED "JT" JOB TYPE CODES, WORLD PR# 739
      * BAH 110411 ADDED "PS" PAYMENT RESCHEDULE REASON CODES, REGACC PR# 290
      * BLM 121203 ADDED "DR" NOTES PAYABLE DORMANT REASON CODES, REGAN PR# 4150
      *  CS 140414 **A30** CHANGED TO SCREEN LINE, SCREEN COL
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-TYPE "LO" TO USE NEW LAYOUT (CD-LO-REC).
      *         CHANGED CD-CL???? TO BE CD-CL-????.
      *         CHANGED CD-ACTION???? TO BE CD-ACTION-????.
      *         CHANGED CD-AC-RE-REC TO BE SPLIT INTO CD-AT-REC &
      *         CD-RE-REC.  FIELDS CD-??-ACTIONCD, CD-??-MEMO-REQUIRED,
      *         CD-??-SUPERVISOR-ONLY ARE IDENTICAL (LOCATION, NAME,
      *         SIZE) IN SITUATIONS WERE NOT SURE WHICH TYPE CD-REC
      *         WAS READ.
      *         CHANGED CD-FC???? TO CD-FC-????
      *         CHANGED CD-LV???? TO CD-LV-????
      *         CHANGED CD-POOL???? TO CD-PL-????
      *         CHANGED CD-RC-CODE TO NO LONGER USE HASH.
      *         REMOVED CD-RC-CODE-ALPHA; USING CD-RC-CODE NOW.
      *         BUT STILL NEED TO USE CDSCAN-RC-CODE-ALPHA.
      *
      * KEC 2016.0324 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-CD-??-KEY (NEW).
      *  CS 170221 ADD LOGIC TO TELL FLDSEL TO SEARCH FIRST 2 CHARACTERS
      *    MOVE "L" TO FSEL-SEARCH-TYPE.
      *    MOVE 1 TO FSEL-SEARCH-COL-BEGIN.
      *    MOVE 2 TO FSEL-SEARCH-COL-END.
      *  CS 180403 SET FSEL-SEARCH-COL-END BACK TO 1, COULDNT FIND THE
      *            REASON I CHANGED IT.
      * BAH 02.10.2020 ADDED CDSCAN-TYPE "OR" #1421
      * BAH 20220420 ADDED CDSCAN-TYPE "A2" ACTION CODE 2, SECURITIZATION  #1551
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/CDSCAN.CBL S35 09/20/22-14:04:32 >".

      ******************************************************************
      *    W O R K E R S
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  CDSCAN-LINES                PIC 9(02)  VALUE 15.
       01  CDSCAN-COLUMNS              PIC 9(02)  VALUE 39.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

      ******************************************************************
      *
      *    F L D S E L
      *
      ******************************************************************
       01  FLDSEL-FIELD                        PIC X(04)   VALUE "010.".
       01  FILLER                    REDEFINES FLDSEL-FIELD.
           03  FLDSEL-ELE                      PIC 9(02).
           03  FILLER                          PIC X(02).

       01  FLDSEL-STAT                         PIC XX.
       01  FLDSEL-LIST.
           03  FILLER                          PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

      *-----------------------------------------------------------------
       01  FLDSEL-BUF                          PIC X(35).
       01  FLDSEL-CD1-BUF            REDEFINES FLDSEL-BUF.
           03  FLDSEL-CODE                     PIC X(08).
           03  FILLER                          PIC X(02).
           03  FLDSEL-DESC                     PIC X(25).
      *-----------------------------------------------------------------
       01  FIRST-FIELD                         PIC X(4)  VALUE "010.".
       01  LAST-FIELD                          PIC X(4)  VALUE "090.".

       01  CDSCAN-WINDOW-HANDLE               PIC X(10).

      ******************************************************************
      *
      *    D I S P L A Y
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER                        PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.
           03  D-LINE                        OCCURS 9.
               05  D-CODE                    PIC X(8).
               05  FILLER                    PIC X(2).
               05  D-DESC                    PIC X(25).
           03  D-PAGENO                      PIC ZZ9.
           03  D-END-MORE                    PIC X(07).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DISPLAY-LV-BUF          REDEFINES DISPLAY-BUF.
           03  D-LV-LINE                     OCCURS 9.
               05  D-LV-LEVEL                PIC 9(1).
               05  FILLER                    PIC X(2).
               05  D-LV-DESC                 PIC X(32).
           03  FILLER                        PIC X(10).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DISPLAY-PL-BUF          REDEFINES DISPLAY-BUF.
           03  D-PL-LINE                     OCCURS 9.
               05  D-PL-POOLID               PIC 9(5).
               05  FILLER                    PIC X(2).
               05  D-PL-DESC                 PIC X(28).
           03  FILLER                        PIC X(10).
      *-----------------------------------------------------------------
       01  CURRENT-PAGE                      PIC S999.
       01  PAGE-COUNT                        PIC 999.
       01  MAX-PAGES                         PIC 999    VALUE 50.

      ******************************************************************
      *
      *    P A G E - W O R K E R S
      *
      ******************************************************************
       01  PAGE-KEY-WORKERS.
           03  PAGE-KEY              OCCURS 50 PIC X(20).

      * LEAVING 20 SO I DONT HAVE TO CHANGE THE FORM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-KEY-WORKERS-CD1      REDEFINES PAGE-KEY-WORKERS.
           03  PAGE-KEY-CD1                    OCCURS 50.
               05  PAGE-KEY-TYPE               PIC X(2).
               05  PAGE-KEY-CODE               PIC X(8).
               05  PAGE-KEY-FILLER             PIC X(10).

      *-----------------------------------------------------------------
      *    SAVING CD1-KEY FOR EACH RECORD (9) DISPLAYED IN WINDOW
      *-----------------------------------------------------------------
       01  PAGE-SEL-WORKERS.
           03  PAGE-SEL-CD1-KEY      OCCURS 9  PIC X(20).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CDSCAN_DEF.CPY".
       COPY "LIBWI/CDSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CDSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CDSCAN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME CDSCAN-BUF EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "XX" TO CDSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CDSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE CDSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  CDSCAN-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    CDSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CDSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CDSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.

           MOVE "S  A300TITLE." TO VDU.

           IF CDSCAN-TYPE = "AC"
              MOVE "    ACTION CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "A2"
              MOVE "    ACTION CODE 2 SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "AT"
              MOVE "   ACTIVITY CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "BR"
              MOVE " BATCH PAYMENT CODE SELECTIO" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "BS"
              MOVE " REFERENCE  CODE  SELECTION " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "BW"
              MOVE " BORR HOUSING CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "CH"
              MOVE "CHARGE OFF REASON CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "CL"
              MOVE "  CAN LOAN GROUP ID SELECT  " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "CO"
              MOVE "  COLLATERAL CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "DF"
              MOVE "  PAID THRU MODIFICATION CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "DR"
              MOVE "NOTES PAYABLE DORMANT REASON CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "EA"
              MOVE " EXTRACTION GROUP SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "EL"
              MOVE " EXTENDED LATE CHARGE CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "FC"
              MOVE " FORM CHARGES CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "FZ"
              MOVE "FROZEN ACCT REASON CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "ID"
              MOVE "     KYC 'ID TYPE' CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "JT"
              MOVE "       JOB TYPE CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "LO"
              MOVE "LOAN PROCESS OFFICE SELECTION " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "LS"
              MOVE "  REPO REASON CODE SELECTION  " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "LV"
              MOVE "  GROUP LEVEL CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "OR"
              MOVE " BORROWER ORIGIN CODE" TO VDUBUF
           ELSE

           IF CDSCAN-TYPE = "ML"
              MOVE "    MAIL LIST SOURCE CODES  " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PA"
              MOVE "PAYROLL 'NOT WORKED' CODES" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PC"
              MOVE "    PROCESS CODE SELECTION    " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PH"
              MOVE "   PHONE NUMBER TYPE CODE   " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PL"
              MOVE " DEALER POOL CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PO"
              MOVE "    PAYOFF CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PR"
              MOVE "    PRODUCT CODE SELECTION    " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PS"
              MOVE "PAYMENT RESCHEDULE REASON" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PU"
              MOVE "    PURPOSE CODE SELECTION " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "PX"
              MOVE "   P&L TYPE CODE SELECTION " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "RC"
              MOVE "      REP CODE SELECTION      " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "RE"
              MOVE "    RESULT CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "RO"
              MOVE " OTHER CHARGES CODE SELECTION " TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "SR"
              MOVE "    SOURCE CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "TS"
              MOVE " TRW SPECIAL CODE SELECTION" TO VDUBUF
           ELSE
           IF CDSCAN-TYPE = "UW"
              MOVE "UNWIND REASON CODES" TO VDUBUF.

           PERFORM SCREENIO.

      D    STOP MESS.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-CD1-FILE.

           MOVE CDSCAN-TYPE       TO PAGE-KEY-TYPE  (PAGE-COUNT).
           MOVE SPACES            TO PAGE-KEY-CODE  (PAGE-COUNT).
           MOVE SPACES            TO PAGE-KEY-FILLER(PAGE-COUNT).

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

           MOVE SPACES                 TO PAGE-SEL-WORKERS.
           MOVE PAGE-KEY(CURRENT-PAGE) TO CD1-KEY.
           PERFORM START-CD1-FILE.

       BUILD-PAGENO.

           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO DISPLAY-PAGE.

           IF ( CD-TYPE NOT = CDSCAN-TYPE )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF ( SUB > 9 )
              GO TO DISPLAY-PAGE.

           IF ( CDSCAN-TYPE       = "EA"            ) AND
              ( CD-EA-CQ-FG   NOT = CDSCAN-EA-CQ-FG ) AND
              ( CDSCAN-EA-CQ-FG NOT = SPACES        )
              GO TO BUILD-PAGENO.

           IF ( CDSCAN-TYPE       = "EA"            ) AND
              ( CD-EA-GQ-FG   NOT = CDSCAN-EA-GQ-FG ) AND
              ( CDSCAN-EA-GQ-FG NOT = SPACES        )
              GO TO BUILD-PAGENO.

      * VALID 'CD' RECORD; LOAD RECORD TO DISPLAY TO SCREEN

           MOVE CD1-KEY                    TO PAGE-SEL-CD1-KEY(SUB).

           IF ( CDSCAN-TYPE = "LV" )
              MOVE CD-LV-LEVEL             TO D-LV-LEVEL(SUB)
              MOVE CD-DESC                 TO D-LV-DESC (SUB)
           ELSE
           IF ( CDSCAN-TYPE = "PL" )
              MOVE CD-PL-POOLID            TO D-PL-POOLID(SUB)
              MOVE CD-DESC                 TO D-PL-DESC  (SUB)
           ELSE
              MOVE CD-CODE                 TO D-CODE(SUB)
              MOVE CD-DESC                 TO D-DESC(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.

      * NEED TO TEST ACTION CODE SEPARATELY BECAUSE IT ONLY TAKES UP
      * THE 1ST BYTE OF THE 2 BYTE CD-TYPE.
           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-FG = 0 ) AND ( CDSCAN-TYPE = CD-TYPE )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE CD1-KEY TO PAGE-KEY(PAGE-COUNT)
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

       SELECT-FIELD-FROM-DISPLAY.

           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "L" TO FSEL-SEARCH-TYPE.
           MOVE 1 TO FSEL-SEARCH-COL-BEGIN.
           MOVE 1 TO FSEL-SEARCH-COL-END.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
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

           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * SEND THE SELECTED CD1-KEY BACK VIA CDSCAN-CD1-KEY

           MOVE PAGE-SEL-CD1-KEY(FLDSEL-ELE) TO CDSCAN-CD1-KEY.
           MOVE "00"                         TO CDSCAN-STATUS.

           GO TO END-ROUTINE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           DISPLAY CDSCAN-SCREEN UPON CDSCAN-WINDOW-HANDLE.
           MOVE CDSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CDSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE CDSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDSCAN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CDSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CDSCAN-SCREEN.
           DESTROY CDSCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
