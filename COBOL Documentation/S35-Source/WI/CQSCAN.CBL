       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CQSCAN.
      ******************************************************************
      *
      *              COLLECTOR QUEUE FILE WINDOW SCAN
      *                 CALLED FROM SP/SPINQ0
      *
      * REV:
      *  JTG 012596 CHANGED MORE WINDOW TO ALLOW F1-TOGGLE BETWEEN
      *             WORKED AND UNWORKED
      *  BLL 031296 CHANGED MORE WINDOW TO DISPLAY DEL, BAL; SEQ OUT
      *  BLL 032196 DON'T GO TO IO-ERROR IF NO CQ HEADER; SEND MESSAGE
      *  KEC 041096 ADDED OPTION TO CHANGE COLLECTOR QUEUE.
      *  KEC 041096 ADDED OPTION TO TOGGLING THE TYPE OF ACCOUNTS:
      *             (WORKED/UNWORKED/RESCHEDULED).  ALSO DISPLAYED THE
      *             THE RESCHEDULED DATE. (ALL IN THE SECONDARY WINDOW).
      *  KEC 042696 FIXED THE CQ-DATE CONVERSION (LIBSP/SPSETCQ).
      *  BLV 110596 ALLOW SELECTION OF STARTING DELINQUENCY IN MORE-INFO
      *  BLV 031397 DISPLAY RESULT CODE ON MORE WINDOW IF WORKED
      *  BLV 041497 CHECK ORDER OF DELINQUENCY USING BR-COL-QUEUE-LOW-TO-HI
      *  BLV 063097 COMPUTE CURRENT DELINQUENCY INSTEAD OF USING CQ
      *  BLV 070297 IF BROKEN PROMISES SELECTED AS FIRST IN QUEUE,
      *             DISPLAY "PRIORITY" & PROMISED DATE IN SCAN
      *  BLV 082697 ALL ACCTS SHOWING AS MATURED - SYSDATE WAS 9(6)
      *             INSTEAD OF X(8) - LPSTAT DISPLAY EXPECTED X(8)
      *  CLS 092397 ADDED DISPLAY OF LN-RECODE TO BROKEN PROMISE SCAN
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  BLV 000911 IN DETAIL INQUIRY, IF NOT A RESCHEDULE & USING TIME
      *             ZONE QUEUEING, DISPLAY CURRENT CUST TIME, TFC PR# 25
      *  BLV 120500 IF I-AM-WORLD, DISPLAY RECENCY INSTEAD OF CONTRACTUAL,
      *             DISPLAY AVAILABLE INSTEAD OF BALANCE, WORLD PR# 244;
      *             NOTE, IN A10 I ADDED A ZILLION COPY MEMBERS NEEDED
      *             FOR AVAILABLE CREDIT ROUTINES, IN A15 I CALLED
      *             CRNORC.
      *  BLV 010207 FIX LOOP IF NO DISPLAYABLE RECORDS OF ANY TYPE FOR
      *             A COLLECTOR
      *  BLV 040616 ADD DISPLAY OF BEHAVIOR GRADE ON DETAIL LINE IF
      *             BR-COL-QUEUE-BEHAVIOR-FG = "Y", REGACC PR# 59
      *  BLV 040622 BY ADDING BEHAVIOR GRADE TO CQ2 KEY, THE ORDER OF
      *             THE FILE HAS BEEN SIGNIFICANTLY ALTERED.  FOR EX.
      *             USED TO BE BASICALLY BY COLLECTOR BY SEQ-FG, SO
      *             FOR EACH COLLECTOR YOU WOULD HAVE ALL THE B'S, THEN
      *             ALL THE N'S, ALL THE R'S, ETC.  NOW THE RISK IS
      *             BETWEEN THE 2 OTHER PARTS OF THE KEY, SO FOR EACH
      *             COLLECTOR YOU HAVE ALL THE 0 RISKS & B'S, THEN ALL
      *             THE 0 RISKS & N'S, ETC.  SO CHANGED READ NEXT,
      *             NOW WILL BASICALLY HAVE TO READ ALL THE RECORDS FOR
      *             A GIVEN COLLECTOR
      *  BLV 040622 SINCE THE ORDER OF QUEUE IF BR-COL-QUEUE-BEHAVE-FG
      *             = "Y" IS BEHAVIOR GRADE BY BROKEN PROMISES, THEN
      *             INCLUDE BROKEN PROMISES IN UNWORKED SCAN IF THIS
      *             FLAG IS ON.
      *
      *  KEC 070202 {PR#00000} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *             ADDED BX-REC FOR TRIAD.
      *
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *  BAH 160519 REMOVED BX, DIDNT SEE IT USED ANYWHERE
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REVOMED USING LIBSP/SPSETCQ & LIBSP/SPSETGQ.
      *         ADDED LOGIC OF CONVERTING & SETTING CQ-DATE (CCYYMMDD).
      *
      * KEC 2017.0109 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED CQSCAN-NUMBER TO CQSCAN-ACCTNO.
      *          ALSO REMOVED CQSCAN-SEQ.
      *
      * BAH 170414 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 190301 BROKE OUT CQ-FILE TO DETAIL (CQ) AND SUMMARY(CQS)
      *            REPLACED PAGE-KEY WITH PKCQ2-KEY-TABLE, ALSO SEE
      *            SPWSCQ, NEEDED A VERSION OF KEY WITH NON-COMPED FIELDS
      *            TO POPULATE E-KEY (START-TIME IS COMPED, NO ONE KNOWS
      *            WHY. USE OF SET-PKCQ2-TO-CQ AND SET-CQ2-TO-PKCQ2
      * BAH 20190705 REMOVED LRR AND LRB AND LR BEHAVIOR  PT0151
      *  CS 20210503 SAVED COPY CQSCAN.20210503
      *  CS 20210506 REMOVE PKCQ-FUTURE-KEY1,KEY2
      * SMR 2023-0925 ADDED VCQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE CQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          CQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      * BAH 2024.0201 REMOVED CRNOR2 AND PERFORMED AVAILABLE-CREDIT
      * BAH 2024.0910 COLLECTOR QUEUE INFO DISPLAY 202020, NEEDED TO READ
      *               THE CQS FILE AGAIN WHEN YOU SELECT THE FIRST QUEUE
      *               TO VIEW S35Q-126
      * SMR 2025-0318 CHANGED REFERENCES OF VCQ1 TO VCQ2 S35Q-191
      *
      * JKC 2025-0401 S35Q-191 CHANGED USE OF LIBSP/SPVCQ2IN TO BE
      *         LIBSP/SPVC2RN;DID THIS SO WE COULD REMOVE THE
      *         NEED FOR LIBSP/SPVCQ2IN
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBSP/SPFSVCQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBSP/SPFDVCQ.CPY".
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CQSCAN.CBL J35 04/23/25-21:45:47 >".

       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SPWSVCQ.CPY".
       COPY "LIBSP/SP01CQS.CPY".
       COPY "LIBSP/SP01CQS_SQL.CPY".
       COPY "LIBSP/SPWSCQS.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
      *-----------------------------------------------------------------
       01  CQSCAN-LINES               PIC 9(02)  VALUE 11.
       01  CQSCAN-COLUMNS             PIC 9(02)  VALUE 44.
       01  CQSCAN-BEGIN-Y             PIC 9(02)  VALUE 2.
       01  CQSCAN-BEGIN-X             PIC 9(02)  VALUE 2.

       01  ERRCD                   PIC X(1) VALUE SPACES.
       01  SCANTYPE                PIC X      VALUE "U".
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.
       01  WK-START-TIME           PIC 9(4).
       01  WK-START-TIMEX          REDEFINES WK-START-TIME.
           03  WK-START-HH         PIC 99.
           03  WK-START-MM         PIC 99.
       01  BAL-WK                  PIC 9(6).

       01  A-NAME                  PIC X(16).
       01  HOLD-DELINQUENCY        PIC 9(5) VALUE 0.
       01  HOLD-NAME               PIC X(20) VALUE SPACES.
       01  RECORDS-FOUND           PIC X         VALUE "N".

      *=================================================================
      *
      *    W S - F I E L D S
      *
      *=================================================================
       01  WS-CHANGE-COLLID-FG     PIC X(1)           VALUE "N".
       01  WS-CQSCAN-COLLID        PIC X(3)           VALUE SPACES.
       01  WS-VDUSTATUS            PIC X(2)           VALUE SPACES.
       01  RENEWABLE-FG            PIC X.
       01  WK-RENEWABLE-PAYS    PIC 9(4).
       01  WK-RENEWABLE-DAYS    PIC 9(4).


      *=================================================================
      *
      *    D I S P L A Y - C Q S C A N
      *
      *=================================================================
       01  DISPLAY-CQSCAN-LIST.
           03  FILLER              PIC X(34)  VALUE
           "COLLID-NAME:R 010 020 030 040 050.".

      *-----------------------------------------------------------------
       01  DISPLAY-CQSCAN-BUF.
           03  D-COLLID-NAME       PIC X(29).
           03  D-CQ-TOT-REC        PIC ZZZZ9.
           03  D-CQ-TO-WORK        PIC ZZZZ9.
           03  D-CQ-RE-SCH         PIC ZZZZ9.
           03  D-CQ-WORKED         PIC ZZZZ9.
           03  D-CQ-NEXT-SEQNO     PIC ZZZZ9.

      *=================================================================
      *
      *    F L D S E L
      *
      *=================================================================
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".

      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

      *-----------------------------------------------------------------
       01  FLDSEL-BUF.
           03  FLDSEL-ACCTNO        PIC X(13).
           03  FILLER               REDEFINES FLDSEL-ACCTNO.
               05  FILLER           PIC X(3).
               05  FLDSEL-LN-ACCTNO PIC 9(6).
               05  FILLER           PIC X(4).
           03  FILLER               PIC X.
           03  FLDSEL-NAME          PIC X(22).
           03  FLDSEL-CON-DEL       PIC ZZZ99.
           03  FILLER               PIC X.
           03  FLDSEL-BAL           PIC ZZZ,ZZ9.
           03  FILLER               PIC XX.
           03  FLDSEL-HH            PIC 99.
           03  FLDSEL-COLON         PIC X.
           03  FLDSEL-MM            PIC 99.
           03  FILLER               PIC X.
           03  FLDSEL-AM-PM         PIC XX.
           03  FILLER               PIC X(2).
           03  FLDSEL-RESCH-DATE    PIC 99/99/99.
           03  FILLER               PIC X(4).
           03  FLDSEL-WORK-COLLID   PIC XXX.

      *-----------------------------------------------------------------
       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

      *=================================================================
      *    D I S P L A Y
      *=================================================================

       01  CQSCAN-WINDOW-HANDLE    PIC X(10).
       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

       01  HEAD-LINE-LIST.
           03  FILLER              PIC X(24)  VALUE
           "HEADLINE1:S HEADLINE2:S.".

       01  HEAD-LINE-BUF.
           03  HEAD-LINE1.
               05  FILLER              PIC X(31)  VALUE SPACES.
               05  BEHAVIOR-PROMPT1    PIC X(5)   VALUE SPACES.
               05  DEL-PROMPT1         PIC X(14)  VALUE
               "CON CURRENT   ".
               05  DATE-PROMPT1        PIC X(13)  VALUE
               "RE-SCHEDULED:".
               05  FILLER              PIC X(12)  VALUE
               "  WORKED    ".
           03  HEAD-LINE2.
               05  FILLER              PIC X(31)  VALUE
               "ACCOUNT NO.   NAME             ".
               05  BEHAVIOR-PROMPT2    PIC X(5)   VALUE SPACES.
               05  DEL-PROMPT2         PIC X(14)  VALUE
               "DEL BALANCE   ".
               05  DATE-PROMPT2        PIC X(13)  VALUE
               "DATE     TIME".
               05  FILLER              PIC X(12)  VALUE
               "    BY   RES".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-LN-ACCTNO          PIC X(13).
               05  FILLER               PIC X.
               05  D-NAME               PIC X(20).
               05  D-CON-DEL            PIC ZZZ99.
               05  FILLER REDEFINES D-CON-DEL.
                   07  FILLER           PIC XX.
                   07  D-CON-DELX       PIC XXX.
               05  D-BAL                PIC ZZZ,ZZ9-.
               05  FILLER               PIC X(3).
               05  D-RESCH-DATE         PIC 99/99/99 BLANK WHEN ZEROES.
               05  FILLER               PIC X.
               05  D-TIME.
                   07  D-HH             PIC Z9.
                   07  D-COLON          PIC X.
                   07  D-MM             PIC 99.
                   07  FILLER           PIC X.
                   07  D-AM-PM          PIC XX.
               05  FILLER               PIC X.
               05  D-WORK-COLLID        PIC XXX.
               05  FILLER               PIC X(1).
               05  D-RESULT             PIC XXX.
           03  D-PAGENO                 PIC ZZ9.
           03  D-END-MORE               PIC X(7).

      *-----------------------------------------------------------------
       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 100.

      *-----------------------------------------------------------------
      *    A15 CQ2-KEY VERSION OF PAGE-KEY                KEC 2016.1005
      *-----------------------------------------------------------------
      *01  PAGE-KEY-01.
      *    03  PAGE-KEY            OCCURS 100 TIMES.
      *        05  PAGE-COLLID     PIC X(3).
      *        05  PAGE-REV-BEHAVIOR-RISK
      *                            PIC 9.
      *        05  PAGE-SEQ-FG     PIC X.
      *        05  FILLER          PIC X.
      *        05  PAGE-DATE       PIC 9(7) COMP-3.
      *        05  PAGE-START-TIME PIC 9999 COMP-3.
      *        05  PAGE-SEQ-FIELDS.
      *            07  PAGE-REV-DEL       PIC 9(5)    COMP-3.
      *            07  PAGE-REV-PDUE-DAYS PIC 9(5)    COMP-3.
      *KC>         07  PAGE-REV-BAL       PIC 9(6)    COMP-3.
      *-----------------------------------------------------------------
      *    A30 CQ2-KEY VERSION OF PAGE-KEY                KEC 2016.1005
      *-----------------------------------------------------------------
      *01  PAGE-KEY-TABLE                     OCCURS 100 TIMES.
      *    03  PAGE-CQ2-KEY                   PIC X(58).
      *    03  FILLER               REDEFINES PAGE-CQ2-KEY.
      *        05  PAGE-CQ-COLLID             PIC XXX.
      *        05  PAGE-CQ-REV-BEHAVIOR-RISK  PIC 9.
      *        05  PAGE-CQ-SEQ-FG             PIC X.
      *        05  FILLER                     PIC X(3).
      *        05  PAGE-CQ-DATE               PIC 9(8).
      *        05  PAGE-CQ-START-TIME         PIC S9(4)   COMP-3.
      *        05  PAGE-CQ-SEQ-FIELDS.
      *            07  PAGE-CQ-REV-DEL        PIC 9(5).
      *            07  PAGE-CQ-MISC-KEY-FLD   PIC X(3).
      *            07  PAGE-CQ-REV-PDUE-DAYS  PIC 9(5).
      *            07  PAGE-CQ-REV-BAL        PIC 9(7).
      *            07  PAGE-CQ-SEQ            PIC 9(3).

      *-----------------------------------------------------------------
      *    A32 CQ2-KEY VERSION OF PAGE-KEY                1/31/2019
      *-----------------------------------------------------------------
       01 PKCQ2-KEY-TABLE                     OCCURS 100 TIMES.
           03  PKCQ2-CQ2-KEY                   PIC X(59).
           03  FILLER               REDEFINES PKCQ2-CQ2-KEY.
               05  PKCQ2-BRNO               PIC 9(4).
               05  PKCQ2-COLLID             PIC XXX.
               05  PKCQ2-REV-BEHAVIOR-RISK  PIC 9.
               05  PKCQ2-SEQ-FG             PIC X.
               05  PKCQ2-DATE               PIC 9(8).
               05  PKCQ2-START-TIME         PIC S9(4)   COMP-3.
               05  PKCQ2-SEQ-FIELDS.
                   07  PKCQ2-REV-DEL        PIC 9(5).
      *KC          07  PKCQ2-MISC-KEY-FLD   PIC X(3).
                   07  PKCQ2-REV-PDUE-DAYS  PIC 9(5).
                   07  PKCQ2-REV-BAL        PIC 9(7).
                   07  PKCQ2-SEQ            PIC 9(3).  
      

      *-----------------------------------------------------------------
       01  PAGE-ACCT-01.
           03  PAGE-ACCT-TAB       OCCURS 100 TIMES.
               05  PAGE-ACCTNO     PIC 9(8) COMP-3.
       01  LEGEND-WK               PIC X(76) VALUE SPACES.
       01  MORE-PROMPT.
           03  FILLER              PIC X(44) VALUE
           "SELECT LOAN,F1-TOGGLE(BROKEN/UNWORKED/WORKED".
           03  FILLER              PIC X(32) VALUE
           "/RESCHED),F6-DELINQUENCY,F7-EXIT".


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETDLSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBWI/CQSCN3W.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CQSCAN_DEF.CPY".
       COPY "LIBWI/CQSCN2_DEF.CPY".
       COPY "LIBWI/CQSCAN_WKS.CPY".
       COPY "LIBWI/CQSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBLP/LPPDUEW.CPY".

       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBGB/RBACTW.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CQSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CQSCAN_SCN.CPY".
       COPY "LIBWI/CQSCN2_SCN.CPY".

      ******************************************************************
      *     P R O C E D U R E - D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME CQSCAN-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 VCQ-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CQS1-FILE.
           PERFORM CLOSE-CQ2-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE VCQ-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP CQSCAN-VCQ-PATH.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM GET-GPENV.

           MOVE "XX"            TO CQSCAN-STATUS.

           MOVE CQSCAN-VCQ-PATH TO VCQ-PATH.  
           MOVE " "             TO VDU-WIMENU-SELECTION.
           MOVE "CQSCAN"        TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CQSCAN-BEGIN-Y
                   AT COL  CQSCAN-BEGIN-X
                   SIZE    CQSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CQSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CQSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE CQSCAN-COLLID TO WS-CQSCAN-COLLID.

           PERFORM OPEN-BR-FILE.
           MOVE CQSCAN-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH &&&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

      * NEED TO MAKE SURE THE QUEUE WAS BUILT FIRST
           IF ( CQSCAN-VCQ-PATH NOT = SPACES )
              MOVE CQSCAN-VCQ-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT NOT = "00"
                 MOVE "QUEUE MUST BE BUILT FIRST" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF BR-COL-QUEUE-BEHAVIOR-FG = "Y"
              MOVE "BEH" TO BEHAVIOR-PROMPT1
              MOVE "GRD" TO BEHAVIOR-PROMPT2.

           PERFORM OPEN-SP1-FILE.

      *-----------------------------------------------------------------
       ACCESS-COLLID.

           PERFORM OPEN-CQS1-FILE.

           MOVE CQSCAN-COLLID TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.
           PERFORM CLOSE-CQS1-FILE.

           IF ( IO-FG = 9 )
              PERFORM INVALID-COLLID
              IF ( CQSCN3-STATUS = "1>" )
                 MOVE "XX" TO CQSCAN-STATUS
                 GO TO END-ROUTINE
               ELSE
      * NEED TO READ THE ONE YOU SELECTED
                 PERFORM OPEN-CQS1-FILE
                 MOVE CQSCAN-COLLID TO CQS-COLLID
                 PERFORM READ-CQS1-FILE
                 PERFORM CLOSE-CQS1-FILE.

           MOVE SPACES             TO MESS.

           MOVE "CN"               TO CDB-TYPE.    
           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.   
           MOVE CQSCAN-COLLID      TO CDB-CODE.    
           PERFORM GET-CDB-FILE.

           IF ( IO-FG = 9 )
              STRING " ",CQSCAN-COLLID," - ","< NOT ON FILE >"
                     DELIMITED BY "  " INTO MESS
           ELSE
              STRING " ",CQSCAN-COLLID," - ",CDB-DESC
                     DELIMITED BY "  " INTO MESS.

           MOVE MESS       TO D-COLLID-NAME.
           MOVE CQS-TOT-REC TO D-CQ-TOT-REC.

           SUBTRACT CQS-WORKED-REC FROM CQS-TOT-REC
                                  GIVING D-CQ-TO-WORK.

           MOVE CQS-TIMED-REC TO D-CQ-RE-SCH.
           MOVE CQS-WORKED-REC TO D-CQ-WORKED.
      *--->MOVE CQ-NEXT-SEQNO TO D-CQ-NEXT-SEQNO.

           MOVE DISPLAY-CQSCAN-BUF TO WR-BUF.
           MOVE DISPLAY-CQSCAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "9" TO FLDSEL-STAT.

      *-----------------------------------------------------------------
       ENTER-SEL.

           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "16" )
              PERFORM SELECT-COLLID
              IF ( IO-FG = ZEROES )
                 MOVE CQSCN3-COLLID TO CQSCAN-COLLID
                 MOVE "Y"           TO WS-CHANGE-COLLID-FG
                 GO TO ACCESS-COLLID
              ELSE
                 GO TO ACCESS-COLLID.

           IF ( VDUSTATUS = "1<" )
              PERFORM MORE-INFO
              IF ( WS-VDUSTATUS = "1>" )
                 GO TO ACCESS-COLLID
              ELSE
                 GO TO END-ROUTINE.

           IF ( VDUSTATUS NOT = "1>" )
              GO TO ENTER-SEL.

           MOVE VDUSTATUS TO FLDSEL-STAT.
           GO TO END-ROUTINE.

      ******************************************************************
       MORE-INFO SECTION.
      ******************************************************************
           MOVE 17 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 6 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "WI/CQSCN2" TO WINDOW-FORM-NAM.
           MOVE "CQSCN2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY CQSCN2-SCREEN.

           MOVE CQSCN2-FORM-FIELDS TO WR-BUF. 
           MOVE CQSCN2-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

           MOVE CQSCAN-BR-LOAN-NUM-FG TO BR-LOAN-NUM-FLAG.
           MOVE CQSCAN-SP-EXPD-FRMLA TO SP-EXPD-FRMLA.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO AGEING-DATE.
           MOVE AGEING-DATE   TO DLS-DATE.
           PERFORM GET-DLS.
           IF BR-COL-QUEUE-BEHAVIOR-FG = "Y"
              MOVE "U" TO SCANTYPE
           ELSE
              MOVE "B" TO SCANTYPE.
           MOVE SPACES TO WS-VDUSTATUS
           MOVE "N" TO RECORDS-FOUND.
           GO TO ENTER-SCANTYPE-CONT.

       ENTER-SCANTYPE.

           IF ( SCANTYPE = "B" )
              MOVE "U" TO SCANTYPE
           ELSE
           IF ( SCANTYPE = "U" )
              MOVE "R" TO SCANTYPE
           ELSE
           IF ( SCANTYPE = "R" )
              MOVE "W" TO SCANTYPE
           ELSE
              MOVE "B" TO SCANTYPE.

           IF BR-COL-QUEUE-BEHAVIOR-FG = "Y"
              IF SCANTYPE = "B"
                 MOVE "U" TO SCANTYPE.

       ENTER-SCANTYPE-CONT.

           MOVE "S  A230DESC." TO VDU.

           IF ( SCANTYPE = "U" )
              MOVE "OF UNWORKED ACCOUNTS" TO VDUBUF
           ELSE
           IF ( SCANTYPE = "W" )
              MOVE "OF WORKED ACCOUNTS" TO VDUBUF
           ELSE
           IF ( SCANTYPE = "B" )
              MOVE "OF BROKEN PROMISES" TO VDUBUF
           ELSE
              MOVE "OF RESCHEDULED ACCOUNTS" TO VDUBUF.

           PERFORM SCREENIO.

           MOVE "REC AVAILABLE " TO DEL-PROMPT1.
           MOVE "DEL CREDIT    " TO DEL-PROMPT2.
           IF SCANTYPE = "R"
              MOVE "RE-SCHEDULED:" TO DATE-PROMPT1
              MOVE "DATE     TIME" TO DATE-PROMPT2
           ELSE
           IF SCANTYPE = "B"
              IF GP-TIME-ZONE-Q NOT = "Y"
                 MOVE "PROMISED" TO DATE-PROMPT1
                 MOVE "DATE"     TO DATE-PROMPT2
              ELSE
                 MOVE "PROM     CUST" TO DATE-PROMPT1
                 MOVE "DATE     TIME" TO DATE-PROMPT2
              END-IF
           ELSE
              IF GP-TIME-ZONE-Q NOT = "Y"
                 MOVE "QUEUED" TO DATE-PROMPT1
                 MOVE "DATE" TO DATE-PROMPT2
                 IF BR-COL-QUEUE-BEHAVIOR-FG = "Y"
                    MOVE "QUE/PROM" TO DATE-PROMPT1
                 END-IF
              ELSE
                 MOVE "QUEUED   CUST" TO DATE-PROMPT1
                 MOVE "DATE     TIME" TO DATE-PROMPT2.
           MOVE HEAD-LINE-LIST TO WR-LIST.
           MOVE HEAD-LINE-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-DELINQUENCY.
           GO TO BEGIN-CQ-SCAN.

       ENTER-DELINQUENCY.
           MOVE "EN N040DEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MORE-INFO-CLOSE-WINDOW.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DELINQUENCY.
           IF CQSCAN-BR-LOW-TO-HI = "Y"
              MOVE VDUNUM0 TO HOLD-DELINQUENCY
           ELSE
              COMPUTE HOLD-DELINQUENCY = 99000 - VDUNUM0.

       BEGIN-CQ-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           IF ( CQSCAN-VCQ-PATH = SPACES )
              PERFORM OPEN-CQ2-FILE
           ELSE
              PERFORM OPEN-VCQ2-FILE.

           MOVE SPACES         TO PKCQ2-CQ2-KEY          (PAGE-COUNT).
           MOVE CQSCAN-BRANCH  TO PKCQ2-BRNO             (PAGE-COUNT).
           MOVE CQSCAN-COLLID  TO PKCQ2-COLLID           (PAGE-COUNT).
           MOVE 0              TO PKCQ2-REV-BEHAVIOR-RISK(PAGE-COUNT).
           MOVE 0              TO PKCQ2-START-TIME       (PAGE-COUNT)
                                  PKCQ2-DATE             (PAGE-COUNT).
           MOVE HOLD-DELINQUENCY
                               TO PKCQ2-REV-DEL          (PAGE-COUNT).
           MOVE 0              TO PKCQ2-REV-PDUE-DAYS    (PAGE-COUNT)
                                  PKCQ2-REV-BAL          (PAGE-COUNT)
                                  PKCQ2-SEQ              (PAGE-COUNT).

           IF ( SCANTYPE = "R" )
              MOVE "R"         TO PKCQ2-SEQ-FG           (PAGE-COUNT)
           ELSE
           IF ( SCANTYPE = "B" ) OR ( SCANTYPE = "W" )
              MOVE "B"         TO PKCQ2-SEQ-FG           (PAGE-COUNT)
           ELSE
              MOVE "N"         TO PKCQ2-SEQ-FG           (PAGE-COUNT).

           IF ( BR-COL-QUEUE-BEHAVIOR-FG = "Y" )
              IF ( SCANTYPE = "U" )
                 MOVE "B"      TO PKCQ2-SEQ-FG           (PAGE-COUNT).

      ***  MOVE 1              TO PKCQ2-DATE             (PAGE-COUNT).

      *-----------------------------------------------------------------
       START-BUILD-PAGENO.
      D    STOP CQSCAN-VCQ-PATH.
      
           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD   TO FLDSEL-FIELD
              MOVE 1             TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD   TO FLDSEL-FIELD
              MOVE 1             TO CURRENT-PAGE.

           MOVE SPACES           TO DISPLAY-BUF.
           MOVE 1                TO SUB.

           IF ( CQSCAN-VCQ-PATH NOT = SPACES )
              MOVE CURRENT-PAGE  TO PAGE-COUNT.
 
           PERFORM SET-PKCQ2-TO-CQ2.

           IF CQ-DATE = 0
              MOVE 19000101      TO CQ-DATE.

           IF ( CQSCAN-VCQ-PATH = SPACES )
              PERFORM START-CQ2-FILE
           ELSE
              PERFORM LOAD-VCQ2-KEY
              PERFORM START-VCQ2-FILE.
      D    STOP CQSCAN-VCQ-PATH.

       BUILD-PAGENO.

           IF ( CQSCAN-VCQ-PATH = SPACES )
              PERFORM READ-CQ2-FILE-NEXT
           ELSE
              PERFORM READ-VCQ2-FILE-NEXT.

           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF ( CQSCAN-VCQ-PATH NOT = SPACES )
              MOVE VCQ-CQ-REC TO CQ-REC.

           IF ( CQ-BRNO NOT = CQSCAN-BRANCH )
               OR CQ-COLLID NOT = CQSCAN-COLLID
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB = 1
              IF CURRENT-PAGE > 1
                 IF CQ-ACCTNO NOT = PAGE-ACCTNO(CURRENT-PAGE)
                    GO TO BUILD-PAGENO.

           IF ( SCANTYPE = "U" ) OR ( SCANTYPE = "R" )
              IF ( CQ-WORKED-FG = "Y" )
                 GO TO BUILD-PAGENO
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ( SCANTYPE = "W" )
                 IF ( CQ-WORKED-FG NOT = "Y" )
                    GO TO BUILD-PAGENO.

           IF SCANTYPE = "B"
              IF BR-COL-QUEUE-BEHAVIOR-FG NOT = "Y"
                 IF CQ-SEQ-FG NOT = "B"
                    GO TO BUILD-PAGENO.

           IF SCANTYPE = "B"
              IF CQ-WORKED-FG = "Y"
                    GO TO BUILD-PAGENO.

           IF SCANTYPE = "U"
              IF BR-COL-QUEUE-BEHAVIOR-FG = "Y"
                 IF NOT (CQ-SEQ-FG = "N" OR "B")
                    GO TO BUILD-PAGENO
                 END-IF
              ELSE
                 IF CQ-SEQ-FG NOT = "N"
                    GO TO BUILD-PAGENO.

           IF SCANTYPE = "R"
              IF CQ-SEQ-FG NOT = "R"
                    GO TO BUILD-PAGENO.

           IF ( HOLD-DELINQUENCY NOT = ZEROES )
              IF ( SCANTYPE = "U" )
                 PERFORM SET-CQ2-TO-PKCQ2 
                 MOVE HOLD-DELINQUENCY TO PKCQ2-REV-DEL  (PAGE-COUNT)
                 MOVE ZEROES       TO PKCQ2-REV-PDUE-DAYS(PAGE-COUNT)
                                      PKCQ2-REV-BAL      (PAGE-COUNT)
                                      PKCQ2-SEQ          (PAGE-COUNT)
                 PERFORM SET-PKCQ2-TO-CQ2 
                 IF (CQSCAN-VCQ-PATH = SPACES)
                    PERFORM START-CQ2-FILE
                    MOVE ZEROES            TO HOLD-DELINQUENCY
                    GO TO BUILD-PAGENO
                 ELSE
                    PERFORM LOAD-VCQ2-KEY
                    PERFORM START-VCQ2-FILE
                    MOVE ZEROES            TO HOLD-DELINQUENCY
                    GO TO BUILD-PAGENO.

           IF SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE SPACES TO D-LINE(SUB).
           MOVE CQ-BRNO   TO LN-OWNBR.
           MOVE CQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN1-FILE.

           IF IO-FG = 9
              MOVE "<NOT ON FILE>" TO HOLD-NAME
           ELSE
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LN-ACCTNO(SUB)
              MOVE LN-OWNBR   TO BW-OWNBR
              MOVE LN-SSNO(1) TO BW-SSNO 
              PERFORM GET-BW1-FILE
              IF IO-FG = 9
                 MOVE "<NOT ON FILE>" TO HOLD-NAME
              ELSE
                 MOVE BW-LNAME TO MESS
                 MOVE BW-FNAME TO A-NAME
                 PERFORM SET-CQ-LNAME
                 MOVE MESS TO HOLD-NAME.

           MOVE HOLD-NAME TO D-NAME(SUB).

           PERFORM GET-SP1-FILE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY
           IF BR-RECDEL-NOT-REC = "Y"
              MOVE RDSUB TO RSUB
              MOVE RECDEL TO RECENCY.
           MOVE RDPOT30 TO CPOT30.
           MOVE RDPOTCHGOFF TO CPOTCHGOFF.
           MOVE RECENCY TO CONTRACTUAL.

           IF CPOT30 = "Y"
              MOVE "POT" TO D-CON-DELX(SUB)
           ELSE
           IF CPOT30 = "M"
              MOVE "MIS" TO D-CON-DELX(SUB)
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "PC" TO D-CON-DELX(SUB)
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "PL" TO D-CON-DELX(SUB)
           ELSE
              MOVE CONTRACTUAL TO D-CON-DEL(SUB).

           COMPUTE BAL-WK ROUNDED = LN-CURBAL - LN-OT2BAL.
           MOVE BAL-WK TO D-BAL(SUB).

           PERFORM CHECK-RENEWABILITY.
           IF RENEWABLE-FG = "Y"
              PERFORM GET-AVAILABLE-CREDIT
              MOVE AVCR-AVAILCREDIT TO D-BAL(SUB)
           ELSE
              MOVE 0 TO D-BAL(SUB).

           MOVE CQ-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-RESCH-DATE(SUB).

           IF CQ-SEQ-FG = "B" OR CQ-SEQ-FG = "R"
              IF ( CQ-WORKED-FG = "Y" )
                 MOVE CQ-DATE-QUEUED      TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY         TO D-RESCH-DATE(SUB).

           IF ( CQ-WORKED-FG = "Y" )
              MOVE LN-RECODE TO D-RESULT(SUB)
           ELSE
              MOVE SPACES TO D-RESULT(SUB).

           IF CQ-SEQ-FG = "N"
              IF GP-TIME-ZONE-Q NOT = "Y"
                 MOVE " " TO D-TIME(SUB)
              ELSE
                 COMPUTE TIME-ZONE-FACTOR =
                    CQ-START-TIME - GP-TIME-ZONE-FACTOR
                 IF GP-TIME-ZONE-DLS = "Y"
                    IF DLS-FG = "Y"
                       SUBTRACT 1 FROM TIME-ZONE-FACTOR
                    END-IF
                 END-IF
                 PERFORM ZONED-GET-TIME
                 MOVE TIME-EDIT TO D-TIME(SUB).

           IF CQ-SEQ-FG = "B"
              IF CQ-WORKED-FG NOT = "Y"
                 MOVE LN-PROMDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-RESCH-DATE(SUB)
                 MOVE "PRIORITY"  TO D-TIME(SUB)
                 MOVE LN-RECODE   TO D-RESULT(SUB).

           IF CQ-SEQ-FG = "B"
              IF CQ-WORKED-FG NOT = "Y"
                 IF GP-TIME-ZONE-Q NOT = "Y"
                    MOVE "PRIORITY" TO D-TIME(SUB)
                 ELSE
                    COMPUTE TIME-ZONE-FACTOR =
                       CQ-START-TIME - GP-TIME-ZONE-FACTOR
                    IF GP-TIME-ZONE-DLS = "Y"
                       IF DLS-FG = "Y"
                          SUBTRACT 1 FROM TIME-ZONE-FACTOR
                       END-IF
                    END-IF
                    PERFORM ZONED-GET-TIME
                    MOVE TIME-EDIT TO D-TIME(SUB).

           IF (CQ-START-TIME = 0) OR (CQ-SEQ-FG NOT = "R")
              OR (CQ-WORKED-FG = "Y")
              GO TO BUILD-PAGE-CONT.

           MOVE CQ-START-TIME TO WK-START-TIME.

           IF WK-START-HH > 11
              MOVE "PM" TO D-AM-PM(SUB)
           ELSE
              MOVE "AM" TO D-AM-PM(SUB).

           IF WK-START-HH > 12
              SUBTRACT 12 FROM WK-START-HH.

           MOVE WK-START-HH TO D-HH(SUB).
           MOVE ":" TO D-COLON(SUB).
           MOVE WK-START-MM TO D-MM(SUB).

       BUILD-PAGE-CONT.

           MOVE "Y" TO RECORDS-FOUND.
           MOVE CQ-WORK-COLLID TO D-WORK-COLLID(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

      D    STOP MESS.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO LEGEND-WK
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-LIST TO WR-LIST
                 MOVE DISPLAY-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 IF (RECORDS-FOUND = "N" AND SCANTYPE = "W")
                    MOVE "1>" TO WS-VDUSTATUS
                    GO TO MORE-INFO-CLOSE-WINDOW
                 ELSE
                    GO TO ENTER-SCANTYPE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    PERFORM SET-CQ2-TO-PKCQ2 
                    MOVE CQ-ACCTNO TO PAGE-ACCTNO(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO LEGEND-WK
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.

           MOVE MORE-PROMPT TO LEGEND-WK.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

           MOVE FLDSEL-STAT TO WS-VDUSTATUS.

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
              GO TO MORE-INFO-CLOSE-WINDOW.

           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANTYPE.

           IF FLDSEL-STAT = "1<"
              GO TO ENTER-DELINQUENCY.

           IF NOT (FLDSEL-STAT = "00")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-NAME = "<NOT ON FILE>"
              MOVE "INVALID SELECTION" TO MESS
              PERFORM SEND-MESS
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-LN-ACCTNO TO CQSCAN-ACCTNO.
           MOVE "00"             TO CQSCAN-STATUS.

       MORE-INFO-CLOSE-WINDOW.
           DESTROY CQSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "CQSCAN" TO VDU-FORM-NAME.

       MORE-INFO-EXIT.
           EXIT.

      ******************************************************************
       SELECT-COLLID SECTION.
      ******************************************************************

           MOVE CQSCAN-COLLID      TO CQSCN3-COLLID.
           MOVE "WI/CQSCN3"        TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CQSCN3-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( CQSCN3-STATUS NOT = "00" )
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SELECT-COLLID.
           EXIT.

      ******************************************************************
       INVALID-COLLID SECTION.
      ******************************************************************

       SELECT-ANOTHER-COLLID.

           PERFORM SELECT-COLLID.
           MOVE CQSCN3-COLLID TO CQSCAN-COLLID.

           IF IO-FG NOT = 0
              IF ( CQSCN3-STATUS = "1>" )
                 GO TO EXIT-INVALID-COLLID
              ELSE
                 GO TO SELECT-ANOTHER-COLLID.

       EXIT-INVALID-COLLID.
           EXIT.

      ******************************************************************
       CHECK-RENEWABILITY SECTION.
      ******************************************************************
           MOVE "Y" TO RENEWABLE-FG.
           IF SP-RENEWABLE-PAYS = 0
              IF SP-RENEWABLE-DAYS = 0
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           IF SP-RENEWABLE-PAYS = -1
              COMPUTE WK-RENEWABLE-PAYS = LN-ORGTERM / 3
              IF PDTH-PAYMENTS < WK-RENEWABLE-PAYS
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY
              END-IF
           ELSE
           IF PDTH-PAYMENTS < SP-RENEWABLE-PAYS
              MOVE "N" TO RENEWABLE-FG
              GO TO EXIT-CHECK-RENEWABILITY.
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE AGEING-DATE TO SYS-DATE.
           PERFORM TIM.
           IF SP-RENEWABLE-DAYS = -1
              COMPUTE WK-RENEWABLE-DAYS = (LN-ORGTERM / 3) * 30 + 1
              IF ELAPSED-DAYS < WK-RENEWABLE-DAYS
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY
              END-IF
           ELSE
           IF ELAPSED-DAYS < SP-RENEWABLE-DAYS
              MOVE "N" TO RENEWABLE-FG.
       EXIT-CHECK-RENEWABILITY.
           EXIT.

      ******************************************************************
       GET-AVAILABLE-CREDIT SECTION.
      ******************************************************************

           IF BR-AVAIL-CREDIT = "Y"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE AGEING-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
           IF BR-AVAIL-CREDIT = "S"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE AGEING-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
              MOVE 0 TO AVCR-AVAILCREDIT.

      ****************************************************************
       FORM-RESET SECTION.
      ****************************************************************
           DISPLAY CQSCAN-SCREEN UPON CQSCAN-WINDOW-HANDLE.
           MOVE CQSCAN-FORM-FIELDS TO WR-BUF. 
           MOVE CQSCAN-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 
       FORM-RESET-EXIT.
           EXIT.

      ****************************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************************
           MOVE CQSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQSCAN-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CQSCAN_EVA.CPY".
              COPY "LIBWI/CQSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *-----------------------------------------------------------------
       SET-CQ-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".

      *-----------------------------------------------------------------
       GET-CDB-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

      *-----------------------------------------------------------------
       GET-LN1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      *-----------------------------------------------------------------
       GET-BW1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

      *-----------------------------------------------------------------
       GET-SP1-FILE.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
              PERFORM FILE-ERRORS
              PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE LEGEND-WK TO VDUBUF.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       SET-PKCQ2-TO-CQ2.
      * USED TO MOVE PAGE-CQ2-KEY TO CQ2-KEY.
           MOVE PKCQ2-BRNO(PAGE-COUNT)           TO CQ-BRNO.
           MOVE PKCQ2-COLLID(PAGE-COUNT)         TO CQ-COLLID.
           MOVE PKCQ2-REV-BEHAVIOR-RISK(PAGE-COUNT)    
                                          TO CQ-REV-BEHAVIOR-RISK.
           MOVE PKCQ2-SEQ-FG(PAGE-COUNT)         TO CQ-SEQ-FG.
           MOVE PKCQ2-DATE(PAGE-COUNT)           TO CQ-DATE.
           MOVE PKCQ2-START-TIME(PAGE-COUNT)     TO CQ-START-TIME.
           MOVE PKCQ2-REV-DEL(PAGE-COUNT)        TO CQ-REV-DEL.
      *KC  MOVE PKCQ2-MISC-KEY-FLD(PAGE-COUNT)   TO CQ-MISC-KEY-FLD.
           MOVE PKCQ2-REV-PDUE-DAYS(PAGE-COUNT)  TO CQ-REV-PDUE-DAYS.
           MOVE PKCQ2-REV-BAL(PAGE-COUNT)        TO CQ-REV-BAL.
           MOVE PKCQ2-SEQ(PAGE-COUNT)            TO CQ-SEQ.
           IF CQ-DATE = 0
              MOVE 19000101                      TO CQ-DATE.
      *-----------------------------------------------------------------
       SET-CQ2-TO-PKCQ2.
      * USED TO MOVE CQ2-KEY TO PAGE-CQ2-KEY.

           MOVE CQ-BRNO              TO PKCQ2-BRNO(PAGE-COUNT).      
           MOVE CQ-COLLID            TO PKCQ2-COLLID(PAGE-COUNT).      
           MOVE CQ-REV-BEHAVIOR-RISK  
                                 TO PKCQ2-REV-BEHAVIOR-RISK(PAGE-COUNT).
           MOVE CQ-SEQ-FG            TO PKCQ2-SEQ-FG(PAGE-COUNT).      
           MOVE CQ-DATE              TO PKCQ2-DATE(PAGE-COUNT).        
           MOVE CQ-START-TIME        TO PKCQ2-START-TIME(PAGE-COUNT).
           MOVE CQ-REV-DEL           TO PKCQ2-REV-DEL(PAGE-COUNT).
      *KC  MOVE CQ-MISC-KEY-FLD      TO PKCQ2-MISC-KEY-FLD(PAGE-COUNT).
           MOVE CQ-REV-PDUE-DAYS     TO PKCQ2-REV-PDUE-DAYS(PAGE-COUNT).
           MOVE CQ-REV-BAL           TO PKCQ2-REV-BAL(PAGE-COUNT).
           MOVE CQ-SEQ               TO PKCQ2-SEQ(PAGE-COUNT).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/ZONEDTIMEIO.CPY".
       COPY "LIBGB/GETDLS.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBGB/RBACT.CPY".

      *****************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
      *COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBSP/SPCQSGS_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBSP/SPCQS1RN.CPY".
       COPY "LIBSP/SPCQ1IN.CPY".
       COPY "LIBSP/SPCQ2RN.CPY".
       COPY "LIBSP/SPVCQ2RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CQSCAN-SCREEN.
           DESTROY CQSCAN-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           PERFORM CLOSE-FILES.
       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
