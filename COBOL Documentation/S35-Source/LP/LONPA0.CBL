       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPA0.
       DATE-WRITTEN.    011884.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   GET TRANSACTION DATE
      *  REV:
      *  081795 BLL  KICK OUT IF IT'S LAST DAY OF MONTH & EOD HAS
      *              STARTED, SO THEY CAN'T OPEN THE NEXT DAY
      *  111795 KEC  A CHECK WAS ADDED IF THE THE ENTERED TRANS-DATE
      *              IS GREATER THAN THE ABSOULTE VALUE OF THE CURRENT
      *              DATE BY THREE DAYS.  IF SO, THEN A WARNING
      *              ROUTINE IS PERFORMED.
      *  121595 KEC  PASSWORDS ARE NOW REQUIRED IF THE TRANS-DATE IS
      *              GOING INTO ANOTHER MONTH OR GOES MORE THAN 10 DAYS
      *              INTO THE FUTURE.
      *  MJD 052996  ADDED LOGIC PREVENT OPENING DAY IF
      *              GP-END-FISCAL-DATE(SUB) < TRANS-DATE < BEG OF NEXT MONTH.
      *  BLV 040297  ALLOW OPENING PRIOR DAYS ON LAST DAY OF MONTH IF
      *              BR-AUTO-OPEN-BAL NOT = "Y"
      *  BLV 050897  DON'T DO LSOP FOR TEST-OPEN-DAYS; READ RCFILE
      *  BLV 050897  OPEN-OP FILE HERE IF VALID DAY; OTHERWISE YOU SIT
      *              AT MENU SELECTION WITHOUT FILE RESERVED; SOMEONE
      *              ELSE CAN CLOSE & YOU WILL GET OPEN ERR 35 ON OP!
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BAH 040298 ADDED GP-POST-INTO-FUTURE FROM A90
      *  BLV 052298  TEST PRIOR EOM COMPLETE & CALENDAR EOY COMPLETE &
      *              DON'T ALLOW OPENING DAY IN NEW MONTH UNLESS DONE;
      *              CHECKS 1ST 10 DAYS OF MONTH & BR-CHECK-EOM-FG
      *  BLV 990525  CHANGE SUB TO FISCAL-SUB, USED IN DIFFERENT
      *              AREAS - AFRAID SUB MAY GET CHANGED!
      *  BLV 990525  CHANGE VERIFY-END-FISCAL TO USE FISCAL END DATE FOR
      *              TRANS-DATE, NOT SYSTEM DATE.  ADD A CHECK TO READ
      *              RC FOR LAST DAY OF FISCAL PERIOD & DON'T ALLOW ANY
      *              DATE IN FISCAL MONTH AS TRANS-DATE IF THIS LAST
      *              DAY HAS GONE THROUGH MONTH-END-UPDATE
      *  CS  010530  ADDED PASSWORD IF ENTERING A PAST DATE AT TRANSACTION
      *              PROMPT UNLESS DAY IS OPEN. REGIONAL ACCEPT #PR1481
      *  BLV 031024  TWEAK LOGIC CHECKING TO VERIFY END-OF-YEAR HAS BEEN
      *              RUN, ADDING LOGIC TO VERIFY FISCAL EOY, PR# 2018
      *  BLF 070105  TWEAK LOGIC STOPPING OPENING A DAY IN A MONTH WHICH
      *              HAS BEEN CLOSED.  FOR EXAMPLE, IN DECEMBER, EOM &
      *              EOY HAVE BEEN RUN, FISCAL TABLE CHANGED FOR NEW
      *              YEAR.  READ WAS BEING DONE ON RCFILE TO CHECK EOM,
      *              BUT WAS USING MONTH END DATE FROM FISCAL TABLE WITH
      *              NEXT YEAR'S DATE IN IT.  CHANGED TO FORCE YEAR FROM
      *              SELECTED TRANSACTION DATE BEFORE READING RCFILE.
      *              REGWAN PL#566
      *  BLM 081221  CHECK BRANCH IN FIL-PATH, IF I-AM-WORLD & BRANCH
      *              BEGINS WITH 3, SET MEXICO-FG TO Y.  IF MEXICO,
      *              BEG FISCAL WILL BE JANUARY, BUT DON'T CHECK FOR
      *              FISCAL EOY IN JANUARY, FORCE TO CHECK IN APRIL;
      *              THEY WILL DO CALENDAR EOY IN DEC, FISCAL IN MARCH,
      *              WORLD PR# 600
      *  MJD 121214  ADDED TCLP-MAIN-WINDOW-HANDLE TO DISPLAY OF SCREEN
      *  BLM 130322  CHECK FOR ANY CLOSED FLAGS, ../ED/CLBBBBD*, AND
      *              STOP FROM OPENING DAYS IF THEY EXIST, WORLD PR# 836
      *              (1/3/13 CHANGE FROM  A15)
      *  BLM 140418  FIX PROBLEM FROM CHANGE ABOVE, WORK FILE FOR
      *              CHECKING CLOSED FLAGS NOT GETTING REMOVED IF
      *              INVALID DATE ENTERED.  REMOVE RIGHT AFTER CHECK,
      *              NOT AT END OF PROGRAM.
      *  BLM 140604  FIX PROBLEM FROM CHANGE ABOVE, WORK FILE FOR
      *              CHECKING CLOSED FLAGS NOT GETTING REMOVED IF
      *              INVALID DATE ENTERED.  REMOVE RIGHT AFTER CHECK,
      *              NOT AT END OF PROGRAM.  SUPPORT PL# 846
      *  BAH 140814 HELP-DIR WAS NOT SET, ALSO ADDED TEST FOR LPSMNU IN HELP
      *  BAH 140814 FIXED USER SIGNON MESSAGE TO DISPLAY THE 2 DIFFERENT
      *             BRANCHES CORRECTLY
      *   CS 150417 CHANGED TEST ON OPEN DAYS/CLOSED/UPDATED TO READ THE RC
      *             FILE. LEFT THE OPEN-OUTPUT-OP FOR LOCKING REASONS
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BLM 170309 CHANGE TRANS-DATE TO YYYYMMDD AND CHANGE ALL ROUTINES
      *            AND CHECKS TO USE YYYYMMDD, PD0006
      * BAH 170313 OPEN PATH DATE NEEDS TO STAY MMDDYY! OPBBBBDMMDDYY
      * BAH 2019.0307 REMOVED REDEFINES OF GP-BEG-FISCAL-DATE, WAS USING
      *               GP-BEG-FISCAL-DATE-MO, USE DATE WORKERS INSTEAD
      *  CS 190627 REMOVED USE OF TAG-PATHS AND REPLACED WITH CHECKING THE
      *            GBFILE 
      *  CS 191010 ALWAYS GETTING MESSAGE THAT THE PRIOR END OF MONTH WAS NOT
      *            DONE, EVEN IF IT WAS ADDED MOVE OF EOM-TAG-CCYY TO
      *            EOY-TAG-CC 
      * BAH 20200819 REMOVED LS-CLOSED-FILE
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2023.0727 ADDED BR-BRANCH-CLOSED TO STOP LIVE CHECKS FROM
      *               GETTING POSTED TO BRANCHES THAT HAVE BEEN PHYSICALLY
      *               CLOSED. DO NOT ALLOW THEM TO OPEN THE DAY! #1618
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       COPY "LIBLP/LPFSOP.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDOP.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LONPA0 S35 03/17/25-14:08:19 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN                      PIC X(2) VALUE "WK".
       01  WK-KEY                      PIC 99.
      *-----------------------------------------------------------------
       01  FISCAL-FG                           PIC X    VALUE "Y".
       01  DEFAULT-BRANCH                      PIC 9(4).

       01  COMPARE-DATE1               PIC 9(8).
       01  COMPARE-DATE1X REDEFINES COMPARE-DATE1.
           03  COMPDATE1-YYYY          PIC 9(4).
           03  COMPDATE1-MM            PIC 9(2).
           03  COMPDATE1-DD            PIC 9(2).

       01  COMPARE-DATE2               PIC 9(8).
       01  FILLER REDEFINES COMPARE-DATE2.
           03  COMPDATE2-YYYY          PIC 9(4).
           03  COMPDATE2-MM            PIC 9(2).
           03  COMPDATE2-DD            PIC 9(2).

       01  CLOSED-DAYS                 PIC X    VALUE SPACES.

       01  DAY-IS-OPEN                 PIC X    VALUE SPACES.
       01  OPEN-TEST-FG                PIC X.
       01  BAD-STATUS-FG               PIC X.
       01  ITS-EOM-FG                  PIC X VALUE "N".
       01  FISCAL-SUB                  PIC 99.
       01  SYSDATE-YYYYMMDD            PIC 9(8).
       01  ABSOLUTE-DAYS               PIC 9(4)    VALUE ZEROES.

           
       01  EOM-TAG-DATE            PIC 9(6)    VALUE 0.
       01  EOM-TAG-DATEX REDEFINES EOM-TAG-DATE.
           05  EOM-TAG-CCYY        PIC 9999.
           05  EOM-TAG-MM          PIC 99.

       01  EOY-TAG-CCYY                 PIC 9999.

       01  MEXICO-FG                   PIC X  VALUE "N".

       01  FLAG-DATE-WARNINGS          PIC X(3)    VALUE SPACES.
       01  FG-DT-WARNINGS              REDEFINES FLAG-DATE-WARNINGS.
           03  FLAG-DATE-NXT-MO        PIC X(1).
           03  FLAG-DATE-3DAYS         PIC X(1).
           03  FLAG-DATE-10DAYS        PIC X(1).

      ******************************************************************
      *    FOR MESSAGE IF TRANS-DATE NOT = CURRENT DATE
      ******************************************************************
       01  TRANS-DATE-WINDOW.
           03  TRANS-WINDOW-LINE1            PIC X(24)  VALUE
               "YOU WILL BE POSTING FOR:".
           03  TRANS-WINDOW-LINE2.
               05  TRANS-WINDOW-LINE2A       PIC 99/99/99.
               05  TRANS-WINDOW-LINE2X   REDEFINES TRANS-WINDOW-LINE2A
                                             PIC X(8).
           03  TRANS-WINDOW-LINE3.
               05  FILLER                    PIC X(23)  VALUE
                   "TYPE 'CONTINUE'; OR    ".
           03  TRANS-WINDOW-LINE4.
               05  FILLER                    PIC X(28)  VALUE
                   "HIT 'ENTER' TO RE-INPUT DATE".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       01  HOLD-REC-SEQ                        PIC 9(2)  VALUE ZEROES.
      * 0 - POST TO DIFFERENT DATE
      * 1 - POST FOR ANOTHER MONTH
      * 2 - POST FOR THIS DATE
      * 3 - POST FOR PRIOR DATE
      * 4 - TO OPEN DAY PRIOR TO TIME
      * 5 - CONTINUE IF BRANCH IS CLOSED

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".

       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPA_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONPA_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPA_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE OP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - LONPA0.CBL
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPA" TO VDU-FORM-NAME.
           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM FORM-RESET.

      *    MOVE EXT-FILPATH-BASE    TO EOM-TAG-BASE
      *                                EOY-TAG-BASE.

      *    MOVE EXT-FILPATH-MACHINE TO EOM-TAG-MACHINE
      *                                EOY-TAG-MACHINE.

           IF ( ROUTE-CD = "D" )
              MOVE "TRANSACTION DATE CLOSED !" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO TRANS-DATE.

           IF ( TRANS-DATE NOT = 0 )
              MOVE "TRANSACTION DATE ERROR !" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO STOP-RUN.

           IF ( EXT-CONAME-USER-LOC NOT NUMERIC )
              MOVE "INVALID USER ID FOR LOAN PROCESSING !" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO STOP-RUN.

           IF ( EXT-FILPATH-BRANCH NOT = LPLOAN-USER-BRANCH )
              MOVE "USER SIGNON DOESN'T MATCH DATA PATH" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO MESS
              MOVE "FIL          SIGN" TO MESS
              MOVE EXT-FILPATH-BRANCH TO MESS-X-NUM-1
              MOVE LPLOAN-USER-BRANCH TO MESS-X-NUM-2
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO STOP-RUN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE SYSDATE-YYYYMMDD.

           PERFORM GET-GPENV.

           IF EXT-FILPATH-BRANCH1 = 3
              MOVE "Y" TO MEXICO-FG.

           MOVE "N" TO ITS-EOM-FG.

           MOVE TRANS-DATE TO NUM-DATE.
           PERFORM FIND-FISCAL-PERIOD.

      * TO WHOM IT MAY CONCERN:
      * I REALLY THINK THE FOLLOWING ROUTINE SHOULD BE TRASHED;
      * IT'S USING SYSTEM DATE INSTEAD OF TRANS-DATE, & CHECKING
      * FOR END-OF-DAY IN PROCESS OR UPDATED...CREATE-RC WILL CHECK
      * IF SELECTED DAY IS UPDATED.  I THINK THIS IS REALLY LEFT-
      * OVER FROM A90, BUT I ALREADY CHANGED OTHER STUFF SO I'M
      * NOT DELETING THIS NOW.....5/25/99 BLV

           IF FISCAL-SUB NOT = 0
              IF SYSDATE-YYYYMMDD = GP-END-FISCAL-DATE(FISCAL-SUB)
                 IF BR-AUTO-OPEN-BAL = "Y"
                    MOVE "Y"         TO ITS-EOM-FG
                    MOVE EXT-CONAME-USER-LOC   TO RC-BRNO
                    MOVE SYSDATE-YYYYMMDD      TO RC-TRANS-DATE
                    PERFORM OPEN-RC1-FILE
                    PERFORM READ-RC1-FILE
                    PERFORM CLOSE-RC1-FILE
                    IF ( IO-FG = 0 )
                       IF ( RC-INPROCESS OR RC-UPDATED
                                         OR RC-EOD-BATCH-DONE )
                          MOVE "END OF MONTH IN PROCESS!!!!" TO MESS
                          PERFORM SEND-MESS
                          MOVE "X" TO ERRCD
                          GO TO STOP-RUN.

      * TEST SYSDATE > END-FISCAL DATE.

           MOVE EXT-CONAME-USER-LOC TO DEFAULT-BRANCH BRANCH.
           GO TO SEND-BRANCH.

      *-----------------------------------------------------------------
       ENTER-BRANCH.
           MOVE "ENNN040BR." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              MOVE "X" TO ERRCD
              GO TO STOP-RUN.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-BRANCH.

           IF ( VDUNUM0 = 0 )
              MOVE BRANCH TO VDUNUM0.

           MOVE VDUNUM0 TO BRANCH.

      *-----------------------------------------------------------------
       SEND-BRANCH.
           MOVE "S  A040BR." TO VDU.
           MOVE BRANCH       TO VDUBUF BR-NO.
           PERFORM SCREENIO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-FG = 9 )
              MOVE "INVALID BRANCH !" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           MOVE "S  A040CONAME." TO VDU.
           MOVE BR-NAME          TO VDUBUF LPLOAN-CONAME.
           PERFORM SCREENIO.

           IF BR-BRANCH-CLOSED = "Y"
              MOVE "BRANCH HAS BEEN CLOSED" TO MESS
              PERFORM SEND-MESS
              PERFORM VERIFY-PASSWORD-MUST-EXIST
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "X" TO ERRCD
                 GO TO STOP-RUN.

      *-----------------------------------------------------------------
       ENTER-TRANS-DATE.
           MOVE "ERNM080TRANSDATE." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1>" )
              MOVE "X" TO ERRCD
              GO TO STOP-RUN.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-TRANS-DATE.

           IF ( VDUNUM0 = 0 )
              MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD.

           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.
           IF ( NUM-DATE = 0 )
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TRANS-DATE.

           IF ( NUM-DATE NOT = TRANS-DATE )
              PERFORM VERIFY-PASSWORD
              IF ( PASSWDW-STAT NOT = "OK" )
                 GO TO ENTER-TRANS-DATE.

           MOVE "S  8000TRANSDATE." TO VDU.
           MOVE VDU-DATE-YYYYMMDD   TO TRANS-DATE
                                       DATE-YYYYMMDD VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           PERFORM VERIFY-END-FISCAL.
           IF FISCAL-FG NOT = "Y"
              MOVE "YOU CANNOT POST AFTER END OF MONTH" TO MESS
              PERFORM SEND-MESS
              PERFORM CLEAR-WARNINGS
              GO TO ENTER-TRANS-DATE.

           IF BR-CHECK-EOM-FG NOT = "N"
              PERFORM VERIFY-EOM-EOY-COMPLETE
              IF FISCAL-FG = "M"
                 MOVE "END OF MONTH FOR LAST MONTH NOT COMPLETED!"
                    TO MESS
                 PERFORM SEND-MESS
                 PERFORM CLEAR-WARNINGS
                 GO TO ENTER-TRANS-DATE
              ELSE IF FISCAL-FG = "C"
               MOVE "CALENDAR END OF YEAR FOR LAST YEAR NOT COMPLETED!"
                    TO MESS
                 PERFORM SEND-MESS
                 PERFORM CLEAR-WARNINGS
                 GO TO ENTER-TRANS-DATE
              ELSE IF FISCAL-FG = "F"
               MOVE "FISCAL END OF YEAR FOR LAST YEAR NOT COMPLETED!"
                    TO MESS
                 PERFORM SEND-MESS
                 PERFORM CLEAR-WARNINGS
                 GO TO ENTER-TRANS-DATE.

      * VERIFY IF TRANS-DATE IS NOT WITHIN CERTAIN RANGES:
      *    - WITHIN THE SAME MONTH CURRENT MONTH;
      *    - WITHIN 3 DAYS OF CURRENT DATE.
      *    - WITHIN 10 DAYS OF CURRENT DATE.
      
           MOVE TRANS-DATE        TO COMPARE-DATE1 NUM-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD     TO COMPARE-DATE2 SYS-DATE.
           PERFORM TIM.
           MOVE ELAPSED-DAYS      TO ABSOLUTE-DAYS.

      * WITHIN THE SAME MONTH 
           IF ( COMPDATE1-MM NOT = COMPDATE2-MM ) OR
              ( COMPDATE1-YYYY NOT = COMPDATE2-YYYY )
              MOVE "Y"  TO FLAG-DATE-NXT-MO
           ELSE
              MOVE "N"  TO FLAG-DATE-NXT-MO.

      * WITHIN 3 DAYS 
           IF ( ABSOLUTE-DAYS > 3 )
              MOVE "Y"  TO FLAG-DATE-3DAYS
           ELSE
              MOVE "N"  TO FLAG-DATE-3DAYS.

      * WITHIN 10 DAYS 
           IF ( ELAPSED-DAYS < -10 )
              MOVE "Y"  TO FLAG-DATE-10DAYS
           ELSE
              MOVE "N"  TO FLAG-DATE-10DAYS.

      * SEE IF DAY OPENED, CLOSED, OR UPDATED
      
           MOVE BRANCH     TO OPEN-PATH-BRNO.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.
           PERFORM LOAD-OPEN-FILE.

           MOVE BRANCH            TO RC-BRNO.
           MOVE TRANS-DATE        TO RC-TRANS-DATE.
           PERFORM OPEN-RC1-FILE.
           PERFORM READ-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-GOOD
             IF RC-STATUS = "B1"
                 MOVE "TRANSACTION DATE CLOSED !" TO MESS
                 PERFORM SEND-MESS
                 MOVE 0 TO TRANS-DATE
                 GO TO ENTER-TRANS-DATE 
             ELSE
             IF RC-STATUS > "B1"
                 MOVE "TRANSACTION DATE UPDATED !" TO MESS
                 PERFORM SEND-MESS
                 MOVE 0 TO TRANS-DATE
                 GO TO ENTER-TRANS-DATE.
     
      * WORLD CAN NOT CLOSE THE DAY THEN OPEN TOMORROW
           IF GP-POST-INTO-FUTURE = "N"
              IF TRANS-DATE > SYSDATE-YYYYMMDD
                 MOVE "YOU CAN'T POST INTO THE FUTURE!!!!!" TO MESS
                 PERFORM SEND-MESS
                 MOVE EXT-CONAME-SYSDATE TO TRANS-DATE
                 GO TO ENTER-TRANS-DATE.

           PERFORM CHECK-FOR-ALL-CLOSED.
           IF CLOSED-DAYS = "Y"
              MOVE 0 TO TRANS-DATE
              GO TO ENTER-TRANS-DATE.

           IF GP-EARLIEST-OPEN-TIME NUMERIC OR
               GP-EARLIEST-OPEN-TIME NOT = 0
              ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE
              IF DATE-YYYYMMDD = TRANS-DATE
                 MOVE "Y" TO TIME-MILITARY-FG
                 PERFORM ZONED-GET-TIME
                 IF TIME-NOW < GP-EARLIEST-OPEN-TIME
                    MOVE GP-EARLIEST-OPEN-TIME  TO H-AND-M
                    PERFORM CONVERT-TIME
                    MOVE 4 TO HOLD-REC-SEQ
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO ENTER-TRANS-DATE.

           IF BR-AUTO-OPEN-BAL = "Y"
              PERFORM TEST-OPEN-DAYS
              IF OPEN-TEST-FG = "N" 
                 MOVE "S  8000TRANSDATE." TO VDU
                 MOVE RC-TRANS-DATE       TO TRANS-DATE
                                             VDU-DATE-YYYYMMDD
                 PERFORM SCREENIO
                 GO TO ENTER-TRANS-DATE.

      * REGIONAL WANTS PROMPTED FOR PASSWORD IF ENTERING A
      * DATE THAT HAS PASSED.    05/30/01
           IF DAY-IS-OPEN NOT = "Y"
              IF TRANS-DATE < SYSDATE-YYYYMMDD
                 MOVE 3 TO HOLD-REC-SEQ
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE ZERO TO HOLD-REC-SEQ
                    MOVE "S  8000TRANSDATE." TO VDU
                    MOVE SYSDATE-YYYYMMDD TO TRANS-DATE
                                             VDU-DATE-YYYYMMDD
                    PERFORM SCREENIO
                    GO TO ENTER-TRANS-DATE.

      * VERIFICATION FOR A TRANS-DATE THAT IS 3 DAYS GREATER OR LESS THAN
      * THE CURRENT DATE.

           IF ( FLAG-DATE-WARNINGS NOT = "NNN" )
              MOVE "S  A240MSG-LN1:B."    TO VDU
              MOVE TRANS-WINDOW-LINE1     TO VDUBUF
              PERFORM SCREENIO
      
              MOVE "S  A080MSG-LN2:BF."   TO VDU
              MOVE TRANS-DATE             TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY            TO TRANS-WINDOW-LINE2A
              MOVE TRANS-WINDOW-LINE2     TO VDUBUF
              PERFORM SCREENIO
      
              MOVE "S  A230MSG-LN3:B."    TO VDU
              MOVE TRANS-WINDOW-LINE3     TO VDUBUF
              PERFORM SCREENIO
      
              MOVE "S  A280MSG-LN4:B."    TO VDU
              MOVE TRANS-WINDOW-LINE4     TO VDUBUF
              PERFORM SCREENIO
      
              MOVE "E  A080SEL." TO VDU
              PERFORM SCREENIO
              IF ( VDUBUF NOT = "CONTINUE" )
                 PERFORM CLEAR-WARNINGS
                 GO TO ENTER-TRANS-DATE.

           IF ( FLAG-DATE-WARNINGS NOT = "NNN" )
              PERFORM CHECK-FOR-PASSWORD
              IF ( PASSWDW-STAT NOT = "OK" )
                 MOVE ZEROES TO HOLD-REC-SEQ
                 PERFORM CLEAR-WARNINGS
                 GO TO ENTER-TRANS-DATE.

      * PUT THE TRANS-DATE BACK ONTO THE SCREEN AND BACK INTO LPLOAN BUF
      * SYSTEM DATE

           MOVE TRANS-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE          TO LPLOAN-SYSDATE.
           MOVE "S  8000SYSDATE." TO VDU.
           MOVE TRANS-DATE        TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           PERFORM CREATE-RC-RECORD.
           IF BAD-STATUS-FG = "Y"
              GO TO ENTER-TRANS-DATE.

           MOVE "LONPA0" TO FORM-PROGX.

           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT NOT = "00" )
              PERFORM OPEN-OP-FILE-OUTPUT
              PERFORM CLOSE-OP-FILE.

           PERFORM OPEN-OP-FILE.

           MOVE "Y" TO VDU-WIMENU-SELECTION.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPA-SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.

      *-----------------------------------------------------------------
       LOAN-MENU.
           PERFORM MENU-SELECTION.

           IF ( MENU-FG = 99 )
              MOVE "MSEL." TO MESS
              PERFORM RESET-MESS
              GO TO ENTER-TRANS-DATE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

      * WK-FILE FOR LOAN PROCESSING USED PROCESS ID XL(PROCESS ID)

           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           MOVE 9 TO IO-FG.
           PERFORM OPEN-WK-FILE.

           GO TO END-ROUTINE.

      ******************************************************************
       TEST-OPEN-DAYS SECTION.
      ******************************************************************
           MOVE " "       TO OPEN-TEST-FG.
           MOVE " "       TO DAY-IS-OPEN.

           PERFORM OPEN-RC2-FILE.

           MOVE "A1"      TO RC-STATUS QRC2-WBEG-STATUS
                                       QRC2-WEND-STATUS.
           MOVE BRANCH    TO RC-BRNO QRC2-WBEG-BRNO
                                     QRC2-WEND-BRNO.
           MOVE 19000101  TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.
          
           MOVE EXT-JULIAN-END TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

       TEST-OPEN-NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-TEST-OPEN-DAYS.

           IF ( RC-BRNO NOT = BRANCH )
              GO TO EXIT-TEST-OPEN-DAYS.

           IF ( RC-STATUS NOT = "A1" )
              GO TO EXIT-TEST-OPEN-DAYS.

           IF RC-TRANS-DATE = TRANS-DATE
              MOVE "Y" TO DAY-IS-OPEN
              GO TO TEST-OPEN-NEXT-RC.

           MOVE "OTHER DAY(S) OPEN, CASH DRAWERS WILL BE AFFECTED !"
                TO MESS.
           PERFORM SEND-MESS.

           MOVE "YOU MUST POST FOR THE DAY OPEN!" TO MESS.
           PERFORM SEND-MESS.

           MOVE "N" TO OPEN-TEST-FG.

       EXIT-TEST-OPEN-DAYS.
           PERFORM CLOSE-RC2-FILE.

      **********************************************
       CHECK-FOR-ALL-CLOSED SECTION.
      **********************************************
      D    STOP MESS.
           MOVE " " TO CLOSED-DAYS.

           PERFORM OPEN-RC2-FILE.

           MOVE "B1"      TO RC-STATUS QRC2-WBEG-STATUS
                                       QRC2-WEND-STATUS.
           MOVE BRANCH    TO RC-BRNO QRC2-WBEG-BRNO
                                     QRC2-WEND-BRNO.
           MOVE 19000101  TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.
          
           MOVE EXT-JULIAN-END TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

       CHECK-NEXT-CLOSED.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CHECK-FOR-ALL-CLOSED.

           IF RC-BRNO NOT = BRANCH
              GO TO EXIT-CHECK-FOR-ALL-CLOSED.

           IF RC-STATUS NOT = "B1"
              GO TO EXIT-CHECK-FOR-ALL-CLOSED.

           MOVE RC-TRANS-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO ALP-DATE-EDIT.

           MOVE "CLOSED FOR &&&&&&&&!" TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&&&&&" BY
                                            ALP-DATE-EDIT
           PERFORM SEND-MESS.

           MOVE "Y" TO CLOSED-DAYS.

       EXIT-CHECK-FOR-ALL-CLOSED.
           PERFORM CLOSE-RC2-FILE.

      ******************************************************************
      *
      *    ENTER PASSWORD IF DATE NOT = TRANS-DATE
      *
      *-----------------------------------------------------------------
      *    HOLD-REC-SEQ = 1; POSTING FOR NEXT MONTH.
      *    HOLD-REC-SEQ = 2; POSTING FOR 10 DAYS IN THE FUTURE.
      *
      ******************************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " "          TO PASSWDW-STAT.
           MOVE "LONPA0"     TO PASSWDW-PGM.
           MOVE HOLD-REC-SEQ TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD,"   TO PASSWDW-PROMPT1.

           IF ( HOLD-REC-SEQ = 1 )
              MOVE "TO POST FOR ANOTHER MONTH:  " TO PASSWDW-PROMPT2
           ELSE
           IF ( HOLD-REC-SEQ = 2 )
              MOVE "TO POST FOR THIS DATE:      " TO PASSWDW-PROMPT2
           ELSE
           IF (HOLD-REC-SEQ = 3)
               MOVE "TO POST FOR A PRIOR DATE"     TO PASSWDW-PROMPT2
           ELSE
           IF (HOLD-REC-SEQ = 4)
              MOVE "TO OPEN DAY PRIOR TO TIME"     TO PASSWDW-PROMPT2
           ELSE
              MOVE "TO POST TO A DIFFERENT DATE:" TO PASSWDW-PROMPT2.

           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************************
      * PASSWORD MUST EXIST SECTION
      *   LIKE VERIFY PASSWORD, BUT WILL GIVE BAD STATUS IF
      *   PASSWORD NOT IN FILE.  IF PASSWORD IN FILE, WILL
      *   ASK FOR PASSWORD & ACT JUST LIKE VERIFY
      * 5 - CONTINUE IF BRANCH IS CLOSED
      *******************************************************
       VERIFY-PASSWORD-MUST-EXIST SECTION.
           MOVE "EX"     TO PASSWDW-STAT.
           MOVE "LONPA0" TO PASSWDW-PGM.
           MOVE 5        TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO OPEN A CLOSED BRANCH"    TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
       CREATE-RC-RECORD SECTION.
      ******************************************************************

           PERFORM OPEN-RC1-FILE.

           MOVE "N" TO BAD-STATUS-FG.

           PERFORM SET-RC1-KEY.

           PERFORM READ-RC1-FILE.
           IF ( IO-FG = 0 )
              IF ( RC-STATUS = "A1" )
                 GO TO CREATE-RC-RECORD-EXIT
              ELSE
                 MOVE "DAY HAS BEEN CLOSED OR UPDATED" TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO BAD-STATUS-FG
                 GO TO CREATE-RC-RECORD-EXIT.

           PERFORM CLEAR-RC-FILE.

           PERFORM SET-RC1-KEY.

           MOVE "A1"            TO RC-STATUS.

           ACCEPT RC-OPENED-DATE FROM CENTURY-DATE.
           MOVE "Y"      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE HH-MM-SS        TO RC-OPENED-TIME.
           MOVE LPLOAN-USERID   TO RC-OP-USERID.

           PERFORM WRITE-RC1-FILE.
           IF ( IO-FG = 9 )
              IF ( E-CODE = "22" )
                 GO TO CREATE-RC-RECORD-EXIT
              ELSE
                 GO TO IO-ERROR.

       CREATE-RC-RECORD-EXIT.
           PERFORM CLOSE-RC1-FILE.

      ******************************************************************
      *
      *    FIND FISCAL END OF PERIOD
      *
      *-----------------------------------------------------------------
      * 1. SET NUM-DATE = TODAY'S DATE
      * 2. SUBSCRIPT TO GP-END-FISCAL-DATE IS IN FISCAL-SUB
      * 3  IF NOT FOUND FISCAL-SUB = 0
      ******************************************************************
       FIND-FISCAL-PERIOD SECTION.
           MOVE 1 TO FISCAL-SUB.

       FIND-FISCAL-AGAIN.
           IF ( FISCAL-SUB > 12 )
              MOVE 0 TO FISCAL-SUB
           ELSE
              MOVE GP-END-FISCAL-DATE(FISCAL-SUB) TO SYS-DATE
              IF ( NUM-MO NOT = S-MM )
                 ADD 1 TO FISCAL-SUB
                 GO TO FIND-FISCAL-AGAIN.

       EXIT-FIND-FISCAL.
           EXIT.

      *********************************************
       VERIFY-END-FISCAL SECTION.
      *********************************************

      * VERIFIES THAT TRANSDATE IS NOT > END-FISCAL DATE OR < BEGINNING
      * OF NEXT PERIOD (FIRST OF NEXT MONTH)

      * TEST IF TRANS-DATE < BEG OF NEXT MONTH
           MOVE TRANS-DATE TO NUM-DATE.
           PERFORM FIND-FISCAL-PERIOD.

           MOVE "Y" TO FISCAL-FG.
           MOVE GP-END-FISCAL-DATE(FISCAL-SUB) TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO NUM-DATE.
           MOVE 01        TO NUM-DA.

           MOVE TRANS-DATE TO SYS-DATE.
           IF SYS-DATE NOT < NUM-DATE
              GO TO EXIT-VERIFY-END-FISCAL.

      *----------------------------------------------------------------
      * TEST IF TRANS-DATE > GP-END-FISCAL-DATE(FISCAL-SUB).
      *----------------------------------------------------------------
           MOVE TRANS-DATE                     TO SYS-DATE.
           MOVE GP-END-FISCAL-DATE(FISCAL-SUB) TO NUM-DATE.
           IF SYS-DATE > NUM-DATE
              MOVE "N" TO FISCAL-FG
              GO TO EXIT-VERIFY-END-FISCAL.

           IF ( FISCAL-SUB NOT = 0 )
              MOVE EXT-CONAME-USER-LOC TO RC-BRNO
              MOVE GP-END-FISCAL-DATE(FISCAL-SUB) TO DATE-YYYYMMDD
                                                     NUM-DATE
              MOVE TRANS-YEAR TO DATE-YYYYMMDD-YY NUM-YR
              MOVE DATE-YYYYMMDD TO RC-TRANS-DATE
              PERFORM OPEN-RC1-FILE
              PERFORM READ-RC1-FILE
              PERFORM CLOSE-RC1-FILE
              IF IO-FG = 0
                 IF RC-STATUS > "D1"
                    MOVE "N" TO FISCAL-FG
                    GO TO EXIT-VERIFY-END-FISCAL.

       EXIT-VERIFY-END-FISCAL.
           EXIT.

      *********************************************
       VERIFY-EOM-EOY-COMPLETE SECTION.
      *********************************************

      * VERIFY END OF MONTH FOR PREVIOUS MONTH HAS BEEN COMPLETED
      * ONLY CHECK IF THERE IS A TR FOR PREVIOUS MONTH
      * ONLY CHECK IF FIRST 10 DAYS OF A MONTH

           MOVE " " TO FISCAL-FG.

           MOVE TRANS-DATE TO NDTE-DATE.
           IF NDTE-DD > 10
              GO TO EXIT-VERIFY-EOM-EOY.

      * CHECK FOR PREVIOUS EOM TAG; EOM(YYYYMM)
           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-CCYY TO EOM-TAG-CCYY.
           MOVE NDTE-MM   TO EOM-TAG-MM.

           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO. 
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO IO-ERROR
           ELSE
              IF GB-EOM-TAG-CCYYMM = 0
                  MOVE "EOM DATE NOT SET IN GLOBAL" TO MESS
                 PERFORM SEND-MESS
                 MOVE "M" TO FISCAL-FG
                 GO TO EXIT-VERIFY-EOM-EOY 
              ELSE  
                 IF GB-EOM-TAG-CCYYMM NOT = EOM-TAG-DATE   
                    MOVE "M" TO FISCAL-FG
                    GO TO EXIT-VERIFY-EOM-EOY.


      * CHECK FOR CALENDAR EOY TAG 

           IF EOM-TAG-MM NOT = 12
              GO TO CHECK-FISCAL-EOY-TAG.


      *MONTH 12 AND CHECKING FOR BOTH FISCAL/CALENDAR COMPLETED
           MOVE EOM-TAG-CCYY   TO EOY-TAG-CCYY.
           IF GB-FISCAL-EOY-TAG-CCYY = EOY-TAG-CCYY
              AND GB-CALENDAR-EOY-TAG-CCYY = EOY-TAG-CCYY
                 GO TO EXIT-VERIFY-EOM-EOY.

      *MONTH 12 AND CHECKING FOR CALENDAR EOY COMPLETED
           IF GB-CALENDAR-EOY-TAG-CCYY = EOY-TAG-CCYY
              GO TO CHECK-FISCAL-EOY-TAG.

           MOVE "C" TO FISCAL-FG.
           GO TO EXIT-VERIFY-EOM-EOY.

       CHECK-FISCAL-EOY-TAG.
           MOVE GP-BEG-FISCAL-DATE TO DATE-YYYYMMDD.
           IF TRANS-MONTH NOT = DATE-YYYYMMDD-MM
              GO TO EXIT-VERIFY-EOM-EOY.

           MOVE EOM-TAG-CCYY   TO EOY-TAG-CCYY.
           IF GB-FISCAL-EOY-TAG-CCYY = EOY-TAG-CCYY
              GO TO EXIT-VERIFY-EOM-EOY.

      *FISCAL YEAR NOT RUN
           MOVE "F" TO FISCAL-FG.
           GO TO EXIT-VERIFY-EOM-EOY.

       EXIT-VERIFY-EOM-EOY.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE LPLOAN-CONAME TO LONPA-CONAME.
           MOVE LPLOAN-SYSDATE TO LONPA-SYSDATE.
           DISPLAY SPACES UPON TCLP-MAIN-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY LONPA-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE. 

      * ONLY LONPA FORM NEEDS TO BE INITIALIZED FOR THE SCREEN ***
      * AS THERE IS REALLY ONLY ONE SCREEN LAYOUT.             ***
           MOVE LONPA-FORM-FIELDS TO WR-BUF
           MOVE LONPA-FORM-LIST   TO WR-LIST. 
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPA-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPA-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPA_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------------------------------
       CONVERT-TIME.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.

           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.

           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.

           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE SPACE TO T-SPACE.
       CLEAR-WARNINGS.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE "S  8080TRANSDATE." TO VDU.
           MOVE DATE-YYYYMMDD TO TRANS-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *-    -    -    -    -    -    -    -    -    -    -    -    -
           MOVE "S  A240MSG-LN1."   TO VDU.
           MOVE SPACES              TO VDUBUF.
           PERFORM SCREENIO.
      *-    -    -    -    -    -    -    -    -    -    -    -    -
           MOVE "S  A080MSG-LN2."   TO VDU.
           MOVE SPACES              TO VDUBUF.
           PERFORM SCREENIO.
      *-    -    -    -    -    -    -    -    -    -    -    -    -
           MOVE "S  A230MSG-LN3."   TO VDU.
           MOVE SPACES              TO VDUBUF.
           PERFORM SCREENIO.
      *-    -    -    -    -    -    -    -    -    -    -    -    -
           MOVE "S  A280MSG-LN4."   TO VDU.
           MOVE SPACES              TO VDUBUF.
           PERFORM SCREENIO.
      *-    -    -    -    -    -    -    -    -    -    -    -    -
           MOVE "S  A080SEL."       TO VDU.
           MOVE SPACES              TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SET-RC1-KEY.
           MOVE BRANCH     TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

      *-----------------------------------------------------------------
       CHECK-FOR-PASSWORD.
      *- - - - - - - - - - - - - - - - - - - - - -  JUST OVER 3 DAYS - -
           IF ( FLAG-DATE-WARNINGS = "NYN" )
              MOVE "OK" TO PASSWDW-STAT
      *- - - - - - - - - - - - - - - - - - - - - - JUST OVER 10 DAYS - -
           ELSE
           IF ( FLAG-DATE-WARNINGS = "NYY" )
              MOVE 2 TO HOLD-REC-SEQ
              PERFORM VERIFY-PASSWORD
      *- - - - - - - - - - - - - - - - - - - -  WENT INTO NEXT MONTH - -
           ELSE
              MOVE 1 TO HOLD-REC-SEQ
              PERFORM VERIFY-PASSWORD.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBCL/TRACE.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GBRCCL.CPY".
       COPY "LIBGB/ZONEDTIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPOPIN.CPY".
      *-----------------------------------------------------------------
      *
      *   WKFILE - OPEN OUTPUT
      *
      *-----------------------------------------------------------------
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-WK-FILE.

           IF ( IO-FG = 7 )
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-FILE.

      *-----------------------------------------------------------------
       CLOSE-WK-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE WK-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************************************************
       END-ROUTINE SECTION.
      **************************************************************************
      D    STOP MESS.
           PERFORM CLOSE-FILES.

       STOP-RUN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *    *NOTE* YOU MAY GET DOUBLE "PROGRAM FINISH" LINES IN THE LOG
      *    DUE TO ONE BEING CREATED IN 'PERFORM CLOSE-FILES' AND THIS
      *    ONE WHICH FOR ANY 'GO TO STOP-RUN'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LONPA0"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH-STOP.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPA-SCREEN.
           EXIT PROGRAM.
