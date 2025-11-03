      ******************************************************************
      * COPYMEMBER: LIBSP/SPINQ2
      *=================================================================
      * THERE ARE TWO PURPOSES FOR THE VCQFILE:
      *
      *  1. FOR SCAN PAGE FUNCTIONALITY THAT CANNOT BE ACHIEVED FOR THE
      *     CQ2-KEY IN SQL BUT CAN BE DONE IN VISION.  DUE TO THE 
      *     COMPLEXITY OF THE CQ2-KEY, IT IS HARD TO NAVIGATE WITH A
      *     PAGE UP & PAGE DOWN WITHIN A SCAN WINDOW (WI/CQSCAN)
      *
      *  2. FOR DEALING WITH THE COLLECTION WORK SCREEN (SPINQ & XPINQ)
      *     WORKING WITH THE CQ2-KEY AND ITS DUPLICATES.  THIS IS
      *     BECAUSE IT REQUIRES A START-GREATER FUNCTION. DUE TO THE
      *     COMPLEXITY OF THE CQ2-KEY, GETTING A PROPER SQL WHERE
      *     CONSTRAINT TO WORK IN A START-GREATER USAGE IS VERY
      *     DIFFICULT.  IT IS EASIER TO USE THE VCQFILE.
      *
      * WITHIN THE CQ2-KEY, CQ-SEQ WAS ADDED BY BARB BACK IN A30 IN
      * ATTEMPT TO WORK WITH THE CQ2-KEY DUPLICATES.  CINDY ALSO TRIED
      * WORKING WITH CQ-SEQ FOR THE DUPLICATES WITHIN S35.  THEIR
      * EFFORTS DID NOT WORK AND NEVER FOUND A RESOLUTION.
      *
      * IN SEPTEMBER 2023, STEVE AND JAY WORKED ON CREATING THE VCQFILE
      * TO DEAL WITH THE NAVIGATION WITHIN THE SCAN WINDOW.  THIS
      * RESOLVED IT.
      *
      * IN MARCH 2025, BETH AND JAY WORKED ON THE ISSUES THAT S35
      * USER TESTING WAS GETTING (LOCKING, DISPLAY OUT OF ORDER,
      * NOT GETTING THE NEXT ACCOUNT WHEN SKIPPING).  THEY FOUND
      * CQ-SEQ WAS ALWAYS ZEROES AND NO LOGIC THAT WOULD INCREMENT IT.
      * NOW IN THE COLLECTION WORK SCREEN (SPINQ & XPINQ; LIBSP/SPINQ2),
      * AT THE START, IT WILL LOAD ALL CQ-REC FOR THAT GIVEN BRANCH
      * INTO THE USER'S VCQFILE THEN IT WILL GO THROUGH IT USING
      * VCQ2-KEY LOOKING FOR DUPLICATES AND ADDING ONE TO VCQ-SEQ IF IT
      * IS A DUPLICATE (CL/VCQUPD).  FURTHER INTO THE PROGRAM, IT
      * WILL USE MOSTLY THE VCQFILE BUT AT TIMES WILL STILL READ THE
      * CQFILE (SQL) FOR THE MATCHING RECORD (VCQ-CQ-REC) TO ENSURE IT
      * WAS NOT WORKED (CQ-WORKED-FG = "Y") OR IS CURRENTLY BEING WORKED
      * BY ANOTHER USER (CQ-IN-PROCESS = "Y") SINCE LOADED INTO THE
      * VCQFILE.  THE REWRITE-CQ1-VCQ1-FILE SECTION (LIBSP/SPINQ2) WAS
      * CREATED TO UPDATE THE CQ-REC AND VCQ-REC FOR CHANGES MADE.
      * THIS SECTION WILL:
      *
      *  1. MOVE CQ-REC TO HOLD-CQ-REC
      *  2. MOVE ZEROES TO CQ-SEQ
      *  3. READ CQFILE TO GET CQ-REC
      *  4. MOVE VCQ-SEQ INTO CQ-SEQ OF RECENT READ CQ-REC
      *  5. CHECK IF CQ-REC (SQL) EQUALS VCQ-CQ-REC
      *     A. IF EQUAL, MOVE HOLD-CQ-REC BACK TO CQ-REC
      *     B. IF NOT EQUAL, DISPLAY MESSAGE & FORCE EXIT OF PROGRAM
      *  6. REWRITE CQ-REC AND VCQ-REC
      *
      * THIS SECTION NEEDS TO ENSURE THAT THE GIVEN CQ-REC HAS NOT BEEN
      * ALTERED BY ANOTHER USER WHILE THE GIVEN USER WAS WORKING IT.
      * IF THE CQ-REC HAD BEEN ALTERED, WE NEED TO MESSAGE THE CURRENT
      * USER THAT A CHANGE HAD BEEN MADE AND NEED RESTART THE COLLECTION
      * WORK SCREEN IN ORDER TO RELOAD THE VCQFILE TO GET IT RESYNC WITH
      * THE LATEST DATA.  IF CQ-REC UPDATE WAS SUCCESSFUL THEN IT WILL
      * USE UPDATE-VCQ1-FILE-VCQUPD (LIBSP/SPVCQ1IN) TO CALL CL/VCQUPD
      * TO UPDATE THE VCQFILE AND RE-SEQUENCE THE VCQ2-KEY FOR
      * DUPLICATES IF FOUND.
      *
      *=================================================================
      * REV:
      * BAH 181002 GLOBAL BWFILE
      *    PLEASE SEE LIBSP/SPINQ1 FOR REVISIONS
      * CS  190130 SEE NOTES IN SPCQ2RN/SPCQ2IN REGARDING CQ2-KEY
      * BAH 190218 CHANGED LTB-CHAPTER-X TO LTB-CHAPTER
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      *  CS 190301 BROKE OUT CQFILE INTO CQFILE(DETAIL) AND CQS-FILE
      *            (SUMMARY/HEADER), THE HEADER RECORD WAS A REDEFINE
      * BAH 190311 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      *         CQ1-KEY = CQ-BRNO, CQ-ACCTNO
      *
      *         CQ2-KEY = CQ-BRNO,CQ-COLLID,CQ-REV-BEHAVIOR-RISK,CQ-SEQ-FG,
      *                   CQ-DATE,CQ-START-TIME,,CQ-REV-DEL
      *                   CQ-MISC-KEY-FLD,CQ-REV-PDUE-DAYS,CQ-REV-BAL,
      *                   CQ-SEQ.
      *         GQ1-KEY   GQ-BRNO, GQ-ACCTNO
      *         GQ2-KEY = GQ-COLLID GQ-SEQ-FG GQ-DATE GQ-START-TIME
      *                    GQ-REV-DEL GQ-MISC-KEY-FLD
      *                    GQ-REV-PDUE-DAYS GQ-REV-BAL GQ-SEQ
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *     S T O P
      * DO NOT REMOVE CQ-TYPE COMMENT, THE TYPE IS A CLUE AS TO WHAT
      * TYPE OF RECORD YOU NEED, ESPECIALLY IN PROGRAMS THAT OPEN CQ AND CQS
      * THIS IS PROBABLY THE MOST DIFFICULT AS IT POPULATES A SAVE-CQ2-KEY
      * THAT IS GENERIC IN THAT IT COULD BE A DETAIL OR SUMMARY RECORD.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *  CS 190314 BROKE OUT QQFILE-GLOBAL QUEUE INTO DETAIL(GQFILE)AND
      *            SUMMARY(GQSFILE)
      *  CS 190307 KEN AND I DECIDED WE NEEDED TO HOLD THE DETAIL-CQ2-KEY
      *            WITHIN THE CQSFILE(CQS-CQ2-KEY-FIELDS). IN A15, EVERYTHING IS
      *            IN THE CQFILE, ITS DRIVEN BY TYPE THAT DETERMINES IF ITS A
      *            SUMMARY OR DETAIL CQ2 RECORD AND BOTH ARE USED TO SETUP THE
      *            GET OF THE NEXT RECORD.
      *            
      *  NOTE:CS  190319 WE STILL HAVE PROBLEMS WITH DUPLICATE CQ2-KEY 
      *                  ON F1-SKIP. WHATS HAPPENING IS WHEN WE F1-SKIP,
      *                  THE HEADER(CQSFILE IN A32)IS ADDING 1 TO 
      *                  CQS-CQ-REV-BAL(SUMMARY FILE) FOR THE NEXT READ 
      *                  OF THE DETAIL CQFILE. SO IF 2 ACCTS WITH THE SAME
      *                  CQ2-KEY,LETS SAY REV-BAL OF 998999, I SKIP THE FIRST
      *                  ACCT AND WRITE THE CQS(SUMMARY) WITH A REV-BAL OF
      *                  999000, SO ON THE NEXT READ,WE MOVE THE CQS-CQ-FIELDS
      *                  A REV-BAL OF 999000, THE DUPLICATES ARE SKIPPED,
      *                  (REV-BAL 998999). IF WE DIDNT ADD 1, WE WOULD CONTINUE
      *                  TO GET THE SAME ACCOUNT.
      * JUST AS A NOTE: WE GO THRU THE SAME STEP OF ADDING 1 TO THE HEADER 
      *         CQ-RECORD IN A15, AND CQ2-KEY DUPLICATES ARE BEING SKIPPED
      *         IN A15,A30,A32
      *
      *         CQ2-KEY = CQ-BRNO,CQ-COLLID,CQ-REV-BEHAVIOR-RISK,CQ-SEQ-FG,
      *                   CQ-DATE,CQ-START-TIME,,CQ-REV-DEL
      *                   CQ-MISC-KEY-FLD,CQ-REV-PDUE-DAYS,CQ-REV-BAL,
      *                   CQ-SEQ.
      *          CQ-SEQ DOES NOT EXIST IN A15 SO ADDED IN THIS RELEASE BUT ITS
      *          NOT BEING UPDATED ON DUPLICATES. THIS WAS ADDED TO CQRUN
      *              MOVE SAVE-CQ-SEQ TO CQ-SEQ. 
      *              PERFORM WRITE-CQ1-FILE.
      *              IF IO-FG NOT = 0      
      *                IF E-CODE = "22"   
      *                IF SAVE-CQ-SEQ < 999
      *                   ADD 1 TO SAVE-CQ-SEQ
      *                   GO TO WRITE-CQ-REC. 
      * I DONT SEE AN ANSWER HERE AND NOT SURE WHAT BARB WAS TRYING TO DO WITH
      * THE SEQUENCE                  
      *  NOTE: 
      *  CS 190327 SAVED WORKING COPY LIBSP/SPINQ2.190327
      *  CS 190327 GQS-BRNO IS ALWAYS POPULATED WITH ZERO, PER KEN REMOVE
      *            FROM FILE LAYOUT
      *  CS 190603 PRIOR TO WRITE OF CQ, ZERO FUTURE FIELDS, NO CLEAR
      *              CLEAR GQ FUTURE FIELDS
      *  CS 190605 ZERO FUTURE NUMERICS
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 20190710 ADDED SPCJCL, FUTURE NUMERICS WERE NOT CLEARED!
      * BAH 20190723 ADDED SPGQCL, FUTURE NUMERICS WERE NOT CLEARED!
      * BAH 20190918 REMOVED ACCESS-CALL ON CQFILE, GQFILE, FMFILE
      *  CS 20191231 IN CHECK FOR REAFFIRMATION WAS SETTING UP SEQ 5 WHEN
      *              ITS A SINGLE RECORD IN A32
      * BAH 20200707 REMOVED CLOSE-BR-FILE, OPEN UP TOP
      *  CS 20210426 REMOVED MOVE OF ZERO TO FUTURE FIELDS IN CREATE-LOC-
      *              QUEUE-DETAIL AND CREATE-GBL-QUEUE-DETAIL
      *  CS 20210506 REMOVED LWCQ2-FUTURE-KEY1,KEY2 AND KEEP-CQ-FUTURE-KEY1,
      *              KEY2,GQ-FUTURE-KEY1,KEY2,LWGQ2-FUTURE-KEY1,KEY2
      *              KEEP-GQ-FUTURE-KEY1,2
      * SMR 2023-0925 ADDED VCQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE CQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          CQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      * BAH 2024.0122 ADDED BILNSC-CALLED-FROM
      *
      * SMR 2024-0213 ADDED VGQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE GQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          GQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      * BAH 2025.0313 REMOVED LPNO1IN AND CLOSE-NO1-FILE. NOT USED. S35Q-176
      * BAH 2025.0320 ADDED ORIG-FORM-PROGX AND XPINQ-FORM-PATHNAME FOR
      *               A USER SECURITY ERROR, NEED SPINQ IN THE FORM-PATHNAME
      *               AND FORM-PROGX WHEN COMING FROM XPINQ, BUT NEED IT
      *               PUT BACK TO XPINQ AFTER USER SECURITY AS IT IS TESTED
      *               IN OTHER PLACES S35Q-187
      *
      * JKC 2025-0324 S35Q-191 MASSIVE FIX FOR COLLECTIONS; FOR USING
      *         A IDENTICAL A15 VISION STRUCTURE FOR THE VCQFILE; FIX
      *         HANDLING DUPLICATE CQ2-KEY WITHIN THE VCQFILE WHICH YOU
      *         CAN READ LIBSP/SP01VCQ FOR MORE DETAIL INFORMATION.
      *
      * JKC 2025-0402 S35Q-191 S35 USER TESTING FOUND PLACEHOLDER MEMOS
      *         CREATED WERE WRONG IN CREATE-MEMO-PLACEHOLDER SECTION.
      *         THE REASON THEY WERE WRONG WAS DUE TO IN THE
      *         REWRITE-PLACEHOLDER-MEMO PARAGRAPH AND 
      *         DELETE-PLACEHOLDER-MEMO SECTION WERE READING THE LMFILE
      *         THEN MOVING LM-MEMO INTO MEMO-PLACEHOLDER-BUF.  THIS
      *         POTENTIALLY REPLACED MEMO-PLACEHOLDER WHERE ITS
      *         VALUE CLAUSE OF "ACCOUNT IS CURRENTLY BEING WORKED BY "
      *         BY WHATEVER THE PREVIOUS READ OF LMFILE HAS IN LM-MEMO.
      *         TO RESOLVE IT I CHANGED IN CREATE-MEMO-PLACEHOLDER
      *         SECTION TO MOVE SPACES TO MEMO-PLACEHOLDER-BUF THEN MOVE
      *         "ACCOUNT IS CURRENTLY BEING WORKED BY " BACK INTO
      *         MEMO-PLACEHOLDER.  THIS WILL RESTORE EVERYTHING BACK.
      *
      * JKC 2025-0424 S35Q-200 ADDED NEW LOGIC FOR UPDATE THE VCQFILE;
      *          THE ORIGINAL VCQFILE WAS CREATED ONE PER USER PUT INTO
      *          /USR/TMP; THE NEW VCQFILE IS CREATED FOR ONE PER BRANCH
      *          PUT INTO /USR/DATA/R#/WK/VCQ#### WHICH WAS DONE FOR
      *          POTENTIALLY AVOID ISSUES WITH A VCQFILE ONE PER USER.
      * BAH 2025-0429 ADDED ACCESS-CALL ON VCQ IN DISPLAY-LOCAL-RESCHEDULE-INFO
      *               SO YOU CAN STILL PUT UP ACCOUNTS TO VIEW. SHOULD NOT
      *               ERROR OUT, JUST GO TO EXIT OF THAT SECTION
      *
      ******************************************************************
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 VCQ-FILE VGQ-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE VCQ-FILE.
           CLOSE VGQ-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CQS1-FILE.
           PERFORM CLOSE-CQ2-FILE.
           PERFORM CLOSE-DM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-GQ2-FILE.
           PERFORM CLOSE-GQS1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LSG1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-TZ1-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *    M A I N - P R O G R A M
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPINQ" TO VDU-FORM-NAME ORIG-FORM-PROGX.

      * WE HAVE TO GO INTO USER SECURITY WITH SPINQ AS THE FORM NAME
      * WITHIN FORM-PATHNAME OR WORLD GETS USER SECURITY ERROR AS THEY
      * HAVE XPINQ LOCKED OUT
           IF FORM-PROGX = "XPINQ"
              MOVE FORM-PROGX TO ORIG-FORM-PROGX
              MOVE FORM-PATHNAME TO XPINQ-FORM-PATHNAME
              MOVE "SPINQ" TO  XPINQ-FORM-NAME
              MOVE XPINQ-FORM-PATHNAME TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

      * NOW PUT FORM-PATHNAME AND FORM-PROGX BACK TO XPINQ AS IT IS CHECKED ALL
      * OVER THIS PROGRAM
           IF ORIG-FORM-PROGX = "XPINQ"
              MOVE "XPINQ" TO  XPINQ-FORM-NAME FORM-PROGX
              MOVE XPINQ-FORM-PATHNAME TO FORM-PATHNAME.

           PERFORM GET-GPENV.
 
      * /USR/TMP
           CALL "C$TOLOWER" USING WK-PATH, VALUE 8.

           IF FORM-PROGX = "SPINQ"
              DISPLAY STANDARD GRAPHICAL WINDOW
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      ERASE SCREEN
                      HANDLE IS SPINQ-WINDOW-HANDLE
           ELSE
              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
               COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS SPINQ-WINDOW-HANDLE.

           MOVE EXT-FILPATH         TO HOLD-EXT-FILPATH.

           PERFORM RESET-SCREEN.

           PERFORM OPEN-BR-FILE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF ( SELECTED-COLLID = SPACES )
              MOVE EXT-CONAME-USER-GROUP TO CASE-BUF
              CALL "C$TOUPPER" USING CASE-BUF, VALUE 3
              MOVE CASE-BUF TO USER-COLLID SELECTED-COLLID.

           MOVE 0 TO BORROWER-KEY.

           IF ( EXT-CONAME-USER-LOC NOT NUMERIC )
              MOVE "E" TO ERRCD
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

      * DO NOT USE FORM-PROGX AS CALLING PROGRAMS WIPES IT OUT!
           IF ORIG-FORM-PROGX = "XPINQ"
              MOVE " " TO VDU-WIMENU-SELECTION
           ELSE
              MOVE "C0" TO VDU-WIMENU-SELECTION.

           MOVE SPACES TO ERRCD LN-REC.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE SYSDATE-YYYYMMDD.

           MOVE 1 TO GB-NO.
           PERFORM OPEN-GB-FILE.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

      * CREATE & LOAD TEMPORARY VISION FILE
           IF BR-GLOBAL-QUEUE = "Y"
              PERFORM CREATE-VGQ-FILE
           ELSE
      *KC     PERFORM CREATE-VCQ-FILE.  >* DONE IN CPRUN CQRUN
              MOVE EXT-FILPATH-BASE     TO VCQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE  TO VCQ-ALL-MACHINE
              MOVE EXT-FILPATH-BRNO     TO VCQ-ALL-BRNO
              MOVE VCQ-ALL-PATH         TO VCQ-PATH.
     
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT

           PERFORM OPEN-SP1-FILE.

       MAIN-MODULE.

      *IN A15, SPINQ0 & SPINQ1 WERE SEPARATE, IN A30, COMBINED INTO 1
      *PROGRAM & THE HOLD-BORR-BUF NEEDS CLEARED. WAS DISPLAYING INFO 
      *FROM THE PRIOR BORROWER IN THE NEXT BORROWER DID NOT HAVE EMPLOYER
      *INFO.

           MOVE SPACES TO HOLD-BORR-INFO.
           MOVE 0      TO REMOTE-PHONE HOLD-PHONE.
           MOVE SPACES TO    ACTIVITY-MEMO-REQUIRED
                             ACTIVITY-RESULT-REQUIRED
                             RESULT-MEMO-REQUIRED
                             RESULT-RESCHED-REQ
                             RESULT-ACTIVITY-REQUIRED
                             ACTIVITY-CODE-ENTERED
                             MEMOSC-MEMO-TYPE
                             MEMOSC-ONLY
                             REMOTE-TIME-EDIT
                             REMOTE-TIME REMOTE-AM-PM.

           MOVE "N" TO  MEMO-ENTERED-FG         
                        TZ-FOUND
                        ALREADY-DISPLAYED-LOANS.

           PERFORM RESET-SCREEN.

      *---------------------------------------------------------------
      *    CHECK TO SEE IF WE WERE CALLED FROM THE BWLNS3 LOAN SCAN
      *    (F3 FROM SPINQ1).    IF FLAG IS 'E' THEN WE HAVE ALREADY
      *    DISPLAYED LOAN AND NEED TO EXIT W/O PROMPTING TO ENTER
      *    ANOTHER LOAN NUMBER.                           PR#2843
      *---------------------------------------------------------------
           IF BWLNS3-SCANYN = "E"
              IF WK-PATH NOT = " "
                 MOVE WK-PATH TO ACCESS-BUF
                 PERFORM REMOVE-WORKFILE
              END-IF
              MOVE "END" TO IO-TYPE
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE NOT = "PST"
              GO TO MAIN-MODULE.

      * ALLOW FOR F9 COLLECTOR MENU:
           MOVE WK-PATH    TO EXT-WI-CL-WK-PATH.
           MOVE TRANS-DATE TO EXT-WI-CL-TRANS-DATE.
           MOVE LN-OWNBR   TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO  TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE LN-SSNO(1) TO EXT-WI-CL-SSNO(1).
           MOVE LN-SSNO(2) TO EXT-WI-CL-SSNO(2).
           MOVE LN-SSNO(3) TO EXT-WI-CL-SSNO(3).
           MOVE LN-SSNO(4) TO EXT-WI-CL-SSNO(4).
           MOVE LN-MAKERCD(1) TO EXT-WI-CL-MAKERCD(1).
           MOVE LN-MAKERCD(2) TO EXT-WI-CL-MAKERCD(2).
           MOVE LN-MAKERCD(3) TO EXT-WI-CL-MAKERCD(3).
           MOVE LN-MAKERCD(4) TO EXT-WI-CL-MAKERCD(4).
      * SET OF (WI-CL-SSNO-) DO DONE IN ENTER-KEYS:
           MOVE LN-DLNO            TO EXT-WI-CL-DLNO.
           MOVE LN-SALEFIN         TO EXT-WI-CL-SALEFIN.
           MOVE LN-PAY-SCHLD-FG    TO EXT-WI-CL-PAY-SCHLD-FG.
           MOVE EXT-CONAME-SYSDATE TO EXT-WI-CL-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE POFF-NETDUE TO PAYOFF.

           MOVE INQ0-FIELDS      TO WR-BUF.
           MOVE INQ0-FIELDS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *
      *    IF GP-I-AM-SOAUTO OR GP-I-AM-FFG
      *       IF GP-CHBORR-PAST-DUE-DAYS NUMERIC
      *          IF GP-CHBORR-PAST-DUE-DAYS NOT = 0
      *             IF NO-PAST-DUE-DAYS NOT < GP-CHBORR-PAST-DUE-DAYS
      *                MOVE EXT-WI-CL-WORKER TO CHBORR-WORKER
      *                MOVE
      *            "PLEASE VERIFY ALL CUSTOMER INFO BEFORE PROCEEDING"
      *                     TO MESS PERFORM SEND-MESS
      *                MOVE "WI/CHBORR" TO FORM-NAM
      *                CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
      *                CANCEL FORM-PROGX.

           PERFORM CREATE-MEMO-PLACEHOLDER.

           PERFORM SPINQ1-SETUP.

       SPINQ1-MAIN-MODULE.

           PERFORM SPINQ1-INITIALIZATION.

           IF BW-LOANS > 1
              IF ALREADY-DISPLAYED-LOANS = "N"
                 MOVE "N" TO BWLNS3-ALL-LOANS
                 PERFORM DISPLAY-ALL-LOANS
                 MOVE "Y" TO ALREADY-DISPLAYED-LOANS.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO MAIN-MODULE.

           IF ( IO-TYPE = "CLR" )
              MOVE " " TO ACTIVITY-RESULT-REQUIRED
                          ACTIVITY-MEMO-REQUIRED
                          RESULT-MEMO-REQUIRED
                          RESULT-RESCHED-REQ
                          ACTIVITY-CODE-ENTERED
                          RESULT-ACTIVITY-REQUIRED
              GO TO SPINQ1-MAIN-MODULE.

           IF ( IO-TYPE = "CON" )
              MOVE " " TO ACTIVITY-RESULT-REQUIRED
                          ACTIVITY-MEMO-REQUIRED
                          RESULT-MEMO-REQUIRED
                          RESULT-RESCHED-REQ
                          ACTIVITY-CODE-ENTERED
                          RESULT-ACTIVITY-REQUIRED
              GO TO SPINQ1-MAIN-MODULE.

           GO TO SPINQ1-MAIN-MODULE.


      ************************************
      *    E N T E R   L N - K E Y       *
      ************************************
       ENTER-KEYS SECTION.
       ENTER-KEYS-NO.
           PERFORM RESET-PATH.
           MOVE SPACES TO ENTER-SEQNO.
           MOVE "Y" TO ENTER-SEQNO.
           MOVE "N" TO CALLED-BWLNSC-FG.
           MOVE SPACES TO IO-TYPE LN-REC.

      * DO NOT USE FORM-PROGX AS CALLING PROGRAMS WIPES IT OUT!
           IF ORIG-FORM-PROGX NOT = "XPINQ"
              IF ( BR-COLLECTOR-QUEUE = "Y" )
                 MOVE LEGEND-E1 TO LEGEND
              ELSE
                 MOVE LEGEND-EX TO LEGEND
           ELSE
              MOVE LEGEND-EX TO LEGEND.

           PERFORM SEND-LEGEND.

      *---------------------------------------------------------------
      *    IF CALLED FROM BWLNS3 LOAN SCAN THEN GET ACCOUNT NUMBER
      *    FROM FORM ACT AS THOUGH LOAN WAS CALLED FROM F6 SCAN. #2843
      *---------------------------------------------------------------
           IF BWLNS3-SCANYN = "Y"
              MOVE BWLNS3-LINK-ACCTNO TO LN-ACCTNO
              MOVE 0 TO IO-FG
              MOVE "Y" TO CALLED-BWLNSC-FG
              MOVE "E" TO BWLNS3-SCANYN
              GO TO READ-LNFILE1.

           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.

           MOVE " " TO QUEUED-FG
                       GET-ACCOUNT-FOR-GQ.

      * F3 - NEXT LOAN
      
      * DO NOT USE FORM-PROGX AS CALLING PROGRAMS WIPES IT OUT!
           IF ORIG-FORM-PROGX NOT = "XPINQ"
              IF ( F3-KEY )
                 IF ( BR-COLLECTOR-QUEUE = "Y" )
                    PERFORM FIND-NX-ACCT-QUEUED
                    IF ( IO-FG NOT = 0 )
                       GO TO ENTER-KEYS-NO
                    ELSE
                       IF BR-GLOBAL-QUEUE = "Y"
                          PERFORM GET-ACCOUNT
                          IF GET-ACCOUNT-ERR = "Y"
                             GO TO ENTER-KEYS-NO
                          ELSE
                             MOVE "Y" TO QUEUED-FG
                             GO TO READ-LNFILE1
                          END-IF
                       ELSE
                          MOVE "Y" TO QUEUED-FG
                          MOVE "00" TO VDUSTATUS.

      * F4 - GLOBAL LOAN SCAN
      
           IF ( F4-KEY )
              PERFORM GET-ACCOUNT
              IF ( GET-ACCOUNT-ERR = "Y" )
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.

      * F5 - QUEUE INFO
      
      * DO NOT USE FORM-PROGX AS CALLING PROGRAMS WIPES IT OUT!
           IF ORIG-FORM-PROGX NOT = "XPINQ"
              IF ( F5-KEY )
                 IF ( BR-COLLECTOR-QUEUE = "Y" )
                    PERFORM QUEUE-SCAN
                    IF ( IO-FG NOT = 0 )
                       GO TO ENTER-KEYS-NO
                    ELSE
                       IF BR-GLOBAL-QUEUE = "Y"
                          MOVE GQSCAN-ACCTNO TO LN-ACCTNO
                          MOVE LN-ACCTNO TO VDUNUM0 BILNSC-ACCTNO
                          MOVE GQSCAN-BRANCH TO BILNSC-BRANCH
                          MOVE "Y" TO GET-ACCOUNT-FOR-GQ
                          PERFORM GET-ACCOUNT
                          IF GET-ACCOUNT-ERR = "Y"
                             GO TO ENTER-KEYS-NO
                          ELSE
                             MOVE "Y" TO QUEUED-FG
                             GO TO READ-LNFILE1
                          END-IF
                       ELSE
                          MOVE "Y" TO QUEUED-FG
                          MOVE "00" TO VDUSTATUS.

      * F6 - LOAN SCAN
      
           IF F6-KEY
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.

      * F7 - EXIT
      
           IF F7-KEY
              IF WK-PATH NOT = " "
                 MOVE WK-PATH TO ACCESS-BUF
                 PERFORM REMOVE-WORKFILE.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              MOVE "E"   TO ERRCD
              GO TO END-ENTER-KEYS.

           IF NOT ENTER-KEY
              GO TO ENTER-KEYS-NO.

           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE VDUNUM0 TO LN-ACCTNO.
       READ-LNFILE1.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF LN-POCD = "**"
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-PB-RESC )
              MOVE "ACCOUNT IS IN RESCISSION RENEWAL STATUS" TO MESS
              PERFORM SEND-MESS.

            IF BORROWER-KEY NOT NUMERIC
              MOVE 0 TO BORROWER-KEY
           ELSE
              IF BORROWER-KEY NOT = LN-SSNO(1)
                           AND NOT = LN-SSNO(2)
                            AND NOT = LN-SSNO(3)
                             AND NOT = LN-SSNO(4)
                 MOVE 0 TO BORROWER-KEY.
           IF BORROWER-KEY NOT = 0
              MOVE BORROWER-KEY TO BW-SSNO
              MOVE 0 TO BORROWER-KEY
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.

      * SET START TIME:
           MOVE "Y" TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE HH-MM-SS TO START-TIME.

           PERFORM BW1-READ-IO.
           MOVE "PST" TO IO-TYPE.
           MOVE "Y" TO TEST-CALL-TIME-FG.
           PERFORM DISPLAY-BORROWER.
           IF TEST-CALL-TIME-FG = "F"
              MOVE SPACES TO D-TIME-ZONE
              IF GP-TIME-ZONE-FACTOR = -5 MOVE "EDT" TO D-TIME-ZONE
                 ELSE
              IF GP-TIME-ZONE-FACTOR = -6 MOVE "CDT" TO D-TIME-ZONE
                 ELSE
              IF GP-TIME-ZONE-FACTOR = -7 MOVE "MDT" TO D-TIME-ZONE
                 ELSE
              IF GP-TIME-ZONE-FACTOR = -8 MOVE "PDT" TO D-TIME-ZONE
                 ELSE
              IF GP-TIME-ZONE-FACTOR = -9 MOVE "ADT" TO D-TIME-ZONE
                 ELSE
              IF GP-TIME-ZONE-FACTOR = -10 MOVE "HDT" TO D-TIME-ZONE.
           IF TEST-CALL-TIME-FG = "F"
              IF GP-TIME-ZONE-DLS = "Y"
                 MOVE "S" TO D-DAYLIGHT
              END-IF
              MOVE GP-CALL-START-TIME TO D-CALL-START-TIME
              INSPECT D-CALL-START-TIME REPLACING ALL "/" BY ":"
              MOVE GP-CALL-END-TIME   TO D-CALL-END-TIME
              INSPECT D-CALL-END-TIME REPLACING ALL "/" BY ":"
              MOVE ILLEGAL-TIME-MSG TO MESS
              PERFORM SEND-MESS.
       
           IF LN-BNKRPTDATE NOT = 0
              PERFORM CHECK-FOR-REAFFIRMATION
              IF BK-REAFF-STAT NOT = "00"
                 MOVE BK-WORLD-SEQ TO PASSWDW-SEQ
                 PERFORM VERIFY-PASSWORD-WORLD
                 IF PASSWDW-STAT NOT = "OK"
                    PERFORM OPEN-BK-MSG-WINDOW
                    MOVE "E  A030SEL." TO VDU
                    PERFORM SCREENIO
                    PERFORM CLOSE-WINDOW
                    MOVE "SPINQ" TO VDU-FORM-NAME
                    IF VDUBUF NOT = "YES"
                       MOVE " " TO DISPLAY-BORR-BUF HOLD-BORR-INFO
                       MOVE DISPLAY-BORR-BUF TO WR-BUF
                       MOVE DISPLAY-BORR-LIST TO WR-LIST
                       PERFORM CALL-WRFORM
                       IF QUEUED-FG NOT = "Y"
                          GO TO ENTER-KEYS-NO
                       ELSE
                          IF BR-GLOBAL-QUEUE = "Y"
                             PERFORM GLOBAL-SKIP-RESET-ACCOUNT
                          ELSE
                             PERFORM LOCAL-SKIP-RESET-ACCOUNT
                          END-IF
                          GO TO ENTER-KEYS-NO
                       END-IF
                    ELSE
                       PERFORM CREATE-BK-ACCESSED-MEMO
                       PERFORM CREATE-BK-LOG.

      * REGIONAL ACC ... ASK FOR PASSWORD IF ACCOUNT HAS DEBTAIDATE
           IF LN-DEBTAIDATE NOT = ZEROS
               MOVE "DEBT AID" TO PASSWDW-PROMPT2
               MOVE DEBTAID-SEQ  TO PASSWDW-SEQ
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  MOVE SPACES TO DISPLAY-BORR-BUF HOLD-BORR-INFO
                  MOVE DISPLAY-BORR-LIST TO WR-LIST
                  MOVE DISPLAY-BORR-BUF TO WR-BUF
                  PERFORM CALL-WRFORM
                  IF QUEUED-FG NOT = "Y"
                     GO TO ENTER-KEYS-NO
                  ELSE
                     IF BR-GLOBAL-QUEUE = "Y"
                        PERFORM GLOBAL-SKIP-RESET-ACCOUNT
                     ELSE
                        PERFORM LOCAL-SKIP-RESET-ACCOUNT
                     END-IF
                     GO TO ENTER-KEYS-NO.

           PERFORM DISPLAY-LOAN.

           IF ( BR-COLLECTOR-QUEUE = "Y" )
              IF BR-GLOBAL-QUEUE = "Y"
                 PERFORM DISPLAY-GLOBAL-RESCHEDULE-INFO
              ELSE
                 PERFORM DISPLAY-LOCAL-RESCHEDULE-INFO.

      *    IF LN-OWNBR NOT = BR-NO
           IF LN-OWNBR NOT = EXT-CONAME-USER-LOC
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

           IF QUEUED-FG = "Y"
              COMPUTE BAL-WORKER = (999999 - (LN-CURBAL - LN-OT2BAL))
              IF BR-GLOBAL-QUEUE = "Y"
                 IF BAL-WORKER NOT = GQ-REV-BAL
                    MOVE "BALANCE HAS CHANGED SINCE QUEUE WAS CREATED!"
                      TO MESS
                    PERFORM SEND-MESS
                 END-IF
              ELSE
                 IF BAL-WORKER NOT = CQ-REV-BAL
                    MOVE "BALANCE HAS CHANGED SINCE QUEUE WAS CREATED!"
                      TO MESS
                    PERFORM SEND-MESS.

       END-ENTER-KEYS.
           EXIT.

      *************************************************
       CHECK-FOR-REAFFIRMATION SECTION.
      *************************************************
           MOVE "XX" TO BK-REAFF-STAT.
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1   TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           IF IO-GOOD
              IF NOT (LTB-REAFF-DATE = 0)
                 AND (LTB-DISCHARGE-DATE = 0)
              MOVE "00" TO BK-REAFF-STAT.

      *************************************************
       OPEN-BK-MSG-WINDOW SECTION.
      *************************************************
           MOVE 20 TO WINDOW-LINES.
           MOVE 75 TO WINDOW-COLUMNS.
           MOVE 2  TO WINDOW-BEGIN-Y.
           MOVE 2  TO WINDOW-BEGIN-X.
           MOVE "WI/WRLDBK" TO WINDOW-FORM-NAM.
           MOVE "WRLDBK" TO VDU-FORM-NAME.
           MOVE "WI/WRLDBK" TO WINDOW-FORM-NAM.
           PERFORM OPEN-WINDOW.
           DISPLAY WRLDBK-SCREEN.
       OPEN-BK-MSG-WINDOW-EXIT.    
           EXIT.

      **********************************************
       CREATE-BK-ACCESSED-MEMO SECTION.
      **********************************************
           MOVE SPACES TO HOLD-BK-MEMO.
           PERFORM OPEN-LM1-FILE.
           MOVE "BK ACCOUNT ACCESSED" TO HOLD-BK-TEXT.
           PERFORM SET-POSTDATE.
           MOVE POST-USERID            TO HOLD-POST-USERID.
           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO HOLD-POST-TIME.
           MOVE LN-OWNBR               TO LM-BRNO.
           MOVE LN-ACCTNO              TO LM-ACCTNO.
           SUBTRACT TRANS-DATE FROM 999999999 GIVING LM-DATE.

           MOVE HOLD-BK-MEMO TO LM-MEMO.
           MOVE 9            TO LM-MEMO-TYPE.
           MOVE 1 TO LM-SEQ.
       CREATE-BK-MEMO-AGAIN.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO CREATE-BK-MEMO-AGAIN.     
           PERFORM CLOSE-LM1-FILE.
       CREATE-BK-ASSESS-MEMO-EXIT.
           EXIT.

      *************************************************
       CREATE-BK-LOG SECTION.
      *************************************************
           MOVE "B" TO ERRLOGW-ACTION.
           MOVE LN-OWNBR TO   ERRLOGW-BRNO.
           MOVE LN-ACCTNO     TO WORK-ERRLOG-NUMBER.
           MOVE HOLD-BK-TEXT  TO  WORK-EL-BK-TEXT.
           MOVE TRANS-DATE    TO  DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WORK-ERRLOG-DATE.
           MOVE WORK-ERRLOG-DETAIL   TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX  USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PATH.
       CREATE-BK-LOG-EXIT.
           EXIT.

      ******************************************************************
      *    FIND NEXT ACCOUNT IN QUEUE
      ******************************************************************
       FIND-NX-ACCT-QUEUED SECTION.
           IF BR-GLOBAL-QUEUE = "Y"
              PERFORM FIND-NX-GLOBAL-ACCT-QUEUED
              PERFORM CLOSE-GQ2-FILE
           ELSE
              PERFORM FIND-NX-LOCAL-ACCT-QUEUED
              PERFORM CLOSE-VCQ2-FILE.

       END-FIND-NX-ACCT-QUEUED.
           EXIT.

      ******************************************************************
      *
      *    FIND NEXT LOCAL (CQ) ACCOUNT IN QUEUE
      *
      ******************************************************************
       FIND-NX-LOCAL-ACCT-QUEUED SECTION.
           IF ROUTE-CD = "W"
              PERFORM FIND-NEXT-WORKED-ACCT
              GO TO END-FIND-NX-LOCAL-ACCT-QUEUED.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM OPEN-CQS1-FILE.
           MOVE SELECTED-COLLID TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.
           IF IO-FG = 9
              MOVE "NO QUEUE FOUND FOR COLLECTOR &&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&" BY SELECTED-COLLID
              PERFORM SEND-MESS
              GO TO END-FIND-NX-LOCAL-ACCT-QUEUED.

      *    MOVE CQ2-KEY TO SAVE-CQ2-KEY.
           MOVE CQS-CQ2-KEY-FIELDS TO KEEP-CQ2-KEY.
           SUBTRACT CQS-WORKED-REC FROM CQS-TOT-REC GIVING ACCTS-TO-WORK.
           PERFORM CLOSE-CQS1-FILE.

           IF CQS-TIMED-REC NOT = 0
              MOVE "Y" TO TIME-MILITARY-FG
              MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR
              PERFORM ZONED-GET-TIME
              MOVE "Y" TO TIME-TEST-FG
              PERFORM CLEAR-CQ2-KEY
              MOVE SELECTED-COLLID TO CQ-COLLID
              MOVE "R" TO CQ-SEQ-FG
              MOVE 1 TO CQ-DATE
              MOVE 0 TO CQ-REV-BEHAVIOR-RISK
                        CQ-START-TIME CQ-REV-DEL
                        CQ-REV-PDUE-DAYS CQ-REV-BAL
                        CQ-SEQ
              PERFORM FIND-NEXT-LOCAL-ACCOUNT
              MOVE " " TO TIME-TEST-FG
              IF IO-FG = 0
                 MOVE CQ-ACCTNO TO LN-ACCTNO
                 MOVE LN-ACCTNO TO VDUNUM0
                 GO TO END-FIND-NX-LOCAL-ACCT-QUEUED.

      * INITIALIZE CQ2-KEY BASED ON CQS-REC  
           PERFORM SET-KEEP-CQ2-TO-CQ2.
          
           PERFORM FIND-NEXT-LOCAL-ACCOUNT.
           IF IO-FG = 9
              IF ACCTS-TO-WORK = 0
                 GO TO END-FIND-NX-LOCAL-ACCT-QUEUED
               ELSE
                 PERFORM CLEAR-CQ2-KEY
                 MOVE SELECTED-COLLID TO CQ-COLLID
                 MOVE 0 TO CQ-START-TIME CQ-REV-DEL
                           CQ-REV-BAL CQ-REV-PDUE-DAYS CQ-SEQ
                           CQ-REV-BEHAVIOR-RISK
                 MOVE 19000101 TO CQ-DATE
                 PERFORM FIND-NEXT-LOCAL-ACCOUNT
                    IF IO-FG = 9
                       GO TO END-FIND-NX-LOCAL-ACCT-QUEUED.

           MOVE CQ-ACCTNO TO LN-ACCTNO.
           MOVE LN-ACCTNO TO VDUNUM0.

       END-FIND-NX-LOCAL-ACCT-QUEUED.
           EXIT.


      ******************************************************************
      *
      *    FIND NEXT LOCAL WORKED ACCOUNT IN QUEUE (REVIEW MODE)
      *
      ******************************************************************
       FIND-NEXT-WORKED-ACCT SECTION.

           PERFORM SET-LWCQ2-TO-CQ2.
           PERFORM LOAD-VCQ2-KEY.
           PERFORM OPEN-VCQ2-FILE.
           PERFORM START-VCQ2-FILE-GREATER.

       NEXT-WORKED-ACCT.
           PERFORM READ-VCQ2-FILE-NEXT.

           IF IO-BAD 
              GO TO END-FIND-WORKED-ACCT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NEED TO READ THE CQFILE (SQL) TO ENSURE THAT ANOTHER USER
      *  HAS NOT WORKED OR IS IN THE PROCESS OF WORKING THIS ACCOUNT.
      *  ONCE DONE VALIDATING, RESTORE THE VCQ-CQ-REC BACK INTO CQ-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VCQ-ACCTNO TO CQ-ACCTNO.
           PERFORM OPEN-CQ1-FILE.
           PERFORM READ-CQ1-FILE.
           PERFORM CLOSE-CQ1-FILE.

           IF ( IO-BAD )
              GO TO END-FIND-WORKED-ACCT.

           IF CQ-WORKED-FG NOT = "Y"
              GO TO NEXT-WORKED-ACCT.

           IF CQ-IN-PROCESS = "Y"
              GO TO NEXT-WORKED-ACCT.

           MOVE VCQ-CQ-REC TO CQ-REC.

           IF CQ-BRNO NOT = CQ-PATH-OWNBR
              GO TO END-FIND-WORKED-ACCT.

           IF CQ-COLLID NOT = SELECTED-COLLID
              MOVE 9 TO IO-FG
              GO TO END-FIND-WORKED-ACCT.

           IF CQ-WORKED-FG NOT = "Y"
              GO TO NEXT-WORKED-ACCT.
           IF CQ-IN-PROCESS = "Y"
              GO TO NEXT-WORKED-ACCT.
      *    IF CQ-TYPE NOT = "D"
      *       MOVE 9 TO IO-FG
      *       GO TO END-FIND-WORKED-ACCT.

      * IN-PROCESS -IF Y MEANS IN PROCESS: ACCT IS BEING WORKED

        MARK-WORKED-ACCOUNT-IN-PROCESS.
           MOVE "Y" TO CQ-IN-PROCESS.
           MOVE USER-COLLID TO CQ-WORK-COLLID.
           PERFORM REWRITE-CQ1-VCQ1-FILE.
           MOVE CQ-ACCTNO TO LN-ACCTNO VDUNUM0.
       END-FIND-WORKED-ACCT.
           IF IO-FG = 9
              MOVE HIGH-VALUES TO LAST-WORKED-CQ2-KEY
              MOVE CQ-PATH-OWNBR TO LWCQ2-BRNO.

      ******************************************************************
      *    FIND NEXT LOCAL (CQ) ACCOUNT TO WORK IN QUEUE
      ******************************************************************
       FIND-NEXT-LOCAL-ACCOUNT SECTION.

           PERFORM OPEN-VCQ2-FILE.
           PERFORM LOAD-VCQ2-KEY.
           PERFORM START-VCQ2-FILE-GREATER.

       FIND-NEXT-LOCAL-ACCOUNT-NEXT.
           PERFORM READ-VCQ2-FILE-NEXT.

           IF IO-BAD 
              GO TO END-FIND-NEXT-LOCAL-ACCOUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NEED TO READ THE CQFILE (SQL) TO ENSURE THAT ANOTHER USER
      *  HAS NOT WORKED OR IS IN THE PROCESS OF WORKING THIS ACCOUNT.
      *  ONCE DONE VALIDATING, RESTORE THE VCQ-CQ-REC BACK INTO CQ-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VCQ-ACCTNO TO CQ-ACCTNO.
           PERFORM OPEN-CQ1-FILE.
           PERFORM READ-CQ1-FILE.
           PERFORM CLOSE-CQ1-FILE.

           IF ( IO-BAD )
              GO TO END-FIND-NEXT-LOCAL-ACCOUNT.

           IF CQ-WORKED-FG = "Y"
              GO TO FIND-NEXT-LOCAL-ACCOUNT-NEXT.

           IF CQ-IN-PROCESS = "Y"
              GO TO FIND-NEXT-LOCAL-ACCOUNT-NEXT.

           MOVE VCQ-CQ-REC TO CQ-REC.

           IF CQ-BRNO NOT = CQ-PATH-OWNBR
              GO TO END-FIND-NEXT-LOCAL-ACCOUNT.

           IF (CQ-COLLID NOT = SELECTED-COLLID)
              MOVE 9 TO IO-FG
              GO TO END-FIND-NEXT-LOCAL-ACCOUNT.

           IF CQ-WORKED-FG = "Y"
              GO TO FIND-NEXT-LOCAL-ACCOUNT-NEXT.
           IF CQ-IN-PROCESS = "Y"
              GO TO FIND-NEXT-LOCAL-ACCOUNT-NEXT.
           IF GP-USE-RESULT-WORKED-FG NOT = "N"
              IF CQ-ACCTNO = LOAN-KEY
                 GO TO FIND-NEXT-LOCAL-ACCOUNT-NEXT.
           IF TIME-TEST-FG NOT = "Y"
              IF CQ-WORKED-FG = "R"
                 MOVE 9 TO IO-FG
                 GO TO END-FIND-NEXT-LOCAL-ACCOUNT
              ELSE
                 GO TO MARK-LOCAL-ACCOUNT-IN-PROCESS.
       CHECK-LOCAL-SCHEDULED-REC.
           IF CQ-DATE > TRANS-DATE
              MOVE 9 TO IO-FG
              GO TO END-FIND-NEXT-LOCAL-ACCOUNT.
           IF CQ-DATE = TRANS-DATE
              IF CQ-START-TIME > H-AND-M
                 MOVE 9 TO IO-FG
                 GO TO END-FIND-NEXT-LOCAL-ACCOUNT.

      * IN-PROCESS -IF Y MEANS IN PROCESS: ACCT IS BEING WORKED

        MARK-LOCAL-ACCOUNT-IN-PROCESS.
           MOVE "Y" TO CQ-IN-PROCESS.
           MOVE USER-COLLID TO CQ-WORK-COLLID.
           PERFORM REWRITE-CQ1-VCQ1-FILE.

       END-FIND-NEXT-LOCAL-ACCOUNT.
           EXIT.

      ******************************************************************
      *
      *    FIND NEXT GLOBAL (GQ) ACCOUNT IN QUEUE
      *
      ******************************************************************
       FIND-NX-GLOBAL-ACCT-QUEUED SECTION.
           IF ROUTE-CD = "W"
              PERFORM FIND-NEXT-GLOBAL-WORKED-ACCT
              GO TO END-FIND-NX-GLOBAL-ACCT-QUEUED.
           MOVE " " TO GET-ACCOUNT-FOR-GQ.
           PERFORM OPEN-GQ1-FILE.
           PERFORM OPEN-GQS1-FILE.              

           MOVE SELECTED-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.   *>SUMMARY-BRANCH/COLLID
           IF IO-FG = 9
              MOVE "NO QUEUE FOUND FOR COLLECTOR &&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&" BY SELECTED-COLLID
              PERFORM SEND-MESS
              GO TO END-FIND-NX-GLOBAL-ACCT-QUEUED.

           MOVE GQS-GQ2-KEY-FIELDS TO KEEP-GQ2-KEY.
           SUBTRACT GQS-WORKED-REC FROM GQS-TOT-REC GIVING ACCTS-TO-WORK.
           PERFORM CLOSE-GQS1-FILE.
 
           IF GQS-TIMED-REC NOT = 0
              MOVE "Y" TO TIME-MILITARY-FG
              MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR
              PERFORM ZONED-GET-TIME
              MOVE "Y" TO TIME-TEST-FG
              MOVE SELECTED-COLLID TO GQ-COLLID
              MOVE "R" TO GQ-SEQ-FG
              MOVE 1 TO GQ-DATE
              MOVE 0 TO GQ-START-TIME GQ-REV-DEL
                        GQ-REV-PDUE-DAYS GQ-REV-BAL
                        GQ-SEQ
              PERFORM FIND-NEXT-GLOBAL-ACCOUNT
              MOVE " " TO TIME-TEST-FG
              IF IO-FG = 0
                 GO TO GLOBAL-ACCOUNT-FOUND.

           PERFORM SET-KEEP-GQ2-TO-GQ2.

           PERFORM FIND-NEXT-GLOBAL-ACCOUNT.
           IF IO-FG = 9
              IF ACCTS-TO-WORK = 0
                 GO TO END-FIND-NX-GLOBAL-ACCT-QUEUED
               ELSE
                 PERFORM CLEAR-GQ2-KEY
                 MOVE SELECTED-COLLID TO GQ-COLLID
                 MOVE 0 TO GQ-DATE GQ-START-TIME GQ-REV-DEL
                     GQ-REV-BAL GQ-REV-PDUE-DAYS GQ-SEQ
                 PERFORM FIND-NEXT-GLOBAL-ACCOUNT
                    IF IO-FG = 9
                       GO TO END-FIND-NX-GLOBAL-ACCT-QUEUED.

       GLOBAL-ACCOUNT-FOUND.
           MOVE GQ-ACCTNO TO LN-ACCTNO.
           MOVE LN-ACCTNO TO VDUNUM0 BILNSC-ACCTNO.
           MOVE GQ-BRNO   TO BILNSC-BRANCH.
           MOVE "Y"       TO GET-ACCOUNT-FOR-GQ.

       END-FIND-NX-GLOBAL-ACCT-QUEUED.
           EXIT.

      ******************************************************************
      *
      *    FIND NEXT GLOBAL WORKED ACCOUNT IN QUEUE (REVIEW MODE)
      *
      ******************************************************************
       FIND-NEXT-GLOBAL-WORKED-ACCT SECTION.
           PERFORM OPEN-VGQ1-FILE.
           PERFORM SET-LWGQ2-TO-GQ2.

           PERFORM LOAD-VGQ1-GQ2-KEY.
           PERFORM START-VGQ1-FILE-GREATER.

       NEXT-GLOBAL-WORKED-ACCT.
           PERFORM READ-VGQ1-FILE-NEXT.

           IF IO-BAD
              GO TO END-FIND-GLOBAL-WORKED-ACCT.

           MOVE VGQ-GQ-REC TO GQ-REC.

           IF GQ-COLLID NOT = SELECTED-COLLID
              MOVE 9 TO IO-FG
              GO TO END-FIND-GLOBAL-WORKED-ACCT.
           IF GQ-WORKED-FG NOT = "Y"
              GO TO NEXT-GLOBAL-WORKED-ACCT.
           IF GQ-IN-PROCESS = "Y"
              GO TO NEXT-GLOBAL-WORKED-ACCT.

      * IN-PROCESS -IF Y MEANS IN PROCESS: ACCT IS BEING WORKED

        MARK-GLOBAL-WORKED-ACCOUNT-IN-PROCESS.
           MOVE "Y" TO GQ-IN-PROCESS.
           MOVE USER-COLLID TO GQ-WORK-COLLID.
           PERFORM REWRITE-GQ2-FILE.
           MOVE GQ-ACCTNO TO LN-ACCTNO.
           MOVE LN-ACCTNO TO VDUNUM0 BILNSC-ACCTNO.
           MOVE GQ-BRNO   TO BILNSC-BRANCH.
           MOVE "Y"       TO GET-ACCOUNT-FOR-GQ.
        END-FIND-GLOBAL-WORKED-ACCT.
           IF IO-FG = 9
              MOVE HIGH-VALUES TO LAST-WORKED-GQ2-KEY.

      ******************************************************************
      *    FIND NEXT GLOBAL (GQ) ACCOUNT TO WORK IN QUEUE
      ******************************************************************
       FIND-NEXT-GLOBAL-ACCOUNT SECTION.
           PERFORM OPEN-VGQ1-FILE.

      *    PERFORM START-GQ2-FILE.
           PERFORM LOAD-VGQ1-GQ2-KEY.
           PERFORM START-VGQ1-FILE-GREATER.

       FIND-NEXT-GLOBAL-ACCOUNT-NEXT.
      *    PERFORM READ-GQ2-FILE-NEXT.
           PERFORM READ-VGQ1-FILE-NEXT.

           IF IO-BAD
              GO TO END-FIND-NEXT-GLOBAL-ACCOUNT.

           MOVE VGQ-GQ-REC TO GQ-REC.

           IF GQ-COLLID NOT = SELECTED-COLLID
              MOVE 9 TO IO-FG
              GO TO END-FIND-NEXT-GLOBAL-ACCOUNT.
           IF GQ-WORKED-FG = "Y"
              GO TO FIND-NEXT-GLOBAL-ACCOUNT-NEXT.
           IF GQ-IN-PROCESS = "Y"
              GO TO FIND-NEXT-GLOBAL-ACCOUNT-NEXT.
           IF GP-USE-RESULT-WORKED-FG NOT = "N"
              IF GQ-ACCTNO = LOAN-KEY
                 GO TO FIND-NEXT-GLOBAL-ACCOUNT-NEXT.
           IF TIME-TEST-FG NOT = "Y"
              IF GQ-WORKED-FG = "R"
                 MOVE 9 TO IO-FG
                 GO TO END-FIND-NEXT-GLOBAL-ACCOUNT
              ELSE
                 GO TO MARK-GLOBAL-ACCOUNT-IN-PROCESS.
       CHECK-GLOBAL-SCHEDULED-REC.
           IF GQ-DATE > TRANS-DATE
              MOVE 9 TO IO-FG
              GO TO END-FIND-NEXT-GLOBAL-ACCOUNT.
           IF GQ-DATE = TRANS-DATE
              IF GQ-START-TIME > H-AND-M
                 MOVE 9 TO IO-FG
                 GO TO END-FIND-NEXT-GLOBAL-ACCOUNT.


      *-----------------------------------------------------------------
      * IN-PROCESS -IF Y MEANS IN PROCESS: ACCT IS BEING WORKED
      *-----------------------------------------------------------------
        MARK-GLOBAL-ACCOUNT-IN-PROCESS.
           MOVE "Y" TO GQ-IN-PROCESS.
           MOVE USER-COLLID TO GQ-WORK-COLLID.
           PERFORM REWRITE-GQ2-FILE.

       END-FIND-NEXT-GLOBAL-ACCOUNT.
           EXIT.

      **********************************************
       BWLN-SCAN SECTION.
      **********************************************
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE   TO BWLNSC-SYSDATE.
           MOVE 0          TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BWLN-SCAN.

           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE BWLNSC-SSNO   TO BORROWER-KEY.
           MOVE 0   TO IO-FG.
           MOVE "Y" TO CALLED-BWLNSC-FG.

       EXIT-BWLN-SCAN.
           EXIT.

      ******************************************************************
      *
      *    Q U E U E - S C A N
      *
      ******************************************************************
       QUEUE-SCAN SECTION.
           IF BR-GLOBAL-QUEUE = "Y"
              PERFORM GQ-SCAN
           ELSE
              PERFORM CQ-SCAN.
       EXIT-QUEUE-SCAN.
           EXIT.

      ******************************************************************
      *
      *    C Q - S C A N
      *
      ******************************************************************
       CQ-SCAN SECTION.

           MOVE SP-EXPD-FRMLA          TO CQSCAN-SP-EXPD-FRMLA.
           MOVE BR-LOAN-NUM-FLAG       TO CQSCAN-BR-LOAN-NUM-FG.
           MOVE BR-COL-QUEUE-LOW-TO-HI TO CQSCAN-BR-LOW-TO-HI.
           MOVE EXT-CONAME-USER-LOC    TO CQSCAN-BRANCH.
           MOVE SELECTED-COLLID        TO CQSCAN-COLLID.
           MOVE VCQ-PATH               TO CQSCAN-VCQ-PATH.
           MOVE "WI/CQSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CQSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           MOVE CQSCAN-COLLID    TO SELECTED-COLLID.

           MOVE "S  A190COLQUEUE." TO VDU.
           IF ROUTE-CD = "W"
              MOVE "REVIEW:" TO COLQUEUE-PROMPT
           ELSE
              MOVE "QUEUE: " TO COLQUEUE-PROMPT.
           MOVE CQSCAN-COLLID      TO SELECTED-COLLID D-COLLECTOR.
           MOVE COLQUEUE-BUF       TO VDUBUF.
           PERFORM SCREENIO.

           IF ( CQSCAN-STATUS NOT = "00" ) OR ( CQSCAN-ACCTNO = 0 )
              MOVE 9 TO IO-FG
              GO TO EXIT-CQ-SCAN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NEED TO READ THE CQFILE (SQL) TO ENSURE THAT ANOTHER USER
      *  HAS NOT WORKED OR IS IN THE PROCESS OF WORKING THIS ACCOUNT.
      *  ONCE DONE VALIDATING, RESTORE THE VCQ-CQ-REC BACK INTO CQ-REC.
      *
      *  IF ACCOUNT SELECTED FROM SCAN, SET QUEUED FLAG SO QUEUE WILL
      *  CONTINUE FORWARD FROM SELECTED ACCOUNT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "D" TO CQ-TYPE.

           MOVE CQSCAN-ACCTNO TO CQ-ACCTNO.
           PERFORM OPEN-CQ1-FILE.
           PERFORM READ-CQ1-FILE.
           PERFORM CLOSE-CQ1-FILE.

           IF ( IO-BAD )
              GO TO EXIT-CQ-SCAN.

           IF ( CQ-IN-PROCESS = "Y" )
              MOVE "ACCOUNT ALREADY BEING WORKED" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-CQ-SCAN.

           MOVE CQSCAN-BRANCH TO VCQ-BRNO.
           MOVE CQSCAN-ACCTNO TO VDUNUM0 VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.
           IF IO-FG = 0
              MOVE VCQ-CQ-REC TO CQ-REC
              IF CQ-IN-PROCESS NOT = "Y"
                 MOVE "Y" TO CQ-IN-PROCESS
                 MOVE USER-COLLID TO CQ-WORK-COLLID
                 PERFORM REWRITE-CQ1-VCQ1-FILE.

           IF CQ-WORKED-FG NOT = "Y"
              MOVE " " TO ROUTE-CD
              GO TO CONTINUE-CQ-SCAN.

       SELECT-REVIEW-MODE.
           MOVE " START REVIEW WORKED ACCOUNTS MODE? (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNA010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = "N"
              GO TO CONTINUE-CQ-SCAN.
           IF VDUBUF NOT = "Y"
              GO TO SELECT-REVIEW-MODE.

           MOVE "W" TO ROUTE-CD.
       CONTINUE-CQ-SCAN.
           MOVE "S  A190COLQUEUE." TO VDU.
           IF ROUTE-CD = "W"
              MOVE "REVIEW:" TO COLQUEUE-PROMPT
           ELSE
              MOVE "QUEUE: " TO COLQUEUE-PROMPT.
           MOVE CQSCAN-COLLID      TO SELECTED-COLLID D-COLLECTOR.
           MOVE COLQUEUE-BUF       TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CQSCAN-ACCTNO TO VDUNUM0.

           MOVE "Y" TO QUEUED-FG.
           MOVE 0             TO IO-FG.

       EXIT-CQ-SCAN.
           EXIT.

      ******************************************************************
      *
      *    G Q - S C A N
      *
      ******************************************************************
       GQ-SCAN SECTION.

           MOVE EXT-CONAME-SYSDATE     TO GQSCAN-SYSDATE.
           MOVE SP-EXPD-FRMLA          TO GQSCAN-SP-EXPD-FRMLA.
           MOVE BR-LOAN-NUM-FLAG       TO GQSCAN-BR-LOAN-NUM-FG.
           MOVE BR-COL-QUEUE-LOW-TO-HI TO GQSCAN-BR-LOW-TO-HI.
           MOVE EXT-CONAME-USER-LOC    TO GQSCAN-BRANCH.
           MOVE VGQ-PATH               TO GQSCAN-VGQ-PATH.
           MOVE SELECTED-COLLID        TO GQSCAN-COLLID.
           MOVE "WI/GQSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GQSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           MOVE GQSCAN-COLLID    TO SELECTED-COLLID.

           MOVE "S  A190COLQUEUE." TO VDU.
           IF ROUTE-CD = "W"
              MOVE "REVIEW:" TO COLQUEUE-PROMPT
           ELSE
              MOVE "QUEUE: " TO COLQUEUE-PROMPT.
           MOVE GQSCAN-COLLID      TO SELECTED-COLLID D-COLLECTOR.
           MOVE COLQUEUE-BUF       TO VDUBUF.
           PERFORM SCREENIO.

           IF ( GQSCAN-STATUS NOT = "00" ) OR ( GQSCAN-ACCTNO = 0 )
              MOVE 9 TO IO-FG
              GO TO EXIT-GQ-SCAN.

      * IF ACCOUNT SELECTED FROM SCAN, SET QUEUED FLAG SO QUEUE WILL
      * CONTINUE FORWARD FROM SELECTED ACCOUNT.
           PERFORM OPEN-GQ1-FILE.
           MOVE GQSCAN-BRANCH TO GQ-BRNO.
      *KC  MOVE GQSCAN-ACCTNO TO VDUNUM0 ACCT-NO-NUM.
      *KC  MOVE 1 TO ACCT-NO-SEQ.
      *KC  MOVE ACCT-AND-SEQ TO GQ-ACCTNO.
           MOVE GQSCAN-ACCTNO TO VDUNUM0 GQ-ACCTNO.
           PERFORM READ-GQ1-FILE.
           IF IO-FG = 0
              IF GQ-IN-PROCESS NOT = "Y"
                 MOVE "Y" TO GQ-IN-PROCESS
                 MOVE USER-COLLID TO GQ-WORK-COLLID
                 PERFORM REWRITE-GQ1-FILE.
           PERFORM CLOSE-GQ1-FILE.

           IF GQ-WORKED-FG NOT = "Y"
              MOVE " " TO ROUTE-CD
              GO TO CONTINUE-GQ-SCAN.

       SELECT-GLOBAL-REVIEW-MODE.
           MOVE " START REVIEW WORKED ACCOUNTS MODE? (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNA010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = "N"
              GO TO CONTINUE-GQ-SCAN.
           IF VDUBUF NOT = "Y"
              GO TO SELECT-GLOBAL-REVIEW-MODE.

           MOVE "W" TO ROUTE-CD.
       CONTINUE-GQ-SCAN.
           MOVE "S  A190COLQUEUE." TO VDU.
           IF ROUTE-CD = "W"
              MOVE "REVIEW:" TO COLQUEUE-PROMPT
           ELSE
              MOVE "QUEUE: " TO COLQUEUE-PROMPT.
           MOVE GQSCAN-COLLID      TO SELECTED-COLLID D-COLLECTOR.
           MOVE COLQUEUE-BUF       TO VDUBUF.
           PERFORM SCREENIO.

      *KC  MOVE GQSCAN-ACCTNO TO VDUNUM0 ACCT-NO-NUM.
           MOVE GQSCAN-ACCTNO TO VDUNUM0.

           MOVE "Y" TO QUEUED-FG.
           MOVE 0             TO IO-FG.

       EXIT-GQ-SCAN.
           EXIT.

      *****************************************
      *    DISPLAY THE BORROWER
      *****************************************
       DISPLAY-BORROWER SECTION.
           MOVE " " TO DISPLAY-BORR-BUF.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-SOC.
           IF GP-HIDE-FIRST-5-SSNO = "Y"
              MOVE ALL "X" TO D-SOC1 D-SOC2.

           MOVE SPACES TO NAME-WORKER.
           IF BW-MNAME NOT = SPACES
              STRING BW-LNAME,", ",BW-FNAME," ",BW-MNAME
                       DELIMITED BY "   " INTO NAME-WORKER
           ELSE
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                       INTO NAME-WORKER.
           MOVE NAME-WORKER TO D-NAME-WORLD(1).
           MOVE BW-REPCODE  TO D-REP-WORLD(1).
           IF GP-DISPLAY-BAD-ADR-FLAG = "Y" AND BW-BAD-ADR-FG = "Y"
              MOVE SPACES TO D-NAME(1)
              MOVE NAME-WORKER TO D-NAME-WORLD(1)
              MOVE " BA" TO D-REP-WORLD(1).

           MOVE BW-ADR1 TO D-NAME(2).
           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2 TO D-NAME(3).

           MOVE SPACES  TO NAME-WORKER.
           MOVE BW-CITY TO NAME-WORKER.
           MOVE BW-STATE TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", *".
           INSPECT NAME-WORKER REPLACING FIRST "*               "
                                                        BY NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY " *".
           MOVE BW-ZIP TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "*               "
                                                        BY NAME.
           IF BW-ADR2 = SPACES
              MOVE NAME-WORKER TO D-NAME(3)
           ELSE
              MOVE NAME-WORKER TO D-NAME(4).
           MOVE BW-LOANS TO D-OPEN-LNS.
           MOVE BW-PRLOANS TO D-CLOSED-LNS.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO REMOTE-PHONE
              PERFORM GET-REMOTE-TIME
              IF TZ-FOUND = "Y"
                 MOVE "HOME:" TO HOME-PROMPT2
                 MOVE BW-PHONE TO D-PHONE2
                 INSPECT D-PHONE2 REPLACING ALL "/" BY "-"
                 MOVE REMOTE-TIME TO D-HTIME
                 MOVE REMOTE-AM-PM TO D-HAMPM
              ELSE
                 MOVE "HOME PHONE:" TO HOME-PROMPT
                 MOVE BW-PHONE TO D-PHONE
                 INSPECT D-PHONE REPLACING ALL "/" BY "-"
                 IF BW-CELL-PHONE NOT = 0 AND NOT = 2020202020 AND
                                    NOT = 0202020202
                    MOVE "H:" TO H-PROMPT
                    MOVE BW-PHONE TO D-HPHONE
                    INSPECT D-HPHONE REPLACING ALL "/" BY "-"
                    MOVE " C:" TO C-PROMPT
                    MOVE BW-CELL-PHONE TO D-CPHONE
                    INSPECT D-CPHONE REPLACING ALL "/" BY "-".

           IF BW-PHONE = 0 AND (BW-CELL-PHONE NOT = 0 AND
                                               NOT = 2020202020 AND
                                                NOT = 0202020202)
              MOVE "CELL:" TO HOME-PROMPT2
              MOVE BW-CELL-PHONE TO D-PHONE2
              INSPECT D-PHONE2 REPLACING ALL "/" BY "-".

           IF BW-EMPPHONE NOT = 0
              MOVE BW-EMPPHONE TO D-WPHONE
              INSPECT D-WPHONE REPLACING ALL "/" BY "-".
           IF BW-EMPEXT NOT = 0
              MOVE BW-EMPEXT TO D-WEXT.

              MOVE BW-EMPPHONE TO REMOTE-PHONE.
              PERFORM GET-REMOTE-TIME.

           IF TZ-FOUND = "Y" AND REMOTE-TIME(1:2) NOT = D-HTIME(1:2)
                 MOVE REMOTE-TIME TO D-WTIME
                 MOVE REMOTE-AM-PM TO D-WAMPM
              ELSE
                 MOVE BW-WORKHRS TO D-EMP-HOURS.
           IF BW-EMPLOYER NOT = SPACES
              MOVE "EMP:" TO EMP-PROMPT
              MOVE BW-EMPLOYER TO D-EMP-NAME.

           MOVE 1 TO SUB.
       DISPLAY-JOINT-OR-ENDORSER.
           ADD 1 TO SUB.
           IF SUB > 4 OR LN-MAKERCD(SUB) = SPACES
              GO TO CONTINUE-DISPLAY-BORROWER.

      * LOOKING FOR JOINT MAKER ANYWHERE, OTHERWISE, DISPLAY ENDORSER, IF
      * NO JOINT MAKER
           IF LN-MAKERCD(SUB) = "J"
              GO TO CONTINUE-DISPLAY-BORROWER
           ELSE
              IF LN-MAKERCD(SUB) = "E"
                 IF (LN-MAKERCD(2) NOT = "J" AND
                      LN-MAKERCD(3) NOT = "J" AND
                       LN-MAKERCD(4) NOT = "J" )
                          GO TO CONTINUE-DISPLAY-BORROWER
                 ELSE
                    GO TO DISPLAY-JOINT-OR-ENDORSER.

       CONTINUE-DISPLAY-BORROWER.
      * TEST THAT YOU DONT HAVE ACCOUNT PULLED UP BY SECOND MAKER !!
           IF LN-SSNO(SUB) NOT = 0 AND (LN-SSNO(SUB) NOT = BW-SSNO)
      * SAVE ORIGINAL BORROWER, CANT ASSUME ITS LN-SSNO(1)
              MOVE BW-SSNO TO HOLD-BW-SSNO
              MOVE LN-SSNO(SUB) TO BW-SSNO NUM-BW
              PERFORM BW1-READ-IO
              IF IO-FG = 0
                 IF LN-MAKERCD(SUB) = "J"
                    MOVE "JOINT :" TO D-SP0
                 ELSE
                    MOVE "ENDOR :" TO D-SP0
                 END-IF
                 PERFORM NUMBW-TO-ALPBW
                 MOVE ALP-BW TO D-SP1
                 IF GP-HIDE-FIRST-5-SSNO = "Y"
                    MOVE ALL "X" TO D-SP1-1 D-SP1-2
                 END-IF
                 MOVE BW-LNAME TO NAME-WORKER
                 MOVE BW-FNAME TO NAME
                 INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?"
                 INSPECT NAME-WORKER REPLACING FIRST "                "
                                                         BY NAME
                 INSPECT NAME-WORKER REPLACING ALL "?" BY " "
                 MOVE NAME-WORKER TO D-SP2
                 IF GP-DISPLAY-BAD-ADR-FLAG = "Y" AND
                                         BW-BAD-ADR-FG = "Y"
                    MOVE NAME-WORKER TO D-SP2-NAME
                    MOVE " ##" TO D-SP2-BAD
                 END-IF
      * GET ORIGINAL BORROWER BACK IN THE BUFFER
                 MOVE HOLD-BW-SSNO TO BW-SSNO
                 PERFORM BW1-READ-IO
                 GO TO END-DISPLAY-BORR
              ELSE
                 MOVE HOLD-BW-SSNO TO BW-SSNO
                 PERFORM BW1-READ-IO
                 GO TO END-DISPLAY-BORR.

           IF BW-SPSSNO = 0
              IF BW-SPLNAME = SPACES
                 IF BW-SPFNAME = SPACES
                    GO TO END-DISPLAY-BORR.

           MOVE "SPOUSE:" TO D-SP0.
           IF BW-SPSSNO NOT = 0
              MOVE BW-SPSSNO TO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE ALP-BW TO D-SP1
              IF GP-HIDE-FIRST-5-SSNO = "Y"
                 MOVE ALL "X" TO D-SP1-1 D-SP1-2.
           MOVE BW-SPLNAME TO NAME-WORKER.
           MOVE BW-SPFNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                                                    BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-SP2.
       END-DISPLAY-BORR.
           IF LN-POCD-PB-RESC
              MOVE "(( RESCISSION RENEWAL ))" TO D-MEMO
           ELSE
              IF BR-PRIO-MEMO-FLAG = "Y"
                 PERFORM GET-PRIORITY-MEMO
              ELSE
              IF BR-PRIO-MEMO-FLAG = "I"
                 PERFORM DISPLAY-INSURANCE-TRACKING
              ELSE
                 IF BR-PRIO-MEMO-FLAG = "C"
                    MOVE LN-COLLCD TO D-COLLCD
                    MOVE LN-COLLID TO D-COLLID.

           IF BW-CREDIT-SCORE NUMERIC
              AND  BW-CREDIT-SCORE > 0
                 MOVE "SCORE:" TO D-SCOREPROMPT
                 MOVE BW-CREDIT-SCORE TO D-CREDIT-SCORE
           ELSE
              MOVE SPACES TO D-SCOREPROMPT
              MOVE 0 TO D-CREDIT-SCORE.

           IF BW-ADR2 = SPACES
              MOVE HOLD-SPOUSE TO D-NAME(4)
              MOVE HOLD-SPOUSE-NAME TO D-NAME(5)
              MOVE HOLD-HOME-PHONE TO D-NAME(6)
              MOVE HOLD-EMP-NAME TO D-NAME(7)
              MOVE HOLD-WORK-PHONE TO D-NAME(8)
           ELSE
              MOVE HOLD-SPOUSE TO D-NAME(5)
              MOVE HOLD-SPOUSE-NAME TO D-NAME(6)
              MOVE HOLD-HOME-PHONE TO D-NAME(7)
              MOVE HOLD-EMP-NAME TO D-NAME(8)
              MOVE HOLD-WORK-PHONE TO D-NAME(9).

           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           MOVE DISPLAY-BORR-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.
           IF GP-DISPLAY-BAD-ADR-FLAG = "Y" AND D-REP-WORLD(1) = " BA"
                MOVE "S  A300A1:R." TO VDU
                MOVE D-NAME(1) TO VDUBUF
                PERFORM SCREENIO.
           IF GP-DISPLAY-BAD-ADR-FLAG = "Y" AND D-JOINT-BAD(5) = "##"
      * TRY TO LINE UP THE "BA"'S, WHAT A PAIN !
                MOVE "BA" TO D-JOINT-BAD(5)
                MOVE "S  A300A5:R." TO VDU
                MOVE D-NAME(5) TO TEST-JOINT-BAD
                MOVE SPACES TO D-NAME(5)
                MOVE TEST-JOINT-NAME TO D-NAME-WORLD(5)
                MOVE " BA" TO D-REP-WORLD(5)
                MOVE D-NAME(5) TO VDUBUF
                PERFORM SCREENIO.
           IF GP-DISPLAY-BAD-ADR-FLAG = "Y" AND D-JOINT-BAD(6) = "##"
                MOVE "BA" TO D-JOINT-BAD(6)
                MOVE "S  A300A6:R." TO VDU
                MOVE D-NAME(6) TO TEST-JOINT-BAD
                MOVE SPACES TO D-NAME(6)
                MOVE TEST-JOINT-NAME TO D-NAME-WORLD(6)
                MOVE " BA"           TO D-REP-WORLD(6)
                MOVE D-NAME(6) TO VDUBUF
                PERFORM SCREENIO.

      ***********************************
      *    GET PRIORITY MEMO
      ***********************************
       GET-PRIORITY-MEMO SECTION.
           MOVE BW-SSNO TO BM-SSNO.
           MOVE 0 TO BM-DATE.
           MOVE 1 TO BM-SEQ.
           PERFORM OPEN-BM1-FILE.
           PERFORM READ-BM1-FILE.
           PERFORM CLOSE-BM1-FILE.
           IF IO-FG = 0
              MOVE BM-MEMO TO D-MEMO.

      *****************************************
      *    DISPLAY THE LOAN
      *****************************************
       DISPLAY-LOAN SECTION.
           IF BORROWER-SCAN = "Y"
              GO TO DISPLAY-LOAN-EXIT.

           MOVE " " TO DISPLAY-LOAN-BUF.

           IF ( LN-POCD-VOID ) OR ( LN-POCD-RESCIND )
              GO TO DISPLAY-LOAN-EXIT.

           PERFORM GET-SP.
           PERFORM GET-LC.

           PERFORM LPSTAT-DISPLAY.

           IF LN-ACCT-FROZEN = "Y"
              STRING LPS-DISPLAY-LNNO,"F", DELIMITED BY "  " INTO
                       D-LOAN-NUM
           ELSE
              MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-SSNO(1) = BW-SSNO
              MOVE 1 TO EXT-WI-CL-SSNO-NO
              MOVE LN-MAKERCD(1) TO MAKER-CD
           ELSE
              IF LN-SSNO(2) = BW-SSNO
                 MOVE 2 TO EXT-WI-CL-SSNO-NO
                 MOVE LN-MAKERCD(2) TO MAKER-CD
              ELSE
                 IF LN-SSNO(3) = BW-SSNO
                    MOVE 3 TO EXT-WI-CL-SSNO-NO
                    MOVE LN-MAKERCD(3) TO MAKER-CD
                 ELSE
                    IF LN-SSNO(4) = BW-SSNO
                       MOVE 4 TO EXT-WI-CL-SSNO-NO
                       MOVE LN-MAKERCD(4) TO MAKER-CD.
           PERFORM DISPLAY-MAKER.

           IF GP-GPS-INFORMATION = "Y"
              MOVE "S  A020OO:RF." TO VDU
              PERFORM OPEN-LSG1-FILE
              MOVE LN-OWNBR  TO LSG-BRNO
              MOVE LN-ACCTNO TO LSG-ACCTNO
              MOVE 1         TO LSG-SEQNO
              PERFORM READ-LSG1-FILE
              PERFORM CLOSE-LSG1-FILE
              IF IO-FG = 0
                 IF LSG-STARTER-STAT = "D"
                    MOVE "CD" TO VDUBUF
                    PERFORM SCREENIO
                 ELSE
                    IF LSG-WARNING-STAT = "E"
                       MOVE "PR" TO VDUBUF
                       PERFORM SCREENIO.


           MOVE LN-LOANDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-LOANDATE.
           MOVE LN-1STPYDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT   TO D-1STPYDATE.
           MOVE LN-SUM-LNAMT    TO D-LOANAMT.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NUM-DATE.
           IF LN-UNITPER-CD-DAYS
              MOVE 0 TO NUM-CCYY
           ELSE
              MOVE NUM-YR TO NUM-DA.
           MOVE 0 TO NUM-CCYY.
           MOVE NUM-DATE TO D-PDTHRU.
           MOVE PDTH-CUR-SCHD-PAY TO D-REGPYAMT.
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE "S" TO D-REGPYAMT-S.

           MOVE LN-LCPDTH-DATE TO LCHG-LCPDTH-DATE.
           MOVE PDTH-CUR-SCHD-PAY TO LCHG-LATE LCHG-CURPAY.
           PERFORM LATE-CHARGE-CALCULATION.

           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                              AND (SP-LCFRMLA NOT = " ")
                               AND (LN-ACCELDATE = 0)
                            OR
                   ( LN-LC-CODE NOT = ZEROES )
              MOVE LCHG-CHARGE TO D-LCHG
              MOVE LN-LCPDTH-DATE TO NUM-DATE
              IF LN-UNITPER-CD-DAYS
                 MOVE 0 TO NUM-CCYY
                 MOVE NUM-DATE TO D-LCPDTH
              ELSE
                 MOVE NUM-YR TO NUM-DA
                 MOVE NUM-DATE TO D-LCPDTH.

           MOVE LN-TOTNODEF TO D-NO-DEF.
           MOVE LN-TOTNOALDEL TO D-ADEL.

           MOVE LN-SUM-ORGTERM TO D-ORGTERM.
           PERFORM MATURITY-DATE-CALCULATION.
      *    MOVE MDTE-DATE TO D-MATURITY.

           PERFORM SET-PAYOFF-DATE-WORKERS.

           MOVE NO-LDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LDUED.
      *    MOVE NO-TDUE-DATE TO D-TDUED.

           MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-NDUED.
           IF LN-MIP-FG = "Y"
              MOVE NO-LDUE-DATE TO POFF-PAYDATE
              PERFORM DETERMINE-MIP-REQUIRED
              IF LN-MIP-CLPDTH-DATE NOT = NO-LDUE-DATE
                 ADD POFF-LXM-MIP-CLPAID
                               TO NO-LDUECONTR NO-LDUEAMT
              END-IF
              IF LN-MIP-AHPDTH-DATE NOT = NO-LDUE-DATE
                 ADD POFF-LXM-MIP-AHPAID
                               TO NO-LDUECONTR NO-LDUEAMT
              END-IF
              MOVE NO-TDUE-DATE TO POFF-PAYDATE
              PERFORM DETERMINE-MIP-REQUIRED
              IF POFF-LXM-MIP-CLPAID NOT = 0
                 MOVE LN-MIP-CLPDTH-DATE TO NDTE-DATE
                 MOVE 1             TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 IF NO-TDUE-DATE NOT < NDTE-DATE
                    ADD POFF-LXM-MIP-CLPAID
                                 TO NO-TDUECONTR NO-TDUEAMT
                 END-IF
              END-IF
              IF POFF-LXM-MIP-AHPAID NOT = 0
                 MOVE LN-MIP-AHPDTH-DATE TO NDTE-DATE
                 MOVE 1             TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 IF NO-TDUE-DATE NOT < NDTE-DATE
                    ADD POFF-LXM-MIP-AHPAID
                                 TO NO-TDUECONTR NO-TDUEAMT
                 END-IF
              END-IF
              MOVE NO-NXDUE-DATE TO POFF-PAYDATE
              PERFORM DETERMINE-MIP-REQUIRED
              ADD POFF-LXM-MIP-CLPAID
                  POFF-LXM-MIP-AHPAID TO NO-NXDUECONTR NO-NXDUEAMT.

           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-LDUECONTR  TO D-LDUE
              MOVE NO-TDUECONTR  TO D-TDUE SAVE-TDUE
              MOVE "DUE CONTR:"  TO D-TDUE-PROMPT
              MOVE NO-TDUEAMT    TO D-TDUE-CONT
              MOVE NO-NXDUECONTR TO D-NDUE HOLD-NDUE
           ELSE
              MOVE NO-LDUEAMT    TO D-LDUE
              MOVE NO-TDUEAMT    TO D-TDUE SAVE-TDUE
              MOVE " "           TO D-TDUE-PROMPT
              MOVE 0             TO D-TDUE-CONT
              MOVE NO-NXDUEAMT   TO D-NDUE HOLD-NDUE.


      * IF ACCOUNT HAS A DUE DAY OF 29,30 OR 31 AND THAT DATE DOES NOT EXIST
      * IN THE CURRENT MONTH, ON THE LAST DAY OF THE MONTH, ADD IN A REGULAR
      * PAYMENT TO THE DUE AMOUNT
           IF GP-DUE-29-30-31 = "Y"
              IF LN-1STPYDA = 29 OR 30 OR 31
      * FIRST GET LAST DAY OF CURRENT MONTH
                 MOVE TRANS-DATE TO NDTE-DATE
                 MOVE 31         TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
      * CHECK TO SEE IF TODAY IS LAST DAY OF CURRENT MONTH
                 IF NDTE-DATE = TRANS-DATE
      * IF SO, DOES THE DUE DAY EXIST IN THIS MONTH ?
                    IF LN-1STPYDA > NDTE-DD
                       ADD LN-REGPYAMT TO SAVE-TDUE
                       MOVE SAVE-TDUE TO D-TDUE.

           MOVE "DF" TO DEFPOL-TRCD.
           MOVE TRANS-DATE TO DEFPOL-DATE.
           PERFORM VALIDATE-DF-POLICY.

           IF (LN-CURBAL NOT = 0) AND (DEFPOL-MAXIMUM = " ")
      * JTG WILL USE NO-DEFERMENT AMOUNT
      *       MOVE 1 TO DEF-NODEF
      *       PERFORM DEF-COMPUTE-DEFERMENT
      * BAH V  1/8/01
      *       MOVE NO-DEFERMENT TO DEF-AMOUNT
      * BAH ^  1/8/01
      *
              COMPUTE DEF-AMOUNT = NO-DEFERMENT - INDU-PENALTY
              IF DEF-AMOUNT NOT = 0
                 MOVE NO-PDTH-DATE TO NDTE-DATE
                 MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
                 MOVE LN-UNITPER-FREQ TO LN-UNITPER-FREQ
                 MOVE 1 TO NDTE-HOLD
                 PERFORM INCREMENT-UNITPER
                 IF LN-UNITPER-CD-DAYS
                    MOVE NDTE-DD TO NDTE-YY
                 END-IF
                 MOVE NDTE-MM TO NDTE-DD
                 MOVE "DEF:" TO D-DEFALP
              ELSE
                 MOVE " " TO D-DEFALP
                 MOVE 0 TO DEF-AMOUNT NDTE-DATE
           ELSE
              MOVE " " TO D-DEFALP
              MOVE 0 TO DEF-AMOUNT NDTE-DATE.

           MOVE NDTE-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-DEFD.
           MOVE DEF-AMOUNT        TO D-DEF.
           MOVE LN-CURBAL         TO D-BALANCE.
           MOVE POFF-NETDUE       TO D-PAYOFF.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-LPAYD.
           MOVE LN-AMT-PAID-LAST  TO D-LPAY.

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF LC-CON-PDUE-DAYS = "D"
              MOVE TRANS-DATE TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              MOVE "DAY:" TO D-CONPROMPT
              MOVE PDUE-DAYS TO D-CON
           ELSE
              MOVE "CON:" TO D-CONPROMPT
              IF CPOT30 = "Y"
                 MOVE "<P>" TO D-CONX
              ELSE
              IF CPOT30 = "M"
                 MOVE "<M>" TO D-CONX
              ELSE
              IF CPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-CONX
              ELSE
              IF CPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-CONX
              ELSE
                 MOVE CONTRACTUAL TO D-CON.

           MOVE LN-CONPROFILE(1) TO D-CONPROFILE.
           INSPECT D-CONPROFILE REPLACING ALL "9" BY "-".

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-RECENCY.
           IF LC-REC-PDUE-DAYS = "D"
              MOVE TRANS-DATE TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              MOVE "DAY:" TO D-RECPROMPT
              MOVE PDUE-DAYS TO D-REC
           ELSE
              MOVE "REC:" TO D-RECPROMPT
              IF RDPOT30 = "Y"
                 MOVE "<P>" TO D-RECX
              ELSE
              IF RDPOT30 = "M"
                 MOVE "<M>" TO D-RECX
              ELSE
              IF RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-RECX
              ELSE
              IF RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-RECX
              ELSE
                 MOVE RECDEL TO D-REC.
           MOVE LN-RECPROFILE(1) TO D-RECPROFILE.
           INSPECT D-RECPROFILE REPLACING ALL "9" BY "-".

           MOVE LN-DLNO TO D-DLNO.
           MOVE LN-DLRECOURSE TO D-DLRECOURSE.

           PERFORM GET-SRCD-SUB.

      * COMMENTED OUT WORLD PR#1229, THEY WANT TO SEE EVEN IF CHARGED OFF
      *    IF SRCD-NOT-PL
              EVALUATE TRUE
                 WHEN SRCD-IS-PBD MOVE "[PBD]" TO D-SRCD
                 WHEN SRCD-IS-PBI MOVE "[PBI]" TO D-SRCD
                 WHEN OTHER       STRING "[" LN-SRCD " ]"
                                  DELIMITED BY SIZE INTO D-SRCD
              END-EVALUATE.

           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           MOVE DISPLAY-LOAN-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 10
               IF LN-STATUSFLAGS(S1) = "C"
                  MOVE 10 TO S1
                  MOVE "S  A160LNNO:R." TO VDU
                  MOVE D-LOAN-NUM TO VDUBUF
                  PERFORM SCREENIO
               END-IF
           END-PERFORM.


           IF (LN-ACH = "Y" AND GP-LOAN-ACH-PROCESSING = "Y")
              MOVE ACH-PROMPT-LIST TO WR-LIST
              MOVE ACH-PROMPT-BUF  TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
              MOVE PMT-PROMPT-LIST TO WR-LIST
              MOVE PMT-PROMPT-BUF  TO WR-BUF
              PERFORM CALL-WRFORM.


           IF GP-USE-SCRA = "Y"
              PERFORM OPEN-LSR1-FILE
              MOVE LN-OWNBR  TO LSR-BRNO
              MOVE LN-ACCTNO TO LSR-ACCTNO
              MOVE 1         TO LSR-SEQNO
              PERFORM READ-LSR1-FILE
              PERFORM CLOSE-LSR1-FILE
              IF IO-FG = 0
                 IF LSR-SCRA-ACTIVE = "Y"
                    MOVE "S  A180SPMEMO:RF." TO VDU
                    MOVE "SCRA" TO VDUBUF
                    PERFORM SCREENIO.

      * ----------------

           IF LN-BNKRPTDATE NOT = 0
              PERFORM GET-LT-BANKRUPT
              IF IO-FG = 9
                 MOVE 0 TO LTB-CHAPTER
                 MOVE SPACES TO LTB-MISC3
              END-IF
              MOVE "S  A180SPMEMO:RF." TO VDU
              IF LTB-MISC3 = " "
                 STRING "<BANKRUPT("
                     LTB-CHAPTER
                     "),"
                     LTB-WHO-FILED
                     ","
                     LTB-ACTIVE-INACTIVE
                     ">"
                  DELIMITED BY SIZE INTO VDUBUF
              ELSE
                 MOVE LTB-MISC3 TO VDUBUF
              END-IF
              PERFORM SCREENIO.


      * CHECK FOR BANKRUPTS REAFFIRMATION OR DISCHARGE
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           IF IO-FG = 0
              MOVE "S  A180SPMEMO:RF." TO VDU
              MOVE SPACES TO VDUBUF
              IF (LTB-REAFF-DATE NOT = 0 AND LTB-RESCIND-DATE = 0)
                 STRING "REAFFIRMED, ", LTB-WHO-FILED DELIMITED BY
                                 "  " INTO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 IF LTB-DISCHARGE-DATE NOT = 0
                    MOVE LTB-DISCHARGE-DATE TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    STRING "DISCH,",LTB-WHO-FILED,",",DATE-MMDDYY
                             DELIMITED BY "  " INTO VDUBUF
                    PERFORM SCREENIO.

           MOVE LN-MESSAGE TO D-MESSAGE.
           IF LN-MESSAGE NOT = SPACES
              MOVE MESSAGE-LIST-ATTR TO WR-LIST
              MOVE D-MESSAGE         TO WR-BUF
              PERFORM CALL-WRFORM.

      * CHECK TO SEE IF ACCT WOULD BE A PBD IF PD OFF TODAY
           MOVE " " TO PBD-FLAG.
           MOVE TRANS-DATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE..
           PERFORM PAST-DUE-DAYS-CALCULATION.
           IF SP-PBDEL-DAYS NOT = 0
              IF PDUE-DAYS > SP-PBDEL-DAYS
                   MOVE "D" TO PBD-FLAG.

           IF BR-AVAIL-CREDIT = "Y" AND
                 (LN-CREDITLIM NOT = 0) AND (LN-BNKRPTDATE = 0)
              MOVE " HIGH CRED:" TO HIGH-CREDIT-X-BUF
              MOVE LN-CREDITLIM TO HIGH-CREDIT-BUF
              MOVE " AVAIL:" TO AVA-BUF
              MOVE NO-AVAILCREDIT TO AVAIL-AMT
              IF NO-AVCR-RESTRICT-FG = " "
                 MOVE PBD-FLAG TO AVAIL-FLAG1
                 MOVE " "      TO AVAIL-FLAG2
              ELSE
                 MOVE NO-AVCR-RESTRICT-FG TO AVAIL-FLAG1
                 MOVE PBD-FLAG              TO AVAIL-FLAG2
              END-IF
              MOVE HIGH-LIST-ATTR TO WR-LIST
              MOVE HIGH-BUF       TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
              IF BR-AVAIL-CREDIT = "S" AND
                    (LN-CREDITLIM NOT = 0) AND (LN-BNKRPTDATE = 0)
                 MOVE " CUR AVAIL:" TO HIGH-CREDIT-X-BUF
      * SETTING CUR. AVAILABLE BY HIGH CREDIT IN LOAN RECORD
                 MOVE AVCR-AVAILCREDIT TO HIGH-CREDIT-BUF
                 MOVE " AVAIL:" TO AVA-BUF
      * SETTING AVAILABLE BY A NET PAYOFF
                 MOVE AVCR-CURRENT-AVAIL TO AVAIL-AMT
                 IF NO-AVCR-RESTRICT-FG = " "
                    MOVE PBD-FLAG TO AVAIL-FLAG1
                    MOVE " "      TO AVAIL-FLAG2
                 ELSE
                    MOVE NO-AVCR-RESTRICT-FG TO AVAIL-FLAG1
                    MOVE PBD-FLAG              TO AVAIL-FLAG2
                 END-IF
                 IF LN-CLASS = 5 OR 8 OR 11
                    MOVE SPACES TO AVA-BUF AVAIL-SPACE
                    MOVE HIGH-LIST-S5 TO WR-LIST
                    MOVE HIGH-BUF     TO WR-BUF
                    PERFORM CALL-WRFORM
                 ELSE
                    MOVE HIGH-LIST-ATTR TO WR-LIST
                    MOVE HIGH-BUF       TO WR-BUF
                    PERFORM CALL-WRFORM
              ELSE
      * DISPLAY BRANCH AND HIGHLIGHT
                 IF BR-AVAIL-CREDIT = "B"
                    MOVE "BRANCH:" TO HIGH-CREDIT-X-BUF
                    MOVE BR-NO     TO HIGH-CREDIT-BUF
                    MOVE "AVAIL :" TO AVA-BUF
                    MOVE NO-AVAILCREDIT TO AVAIL-BUF
                    MOVE HIGH-LIST-S5   TO WR-LIST
                    MOVE HIGH-BUF       TO WR-BUF
                    PERFORM CALL-WRFORM
                 ELSE
                    MOVE SPACE TO HIGH-BUF AVA-BUF
                    MOVE MDTE-DATE TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO MATU-DATE
                    MOVE "MATURE:" TO AVA-BUF
                    MOVE "BRANCH:" TO HIGH-CREDIT-X-BUF
                    MOVE BR-NO TO HIGH-CREDIT-BUF
                    MOVE HIGH-LIST TO WR-LIST
                    MOVE HIGH-BUF  TO WR-BUF
                    PERFORM CALL-WRFORM.

           IF IO-TYPE = "PST"
              GO TO DISPLAY-LOAN-EXIT.

       DISPLAY-LOAN-EXIT.
           EXIT.

      **********************************
      *TEST-IF-SC-RESTRICTION SECTION.
      **********************************
      * CHECK FOR THE SOUTH CAROLINA SERVICE CHARGE FRMLA 18 RESTRICTION
      * IF WE WOULD REJECT A RENEWAL, DONT DISPLAY AVAIL CREDIT
      * IF CASH AVAILBLE IS < 10% OF LOANS AMOUNT FINANCED, RESTRICT IT
      * NOT ALLOWED TO RENEW AN ACCOUNT FOR A CASH ADVANCE LESS THAN 10%
      * MORE THAN ONCE IN 15 MONTHS.
      *    IF NOT GP-I-AM-WORLD
      *       GO TO EXIT-TEST-IF-SC-RESTRICTION.
      *    IF SP-SCFRMLA NOT = 18
      *       GO TO EXIT-TEST-IF-SC-RESTRICTION.

      *    PERFORM LOAN-CALCULATIONS.
      *    IF NO-AVAILCREDIT < (FINANCED-AMOUNT * .1)
      *       IF BW-LAST-RN-FOR10-DATE NOT = 0
      *          MOVE BW-LAST-RN-FOR10-DATE TO NUM-DATE
      *          MOVE TRANS-DATE TO SYS-DATE
      *          PERFORM TIM360
      *          IF ELAPSED-MONTHS < 15
      *             MOVE 0 TO AVAIL-BUF.


      *EXIT-TEST-IF-SC-RESTRICTION.
      *    EXIT.

      ******************************************************************
      *    DISPLAY LOCAL RESCHEDULE INFO FROM QUEUE
      ******************************************************************
       DISPLAY-LOCAL-RESCHEDULE-INFO SECTION.

           MOVE VCQ-PATH                TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              GO TO EXIT-DISPLAY-LOCAL-RESCH-INFO.

           MOVE LN-OWNBR  TO VCQ-BRNO.
           MOVE LN-ACCTNO TO VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.

           IF ( IO-BAD )
              GO TO EXIT-DISPLAY-LOCAL-RESCH-INFO.

           MOVE VCQ-CQ-REC TO CQ-REC.

           IF ( CQ-SEQ-FG NOT = "R" )
              GO TO EXIT-DISPLAY-LOCAL-RESCH-INFO.

           MOVE CQ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-RESCH-DATE.
           MOVE CQ-START-TIME TO WK-START-TIME.
           IF WK-START-HH > 11
              MOVE "PM" TO D-AM-PM
           ELSE
              MOVE "AM" TO D-AM-PM.
           IF WK-START-HH > 12
              SUBTRACT 12 FROM WK-START-HH.
           MOVE WK-START-HH TO D-HH.
           MOVE ":" TO D-COLON.
           MOVE WK-START-MM TO D-MM.
           IF LN-BNKRPTDATE = 0
              MOVE "S  A000SPMEMO." TO VDU
           ELSE
              MOVE "S  A000RESCHED." TO VDU.
           MOVE RESCHEDULE-BUF TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-DISPLAY-LOCAL-RESCH-INFO.
           EXIT.

      ******************************************************************
      *    DISPLAY GLOBAL RESCHEDULE INFO FROM QUEUE
      ******************************************************************
       DISPLAY-GLOBAL-RESCHEDULE-INFO SECTION.
           PERFORM OPEN-GQ1-FILE.

           MOVE LN-OWNBR  TO GQ-BRNO.
           MOVE LN-ACCTNO TO GQ-ACCTNO.
           PERFORM READ-GQ1-FILE.
           IF IO-FG = 9 OR GQ-SEQ-FG NOT = "R"
              GO TO EXIT-DISPLAY-GLOBAL-RESCH-INFO.

           MOVE GQ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-RESCH-DATE.
           MOVE GQ-START-TIME TO WK-START-TIME.
           IF WK-START-HH > 11
              MOVE "PM" TO D-AM-PM
           ELSE
              MOVE "AM" TO D-AM-PM.
           IF WK-START-HH > 12
              SUBTRACT 12 FROM WK-START-HH.
           MOVE WK-START-HH TO D-HH.
           MOVE ":" TO D-COLON.
           MOVE WK-START-MM TO D-MM.
           IF LN-BNKRPTDATE = 0
              MOVE "S  A000SPMEMO." TO VDU
           ELSE
              MOVE "S  A000RESCHED." TO VDU.
           MOVE RESCHEDULE-BUF TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-DISPLAY-GLOBAL-RESCH-INFO.
           PERFORM CLOSE-GQ1-FILE.

      *******************************************
      *    SET NEW PAYOFF DATE WORKERS ROUTINE
      *******************************************
       SET-PAYOFF-DATE-WORKERS SECTION.

           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
           MOVE "PO"          TO CRNOR2-LPTRCD.
           MOVE "N"           TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

      ********************************************************
      *    CREATE MEMO RECORD
      ********************************************************
       CREATE-MEMO-PLACEHOLDER SECTION.

           MOVE SPACES                     TO MEMO-PLACEHOLDER-BUF.
           MOVE BR-TIME-ZONE-FACTOR        TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE TIME-EDIT                  TO MEMO-PLACEHOLDER-TIME.
           MOVE "ACCOUNT IS CURRENTLY BEING WORKED BY "
                                           TO MEMO-PLACEHOLDER.
           MOVE USER-COLLID                TO MEMO-PLACEHOLDER-COLLID.

           PERFORM OPEN-LM1-FILE.

           MOVE MEMO-PLACEHOLDER-BUF TO LM-MEMO.
           MOVE "C" TO LM-MEMO-TYPE.
           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT EXT-WI-CL-TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 1 TO LM-SEQ.
           MOVE 0 TO SAVE-LM-COL-SEQ.
       CREATE-PLACEHOLDER-LOOP.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO CREATE-PLACEHOLDER-LOOP
              ELSE
                 MOVE "MAXIMUM LINES FOR DAY ALREADY CREATED"
                                               TO MESS
                 PERFORM SEND-MESS.
           MOVE LM-SEQ TO SAVE-LM-COL-SEQ.
           PERFORM CLOSE-LM1-FILE.

      *----------------------------------------------------------------
      * CALL GLOBAL LOAN SCAN TO GET ACCOUNT NUMBER.
      *----------------------------------------------------------------
       GET-ACCOUNT SECTION.
           MOVE " " TO GET-ACCOUNT-ERR.

           IF GET-ACCOUNT-FOR-GQ = "Y"
              MOVE " " TO GET-ACCOUNT-FOR-GQ
           ELSE
              IF BR-INBR-BILNSC-FG = "M"
                 PERFORM LSCAN-LOCAL-SCAN
                 IF LSCAN-ERR = "Y"
                    GO TO EXIT-GET-ACCOUNT-ERR
                 END-IF
              ELSE
                 MOVE GLOBAL-SCAN TO PASSWDW-SEQ
                 MOVE "TO USE GLOBAL LOAN SCAN" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
      *             MOVE SPACES TO DATA-PATH-BUF
                    GO TO EXIT-GET-ACCOUNT-ERR
                 ELSE
                    MOVE 0                   TO BILNSC-SSNO
                    MOVE "T"                 TO BILNSC-ERROR
                    MOVE EXT-CONAME-SYSDATE  TO BILNSC-SYSDATE
                    MOVE EXT-CONAME-USER-LOC TO BILNSC-BRANCH
                    MOVE "SPINQ"             TO BILNSC-CALLED-FROM
                    MOVE "WI/BILNSC" TO FORM-NAM
                    CALL FORM-PROGX USING FORM-PATH BILNSC-BUF
                                          EXIT-PATHNAME
                    CANCEL FORM-PROGX
                    IF BILNSC-ERROR = "Y"
                       GO TO EXIT-GET-ACCOUNT-ERR.

           INSPECT BILNSC-ACCTNO REPLACING ALL SPACES BY ZEROS.
           INSPECT BILNSC-BRANCH REPLACING ALL SPACES BY ZEROS.

      *-----------------------------------------------------------------
      *    IF YOU PICKED AN ACCT IN YOUR OWN BRANCH, DON'T BOTHER
      *    SETTING UP NEW EXT-FILPATH, JUST GET OUT.
      *-----------------------------------------------------------------
           IF BILNSC-BRANCH = EXT-FILPATH-BRANCH
              MOVE BILNSC-ACCTNO TO LN-ACCTNO
              GO TO EXIT-GET-ACCOUNT.

      *-----------------------------------------------------------------
      *    SET UP OWNING BRANCHES DATA PATH
      *-----------------------------------------------------------------
           MOVE BILNSC-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM IO-ERROR
              MOVE "BRANCH &&&& DOES NOT EXIST " TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BILNSC-BRANCH
              PERFORM SEND-MESS
              GO TO EXIT-GET-ACCOUNT-ERR.

           MOVE BR-MACHINE         TO EXT-FILPATH-MACHINE
                                            SOURCE-MACHINE.
           MOVE BR-DATA-PATH       TO EXT-FILPATH-DATA.

           SET ENVIRONMENT "FIL" TO EXT-FILPATH-FIL.

           MOVE "  " TO VDU-WIMENU-SELECTION.

      * SETUP NEW BR-REC HERE, AFTER WE HAVE CHANGED PATH WITHOUT ERR
      
           MOVE BILNSC-ACCTNO TO LN-ACCTNO.

           GO TO EXIT-GET-ACCOUNT.

       EXIT-GET-ACCOUNT-ERR.
           MOVE "Y" TO GET-ACCOUNT-ERR.
     
      * RESTORE BR-REC HERE IF WE ERROR OUT.
      
       EXIT-GET-ACCOUNT.
           EXIT.

      **********************************************************
      *  RESET DATA PATH TO POSTING BRANCH
      **********************************************************
       RESET-PATH SECTION.
           MOVE HOLD-EXT-FILPATH   TO EXT-FILPATH.
           SET ENVIRONMENT "FIL" TO EXT-FILPATH-FIL.
           MOVE EXT-FILPATH-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              PERFORM IO-ERROR
              MOVE "BRANCH &&&& DOES NOT EXIST " TO MESS
              INSPECT MESS REPLACING FIRST "&&&&"
                                   BY EXT-CONAME-USER-LOC
              PERFORM SEND-MESS
           END-IF.
      * DO NOT USE FORM-PROGX AS CALLING PROGRAMS WIPES IT OUT!
           IF ORIG-FORM-PROGX = "XPINQ"
              MOVE " " TO VDU-WIMENU-SELECTION
           ELSE
              MOVE "C0" TO VDU-WIMENU-SELECTION.
       EXIT-RESET-PATH.
           EXIT.

      ******************************************************************
      * SKIP ACCOUNT IF BAD BANKRUPT PASSWORD (REGIONAL ACC)
      ******************************************************************
       LOCAL-SKIP-RESET-ACCOUNT SECTION.

           MOVE LN-OWNBR  TO VCQ-BRNO.
           MOVE LN-ACCTNO TO VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.
           IF IO-FG = 9
              GO TO END-LOC-SKIP-RESET-ACCOUNT.

           MOVE VCQ-CQ-REC TO CQ-REC.

           MOVE SPACES TO CQ-IN-PROCESS CQ-WORK-COLLID.
           PERFORM REWRITE-CQ1-VCQ1-FILE.

           IF SELECTED-COLLID NOT = CQ-COLLID
              GO TO END-LOC-SKIP-RESET-ACCOUNT.

           PERFORM SET-CQ2-TO-KEEP-CQ2.

       RESET-LOC-NEXT-ACCT-IN-QUEUE.
           PERFORM OPEN-CQS1-FILE.
           MOVE SELECTED-COLLID TO CQS-COLLID
           PERFORM READ-CQS1-FILE.
           IF IO-FG = 9
              GO TO END-LOC-SKIP-RESET-ACCOUNT.

           IF KEEP-CQ-SEQ-FG NOT = "R"
              MOVE KEEP-CQ2-KEY TO CQS-CQ2-KEY-FIELDS.

           PERFORM REWRITE-CQS1-FILE.

       END-LOC-SKIP-RESET-ACCOUNT.
           PERFORM CLOSE-CQS1-FILE.

      ******************************************************************
      * SKIP ACCOUNT IF BAD BANKRUPT PASSWORD (REGIONAL ACC)
      ******************************************************************
       GLOBAL-SKIP-RESET-ACCOUNT SECTION.
           MOVE LN-OWNBR  TO GQ-BRNO.
           MOVE LN-ACCTNO TO GQ-ACCTNO.
           PERFORM OPEN-GQ1-FILE.
           PERFORM READ-GQ1-FILE.
           IF IO-FG = 9
              GO TO END-GLB-SKIP-RESET-ACCOUNT.

           MOVE SPACES TO GQ-IN-PROCESS GQ-WORK-COLLID.
           PERFORM REWRITE-GQ1-FILE.

           IF SELECTED-COLLID NOT = GQ-COLLID
              GO TO END-GLB-SKIP-RESET-ACCOUNT.

           PERFORM SET-GQ2-TO-KEEP-GQ2.
       RESET-GLB-NEXT-ACCT-IN-QUEUE.
           PERFORM OPEN-GQS1-FILE.
           MOVE SELECTED-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           IF IO-FG = 9
              GO TO END-GLB-SKIP-RESET-ACCOUNT.

           IF KEEP-GQ-SEQ-FG NOT = "R"
              MOVE KEEP-GQ2-KEY TO GQS-GQ2-KEY-FIELDS
              ADD 1 TO GQS-GQ-REV-BAL.

           PERFORM REWRITE-GQS1-FILE.

       END-GLB-SKIP-RESET-ACCOUNT.
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-GQS1-FILE.

      *******************************************************
      * VERIFY PASSWORD SECTION
      *   --> ENTER PASSWORD IF USING GLOBAL SCAN
      *******************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "SPINQ" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************************
       VERIFY-PASSWORD-WORLD SECTION.
      *******************************************************
           MOVE " " TO  PASSWDW-STAT.
           MOVE "SPINQ" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD TO ACCESS" TO
                                             PASSWDW-PROMPT1.
           MOVE "BANKRUPT ACCOUNT"     TO    PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
      * LOCAL SCAN SECTION.
      *  ALTERNATIVE TO GLOBAL LOAN SCAN. ALLOWS USER TO ENTER A SINGLE
      *  BRANCH AND THEN BR SENT INTO PAYMENT PROCESSING.
      ******************************************************************
       LSCAN-LOCAL-SCAN SECTION.
           MOVE 7 TO WINDOW-LINES.
           MOVE 60 TO WINDOW-COLUMNS.
           MOVE 16 TO WINDOW-BEGIN-Y.
           MOVE 10 TO WINDOW-BEGIN-X.
           MOVE " " TO WINDOW-FIT-TO.
           MOVE "WI/INBRT2" TO WINDOW-FORM-NAM.
           MOVE "INBRT2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY INBRT2-SCREEN.

           MOVE INBRT2-FORM-FIELDS TO WR-BUF.
           MOVE INBRT2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACE TO LSCAN-ERR.

      *    -------------------------------------------------------------
      *    ENTER OWNING BRANCH OF ACCOUNT.
      *    -------------------------------------------------------------
       LSCAN-ENTER-BRANCH.
           MOVE " ENTER OWNING BRANCH OF ACCOUNT:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.
           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY  GO TO LSCAN-CLOSE-ERR.
           IF NOT ( ENTER-KEY OR DOWN-KEY ) GO TO LSCAN-ENTER-BRANCH.

           MOVE VDUNUM0 TO BILNSC-BRANCH BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "BRANCH RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO LSCAN-ENTER-BRANCH.

           MOVE "S  A040NAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           IF BR-MACHINE NOT = SOURCE-MACHINE
              MOVE 0 TO PASSWDW-SEQ
              MOVE "TO POST ACROSS MACHINES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "POSTING TO ANOTHER MACHINE IS NOT ALLOWED"
                     TO MESS
                 PERFORM SEND-MESS
                 GO TO LSCAN-ENTER-BRANCH.

       LSCAN-ENTER-ACCTNO.
           MOVE " ENTER ACCOUNT NUMBER:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.
           MOVE "ENNN060ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF NOT ( ENTER-KEY OR DOWN-KEY ) GO TO LSCAN-ENTER-ACCTNO.
           IF F1-KEY GO TO LSCAN-ENTER-BRANCH.
           IF UP-KEY GO TO LSCAN-ENTER-BRANCH.
           MOVE VDUNUM0 TO BILNSC-ACCTNO.

      *    -------------------------------------------------------------
      *    ACCEPY BRANCH (Y/N)
      *    -------------------------------------------------------------
           MOVE " ACCEPT ACCOUNT (Y/N):" TO MESS.
           PERFORM SEND-WSEL-LEGEND.
           MOVE "ENNA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO LSCAN-ENTER-BRANCH.
           IF UP-KEY GO TO LSCAN-ENTER-ACCTNO.
           IF F7-KEY GO TO LSCAN-CLOSE-ERR.
           IF NOT (ENTER-KEY OR DOWN-KEY ) GO TO LSCAN-ENTER-BRANCH.
           IF VDUBUF NOT = "Y"
              GO TO LSCAN-ENTER-BRANCH.
           GO TO LSCAN-LOCAL-SCAN-EXIT.
       LSCAN-CLOSE-ERR.
           MOVE "Y" TO LSCAN-ERR.
       LSCAN-LOCAL-SCAN-EXIT.
      *    -------------------------------------------------------------
      *    RESTORE BRANCH INFO TO WHERE IT WAS BEFORE CALLING WINDOW
      *    CALLING ROUTINE ONLY NEEDS BILNSC INFO
      *    -------------------------------------------------------------
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INBRT2-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           MOVE "SPINQ" TO VDU-FORM-NAME.
       LSCAN-LOCAL-SCAN-REAL-EXIT.
           EXIT.


      ******************************************************************
      * GET REMOTE TIME SECTION:
      * DETERMINE THE LOCAL TIME FOR THE PERSON WE ARE TRYING TO CALL.
      ******************************************************************
       GET-REMOTE-TIME SECTION.
           MOVE SPACES TO REMOTE-TIME REMOTE-AM-PM.
           IF GP-TIME-ZONE-Q NOT = "Y"
              MOVE "N" TO TZ-FOUND
              GO TO GET-REMOTE-TIME-EXIT.

      *    -------------------------------------------------------------
      *    DETERMINE WHETHER THE COUNTRY IN OBSERVING DLS
      *    -------------------------------------------------------------
           MOVE TRANS-DATE TO DLS-DATE.
           PERFORM GET-DLS.

           MOVE "Y" TO TZ-FOUND.
           MOVE REMOTE-PHONE TO HOLD-PHONE.
           MOVE HOLD-AC TO TZ-CODE.
           MOVE HOLD-PF TO TZ-PREFIX.
           PERFORM GET-TZ.
           IF IO-BAD
               MOVE "N" TO TZ-FOUND
               GO TO GET-REMOTE-TIME-EXIT.

      *    -------------------------------------------------------------
      *    COMPUTE DIFFERENCE IN TIME BETWEEN MY MACHINE AND WHO
      *           I AM CALLING
      *    -------------------------------------------------------------
           SUBTRACT GP-TIME-ZONE-FACTOR FROM TZ-FACTOR
                              GIVING HOLD-FACTOR.

      *    -------------------------------------------------------------
      *    ADJUST TIME FOR THE FOLKS WHO JUST DONT GIVE A $&!# ABOUT DLS
      *    ( HAWAII, MOST OF ARIZONA AND EAST INDIANA DO NOT FOLLOW DLS)
      *    IF THE REST OF THE COUNTRY IS OBSERVING DLS
      *       IF (I AM OBSERVING DLS) AND
      *                 (I AM CALLING SOMEONE WHO IS NOT OBSERVING DLS)
      *       THEN  SUBTRACT 1 FROM MY TIME ZONE FACTOR.
      *       IF (I AM NOT OBSERVING DLS) AND
      *                 (I AM CALLING SOMEONE WHO IS OBSERVING DLS)
      *       THEN  ADD 1 TO MY TIME ZONE FACTOR.
      *    -------------------------------------------------------------
           IF DLS-FG = "Y"
              IF GP-TIME-ZONE-DLS = "Y" AND TZ-DLS NOT = "Y"
                 SUBTRACT +1 FROM HOLD-FACTOR
              ELSE
              IF GP-TIME-ZONE-DLS NOT = "Y" AND TZ-DLS = "Y"
                 ADD +1 TO HOLD-FACTOR.

      *    -------------------------------------------------------------
      *    GET TIME FROM OS AND CALCULATE CORRECT TIME GIVING THE FACTOR
      *    -------------------------------------------------------------
           ACCEPT TIME-NOW FROM TIME.
           ADD HOLD-FACTOR TO CUR-HH.
           IF CUR-HH > 23
              SUBTRACT 24 FROM CUR-HH.

      *    -------------------------------------------------------------
      *    TEST CALLING HOURS - ONLY TEST ONCE PER ACCOUNT
      *    -------------------------------------------------------------
           IF TEST-CALL-TIME-FG = "Y"
              MOVE "N" TO TEST-CALL-TIME-FG
              IF (GP-CALL-START-TIME NUMERIC) AND
                 (GP-CALL-END-TIME NUMERIC)
                 IF H-AND-M < GP-CALL-START-TIME OR
                    H-AND-M > GP-CALL-END-TIME
                    MOVE "F" TO TEST-CALL-TIME-FG.
      *    -------------------------------------------------------------
      *    CONVERT 24 HOUR CLOCK TIME TO 12 HOUR CLOCK TIME
      *    -------------------------------------------------------------
           IF CUR-HH = 00
              MOVE 12 TO CUR-HH.

           IF CUR-HH > 11
              MOVE WS-PM TO REMOTE-AM-PM
           ELSE
              MOVE WS-AM TO REMOTE-AM-PM.

           IF CUR-HH > 12
              SUBTRACT 12 FROM CUR-HH.

           IF H-AND-M = 0
              MOVE SPACES TO REMOTE-TIME REMOTE-AM-PM
              MOVE "N" TO TZ-FOUND
           ELSE
              MOVE H-AND-M TO REMOTE-TIME-X
              INSPECT REMOTE-TIME-X REPLACING ALL "/" BY ":".

       GET-REMOTE-TIME-EXIT.
           EXIT.

      ******************************************************************
      * GET TIME ZONE RECORD.
      * IF RECORD IS NOT FOUND FOR AREA CODE / PREFIX COMBINATION
      *    THEN SEARCH ON THE AREA CODE ONLY.
      ******************************************************************
       GET-TZ SECTION.
           PERFORM OPEN-TZ1-FILE.
           PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO TZ-PREFIX
              PERFORM READ-TZ1-FILE.
           PERFORM CLOSE-TZ1-FILE.
       GET-TZ-EXIT.
           EXIT.

      **************************************************************************
      *                        SRCD = PERIOD TABLE INDEX
      *                           1 - NB            7 - RN
      *                           2 - SEE BELOW     8 - SC OR DM
      *                           3 - FB            9 - OT
      *                           4 - PB           10 - RB
      *                           5 - PBD          11 - RO
      *                           6 - PBI
      *
      **************************************************************************
      * BAH WORLD PR#1229 HARD CODED THIS SO P&L ACCOUNTS WILL SHOW PBI,PBD
      **************************************************************************

       GET-SRCD-SUB SECTION.

           MOVE 0 TO SRCD.
      *    IF LN-PLCD = " "
              MOVE "N" TO SRCD-FG.
              PERFORM GET-SRCD-SUB-NO-PL.
      *       GO TO EXIT-GET-SRCD.

       EXIT-GET-SRCD.
           EXIT.

       GET-SRCD-SUB-NO-PL SECTION.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE 2 TO SRCD
                 GO TO GET-SRCD-SUB-EXIT
              ELSE
                 IF LN-SRCD = "RF"
                    MOVE 9 TO SRCD
                    GO TO GET-SRCD-SUB-EXIT.

           IF LN-SRCD = "NB"
              MOVE 1 TO SRCD
           ELSE
           IF LN-SRCD = "RF"
              MOVE 2 TO SRCD
           ELSE
           IF LN-SRCD = "FB"
              MOVE 3 TO SRCD
           ELSE
           IF LN-SRCD = "PB"
              PERFORM GET-SRCD-PB
           ELSE
           IF LN-SRCD = "RN"
              MOVE 7 TO SRCD
           ELSE
           IF LN-SRCD = "DM" AND GP-NO-SC-USE-DM = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "SC" AND GP-NO-SC-USE-DM NOT = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "RB"
              MOVE 10 TO SRCD
           ELSE
           IF LN-SRCD = "RO"
              MOVE 11 TO SRCD
           ELSE
              MOVE 9 TO SRCD.
       GET-SRCD-SUB-EXIT.
           EXIT.

      *-------------------------------------------------------
      *    GET SRCD INDEX FOR PB
      *    DETERMINE IF ACCOUNT IS 'PB' 'PBD' OR 'PBI'
      *
      * NOTE: ACCOUNTS WILL BE CATAGORIZED AS
      *       'PB DELINQ' IF BOTH CATAGORIES APPLY.
      * 9/25/97 - FOR WORLD: IF SP-PBINCR-PERCENT = 1, JUST
      *           CHECK IF LNAMT IS > PRIOR FOR SRCD OF 6
      *-------------------------------------------------------
       GET-SRCD-PB SECTION.
           MOVE 4 TO SRCD.
           IF SP-PBDEL-DAYS NOT = 0
              IF LN-PRPDUE-DAYS > SP-PBDEL-DAYS
      * PBD
                 MOVE 5 TO SRCD
                 GO TO GET-SRCD-PB-EXIT.
           IF SP-PBINCR-PERCENT NOT = 0
              IF SP-PBINCR-PERCENT = 1
                 IF LN-LNAMT > LN-PRLNAMT
      * PBI
                    MOVE 6 TO SRCD
                 END-IF
              ELSE
                 COMPUTE SRCD-WORKER ROUNDED =
                     LN-PRLNAMT - 0.01
                         + LN-PRLNAMT * SP-PBINCR-PERCENT / 100
                 IF LN-LNAMT > SRCD-WORKER
                    MOVE 6 TO SRCD.

       GET-SRCD-PB-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           IF VDU-FORM-NAME = "NXCNT2"
              MOVE NXCNT2-HELP--FILE TO HELP-LINK-FILE
              MOVE NXCNT2-HELP--DIR  TO HELP-LINK-DIR
           ELSE
           IF VDU-FORM-NAME = "NXCNTC"
              MOVE NXCNTC-HELP--FILE TO HELP-LINK-FILE
              MOVE NXCNTC-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE SPINQ-HELP--FILE TO HELP-LINK-FILE
              MOVE SPINQ-HELP--DIR TO HELP-LINK-DIR.
      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPINQ-SCREEN UPON SPINQ-WINDOW-HANDLE.
           MOVE EXT-CONAME-SYSDATE TO SPINQ-TRDATE.
           MOVE SPINQ-FORM-FIELDS TO WR-BUF.
           MOVE SPINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF BR-COLLECTOR-QUEUE = "Y"
              MOVE "S  A190COLQUEUE." TO VDU
              MOVE SELECTED-COLLID TO D-COLLECTOR
              IF ROUTE-CD = "W"
                 MOVE "REVIEW:" TO COLQUEUE-PROMPT
              ELSE
                 MOVE "QUEUE: " TO COLQUEUE-PROMPT
              END-IF
              MOVE COLQUEUE-BUF TO VDUBUF
              PERFORM SCREENIO.
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      * ROUTINES FROM SPINQ1:
      ******************************************************************
      ******************************************************************
      *    SPINQ1 SET UP
      ******************************************************************
       SPINQ1-SETUP SECTION.

      * ALLOW CTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

      * ALLOW CTRL 'R' ESCROW DISPLAY
      *    IF (LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I")
      *       MOVE "WI/ESINFO"   TO VDU-CTRL-R-PROG.

      * ALLOW CNTRL 'B' INTERRUPT:
           MOVE "WI/RVLINE"   TO VDU-CTRL-B-PROG.
           MOVE LN-OWNBR      TO RVLINE-BRNO.
           MOVE LN-ACCTNO     TO RVLINE-ACCTNO.
           MOVE LN-UNITPER-CD TO RVLINE-UNITPER-CD.
           MOVE RVLINE-BUF    TO VDU-CTRL-B-BUF.

           MOVE 0 TO ERRCD.

           MOVE "C" TO VDU-CLMENU-SELECTION.
      * DO NOT USE FORM-PROGX AS CALLING PROGRAMS WIPES IT OUT!
           IF ORIG-FORM-PROGX = "XPINQ"
              MOVE " " TO VDU-WIMENU-SELECTION
           ELSE
              IF VDU-WIMENU-SELECTION = "C0"
                 MOVE "C1" TO VDU-WIMENU-SELECTION.

      ******************************************************************
      *    SPINQ1 INITIALIZATION
      ******************************************************************
       SPINQ1-INITIALIZATION SECTION.
           MOVE SPACES TO IO-TYPE.

           MOVE EXT-WI-CL-TRANS-DATE TO ORIG-TRANS-DATE.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.

           MOVE EXT-WI-CL-MAKERCD(EXT-WI-CL-SSNO-NO) TO MAKER-CD.
           MOVE " " TO ORIG-LN-ATCODE.
           MOVE " " TO ORIG-LN-RECODE.
           MOVE LN-PROMDATE TO ORIG-LN-PROMDATE.
           MOVE LN-PROMAMT TO ORIG-LN-PROMAMT.
           MOVE LN-PROMDATE2 TO ORIG-LN-PROMDATE2.
           PERFORM GET-ORIG-SEND-FORM.
           MOVE "N" TO MEMO-ENTERED-FG.

           MOVE LN-MESSAGE TO ORIG-LN-MESSAGE.
           MOVE LN-COLLECTOR TO ORIG-LN-COLLECTOR.
           MOVE LN-ACTIONCD TO ORIG-LN-ACTIONCD.

           MOVE 0 TO HOLD-SSNO.

           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           PERFORM NEW-RECORD-SETUP.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE ORIG-FIELDS TO NEW-FIELDS.

           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           MOVE "N" TO MAX-FLAG.
           IF ELE > MAX
              MOVE "Y" TO MAX-FLAG
              MOVE MAX TO ELE.
           GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.

      * I KNOW THIS IS UGLY, BUT I WANTED TO AVOID THE PASSWORD FOR PROMISE
      * STUFF IF YOU ALREADY KNOW THEY DONT KNOW THE PASSWORD !!
      * LET THE UP-ARROW TAKE THEM ALL THE WAY BACK TO THE RESULT CODE
           IF VDUSTATUS = "1U"
              IF ELE = 3 OR 4
                 IF VALID-PROMISE-PASSWD = "N"
                    MOVE 2 TO ELE.
      * NEED TO TEST SECOND PROMISE DATE SEPARATE, BECAUSE IT MAY NOT HAVE
      * NEEDED A PASSWORD ??? MAYBE THAT DATE WAS ZERO.
           IF VDUSTATUS = "1U"
              IF ELE = 5
                 IF VALID-PROM2-PASSWD = "N"
                    MOVE 4 TO ELE.

           MOVE " " TO LEGEND.
           IF ELE = 1
              IF BWLNS3-SCANYN = "N"
                 MOVE LEGEND-01 TO LEGEND
              ELSE
                 MOVE LEGEND-01-NO-F3 TO LEGEND
              END-IF
              IF BR-COLLECTOR-QUEUE = "Y"
                 IF QUEUED-FG = "Y"
                    IF BWLNS3-SCANYN = "N"
                       MOVE LEGEND-01-SKIP TO LEGEND
                    ELSE
                       MOVE LEGEND-01-SKIP-NO-F3 TO LEGEND.

           IF ELE = 2
              MOVE LEGEND-02 TO LEGEND
           ELSE
           IF ELE = 3
              MOVE LEGEND-03 TO LEGEND
           ELSE
           IF ELE = 4
              MOVE LEGEND-04 TO LEGEND
           ELSE
           IF ELE = 5
              MOVE LEGEND-05 TO LEGEND
           ELSE
           IF ELE = 6
              MOVE LEGEND-06 TO LEGEND
           ELSE
           IF ELE = 7
              MOVE LEGEND-07 TO LEGEND
           ELSE
           IF ELE = 8
              MOVE LEGEND-08 TO LEGEND
           ELSE
           IF ELE = 9
              MOVE LEGEND-09 TO LEGEND.
           PERFORM SEND-LEGEND.

      * DO NOT ALLOW PROMISE INFO IF ACCOUNT IS PAIDOUT, PL# 673
           IF ELE = 3 OR 4 OR 5
              IF LN-POFFDATE NOT = 0
                 MOVE "N" TO CHFG(3) CHFG(4) CHFG(5)
                 GO TO CREATE-ROUTINE.

           IF ELE = 3
              MOVE "Y" TO VALID-PROMISE-PASSWD
              IF LN-PROMDATE NOT = 0
                 MOVE EXT-CONAME-USER-GROUP TO CASE-BUF
                 CALL "C$TOUPPER" USING CASE-BUF, VALUE 3
                 IF TRANS-DATE NOT > LN-PROMDATE
                    MOVE PRIOR-PROM-SEQ TO PASSWDW-SEQ
                    MOVE "FOR PRIOR TO PROMISE DATE" TO
                                                   PASSWDW-PROMPT2
                    PERFORM VERIFY-SPINQ1-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       MOVE "N" TO VALID-PROMISE-PASSWD
                       GO TO CREATE-ROUTINE.

      * PROMISE AMOUNT
      * DONT STOP HERE IF THEY CANT CHANGE TO PROMISE DATE
           IF ELE = 4
              IF VALID-PROMISE-PASSWD = "N"
                 GO TO CREATE-ROUTINE.

           IF ELE = 5
              MOVE "Y" TO VALID-PROM2-PASSWD
              IF LN-PROMDATE2 NOT = 0
                 MOVE EXT-CONAME-USER-GROUP TO CASE-BUF
                 CALL "C$TOUPPER" USING CASE-BUF, VALUE 3
                 IF TRANS-DATE NOT > LN-PROMDATE2
                    MOVE PRIOR-PROM-SEQ TO PASSWDW-SEQ
                    MOVE "FOR PRIOR TO 2ND PROMISE DATE"
                                        TO PASSWDW-PROMPT2
                    PERFORM VERIFY-SPINQ1-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       MOVE "N" TO VALID-PROM2-PASSWD
                       GO TO CREATE-ROUTINE.

      * FORM REQUEST
      * DONT STOP HERE IF ACCOUNT OR BORROWER IS CONFIDENTIAL
           IF ELE = 6
              IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
                 GO TO CREATE-ROUTINE.

           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-UP.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F2.
           IF VDUSTATUS = "12"
              PERFORM MEMO-MODULE
              MOVE STAR-LINE TO WR-BUF
              MOVE MEMO-ENTRY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO ENTER-ELE.
       TEST-F3.
           IF VDUSTATUS = "16"
              MOVE "Y" TO BWLNS3-ALL-LOANS
              PERFORM DISPLAY-ALL-LOANS
              GO TO ENTER-ELE.
       TEST-F4.
           IF VDUSTATUS = "18"
              PERFORM SEND-PAYMENT-LINES
              GO TO ENTER-ELE.
       TEST-F5.
           IF VDUSTATUS = "1:"
              PERFORM SEND-PAYOFF
              GO TO ENTER-ELE.
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF (ELE = 1 OR 2 OR 7 OR 8)
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE
              ELSE
              IF ELE = 6
                 PERFORM FORM-SCAN
                 IF IO-FG = 0
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
       TEST-F7.
           IF VDUSTATUS = "1>"
              PERFORM VERIFY-UPD-ROUTINE
              IF IO-TYPE = "ERR"
                 GO TO ENTER-ELE
              END-IF
              PERFORM SET-NEXT-CONTACT
              IF IO-TYPE = "CLR"
                 GO TO ENTER-ELE
              ELSE
                 PERFORM UPDATE-LOAN-CHANGES
                 IF IO-TYPE = "CON"
                    PERFORM CREATE-MEMO-PLACEHOLDER
                    GO TO END-ENTRY-MODULE
                 ELSE
                    MOVE "END" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.
       TEST-F1.
           IF VDUSTATUS = "10"
              IF ELE = 1
                 IF QUEUED-FG = "Y"
                    PERFORM UPDATE-SKIP-TO-QUEUE
                    MOVE "END" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F11.
           IF VDUSTATUS = "1!"
              MOVE LN-OWNBR  TO COLAST-LN-OWNBR
              MOVE LN-ACCTNO TO COLAST-LN-ACCTNO
              MOVE "WI/COLAST" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH COLAST-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE SPACES TO CD1-KEY NEW-LN-ATCODE.
           MOVE VDUBUF TO CD-CODE.
           PERFORM VERIFY-ACTIVITY-CODE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE CD-CODE TO NEW-LN-ATCODE.
      *    MOVE "S  A260011." TO VDU.
      *    MOVE CD-DESC TO VDUBUF.
      *    PERFORM SCREENIO.
           MOVE CD-AT-MEMO-REQUIRED TO ACTIVITY-MEMO-REQUIRED.
           MOVE CD-AT-RESULT-REQUIRED TO ACTIVITY-RESULT-REQUIRED.
           IF CD-CODE = "MO"
              MOVE " " TO WR-BUF NEW-LN-RECODE
              MOVE "020." TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "N" TO CHFG(2) CHFG(3) CHFG(4) CHFG(5) CHFG(6)
              GO TO CREATE-ROUTINE
           ELSE
              MOVE " " TO CHFG(2) CHFG(3) CHFG(4) CHFG(5) CHFG(6).
      *    IF ACTIVITY-RESULT-REQUIRED = "N"
      *       MOVE " " TO WR-BUF NEW-LN-RECODE
      *       MOVE "020." TO WR-LIST
      *       PERFORM CALL-WRFORM
      *       MOVE "N" TO CHFG(2)
      *    ELSE
      *       MOVE " " TO CHFG(2).
           PERFORM TEST-ACTIONCD.
           MOVE "Y" TO ACTIVITY-CODE-ENTERED.
           GO TO CREATE-ROUTINE.
       F-02.
           IF ACTIVITY-RESULT-REQUIRED = "Y"
              IF VDUBUF = " "
                 MOVE "A RESULT CODE IS REQUIRED!!!!!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE SPACES TO CD1-KEY NEW-LN-RECODE.
           MOVE VDUBUF TO CD-CODE.
           PERFORM VERIFY-RESULT-CODE

      *    IF IO-FG = 9
      *       IF ACTIVITY-RESULT-REQUIRED = "Y"
      *          GO TO ENTER-ELE
      *       ELSE
      *          MOVE SPACES TO CD-DESC CD-CODE
      *          MOVE "N" TO CD-RE-MEMO-REQUIRED
      *                      CD-RE-RESCHEDULE-REQ.
           IF IO-FG = 9
              IF ACTIVITY-RESULT-REQUIRED = "Y"
                 MOVE "VALID RESULT CODE REQUIRED" TO MESS
              ELSE
                 MOVE "PLEASE ENTER VALID RESULT CODE OR SPACES" TO MESS
              END-IF
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF CD-CODE = SPACES
              MOVE "N" TO CD-RE-MEMO-REQUIRED
                          CD-RE-RESCHEDULE-REQ
                          SAVE-RESULT-WORKED-FG
                          CD-RE-ACTIVITY-REQUIRED
           ELSE
              MOVE CD-RE-WORKED-ACCOUNT TO SAVE-RESULT-WORKED-FG.
           MOVE CD-CODE TO NEW-LN-RECODE.
      *    MOVE "S  A260021." TO VDU.
      *    MOVE CD-DESC TO VDUBUF.
      *    PERFORM SCREENIO.
           MOVE CD-RE-MEMO-REQUIRED TO RESULT-MEMO-REQUIRED.
           MOVE CD-RE-RESCHEDULE-REQ TO RESULT-RESCHED-REQ.
           MOVE CD-RE-ACTIVITY-REQUIRED TO RESULT-ACTIVITY-REQUIRED.
           PERFORM TEST-ACTIONCD.
           IF BR-COLLECTOR-QUEUE = "Y"
              IF CD-RE-RESCHEDULE-REQ = "A"
                 IF CD-RE-RESCHEDULE-TIME NOT = 0
                    PERFORM AUTO-RESCHEDULE
                    PERFORM UPDATE-LOAN-CHANGES
                    MOVE "END" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.

           IF CD-RE-ACTIVITY-REQUIRED = "Y"
              IF NEW-LN-ATCODE = " "
                 MOVE "VALID ACTIVITY CODE REQUIRED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.


           GO TO CREATE-ROUTINE.

       F-03.
           IF VDUNUM0 = 0
              MOVE 0 TO NEW-LN-PROMDATE
                        NEW-LN-PROMAMT
                        NEW-LN-PROMDATE2
              MOVE "N" TO CHFG(4) CHFG(5)
              MOVE " " TO WR-BUF
              MOVE PROM-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO CREATE-ROUTINE.

           MOVE VDU-DATE-YYYYMMDD TO SYS-DATE.
           IF SYS-DATE < EXT-WI-CL-TRANS-DATE
                 AND (SYS-DATE NOT = LN-PROMDATE)
              GO TO ENTER-ELE.

           MOVE VDU-DATE-YYYYMMDD TO NEW-LN-PROMDATE.
           MOVE TRANS-DATE TO NUM-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS > 30
              MOVE PROM30-SEQ1 TO PASSWDW-SEQ
              MOVE "FOR > 30 DAYS PROMISE DATE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "CANNOT BE MORE THAN 30 DAYS FROM TODAYS DATE"
                       TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF GP-MAX-PROMISE-DAYS NUMERIC
              IF GP-MAX-PROMISE-DAYS NOT = ZERO
                 IF ELAPSED-DAYS > GP-MAX-PROMISE-DAYS
                    MOVE PROM-GPMAX-SEQ TO PASSWDW-SEQ
                    MOVE SPACES TO MESS
                 STRING
                   "FOR PROMISE > THAN " GP-MAX-PROMISE-DAYS " DAYS"
                    DELIMITED BY "  " INTO  PASSWDW-PROMPT2
                    PERFORM VERIFY-SPINQ1-PASSWORD-MUST-EXIST
                    IF PASSWDW-STAT NOT = "OK"
                       MOVE SPACES TO MESS
                       STRING
                 "CANNOT BE MORE THAN " GP-MAX-PROMISE-DAYS
                                             "   DAYS FROM TODAY"
                                        DELIMITED BY "    " INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           MOVE " " TO CHFG(4) CHFG(5).

           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUNUM2 TO NEW-LN-PROMAMT.
           GO TO CREATE-ROUTINE.
       F-05.
           IF VDUNUM0 = 0
              MOVE "S  A080050." TO VDU
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
      
           MOVE VDU-DATE-YYYYMMDD TO SYS-DATE.
           IF SYS-DATE < EXT-WI-CL-TRANS-DATE
              AND (SYS-DATE NOT = LN-PROMDATE2)
              GO TO ENTER-ELE. 
           
           MOVE VDU-DATE-YYYYMMDD TO NEW-LN-PROMDATE2.

           MOVE TRANS-DATE TO NUM-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS > 30
              MOVE PROM30-SEQ1 TO PASSWDW-SEQ
              MOVE "FOR > 30 DAYS PROMISE DATE2" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "CANNOT BE MORE THAN 30 DAYS FROM TODAYS DATE"
                      TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF GP-MAX-PROMISE-DAYS NUMERIC
              IF GP-MAX-PROMISE-DAYS NOT = ZERO
                 IF ELAPSED-DAYS > GP-MAX-PROMISE-DAYS
                    MOVE PROM-GPMAX-SEQ TO PASSWDW-SEQ
                    MOVE SPACES TO MESS
                 STRING
                   "FOR PROMISE 2 > THAN " GP-MAX-PROMISE-DAYS " DAYS"
                    DELIMITED BY "  " INTO  PASSWDW-PROMPT2
                    PERFORM VERIFY-SPINQ1-PASSWORD-MUST-EXIST
                    IF PASSWDW-STAT NOT = "OK"
                      MOVE SPACES TO MESS
                      STRING
                 "CANNOT BE MORE THAN " GP-MAX-PROMISE-DAYS
                                             "   DAYS FROM TODAY"
                                        DELIMITED BY "    " INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUNUM0 TO NEW-SEND-FORM.
           IF VDUNUM0 = 0
              MOVE "S  A030060." TO VDU
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           IF GP-DENY-FORM-NO-ADDRESS = "Y"
              IF BW-ADR1 = SPACES OR
                  BW-CITY = SPACES OR
                   BW-STATE = SPACES OR
                    BW-ZIP = SPACES
                 MOVE "SN N030060." TO VDU
                 MOVE ORIG-SEND-FORM TO NEW-SEND-FORM VDUNUM0
                 PERFORM SCREENIO
                 MOVE "FORM NOT ALLOWED - ADDRESS INCOMPLETE" TO MESS
                 PERFORM SEND-MESS
                 GO TO CREATE-ROUTINE.
           MOVE NEW-SEND-FORM TO FD-FMNO.
           PERFORM GET-FD.
           IF IO-FG = 9
              MOVE "INVALID FORM #; PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              MOVE "SN N030060." TO VDU
              MOVE ORIG-SEND-FORM TO NEW-SEND-FORM VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-ELE.

           IF FD-TYPE NOT = "L"
              GO TO ENTER-ELE.

           IF FD-SECURITY-PROFILE NOT = " "
              IF EXT-CONAME-USER-PROFILE-CD > FD-SECURITY-PROFILE
                 MOVE "YOU ARE NOT AUTHORIZED TO REQUEST THIS FORM."
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE "SN N030060." TO VDU
                 MOVE 0 TO NEW-SEND-FORM VDUNUM0
                 PERFORM SCREENIO
                 GO TO ENTER-ELE.

           IF FD-VALID-BRANCH NOT = SPACES
              IF FD-VALID-BRANCH NUMERIC
                 IF EXT-CONAME-USER-LOC NOT = FD-VALID-BRANCH
                    MOVE "NOT A VALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    MOVE "SN N030060." TO VDU
                    MOVE ORIG-SEND-FORM TO NEW-SEND-FORM VDUNUM0
                    PERFORM SCREENIO
                    GO TO ENTER-ELE
                 ELSE
                    NEXT SENTENCE
              ELSE
                 MOVE FD-VALID-BRANCH TO GR-ID
                 PERFORM OPEN-GR1-FILE
                 PERFORM SEARCH-GR-INIT
                 MOVE EXT-CONAME-USER-LOC TO WGR-SRESULT
                 PERFORM SEARCH-GR-VERIFY
                 IF WGR-ERR > 0
                    MOVE "NOT A VALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    MOVE "SN N030060." TO VDU
                    MOVE ORIG-SEND-FORM TO NEW-SEND-FORM VDUNUM0
                    PERFORM SCREENIO
                    GO TO ENTER-ELE.

           IF FD-FMTYPE NOT = "C"
              MOVE "THIS FORM IS NOT A COLLECTION LETTER !" TO MESS
              PERFORM SEND-MESS.

      * NEW FLAG TO CHECK IF STATE IS ALLOWED TO REQUEST THIS FORM
      * NEW FTFILE THAT HOLDS TABLE OF ALLOWED STATES
           PERFORM CHECK-STATE-ALLOWED.
           IF NOT ( STAT-GOOD )
              MOVE "STATE NOT ALLOWED TO REQUEST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      * GP FLAG TO USE THE DOC DESIGN MINIMUM DAYS SINCE LAST FORM SENT
           IF GP-USE-MIN-FORM-SENT-DAYS = "Y"
              IF FD-FMTYPE = "C" 
                 IF FD-MIN-FORM-SENT-DAYS NOT = 0 AND NOT = 99
                    IF LN-LASTCFORMDATE NOT = 0
                       MOVE LN-LASTCFORMDATE TO NUM-DATE
                       MOVE TRANS-DATE TO SYS-DATE
                       PERFORM TIM
                       IF ELAPSED-DAYS NOT > FD-MIN-FORM-SENT-DAYS
                          MOVE "MINIMUM DAYS NOT MET" TO MESS
                          PERFORM SEND-MESS
                          MOVE FD-MIN-DAYS-SEQ TO PASSWDW-SEQ
                          MOVE "FOR PRIOR TO MINIMUM DAYS" TO
                                                      PASSWDW-PROMPT2
                          PERFORM VERIFY-SPINQ1-PASSWORD
                          IF PASSWDW-STAT NOT = "OK"
                             GO TO ENTER-ELE.

      * REGACC PR# 256 10/2/2009 PREVENT LETTER REQUESTED IF PRIMARY
      *   MAKER HAS THE BAD ADDRESS FLAG SET
           IF FD-SKIP-BAD-ADR = "Y"
      * ONLY TEST BAD ADDRESS FLAG IF PRIMARY BORROWER
              IF BW-SSNO = LN-SSNO(1)
                 IF BW-BAD-ADR-FG = "Y"
                    MOVE "BAD ADDRESS, CAN NOT CREATE REQUEST" TO MESS
                    PERFORM SEND-MESS
                    MOVE "SN N030060." TO VDU
                    MOVE ORIG-SEND-FORM TO NEW-SEND-FORM VDUNUM0
                    PERFORM SCREENIO
                    GO TO ENTER-ELE.

           MOVE FD-FMTYPE TO HOLD-FMTYPE.
           MOVE FD-OPTION TO HOLD-FD-OPTION.
           MOVE FD-1STFILE TO HOLD-1STFILE.
           GO TO CREATE-ROUTINE.

       F-07.
           IF VDUBUF NOT = LN-ACTIONCD
              MOVE ACTIONCD-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE THE ACTIONCD" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.

           IF VDUBUF NOT = LN-ACTIONCD
              IF LN-ACTIONCD NOT = " "
                 MOVE "TO CHANGE ACTION CODE OF &&&&" TO PASSWDW-PROMPT2
                 MOVE LN-ACTIONCD TO TEST-LN-ACTIONCD
                 PERFORM VERIFY-ACTIONCD-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "S  A000070." TO VDU
                    MOVE LN-ACTIONCD TO VDUBUF
                    PERFORM SCREENIO
                    GO TO ENTER-ELE.

           IF LN-ACTIONCD = "NIVR" OR "CND " OR "LGL " OR "BKD "
              MOVE NIVR-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE THE EXISTING ACTIONCD" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.

           MOVE VDUBUF TO NEW-LN-ACTIONCD
                          CD-CODE.

           IF NEW-LN-ACTIONCD NOT = ORIG-LN-ACTIONCD
              IF NEW-LN-ACTIONCD NOT = " "
                 MOVE "TO USE ACTION CODE OF &&&&" TO PASSWDW-PROMPT2
                 MOVE NEW-LN-ACTIONCD TO TEST-LN-ACTIONCD
                 PERFORM VERIFY-ACTIONCD-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "S  A000070." TO VDU
                    MOVE ORIG-LN-ACTIONCD TO NEW-LN-ACTIONCD VDUBUF
                    PERFORM SCREENIO
                    GO TO ENTER-ELE.

           IF GP-ACTIONCD-VERIFY = "Y" OR " "
              IF NEW-LN-ACTIONCD NOT = " "
                 MOVE "AC" TO CD-TYPE
                 PERFORM GET-CD
                 IF IO-FG = 9
                    MOVE "INVALID ACTION CODE" TO MESS
                    PERFORM SEND-MESS
      * WAS UPDATING THE LOAN WITH AN INVALID CODE IF YOU PRESS F7-UPD
                    MOVE ORIG-LN-ACTIONCD TO NEW-LN-ACTIONCD
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-08.
           IF VDUBUF NOT = LN-COLLECTOR
              MOVE COLLECTOR-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE THE COLLECTOR ID" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.

           MOVE VDUBUF TO NEW-LN-COLLECTOR CDB-CODE.
      *  DON'T VERIFY COLLECTOR CODE UNLESS USING COLLECTOR QUEUE
           IF BR-COLLECTOR-QUEUE NOT = "Y"
              GO TO CREATE-ROUTINE.

           PERFORM VERIFY-COLLECTOR-CODE.
           IF ( CODE-NOT-FOUND )
              MOVE "S  A050080." TO VDU
              MOVE ORIG-LN-COLLECTOR TO NEW-LN-COLLECTOR VDUBUF
              PERFORM SCREENIO
              MOVE "INVALID COLLECTOR ID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-09.
           MOVE VDUBUF TO NEW-LN-MESSAGE.
           GO TO CREATE-ROUTINE.
      *F-10. -----> RESERVED AND ENTERED UNDER F-05 <-----
           GO TO CREATE-ROUTINE.

      *=================================================================
       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
      *
      *    V E R I F Y - C O L L E C T O R - C O D E
      *
      ******************************************************************
       VERIFY-COLLECTOR-CODE SECTION.

           MOVE "Y" TO CODE-FOUND.

           IF ( CDB-CODE = SPACES )
              MOVE SPACES TO CDB-DESC
           ELSE
              MOVE "CN" TO CDB-TYPE
              IF BR-GLOBAL-QUEUE = "Y"
                 MOVE 0 TO CDB-BRANCH
              ELSE
                 MOVE EXT-CONAME-USER-LOC TO CDB-BRANCH
              END-IF
              PERFORM GET-CDB
              IF ( IO-FG NOT = 0 )
                 MOVE "N" TO CODE-FOUND.

      ******************************************************************
      *    *     VERIFY ACTIVITY CODE
      ******************************************************************
       VERIFY-ACTIVITY-CODE SECTION.
           IF CD-CODE = " "
              MOVE 0 TO IO-FG
              MOVE " " TO CD-DESC
           ELSE
              MOVE "AT" TO CD-TYPE
              PERFORM GET-CD
              IF IO-GOOD
                 IF NOT (CD-AT-SUPERVISOR-ONLY = " " OR "Y" OR "N")
                   IF EXT-CONAME-USER-PROFILE-CD > CD-AT-SUPERVISOR-ONLY
                       MOVE SUPERVISOR-ACT-SEQ TO PASSWDW-SEQ
                       MOVE "TO POST THIS ACTIVITY CODE"
                                TO PASSWDW-PROMPT2
                       PERFORM VERIFY-SPINQ1-PASSWORD
                       IF PASSWDW-STAT NOT = "OK"
                          MOVE 9 TO IO-FG.

      *    ****************************
      *    *     VERIFY RESULT CODE
      *    ****************************
       VERIFY-RESULT-CODE SECTION.
           IF CD-CODE = " "
              MOVE 0 TO IO-FG
              MOVE " " TO CD-DESC
           ELSE
              MOVE "RE" TO CD-TYPE
              PERFORM GET-CD
              IF IO-GOOD
                 IF NOT (CD-RE-SUPERVISOR-ONLY = " " OR "Y" OR "N")
                   IF EXT-CONAME-USER-PROFILE-CD > CD-RE-SUPERVISOR-ONLY
                       MOVE SUPERVISOR-RES-SEQ TO PASSWDW-SEQ
                       MOVE "TO POST THIS RESULT CODE"
                                TO PASSWDW-PROMPT2
                       PERFORM VERIFY-SPINQ1-PASSWORD
                       IF PASSWDW-STAT NOT = "OK"
                          MOVE 9 TO IO-FG.

      *    ****************************
      *    *     VERIFY FOR UPDATE
      *    ****************************
       VERIFY-UPD-ROUTINE SECTION.
           MOVE "ERR" TO IO-TYPE.
           IF ACTIVITY-RESULT-REQUIRED = "Y"
              IF NEW-LN-RECODE = " "
                 MOVE "A RESULT CODE IS REQUIRED!!!!!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO VERIFY-UPD-ROUTINE-EXIT.

           IF ACTIVITY-MEMO-REQUIRED = "Y"
                         OR RESULT-MEMO-REQUIRED = "Y"
              IF MEMO-ENTERED-FG NOT = "Y"
                 MOVE "A MEMO ENTRY IS REQUIRED!!!!!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO VERIFY-UPD-ROUTINE-EXIT.

           MOVE " " TO IO-TYPE.

       VERIFY-UPD-ROUTINE-EXIT.
           EXIT.

      ******************************************
       CHECK-STATE-ALLOWED SECTION.
      ******************************************

           IF FD-TEST-WHICH-STATE = "B"
              MOVE BW-STATE TO TEST-STATE
           ELSE
           IF FD-TEST-WHICH-STATE = "L"
              MOVE LN-ORGST TO TEST-STATE
           ELSE
              GO TO EXIT-CHECK-STATE-ALLOWED.

      * TEST DEFAULT TO BAD, NOT ALLOWED
      * SET TO GOOD IF STATE IS FOUND IN TABLE
           MOVE STAT---BAD TO STAT.
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 50
           IF FD-STATE(S1) = TEST-STATE
              MOVE 50 TO S1
              MOVE STAT---GOOD TO STAT
           END-IF
           END-PERFORM.

       EXIT-CHECK-STATE-ALLOWED.
           EXIT.

      *    ****************************
      *    *     NEW RECORD ROUTINE
      *    ****************************
       NEW-RECORD-SETUP SECTION.
       NEW-RECORD.
           MOVE 1 TO NEW-FLAG.
           MOVE 0 TO ELE.
           MOVE "Y" TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE H-AND-M TO SAVE-START-TIME.

      *    ***********************
      *    *    DISPLAY FIELDS
      *    ***********************
       DISPLAY-FIELDS SECTION.
           MOVE SPACES TO DISPLAY-BUF.

           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.
           MOVE LN-PROMAMT TO F04.
           MOVE LN-PROMDATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F05.
           MOVE ORIG-SEND-FORM TO F06.
           MOVE LN-ACTIONCD TO F07.
           MOVE LN-COLLECTOR TO F08.
           MOVE LN-MESSAGE TO F09.

           MOVE ORIG-TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CPOT30 = "Y"
              MOVE "POT" TO SAVE-CON-STAT
           ELSE
           IF CPOT30 = "M"
              MOVE "MIS" TO SAVE-CON-STAT
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "PC" TO SAVE-CON-STAT
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "PL" TO SAVE-CON-STAT
           ELSE
              MOVE CONTRACTUAL TO CON-REC-STAT
              MOVE CON-REC-STAT-ALP TO SAVE-CON-STAT.

           MOVE ORIG-TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-RECENCY.
           IF RDPOT30 = "Y"
              MOVE "POT" TO SAVE-REC-STAT
           ELSE
           IF RDPOT30 = "M"
              MOVE "MIS" TO SAVE-REC-STAT
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "PC" TO SAVE-REC-STAT
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "PL" TO SAVE-REC-STAT
           ELSE
              MOVE RECDEL TO CON-REC-STAT
              MOVE CON-REC-STAT-ALP TO SAVE-REC-STAT.

           PERFORM SET-DISPLAY-F09-ATTR.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
           PERFORM LM-MEMO-RESET-LINE1-KEY.
           MOVE EXT-WI-CL-TRANS-DATE TO MEMO-HOLD-SYS.
           PERFORM LM-MEMO-SCAN.
           MOVE MEMO-ENTRY-LIST TO WR-LIST.
           MOVE STAR-LINE TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************
      *    GET ORIGINAL SEND FORM
      ******************************************
       GET-ORIG-SEND-FORM SECTION.

           PERFORM OPEN-FM1-FILE.

           MOVE 0 TO ORIG-SEND-FORM.

      * GET WAITING COLLECTION FORM REQUEST:
           MOVE FM-PATH-OWNBR TO FM-BRNO.

           MOVE SPACES             TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE 0                  TO LPWSFM-EDIT-FM-FORMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE 0                  TO FM-FMNO.

           PERFORM START-FM1-FILE.
       GET-ORIG-SEND-FORM-NEXT-FM.
           PERFORM READ-FM1-FILE-NEXT.
      *    UNLOCK FM-FILE.
           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO GET-ORIG-SEND-FORM-TEST-AW.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF LPWSFM-EDIT-FM-ACCTNO NOT = LN-ACCTNO
              GO TO GET-ORIG-SEND-FORM-TEST-AW.

           IF FM-PRINT = "Y"
              GO TO GET-ORIG-SEND-FORM-NEXT-FM.

           MOVE FM-FMNO TO FD-FMNO.
           PERFORM GET-FD.
           IF IO-FG = 9
              GO TO GET-ORIG-SEND-FORM-TEST-AW.

           IF FD-FMTYPE NOT = "C"
              GO TO GET-ORIG-SEND-FORM-NEXT-FM.

           MOVE FM-FMNO TO ORIG-SEND-FORM.
           GO TO GET-ORIG-SEND-FORM-EXIT.

       GET-ORIG-SEND-FORM-TEST-AW.
      * GET WAITING ACTION WORK FILE FORM REQUESTS:
           IF BR-EXTRACT-AUTO-UPD = "Y"
              GO TO GET-ORIG-SEND-FORM-EXIT.

           MOVE LN-ACCTNO TO AW-ACCTNO.
      *    PERFORM OPEN-AW1-FILE.
      *    PERFORM READ-AW1-FILE.
      *    IF IO-FG = 0
           IF AW-SETFMNO NOT = 0
               MOVE AW-SETFMNO TO ORIG-SEND-FORM
           ELSE
           IF AW-SETFM(1) NOT = 0
                MOVE AW-SETFM(1) TO ORIG-SEND-FORM
           ELSE
           IF AW-SETFM(2) NOT = 0
                MOVE AW-SETFM(2) TO ORIG-SEND-FORM
           ELSE
           IF AW-SETFM(3) NOT = 0
                MOVE AW-SETFM(3) TO ORIG-SEND-FORM.
      *    PERFORM CLOSE-AW1-FILE.

           MOVE ORIG-SEND-FORM TO FD-FMNO.
           PERFORM GET-FD.
           IF IO-FG = 9
                  OR FD-FMTYPE NOT = "C"
              MOVE 0 TO ORIG-SEND-FORM.

       GET-ORIG-SEND-FORM-EXIT.
           PERFORM CLOSE-FM1-FILE.

      ***********************************
      *   UPDATE MODIFY LOAN FIELDS
      ***********************************
       UPDATE-LOAN-CHANGES SECTION.

           MOVE "UPDATING..........................." TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ORIG-FIELDS = NEW-FIELDS
              GO TO CHECK-UPDATE-MEMO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF NEW-LN-ATCODE NOT = " "
      *JTG IF NEW-LN-ATCODE NOT = ORIG-LN-ATCODE
              MOVE NEW-LN-ATCODE TO LN-ATCODE.
           IF NEW-LN-RECODE NOT = " "
      *JTG IF NEW-LN-RECODE NOT = ORIG-LN-RECODE
              MOVE NEW-LN-RECODE TO LN-RECODE.
           IF NEW-LN-PROMDATE NOT = ORIG-LN-PROMDATE
              IF NEW-LN-PROMDATE = 0
                 MOVE 0 TO LN-PROMDAYS-PASTDUE
              ELSE
                 MOVE TRANS-DATE TO PDUE-TODAY-DATE
                 MOVE LN-1STPYDATE TO PDUE-1STPYDATE
                 PERFORM PAST-DUE-DAYS-CALCULATION
                 MOVE PDUE-DAYS TO LN-PROMDAYS-PASTDUE
              END-IF
              MOVE NEW-LN-PROMDATE TO LN-PROMDATE
              MOVE USER-COLLID TO LN-PROMID.
           IF NEW-LN-PROMDATE2 NOT = ORIG-LN-PROMDATE2
              MOVE NEW-LN-PROMDATE2 TO LN-PROMDATE2.
           IF NEW-LN-PROMAMT NOT = ORIG-LN-PROMAMT
              MOVE NEW-LN-PROMAMT TO LN-PROMAMT.
           IF NEW-LN-MESSAGE NOT = ORIG-LN-MESSAGE
              PERFORM TEST-IF-CREATE-MEMO
              MOVE NEW-LN-MESSAGE TO LN-MESSAGE.

           IF NEW-LN-ACTIONCD NOT = ORIG-LN-ACTIONCD
              MOVE NEW-LN-ACTIONCD TO LN-ACTIONCD.
           IF NEW-LN-COLLECTOR NOT = ORIG-LN-COLLECTOR
              MOVE NEW-LN-COLLECTOR TO LN-COLLECTOR.

           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           IF NEW-SEND-FORM NOT = 0
              IF NEW-SEND-FORM NOT = ORIG-SEND-FORM
                 PERFORM CREATE-NEW-FORM-REQ.

       CHECK-UPDATE-MEMO.

           IF ORIG-FIELDS = NEW-FIELDS
              IF MEMO-ENTERED-FG = "N"
                 PERFORM DELETE-PLACEHOLDER-MEMO
                 GO TO CHECK-UPDATE-QUEUE.

       SETUP-COLLECTOR-MEMO.
           MOVE " " TO MEMO-COLLECTOR-BUF.
           MOVE USER-COLLID TO MEMO-COLLID.
           MOVE " " TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE TIME-EDIT TO MEMO-TIME.
           MOVE NEW-LN-ATCODE TO MEMO-ACTIVITY.
           MOVE NEW-LN-RECODE TO MEMO-RESULT.
           MOVE NEW-LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO MEMO-PROMDATE.
           MOVE NEW-LN-PROMDATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO MEMO-PROMDATE2.
           MOVE NEW-LN-PROMAMT TO MEMO-PROMAMT.
           IF NEW-SEND-FORM NOT = 0
              IF NEW-SEND-FORM NOT = ORIG-SEND-FORM
                 MOVE NEW-SEND-FORM TO MEMO-FORM.

           MOVE NEW-LN-ACTIONCD            TO MEMO-ACTIONCD.

           MOVE NEW-LN-COLLECTOR TO MEMO-NEW-COLLID.
           MOVE MEMO-COLLECTOR-BUF TO LEGEND.
           MOVE "C" TO SET-LM-MEMO-TYPE.
           IF SAVE-LM-COL-SEQ NOT NUMERIC
              OR SAVE-LM-COL-SEQ = 0
                 PERFORM CREATE-MEMO-RECORDS
                 GO TO SKIP-REWRITE-PLACEHOLDER.

       REWRITE-PLACEHOLDER-MEMO.
           PERFORM OPEN-LM1-FILE.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT EXT-WI-CL-TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE SAVE-LM-COL-SEQ TO LM-SEQ.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 9
              MOVE 0 TO SAVE-LM-COL-SEQ
              GO TO SETUP-COLLECTOR-MEMO.

           MOVE LM-MEMO TO MEMO-PLACEHOLDER-BUF.
           IF MEMO-PLACEHOLDER NOT =
                      "ACCOUNT IS CURRENTLY BEING WORKED BY"
              MOVE 0 TO SAVE-LM-COL-SEQ
              GO TO SETUP-COLLECTOR-MEMO.
           MOVE LEGEND TO LM-MEMO.
           PERFORM REWRITE-LM1-FILE.

           MOVE 0 TO SAVE-LM-COL-SEQ.

       SKIP-REWRITE-PLACEHOLDER.
           PERFORM UPDATE-CJ-FILE.

       CHECK-UPDATE-QUEUE.
           IF ( BR-COLLECTOR-QUEUE = "Y" )
      *       IF QUEUED-FG = "Y"
                 PERFORM UPDATE-COLLECTOR-QUEUE.

           IF GP-RESCHEDULE-MEMO = "Y"
              IF (SAVE-NEXT-TIME NOT = 0 OR SAVE-NEXT-DATE NOT = 0)
                 IF ORIG-FIELDS = NEW-FIELDS
                    IF MEMO-ENTERED-FG = "N"
                      IF RESCHED-MEMO-FG NOT = "N"
                         PERFORM CREATE-RESCHEDULE-MEMO.

           IF GP-RESCHEDULE-MEMO = "Y"
              IF UN-RESCHEDULE-FG = "Y"
                 IF ORIG-FIELDS = NEW-FIELDS
                    IF MEMO-ENTERED-FG = "N"
                      IF RESCHED-MEMO-FG NOT = "N"
                         PERFORM CREATE-RESCHEDULE-MEMO.

       UPDATE-LOAN-CHANGES-EXIT.
           EXIT.


      ********************************************************
      *    CREATE NEW FORM REQUEST
      ********************************************************
       CREATE-NEW-FORM-REQ SECTION.

      * NEED TO RESET FM-PATH-OVERIDE BECAUSE HOLD-FD-OPTION MAY
      * CHANGE MULTIPLE TIMES.
      
           MOVE SPACES                     TO FM-PATH-OVERRIDE.
           IF ( HOLD-FD-OPTION-EXTRACTION )
              PERFORM GET-FM-PATH
           ELSE

           PERFORM OPEN-FM1-FILE.

      * DELETE ACTION WORK FILE RECORD:
      *    IF BR-EXTRACT-AUTO-UPD NOT = "Y"
      *       MOVE LN-ACCTNO TO AW-ACCTNO
      *       PERFORM OPEN-AW1-FILE
      *       PERFORM READ-AW1-FILE
      *       IF IO-FG = 9
      *          PERFORM CLOSE-AW1-FILE
      *       ELSE
      *          IF AW-SETFMNO NOT = 0
      *                 OR AW-SETFM(1) NOT = 0
      *                  OR AW-SETFM(2) NOT = 0
      *                   OR AW-SETFM(3) NOT = 0
      *             PERFORM DELETE-AW1-FILE
      *             PERFORM CLOSE-AW1-FILE
      *          ELSE
      *             PERFORM CLOSE-AW1-FILE.

      *****************************************
      * DELETE COLLECTION FORM/NOTICE REQUESTS:
      *****************************************

      * ONLY DELETE THEM IF THE GP FLAG IS NOT SET
           IF ( GP-ALLOW-MORE-COLLECT = "Y" ) OR
              ( HOLD-FD-OPTION-EXTRACTION   )
              GO TO CREATE-NEW-FORM-REQUEST.

           MOVE FM-PATH-OWNBR      TO FM-BRNO.
           MOVE SPACES             TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE 0                  TO LPWSFM-EDIT-FM-FORMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE 0                  TO FM-FMNO.

           PERFORM START-FM1-FILE.
       CREATE-NEW-FORM-REQ-FM1-DEL.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO CREATE-NEW-FORM-REQUEST.

      *    UNLOCK FM-FILE.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF LPWSFM-EDIT-FM-ACCTNO NOT = LN-ACCTNO
              GO TO CREATE-NEW-FORM-REQUEST.

           MOVE FM-FMNO TO FD-FMNO.
           PERFORM GET-FD.
           IF (IO-FG NOT = 0) OR (FD-FMTYPE NOT = "C")
              GO TO CREATE-NEW-FORM-REQ-FM1-DEL.

           PERFORM READ-FM1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM DELETE-FM1-FILE.

           GO TO CREATE-NEW-FORM-REQ-FM1-DEL.

       CREATE-NEW-FORM-REQUEST.
           PERFORM CLEAR-FM-FILE.

           MOVE SPACES             TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE NEW-SEND-FORM      TO LPWSFM-EDIT-FM-FORMNO.
           IF ( HOLD-FD-OPTION-EXTRACTION )
              MOVE LN-OWNBR        TO LPWSFM-EDIT-FM-BRNO2.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE NEW-SEND-FORM          TO FM-FMNO.

           MOVE BW-ZIP TO FM-ZIP.

           IF BR-BANK-ACCTNO = "FDFC" 
              MOVE SPACES TO FM-ZIP.

           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           PERFORM WRITE-FM1-FILE.
           IF ( IO-BAD                    ) AND
              ( HOLD-FD-OPTION-EXTRACTION )
              PERFORM REWRITE-FM1-FILE.

           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.

       CREATE-NEW-FORM-REQ-EXIT.
           PERFORM CLOSE-FM1-FILE.


      * IF FORM IS AN "E" EXTRACTION TYPE FORM (FD-OPTION), THEN
      * PUT THE CORPORATE BRNO PATH IN THE FM-PATH, SO ALL FORM REQUESTS
      * ARE CREATED IN THE HOME OFFICE, THEN WHEN THEY GO "PRINT" THEM ALL
      * IT WILL CREATE AN ASCII FILE

      ******************************
       GET-FM-PATH SECTION.
      ******************************

           IF GP-CORP-BRNO = 0
              GO TO EXIT-GET-FM-PATH.

           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO END-GET-FM-PATH.

           MOVE EXT-FILPATH-BASE           TO FM-ALL-BASE.
           MOVE BR-MACHINE                 TO FM-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO FM-ALL-DATA.
           MOVE FM-ALL-PATH                TO FM-PATH.
           MOVE "Y"                        TO FM-PATH-OVERRIDE.

      *    MOVE FM-PATH-SQL                TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF ( STAT-BAD )
      *       MOVE SPACES                  TO FM-PATH-OVERRIDE.

       END-GET-FM-PATH.
      * GET ORIGINAL BRANCH BACK
           MOVE EXT-CONAME-USER-LOC TO BR-NO
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       EXIT-GET-FM-PATH.
           EXIT.

      ********************************************************
      *    CREATE MEMO RECORDS
      ********************************************************
       CREATE-MEMO-RECORDS SECTION.
           PERFORM OPEN-LM1-FILE.

           MOVE LEGEND TO LM-MEMO.
           MOVE SET-LM-MEMO-TYPE TO LM-MEMO-TYPE.
           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT EXT-WI-CL-TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 1 TO LM-SEQ.
       CREATE-MEMO-RECORDS-AGAIN.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO CREATE-MEMO-RECORDS-AGAIN
              ELSE
                 MOVE "MAXIMUM LINES FOR DAY ALREADY CREATED"
                                               TO MESS
                 PERFORM SEND-MESS.
           PERFORM CLOSE-LM1-FILE.

      ********************************************************
      *    UPDATE CJ-FILE
      ********************************************************
       UPDATE-CJ-FILE SECTION.

           PERFORM OPEN-CJ1-FILE.

           PERFORM CLEAR-CJ-FILE.

           MOVE LN-OWNBR             TO CJ-BRNO.
           MOVE USER-COLLID          TO CJ-COLLID.
           MOVE EXT-WI-CL-TRANS-DATE TO CJ-DATE.
           MOVE LN-ACCTNO            TO CJ-ACCTNO.
           MOVE "Y" TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR  TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE SAVE-START-TIME      TO CJ-START-TIME.
           MOVE H-AND-M              TO CJ-END-TIME.
           MOVE NEW-LN-ATCODE        TO CJ-ACTIVITY-CD.
           MOVE NEW-LN-RECODE        TO CJ-RESULT-CD.
           MOVE NEW-LN-ACTIONCD      TO CJ-ACTION-CD.
           MOVE NEW-LN-COLLECTOR     TO CJ-NEW-COLLID.
           MOVE SAVE-CON-STAT        TO CJ-CON-STAT.
           MOVE SAVE-REC-STAT        TO CJ-REC-STAT.
           MOVE NEW-SEND-FORM        TO CJ-FORM-NO.
           MOVE NEW-LN-PROMDATE      TO CJ-PROMISE-DATE1.
           MOVE NEW-LN-PROMAMT       TO CJ-PROMISE-AMT.
           MOVE NEW-LN-PROMDATE2     TO CJ-PROMISE-DATE2.
           MOVE SAVE-BEG-LM-SEQ      TO CJ-BEG-LM-SEQ.
           MOVE SAVE-END-LM-SEQ      TO CJ-END-LM-SEQ.
           MOVE LN-CURBAL            TO CJ-CUR-BAL.

           MOVE SAVE-TDUE            TO CJ-PAST-DUE.
           IF BR-GLOBAL-QUEUE = "Y"
              MOVE EXT-CONAME-USER-LOC TO CJ-POSTING-BR
           ELSE
              MOVE 0                   TO CJ-POSTING-BR.

           IF QUEUED-FG = "Y"
              MOVE SELECTED-COLLID   TO CJ-QUEUE
           ELSE
      *       MOVE "D" TO CQ-TYPE
              MOVE LN-OWNBR  TO VCQ-BRNO
              MOVE LN-ACCTNO TO VCQ-ACCTNO
              PERFORM OPEN-VCQ1-FILE
              PERFORM READ-VCQ1-FILE
              PERFORM CLOSE-VCQ1-FILE
              IF IO-GOOD
                 MOVE VCQ-CQ-REC     TO CQ-REC
                 MOVE CQ-COLLID      TO CJ-QUEUE.

           MOVE 1 TO CJ-SEQNO.

       WRITE-CJ-AGAIN.
           PERFORM WRITE-CJ1-FILE.
           IF IO-FG NOT = 0
              IF CJ-SEQNO NOT = 99
                 ADD 1 TO CJ-SEQNO
                 GO TO WRITE-CJ-AGAIN
              ELSE
                 MOVE "PROBLEM UPDATING COLLECTOR JOURNAL"
                                               TO MESS
                 PERFORM SEND-MESS.
           PERFORM CLOSE-CJ1-FILE.

      ********************************************************
      *    CREATE RESCHEDULE MEMO
      *    NOTE - ONLY CREATED IF GP FLAG YES & NO OTHER
      *    ACTIVITY PERFORMED, SO WE CAN KNOW WHO RESCHEDULED
      *    09/060/07 WRITE MEMO IF RESCHEDULE REMOVED, TOO
      ********************************************************
       CREATE-RESCHEDULE-MEMO SECTION.
           PERFORM OPEN-LM1-FILE.
           MOVE " " TO RESCHEDULE-MEMO-BUF.
           MOVE EXT-CONAME-USER-ID TO RESCHED-MEMO-USERID.
           MOVE " " TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE TIME-EDIT TO RESCHED-MEMO-TIME.

           IF UN-RESCHEDULE-FG = "Y"
              MOVE "RESCHEDULE REMOVED." TO RESCHED-MEMO
           ELSE
              MOVE "RESCHEDULED ACCOUNT FOR " TO RESCHED-MEMO
              MOVE SAVE-NEXT-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO RESCHED-MEMO-DATE
              MOVE SAVE-NEXT-TIME TO H-AND-M
              IF H-AND-M NOT GREATER THAN 1159
                 MOVE "AM" TO T-AMPM
              ELSE
                 MOVE "PM" TO T-AMPM
              END-IF
              IF H-AND-M IS NOT LESS THAN 1300
                 SUBTRACT 1200 FROM H-AND-M
              END-IF
              IF CUR-HH = 0
                 MOVE 12 TO CUR-HH
              END-IF
              MOVE CUR-HH TO T-HH
              MOVE CUR-MM TO T-MM
              MOVE ":" TO T-COL
              MOVE " " TO T-SPACE
              MOVE TIME-EDIT TO RESCHED-MEMO-TIME2.

           MOVE RESCHEDULE-MEMO-BUF TO LM-MEMO.
           MOVE SET-LM-MEMO-TYPE TO LM-MEMO-TYPE.
           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT EXT-WI-CL-TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 1 TO LM-SEQ.
       CREATE-RESCHEDULE-MEMO-AGAIN.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO CREATE-RESCHEDULE-MEMO-AGAIN
              ELSE
                 MOVE "MAXIMUM LINES FOR DAY ALREADY CREATED"
                                               TO MESS
                 PERFORM SEND-MESS.
           PERFORM CLOSE-LM1-FILE.


      *******************************************
      *    DELETE PLACEHOLD MEMO (NO UPDATE)
      *******************************************
       DELETE-PLACEHOLDER-MEMO SECTION.
       DEL-PLACEHOLDER-MEMO.
           IF SAVE-LM-COL-SEQ NOT NUMERIC
              OR SAVE-LM-COL-SEQ = 0
                 GO TO EXIT-DELETE-PLACEHOLDER-MEMO.
           PERFORM OPEN-LM1-FILE.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT EXT-WI-CL-TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE SAVE-LM-COL-SEQ TO LM-SEQ.
           PERFORM READ-LM1-FILE.
           MOVE LM-MEMO TO MEMO-PLACEHOLDER-BUF.
           IF IO-FG = 0
              IF MEMO-PLACEHOLDER =
                      "ACCOUNT IS CURRENTLY BEING WORKED BY"
                 PERFORM DELETE-LM1-FILE.
           MOVE 0 TO SAVE-LM-COL-SEQ.
       EXIT-DELETE-PLACEHOLDER-MEMO.
           EXIT.

       VERIFY-SPINQ1-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "SPINQ1" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-SPINQ1-PASSWORD.
           EXIT.


       VERIFY-SPINQ1-PASSWORD-MUST-EXIST SECTION.
           MOVE "EX" TO PASSWDW-STAT.
           MOVE "SPINQ1" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       VERIFY-MEMO-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-MEMO-PASSWORD.
           EXIT.

       VERIFY-ACTIONCD-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "AC-&&&&" TO PASSWDW-PGM.
           INSPECT PASSWDW-PGM REPLACING FIRST "&&&&" BY
                                              TEST-LN-ACTIONCD.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD " TO PASSWDW-PROMPT1.
           INSPECT PASSWDW-PROMPT2 REPLACING FIRST "&&&&" BY
                                              TEST-LN-ACTIONCD.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-ACTIONCD-PASSWORD.
           EXIT.

      *****************************************
      *    AUTO RESCHEDULE NEXT CONTACT
      *****************************************
       AUTO-RESCHEDULE SECTION.
       RESCHEDULE-NEXT-CONTACT.
           MOVE "Y" TO TIME-MILITARY-FG.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           COMPUTE TOTAL-MINUTES = (CUR-HH * 60) + CUR-MM +
                                   CD-RE-RESCHEDULE-TIME.
           DIVIDE TOTAL-MINUTES BY 60 GIVING CUR-HH
                                        REMAINDER CUR-MM.
           IF CUR-HH >23
              MOVE 24 TO CUR-HH
              MOVE 0 TO CUR-MM.
           MOVE H-AND-M TO SAVE-NEXT-TIME.
           MOVE TRANS-DATE TO SAVE-NEXT-DATE.

      *****************************************
      *    SET NEXT CONTACT DATE
      *****************************************
       SET-NEXT-CONTACT SECTION.
           MOVE " " TO IO-TYPE.
           MOVE 0 TO SAVE-NEXT-DATE
                     SAVE-NEXT-TIME.
           IF RESULT-RESCHED-REQ = "Y"
              GO TO OPEN-RESCHED-WINDOW.

       UPDATE-QUESTION.
           MOVE
            "F7-UPD,F6-UPD & CONT,F1-BACK,R-RESCHEDULE,U-UNRESCHEDULE:"
                                                 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E RA010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-SET-NEXT-CONTACT.
           IF VDUSTATUS = "1<"
              MOVE "CON" TO IO-TYPE
              GO TO EXIT-SET-NEXT-CONTACT.
           IF VDUSTATUS = "1>"
              GO TO EXIT-SET-NEXT-CONTACT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UPDATE-QUESTION.
           IF VDUBUF = "R"
              GO TO OPEN-RESCHED-WINDOW.
           IF VDUBUF NOT = "U"
              GO TO UPDATE-QUESTION.

       UN-RESCHEDULE-ACCOUNT.
           IF BR-GLOBAL-QUEUE = "Y"
              MOVE LN-OWNBR  TO GQ-BRNO
              MOVE LN-ACCTNO TO GQ-ACCTNO
              PERFORM OPEN-GQ1-FILE
              PERFORM READ-GQ1-FILE
              PERFORM CLOSE-GQ1-FILE
              IF (IO-BAD OR GQ-SEQ-FG NOT = "R")
                 MOVE "ACCOUNT NOT RESCHEDULED!" TO MESS
                 PERFORM SEND-MESS
                 GO TO UPDATE-QUESTION
              END-IF
           ELSE
              MOVE LN-OWNBR  TO VCQ-BRNO
              MOVE LN-ACCTNO TO VCQ-ACCTNO
              PERFORM OPEN-VCQ1-FILE
              PERFORM READ-VCQ1-FILE
              PERFORM CLOSE-VCQ1-FILE
              IF ( IO-BAD )
                 MOVE "ACCOUNT NOT RESCHEDULED!" TO MESS
                 PERFORM SEND-MESS
                 GO TO UPDATE-QUESTION
              ELSE
                 MOVE VCQ-CQ-REC TO CQ-REC
                 IF ( CQ-SEQ-FG NOT = "R" )
                    MOVE "ACCOUNT NOT RESCHEDULED!" TO MESS
                    PERFORM SEND-MESS
                    GO TO UPDATE-QUESTION.

           MOVE UN-RESCHED-SEQ TO PASSWDW-SEQ.
           MOVE "TO REMOVE RE-SCHEDULE" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-SPINQ1-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO UPDATE-QUESTION.

           MOVE "REMOVE RESCHEDULE (Y/N)?:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E RA010SEL." TO VDU.
           PERFORM SCREENIO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UN-RESCHEDULE-ACCOUNT.
           IF VDUBUF = "N"
              GO TO UPDATE-QUESTION.
           IF VDUBUF NOT = "Y"
              GO TO UN-RESCHEDULE-ACCOUNT.
           MOVE "Y" TO UN-RESCHEDULE-FG.
           GO TO EXIT-SET-NEXT-CONTACT.

       OPEN-RESCHED-WINDOW.
           IF GP-TIME-ZONE-Q = "Y"
              MOVE 10 TO WINDOW-LINES
              MOVE 52 TO WINDOW-COLUMNS
              MOVE 13 TO WINDOW-BEGIN-Y
              MOVE 14 TO WINDOW-BEGIN-X
              MOVE "WI/NXCNT2" TO WINDOW-FORM-NAM
              MOVE "NXCNT2" TO VDU-FORM-NAME
              PERFORM OPEN-WINDOW
              DISPLAY NXCNT2-SCREEN
              MOVE NXCNT2-FORM-FIELDS TO WR-BUF
              MOVE NXCNT2-FORM-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE 7 TO WINDOW-LINES
              MOVE 39 TO WINDOW-COLUMNS
              MOVE 16 TO WINDOW-BEGIN-Y
              MOVE 20 TO WINDOW-BEGIN-X
              MOVE "WI/NXCNTC" TO WINDOW-FORM-NAM
              MOVE "NXCNTC" TO VDU-FORM-NAME
              PERFORM OPEN-WINDOW
              DISPLAY NXCNTC-SCREEN 
              MOVE NXCNTC-FORM-FIELDS TO WR-BUF
              MOVE NXCNTC-FORM-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SEND-TIME.
           MOVE BR-TIME-ZONE-FACTOR TO TIME-ZONE-FACTOR.
           PERFORM ZONED-GET-TIME.
           MOVE TIME-EDIT TO TIME-NOW-BUF REMOTE-TIME-EDIT WR-BUF.
           MOVE TIME-NOW-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF GP-TIME-ZONE-Q = "Y"
              IF BW-PHONE = 0
                 MOVE BW-EMPPHONE TO REMOTE-PHONE
              ELSE
                 MOVE BW-PHONE    TO REMOTE-PHONE 
              END-IF
              PERFORM GET-REMOTE-TIME
              IF TZ-FOUND = "Y"
                 MOVE CUR-HH TO REMOTE-HOUR
                 MOVE ":" TO REMOTE-COLON
                 MOVE CUR-MM  TO REMOTE-MIN  
                 MOVE REMOTE-AM-PM TO REMOTE-AMPM
                 MOVE REMOTE-TIME-EDIT TO WR-BUF
                 MOVE REMOTE-TIME-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
              ELSE
                 MOVE "UNKNOWN " TO WR-BUF
                 MOVE REMOTE-TIME-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.
           MOVE SPACES TO REMOTE-TIME-EDIT.
           MOVE SPACES TO REMOTE-TIME-X.

       SET-NEXT-CONTACT-PROM-DATE.
           MOVE 0 TO SAVE-NEXT-DATE.
           MOVE "F7-UPD,F6-UPD & CONT,F1,ENTER DATE:"
                                              TO WINDOW-LEGEND.
           PERFORM SEND-WINDOW-LEGEND.
           MOVE "ER M060010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-SET-NEXT-CONTACT.
      *    IF QUEUED-FG = "Y"
      *       IF NOT (VDUSTATUS = "00" OR "1D")
      *          GO TO SET-NEXT-CONTACT-PROM-DATE
      *       ELSE
      *          NEXT SENTENCE
      *    ELSE
      *       IF NOT (VDUSTATUS = "00" OR "1D" OR "1>")
      *          GO TO SET-NEXT-CONTACT-PROM-DATE.
      *    IF VDUNUM0 = 0
      *       IF QUEUED-FG = "Y"
      *          GO TO SET-NEXT-CONTACT-PROM-DATE
      *       ELSE
      *          GO TO END-SET-NEXT-CONTACT.
           IF VDUSTATUS = "1<"
              MOVE "CON" TO IO-TYPE
              GO TO END-SET-NEXT-CONTACT.
           IF VDUSTATUS = "1>"
              IF RESULT-RESCHED-REQ NOT = "Y"
                 GO TO END-SET-NEXT-CONTACT
              ELSE
                 MOVE "RESCHEDULING REQUIRED FOR THIS RESULT CODE!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO SET-NEXT-CONTACT-PROM-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SET-NEXT-CONTACT-PROM-DATE.
           IF VDUNUM0 = 0
              MOVE "S  8080010." TO VDU
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

           MOVE VDU-DATE-YYYYMMDD TO SAVE-NEXT-DATE SYS-DATE.
           MOVE EXT-WI-CL-TRANS-DATE TO NUM-DATE.

           IF SAVE-NEXT-DATE < EXT-WI-CL-TRANS-DATE
              GO TO SET-NEXT-CONTACT-PROM-DATE.

           PERFORM TIM365.

           IF ELAPSED-DAYS > 30
              MOVE ELAPS-30 TO PASSWDW-SEQ
              MOVE "TO SCHEDULE MORE THAN 30 DAYS" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO SET-NEXT-CONTACT-PROM-DATE
              END-IF
              MOVE "WARNING: YOU ARE SCHEDULING MORE THAN 30 DAYS OUT!"
                                                                 TO MESS
              PERFORM SEND-MESS
           ELSE
           IF ELAPSED-DAYS > 5
              MOVE ELAPS-5 TO PASSWDW-SEQ
              MOVE "TO SCHEDULE MORE THAN 5 DAYS" TO PASSWDW-PROMPT2
              PERFORM VERIFY-SPINQ1-PASSWORD-MUST-EXIST
              IF PASSWDW-STAT NOT = "OK"
                 GO TO SET-NEXT-CONTACT-PROM-DATE.

      *    IF SYS-IS-NE
      *       GO TO END-SET-NEXT-CONTACT.

      ************************************
      *    ENTER START TIME
      ************************************
       ENTER-TIME.
           ACCEPT TIME-NOW FROM TIME.
           IF CUR-HH > 11
              MOVE "PM" TO TIME-WK-BUF-AM-PM 
                           TIME-WK-BUF-AM-PM-2 
           ELSE
              MOVE "AM" TO TIME-WK-BUF-AM-PM
                           TIME-WK-BUF-AM-PM-2.
           IF VDU-FORM-NAME = "NXCNTC"
              MOVE TIME-WK-BUF TO WR-BUF 
              MOVE TIME-WK-LIST TO WR-LIST 
           ELSE
              MOVE TIME-WK-BUF-2 TO WR-BUF
              MOVE TIME-WK-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-NEXT-CONTACT-HH.
           MOVE 0 TO SAVE-NEXT-TIME.
           MOVE " ENTER HOUR (0-12):" TO WINDOW-LEGEND.
           PERFORM SEND-WINDOW-LEGEND.
           MOVE "E NN020020." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "S  A020022." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020021." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020020." TO VDU
              PERFORM SCREENIO
              GO TO SET-NEXT-CONTACT-PROM-DATE.
           IF VDUSTATUS = "1U"
              GO TO SET-NEXT-CONTACT-PROM-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SET-NEXT-CONTACT-HH.
           IF VDUNUM0 > 12
              GO TO SET-NEXT-CONTACT-HH.
           MOVE VDUNUM0 TO HOLD-HH.
           IF GP-TIME-ZONE-Q = "Y"
              MOVE HOLD-HH TO TEST-HOUR
              ADD HOLD-FACTOR TO TEST-HOUR
              IF TEST-HOUR < 1
                 ADD 12 TO TEST-HOUR
                 IF REMOTE-AMPM = "PM"
                    MOVE "AM" TO REMOTE-AMPM
                 ELSE
                    MOVE "PM" TO REMOTE-AMPM
                 END-IF
              END-IF
              IF TEST-HOUR > 12
                 SUBTRACT 12 FROM TEST-HOUR
                 IF REMOTE-AMPM = "PM"
                    MOVE "AM" TO REMOTE-AMPM
                 ELSE
                    MOVE "PM" TO REMOTE-AMPM
                 END-IF
              END-IF
              IF (HOLD-HH = 12 AND HOLD-FACTOR < 0
                    AND TEST-HOUR > 0)
                       MOVE "AM" TO REMOTE-AMPM
              END-IF
              MOVE TEST-HOUR TO REMOTE-HOUR
              MOVE SPACES TO REMOTE-AMPM
              MOVE REMOTE-TIME-EDIT TO WR-BUF
              MOVE REMOTE-TIME-LIST2 TO WR-LIST
              PERFORM CALL-WRFORM.
       SET-NEXT-CONTACT-MM.
           MOVE " ENTER MINUTE (0-59):" TO WINDOW-LEGEND.
           PERFORM SEND-WINDOW-LEGEND.
           MOVE "E NN020022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "S  A020022." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020021." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020020." TO VDU
              PERFORM SCREENIO
              GO TO SET-NEXT-CONTACT-PROM-DATE.
           IF VDUSTATUS = "1U"
              GO TO SET-NEXT-CONTACT-HH.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SET-NEXT-CONTACT-MM.
           IF VDUNUM0 > 59
              GO TO SET-NEXT-CONTACT-MM.
           MOVE VDUNUM0 TO HOLD-MM.
           MOVE "S  A020022." TO VDU.
           MOVE HOLD-MM TO VDUBUF.
           PERFORM SCREENIO.
           IF GP-TIME-ZONE-Q = "Y"
              MOVE HOLD-MM TO REMOTE-MIN
              IF TZ-FOUND = "Y"
                 MOVE REMOTE-TIME-EDIT TO WR-BUF
                 MOVE REMOTE-TIME-LIST2 TO WR-LIST
                 PERFORM CALL-WRFORM
              ELSE
                 MOVE "UNKNOWN " TO WR-BUF
                 MOVE REMOTE-TIME-LIST2 TO WR-LIST
                 PERFORM CALL-WRFORM.

       SET-NEXT-CONTACT-AM-PM.
           MOVE " ENTER 'A' FOR AM OR 'P' FOR PM" TO WINDOW-LEGEND.
           PERFORM SEND-WINDOW-LEGEND.
           MOVE "E  A020023." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "S  A020022." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020021." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020020." TO VDU
              PERFORM SCREENIO
              GO TO SET-NEXT-CONTACT-PROM-DATE.
           IF VDUSTATUS = "1U"
              GO TO SET-NEXT-CONTACT-MM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SET-NEXT-CONTACT-AM-PM.
           IF NOT (VDUBUF = "A" OR "P")
              GO TO SET-NEXT-CONTACT-AM-PM.
      * SETUP HOLD-AM-PM:
           MOVE VDUBUF TO HOLD-A-P.
           MOVE "M" TO HOLD-A-P-M.

           IF HOLD-HH < 1
              IF HOLD-AM-PM = "PM"
                 GO TO SET-NEXT-CONTACT-AM-PM.
           IF HOLD-HH = 12
              IF HOLD-AM-PM = "AM"
                 GO TO SET-NEXT-CONTACT-AM-PM.
           COMPUTE SAVE-NEXT-TIME = HOLD-HH * 100 + HOLD-MM.
           IF HOLD-HH < 12
              IF HOLD-AM-PM = "PM"
                 ADD 1200 TO SAVE-NEXT-TIME.
           MOVE SAVE-NEXT-TIME TO REMOTE-MILITARY-TIME.
           ADD HOLD-FACTOR TO REMOTE-MIL-HH.

       CHECK-TIME-NOT-PREVIOUS.
           IF TRANS-DATE = SAVE-NEXT-DATE
              MOVE "Y" TO TIME-MILITARY-FG
              PERFORM ZONED-GET-TIME
              IF SAVE-NEXT-TIME < H-AND-M
                 MOVE "RESCHEDULE TIME IS EARLIER THAN CURRENT TIME!"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TIME.

           MOVE SAVE-NEXT-HH TO REMOTE-HOUR.
           MOVE SAVE-NEXT-MM TO REMOTE-MIN.
           ADD HOLD-FACTOR TO REMOTE-HOUR.
           IF REMOTE-HOUR < 12
              MOVE "AM" TO REMOTE-AMPM
           ELSE
              MOVE "PM" TO REMOTE-AMPM.
           IF REMOTE-HOUR > 12
              SUBTRACT 12 FROM REMOTE-HOUR.
           IF REMOTE-HOUR = 0
              MOVE 12 TO REMOTE-HOUR.
           MOVE ":" TO REMOTE-COLON.
           MOVE " " TO REMOTE-SPACE.

           IF GP-TIME-ZONE-Q = "Y"
              IF TZ-FOUND = "Y"
                 MOVE REMOTE-TIME-EDIT TO WR-BUF
                 MOVE REMOTE-TIME-LIST2 TO WR-LIST
                 PERFORM CALL-WRFORM
                 IF (GP-CALL-START-TIME NUMERIC) AND
                    (GP-CALL-END-TIME NUMERIC)
                    IF REMOTE-MILITARY-TIME < GP-CALL-START-TIME OR
                       REMOTE-MILITARY-TIME > GP-CALL-END-TIME
                       MOVE SPACES TO MESS
                       STRING
                        "WARNING: SCHEDULED TIME OUTSIDE LEGAL HOURS: "
                         GP-CALL-START-TIME " THRU " GP-CALL-END-TIME
                         DELIMITED BY SIZE INTO MESS
                       PERFORM SEND-MESS
                       GO TO SET-NEXT-CONTACT-HH
                    END-IF
                 END-IF
              ELSE
                 MOVE "UNKNOWN " TO WR-BUF
                 MOVE REMOTE-TIME-LIST2 TO WR-LIST
                 PERFORM CALL-WRFORM
              END-IF
           ELSE
              IF TZ-FOUND = "Y"
                 IF (GP-CALL-START-TIME NUMERIC) AND
                    (GP-CALL-END-TIME NUMERIC)
                    IF REMOTE-MILITARY-TIME < GP-CALL-START-TIME OR
                       REMOTE-MILITARY-TIME > GP-CALL-END-TIME
                 MOVE SPACES TO MESS
                 STRING
                    "WARNING: SCHEDULED TIME OUTSIDE LEGAL HOURS: "
                     GP-CALL-START-TIME " THRU " GP-CALL-END-TIME
                     DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS.

       END-SET-NEXT-CONTACT.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           IF GP-TIME-ZONE-Q = "Y"
              DESTROY NXCNT2-SCREEN
           ELSE
              DESTROY NXCNTC-SCREEN.
           MOVE "SPINQ" TO VDU-FORM-NAME.
           PERFORM CLOSE-WINDOW.

       EXIT-SET-NEXT-CONTACT.
           EXIT.

      ******************************************************************
      *    UPDATE LOCAL COLLECTOR QUEUE
      ******************************************************************
       UPDATE-COLLECTOR-QUEUE SECTION.
           IF BR-GLOBAL-QUEUE = "Y"
              PERFORM UPDATE-GBL-COLLECTOR-QUEUE
           ELSE
              PERFORM UPDATE-LOC-COLLECTOR-QUEUE.

      ******************************************************************
      *    UPDATE LOCAL COLLECTOR QUEUE
      ******************************************************************
       UPDATE-LOC-COLLECTOR-QUEUE SECTION.

      * UPDATE ACCOUNT QUEUE ENTRY:

           MOVE VCQ-PATH                TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              GO TO END-UPDATE-LOC-COLLECTOR-QUEUE.

           MOVE LN-OWNBR  TO VCQ-BRNO.
           MOVE LN-ACCTNO TO VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.

           IF IO-FG = 9
              IF QUEUED-FG NOT = "Y"
                 IF (SAVE-NEXT-TIME NOT = 0 OR SAVE-NEXT-DATE NOT = 0)
                    PERFORM CREATE-LOC-QUEUE-FOR-RESCHED
                 END-IF
              END-IF
              GO TO END-UPDATE-LOC-COLLECTOR-QUEUE.

           MOVE VCQ-CQ-REC TO CQ-REC.

           IF ROUTE-CD = "W"
              MOVE SPACES TO CQ-IN-PROCESS
              PERFORM REWRITE-CQ1-VCQ1-FILE
              PERFORM SET-CQ2-TO-LWCQ2
              GO TO END-UPDATE-LOC-COLLECTOR-QUEUE.

           MOVE CQ-WORKED-FG TO SAVE-OLD-WORKED-FG.
           IF UN-RESCHEDULE-FG = "Y"
              MOVE CQ-DATE-QUEUED TO CQ-DATE
              MOVE 0              TO CQ-START-TIME
              MOVE "N"            TO CQ-SEQ-FG
              MOVE "N"            TO CQ-WORKED-FG.
           IF GP-USE-RESULT-WORKED-FG = "N"
              IF ORIG-FIELDS = NEW-FIELDS
                 IF MEMO-ENTERED-FG = "N"
                    IF SAVE-NEXT-DATE = 0
                       IF SAVE-NEXT-TIME = 0
                          MOVE SPACES TO CQ-IN-PROCESS
                          MOVE SPACES TO CQ-WORK-COLLID
                          PERFORM REWRITE-CQ1-VCQ1-FILE
                          GO TO END-UPDATE-LOC-COLLECTOR-QUEUE.
           IF GP-USE-RESULT-WORKED-FG NOT = "N"
              IF SAVE-RESULT-WORKED-FG NOT = "Y"
                 IF CQ-WORKED-FG NOT = "Y"
                    IF SAVE-NEXT-DATE = 0
                       IF SAVE-NEXT-TIME = 0
                          MOVE SPACES TO CQ-WORK-COLLID
                          PERFORM SET-CQ2-TO-KEEP-CQ2
                          IF CQ-WORKED-FG NOT = "R"
                             MOVE "N" TO CQ-WORKED-FG
                          END-IF
                          GO TO SKIP-ACCOUNT-WORKED.
           MOVE "Y" TO CQ-WORKED-FG.
           MOVE USER-COLLID TO CQ-WORK-COLLID.
           PERFORM SET-CQ2-TO-KEEP-CQ2.
           IF (SAVE-NEXT-TIME NOT = 0 OR SAVE-NEXT-DATE NOT = 0)
              MOVE "R" TO CQ-WORKED-FG CQ-SEQ-FG
              MOVE 0 TO CQ-REV-BEHAVIOR-RISK
              MOVE SAVE-NEXT-DATE TO CQ-DATE
              MOVE SAVE-NEXT-TIME TO CQ-START-TIME.
      * 6/28/00 - MOVED THE SAVE OF CQ2-KEY TO ABOVE, BEFORE
      *           RESCHEDULE MESSED UP SEQUENCE, FOR EX. YOU
      *           SELECT DIFFERENT DELINQUENCY, THEN RESCHEDULE &
      *           F3 KICKS YOU BACK TO FIRST UNWORKED ACCT IN QUEUE
      *    MOVE CQ2-KEY TO SAVE-CQ2-KEY.

       SKIP-ACCOUNT-WORKED.
           MOVE CQ-WORKED-FG TO SAVE-NEW-WORKED-FG.
           MOVE SPACES TO CQ-IN-PROCESS.
           PERFORM REWRITE-CQ1-VCQ1-FILE.

           IF SELECTED-COLLID = CQ-COLLID
              PERFORM UPDATE-THIS-LOC-QUEUE
           ELSE
              PERFORM UPDATE-OTHERS-LOC-QUEUE.

       END-UPDATE-LOC-COLLECTOR-QUEUE.
           PERFORM CLOSE-CQS1-FILE.

      ******************************************************************
      *    UPDATE THIS LOCAL QUEUE
      ******************************************************************
       UPDATE-THIS-LOC-QUEUE SECTION.
           PERFORM OPEN-CQS1-FILE.
           MOVE SELECTED-COLLID TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-THIS-LOC-QUEUE.

           IF QUEUED-FG = "Y"
              IF SAVE-OLD-WORKED-FG NOT = "R"
                    MOVE KEEP-CQ2-KEY TO CQS-CQ2-KEY-FIELDS.
           IF SAVE-NEW-WORKED-FG = "N"
              IF SAVE-OLD-WORKED-FG = "R"
                 SUBTRACT 1 FROM CQS-TIMED-REC
              END-IF
              PERFORM REWRITE-CQS1-FILE
              GO TO END-UPDATE-THIS-LOC-QUEUE.
           IF SAVE-OLD-WORKED-FG = "R"
              IF SAVE-NEW-WORKED-FG = "R"
                 NEXT SENTENCE
              ELSE
                 ADD 1 TO CQS-WORKED-REC
                 SUBTRACT 1 FROM CQS-TIMED-REC
           ELSE
              IF SAVE-NEW-WORKED-FG = "R"
                 ADD 1 TO CQS-TIMED-REC
              ELSE
                 IF SAVE-OLD-WORKED-FG NOT = "Y"
                    ADD 1 TO CQS-WORKED-REC.
           PERFORM REWRITE-CQS1-FILE. 
       END-UPDATE-THIS-LOC-QUEUE.
           EXIT.

      ******************************************************************
      *    UPDATE OTHERS LOCAL QUEUE
      ******************************************************************
       UPDATE-OTHERS-LOC-QUEUE SECTION.
           MOVE CQ-COLLID TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-OTHERS-LOC-QUEUE.

           IF SAVE-NEW-WORKED-FG = "N"
              IF SAVE-OLD-WORKED-FG = "R"
                 SUBTRACT 1 FROM CQS-TIMED-REC
              END-IF
              PERFORM REWRITE-CQS1-FILE
              GO TO END-UPDATE-OTHERS-LOC-QUEUE.

           IF SAVE-OLD-WORKED-FG = "R"
              IF SAVE-NEW-WORKED-FG = "R"
                 NEXT SENTENCE
              ELSE
                 ADD 1 TO CQS-WORKED-REC
                 SUBTRACT 1 FROM CQS-TIMED-REC
           ELSE
              IF SAVE-NEW-WORKED-FG = "R"
                 ADD 1 TO CQS-TIMED-REC
              ELSE
                 IF SAVE-OLD-WORKED-FG NOT = "Y"
                    ADD 1 TO CQS-WORKED-REC.
           PERFORM REWRITE-CQ1-VCQ1-FILE.

       END-UPDATE-OTHERS-LOC-QUEUE.
           EXIT.

      ******************************************************************
      * UPDATE LOCAL COLLECTOR QUEUE FOR SKIP
      ******************************************************************
       UPDATE-SKIP-TO-QUEUE SECTION.
           IF BR-GLOBAL-QUEUE = "Y"
              PERFORM UPDATE-GBL-SKIP-TO-QUEUE
           ELSE
              PERFORM UPDATE-LOC-SKIP-TO-QUEUE.

      ******************************************************************
      * UPDATE LOCAL COLLECTOR QUEUE FOR SKIP
      ******************************************************************
       UPDATE-LOC-SKIP-TO-QUEUE SECTION.

      * HIT F1 TO SKIP ACCT; CLEAR IN-PROCESS & UPDATE NEXT KEY
      * IN COLLECTOR HEADER REC:

           MOVE LN-OWNBR  TO VCQ-BRNO.
           MOVE LN-ACCTNO TO VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.

           IF IO-FG = 9
              GO TO END-UPDATE-LOC-SKIP-QUEUE.

           MOVE VCQ-CQ-REC TO CQ-REC.
           MOVE SPACES TO CQ-IN-PROCESS CQ-WORK-COLLID.
           PERFORM REWRITE-CQ1-VCQ1-FILE.

           IF ROUTE-CD = "W"
              ADD 1 TO CQ-REV-BAL
              PERFORM SET-CQ2-TO-LWCQ2
              GO TO END-UPDATE-LOC-SKIP-QUEUE.

           IF SELECTED-COLLID NOT = CQ-COLLID
              GO TO END-UPDATE-LOC-SKIP-QUEUE.

           PERFORM SET-CQ2-TO-KEEP-CQ2.

       UPDATE-LOC-NEXT-ACCT-IN-QUEUE.
           PERFORM OPEN-CQS1-FILE.
           MOVE SELECTED-COLLID TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-LOC-SKIP-QUEUE.

           IF QUEUED-FG = "Y"
              MOVE KEEP-CQ2-KEY TO CQS-CQ2-KEY-FIELDS.

           PERFORM REWRITE-CQS1-FILE.

       END-UPDATE-LOC-SKIP-QUEUE.
           PERFORM CLOSE-CQS1-FILE.
           PERFORM DELETE-PLACEHOLDER-MEMO.

      ******************************************************************
      * UPDATE LOCAL COLLECTOR QUEUE FOR MANUAL RESCHEDULE
      ******************************************************************
       CREATE-LOC-QUEUE-FOR-RESCHED SECTION.
           PERFORM OPEN-CQ1-FILE.
           PERFORM OPEN-CQS1-FILE.
           MOVE "Y" TO RESCHED-MEMO-FG.

       UPDATE-LOC-RESCHED-QUE-HEADER.
           MOVE SELECTED-COLLID TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.

           IF IO-FG = 9
              MOVE "UNABLE TO RESCHEDULE ACCOUNT!" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO RESCHED-MEMO-FG
              GO TO END-CREATE-LOC-RESCHED-QUEUE.

           ADD 1 TO CQS-TOT-REC.
           ADD 1 TO CQS-TIMED-REC.
           PERFORM REWRITE-CQS1-FILE.

       CREATE-LOC-QUEUE-DETAIL.
           MOVE SPACES TO CQ-REC.

           MOVE BR-NO           TO CQ-BRNO
           MOVE LN-ACCTNO       TO CQ-ACCTNO.
           MOVE SELECTED-COLLID TO CQ-COLLID.
           MOVE SAVE-NEXT-DATE  TO CQ-DATE.
           MOVE SAVE-NEXT-TIME  TO CQ-START-TIME.
           MOVE 0 TO CQ-REV-PDUE-DAYS.
           MOVE 0 TO CQ-SEQ.
           IF CPOT30 = "Y"
              MOVE 98985        TO CQ-REV-DEL
              MOVE TRANS-DATE   TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              COMPUTE CQ-REV-PDUE-DAYS = 9999 - PDUE-DAYS
           ELSE
              COMPUTE CQ-REV-DEL = 99000 - CONTRACTUAL
              MOVE 0 TO AW-REV-PDUE-DAYS.
           COMPUTE CQ-REV-BAL = 999999 - (LN-CURBAL - LN-OT2BAL).
           MOVE USER-COLLID          TO CQ-WORK-COLLID.
           MOVE "R"                  TO CQ-WORKED-FG CQ-SEQ-FG.
           MOVE 0                    TO CQ-REV-BEHAVIOR-RISK.
           MOVE SPACES               TO CQ-IN-PROCESS.
           MOVE EXT-WI-CL-TRANS-DATE TO CQ-DATE-QUEUED.

           PERFORM WRITE-CQ1-FILE.

           PERFORM CLOSE-VCQ1-FILE.
           MOVE "W"       TO VCQUPD-ACTION.            *> WRITE
           MOVE BR-NO     TO VCQUPD-BRNO.
           MOVE CQ-REC    TO VCQUPD-CQ-REC.
           MOVE SPACES    TO VCQUPD-VCQ-REC.
           PERFORM UPDATE-VCQ1-FILE-VCQUPD.

           IF IO-FG NOT = 0
              MOVE "UNABLE TO RESCHEDULE ACCOUNT!" TO MESS
              PERFORM SEND-MESS.

       END-CREATE-LOC-RESCHED-QUEUE.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CQS1-FILE.

      ******************************************************************
      *    UPDATE GLOBAL COLLECTOR QUEUE
      ******************************************************************
       UPDATE-GBL-COLLECTOR-QUEUE SECTION.
      * UPDATE ACCOUNT QUEUE ENTRY:
           MOVE LN-OWNBR  TO GQ-BRNO.
           MOVE LN-ACCTNO TO GQ-ACCTNO.
           PERFORM OPEN-GQ1-FILE.
           PERFORM READ-GQ1-FILE.
           IF IO-FG = 9
              IF QUEUED-FG NOT = "Y"
                 IF (SAVE-NEXT-TIME NOT = 0 OR SAVE-NEXT-DATE NOT = 0)
                    PERFORM CREATE-GBL-QUEUE-FOR-RESCHED
                 END-IF
              END-IF
              GO TO END-UPDATE-GBL-COLLECTOR-QUEUE.

           IF ROUTE-CD = "W"
              MOVE SPACES TO GQ-IN-PROCESS
              PERFORM REWRITE-GQ1-FILE
              PERFORM SET-GQ2-TO-LWGQ2
              GO TO END-UPDATE-GBL-COLLECTOR-QUEUE.

           MOVE GQ-WORKED-FG TO SAVE-OLD-WORKED-FG.
           IF UN-RESCHEDULE-FG = "Y"
              MOVE GQ-DATE-QUEUED TO GQ-DATE
              MOVE 0              TO GQ-START-TIME
              MOVE "N"            TO GQ-SEQ-FG
              MOVE "N"            TO GQ-WORKED-FG.
           IF GP-USE-RESULT-WORKED-FG = "N"
              IF ORIG-FIELDS = NEW-FIELDS
                 IF MEMO-ENTERED-FG = "N"
                    IF SAVE-NEXT-DATE = 0
                       IF SAVE-NEXT-TIME = 0
                          MOVE SPACES TO GQ-IN-PROCESS
                          MOVE SPACES TO GQ-WORK-COLLID
                          PERFORM REWRITE-GQ1-FILE
                          GO TO END-UPDATE-GBL-COLLECTOR-QUEUE.
           IF GP-USE-RESULT-WORKED-FG NOT = "N"
              IF SAVE-RESULT-WORKED-FG NOT = "Y"
                 IF GQ-WORKED-FG NOT = "Y"
                    IF SAVE-NEXT-DATE = 0
                       IF SAVE-NEXT-TIME = 0
                          MOVE SPACES TO GQ-WORK-COLLID
                          PERFORM SET-GQ2-TO-KEEP-GQ2
                          IF GQ-WORKED-FG NOT = "R"
                             MOVE "N" TO GQ-WORKED-FG
                          END-IF
                          GO TO GBL-SKIP-ACCOUNT-WORKED.
           MOVE "Y" TO GQ-WORKED-FG.
           MOVE USER-COLLID TO GQ-WORK-COLLID.
           PERFORM SET-GQ2-TO-KEEP-GQ2.
           IF (SAVE-NEXT-TIME NOT = 0 OR SAVE-NEXT-DATE NOT = 0)
              MOVE "R" TO GQ-WORKED-FG GQ-SEQ-FG
              MOVE SAVE-NEXT-DATE TO GQ-DATE
              MOVE SAVE-NEXT-TIME TO GQ-START-TIME.

       GBL-SKIP-ACCOUNT-WORKED.
           MOVE GQ-WORKED-FG TO SAVE-NEW-WORKED-FG.
           MOVE SPACES TO GQ-IN-PROCESS.
           PERFORM REWRITE-GQ1-FILE.

           IF SELECTED-COLLID = GQ-COLLID
              PERFORM UPDATE-THIS-GBL-QUEUE
           ELSE
              PERFORM UPDATE-OTHERS-GBL-QUEUE.

       END-UPDATE-GBL-COLLECTOR-QUEUE.
           PERFORM CLOSE-GQ1-FILE.

      ******************************************************************
      *    UPDATE THIS GLOBAL QUEUE
      ******************************************************************
       UPDATE-THIS-GBL-QUEUE SECTION.
           PERFORM OPEN-GQS1-FILE.
           MOVE SELECTED-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-THIS-GBL-QUEUE.
           IF QUEUED-FG = "Y"
              IF SAVE-OLD-WORKED-FG NOT = "R"
                    MOVE KEEP-GQ2-KEY TO GQS-GQ2-KEY-FIELDS.
           IF SAVE-NEW-WORKED-FG = "N"
              IF SAVE-OLD-WORKED-FG = "R"
                 SUBTRACT 1 FROM GQS-TIMED-REC
              END-IF
              PERFORM REWRITE-GQS1-FILE
              GO TO END-UPDATE-THIS-GBL-QUEUE.
           IF SAVE-OLD-WORKED-FG = "R"
              IF SAVE-NEW-WORKED-FG = "R"
                 NEXT SENTENCE
              ELSE
                 ADD 1 TO GQS-WORKED-REC
                 SUBTRACT 1 FROM GQS-TIMED-REC
           ELSE
              IF SAVE-NEW-WORKED-FG = "R"
                 ADD 1 TO GQS-TIMED-REC
              ELSE
                 IF SAVE-OLD-WORKED-FG NOT = "Y"
                    ADD 1 TO GQS-WORKED-REC.
           PERFORM REWRITE-GQS1-FILE.
       END-UPDATE-THIS-GBL-QUEUE.
           EXIT.

      ******************************************************************
      *    UPDATE OTHERS GLOBAL QUEUE
      ******************************************************************
       UPDATE-OTHERS-GBL-QUEUE SECTION.
           MOVE GQ-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-OTHERS-GBL-QUEUE.
           IF SAVE-NEW-WORKED-FG = "N"
              IF SAVE-OLD-WORKED-FG = "R"
                 SUBTRACT 1 FROM GQS-TIMED-REC
              END-IF
              PERFORM REWRITE-GQS1-FILE
              GO TO END-UPDATE-OTHERS-GBL-QUEUE.
           IF SAVE-OLD-WORKED-FG = "R"
              IF SAVE-NEW-WORKED-FG = "R"
                 NEXT SENTENCE
              ELSE
                 ADD 1 TO GQS-WORKED-REC
                 SUBTRACT 1 FROM GQS-TIMED-REC
           ELSE
              IF SAVE-NEW-WORKED-FG = "R"
                 ADD 1 TO GQS-TIMED-REC
              ELSE
                 IF SAVE-OLD-WORKED-FG NOT = "Y"
                    ADD 1 TO GQS-WORKED-REC.
           PERFORM REWRITE-GQS1-FILE.
       END-UPDATE-OTHERS-GBL-QUEUE.
           EXIT.

      ******************************************************************
      * UPDATE GLOBAL COLLECTOR QUEUE FOR SKIP
      ******************************************************************
       UPDATE-GBL-SKIP-TO-QUEUE SECTION.
      * HIT F1 TO SKIP ACCT; CLEAR IN-PROCESS & UPDATE NEXT KEY
      * IN COLLECTOR HEADER REC:
           MOVE LN-OWNBR  TO GQ-BRNO.
           MOVE LN-ACCTNO TO GQ-ACCTNO.
           PERFORM OPEN-GQ1-FILE.
           PERFORM READ-GQ1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-GBL-SKIP-QUEUE.

           MOVE SPACES TO GQ-IN-PROCESS GQ-WORK-COLLID.
           PERFORM REWRITE-GQ1-FILE.
           IF ROUTE-CD = "W"
              ADD 1 TO GQ-REV-BAL
              PERFORM SET-GQ2-TO-LWGQ2
              GO TO END-UPDATE-GBL-SKIP-QUEUE.
           IF SELECTED-COLLID NOT = GQ-COLLID
              GO TO END-UPDATE-GBL-SKIP-QUEUE.
           PERFORM SET-GQ2-TO-KEEP-GQ2.
       UPDATE-GBL-NEXT-ACCT-IN-QUEUE.
           PERFORM OPEN-GQS1-FILE.
           MOVE SELECTED-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           IF IO-FG = 9
              GO TO END-UPDATE-GBL-SKIP-QUEUE.
           IF QUEUED-FG = "Y"
              MOVE KEEP-GQ2-KEY TO GQS-GQ2-KEY-FIELDS 
              ADD 1 TO GQS-GQ-REV-BAL.
           PERFORM REWRITE-GQS1-FILE.
       END-UPDATE-GBL-SKIP-QUEUE.
           PERFORM CLOSE-GQS1-FILE.
           PERFORM DELETE-PLACEHOLDER-MEMO.

      ******************************************************************
      * UPDATE GLOBAL COLLECTOR QUEUE FOR MANUAL RESCHEDULE
      ******************************************************************
       CREATE-GBL-QUEUE-FOR-RESCHED SECTION.
           PERFORM OPEN-GQ1-FILE.
           PERFORM OPEN-GQS1-FILE.
           MOVE "Y" TO RESCHED-MEMO-FG.
       UPDATE-GBL-RESCHED-QUE-HEADER.
           MOVE SELECTED-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           IF IO-FG = 9
              MOVE "UNABLE TO RESCHEDULE ACCOUNT!" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO RESCHED-MEMO-FG
              GO TO END-CREATE-GBL-RESCHED-QUEUE.
           ADD 1 TO GQS-TOT-REC.
           ADD 1 TO GQS-TIMED-REC.
           PERFORM REWRITE-GQS1-FILE.

       CREATE-GBL-QUEUE-DETAIL.
           PERFORM CLEAR-GQ-FILE.

           MOVE LN-OWNBR        TO GQ-BRNO.
           MOVE LN-ACCTNO       TO GQ-ACCTNO.
           MOVE SELECTED-COLLID TO GQ-COLLID.
           MOVE SAVE-NEXT-DATE  TO GQ-DATE.
           MOVE SAVE-NEXT-TIME  TO GQ-START-TIME.
           MOVE 0 TO GQ-REV-PDUE-DAYS.
           IF CPOT30 = "Y"
              MOVE 98985        TO GQ-REV-DEL
              MOVE TRANS-DATE   TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              COMPUTE GQ-REV-PDUE-DAYS = 9999 - PDUE-DAYS
           ELSE
              COMPUTE GQ-REV-DEL = 99000 - CONTRACTUAL
              MOVE 0 TO AW-REV-PDUE-DAYS.
           COMPUTE GQ-REV-BAL = 999999 - (LN-CURBAL - LN-OT2BAL).
           MOVE USER-COLLID TO GQ-WORK-COLLID.
           MOVE "R"         TO GQ-WORKED-FG GQ-SEQ-FG.
           MOVE SPACES      TO GQ-IN-PROCESS.
           MOVE EXT-WI-CL-TRANS-DATE  TO GQ-DATE-QUEUED.
           MOVE 0 TO HOLD-GQ-SEQ.

       WRITE-GQ-REC.
           MOVE HOLD-GQ-SEQ TO GQ-SEQ.
           PERFORM WRITE-GQ1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE = "22"
                 IF HOLD-GQ-SEQ < 999
                    ADD 1 TO HOLD-GQ-SEQ
                    GO TO WRITE-GQ-REC.

           IF IO-FG NOT = 0
              MOVE "UNABLE TO RESCHEDULE ACCOUNT!" TO MESS
              PERFORM SEND-MESS
              GO TO END-CREATE-LOC-RESCHED-QUEUE.

       END-CREATE-GBL-RESCHED-QUEUE.
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-GQS1-FILE.

      ******************************************************************
      *
      *    C O D E - S C A N
      *
      ******************************************************************
       CODE-SCAN SECTION.

           MOVE SPACES TO CDSCAN-CD1-KEY.

           IF ( ELE = 1 )
              MOVE 41   TO CDSCAN-BEGIN-X
              MOVE "AT" TO CDSCAN-TYPE
           ELSE
           IF ( ELE = 2 )
              MOVE 41   TO CDSCAN-BEGIN-X
              MOVE "RE" TO CDSCAN-TYPE
           ELSE
           IF ( ELE = 7 )
              MOVE 3    TO CDSCAN-BEGIN-X
              MOVE "AC" TO CDSCAN-TYPE
           ELSE
           IF ( ELE = 8 )
              MOVE 3 TO CDBSCN-BEGIN-X
              MOVE "CN"   TO CDBSCN-TYPE
              IF BR-GLOBAL-QUEUE = "Y"
                 MOVE 0 TO CDBSCN-BRANCH
              ELSE
                 MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH.

           MOVE 8 TO CDSCAN-BEGIN-Y
                     CDBSCN-BEGIN-Y.

           IF ELE = 8
              MOVE "WI/CDBSCN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF CDBSCN-STATUS NOT = "00"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-CODE-SCAN
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE "WI/CDSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF CDSCAN-STATUS NOT = "00"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-CODE-SCAN.


           IF ((CDSCAN-TYPE = "AT") AND (CDSCAN-CODE = SPACES)) OR
              ((CDSCAN-TYPE = "AC") AND (CDSCAN-CODE = SPACES)) OR
              ((CDSCAN-TYPE = "RE") AND (CDSCAN-CODE = SPACES)) OR
              ((CDBSCN-TYPE = "CN") AND (CDBSCN-CODE = SPACES))
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           IF ( ELE = 1  OR 2 )
              MOVE "S  A030  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE CDSCAN-CODE TO VDUBUF
           ELSE
           IF ( ELE = 7 )
              MOVE "S  A040  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE CDSCAN-CODE TO VDUBUF
           ELSE
           IF ( ELE = 8 )
              MOVE "S  A030  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE CDBSCN-CODE TO VDUBUF.

           PERFORM SCREENIO.

           IF ( ELE = 1 OR 2 OR 7)
              MOVE CDSCAN-CODE TO VDUBUF
           IF ( ELE = 8 )
              MOVE CDBSCN-CODE TO VDUBUF.

           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ************************************
      *    FORM NO SCAN
      ************************************
       FORM-SCAN SECTION.
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-SCAN.
           MOVE "S  N030  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE FDSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE FDSCAN-KEY TO VDUNUM0.

       EXIT-FORM-SCAN.
           EXIT.

       SEND-HEAD-3 SECTION.
           EXIT.

      ******************************************************************
      *
      *    R E W R I T E - C Q 1 - V C Q 1 - F I L E
      *
      *=================================================================
      * IN  : CQ-REC
      *       VCQ-REC
      * OUT : IO-FG
      *       VCQ-REC
      * DESC: BEFORE REWRITE/UPDATE THE CQFILE (CQ-REC), WE NEED TO
      *       READ BACK IN THE EXISTING CQ-REC TO VERIFY THERE WAS
      *       NO CHANGES MADE.  IF SO, THEN WE NEED TO SEND A MESSAGE
      *       AND NOT TO PROCEED ANY FURTHER.
      *       AFTER REWRITING/UPDATING CQ-REC, NEED TO MOVE CQ-REC TO
      *       VCQ-REC TO KEEP THEM IN SYNC.
      *       THIS SECTION WILL:
      *
      *         1. MOVE CQ-REC TO HOLD-CQ-REC
      *         2. MOVE ZEROES TO CQ-SEQ
      *         3. READ CQFILE TO GET CQ-REC
      *         4. MOVE VCQ-SEQ INTO CQ-SEQ OF RECENT READ CQ-REC
      *         5. CHECK IF CQ-REC (SQL) EQUALS VCQ-CQ-REC
      *            A. IF EQUAL, MOVE HOLD-CQ-REC BACK TO CQ-REC
      *            B. IF NOT EQUAL, DISPLAY MESSAGE & EXIT PROGRAM
      *         6. REWRITE CQ-REC AND VCQ-REC
      *
      *       THIS SECTION NEEDS TO ENSURE THAT THE GIVEN CQ-REC HAS NOT
      *       BEEN ALTERED BY ANOTHER USER WHILE THE GIVEN USER WAS
      *       WORKING IT.  IF THE CQ-REC HAD BEEN ALTERED, WE NEED TO
      *       MESSAGE THE CURRENT USER THAT A CHANGE HAD BEEN MADE AND
      *       NEED RESTART THE COLLECTION WORK SCREEN IN ORDER TO
      *       RELOAD THE VCQFILE TO GET IT RESYNC WITH THE LATEST DATA.
      *
      * NOTE: ALWAYS UPDATING TO CQ1-KEY EVENTHOUGH THE ORIGINAL
      *       CODE HAD TWO 'PERFORM REWRITE-CQ2-FILE'; LOGICALLY THERE
      *       IS NO DIFFERENCE ON DOING A VISION REWRITE ON A
      *       CQ1 AND CQ2.  NO NEED TO HAVE TWO SECTIONS DOING EXACTLY
      *       THE SAME THING ONLY TO REWRITE/UPDATE ON A DIFFERENT KEY.
      ******************************************************************
       REWRITE-CQ1-VCQ1-FILE SECTION.

           MOVE SPACES TO SHOW-BRNO-ACCT-COLLID.
           STRING CQ-BRNO, "-",
                  CQ-ACCTNO, "-",
                  CQ-COLLID
                  DELIMITED BY SIZE INTO SHOW-BRNO-ACCT-COLLID.
          
           MOVE ZEROES TO CQ-SEQ.
           MOVE CQ-REC TO HOLD-CQ-REC.
 
           PERFORM OPEN-CQ1-FILE.
           PERFORM READ-CQ1-FILE.

           IF ( IO-BAD )
              DISPLAY MESSAGE BOX
                      "ERROR: QUEUE RECORD NOT FOUND", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                    "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!", X"0A"
                      "NEED TO REBUILD COLLECTOR QUEUE"
              GO TO END-ROUTINE.

           MOVE VCQ-SEQ TO CQ-SEQ.

           IF ( CQ-REC NOT = VCQ-CQ-REC )
              DISPLAY MESSAGE BOX
                      "ERROR: ORIGINAL QUEUE HAS BEEN CHANGED", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                    "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!", X"0A"
                      "NEED TO REBUILD COLLECTOR QUEUE"
              GO TO END-ROUTINE.

           MOVE HOLD-CQ-REC TO CQ-REC.
           PERFORM REWRITE-CQ1-FILE.

           IF ( IO-BAD )
              DISPLAY MESSAGE BOX
                      "ERROR: QUEUE RECORD UPDATE FAILED [1]", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                     "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!", X"0A"
                      "NEED TO REBUILD COLLECTOR QUEUE"
              GO TO END-ROUTINE.

           MOVE "R"       TO VCQUPD-ACTION.            *> REWRITE
           MOVE BR-NO     TO VCQUPD-BRNO.
           MOVE CQ-REC    TO VCQUPD-CQ-REC.
           MOVE SPACES    TO VCQUPD-VCQ-REC.
           PERFORM UPDATE-VCQ1-FILE-VCQUPD.

           IF ( IO-BAD )
              DISPLAY MESSAGE BOX
                      "ERROR: QUEUE RECORD UPDATE FAILED [2]", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                     "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!", X"0A"
                      "NEED TO REBUILD COLLECTOR QUEUE"
              GO TO END-ROUTINE.

           PERFORM CLOSE-CQ1-FILE.

      *=================================================================
       REWRITE-CQ1-VCQ1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SPINQ_EVA.CPY".
              COPY "LIBWI/NXCNTC_EVA.CPY".
              COPY "LIBWI/NXCNT2_EVA.CPY".
              COPY "LIBWI/INBRT2_EVA.CPY".
              COPY "LIBWI/WRLDBK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GETDLS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPEARN.CPY".
      *COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
      * HAVE TO HARD CODE THE ROUTINE SO P&L WILL SHOW PBI, PBD
      *COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBSP/SPGQCL.CPY".
       COPY "LIBSP/SPCJCL.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/ZONEDTIMEIO.CPY".
       COPY "LIBSP/SPMEMO.CPY".
       COPY "LIBWI/WILMEMO.CPY".
       COPY "LIBWI/WIBMEMO.CPY".
       COPY "LIBWI/WIDMEMO.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBSP/SPFRVGQ.CPY".


      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.
       GET-LT-BANKRUPT.
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.
       BW1-READ-IO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
       RESET-SCREEN.
           PERFORM FORM-RESET.
       SEND-LEGEND.
           MOVE MSEL-LIST TO WR-LIST.
           MOVE LEGEND TO    WR-BUF.
           PERFORM CALL-WRFORM.
 
       SEND-WSEL-LEGEND.
           MOVE WSEL-LIST TO WR-LIST.
           MOVE MESS      TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-WINDOW-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE WINDOW-LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
       GET-DELINQ-VALUE.
           IF DELINQ-CODE = "R"
              MOVE LN-RECPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED TO DELINQ-ALP
           ELSE IF DELINQ-CODE = "C"
              MOVE LN-CONPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED TO DELINQ-ALP
           ELSE
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE TRANS-DATE TO CEPP2-DATE
              MOVE MAKER-CD TO CEPP-MAKERCD
              IF GP-NO-LC-IN-ARREARAGE
                 PERFORM CONTRACTUAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-DELINQUENT GIVING DELINQ-NUM
              ELSE
                 PERFORM TOTAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-ARREARAGE GIVING DELINQ-NUM.
       SEND-DELINQ-SCAN.
      *    X IS INITIAL FORM VALUE
           IF DELINQ-PROF1 = "X"
              MOVE "D" TO DELINQ-PROF3.
           MOVE DELINQ-PROF3 TO DELINQ-CODE.
           IF DELINQ-CODE = "R"
              MOVE "REC/DELINQ" TO DELINQ-PROMPT
           ELSE IF DELINQ-CODE = "C"
              MOVE "CONTRACTUAL" TO DELINQ-PROMPT
           ELSE
              MOVE "DELINQUENCY" TO DELINQ-PROMPT.
       GET-LC.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
       DISPLAY-INSURANCE-TRACKING.
           MOVE LN-OWNBR  TO LTA-BRNO.
           MOVE LN-ACCTNO TO LTA-ACCTNO.
           MOVE 1         TO LTA-SEQNO.
           PERFORM OPEN-LTA1-FILE.
           PERFORM READ-LTA1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           IF IO-FG = 0
              MOVE LTA-TRACK-ONLY TO D-TRACK
              MOVE LTA-INSURANCE TO D-INS
           ELSE
              MOVE SPACES TO D-TRACK D-INS.

           MOVE "N" TO D-CPP.
           IF LN-INSEFF-DATE(5) NOT = 0
              IF LN-INSEFF-DATE(5) NOT > TRANS-DATE
                 IF LN-INSEXP-DATE(5) NOT = 0
                    IF LN-INSEXP-DATE(5) > TRANS-DATE
                       IF LN-O2-REBATE = " "
                          MOVE "Y" TO D-CPP.

           MOVE DISPLAY-TRACKING TO D-MEMO.
       GET-BW1.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM BW1-READ-IO.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-REC
              MOVE 0 TO BW-SSNO
              MOVE "BORROWER " TO BW-LNAME
              MOVE "DOES NOT EXIST" TO BW-FNAME.

       SEND-LOAN-MESS.
           IF LN-MESSAGE NOT = " "
              PERFORM ALARM
              PERFORM BLINK-STARS 3 TIMES
              PERFORM ALARM.
       BLINK-STARS.
           MOVE "S  A000STAR1." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR1." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           PERFORM SCREENIO.

       DISPLAY-MAKER.
           IF MAKER-CD = "M"
              MOVE "MAKER" TO D-MAKER
           ELSE
              IF MAKER-CD = "J"
                 MOVE "JOINT" TO D-MAKER
              ELSE
                 IF MAKER-CD = "E"
                    MOVE "ENDORSER" TO D-MAKER.

       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

       TEST-IF-CREATE-MEMO.
           IF BR-CHANGE-MEMO = "Y"
              MOVE EXT-CONAME-USER-ID TO MEMO-USERID
              MOVE "FLASH MESSAGE:" TO MEMO-DESCRIPT
              MOVE LN-MESSAGE TO MEMO-ORG-CHAR
              MOVE MEMO-AREA TO LEGEND
              MOVE "S" TO SET-LM-MEMO-TYPE
              PERFORM CREATE-MEMO-RECORDS.

       DISPLAY-ALL-LOANS.
      *    --------------------------------------------------------
      *    ONCE WE HAVE CALLED MORE INFO SCREEN, DON'T ALLOW USERS
      *    GO DEEPER AND RECURSIVLY CALL F3-LOANS           #2843
      *    --------------------------------------------------------
           IF BWLNS3-SCANYN = "N"
              IF VDU-WIMENU-SELECTION = " "
                 MOVE "Y" TO BWLNS3-SCAN-ONLY
              ELSE
                 MOVE "N" TO BWLNS3-SCAN-ONLY
              END-IF
              MOVE EXT-WI-CL-WORKER TO SAVE-WI-CL-WORKER
              MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BWLNS3-KEY
              MOVE LN-ACCTNO                         TO BWLNS3-ACCTNO
              MOVE EXT-WI-CL-TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE                          TO BWLNS3-SYSDATE
              MOVE "WI/BWLNS3" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH BWLNS3-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              MOVE SAVE-WI-CL-WORKER TO EXT-WI-CL-WORKER.
       SEND-PAYMENT-LINES.
           MOVE "WI/LPLINE"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       SEND-PAYOFF.
           MOVE "WI/WIPOFF" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       GET-CDB.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

      *-----------------------------------------------------------------
      *    IN A15, CODE TYPES OF "AT" & "RE" USED THE SAME LAYOUT
      *    WHICH CD-ACTIONCD WAS USED.  SO AT THIS POINT, CD-ACTIONCD
      *    COULD EITHER BE FROM USING "AT" OR "RE".
      *    IN A30, CODE TYPES OF "AT" & "RE" WERE SPLIT INTO THEIR OWN
      *    REDEFINITION (CD-AT-REC & CD-RE-REC).  THE FIELDS THAT WERE
      *    BEING USED IN "AT" & "RE" WERE KEPT IDENTICAL (LOCATION,
      *    NAME {EXCEPT FOR PREFIX} AND SIZE ) FOR SITUATIONS LIKE
      *    BELOW.  SO CD-AT-ACTIONCD & CD-RE-ACTIONCD ARE IDENTICAL AND
      *    EITHER ONE CAN BE USED.  BUT CHECKING CD-AT-TYPE FOR USE.
      *-----------------------------------------------------------------
       TEST-ACTIONCD.
           IF ( CD-TYPE = "AT" )
              IF ( CD-AT-ACTIONCD NOT = SPACES )
                 MOVE "S  A040070."  TO VDU
                 MOVE CD-AT-ACTIONCD TO VDUBUF
                                        NEW-LN-ACTIONCD
                 PERFORM SCREENIO
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ( CD-RE-ACTIONCD NOT = SPACES )
                 MOVE "S  A040070."  TO VDU
                 MOVE CD-RE-ACTIONCD TO VDUBUF
                                        NEW-LN-ACTIONCD
                 PERFORM SCREENIO.

      *-----------------------------------------------------------------
       GET-FD.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
       SET-DISPLAY-F09-ATTR.
           IF ( LN-MESSAGE = SPACES )
              MOVE "."                     TO DISPLAY-F09-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-FLASH )
              MOVE ":F."                   TO DISPLAY-F09-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-REVERSE )
              MOVE ":R."                   TO DISPLAY-F09-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-BOTH )
              MOVE ":FR."                  TO DISPLAY-F09-ATTR
           ELSE
              MOVE "."                     TO DISPLAY-F09-ATTR.

      *=================================================================
      *    USED TO MOVE LAST-WORKED-CQ2-KEY TO CQ2-KEY.
      *=================================================================
       SET-LWCQ2-TO-CQ2.

           MOVE LWCQ2-BRNO               TO CQ-BRNO.
           MOVE LWCQ2-COLLID             TO CQ-COLLID.
           MOVE LWCQ2-REV-BEHAVIOR-RISK  TO CQ-REV-BEHAVIOR-RISK.
           MOVE LWCQ2-SEQ-FG             TO CQ-SEQ-FG.
           MOVE LWCQ2-DATE               TO CQ-DATE.
           MOVE LWCQ2-START-TIME         TO CQ-START-TIME.
           MOVE LWCQ2-REV-DEL            TO CQ-REV-DEL.
           MOVE LWCQ2-MISC-KEY-FLD       TO CQ-MISC-KEY-FLD.
           MOVE LWCQ2-REV-PDUE-DAYS      TO CQ-REV-PDUE-DAYS.
           MOVE LWCQ2-REV-BAL            TO CQ-REV-BAL.
           MOVE LWCQ2-SEQ                TO CQ-SEQ.
           IF CQ-DATE = 0
              MOVE 19000101              TO CQ-DATE.

      *=================================================================
      *    USED TO MOVE CQ2-KEY TO LAST-WORKED-CQ2-KEY.
      *=================================================================
       SET-CQ2-TO-LWCQ2.

           MOVE CQ-BRNO               TO LWCQ2-BRNO.
           MOVE CQ-COLLID             TO LWCQ2-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK  TO LWCQ2-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG             TO LWCQ2-SEQ-FG.
           MOVE CQ-DATE               TO LWCQ2-DATE.
           MOVE CQ-START-TIME         TO LWCQ2-START-TIME.
           MOVE CQ-REV-DEL            TO LWCQ2-REV-DEL.
           MOVE CQ-MISC-KEY-FLD       TO LWCQ2-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS      TO LWCQ2-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL            TO LWCQ2-REV-BAL.
           MOVE CQ-SEQ                TO LWCQ2-SEQ.

      *=================================================================
      *    USED TO MOVE KEEP-CQ2-KEY TO CQ2-KEY.
      *=================================================================
       SET-KEEP-CQ2-TO-CQ2.

           MOVE KEEP-CQ-BRNO               TO CQ-BRNO.
           MOVE KEEP-CQ-COLLID             TO CQ-COLLID.
           MOVE KEEP-CQ-REV-BEHAVIOR-RISK  TO CQ-REV-BEHAVIOR-RISK.
           MOVE KEEP-CQ-SEQ-FG             TO CQ-SEQ-FG.
           MOVE KEEP-CQ-DATE               TO CQ-DATE.
           MOVE KEEP-CQ-START-TIME         TO CQ-START-TIME.
           MOVE KEEP-CQ-REV-DEL            TO CQ-REV-DEL.
           MOVE KEEP-CQ-MISC-KEY-FLD       TO CQ-MISC-KEY-FLD.
           MOVE KEEP-CQ-REV-PDUE-DAYS      TO CQ-REV-PDUE-DAYS.
           MOVE KEEP-CQ-REV-BAL            TO CQ-REV-BAL.
           MOVE KEEP-CQ-SEQ                TO CQ-SEQ.

      *=================================================================
      *    USED TO MOVE CQ2-KEY TO KEEP-CQ2-KEY.
      *=================================================================
       SET-CQ2-TO-KEEP-CQ2.

           MOVE CQ-BRNO               TO KEEP-CQ-BRNO.
           MOVE CQ-COLLID             TO KEEP-CQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK  TO KEEP-CQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG             TO KEEP-CQ-SEQ-FG.
           MOVE CQ-DATE               TO KEEP-CQ-DATE.
           MOVE CQ-START-TIME         TO KEEP-CQ-START-TIME.
           MOVE CQ-REV-DEL            TO KEEP-CQ-REV-DEL.
           MOVE CQ-MISC-KEY-FLD       TO KEEP-CQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS      TO KEEP-CQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL            TO KEEP-CQ-REV-BAL.
           MOVE CQ-SEQ                TO KEEP-CQ-SEQ.

      *=================================================================
      *    USED TO MOVE LAST-WORKED-GQ2-KEY TO GQ2-KEY.
      *=================================================================
       SET-LWGQ2-TO-GQ2.
      *    MOVE LWGQ2-BRNO               TO GQ-BRNO.
           MOVE LWGQ2-COLLID             TO GQ-COLLID.
           MOVE LWGQ2-SEQ-FG             TO GQ-SEQ-FG.
           MOVE LWGQ2-DATE               TO GQ-DATE.
           MOVE LWGQ2-START-TIME         TO GQ-START-TIME.
           MOVE LWGQ2-REV-DEL            TO GQ-REV-DEL.
           MOVE LWGQ2-MISC-KEY-FLD       TO GQ-MISC-KEY-FLD.
           MOVE LWGQ2-REV-PDUE-DAYS      TO GQ-REV-PDUE-DAYS.
           MOVE LWGQ2-REV-BAL            TO GQ-REV-BAL.
           MOVE LWGQ2-SEQ                TO GQ-SEQ.
           IF GQ-DATE = 0 
              MOVE 19000101              TO GQ-DATE.

      *=================================================================
      *    USED TO MOVE GQ2-KEY TO LAST-WORKED-GQ2-KEY.
      *=================================================================
       SET-GQ2-TO-LWGQ2.
      *    MOVE GQ-BRNO               TO LWGQ2-BRNO.
           MOVE GQ-COLLID             TO LWGQ2-COLLID.
           MOVE GQ-SEQ-FG             TO LWGQ2-SEQ-FG.
           MOVE GQ-DATE               TO LWGQ2-DATE.
           MOVE GQ-START-TIME         TO LWGQ2-START-TIME.
           MOVE GQ-REV-DEL            TO LWGQ2-REV-DEL.
           MOVE GQ-MISC-KEY-FLD       TO LWGQ2-MISC-KEY-FLD.
           MOVE GQ-REV-PDUE-DAYS      TO LWGQ2-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL            TO LWGQ2-REV-BAL.
           MOVE GQ-SEQ                TO LWGQ2-SEQ.

      *=================================================================
      *    USED TO MOVE KEEP-GQ2-KEY TO GQ2-KEY.
      *=================================================================
       SET-KEEP-GQ2-TO-GQ2.
      *    MOVE KEEP-GQ-BRNO               TO GQ-BRNO.
           MOVE KEEP-GQ-COLLID             TO GQ-COLLID.
           MOVE KEEP-GQ-SEQ-FG             TO GQ-SEQ-FG.
           MOVE KEEP-GQ-DATE               TO GQ-DATE.
           MOVE KEEP-GQ-START-TIME         TO GQ-START-TIME.
           MOVE KEEP-GQ-REV-DEL            TO GQ-REV-DEL.
           MOVE KEEP-GQ-MISC-KEY-FLD       TO GQ-MISC-KEY-FLD.
           MOVE KEEP-GQ-REV-PDUE-DAYS      TO GQ-REV-PDUE-DAYS.
           MOVE KEEP-GQ-REV-BAL            TO GQ-REV-BAL.
           MOVE KEEP-GQ-SEQ                TO GQ-SEQ.

      *=================================================================
      *    USED TO MOVE GQ2-KEY TO KEEP-GQ2-KEY.
      *=================================================================
       SET-GQ2-TO-KEEP-GQ2.
      *    MOVE GQ-BRNO               TO KEEP-GQ-BRNO.
           MOVE GQ-COLLID             TO KEEP-GQ-COLLID.
           MOVE GQ-SEQ-FG             TO KEEP-GQ-SEQ-FG.
           MOVE GQ-DATE               TO KEEP-GQ-DATE.
           MOVE GQ-START-TIME         TO KEEP-GQ-START-TIME.
           MOVE GQ-REV-DEL            TO KEEP-GQ-REV-DEL.
           MOVE GQ-MISC-KEY-FLD       TO KEEP-GQ-MISC-KEY-FLD.
           MOVE GQ-REV-PDUE-DAYS      TO KEEP-GQ-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL            TO KEEP-GQ-REV-BAL.
           MOVE GQ-SEQ                TO KEEP-GQ-SEQ.


      *********************************
      *      F I L E   I / O          *
      *********************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
      *COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBSP/SPGQSGS_SQL.CPY".
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBSP/SPCQSGS_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLSGGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDMGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLSG1RN.CPY".
       COPY "LIBLP/LPLSR1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPLTA1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBSP/SPVCQ1IN.CPY".
       COPY "LIBSP/SPVCQ2RN.CPY".
       COPY "LIBSP/SPVGQ1IN.CPY".
       COPY "LIBSP/SPCQ1IN.CPY".
       COPY "LIBSP/SPCQS1IN.CPY".
       COPY "LIBSP/SPCQ2RN.CPY".
       COPY "LIBSP/SPGQ1IN.CPY".
       COPY "LIBSP/SPGQ2IN.CPY".
       COPY "LIBSP/SPGQS1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPTZ1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBSP/SPCJ1IN.CPY".
       COPY "LIBLP/LPDM1IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           PERFORM REMOVE-VGQ1-FILE.

       EXIT-PROG.

      * NEED TO RESTORE EXT-FILPATH ON EXIT.

           MOVE HOLD-EXT-FILPATH         TO EXT-FILPATH.
           SET ENVIRONMENT "FIL" TO EXT-FILPATH-FIL.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPINQ-SCREEN.
           DESTROY SPINQ-WINDOW-HANDLE.

           MOVE "LP/LPCPMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
      *    STOP RUN.
           EXIT PROGRAM.
