      * COPYMEMBER: LIBWI/WILMEMO
      ********************************************************
      *    LM   M E M O
      * REV:
      *  BLV 080497 CORRECT PROBLEMS WITH SAVE-CJ-BEG-SEQ &
      *             SAVE-CJ-END-SEQ: PRIORITY MEMO OR AUDIT
      *             SHOULD NOT UPDATE THESE FIELDS
      *  GLF 110498 MADE DATE 2000 COMPATIBLE
      *  GLF 990413 FIX NOT BEING ABLE TO AUDIT PRIORITY
      *             MEMOS (Y2K PROBLEM)
      *  BLV 990616 ADD ROUTINE VERIFY-MEMO-PASSWORD TO RESOLVE
      *             CONFLICT WITH VERIFY-PASSWORD ROUTINE IN
      *             SPINQ1 WHICH COPIES THIS ROUTINE
      *  GLF 990809 ADD PASSWORDS TO DELETE AND AUDIT PRIORITY MEMOS
      *  MJD 001004 ADDED LOGIC FOR LOAN MEMO TYPE "R" FOR REVERSALS.
      *             IF THIS OPTION IS TURNED ON AND THE USER DOES NOT ENTER
      *             MEMO TEXT THEN A CHANGE MEMO WILL BE GENERATED IN ITS
      *             PLACE. THE USER IS NOT ALLOWED TO DELETE THESE
      *             NEW TRANSACTION REVERSAL MEMOS.
      *  MJD 001019 CHANGED TO ACCEPT PAGE-UP AND PAGE-DOWN (AS PER JTG)
      *  BAH 010112 TEST GP-I-AM-MODERN AND UP-KEY FROM ENTER-DESC AND
      *             ALLOW THEM TO ENTER ANY SEQ LIKE A48 DID !!
      *  CS  010523 CHANGED TEST ON GP-RV-MEMO TO INCLUDE COMPARE ON
      *             TEST-USER-PROFILE (1ST CHAR OF LOGIN) AGAINST A
      *             LOWERCASE GP-RV-MEMO VALUE.  PR# 1480 (REGIONAL ACC)
      *  BLV 011206 IF I-AM-MODERN, INITIALIZE VDUNUM0 TO 0 TO PREVENT
      *             NON-NUMERICS...I THINK MOST OF THESE OCCURRED IN
      *             THE INITIALIZE ROUTINE TO FIND THE NEXT AVAILABLE
      *             SEQ, BECAUSE THE SYSTEM DATE WAS LOADED IN VDUBUF &
      *             THIS CAUSED NON-NUMERICS WHEN CHECKING VDUNUM0 FOR 0
      *  BLV 020214 REMOVED VERIFY-MEMO-PASSWORD ROUTINE, ROUTINE IS NOW
      *             IN SPINQ1 OR MEMOSC
      *  BAH 020430 IF I-AM-MODERN, ALLOW THEM TO DELETE ANY TYPE OF MEMO
      *             THEY USE MEMOS FOR CUSTOM LEGAL DOCUMENTS
      *  BLV 030219 TRY TO DISPLAY BETTER MESSAGES ABOUT AUDITING SPECIAL
      *             MEMO TYPES (AUTO INS & COLLATERAL)
      *  BAH 030709 LM-MEMO-SCROLL-DOWN WAS ADDING 1 TO LM-SEQ, EVEN IF IT
      *             WAS 99, CAUSING IT TO START AGAIN AT 0, KEEPING YOU IN
      *             A LOOP IF PRIORITY SEQ 1 - 7, THEN 99, CAN NEVER DOWN
      *             DOWN ARROW TO SEE MORE MEMOS....ONLY ADD IF ITS NOT 99
      *             IT WILL DISPLAY THE 99 MEMO AGAIN ON THE NEXT PAGE, WHO
      *             CARES, EASIEST WAY TO FIX IT, MODERN PL# 415
      *  BAH 040315 ADDED GP-BACKDATE-MEMO AND LM-MEMO-TYPE "P" FOR BACKDATED
      *             PAYOFFS/PAYMENTS/REBATES, WORLD PR# 344
      *  BAH 040825 ADDED AUDMEM SEQ 2 FOR EDITING TODAYS MEMO AND ADDED
      *             DELAUD SEQ 4 FOR DELETING TODAYS MEMO, REGACC PR# 75
      *  BAH 070313 CHANGED NAME OF MEMO-VERIFY-PASSWORD TO MEMO-PASSWORD-00-OK
      *             AND ADDED NEW ONES FOR 01-OK AND 02-OK, CAUSING TROUBLE
      *              REGENCY PL# 582
      *  BLF 070810 IF ARCHIVED LOAN CPINQ1 WILL SET MEMOSC-ALLOW-MOD TO
      *             "N"; CHANGED ORDER OF CHECKING FUNCTION KEYS IN
      *             LM-MEMO-ENTER-DESC, SO IF ARCHIVED, JUST ALLOW
      *             SCROLLING UP & DOWN TO VIEW MEMOS, & F7 FOR EXIT,
      *             DON'T ALLOW CHANGING, ENTER, OR DELETING MEMOS,
      *             WORLD PR# 547
      *  BAH 110208 REWRITING AN EXISTING MEMO WAS NOT WORKING, CHANGE
      *             WAS NOT BEING SAVED, MOVE VDUBUF WAS REMOVED FROM ABOVE
      *             CHANGE, PUT IT BACK. UNITED PL# 724
      *  BLM 130530 GOT RID OF RESET-MESS, REPLACED WITH CALL-WRFORM
      *  BAH 161102 NEW CH FIELDS
      *  BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181127 GLOBAL LMFILE
      *  BAH 20190624 PRIORITY MEMO EDIT, PRESS F3, F4-DATE, F3 AND IT WAS
      *               DISPLAYING DATE, NOT "PRIORITY". NEEDED TO MOVE
      *               99999999 TO VDU-DATE-YYYYMMDD BECAUSE VDUNUM0 WAS ONLY
      *               999999. QA0038
      *********************************************************************
       LM-MEMO-MODULE SECTION.
           MOVE 0 TO RV-MEMO-COUNTER.
       LM-MEMO-RESET.
      *    PERFORM RESET-MESS.
           MOVE MEMO-ENTRY-LIST TO WR-LIST.
           MOVE SPACES          TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "N" TO MEMO-AUDIT-FLAG.

           GO TO LM-MEMO-SEND-DATE.
       LM-MEMO-ENTER-DATE.
           MOVE LM-MSG1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE EXT-WI-CL-OWNBR  TO LM-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LM-ACCTNO.

           MOVE "ER M060MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
      *       PERFORM RESET-MESS
              MOVE "MEMODATE." TO WR-LIST
              MOVE SPACES TO WR-BUF
              PERFORM CALL-WRFORM
              IF HOLD-MEMO-TYPE = "P " AND RV-MEMO-COUNTER = 0
                 GO TO LM-MEMO-ENTER-DATE
              ELSE
                 GO TO LM-MEMO-MODULE-EXIT.
           IF UP-KEY OR PAGE-UP-KEY
      * SCROLL BACKWARD FROM CURRENT FILE POINTER
              PERFORM LM-MEMO-SCROLL-UP
              GO TO LM-MEMO-ENTER-DATE.
           IF DOWN-KEY OR PAGE-DOWN-KEY
      * SCROLL FORWARD FROM CURRENT FILE POINTER
              PERFORM LM-MEMO-SCROLL-DOWN
              GO TO LM-MEMO-ENTER-DATE.
           IF VDUSTATUS = "16"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE "PRIORITY" TO VDUBUF
              PERFORM SCREENIO
              MOVE 999999   TO VDUNUM0
              MOVE 99999999 TO VDU-DATE-YYYYMMDD
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO LM-MEMO-ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO DATE-YYYYMMDD.
           MOVE "N" TO MEMO-IS-TODAY.
            
           IF DATE-YYYYMMDD = 0
              IF MEMO-HOLD-SYS = 99999999
                 MOVE 99999999 TO DATE-YYYYMMDD LM-DATE
                 MOVE "S  A080MEMODATE." TO VDU
                 MOVE "PRIORITY" TO VDUBUF
                 PERFORM SCREENIO.

           IF DATE-YYYYMMDD = 0
              MOVE "S  A080MEMODATE." TO VDU
              MOVE MEMO-HOLD-SYS TO DATE-YYYYMMDD LM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE "Y" TO MEMO-IS-TODAY
           ELSE
              IF DATE-YYYYMMDD NOT = 99999999
                 IF MEMO-HOLD-SYS < DATE-YYYYMMDD
                    GO TO LM-MEMO-ENTER-DATE
                 ELSE
                    IF MEMO-HOLD-SYS = DATE-YYYYMMDD
                       MOVE "Y" TO MEMO-IS-TODAY
                    ELSE
                       IF MEMO-PASSWORD-00-OK = "N"
                          MOVE "AUDMEM" TO PASSWDW-PGM
                          MOVE "TO ADJUST OLD MEMO ENTRIES:"
                                           TO PASSWDW-PROMPT2
                          MOVE 0 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO MEMO-PASSWORD-00-OK
                          ELSE
                             GO TO LM-MEMO-RESET.

           IF MEMO-IS-TODAY = "Y"
              IF MEMO-PASSWORD-02-OK = "N"
                MOVE "AUDMEM" TO PASSWDW-PGM
                MOVE "TO ADJUST TODAYS ENTRIES:" TO PASSWDW-PROMPT2
                MOVE 2 TO PASSWDW-SEQ
                PERFORM VERIFY-MEMO-PASSWORD
                IF PASSWDW-STAT = "OK"
                   MOVE "Y" TO MEMO-PASSWORD-02-OK
                ELSE
                   GO TO LM-MEMO-RESET.

           IF DATE-YYYYMMDD = 99999999
             IF MEMO-PASSWORD-01-OK = "N"
                MOVE "AUDMEM" TO PASSWDW-PGM
                MOVE "TO ADJUST PRIORITY ENTRIES:"
                      TO PASSWDW-PROMPT2
                MOVE 1 TO PASSWDW-SEQ
                PERFORM VERIFY-MEMO-PASSWORD
                IF PASSWDW-STAT = "OK"
                   MOVE "Y" TO MEMO-PASSWORD-01-OK
                ELSE
                   GO TO LM-MEMO-RESET.

       LM-MEMO-SEND-DATE.
           MOVE EXT-WI-CL-OWNBR  TO LM-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LM-ACCTNO.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF MEMO-HOLD-SYS = 99999999
                 MOVE 99999999 TO DATE-YYYYMMDD LM-DATE
                 MOVE "S  A080MEMODATE." TO VDU
                 MOVE "PRIORITY" TO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE "S  A080MEMODATE." TO VDU
                 MOVE MEMO-HOLD-SYS TO LM-DATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO.

           IF DATE-YYYYMMDD = 99999999
              MOVE 0 TO LM-DATE
           ELSE
              COMPUTE LM-DATE = 99999999 - DATE-YYYYMMDD.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              MOVE 0 TO LM-SEQ
              GO TO LM-MEMO-NEXT-SEQ.

       LM-MEMO-ENTER-SEQ.
           MOVE LM-MSG2 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020MEMOSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR = "1U"
              GO TO LM-MEMO-RESET.
           IF VDUSTATUS NOT = "00"
              GO TO LM-MEMO-ENTER-SEQ.
           MOVE VDUNUM0 TO LM-SEQ.

       LM-MEMO-NEXT-SEQ.

           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF LM-SEQ = 999
                 MOVE "MEMO LIMIT FOR TODAY HAS BEEN EXCEEDED!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO LM-MEMO-ENTER-DESC
              ELSE
                 ADD 1 TO LM-SEQ.

       LM-MEMO-READ-LM.
           PERFORM OPEN-LM1-FILE.
           PERFORM READ-LM1-FILE.
           MOVE IO-FG TO MEMO-IO-FG.
           PERFORM CLOSE-LM1-FILE.


           IF LM-MEMO-TYPE = "9"
              MOVE SPACES TO MESS.
           IF LM-MEMO-TYPE = " "
              MOVE SPACES TO MESS.
           IF MEMO-AUDIT-FLAG NOT = "Y"
              IF IO-FG NOT = 0
                 GO TO LM-MEMO-SEND-SEQ
              ELSE
                 GO TO LM-MEMO-NEXT-SEQ.

           IF IO-FG NOT = 0
              GO TO LM-MEMO-RESET.

           IF LM-MEMO-TYPE = "P " AND HOLD-MEMO-TYPE = "P "
              MOVE
              "BACKDATED PAYOFF/REBATE MEMO LINE, CAN'T CHANGE"
                                            TO MESS
              PERFORM SEND-MESS
              GO TO LM-MEMO-RESET.

           IF LM-MEMO-TYPE = "R " AND HOLD-MEMO-TYPE = "R "
              MOVE
              "REVERSAL MEMO LINE, CAN'T CHANGE"
                                            TO MESS
              PERFORM SEND-MESS
              GO TO LM-MEMO-RESET.

           IF LM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              IF LM-MEMO-TYPE NOT = "  "
      * THIS CODE IS IN WIBMEMO AND THE PASSWDW-SEQ NEEDS TO STAY
      * CONSISTENT SO I ADDED THIS COMMENT CODE HERE..JUST TO KEEP
      * TRACK OF PASSWORD SEQUENCES, CL/PASSWD.C HAS HARD CODED LOGIC
      * TO CHECK FOR "DELAUD" SEQ 3
      *          MOVE "DELAUD" TO PASSWDW-PGM
      *          MOVE "TO AUDIT SYSTEM MEMOS:" TO PASSWDW-PROMPT2
      *          MOVE 3 TO PASSWDW-SEQ
      *          PERFORM VERIFY-PASSWORD
      *          IF PASSWDW-STAT = "OK"
      *             NEXT SENTENCE
      *          ELSE
                 IF LM-MEMO-TYPE = "J " OR "X " OR "L " OR "D " OR "B "
                    MOVE
                     "LOAN STATUS MEMO LINE, CAN'T CHANGE"
                                                   TO MESS
                    PERFORM SEND-MESS
                    GO TO LM-MEMO-RESET
                 ELSE
                 IF LM-MEMO-TYPE = "R "
                    MOVE
                    "TRANSACTION REVERSAL MEMO LINE, CAN'T CHANGE"
                                                  TO MESS
                    PERFORM SEND-MESS
                    GO TO LM-MEMO-RESET
                 ELSE
                 IF LM-MEMO-TYPE = "P "
                    MOVE
                    "BACKDATED PAYOFF/REBATE MEMO LINE, CAN'T CHANGE"
                                                  TO MESS
                    PERFORM SEND-MESS
                    GO TO LM-MEMO-RESET
                 ELSE
                    MOVE
                     "SYSTEM GENERATED MEMO LINE, CAN'T CHANGE"
                                                   TO MESS
                    IF LM-MEMO-TYPE = "A " MOVE
                     "INSURANCE MEMO; AUDIT IN AUTO INSURANCE WINDOW"
                                                   TO MESS
                    END-IF
                    IF LM-MEMO-TYPE = "T " MOVE
                     "COLLATERAL MEMO; AUDIT IN COLLATERAL INFO WINDOW"
                                                   TO MESS
                    END-IF
                    PERFORM SEND-MESS
                    GO TO LM-MEMO-RESET.

           MOVE "S  A600MEMODESC." TO VDU.
           MOVE LM-MEMO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE LM1-KEY TO LM-MEMO-HOLD-KEY.
           GO TO LM-MEMO-ENTER-DESC.

       LM-MEMO-SEND-SEQ.
           MOVE "SN N020MEMOSEQ." TO VDU.
           MOVE LM-SEQ TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LM1-KEY TO LM-MEMO-HOLD-KEY.

       LM-MEMO-ENTER-DESC.

           IF MEMO-AUDIT-FLAG = "Y"
              IF HOLD-MEMO-TYPE = "  "
                 MOVE LM-MSG3-AUDIT-TOGGLE TO LEGEND
              ELSE
                 MOVE LM-MSG3-AUDIT TO LEGEND
           ELSE
              IF HOLD-MEMO-TYPE = "  "
                 MOVE LM-MSG3-TOGGLE TO LEGEND
              ELSE
                 MOVE LM-MSG3 TO LEGEND.
           IF EXT-WI-CL-ARCHIVE-FG = "Y"
              MOVE LM-MSG3-ARCHIVED TO LEGEND.

           PERFORM SEND-LEGEND.
           MOVE "E  A600MEMODESC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO LM-MEMO-ENTER-DESC.

           IF UP-KEY OR PAGE-UP-KEY
      * SCROLL BACKWARD FROM CURRENT FILE POINTER
              PERFORM LM-MEMO-SCROLL-UP
              GO TO LM-MEMO-ENTER-DESC.

           IF DOWN-KEY OR PAGE-DOWN-KEY
      * SCROLL FORWARD FROM CURRENT FILE POINTER
              PERFORM LM-MEMO-SCROLL-DOWN
              GO TO LM-MEMO-ENTER-DESC.

           IF VDUSTATUS = "1>"
              IF HOLD-MEMO-TYPE = "P " AND RV-MEMO-COUNTER = 0
                 GO TO LM-MEMO-ENTER-DESC
              ELSE
                 GO TO LM-MEMO-MODULE-EXIT.

      * EXT-WI-CL-ARCHIVE-FG = "N" IF ARCHIVED LOAN, CHANGES NOT ALLOWED
      * SO PUT FUNCTIONS ALLOWABLE FOR ARCHIVED ABOVE (UP, DOWN, F7)
      * BUT DON'T ALLOW STUFF BELOW (F6, MODIFY, ADD, PRIORITY).

           IF EXT-WI-CL-ARCHIVE-FG = "Y"
              GO TO LM-MEMO-ENTER-DESC.

           IF VDUSTATUS = "16"
              MOVE "S  A080MEMODATE." TO VDU
              MOVE "PRIORITY" TO VDUBUF
              PERFORM SCREENIO
              MOVE EXT-WI-CL-OWNBR  TO LM-BRNO
              MOVE EXT-WI-CL-ACCTNO TO LM-ACCTNO
              MOVE 0 TO LM-DATE LM-SEQ VDUNUM0
              GO TO LM-MEMO-NEXT-SEQ.

           IF VDUSTATUS = "1<"
              IF HOLD-MEMO-TYPE = "  "
                 GO TO LM-MEMO-MODULE-EXIT.

           IF VDUSTATUS = "18"
              MOVE "Y" TO MEMO-AUDIT-FLAG
              GO TO LM-MEMO-ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO LM-MEMO-ENTER-DESC.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO LM-MEMO-ENTER-DESC.

           IF VDUBUF = SPACES
              GO TO LM-MEMO-ENTER-DESC.

           IF LM-SEQ = 999
              GO TO LM-MEMO-ENTER-DESC.

           IF LM-DATE NOT = 0
              GO TO DELETE-MEMO
           ELSE
              GO TO DELETE-PRIORITY.

       DELETE-MEMO.
           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-LM1-FILE
                 MOVE LM-MEMO-HOLD-KEY TO LM1-KEY
                 PERFORM READ-LM1-FILE
                 IF IO-FG = 0
                    IF MEMO-IS-TODAY NOT = "Y"
                       IF DELE-VERIFY-PASSWORD = "N"
                          MOVE "DELAUD" TO PASSWDW-PGM
                          MOVE "TO DELETE OLD MEMO ENTRIES:"
                                              TO PASSWDW-PROMPT2
                          MOVE 1 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO DELE-VERIFY-PASSWORD
                          ELSE
                             MOVE "S  A600MEMODESC." TO VDU
                             MOVE LM-MEMO TO VDUBUF
                             PERFORM SCREENIO
                             GO TO LM-MEMO-ENTER-DESC
                          END-IF
                       END-IF
                    ELSE
      * MEMO IS FROM TODAY
                       IF DELE-VERIFY-PASSWORD = "N"
                          MOVE "DELAUD" TO PASSWDW-PGM
                          MOVE "TO DELETE TODAYS ENTRIES:"
                                              TO PASSWDW-PROMPT2
                          MOVE 4 TO PASSWDW-SEQ
                          PERFORM VERIFY-MEMO-PASSWORD
                          IF PASSWDW-STAT = "OK"
                             MOVE "Y" TO DELE-VERIFY-PASSWORD
                          ELSE
                             MOVE "S  A600MEMODESC." TO VDU
                             MOVE LM-MEMO TO VDUBUF
                             PERFORM SCREENIO
                             GO TO LM-MEMO-ENTER-DESC
                          END-IF
                       END-IF
                    END-IF
                    PERFORM DELETE-LM1-FILE
                    PERFORM CLOSE-LM1-FILE
                    MOVE 0 TO LM-SEQ
                    PERFORM LM-MEMO-SCAN
                    GO TO LM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-LM1-FILE
                    GO TO LM-MEMO-ENTER-DESC
              ELSE
                 GO TO LM-MEMO-ENTER-DESC.

       DELETE-PRIORITY.
           IF VDUBUF = "DELETE"
              IF MEMO-AUDIT-FLAG = "Y"
                 PERFORM OPEN-LM1-FILE
                 MOVE LM-MEMO-HOLD-KEY TO LM1-KEY
                 PERFORM READ-LM1-FILE
                 IF IO-FG = 0
                    IF DELE-VERIFY-PASSWORD = "N"
                       MOVE "DELAUD" TO PASSWDW-PGM
                       MOVE "TO DELETE PRIORITY ENTRIES:"
                                           TO PASSWDW-PROMPT2
                       MOVE 2 TO PASSWDW-SEQ
                       PERFORM VERIFY-MEMO-PASSWORD
                       IF PASSWDW-STAT = "OK"
                          MOVE "Y" TO DELE-VERIFY-PASSWORD
                       ELSE
                          MOVE "S  A600MEMODESC." TO VDU
                          MOVE LM-MEMO TO VDUBUF
                          PERFORM SCREENIO
                          GO TO LM-MEMO-ENTER-DESC
                       END-IF
                    END-IF
                    PERFORM DELETE-LM1-FILE
                    PERFORM CLOSE-LM1-FILE
                    MOVE 0 TO LM-SEQ
                    PERFORM LM-MEMO-SCAN
                    GO TO LM-MEMO-RESET
                 ELSE
                    PERFORM CLOSE-LM1-FILE
                    GO TO LM-MEMO-ENTER-DESC
              ELSE
                 GO TO LM-MEMO-ENTER-DESC.

       LM-MEMO-UPDATE.
           PERFORM OPEN-LM1-FILE.
           PERFORM DETERMINE-LM-VDUBUF.
           MOVE HOLD-MEMO-TYPE TO LM-MEMO-TYPE.
           MOVE LM-MEMO-HOLD-KEY TO LM1-KEY.
           IF MEMO-IO-FG = 0
              PERFORM READ-LM1-FILE
              IF IO-FG = 0
                 PERFORM DETERMINE-LM-VDUBUF
                 PERFORM REWRITE-LM1-FILE
                 PERFORM CLOSE-LM1-FILE
                 GO TO CONTINUE-LM-MEMO-UPDATE.
           MOVE EXT-WI-CL-OWNBR TO LM-BRNO.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG = 9
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 ADD 1 TO LM-MEMO-HOLD-SEQ
                 IF LM-MEMO-HOLD-SEQ < 999
                    GO TO LM-MEMO-UPDATE
                 ELSE
                    MOVE 999 TO LM-SEQ
                    PERFORM CLOSE-LM1-FILE
                    GO TO LM-MEMO-NEXT-SEQ.
           PERFORM CLOSE-LM1-FILE.
           ADD 1 TO RV-MEMO-COUNTER.

       CONTINUE-LM-MEMO-UPDATE.
      * SAVE SEQ OF FIRST MEMO IN SESSION FOR CJ UPDATE
           IF LM-DATE = 0 OR MEMO-AUDIT-FLAG = "Y"
              GO TO RESTART-LM-SCAN.
           IF SAVE-BEG-LM-SEQ = 0
              MOVE LM-SEQ TO SAVE-BEG-LM-SEQ
                             SAVE-END-LM-SEQ.
           IF LM-SEQ > SAVE-BEG-LM-SEQ
              MOVE LM-SEQ TO SAVE-END-LM-SEQ.
           MOVE "Y" TO MEMO-ENTERED-FG.

       RESTART-LM-SCAN.
      *    START SCAN FROM FIRST SEQ# FOR DATE AND ACCTNO#;
      *    IF MEMO CONTAINS MORE THAN MEMO-MAX LINES, SCAN.
           MOVE 0 TO LM-SEQ.

           PERFORM LM-MEMO-SCAN.

           GO TO LM-MEMO-RESET.

       LM-MEMO-RESET-LINE1-KEY.
           MOVE EXT-WI-CL-OWNBR  TO LM-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LM-ACCTNO.
           MOVE 0                TO LM-DATE LM-SEQ.
           MOVE LM1-KEY TO LM-MEMO-LINE1-KEY.

       LM-MEMO-MODULE-EXIT.
           PERFORM SEND-LEGEND.
           IF HOLD-MEMO-TYPE = "R "
              MOVE EXT-CONAME-USER-ID TO TEST-USER-PROFILE
              IF (GP-RV-MEMO = "Y" OR   
                   TEST-USER-PROFILE > GP-RV-MEMO)
                    AND RV-MEMO-COUNTER = 0
                 MOVE SPACES                  TO CH-REC
                 MOVE EXT-CONAME-USER-ID(2:4) TO CH-BRNO
                 PERFORM GETDAT
                 MOVE SYS-DATE                TO CH-DATE
                 MOVE "R"                     TO CH-CODE
                 MOVE EXT-CONAME-USER-ID      TO CH-USERID
                 PERFORM GET-TIME
                 MOVE HH-MM-SS                TO CH-TIME
                 MOVE 1                       TO CH-SEQ
                 MOVE 0                       TO CH-SSNO 
                 MOVE 0                       TO CH-TYPE 
                 MOVE EXT-WI-CL-ACCTNO        TO CH-ACCTNO
                 MOVE "REVERSALS"             TO CH-DESCRIPT
                 MOVE "CL/CHMEMO" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH CH-REC
                 CANCEL FORM-PROGX.
      * JIM DOES NOT WANT THIS, BUT I DIDNT WANT TO REMOVE IT YET 3/15/04
      *    IF HOLD-MEMO-TYPE = "P"
      *       CALL FLDVAL USING STAT "USERID."  CH-USERID
      *       MOVE CH-USERID TO TEST-USER-PROFILE
      *       IF (GP-BACKDATE-MEMO = "Y" OR   
      *            TEST-USER-PROFILE > GP-BACKDATE-MEMO)
      *          MOVE SPACES           TO CH-REC
      *          MOVE EXT-WI-CL-ACCTNO TO CH-CREDIT-ACCTNO
      *          PERFORM GETDAT
      *          MOVE S-DD TO CH-DAY
      *          PERFORM GET-TIME
      *          MOVE CUR-HH TO CH-HR
      *          MOVE CUR-MM TO CH-MIN
      *          MOVE CUR-SS TO CH-SEC
      *          CALL FLDVAL USING STAT "USERID."  CH-USERID
      *          MOVE CH-USERID(2:4) TO CH-BRNO
      *          MOVE "R" TO CH-CODE
      * NEED THIS TO DISTINGUISH BETWEEN REVERSAL MEMO AND THIS
      *          MOVE "BACKDATE" TO CH-DESCRIPT
      *          MOVE "CL/CHMEMO" TO FORM-NAM
      *          CALL FORM-PROGX USING FORM-PATH CH-REC
      *          CANCEL FORM-PROGX.


      **************************
       DETERMINE-LM-VDUBUF SECTION.
      **************************
           MOVE VDUBUF TO LM-MEMO.

      ******************************************************************
      *
      *    LM   M E M O   S C A N
      *    SHOW MEMO-MAX MEMOS
      *
      ******************************************************************
       LM-MEMO-SCAN SECTION.
           PERFORM SET-MEMO-WHICH.
           PERFORM SEND-HEAD-3.
           MOVE SPACES TO MEMO-BUF.
           MOVE 0 TO MEMO-LINE.

           PERFORM OPEN-LM1-FILE.
           PERFORM START-LM1-FILE-SCAN.

      *----------------------------------------------------------------
       LM-MEMO-SCAN-NEXT-SEQ.
           PERFORM READ-LM1-FILE-NEXT.
      *    UNLOCK LM-FILE.

           IF (IO-FG = 9) OR (LM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                   (LM-ACCTNO NOT = EXT-WI-CL-ACCTNO)
              PERFORM LM-MEMO-BUMP-DOWN
              GO TO LM-MEMO-SCAN-EXIT.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND LM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO LM-MEMO-SCAN-NEXT-SEQ.

           IF MEMO-LINE NOT < MEMO-MAX
              GO TO LM-MEMO-SCAN-EXIT.

           ADD 1 TO MEMO-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(MEMO-LINE)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(MEMO-LINE).

           MOVE LM-SEQ TO MEMO-SEQ(MEMO-LINE).
           MOVE LM-MEMO TO MEMO-MEMO(MEMO-LINE).

           IF MEMO-LINE = 1
              MOVE LM1-KEY TO LM-MEMO-LINE1-KEY.

           MOVE LM1-KEY TO LM-MEMO-LAST-LINE-KEY.

           GO TO LM-MEMO-SCAN-NEXT-SEQ.

      *----------------------------------------------------------------
       LM-MEMO-SCAN-EXIT.
           MOVE MEMO-BUF  TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-LM1-FILE.

      ******************************************************************
      *  L M   M E M O   S P A C E
      ******************************************************************
       LM-MEMO-BUMP-DOWN SECTION.
       LM-BUMP-DOWN-START.
           IF MEMO-LINE > 7
              GO TO LM-EXIT-BUMP-DOWN.

           MOVE LM-MEMO-LINE1-KEY TO LM1-KEY.
           PERFORM START-LM1-FILE-LESS-THAN.

       LM-BUMP-DOWN-READ-PREV.
           PERFORM READ-LM1-FILE-PREVIOUS.
      *    UNLOCK LM-FILE.
           IF (IO-FG NOT = 0) OR (LM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                   (LM-ACCTNO NOT = EXT-WI-CL-ACCTNO)
              GO TO LM-BUMP-DOWN-SAVE-LAST-KEY.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND LM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO LM-BUMP-DOWN-READ-PREV.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF LM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(1).
           MOVE LM-SEQ TO MEMO-SEQ(1).
           MOVE LM-MEMO TO MEMO-MEMO(1).
           MOVE LM1-KEY TO LM-MEMO-LINE1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO LM-BUMP-DOWN-READ-PREV.
       LM-BUMP-DOWN-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(MEMO-LINE) NOT = SPACES
                 MOVE EXT-WI-CL-OWNBR  TO LM-MEMO-LAST-LINE-BRNO
                 MOVE EXT-WI-CL-ACCTNO TO LM-MEMO-LAST-LINE-ACCTNO
                 MOVE MEMO-SEQ(MEMO-LINE) TO LM-MEMO-LAST-LINE-SEQ
                 IF MEMO-DATEX(MEMO-LINE) = "PRIORITY"
                    MOVE 0 TO LM-MEMO-LAST-LINE-DATE
                 ELSE
                    MOVE MEMO-DATE(MEMO-LINE) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE LM-MEMO-LAST-LINE-DATE = 99999999
                                              - DATE-YYYYMMDD.
       LM-EXIT-BUMP-DOWN.
           EXIT.

      *********************************************
      *   SCROLL DOWN ROUTINE
      *********************************************
       LM-MEMO-SCROLL-DOWN SECTION.
           PERFORM OPEN-LM1-FILE.

           MOVE LM-MEMO-LAST-LINE-KEY TO LM1-KEY.
           IF LM-SEQ NOT = 999
              ADD 1 TO LM-SEQ.

           PERFORM START-LM1-FILE-SCAN.
           PERFORM READ-LM1-FILE-NEXT.
      *    UNLOCK LM-FILE.
           IF IO-FG = 0
              IF (LM-BRNO = EXT-WI-CL-OWNBR) AND
                    (LM-ACCTNO = EXT-WI-CL-ACCTNO)
                 MOVE LM-MEMO-LAST-LINE-KEY TO LM1-KEY
                 IF LM-SEQ NOT = 999
                    ADD 1 TO LM-SEQ
                 END-IF
                 PERFORM LM-MEMO-SCAN
              ELSE
                 PERFORM CLOSE-LM1-FILE.

      *********************************************
      *   SCROLL UP ROUTINE
      *********************************************
       LM-MEMO-SCROLL-UP SECTION.
           PERFORM OPEN-LM1-FILE.
           MOVE LM-MEMO-LINE1-KEY TO LM1-KEY.
           PERFORM START-LM1-FILE-LESS-THAN.
           PERFORM READ-LM1-FILE-PREVIOUS.
      *    UNLOCK LM-FILE.
           IF (IO-FG NOT = 0) OR (LM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                  (LM-ACCTNO NOT = EXT-WI-CL-ACCTNO)
              GO TO LM-SCROLL-UP-EXIT.

           IF LM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO WS-MEMO-DATE-MDY
           ELSE
              MOVE "PRIORITY"  TO WS-MEMO-DATEX.

           MOVE LM-SEQ         TO WS-SEQ. 

           IF ( LM-BRNO      = EXT-WI-CL-OWNBR )  AND
              ( LM-ACCTNO    = EXT-WI-CL-ACCTNO ) AND
              ( WS-SEQ       = MEMO-SEQ(1) )      AND
              ( WS-MEMO-DATE = MEMO-DATEX(1) )    AND
              ( LM-MEMO      = MEMO-MEMO(1) )
              GO TO LM-SCROLL-UP-EXIT.

           MOVE 0 TO MEMO-LINE.
           MOVE LM-MEMO-LINE1-KEY TO LM1-KEY.
           PERFORM START-LM1-FILE-LESS-THAN.

       LM-SCROLL-UP-READ-PREV.
           PERFORM READ-LM1-FILE-PREVIOUS.
      *    UNLOCK LM-FILE.
           IF (IO-FG NOT = 0) OR (LM-BRNO NOT = EXT-WI-CL-OWNBR) OR
                  (LM-ACCTNO NOT = EXT-WI-CL-ACCTNO)
              GO TO LM-SCROLL-UP-SAVE-LAST-KEY.

           IF HOLD-MEMO-TYPE NOT = "  "
                   AND LM-MEMO-TYPE NOT = HOLD-MEMO-TYPE
              GO TO LM-SCROLL-UP-READ-PREV.

           MOVE MEMO-TAB(7) TO MEMO-TAB(8).
           MOVE MEMO-TAB(6) TO MEMO-TAB(7).
           MOVE MEMO-TAB(5) TO MEMO-TAB(6).
           MOVE MEMO-TAB(4) TO MEMO-TAB(5).
           MOVE MEMO-TAB(3) TO MEMO-TAB(4).
           MOVE MEMO-TAB(2) TO MEMO-TAB(3).
           MOVE MEMO-TAB(1) TO MEMO-TAB(2).
           IF LM-DATE NOT = 0
              COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO MEMO-DATE(1)
           ELSE
              MOVE "PRIORITY" TO MEMO-TAB(1).
           MOVE LM-SEQ TO MEMO-SEQ(1).
           MOVE LM-MEMO TO MEMO-MEMO(1).
           MOVE LM1-KEY TO LM-MEMO-LINE1-KEY.
           ADD 1 TO MEMO-LINE.
           IF MEMO-LINE < 8
              GO TO LM-SCROLL-UP-READ-PREV.
       LM-SCROLL-UP-SAVE-LAST-KEY.
           IF MEMO-LINE > 0
              IF MEMO-TAB(8) NOT = SPACES
                 MOVE EXT-WI-CL-OWNBR  TO LM-MEMO-LAST-LINE-BRNO
                 MOVE EXT-WI-CL-ACCTNO TO LM-MEMO-LAST-LINE-ACCTNO
                 MOVE MEMO-SEQ(8) TO LM-MEMO-LAST-LINE-SEQ
                 IF MEMO-DATEX(8) = "PRIORITY"
                    MOVE 0 TO LM-MEMO-LAST-LINE-DATE
                 ELSE
                    MOVE MEMO-DATE(8) TO DATE-MMDDYY
                    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                    COMPUTE LM-MEMO-LAST-LINE-DATE = 99999999
                                                   - DATE-YYYYMMDD.
           MOVE MEMO-BUF TO WR-BUF.
           MOVE MEMO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       LM-SCROLL-UP-EXIT.
           PERFORM CLOSE-LM1-FILE.
