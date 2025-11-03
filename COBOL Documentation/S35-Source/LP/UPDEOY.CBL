       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UPDEOY.
      **************************************************************************
      *  DESC:  EOY CLEARS AND UPDATES
      *
      *  NOTE:  1/2/3 - NOT SURE WHY WE STARRED OFF THE LOGIC TO VERIFY
      *             THAT END-OF-MONTH WAS COMPLETED BEFORE ALLOWING
      *             END-OF-YEAR TO RUN...I THINK IT WAS BECAUSE WE
      *             USED TO KICK YOU OUT IF THE EOM FLAG ISN'T THERE,
      *             AND NOW WE WOULD HAVE TO CROSS PATHS TO CHECK EOM
      *             FLAGS.   I THINK IF WE NEED TO CHECK EOM, IT
      *             SHOULD BE A LOOP AT THE BEGINNING THAT GOES THROUGH
      *             THE BRANCH RANGE OR GROUP & VERIFIES THE EOM TAG
      *             FOR ALL BEFORE PROCEEDING.  MEANWHILE, ASSUMING
      *             CUSTOMER HAS THE FLAG SET TO CHECK EOM & EOY, THEY
      *             CAN'T OPEN THE FIRST DATE IN JANUARY UNLESS THEY'VE
      *             DONE CALENDAR (OR BOTH) EOY PROCESS.  IF CUSTOMER
      *             DOES END-OF-YEAR AT THE END OF DECEMBER (BOTH, IE
      *             CUSTOMER HAS CALENDAR FISCAL) WITHOUT DOING
      *             END-OF-MONTH, THE EOY LOG WILL REFLECT THAT
      *             LN & BW YTD STATS WERE PROCESSED BUT BY & DS
      *             WERE NOT (SINCE EOM DIDN'T CREATE THE NEW MONTH'S
      *             RECORD).  IF THIS HAPPENS:
      *
      *               1)  REMOVE THE LOCAL FLAG (ED/EOY02B)
      *               2)  RUN END-OF-MONTH
      *               3)  RUN FISCAL END-OF-YEAR (TO DO JUST STATS)
      *               4)  MOVE THE EOY FLAG BACK SO THEY WILL PASS
      *                   OPEN DAY CHECK (MV EOY02F EOY02B)
      *
      *  REV:
      *  MJD 122895 ADDED GROUP LOGIC. CHANGED TAG CHECK TO WRITE
      *             ERROR LOG MESSAGE INSTEAD OF ABORTING (IF ONE
      *             BRANCH BOMBS OUT THEN A ERROR MESSAGE IS WRITTEN
      *             AND THE PROGRAM CONTINUES WITH THE NEXT BRANCH.)
      *             THE BRANCH TO PROCESS IS PASSED TO EOYUPD VIA XFER
      *             FOR EACH BRANCH IN THE RANGE.
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 000103 Y2K BUG, SUBTRACTED 1 FROM SYSTEM DATE YEAR CAUSING
      *             01 TO DISPLAY ON SCREEN, INSTEAD OF 99.
      *              EX: TODAY 010100, SUBTRACT 1 FROM 00 = -01
      *  BLV 000320 INVESTIGATED ENHANCING LOGIC TO PREVENT SOMEONE FROM
      *             RUNNING BOTH A FISCAL & BOTH EOY OR A CALENDAR & BOTH
      *             FOR THE SAME YEAR.  DECIDED TO LEAVE WELL ENOUGH ALONE,
      *             STARRED OFF CHECKS, PROGRAM WAS ALREADY SKIPPING THEM;
      *             TOO HARD TO DETERMINE WHAT MONTH & YR TO USE IN TAG FOR
      *             NON-CALENDAR FISCALS.....
      *  BLV 030212 IF THE BEGINNING FISCAL MONTH IN GLOBAL PARAMETERS IS A 1
      *             FORCE THE CLEAR TYPE TO A "B" FOR BOTH.  SINCE MAJORITY OF
      *             CUSTOMERS ARE ON A CALENDAR FISCAL, THIS WILL STOP THEM
      *             FROM ACCIDENTLY RUNNING TYPE "C" OR "F", PR# 2018
      *  BLV 031024 AFTER A RASH OF END-OF-YEAR PROBLEMS LAST YEAR WE HAVE
      *             ATTEMPTED TO FOOL-PROOF THE END-OF-YEAR PROCEDURE AS MUCH
      *             AS POSSIBLE.  SOME OF THE PROBLEMS WHICH OCCURRED & WHICH
      *             WE TRIED TO ADDRESS INCLUDE:  RUNNING EOY BEFORE RUNNING
      *             EOM, NOT RUNNING EOY, RUNNING INCORRECT COMBINATIONS OF
      *             YEAR-END TYPES SUCH AS "BOTH" AND "FISCAL", ETC.
      *             TO TRY TO PREVENT FURTHER PROBLEMS, WE HAVE CHANGE
      *             END-OF-YEAR TO FORCE THE TYPE AND YEAR BASED ON THE
      *             CURRENT SYSTEM DATE AND THE GLOBAL PARAMETER FISCAL
      *             DATE TABLE.
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *  BLM 081118 SET MEXICO-FG IF I-AM-WORLD & FIL BRANCH # STARTS WITH 3.
      *             IF MEXICO, CHANGE CHECKS TO ALLOW THEM TO DO CALENDAR
      *             YEAR-END IN DECEMBER, FISCAL IN MARCH, WORLD PR# 600
      *  BLM 081119 CHANGE "BEGINNING EOY UPDATE" MESSAGE IN LOG TO INCLUDE
      *             TYPE OF UPDATE: CALENDAR, FISCAL OR BOTH.
      *  BLM 170113 FIX SECURITY, MOVE EOYUPD TO FORM-NAME BEFORE CALLING
      *             CHKSEC
      *  BAH 181101 CHANGE ACCESS CALL ON GBFILE TO DIRECTORY ACCESS ON THE
      *             BRANCH DATA SET, GBFILE
      * BAH 2019.0307 REMOVED REDEFINES OF GP-BEG-FISCAL-DATE, WAS USING
      *               GP-BEG-FISCAL-DATE-MO, USE DATE WORKERS INSTEAD
      *  BAH 190603 BRANCH-BUF WAS NOT GETTING SET, READ-ERROR IN EOYUPD QA0032
      *   CS 190627 REMOVED EOM EOY TAG PATHS, NEEDED TO FIX THE TEST ON
      *             < 15, AND RE-ENTER-DATE, THEY WERE IN MMDDYY FORMAT.
      *             NOT STRIPPING OUT THE A30 CODE THAT I COMMENTED,
      *             SAVED UPDEOY.190627
      *  KEC 191003 ADDED SETTING GB-EOYC-UPDATE-USERID IF YRTYPE-BUF
      *             = "B".
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/UPDEOY S35 03/05/24-09:45:55 >".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

       01  FIRST-ENTRY       PIC X          VALUE "Y".

       01  ACTION.
           03  CHKEY       PIC 9(4).

       01  D-DATE1           PIC X(6)      VALUE " ".
       01  D-DATE           REDEFINES D-DATE1.
           03  D-YY          PIC 99.
           03  D-MM          PIC 99.
           03  D-DD          PIC 99.


       01  BAD-EOY-FG        PIC X VALUE " ".
       01  BAD-EOM-FG        PIC X VALUE " ".

       01  BEG-BRANCH        PIC 9(4).
       01  END-BRANCH        PIC 9(4).
       01  GROUP-FLAG        PIC X.
       01  ENT-GROUP         PIC X(4).
       01  SV-BEG-BR         PIC 9(4)  VALUE 9999.
       01  SV-END-BR         PIC 9(4)  VALUE 9999.
       COPY "LIBGB/PASSWDW.CPY".

       01  EOY-LINK.
           03  YRTYPE-BUF        PIC X    VALUE " ".
           03  BRANCH-BUF        PIC 9(4).
           03  TEST-DATE         PIC X(5).
           03  TEST-DATEX REDEFINES TEST-DATE.
               05  TEST-MM       PIC 99.
               05  TEST-DASH     PIC X.
               05  TEST-YY       PIC 99.
           03  ERRCD             PIC X    VALUE " ".
           03  EOYUPD-WINDOW-HANDLE PIC X(10).

       01  YRTYPE-LIST       PIC X(7)   VALUE "YRTYPE.".
       01  YEAR-BUF          PIC 99     VALUE 0.
       01  YEAR-LIST         PIC X(5)   VALUE "DATE.".
       01  MM-BUF            PIC 9(2)   VALUE 0.
       01  ACCEPT-BUF        PIC X      VALUE " ".
       01  MESS-LIST         PIC X(5)   VALUE "MESS.".

       01  SAVE-LOGNAME      PIC X(10)  VALUE " ".
       01  UPDATE-DATE       PIC 9(8)   VALUE ZEROS.

      *----------------------------------------------------------------
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EOYUPD_DEF.CPY".
       COPY "LIBLP/EOYUPD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/EOYUPD_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE "EOYUPD" TO VDU-FORM-NAME.
           MOVE "LP/EOYUPD" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EOYUPD-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

      * GET SYSTEM DATE TO IN ORDER TO VALIDATE EOY TYPE
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO       TO D-MM.
           MOVE ALP-DA       TO D-DD.
           MOVE ALP-YR       TO D-YY.

           MOVE D-MM         TO  TEST-MM.
           MOVE "/"          TO  TEST-DASH.
           MOVE D-YY         TO  TEST-YY.
           PERFORM DETERMINE-CALENDAR-FISCAL.
           MOVE TEST-YY      TO YEAR-BUF.
           MOVE TEST-MM      TO MM-BUF.

           MOVE TEST-DATE TO WR-BUF.
           MOVE YEAR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE YRTYPE-BUF TO WR-BUF.
           MOVE YRTYPE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF YRTYPE-BUF = "B" OR "C"
              IF TEST-MM NOT = 12
                 MOVE "SYSTEM DATE INDICATES NOT TIME FOR END-OF-YEAR"
                    TO MESS PERFORM SEND-MESS
                 MOVE "SUPPORT ASSISTANCE MAY BE REQUIRED "
                    TO MESS PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           IF YRTYPE-BUF = "F"
              MOVE GP-END-FISCAL-DATE(12) TO NUM-DATE
              IF TEST-MM NOT = NUM-MO
                 MOVE SPACES TO MESS
                 STRING "MISMATCH! SYSTEM MONTH = " TEST-MM
                    " EOY MONTH = " NUM-MO DELIMITED BY SIZE
                     INTO MESS
                     PERFORM SEND-MESS
                 MOVE "SUPPORT ASSISTANCE MAY BE REQUIRED "
                    TO MESS PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO SAVE-LOGNAME.


      *----------------------------------------------------------------
      *    ENTER BRANCH / GROUP RANGES
      *----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY    GO TO BR-01.
           IF F3-KEY    GO TO ALL-BR.
           IF F7-KEY    GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY    GO TO BR-01.
           IF UP-KEY    GO TO BR-01.
           IF F7-KEY    GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.


           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

      *----------------------------------------------------------------
      *  ENTER END OF YEAR MONTH / YEAR IF SECURITY ALLOWS
      *----------------------------------------------------------------
       E-EOY-MMYY.
           IF FIRST-ENTRY = "Y"
              GO TO VERIFY-YEAR-TYPE.

       RE-ENTER-DATE.
      *DONT SEE HOW THIS WORKED BEFORE, I ENTERED 0120 ON SCREEN AND THIS
      *SETS UP TEST-DATE AS 20/00?, I CHANGED THE ENTRY TO BE MM/YY WHICH IS
      *WHAT THE SCREEN IMPLIES AND CHANGED FROM NUMERIC TO ALPHA ENTRY.
           MOVE "E  A050DATE."       TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY    GO TO BR-01.
           IF UP-KEY    GO TO BR-01.
           IF F7-KEY    GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RE-ENTER-DATE.

           MOVE VDUBUF          TO TEST-DATE.
           MOVE TEST-MM         TO DATE-MMDDYY-MM.
           MOVE TEST-YY         TO DATE-MMDDYY-YY.
           MOVE 1               TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD   TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO RE-ENTER-DATE.
                
           PERFORM DETERMINE-CALENDAR-FISCAL.
           MOVE TEST-YY            TO YEAR-BUF.
           MOVE TEST-MM            TO MM-BUF.
        

           MOVE TEST-DATE TO WR-BUF.
           MOVE YEAR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE YRTYPE-BUF TO WR-BUF.
           MOVE YRTYPE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
      *  DETERMINE FISCAL TYPE OF EOY BASED ON SYSTEM DATE
      *  AND END OF YEAR DATES AS FOUND IN GLOBAL G/L
      *----------------------------------------------------------------
       E-YRTYPE.
           IF FIRST-ENTRY = "N"
              GO TO E-YRTYPE-CONT.
           MOVE "S  A000PASSWDX."    TO VDU.
           MOVE "ENTER PASSWORD: "   TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "EN N040PASSWD."     TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0              TO CHKEY.
           IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
              MOVE "S  A000PASSWDX." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000PASSWD."  TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO OK-YN.
       E-YRTYPE-CONT.
           MOVE "N" TO FIRST-ENTRY.
           MOVE "E  A000YRTYPE."     TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY    GO           TO BR-01.
           IF UP-KEY    GO           TO E-EOY-MMYY.
           IF F7-KEY    GO           TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO E-YRTYPE.
           MOVE VDUBUF               TO YRTYPE-BUF.
           IF YRTYPE-BUF  = "C" OR "F" OR "B"
              GO TO OK-YN.
           MOVE "INVALID END OF YEAR CLEAR TYPE " TO MESS.
           PERFORM SEND-MESS.
           GO TO E-YRTYPE.

      *----------------------------------------------------------------
       VERIFY-YEAR-TYPE.
           MOVE "N" TO BAD-EOY-FG BAD-EOM-FG.

           PERFORM VERIFY-MONTH-TAGS.
           IF BAD-EOY-FG = "Y"
              OR BAD-EOM-FG = "Y"
                 GO TO END-ROUTINE.

      *    MOVE "BACK FROM VERIFY-MONTH" TO MESS PERFORM SEND-MESS.

      *----------------------------------------------------------------
       OK-YN.
           MOVE "S  A000PASSWDX." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000PASSWD." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY      GO TO BR-01.
           IF F7-KEY      GO TO END-ROUTINE.
           IF UP-KEY      GO TO E-YRTYPE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF      = "N" GO TO BR-01.
           IF ACCEPT-BUF      = "Y" GO TO BEGIN-EOY-UPDATE.
           GO TO OK-YN.

      *----------------------------------------------------------------

       BEGIN-EOY-UPDATE.
           IF YRTYPE-BUF = "C"
              MOVE "BEGINNING CALENDAR EOY UPDATE...&&:&&&&&&&&&"
                      TO ERRLOGW-EOM-MSG
           ELSE IF YRTYPE-BUF = "F"
              MOVE "BEGINNING FISCAL EOY UPDATE...&&:&&&&&&&&&"
                      TO ERRLOGW-EOM-MSG
           ELSE
              MOVE "BEGINNING EOY UPDATE(BOTH)...&&:&&&&&&&&&"
                      TO ERRLOGW-EOM-MSG.
           PERFORM BEG-END-MSG.
           PERFORM LOG-ERROR.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              MOVE "EXITING EOY UPDATE...&&:&&&&&&&&&"
                 TO ERRLOGW-EOM-MSG
              PERFORM BEG-END-MSG
              PERFORM LOG-ERROR
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-FG = 9 OR BR-NO > END-BRANCH
              MOVE "EXITING EOY UPDATE...&&:&&&&&&&&&"
                 TO ERRLOGW-EOM-MSG
              PERFORM BEG-END-MSG
              PERFORM LOG-ERROR
              GO TO END-ROUTINE.
      
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BR-MACHINE        TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH      TO FILPATH-DATA.
           MOVE "B"               TO GB-PATH-OVERRIDE.
           PERFORM OPEN-GB-FILE. 

           MOVE BR-NO            TO BRANCH-BUF.

      * CALL END OF YEAR CLEAR PROGRAM

           MOVE SPACES TO ERRCD.
           MOVE "LP/EOYUPD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EOY-LINK.
           CANCEL FORM-PROGX.

           IF ERRCD = "Y"
              MOVE "ERRORS HAVE OCCURRED IN UPDATE"
                  TO ERRLOGW-EOM-MSG
              PERFORM LOG-ERROR
              GO TO END-OF-GROUP.

      * UPDATE GBFILE DATE TO PREVENT RE-UPDATE.
      
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-BAD 
              GO TO IO-ERROR.
           MOVE YEAR-BUF TO DATE-MMDDYY-YY.
           MOVE MM-BUF   TO DATE-MMDDYY-MM.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           
           ACCEPT UPDATE-DATE FROM CENTURY-DATE.
           PERFORM GET-TIME.

           IF YRTYPE-BUF = "F"
                MOVE DATE-YYYYMM     TO GB-FISCAL-EOY-TAG-CCYYMM    
                MOVE UPDATE-DATE     TO GB-EOYF-UPDATE-DATE
                MOVE HH-MM-SS        TO GB-EOYF-UPDATE-TIME
                MOVE SAVE-LOGNAME     TO GB-EOYF-UPDATE-USERID
                PERFORM REWRITE-GB-FILE
           ELSE
           IF YRTYPE-BUF = "C"
                MOVE DATE-YYYYMM     TO GB-CALENDAR-EOY-TAG-CCYYMM
                MOVE UPDATE-DATE     TO GB-EOYC-UPDATE-DATE
                MOVE HH-MM-SS        TO GB-EOYC-UPDATE-TIME
                MOVE SAVE-LOGNAME     TO GB-EOYC-UPDATE-USERID
                PERFORM REWRITE-GB-FILE
           ELSE
                MOVE DATE-YYYYMM     TO GB-CALENDAR-EOY-TAG-CCYYMM
                                     GB-FISCAL-EOY-TAG-CCYYMM 

                MOVE UPDATE-DATE TO  GB-EOYC-UPDATE-DATE 
                                     GB-EOYF-UPDATE-DATE
                MOVE HH-MM-SS        TO GB-EOYF-UPDATE-TIME
                                        GB-EOYC-UPDATE-TIME 
                MOVE SAVE-LOGNAME     TO GB-EOYC-UPDATE-USERID
                                         GB-EOYF-UPDATE-USERID
                PERFORM REWRITE-GB-FILE.

           PERFORM CLOSE-GB-FILE.
           
           GO TO END-OF-GROUP.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

      *********************************************
       DETERMINE-CALENDAR-FISCAL SECTION.
      *********************************************

      * IF SYSTEM DATE LESS THAN MIDDLE OF THE MONTH,
      * ASSUME END OF YEAR WAS LAST MONTH
           IF D-DD < 15
                 MOVE -1 TO NDTE-HOLD
                 MOVE D-YY TO DATE-YYMMDD-YY
                 MOVE D-MM TO DATE-YYMMDD-MM
                 MOVE D-DD TO DATE-YYMMDD-DD
                 PERFORM CONVERT-YYMMDD-TO-YYYYMMDD  
                 MOVE DATE-YYYYMMDD TO NDTE-DATE
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE      TO DATE-YYYYMMDD 
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY-MM    TO TEST-MM            
                 MOVE DATE-MMDDYY-YY    TO TEST-YY.


      * IF FIRST FISCAL MONTH = 1, LAST FISCAL MONTH
      * MUST BE 12 INDICATING BOTH CALENDAR/FISCAL
      * FORCE "B"OTH

           MOVE GP-BEG-FISCAL-DATE TO DATE-YYYYMMDD.
           IF DATE-YYYYMMDD-MM = 1
              MOVE "B" TO YRTYPE-BUF
              GO TO EXIT-CALENDAR-FISCAL.

      * IF SYSTEM MONTH = 12, FORCE "C"ALENDAR
      * OTHERWISE, MUST BE "F"ISCAL

           IF TEST-MM = 12
              MOVE "C" TO YRTYPE-BUF
           ELSE
              MOVE "F" TO YRTYPE-BUF.

       EXIT-CALENDAR-FISCAL.
           EXIT.

      ********************************************
       VERIFY-MONTH-TAGS SECTION.
      ********************************************

           MOVE "VERIFYING EOM/EOY " TO WR-BUF.
           MOVE MESS-LIST            TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "BEGINNING EOY VERIFY...&&:&&&&&&&&&"
                                      TO ERRLOGW-EOM-MSG.
           PERFORM BEG-END-MSG.
           PERFORM LOG-ERROR.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO1.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC1.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO1.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              MOVE "EXITING EOY VERIFY...&&:&&&&&&&&&"
                 TO ERRLOGW-EOM-MSG
              PERFORM BEG-END-MSG
              PERFORM LOG-ERROR
              GO TO EXIT-VERIFY-TAGS.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO1.

       NEXT-BRANCH-NO1.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              MOVE "EXITING EOY VERIFY...&&:&&&&&&&&&"
                 TO ERRLOGW-EOM-MSG
              PERFORM BEG-END-MSG
              PERFORM LOG-ERROR
              GO TO EXIT-VERIFY-TAGS.

       MOVE-BRANCH-PATH-INFO1.

           MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BR-MACHINE        TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH      TO FILPATH-DATA.
           MOVE "B"               TO GB-PATH-OVERRIDE.           
           PERFORM OPEN-GB-FILE.

      * IF DATA PATH DOES NOT EXIST, CHECK NEXT BRANCH

      * CAN NOT TEST ACCESS ON GBFILE, IT'S A GLOBAL FILE NOW

      * CHECK EOY UPDATE PREVIOUSLY RUN

           MOVE "VERIFYING END OF YEAR BRANCH $$$$" TO MESS.
           INSPECT MESS REPLACING FIRST "$$$$" BY BR-NO.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE GB-PATH-OWNBR       TO GB-BRNO.
           MOVE 1                   TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-BAD
              GO TO IO-ERROR.                      

           MOVE YEAR-BUF            TO DATE-MMDDYY-YY.
           MOVE TEST-MM             TO DATE-MMDDYY-MM.

           PERFORM CONVERT-MMDDYY-TO-YYYYMM.

           IF YRTYPE-BUF = "F"
              IF GB-FISCAL-EOY-TAG-CCYYMM = DATE-YYYYMM    
                 MOVE SPACES TO ERRLOGW-EOM-MSG
                 STRING "EOY PREVIOUSLY RUN FOR " TEST-DATE               
                  DELIMITED BY SIZE INTO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR
                 MOVE ERRLOGW-EOM-MSG TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO BAD-EOY-FG
                 GO TO END1-OF-LOC.

           IF YRTYPE-BUF = "C"
              IF GB-CALENDAR-EOY-TAG-CCYYMM = DATE-YYYYMM
                 MOVE SPACES TO ERRLOGW-EOM-MSG
                 STRING "EOY PREVIOUSLY RUN FOR " TEST-DATE
                  DELIMITED BY SIZE INTO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR
                 MOVE ERRLOGW-EOM-MSG TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO BAD-EOY-FG
                 GO TO END1-OF-LOC.

      * FISCAL MUST HAVE EOM UPDATE RUN 1ST GET IT FROM THE GBFILE

           MOVE "VERIFYING END OF MONTH BRANCH $$$$" TO MESS.
           INSPECT MESS REPLACING FIRST "$$$$" BY BR-NO.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

         
           MOVE YEAR-BUF    TO DATE-MMDDYY-YY.
           MOVE MM-BUF      TO DATE-MMDDYY-MM.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.

           IF GB-EOM-TAG-CCYYMM NOT = DATE-YYYYMM
              MOVE "PLEASE RUN EOM FOR BRANCH $$$$ PRIOR TO EOY !"
                                             TO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "$$$$" BY BR-NO
              PERFORM LOG-ERROR
              MOVE ERRLOGW-EOM-MSG TO MESS
              PERFORM SEND-MESS
              MOVE "Y" TO BAD-EOM-FG
              GO TO END1-OF-LOC.
           

      * IF BOTH SELECTED, CHECK IF FISCAL UPDATE PREVIOUSLY RUN 
      * GET IT FROM THE GBFILE

           IF YRTYPE-BUF = "B"
              MOVE YEAR-BUF      TO DATE-MMDDYY-YY
              MOVE MM-BUF        TO DATE-MMDDYY-MM 
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD 
              IF GB-FISCAL-EOY-TAG-CCYYMM = DATE-YYYYMM
                 MOVE "FISCAL UPDATE ALREADY COMPLETED" 
                                        TO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR
                 MOVE ERRLOGW-EOM-MSG TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO BAD-EOY-FG
                 GO TO END1-OF-LOC.


           IF YRTYPE-BUF = "B"
              MOVE YEAR-BUF            TO DATE-MMDDYY-YY
              MOVE MM-BUF        TO DATE-MMDDYY-MM
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              IF GB-CALENDAR-EOY-TAG-CCYYMM = DATE-YYYYMM
                 MOVE "CALENDAR UPDATE ALREADY COMPLETED"
                                    TO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR
                 MOVE ERRLOGW-EOM-MSG TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO BAD-EOY-FG
                 GO TO END1-OF-LOC.

       END1-OF-LOC.
           PERFORM CLOSE-GB-FILE.

           GO TO NEXT-GROUP-LOC1.

       EXIT-VERIFY-TAGS.
           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GR1-FILE.


      *****************************************************************
       LOG-ERROR SECTION.
      *****************************************************************

      * LOG ERROR IN GLOBAL LG DIRECTORY  (EOY.YYMMDD)

           MOVE "Y" TO ERRLOGW-ACTION.
           MOVE BR-NO TO ERRLOGW-BRNO.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GPENV.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO EOYUPD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOYUPD-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EOYUPD-SCREEN UPON EOYUPD-WINDOW-HANDLE.
           MOVE EOYUPD-FORM-FIELDS TO WR-BUF.
           MOVE EOYUPD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE EOYUPD-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOYUPD-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EOYUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       MISC-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *----------------------------------------------------------------
       BEG-END-MSG.
           IF GROUP-FLAG = "Y"
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&"
                   BY "GR"
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&"
                   BY ENT-GROUP
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&&"
                   BY "     "
           ELSE
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&"
                   BY "BR"
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&"
                   BY BEG-BRANCH
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&"
                   BY "-"
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&"
                   BY END-BRANCH.

      **************************************************
       IO-ROUTINE SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************************************
       END-ROUTINE SECTION.
      **************************************************
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           MOVE "LP/LPEYMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOYUPD-SCREEN.
           DESTROY EOYUPD-WINDOW-HANDLE.
           EXIT PROGRAM.
