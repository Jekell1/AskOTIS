       IDENTIFICATION DIVISION.
       PROGRAM-ID.       OTHBR.
       AUTHOR.             MJD.
       DATE-WRITTEN.    022296.
      ********************************************************
      *                (LP/OTHBR)
      * REMOTE BRANCH LOGIN PROGRAM.
      *
      * DESCRIPTION:
      *     GIVES THE USER THE ABILITY TO LOG INTO ANOTHER BRANCH
      *
      * DESCRIPTIONS:
      * -------------
      * USER CURRENTLY ON R1
      *
      * SITUATION #1:
      *
      *    ACTION  : THE USER ENTERS A BRANCH ON CURRENT MACHINE.
      *    RESPONSE: THE USERS FIL IS CHANGED TO REFLECT THE CHANGE IN
      *              DATA PATHS. THE USERS CURRENT WORKING DIRECTORY IS
      *              CHANGED TO THIS NEW PATH (/$FIL/GB), AND THE
      *              USERS POSTNAME IS CHANGED TO INDICATE THE NEW BRANCH.
      *              THE USER IS RETURNED TO LPMENU IN THE NEW BRANCH,
      *              READY TO PROCESS.
      *    EXAMPLE:  ORIGINAL INFO:  FIL=USR/DATA/R1/GA0102
      *                              PWD=/USR/DATA/R1/GA0102/GB
      *                              POSTNAME=A0102XXX
      *              AFTER ALTERNATE LOGIN TO BRANCH 201.
      *                              FIL=USR/DATA/R1/GA0201
      *                              PWD=/USR/DATA/R1/GA0201/GB
      *                              POSTNAME=A0201XXX
      *    NOTE: THIS OPERATION CAN BE PERFORMED AN INFINITE NUMBER OF TIMES
      *          WITHOUT CAUSING ANY PROBLEMS. THE USER CAN RETURN TO
      *          HIS/HER ORIGINAL BRANCH SIMPLY BY SELECTING IT. THEY
      *          DO NOT HAVE TO BACK OUT OF EVERY BRANCH THEY HAVE
      *          ACCESSED.
      *
      * REV:
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 021698 CHANGED TO FORGO THE 'SU -' AND JUST CHANGE
      *             FIL,AND DATA PATH.
      *  MJD 032598 ADDED LIBGB/RLOGINW. ADDED SEPARATE FLAG FOR
      *             RLOGIN VS. ALTERNATE LOGIN.
      *  MJD 032698 CHANGED ENTRY OF BRANCH. THE USER CAN NOW ENTER
      *             A BRANCH NUMBER ON THE CURRENT MACHINE OR
      *             A TWO DIGIT MACHINE NAME FOR THE OTHER MACHINE.
      *  MJD 061798 CHANGED THE WAY USERS GET TO/RETURN FROM REMOTE
      *             MACHINES. (SEE DESCRIPTION OF POSSIBLE SITUATIONS ABOVE)
      *  MJD 001026 CHANGED TO WRITE TMP FILE TO /USR/PFSI/??/OTHBR.???????
      *             INSTEAD OF /USR/TMP/OTHBR.????????.  WE NEED TO STOP
      *             USING REMOTE1 TO RCP BECAUSE OF SECURITY PROBLEMS.
      *  MJD 001027 REPLACED GPENV WITH ACTUAL READ.
      *  BAH 040727 TEST NEW FLAG BR-ALT-INTO-THIS-BRANCH, SOAUTO PR# 2327
      * BAH 120514 REMOVED SPECIAL-FG
      * JKC 210702 ADDED SQL-DISCONNECT BEFORE 'STOP RUN'; WE FOUND THAT
      *            THE USE OF SQL-DISCONNECT WHEN EXITING THE
      *            APPLICATION IS ONLY NEEDED; DOING A SQL-DISCONNECT
      *            WHEN EXITING A PROGRAM WILL CAUSE ALL CURSORS TO BE
      *            CLOSED FROM THE CALLING PROGRAM WHICH EFFECTS IT 
      *            WHEN CONTINUING WITH A READ-NEXT.
      *
      * JKC 2024-0124 ADDED OTIS TRACE LOGGING LOGIC
      * BAH 2024.0509 MOVE BR-MACHINE TO HOLD-MACHINE IN SU-MODULE, S35Q-2
      * BAH 2024.0612 PUT CODE BACK TO NOT ALLOW YOU TO ALTERNATE BRANCH
      *               INTO ANOTHER MACHINE BRANCH. DAILY PROCESSING MUST
      *               BE ABLE TO CREATE THE OPEN TAG FILE IN THE GLOBAL
      *               DP DIRECTORY UNDER THAT MACHINE! S35Q-66
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/OTHBR.CBL S35 05/20/24-10:45:18 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".

       01  ERRCD           PIC X.
       01  TEST-BUFFER     PIC X(6)    VALUE SPACES.

       01  TO-DATA         PIC X(6).
       01  TO-MACHINE      PIC XX.
       01  FROM-MACHINE    PIC XX.

       01  HOLD-LOGNAME      PIC X(8).

      *-----------------------------------------------------------------
      * TO-LOCATION   : TARGET LOCATION (COULD BE BRNO OR MACHINE NAME)
      * VERIFY-LOCATION : USED FOR TESTING LOCATION FOR NUMERIC/ALPHA
      *-----------------------------------------------------------------
       01  TO-LOCATION.
           03  TO-BRANCH       PIC 9(4).
       01  VERIFY-LOCATION.
           03  VERIFY-BRANCH   PIC 9(4).
       01  FROM-BRANCH         PIC 9(4).
       01  TEST-LOCATION.
           03  TEST-JUNK       PIC XX.
           03  TEST-MACHINE.
               05  TEST-HOST   PIC X.
               05  FILLER      PIC X.

       01  DISP-BUF.
           03  DISP-LOGNAME        PIC X(8).
       01  DISP-LIST               PIC X(32)  VALUE
           "D_LOGNAME.".

      * ---- DUMMY FIELDS USED TO CALL BRSECURE ----
       01  BEG-BRANCH        PIC 9(4).
       01  END-BRANCH        PIC 9(4).
       01  ENT-GROUP         PIC X(4).
       01  GROUP-FLAG        PIC X.
       01  NUM-BRANCH        PIC 9(4).
       01  ALP-BRANCH  REDEFINES NUM-BRANCH.
           03  ALP-BRNO      PIC X  OCCURS 4.
      * --------------------------------------------

       01 WORK-USERID.
          03  WORK-SECURITY        PIC X.
          03  WORK-BR              PIC 9(4).
          03  WORK-NAME            PIC X(5).

       01 HOLD-FIL.
          03  HOLD-XXX             PIC X(3).
          03  HOLD-SLASH1          PIC X.
          03  HOLD-XXXX            PIC X(4).
          03  HOLD-SLASH2          PIC X.
          03  HOLD-MACHINE         PIC X(2).
          03  HOLD-SLASH3          PIC X.
          03  HOLD-DATA-PATH       PIC X(6).

       01 NEW-DIRECTORY            PIC X(22).

       01  IO-TYPE         PIC X(3).

      * RLOGIN
       01 RLOGIN-CMD      PIC X(6)  VALUE "RLOGIN".
       01 USRPFSI-CMD     PIC X(10) VALUE "/USR/PFSI/".
       01 OTHBR-CMD       PIC X(5)  VALUE "OTHBR".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/RLOGINW.CPY".
       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  OTHBR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/OTHBR_DEF.CPY".
       COPY "LIBLP/OTHBR_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/OTHBR_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************************
      *
      *          MAIN PROGRAM MODULE
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           CALL "C$TOLOWER" USING RLOGIN-CMD, VALUE 6.
           CALL "C$TOLOWER" USING USRPFSI-CMD, VALUE 10.
           CALL "C$TOLOWER" USING OTHBR-CMD, VALUE 5.

      * GET FIELDS CREATED BY LP/WHOAMI

      * FOR NOW, JUST SET FLAGS TO "Y" TO TEST PATHS IN A30, BARB 9/20/13
           MOVE "Y" TO RLOGIN-FG
                       RLOGIN-ALLOW-REMOTE
                       RLOGIN-ALLOW-LOCAL.

      * TEST GP-RECORD HERE IF FLAGS IN GP-REC ARE SPACES THEN DO NOT
      * ALLOW SELECTED ACTIVITY.  THIS ALLOWS US TO STOP REMOVE LOGINS
      * ON THE FLY BY CHANGING THE GP-FLAGS.  EVEN IF THE USERS HAVE
      * NOT REFRESHED THEIR ENVIRONMENT.

           PERFORM GET-GP.
           IF IO-BAD
              GO TO END-ROUTINE.

      * VALUE OF SPACE IN GP SUPERCEDES ANY VALUE FROM ABOVE FLDVAL.

           IF GP-RLOGIN-LOCAL-PROFILE = SPACE
              MOVE "N" TO RLOGIN-ALLOW-LOCAL.
           IF GP-RLOGIN-REMOTE-PROFILE = SPACE
              MOVE "N" TO RLOGIN-ALLOW-REMOTE.

      * THIS LOGIC WAS INSTALLED SO THAT PARADATA CAN TEST REMOTE
      * AND ALTERNATE LOGINS WHILE LOCKING OUT ALL OTHER USERS.

           MOVE SPACES TO TEST-BUFFER.
           ACCEPT TEST-BUFFER FROM ENVIRONMENT "TSTOTHBR".
           IF TEST-BUFFER = "PFSIMI"
              MOVE "Y" TO RLOGIN-FG
                          RLOGIN-ALLOW-LOCAL
                          RLOGIN-ALLOW-REMOTE.
      *

           IF RLOGIN-FG NOT = "Y"
              GO TO END-ROUTINE.

           IF RLOGIN-FG = "Y"
              IF RLOGIN-ALLOW-REMOTE = "N"
                 AND RLOGIN-ALLOW-LOCAL = "N"
              GO TO END-ROUTINE.

      * MENU MAY BE LOCKED OUT VIA PASSWORD

           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO END-ROUTINE.

           PERFORM DISPLAY-FORM.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO HOLD-LOGNAME.

      * TEST "FROM" BRANCH
           PERFORM UNAME-CALL.
           MOVE UNAME-BR TO BR-NO FROM-BRANCH.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           MOVE  BR-MACHINE TO FROM-MACHINE.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE "SNNN040CUR-BR." TO VDU.
           MOVE FROM-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM REC-BA-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           GO TO MAIN-MODULE.

      *****************************************************************
       REC-BA-MODULE SECTION.
      *****************************************************************

           MOVE SPACES TO IO-TYPE.

       REC-BA-BRANCH.
           PERFORM SETUP-INFO-AREA.
           MOVE "E RA040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BA-MODULE.

      * CALL GROUP SCAN FOR INFO ONLY. MUST ENTER ONE NUMERIC BRANCH.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX.

           IF VDUSTATUS NOT = "00"
              GO TO REC-BA-BRANCH.

       REC-BA-BRANCH-SKIP-INPUT.

           MOVE VDUBUF TO VERIFY-LOCATION
                          TEST-LOCATION
                          TO-LOCATION.

           INSPECT VERIFY-LOCATION REPLACING LEADING " " BY "0".
           IF VERIFY-LOCATION NOT NUMERIC
              GO TO REC-BA-BRANCH.

      * NOT SURE WHAT YOU WOULD ENTER HERE BESIDE A VALIDE NUMERIC BRANCH
      * COMMENTED IT OUT 6/12/2024 BAH
      *       IF RLOGIN-ALLOW-REMOTE = "Y"
      *          IF TEST-JUNK NOT = SPACES
      *             MOVE "INVALID MACHINE NAME: &&&&" TO MESS
      *             INSPECT MESS REPLACING FIRST "&&&&"
      *                  BY TEST-LOCATION
      *             PERFORM SEND-MESS
      *             GO TO REC-BA-BRANCH
      *          END-IF
      *          MOVE FROM-MACHINE TO TEST-MACHINE
      *          MOVE RLOGIN-HOST TO TEST-HOST
      *          IF TEST-LOCATION = TO-LOCATION
      *             MOVE "YOU ARE ALREADY ON MACHINE &&" TO MESS
      *             INSPECT MESS REPLACING FIRST "&&" BY TEST-MACHINE
      *             PERFORM SEND-MESS
      *             GO TO REC-BA-BRANCH
      *          END-IF
      *          PERFORM RLOGIN-MODULE
      *       ELSE
      *          IF RLOGIN-IS-REMOTE = "Y"
      *             GO TO ALTERNATE-END-ROUTINE
      *          ELSE
      *             MOVE "ACCESS TO ANOTHER MACHINE IS NOT PERMITTED"
      *                                                       TO MESS
      *             PERFORM SEND-MESS
      *          END-IF
      *       END-IF
      *       GO TO END-REC-BA-MODULE.

           MOVE VERIFY-BRANCH TO TO-BRANCH.

      * TEST FOR TO-BRANCH SAME AS FROM-BRANCH

           IF TO-BRANCH = FROM-BRANCH
              MOVE "ALREADY LOGGED IN TO BRANCH &&&&" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY FROM-BRANCH
              PERFORM SEND-MESS
              GO TO REC-BA-BRANCH.

      * TEST SECURITY:
           MOVE TO-BRANCH TO BEG-BRANCH
                             END-BRANCH
                             NUM-BRANCH
                             BR-NO.

      * READ BR-FILE TO CONFIRM TO-BRANCH EXISTS
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "BRANCH NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO REC-BA-BRANCH.

      * TEST IF OTHER BRANCHES ARE ALLOWED INTO THIS BRANCH
           IF BR-ALT-INTO-THIS-BRANCH = "N"
              MOVE "CAN NOT ALTERNATE INTO THIS BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO REC-BA-BRANCH.

      * TEST BRANCH SECURITY

           PERFORM BRANCH-TO-ALPHA.
           MOVE ALP-BRANCH TO WGR-WORD ENT-GROUP.

           MOVE "N" TO GROUP-FLAG.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO REC-BA-BRANCH.

      * SETUP "TO" PATH.
           MOVE BR-DATA-PATH TO TO-DATA.
           MOVE BR-MACHINE   TO TO-MACHINE.
 
           IF TO-MACHINE NOT = FROM-MACHINE
              MOVE "ACCESS TO ANOTHER MACHINE IS NOT PERMITTED"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO REC-BA-BRANCH.

           IF RLOGIN-ALLOW-LOCAL = "Y"
              PERFORM SU-MODULE
           ELSE
              MOVE "ACCESS TO ANOTHER BRANCH IS NOT PERMITTED" TO MESS
              PERFORM SEND-MESS.

       END-REC-BA-MODULE.
           EXIT.

      *****************************************************************
       SU-MODULE SECTION.
      *****************************************************************

      * IF A0201MGR WANTS TO LOGIN TO BR 146 ON SAME MACHINE:
      *  CHANGE 'POSTNAME' TO NEW BRANCH
      *  CHANGE 'FIL' TO NEW BRANCH


           MOVE EXT-CONAME-USER-ID TO WORK-USERID.
           MOVE TO-BRANCH          TO WORK-BR.
           MOVE EXT-FILPATH-FIL    TO HOLD-FIL.
           MOVE BR-DATA-PATH       TO HOLD-DATA-PATH.
           MOVE BR-MACHINE         TO HOLD-MACHINE.

      *5/1/2019 BAH I CHANGED THIS TO ONLY LOOK FOR THE BRANCH DIRECTORY
      *         WITH GLOBAL FILES, WE MAY NOT NEED GB DIRECTORY

           STRING "/" HOLD-FIL DELIMITED BY " " INTO NEW-DIRECTORY.
           MOVE NEW-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "DATA PATH &&&&&&&&&&&&&&&&&&&&&& DOES NOT EXIST"
                              TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&&&&&&&&&&&" BY
                              NEW-DIRECTORY
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-SU-MODULE.

      * OTIS TRACE LOGGING
           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE FORM-PATHNAME         TO EXT-TCLP-TRACE-FORM-PATH
              MOVE "OTHBR"               TO EXT-TCLP-TRACE-FORM-PROGX
              STRING "ALTERNATE BRANCH | ", TO-BRANCH
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              PERFORM TRACE-SEND-MESS.

      * SET ENVIRONMENT VARIABLE FOR CHANGES NEEDED
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           SET ENVIRONMENT "FIL"      TO HOLD-FIL.

      * THIS REPLACES PHYSICALLY CHANGING DIRECTORY INTO NEW BRANCH
           MOVE HOLD-FIL              TO EXT-FILPATH-FIL.

      * MUST SET UP NEW ENVIRONMENT FOR NEW POSTING BRANCH!
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH.
           CANCEL FORM-PROGX.

           GO TO END-ROUTINE.

       EXIT-SU-MODULE.
            EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "OTHBR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO USE REMOTE BRANCH LOGIN" TO PASSWDW-PROMPT2
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************************
      * BRANCH TO ALPHA CONVERSION
      *   CONVERTS "0102" TO "102 "
      *******************************************************
       BRANCH-TO-ALPHA SECTION.
           INSPECT ALP-BRANCH REPLACING LEADING "0" BY " ".
       ALP-SHIFT.
           IF ALP-BRNO (1) NOT = " " OR ALP-BRANCH = SPACES
              GO TO EXIT-BRANCH-TO-ALPHA.

           MOVE ALP-BRNO(2) TO ALP-BRNO(1).
           MOVE ALP-BRNO(3) TO ALP-BRNO(2).
           MOVE ALP-BRNO(4) TO ALP-BRNO(3).
           MOVE " " TO ALP-BRNO(4).

           IF ALP-BRNO (1) = " "
              GO TO ALP-SHIFT.

       EXIT-BRANCH-TO-ALPHA.
           EXIT.

      ******************************************************************
       SETUP-INFO-AREA SECTION.
      ******************************************************************
           MOVE HOLD-LOGNAME      TO DISP-LOGNAME.

           MOVE DISP-LIST TO WR-LIST.
           MOVE DISP-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       EXIT-SETUP-INFO-AREA.
           EXIT.

      **************************************************
       DISPLAY-FORM SECTION.
      **************************************************

           MOVE "OTHBR" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS OTHBR-WINDOW-HANDLE.
       COPY "LIBGB/CHKSEC.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO OTHBR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO OTHBR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY OTHBR-SCREEN UPON OTHBR-WINDOW-HANDLE.
           MOVE OTHBR-FORM-FIELDS TO WR-BUF.
           MOVE OTHBR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE OTHBR-HELP--FILE TO HELP-LINK-FILE.
           MOVE OTHBR-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/OTHBR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/DACCESS.CPY".

      ******************************************************************
      *      THIS PROGRAM MUST HAVE ACTUAL READ OF THE GP-FILE
      *
      *             D O    N O T    U S E    G P E N V ! ! !
      ******************************************************************
       GET-GP.
           MOVE 1 TO GP-KEY.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGPRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *************************************************
       END-ROUTINE SECTION.
      *************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY OTHBR-SCREEN.
           DESTROY OTHBR-WINDOW-HANDLE.
           EXIT PROGRAM.
