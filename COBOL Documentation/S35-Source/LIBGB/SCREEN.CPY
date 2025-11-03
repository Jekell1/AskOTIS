      * COPYMEMBER SCREEN
      ******************************************************************
      * SCREEN HANDLER SECTION
      * REV
      * BLM 111227 STARRED OFF EXECUTION OF UT/NONDSP IN PARAGRAPH
      *            VDU-NONDISPLAY-FIELD
      * BLM 120523 STARRED OFF HELP STUFF FOR NOW UNTIL WE FIGURE IT OUT
      * MJD 120529 FIXED ISSUE WITH RJUS FLAG IN THE FORM.
      *            IF THE OLD FORM HAD RJUS = "Y" THEN MAKE SURE
      *            CONTENTS OF VDU-BUF MATCH.
      * MJD 121004 CHANGED TO MAKE ACCEPT READ NUMERIC OF CLASS = N OR S.
      *            WILL USE VDUNUMX BASED ON DEC SICE OF 0-5
      * MJD 121008 FIXED PROBLEM I CREATED WITH THE PREVIOUS CHANGE.
      *            THE SYSTEM WILL NOW PROPERLY DISTINGUISH BETWEEN
      *            SIGNED AND UNSIGNED NUMBERS.
      * MJD 121010 COMMENTED OUT CALCULATION OF VDU-BUFFER-LENGTH AND
      *            REPLACED WITH VDU-SIZEX.
      *            CALCULATION WAS MISCOUNTING ON SIGNED NUMERICS AND NOT
      *            SURE WE NEED IT AS LONG AS WE ALREADY KNOW THE LENGTH
      *            OF THE BUFFER FROM VDU-SIZEX
      * MJD 121018 COMMENTED OUT GROUP 7 PROMPT LOGIC AS A TEST
      *            IT IS FILLING TRAILING "_" ON REDISPLAY OF FIELDS.
      * MJD 121022 MADE CHANGES TO DATE ENTRY.   IF NO "/" ARE DETECTED
      *            IGNORE THE LAST 2 DIGITS, ONLY PROCESS THE FIRST 6.
      *            MADE THE SAME CHANGES RESPECTIVELY FOR DATE8 AND SSNO ENTRY.
      * MJD 121031 REMOVED "ECHO" FROM ACCEPT STATEMENT, IT WAS
      *            OVERRIDDING WHAT WAS IN THE CONTROL STRING.
      *            INCORPORATED SOME OF THE FORM ATTR INTO THE VDU-CONTROL
      *            DO NOT REDISPLAY ACCEPTED FIELD IF NO ECHO IS SET TO YES.
      * MJD 121218 ADDED VDU-FIELD-PROMPT TO ADJUST ATTRIBUTES BASED ON
      *            PREFERENCES ON INPUT OF OF A FIELD.
      * MJD 121219 WORKING ON HELP.....
      * BAH 130110 REMOVED FORM-REFRESH AFTER WIMENU
      * MJD 130115 REMOVED ALL CALLS TO FORM-REFRESH.   GOING FORWARD WE SHOULD
      *            BE CALLING A WINDOW WHICH WILL REFRESH SCREEN AFTER IT
      *            CLOSES OR WE WILL BE REWRITING THE CURRENT SCREEN/WINDOW.
      *            REMOVED CALL-GETFM/CALL-DLFORM/CALL-CRFORM
      * MJD 130122 CHANGED CALL TMMSG TO USE SIMILAR LOGIC AS CALL-TMMSG.
      * MJD 120322 REMOVED THE DEFAULT CONVERSION OF TEXT TO UPPER CASE.
      * MJD 130605 PUT RESET-MESS HERE INSTEAD OF DECLARATIVES
      * BAH 130626 ADDED INSPECT AND C$JUSTIFY SO ISCOBOL WILL LEFT
      *            JUSTIFY NUMERICS LIKE ACU DOES
      *  CS 130813 ADDED CTRL-R TO VDU-SET-STATUS, ADDED TEST FOR VDU-CTRL-
      *            R-PROGRAM.  THE STAT IN SET-VDU-SET-STATUS IS CROSS-
      *            REFERENCED IN /USR/A30/SH/XCBLCONG FOR EXAMPLE
      *             KEYSTROKE EXCEPTION=17 ^R
      * BLM 130821 CHANGE VDU-SEND-TMMSG TO USE "DISPLAY MESSAGE BOX"; THE
      *            OLD ROUTINE DISPLAYED ON LINE 24 & MESSED UP SMALLER
      *            WINDOWS
      * MJD 130905 REMOVED OUT CALL-FLDVAL.  NOT USED WITH NEW SCREEN HANDLER
      *            SEE LIBGB/SV/SCREEN.130905 FOR REFERNCE TO OLD CODE IF
      *            NEEDED.
      * BLM 131014 CHANGED WR-SIZE TO WR-SIZEX, CONFLICTED WITH WR-SIZE
      *            USED FOR WRFILE
      * BAH 131025 ADDED EXIT-PATHNAME TO THE CALL OF WIMENU OR GNMENU GAVE
      *            ME LINKAGE ERROR
      * MJD 131105 ADDED EXIT-PATHNAME TO CALL OF PWDALY
      * BLM 131120 ADDED UPDATE OF WORKER VDU-ZERO-OR-RETURN, IF
      *            ENTERING A NUMERIC FIELD & USER HITS RETURN KEY
      *            WORKER WILL HAVE A VALUE OF "R"; ATTRIBUTES, RESET
      *            HAVE AN EFFECT, SEE XONPC0 FOR USAGE
      *            NOTE, YOU NORMALLY CAN JUST GIVE A FIELD THE
      *            ATTRIBUTE OF "MUST ENTER", BUT I COULD NOT DO THIS
      *            SINCE THE FIELD WAS "SEL" IN LOAN PROCESSING
      * MJD 131126 ADDED "E" ECHO ATTRIBUTE TO OVERRIDE THE FORM ATTRIBUTE.
      *            WHEN TRYING TO CLEAR PASSWORD ENTRY FIELD IN CAMAIN THE
      *            PROMPT WOULD NOT CLEAR WHEN MOVING SPACES TO WR-BUF AN
      *            PERFORMING CALL-WRFORM BECAUSE THE DEFAULT FORM ATTRIBUTE
      *            WAS SET TO NO ECHO (SECURE)
      * BLM 140106 FIX PROBLEM WITH 8 DIGIT DATE ROUTINE, IF YOU ENTERED
      *            01012014, IT DISPLAYED 01/01/201
      * MJD 140612 ADDED EXIT-PATHNAME TO CALLS
      * MJD 140722 CHANGED "ACCEPT VDU-BUF-AREA" FOR ALPHA ENTRY TO BE 
      *            "ACCEPT VDU-BUF-AREA(1:VDU-SIZE)"  FOR SOME REASON IN
      *            VERYANT IT WAS USING THE ENTIRE BUFFER LENGTH AND
      *            WIPING OUT VALUES ELSEWHERE ION THE SCREEN I.E. LNAGED
      *            WHERE WE HAVE MULTIPLE COLUMNS OF DATA ENTRY.
      * MJD 140731 CHANGED TO IGNORE THE "MUST" FLAG COMING FROM FORM ATTRIBUTES
      * MJD 140807 ADDED VDU-NUMX THRU VDU-NUM5X AND VDUNUM THROUGH VDUNUM5X
      *            THESE ARE NEW REDEFINES OF VDU-NUM AND USED IN THE
      *            ACCEPT STATEMENTS SO THAT WE DO NOT DISPLAY "0" AS
      *            INITIAL VALUES WHEN ENTYERING NUMERICS.
      * BLM 140819 ADDED CANCELS TO F9 & F10 INTERRUPTS; PROGRAMS WERE
      *            STAYING IN MEMROY.  CHANGED IN A15 09/08, NEVER GOT
      *            CHANGED IN A30
      * MJD 140919 ADDED LOGIC TO PREVENT SIGNED NUMERIC FIELDS FROM 
      *            LOSING TRAILING SPACE IF NON-NEGATIVE WHEN WE DO THE RIGHT
      *            JUSTIFY ON DISPLAY.
      * MJD 141002 IF ATTRIBUTE(3) "BOLD" COMES IN AS A "R" MAKE THE FIELD
      *            "BOLD" AND "REVERSE" VIDEO.  THIS WILL BE SET IN SCRN1
      *            IF THE PROGRAM IS A SCAN (INCL PG/CL/WIMENU) AND THE FIELD
      *            IS TITLE/TITLEX/WSEL/LEGEND (NOTE OPTIONS SPECIFIED AT THE 
      *            TIME SCREENIO OR WRFORM IS CALLED MAY OVERRIDE THIS)
      * MJD 161111 ATTEMPTING TO FIX A PROBLEM THAT IS CAUSING A REFERENCE
      *             MODIFIER ERROR IN THE ACU_TCLP LOG. THIS IS CAUSED THE
      *             THE SECOND SUBSCRIPT OF THE FOLLOWING IS ZERO:   
      *             MOVE ABC(X:Y) TO XXXX.    IF 'Y' IS ZERO AN ERROR IS
      *             LOGGED.  I ADDED A FEW TESTS TO ENSURE THAT VDU-SIZE,
      *             VDU-CP AND VDU-SIZEX NEVER FALL BELOW '1' (ONE).   THERE
      *             IS NO REASON FOR THE SIZE OR CURSOR POSITION TO BE ZERO
      *             OR LESS BUT THE SITUATION IS OCCURING SOMEHOW.  
      *             HOPEFULLY THIS TEST WILL PREVENT IT DESPITE NOT DIRECTLY
      *             IDENTIFYING HOW THE SITUATION IS OCCURING.
      * MJD 161117 STILL ATTEMPTING TO CORRECT THE REFERENCE MODIFIER ERROR
      *            IN ACU_TCLP.  FOUND THAT THE VDU-ACTUAL-LENGTH WORKER
      *            WAS NETTING TO ZERO IF YOU JUST HIT A FUNCTION KEY LIKE F2
      *            TO EXIT THE SCREEN.   CORRECTED TO SKIP THE MOVE OF LENGTH
      *            IS ZERO.
      * BAH 161228 8 DIGIT DATE ENTRY WAS NOT ALLOW ANY FUNCTION KEYS NOR
      *            COULD YOU RETURN THROUGH THE DATE FIELD WITH 0
      * BLM 170309 DATE ENTRY WILL NOW RETURN 8 DIGIT DATE VDU-YYYYMMDD
      *            IN ADDITION TO RETURNING VDUNUM0 (MMDDYY), PD0006
      * MJD 170315 REMOVED FIELD LENGTH FROM ACCEPT STATEMENTS IN AN
      *            ATTEMPT TO ELIMINATE THE REFERENCE MODIFIER ERROR
      *            WE ARE GETTING IN THE ERROR LOG. THIS SHOULD FORCE
      *            THE SUBSTRING TO END AT THE END OF THE STRING RATHER
      *            THAN FORCE A LENGTH OF 18.
      * BLM 170323 8 DIGIT DATE ENTRY WILL NOW RETURN 8 DIGIT DATE
      *            VDU-YYYYMMDD IN ADDITION TO RETURNING VDUNUM0(MMDDYYYY)
      *            PD0006
      * BLM 170424 ADD VDUCLASS OF "8", SO MOVING 8 DIGIT DATE TO
      *            VDU-DATE-YYYYMMDD & PERFORM SCREEN WITH VDU =
      *            "S  8000FIELD." WILL CONVERT DATE TO MM/DD/YY
      * BAH 170427 ISSUE WITH PRESSING RETURN OR ENTERING 0 IN A DATE FIELD
      *            IN MAINT. VDU-DATE-YYYYMMDD IS A STAND ALONE FIELD WITH
      *            INITIAL VALUE OF 0, BUT NEVER SET TO ZERO IF PROGRAM DOES
      *            NOT EXIT BEFORE USING IT AGAIN, AND THIS ROUTINE ONLY
      *            FORMATTED VDU-DATE-YYYYMMDD IF IT WAS NOT 0, IF IT WAS 0,
      *            IT CONTAINED AN OLD VALUE! CHANGED THE "IF" STATEMENT TO
      *            CHECK FOR 0 FIRST, OTHERWISE SET IT.
      *
      * JKC 191107 FOUND THAT THE ESCAPE-KEY ACTS LIKE AN ENTER-KEY.
      *            IT ALSO CAUSE SOME DISTORTIONS IN THE SCREEN.
      *            IT WAS DECIDED TO BLOCK ITS USAGE.
      *            IN THE VDU-SET-STATUS SECTION, ADDED:
      *               WHEN 27 MOVE "1E" TO STAT
      *            IN THE VDU-RECEIVE-ROUTINE SECTION, AFTER THE
      *            'PERFORM VDU-READ-FIELD' ADDED:
      *               IF STAT = "1E"   ESCAPE-KEY; DO NOT ALLOW
      *                  MOVE VDU TO HOLD-VDU
      *                  MOVE HOLD-VDU TO VDU
      *                  GO TO VDU-RECEIVE-ROUTINE.
      *
      * JKC 230922 CREATED VDU-SHIFT-MUST SECTION FOR CONTROL OF FORM
      *            FIELD CASE SENSITIVITY WITH SQL IO OPERATIONS.
      *            THE USE OF THE FORM FIELD 'MUST' ATTRIBUTE WILL
      *            CONTROL THE USER'S FIELD ENTRY. IF 'MUST' ATTRIBUTE
      *            EQUALS "Y" ALLOWS LOWERCASE ENTRY OTHERWISE "N" WILL
      *            FORCE THE USER'S ENTRY TO UPPERCASE.
      *
      * BAH 2024.0223 FIXED A ACU_TCLP ERROR;
      *               "REFERENCE MODIFIER RANGE ERROR; START = 1, LENGTH = 0".
      *     THERE WAS A TEST FOR VDU-SIZEX < 1 BUT IT WAS SPACES, ADDED OR NOT
      *     NUMERIC IN 3 PLACES, ONE WAS ON VDU-SIZE, 2 ON VDU-SIZEX.
      *     HAPPENING IN CLOSE DAY TRYING TO SEND PROGRESS-MSG IN ALL THE
      *     PROGRAMS AND IT DOESNT HAVE A SCREEN TO DISPLAY THE MESSAGE ON,
      *     SO SOME OF THOSE VALUES ARE SPACES.
      * BAH 2024.0404 CHANGE VDU-FIELD-PROMPT TO VDU-FIELD-PROMPT AND
      *                ADDED TO SCREENW
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      ******************************************************************
       SCREENIO SECTION.
       SCREEN-HANDLER.

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE "00" TO VDUSTATUS
              GO TO VDU-REAL-EXIT.          *> SKIP SCREEN INTERACTIONS

           MOVE SPACES TO VDU-BUF-AREA VDU-STOR-BUF
                          VDU-DISP-BUF VDU-INIT-FIELD.
           CALL "C$TOUPPER" USING VDUPROCESS, VALUE 20.
           CALL "C$TOUPPER" USING VDU-ATTRIBUTES, VALUE  8.
           CALL "C$TOUPPER" USING VDUFIELD, VALUE  13.
           IF VDU-ATTRIBUTES = SPACES
              PERFORM VDU-EXTRACT-ATTRIBUTES.

           IF VDUACTION = "E" OR "R"
              IF VDU-ATTRIBUTES = SPACES
                 MOVE VDU-FIELD-PROMPT TO VDU-ATTRIBUTES
              ELSE
                 MOVE SPACES TO VDU-GARBAGE
                 STRING VDU-FIELD-PROMPT VDU-ATTRIBUTES
                        DELIMITED BY " " INTO VDU-GARBAGE
                 MOVE VDU-GARBAGE TO VDU-ATTRIBUTES.

           MOVE 1 TO VDU-NEG-FLAG.

           MOVE VDUFIELD TO VDU-FIELD-TO-CONVERT VDU-NM-LIST.
           PERFORM VDU-CONVERT-FIELD.
           IF STAT NOT = "00"
              GO TO VDU-INVALID-FIELD.
      * 2/13/2024 ADDED OR NOT NUMERIC
           IF VDU-SIZEX < 1 OR VDU-SIZEX NOT NUMERIC
              MOVE 1 TO VDU-SIZEX.
           MOVE VDU-SIZEX TO VDU-SIZE.
           MOVE VDU-POSX  TO VDU-POS.
      *    INITIALIZE VDU-STOR-BUF.
           MOVE SPACES TO VDU-STOR-BUF.
           ACCEPT VDU-STOR-BUF
              FROM SCREEN
              AT VDU-POS
              SIZE VDU-SIZE.

      *    MODIFY CONTENTS OF A FIELD
           IF VDUACTION = "M"
              MOVE "S" TO VDUACTION
              MOVE " " TO VDUPUNC
                          VDURESET
              MOVE "A" TO VDUCLASS
              MOVE VDU-STOR-BUF TO VDUBUF.

           INSPECT VDU-STOR-BUF REPLACING LEADING "_" BY " ".
           MOVE VDU-STOR-BUF TO VDU-INIT-FIELD

           IF VDUACTION NOT = "S"
              GO TO VDU-RECEIVE-ROUTINE-CLR.
           IF VDUACTION = "S" AND VDUCLASS = "8"
              MOVE VDU-YYYYMMDD-MM TO VDU-DATE-EDIT-MM
              MOVE VDU-YYYYMMDD-DD TO VDU-DATE-EDIT-DD
              MOVE VDU-YYYYMMDD-YY TO VDU-DATE-EDIT-YY
              MOVE "A" TO VDUCLASS.
           IF VDUCLASS = "A" OR "X"
              MOVE VDUBUFFER TO VDU-BUF-AREA
              IF VDUCLASS = "X"
                 INSPECT VDU-BUF-AREA REPLACING ALL "/" BY "-"
              END-IF
              GO TO VDU-MOVE-FIELD.
           PERFORM VDU-NUMERIC-ROUTINE.
           GO TO VDU-TEST-SIZE.
       VDU-RECEIVE-ROUTINE-CLR.
           MOVE 0 TO VDU-PROGRAMMING.
       VDU-RECEIVE-ROUTINE.
           MOVE SPACES TO VDUBUFFER VDU-BUF-AREA.
      *    ---------------------------------------------------------------------
      *    DECIDE HERE IF WE ARE TO REDISPLY THE FIELDS CURRENT VALUE OR
      *    RESET THE FIELD.
      *    ---------------------------------------------------------------------
           IF VDUCLASS = "A"
              IF VDURESET = "R"
                 PERFORM RESET-VDU-NM-LIST
              ELSE
                 MOVE VDU-INIT-FIELD TO VDU-DEFAULT.
           IF VDUCLASS NOT = "A"
              IF VDURESET = "N"
                 MOVE VDU-INIT-FIELD TO VDU-DEFAULT
              ELSE
                 PERFORM RESET-VDU-NM-LIST.
           PERFORM VDU-READ-FIELD.
           MOVE STAT TO VDUSTATUS.

      * ESCAPE KEY, DO NOT ALLOW
           IF STAT = "1E" 
              MOVE VDU TO HOLD-VDU
              MOVE HOLD-VDU TO VDU
              GO TO VDU-RECEIVE-ROUTINE.

           IF STAT = "1 "
              IF HELP-LINK-FILE = SPACES
                 MOVE VDU-FORM-NAME TO HELP-LINK-FILE
              END-IF
              MOVE VDUFIELD TO HELP-LINK-FIELD
              INSPECT HELP-LINK-FIELD REPLACING ALL "." BY " "
              PERFORM SETUP-LINK-INFO
              MOVE "GB/HELP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATHNAME HELP-LINK-BUF
                                   EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO SCREEN-HANDLER
           END-IF.

           IF VDU-WIMENU-SELECTION NOT = " "
              IF STAT = "1)"
      *--------->PERFORM SEND-NO-MAIL-MESSAGE
                 MOVE "CL/WIMENU" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATHNAME
                                      VDU-WIMENU-SELECTION
                                      EXIT-PATHNAME
                 CANCEL FORM-PROGX
      *--------->PERFORM SEND-MAIL-MESSAGE
                 MOVE VDU TO HOLD-VDU
      * THE REFRESH WAS WIPING OUT THE PARAMETERS ON MY CURRENT SCREEN THAT
      * I HAD ALREADY ENTERED! WIMENU SHOULD TAKE ME RIGHT BACK TO WHERE I
      * WAS AND NOT MESS WITH MY CURRENT SCREEN!
      * BAH      PERFORM FORM-REFRESH
                 MOVE HOLD-VDU TO VDU
                 GO TO VDU-RECEIVE-ROUTINE.

           IF VDU-CLMENU-SELECTION NOT = " "
              IF STAT = "1("
                 MOVE "CL/CLMENU" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATHNAME
                                      VDU-CLMENU-SELECTION
                                      EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 MOVE VDU TO HOLD-VDU
      * FOR SAME REASON AS ABOVE WITH WIMENU
      * BAH      PERFORM FORM-REFRESH
                 MOVE HOLD-VDU TO VDU
                 GO TO VDU-RECEIVE-ROUTINE.

      * TEST FOR CTRL 'L' INTERRUPT:
      *    IF STAT = "^L"
      *       MOVE VDU TO HOLD-VDU
      *MJD    PERFORM FORM-REFRESH
      *       MOVE HOLD-VDU TO VDU
      *       GO TO SCREEN-HANDLER.

      * TEST FOR CTRL 'N' INTERRUPT:
           IF VDU-CTRL-N-PROG NOT = " "
              IF STAT = "^N"
                 MOVE VDU-CTRL-N-PROG TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATHNAME
                                      EXIT-PATHNAME
                                      VDU-CLMENU-SELECTION
                 MOVE VDU-CTRL-N-PROG TO FORM-NAM
                 CANCEL FORM-PROGX
                 MOVE VDU TO HOLD-VDU
      *MJD       PERFORM FORM-REFRESH
                 MOVE HOLD-VDU TO VDU
                 GO TO VDU-RECEIVE-ROUTINE.

      * TEST FOR CTRL 'B' INTERRUPT:
           IF VDU-CTRL-B-PROG NOT = " "
              IF STAT = "^B"
                 MOVE VDU-CTRL-B-PROG TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATHNAME
                                      VDU-CLMENU-SELECTION
                                      VDU-CTRL-B-BUF
                                      EXIT-PATHNAME
                 MOVE VDU-CTRL-B-PROG TO FORM-NAM
                 CANCEL FORM-PROGX
                 MOVE VDU TO HOLD-VDU
      *MJD       PERFORM FORM-REFRESH
                 MOVE HOLD-VDU TO VDU
                 GO TO VDU-RECEIVE-ROUTINE.


      * TEST FOR CTRL 'R' INTERRUPT:
           IF VDU-CTRL-R-PROG NOT = " "
              IF STAT = "^R"
                 MOVE VDU-CTRL-R-PROG TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATHNAME
                                      VDU-CLMENU-SELECTION
                                      EXIT-PATHNAME
                 MOVE VDU-CTRL-R-PROG TO FORM-NAM
                 CANCEL FORM-PROGX
                 MOVE VDU TO HOLD-VDU
                 MOVE HOLD-VDU TO VDU
                 GO TO VDU-RECEIVE-ROUTINE.

           IF STAT = "1D"
              IF VDU-PROGRAMMING = 1 OR 5 OR 9
                 ADD 4 TO VDU-PROGRAMMING
                 GO TO VDU-RECEIVE-ROUTINE.

           IF STAT = "1U"
              IF VDU-PROGRAMMING = 1 OR 2
                 ADD 1 TO VDU-PROGRAMMING
                 GO TO VDU-RECEIVE-ROUTINE.

      *-----------------------------------------------------
      *    TEST FOR ONLINE (F11) HELP EDITOR,
      *    VIA HELPSH= ENVIRONMENT VARIABLE:
      *-----------------------------------------------------
      *    IF STAT = "1!"
      *       MOVE FORM-PATHNAME TO FORM-PATH
      *       MOVE FORM-NAM TO VDU-HELP-FORM-NAM
      *       MOVE "CO" TO FORM-OBJ
      *       MOVE "CL/HELPSH" TO FORM-NAM
      *       CALL FORM-PROGX USING FORM-PATHNAME
      *                             VDU-HELP-FORM-NAM
      *                              VDUFIELD
      *       CANCEL FORM-PROGX
      *       GO TO VDU-RECEIVE-ROUTINE.
      *    IF VDU-F11MENU-SELECTION NOT = " "
      *       IF STAT = "1!"
      *          ACCEPT VDU-F11MENU-PROG FROM ENVIRONMENT "F11PROG"
      *             IF VDU-F11MENU-PROG NOT = " "
      *                IF VDU-F11MENU-PROG = "CL/HELPSH"
      *                   MOVE FORM-PATHNAME TO FORM-PATH
      *                   MOVE FORM-NAM TO VDU-HELP-FORM-NAM
      *                   MOVE "CO" TO FORM-OBJ
      *                   MOVE "CL/HELPSH" TO FORM-NAM
      *                   DISPLAY WINDOW POP-UP AREA VDU-POP-UP-WINDOW
      *                   CALL FORM-PROGX USING FORM-PATHNAME
      *                                        VDU-HELP-FORM-NAM
      *                                        VDUFIELD
      *                   CANCEL FORM-PROGX
      *                   CLOSE WINDOW VDU-POP-UP-WINDOW
      *                   MOVE VDU TO HOLD-VDU
      *MJD                PERFORM FORM-REFRESH
      *                   MOVE HOLD-VDU TO VDU
      *                   GO TO VDU-RECEIVE-ROUTINE
      *                ELSE
      *                   MOVE VDU-F11MENU-PROG TO FORM-NAM
      *                   CALL FORM-PROGX USING FORM-PATH
      *                                        VDU-F11MENU-SELECTION
      *                   MOVE VDU TO HOLD-VDU
      *MJD                PERFORM FORM-REFRESH
      *                   MOVE HOLD-VDU TO VDU
      *                   GO TO VDU-RECEIVE-ROUTINE.

           IF STAT = "1#"
              IF VDU-PROGRAMMING < 1
                 ADD 1 TO VDU-PROGRAMMING
                 GO TO VDU-RECEIVE-ROUTINE
              ELSE
                 IF VDU-PGMENU-SELECTION NOT = " "
                    IF VDU-PROGRAMMING = 3
                       MOVE "CL/PGMENU" TO FORM-NAM
                       CALL FORM-PROGX USING FORM-PATHNAME
                                         VDU-PGMENU-SELECTION
                                            EXIT-PATHNAME
                       CANCEL FORM-PROGX
                       MOVE VDU TO HOLD-VDU
      * 1/3/2014 HAD TO REMOVE THE RESET FOR DAILY PROCESSING TO COME BACK
      *          TO A CLEAN SCREEN! SHOULDNT HAVE TO RESET FORM, UNDERLYING
      *          FORM DOESNT CHANGE!
      *                PERFORM FORM-RESET
                       MOVE HOLD-VDU TO VDU
                    END-IF
                 END-IF
                 IF VDU-PROGRAMMING = 13
                    MOVE "CL/PWDALY" TO FORM-NAM
                    CALL FORM-PROGX USING FORM-PATHNAME
                                         EXIT-PATHNAME
                                         VDU-PGMENU-SELECTION
                          MOVE VDU TO HOLD-VDU
      *MJD                PERFORM FORM-REFRESH
                          MOVE HOLD-VDU TO VDU
                 END-IF
                 GO TO SCREEN-HANDLER.
      *          GO TO VDU-RECEIVE-ROUTINE-CLR.

           IF STAT =  "1D" OR STAT = "1U"
              MOVE VDU-STOR-BUF TO VDU-BUF-AREA.
           IF VDUCLASS =  "A"
              INSPECT VDU-BUF-AREA REPLACING LEADING "_" BY " "
              MOVE VDU-BUF-AREA TO VDUBUFFER
              GO TO VDU-MOVE-FIELD.
           IF VDUBUF = SPACES
              MOVE "R" TO VDU-ZERO-OR-RETURN
           ELSE
              MOVE " " TO VDU-ZERO-OR-RETURN.
           INSPECT VDU-BUF-AREA REPLACING LEADING " " BY "0".
           INSPECT VDU-BUF-AREA REPLACING LEADING "_" BY "0".
           IF VDUCLASS = "M"
              GO TO VDU-MOVE-DBUFFER.
           IF VDUCLASS = "D"
              GO TO VDU-MOVE-DBUFFER-8.
           IF VDUCLASS = "X"
              GO TO VDU-MOVE-SBUFFER.
      *
       VDU-NUMERIC-ROUTINE.
      * WE NOW USE VDU-SIZEX FOR BUFFER LENGTH
      * HAVE AN ISSUE BECAUSE VDU-BUFFER LENGTH CALCULATES 1 TOO SHORT
      * FOR NUMERICS.   NOT SURE WHY WE NEED IT ANY MORE BECAUSE FIELD LENGTH
      * IS STORED IN VDU-SIZEX. /MJD
      *    IF VDUPUNC = "N"
      *       COMPUTE VDU-BUFFER-LENGTH =
      *            VDUNUM + VDUDEC
      *     ELSE
      *       COMPUTE VDU-BUFFER-LENGTH =
      *            (VDUNUM - 1) / 3 + VDUNUM  + VDUDEC.
      *    IF VDUDEC NOT =  0
      *       ADD 1 TO VDU-BUFFER-LENGTH.
      *    IF VDUCLASS =  "S"
      *       ADD 1 TO VDU-BUFFER-LENGTH.
       VDU-TRANSFER-BUFFER.
           MOVE ZEROS TO VDUBUF.
           MOVE 0 TO VDU-NUM-RECEIVED VDU-DEC-RECEIVED
                     VDU-DEC-FLAG     VDU-ZERO-CNT.
      *--->MOVE VDU-BUFFER-LENGTH TO VDU-I.
           MOVE VDU-SIZEX TO VDU-I.
           MOVE 18 TO VDU-J.
       VDU-CHECK-SPACE.
           IF VDU-BUF-TAB(VDU-I) = " "
              SUBTRACT 1 FROM VDU-I
              IF VDU-I = 0
                 GO TO VDU-MOVE-VALUE
               ELSE
                 GO TO VDU-CHECK-SPACE.
       VDU-CHECK-SIGN.
           IF VDUCLASS =  "S"
              IF VDU-BUF-TAB(VDU-I) =  "-"
                 MOVE -1 TO VDU-NEG-FLAG
                 GO TO VDU-NEXT-CHARACTER
              ELSE
                 MOVE 1 TO VDU-NEG-FLAG.
       VDU-CHECK-CHARACTER.
           IF VDU-BUF-TAB(VDU-I) NUMERIC
              GO TO VDU-ADD-ONE.
           IF VDU-BUF-TAB(VDU-I) =  ","
              GO TO VDU-NEXT-CHARACTER.
           IF VDU-DEC-FLAG NOT =  0
              GO TO VDU-ERROR-RECEIVE.
           IF VDU-BUF-TAB(VDU-I) =  "."
              MOVE VDU-NUM-RECEIVED TO VDU-DEC-RECEIVED
              MOVE 0 TO VDU-NUM-RECEIVED
              MOVE 1 TO VDU-DEC-FLAG
              GO TO VDU-NEXT-CHARACTER.
           GO TO VDU-ERROR-RECEIVE.
       VDU-ADD-ONE.
           MOVE VDU-BUF-TAB(VDU-I) TO VDUNUM-TAB(VDU-J).
           SUBTRACT 1 FROM VDU-J.
           IF VDU-BUF-TAB(VDU-I) = 0
              ADD 1 TO VDU-ZERO-CNT
            ELSE
              MOVE 0 TO VDU-ZERO-CNT.
           ADD 1 TO VDU-NUM-RECEIVED.
       VDU-NEXT-CHARACTER.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I NOT =  0
              GO TO VDU-CHECK-CHARACTER.
       VDU-MOVE-VALUE.
           IF VDU-DEC-RECEIVED > VDUDEC
              GO TO VDU-ERROR-RECEIVE.
           IF VDU-DEC-FLAG NOT =  0
              GO TO VDU-CHECK-NUM.
           MOVE VDUDEC TO VDU-DEC-RECEIVED.
           SUBTRACT VDUDEC FROM VDU-NUM-RECEIVED.
       VDU-CHECK-NUM.
           IF STAT =  "1D" OR STAT = "1U"
              GO TO VDU-MOVE-F5.
           IF (VDU-NUM-RECEIVED - VDU-ZERO-CNT) > VDUNUM
              IF VDU-NUM NOT = 0
                 GO TO VDU-ERROR-RECEIVE.
           IF VDUNUM-TABLE NOT NUMERIC
              GO TO VDU-ERROR-RECEIVE.
           SUBTRACT VDU-DEC-RECEIVED FROM VDUDEC GIVING VDU-I.
           PERFORM VDU-DECIMAL-ALIGN VDU-I TIMES.
       VDU-MOVE-F5.
           MULTIPLY VDU-NUM BY VDU-NEG-FLAG GIVING VDUNUM0.
       VDU-TEST-SIZE.
           IF VDUDEC =  0
              MOVE VDUNUM0 TO VDU-EDIT0
              GO TO VDU-MOVE-VDU-EDIT.
           IF VDUDEC =  2
              MOVE VDUNUM2 TO VDU-EDIT2
              GO TO VDU-MOVE-VDU-EDIT.
           IF VDUDEC =  1
              MOVE VDUNUM1 TO VDU-EDIT1.
           IF VDUDEC =  3
              MOVE VDUNUM3 TO VDU-EDIT3
              GO TO VDU-MOVE-VDU-EDIT.
           IF VDUDEC =  4
              MOVE VDUNUM4 TO VDU-EDIT4.
           IF VDUDEC =  5
              MOVE VDUNUM5 TO VDU-EDIT5.
       VDU-MOVE-VDU-EDIT.
           IF VDUACTION =  "R"
              PERFORM VDU-XFER
              GO TO VDU-END-ROUTINE.
           MOVE SPACES TO VDU-BUF-AREA.
      *    MOVE VDU-BUFFER-LENGTH TO VDU-I.
           MOVE VDU-SIZEX TO VDU-I.
           MOVE 24 TO VDU-J.
           IF VDUCLASS =  "S"
              MOVE 25 TO VDU-J.
       VDU-MOVE-CHARACTER.
           MOVE VDU-EDIT-TAB(VDU-J) TO VDU-BUF-TAB(VDU-I).
           SUBTRACT 1 FROM VDU-I.
           IF VDUPUNC = "N"
              IF VDU-EDIT-TAB(VDU-J) = ","
                 ADD 1 TO VDU-I.
           SUBTRACT 1 FROM VDU-J.
           IF VDU-I =  0 OR VDU-J = 0
              GO TO VDU-MOVE-FIELD.
           GO TO VDU-MOVE-CHARACTER.
       VDU-DECIMAL-ALIGN.
           MULTIPLY 10 BY VDU-NUM.

      ******************************************************************
      * DATE ROUTINES
      ******************************************************************
       VDU-MOVE-DBUFFER.
           MOVE 0 TO VDU-COUNTER.
           INSPECT VDU-ERROR-MESS1 TALLYING VDU-COUNTER FOR ALL "/".
      *    SIX DIGIT DATE ENTERRED WITH NO "/"
           IF VDU-COUNTER  = 0
              MOVE SPACES TO VDU-BUF-TAB(7)
                             VDU-BUF-TAB(8).
           MOVE 8 TO VDU-I.
           MOVE 0 TO VDU-NUM-RECEIVED.
           MOVE 18 TO VDU-J.
           MOVE ZEROS TO VDUBUF.
       VDU-CHECK-DSPACE.
           IF VDU-BUF-TAB(VDU-I) NOT =  " "
              GO TO VDU-CHECK-DCHARACTER.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I =  0
              GO TO VDU-MOVE-DVALUE.
           GO TO VDU-CHECK-DSPACE.
       VDU-CHECK-DCHARACTER.
           IF VDU-BUF-TAB(VDU-I) =  "/"
              GO TO VDU-NEXT-DCHARACTER.
           MOVE VDU-BUF-TAB(VDU-I) TO VDUNUM-TAB(VDU-J).
           SUBTRACT 1 FROM VDU-J.
           ADD 1 TO VDU-NUM-RECEIVED.
       VDU-NEXT-DCHARACTER.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I NOT =  0
              GO TO VDU-CHECK-DCHARACTER.
       VDU-MOVE-DVALUE.
           IF STAT = "1U"
              GO TO VDU-SEND-DATE.
           IF VDUNUM-DATE NOT NUMERIC OR
              (VDU-NUM-RECEIVED > 6 AND
                             VDU-NUM NOT = 0)
              GO TO VDU-ERROR-RECEIVE.
           IF VDUNUM-DATE = "000000" AND VDUPUNC = "R"
              GO TO VDU-SEND-DATE.
       VDU-ARRANGE-DATE.
           MOVE VDU-DATE-1 TO VDU-MM.
           MOVE VDU-DATE-2 TO VDU-DD.
           MOVE VDU-DATE-3 TO VDU-YY.
       VDU-CHECK-DATE.
           IF VDU-DD = 0 OR VDU-MM = 0
              GO TO VDU-ERROR-RECEIVE.
           IF VDU-MM > 12 OR
              VDU-DD > VDU-DAYS(VDU-MM)
              GO TO VDU-ERROR-RECEIVE.
           IF VDU-MM = 2 AND VDU-DD = 29
              DIVIDE VDU-YY BY 4 GIVING VDU-I REMAINDER VDU-I
              IF VDU-I NOT = 0
                 GO TO VDU-ERROR-RECEIVE.
       VDU-SEND-DATE.
           MOVE VDU-NUM TO VDU-EDIT-DATE.
           MOVE VDU-EDIT-DATE TO VDU-BUF-AREA.
      * IF ENTERED 0 OR RETURN, THIS FIELD HELD ORIGINAL VALUE IN MAINTENANCES
           IF VDUNUM0 = 0
              MOVE 0 TO VDU-DATE-YYYYMMDD
           ELSE
              MOVE 0      TO VDU-DATE-YYYYMMDD
              MOVE VDU-MM TO VDU-YYYYMMDD-MM
              MOVE VDU-DD TO VDU-YYYYMMDD-DD
              MOVE VDU-YY TO VDU-YYYYMMDD-YY
              IF VDU-YYYYMMDD-YYYY < VDU-CC
                 ADD 2000 TO VDU-YYYYMMDD-YYYY
              ELSE
                 ADD 1900 TO VDU-YYYYMMDD-YYYY.
           IF VDUACTION =  "R"
              PERFORM VDU-XFER
              GO TO VDU-END-ROUTINE
           ELSE
              GO TO VDU-MOVE-FIELD.

      ******************************************************************
      * DATE 8 ROUTINES
      ******************************************************************
       VDU-MOVE-DBUFFER-8.
           MOVE 0 TO VDU-COUNTER.
           INSPECT VDU-ERROR-MESS1 TALLYING VDU-COUNTER FOR ALL "/".
      *    EIGHT DIGIT DATE ENTERRED WITH NO "/"
           IF VDU-COUNTER  = 0
              MOVE SPACES TO VDU-BUF-TAB(9)
                             VDU-BUF-TAB(10).
           MOVE 10 TO VDU-I.
           MOVE 0 TO VDU-NUM-RECEIVED.
           MOVE 18 TO VDU-J.
           MOVE ZEROS TO VDUBUF.
       VDU-CHECK-DSPACE-8.
           IF VDU-BUF-TAB(VDU-I) NOT =  " "
              GO TO VDU-CHECK-DCHARACTER-8.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I =  0
              GO TO VDU-MOVE-DVALUE-8.
           GO TO VDU-CHECK-DSPACE-8.
       VDU-CHECK-DCHARACTER-8.
           IF VDU-BUF-TAB(VDU-I) =  "/"
              GO TO VDU-NEXT-DCHARACTER-8.
           MOVE VDU-BUF-TAB(VDU-I) TO VDUNUM-TAB(VDU-J).
           SUBTRACT 1 FROM VDU-J.
           ADD 1 TO VDU-NUM-RECEIVED.
       VDU-NEXT-DCHARACTER-8.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I NOT =  0
              GO TO VDU-CHECK-DCHARACTER-8.
       VDU-MOVE-DVALUE-8.
           IF STAT = "1U"
              GO TO VDU-SEND-DATE-8.
           IF VDUNUM-DATE-8 NOT NUMERIC OR
              (VDU-NUM-RECEIVED > 8 AND
                             VDU-NUM NOT = 0)
              GO TO VDU-ERROR-RECEIVE.
     
           IF VDUNUM-DATE-8 = "00000000" AND VDUPUNC = "R"
              GO TO VDU-SEND-DATE-8.
       VDU-ARRANGE-DATE-8.
           MOVE VDU-DATE-8-MM TO VDU-8-MM.
           MOVE VDU-DATE-8-DD TO VDU-8-DD.
           MOVE VDU-DATE-8-YY TO VDU-8-YY.
       VDU-CHECK-DATE-8.
           IF VDU-8-DD = 0 OR VDU-8-MM = 0
              GO TO VDU-ERROR-RECEIVE.
           IF VDU-8-MM > 12 OR
              VDU-8-DD > VDU-DAYS(VDU-8-MM)
              GO TO VDU-ERROR-RECEIVE.
           IF VDU-8-MM = 2 AND VDU-8-DD = 29
              DIVIDE VDU-8-YY BY 4 GIVING VDU-I REMAINDER VDU-I
              IF VDU-I NOT = 0
                 GO TO VDU-ERROR-RECEIVE.
       VDU-SEND-DATE-8.
           MOVE VDU-NUM TO VDU-EDIT-DATE-8.
           MOVE VDU-EDIT-DATE-8 TO VDU-BUF-AREA.
           MOVE VDU-DATE-8-MM TO VDU-YYYYMMDD-MM.
           MOVE VDU-DATE-8-DD TO VDU-YYYYMMDD-DD.
           MOVE VDU-DATE-8-YY TO VDU-YYYYMMDD-YYYY.
           IF VDUACTION =  "R"
              PERFORM VDU-XFER
              GO TO VDU-END-ROUTINE
           ELSE
              GO TO VDU-MOVE-FIELD.

      ******************************************************************
      * SSNO ROUTINES
      ******************************************************************
       VDU-MOVE-SBUFFER.
           MOVE 0 TO VDU-COUNTER.
           INSPECT VDU-ERROR-MESS1 TALLYING VDU-COUNTER FOR ALL "-".
           IF VDU-COUNTER = 0
              INSPECT VDU-ERROR-MESS1 TALLYING VDU-COUNTER FOR ALL "/".
      *    NINE DIGIT SSNO ENTERRED WITH NO "-"
           IF VDU-COUNTER  = 0
              MOVE SPACES TO VDU-BUF-TAB(10)
                             VDU-BUF-TAB(11).
           MOVE 11 TO VDU-I.
           MOVE 0 TO VDU-NUM-RECEIVED.
           MOVE 18 TO VDU-J.
           MOVE ZEROS TO VDUBUF.
       VDU-CHECK-SSPACE.
           IF VDU-BUF-TAB(VDU-I) NOT =  " "
              GO TO VDU-CHECK-SCHARACTER.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I =  0
              GO TO VDU-MOVE-SVALUE.
           GO TO VDU-CHECK-SSPACE.
       VDU-CHECK-SCHARACTER.
           IF VDU-BUF-TAB(VDU-I) =  "-" OR "/"
              GO TO VDU-NEXT-SCHARACTER.
           MOVE VDU-BUF-TAB(VDU-I) TO VDUNUM-TAB(VDU-J).
           SUBTRACT 1 FROM VDU-J.
           ADD 1 TO VDU-NUM-RECEIVED.
       VDU-NEXT-SCHARACTER.
           SUBTRACT 1 FROM VDU-I.
           IF VDU-I NOT =  0
              GO TO VDU-CHECK-SCHARACTER.
       VDU-MOVE-SVALUE.
           IF STAT = "1U"
              GO TO VDU-SEND-SSNO.
           IF VDUNUM-SSNO NOT NUMERIC OR
              (VDU-NUM-RECEIVED > 9 AND
                             VDU-NUM NOT = 0)
              GO TO VDU-ERROR-RECEIVE.
           IF VDUNUM-SSNO = "000000000" AND VDUPUNC = "R"
              GO TO VDU-SEND-SSNO.
       VDU-ARRANGE-SSNO.
           MOVE VDU-SSNO-1 TO VDU-SSNO1.
           MOVE VDU-SSNO-2 TO VDU-SSNO2.
           MOVE VDU-SSNO-3 TO VDU-SSNO3.
       VDU-CHECK-SSNO.
           IF VDU-NUM = 0
              GO TO VDU-ERROR-RECEIVE.
       VDU-SEND-SSNO.
           MOVE VDU-NUM TO VDU-EDIT-SSNO.
           MOVE VDU-EDIT-SSNO TO VDU-BUF-AREA.
           INSPECT VDU-BUF-AREA REPLACING ALL "/" BY "-".
           IF VDUACTION =  "R"
              PERFORM VDU-XFER
              GO TO VDU-END-ROUTINE
           ELSE
              GO TO VDU-MOVE-FIELD.



       VDU-MOVE-FIELD.
           PERFORM VDU-XFER.
       VDU-SEND-ROUTINE.
      *?   CALL WRFORM USING STAT VDU-NM-LIST.
           IF STAT NOT = "00"
              GO TO VDU-SEND-ROUTINE.
           IF VDUACTION =  "S"
              MOVE STAT TO VDUSTATUS.
       VDU-END-ROUTINE.
           GO TO VDU-EXIT.
       VDU-INVALID-FIELD.
      *    LOOP IN THIS ROUTINE IS INTENTIONAL.
      *    (FOR PROGRAM DEBUGGING ONLY; STOP WITH ~~)
           MOVE " MISSING FIELD =" TO VDU-ERROR-MESSAGE.
           MOVE VDU-NM-LIST TO VDU-ERROR-FIELD.
           MOVE VDU-ERROR-MESSAGE TO MESS.
           PERFORM SEND-MESS.
           IF STAT = "~~" OR "00"
              GO TO VDU-REAL-EXIT.
           GO TO VDU-INVALID-FIELD.
       VDU-ERROR-RECEIVE.
           MOVE " INVALID ENTRY, PLEASE PRESS ENTER."
              TO VDU-ERROR-MESSAGE.
       VDU-SEND-ERROR.
           PERFORM VDU-SEND-MESS.
           GO TO VDU-RECEIVE-ROUTINE.
       VDU-SEND-MESS.
           DISPLAY MESSAGE BOX VDU-ERROR-MESS1.
       VDU-READ-FIELD.
      *--->CALL RRFORM USING STAT VDU-NM-LIST VDU-BUF-AREA.
           MOVE VDU-ATTRIBUTES TO VDU-DISP-ATTRIBUTES.
           MOVE VDUPROCESS TO VDU-DISP-PROCESS.
           PERFORM SETUP-CONTROL.
           MOVE "00" TO STAT.
           MOVE 0 TO VDU-CP.
      * 2/13/2024 ADDED OR NOT NUMERIC
           IF VDU-SIZE < 1 OR VDU-SIZE NOT NUMERIC
              MOVE 1 TO VDU-SIZE.
           IF VDU-CP < 1 OR VDU-CP NOT NUMERIC
              MOVE 1 TO VDU-CP.
      *    IF VDU-CP < 1 MOVE 1 TO VDU-CP.
           SUBTRACT VDU-SIZE FROM 19 GIVING VDU-BP.
           IF VDUCLASS = "N"
              EVALUATE VDUDEC
      *          WHEN 0 ACCEPT VDU-NUMX(19 - VDU-SIZE:18)
                 WHEN 0 ACCEPT VDU-NUMX(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 1 ACCEPT VDU-NUM1X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 2 ACCEPT VDU-NUM2X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 3 ACCEPT VDU-NUM3X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 4 ACCEPT VDU-NUM4X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 5 ACCEPT VDU-NUM5X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                END-EVALUATE
      *         INSPECT VDU-BUF-AREA REPLACING ALL "0" BY " "
      *         CALL "C$JUSTIFY" USING VDU-BUF-AREA "L"
      
      *          MOVE VDUBUF(VDU-BP:VDU-CP - 1) TO VDU-BUF-AREA
                IF VDU-CP = 1
                   IF VDUBUF(VDU-BP:VDU-SIZE) NOT = VDU-DEFAULT
                      MOVE VDUBUF(VDU-BP:VDU-CP) TO VDU-BUF-AREA
                   ELSE 
                      MOVE VDUBUF(VDU-BP:VDU-SIZE) TO VDU-BUF-AREA
                   END-IF
                ELSE
                   IF VDU-CP = VDU-SIZE
                      MOVE VDUBUF(VDU-BP:VDU-CP) TO VDU-BUF-AREA
                   ELSE
                      MOVE VDUBUF(VDU-BP:VDU-CP - 1) TO VDU-BUF-AREA
                   END-IF
                END-IF
                INSPECT VDU-BUF-AREA REPLACING LEADING "0" BY " "
                CALL "C$JUSTIFY" USING VDU-BUF-AREA "L"
           ELSE IF VDUCLASS = "S"
              EVALUATE VDUDEC
                 WHEN 0 ACCEPT VDUNUMX(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 1 ACCEPT VDUNUM1X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 2 ACCEPT VDUNUM2X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 3 ACCEPT VDUNUM3X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 4 ACCEPT VDUNUM4X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                 WHEN 5 ACCEPT VDUNUM5X(19 - VDU-SIZE:)
                           AT VDU-POS
                           PROTECTED SIZE VDU-SIZE
      *                    PROMPT "_"
                           DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *------------------->ECHO
                           CONTROL VDU-CONTROL
                           ON EXCEPTION VDU-KEY-DEST
                                        PERFORM VDU-SET-STATUS
                        END-ACCEPT
                END-EVALUATE
      *         INSPECT VDU-BUF-AREA REPLACING ALL "0" BY " "
      *         CALL "C$JUSTIFY" USING VDU-BUF-AREA "L"
      
      *         MOVE VDUBUF(VDU-BP:VDU-CP - 1) TO VDU-BUF-AREA
                IF VDU-CP = 1
                   IF VDUBUF(VDU-BP:VDU-SIZE) NOT = VDU-DEFAULT
                      MOVE VDUBUF(VDU-BP:VDU-CP) TO VDU-BUF-AREA
                   ELSE 
                      MOVE VDUBUF(VDU-BP:VDU-SIZE) TO VDU-BUF-AREA
                   END-IF
                ELSE
                   IF VDU-CP = VDU-SIZE
                      MOVE VDUBUF(VDU-BP:VDU-CP) TO VDU-BUF-AREA
                   ELSE
                      MOVE VDUBUF(VDU-BP:VDU-CP - 1) TO VDU-BUF-AREA
                   END-IF
                END-IF
                INSPECT VDU-BUF-AREA REPLACING LEADING "0" BY " "
                CALL "C$JUSTIFY" USING VDU-BUF-AREA "L"

           ELSE
              ACCEPT VDU-BUF-AREA(1:VDU-SIZE)
                 AT VDU-POS
                 PROTECTED SIZE VDU-SIZE
      *          PROMPT "_"
                 DEFAULT VDU-DEFAULT
                           CURSOR VDU-CP
      *--------->ECHO
      *--------->UPPER
                 CONTROL VDU-CONTROL
                 ON EXCEPTION VDU-KEY-DEST PERFORM VDU-SET-STATUS
              END-ACCEPT
      *       IF CHAR POS = 1 MEANS ONE CHARACTER ENTERRED IN A ONE CHAR FIELD
      *            OR USER HIT RETURN TO ACCEPT PREFILLED VALUE.
      *            IF VALUE IS SAME AS DEFAULT ASSUME HITTING RETURN ON
      *             EXISTING VALUE
      *       IF CURSOR POSITION IS IN THE LAST POS OF THE STRING:
      *       THEN THE USER HAS ENTERRED 1 LESS THAN THE MAX LEN STRING OR
      *            THE USER HAS ENTERRED EXACTLY THE MAX LENGTH STRING. I HAVE
      *            NO WAY OF KNOWING AS THE CURS POS IS THE SAME EITHER WAY.
      *            ERR ION THE SIDE OF CAUTION AND ALWAYS GRAP CHAR IN LAST POSS
      *            WHEN THIS HAPPENS
      *            ELSE GRAB THE POSITION LESS ONE (I.E. EXCLUDING CURRENT
      *                 CURSOR POSITION)
              IF VDU-CP = 1
                 IF VDU-BUF-AREA NOT = VDU-DEFAULT
                    MOVE VDU-BUF-AREA(1:VDU-CP) TO VDU-BUF-AREA
                 END-IF
              ELSE
                IF VDU-CP = VDU-SIZE
                   MOVE VDU-BUF-AREA(1:VDU-CP) TO VDU-BUF-AREA
                ELSE
                   MOVE VDU-BUF-AREA(1:VDU-CP - 1) TO VDU-BUF-AREA
              END-IF.

           IF VDU-ATR-RJUS = "Y"
              PERFORM VDU-SHIFT-RJUS.

      * MUST KEEP
      * MUST FORCE UPPERCASE
           IF VDU-ATR-MUST = "N"
              PERFORM VDU-SHIFT-MUST. 

           MOVE SPACES TO VDU-DEFAULT.
       RESET-VDU-NM-LIST.
           DISPLAY "___________________________________________________"
             AT VDU-POS
             SIZE VDU-SIZE
           END-DISPLAY.
       VDU-XFER.
           MOVE VDU-POS TO VDU-DISP-POS.
           MOVE VDU-SIZE TO VDU-DISP-SIZE.
           MOVE VDU-BUF-AREA TO VDU-DISP-BUF.
           MOVE VDUPROCESS TO VDU-DISP-PROCESS.
           MOVE VDU-ATTRIBUTES TO VDU-DISP-ATTRIBUTES.
           PERFORM DISPLAY-FIELD.
           MOVE ZEROS TO STAT.
       VDU-EXIT.
           MOVE SPACES TO VDU-ATTRIBUTES.
       VDU-REAL-EXIT.
           EXIT.

      ******************************************************************
      * SEND MESSAGE TO SCREEN WITH ALARM
      ******************************************************************
      ******

      *    SEND MESS RESIDES IN LIBGB/DECLARE

      ******
      *SEND-MESS SECTION.
      *    MOVE "00" TO STAT.
      *    PERFORM ALARM.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE TMMSG TO FORM-NAM.
      *    CALL FORM-PROGX USING MESS.
      *    CANCEL FORM-PROGX.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE TMRSP TO FORM-NAM.
      *    CALL FORM-PROGX USING MESS.
      *    CANCEL FORM-PROGX.
      *    MOVE MESS TO STAT.
      *    IF STAT EQUAL "~~"
      *       STOP RUN.
      *    MOVE SPACES TO MESS.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE TMMSG TO FORM-NAM.
      *    CALL FORM-PROGX USING MESS.
      *    CANCEL FORM-PROGX.

      ******************************************************************
      * CONVERT ACUCOBOL FUNCTION KEYS TO CURRENT PARADATA CODES
      ******************************************************************
       VDU-SET-STATUS SECTION.
      * ENTER
              MOVE "00" TO STAT.

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              GO TO VDU-SET-STATUS-EXIT.

           EVALUATE VDU-KEY-DEST
      * ENTER
              WHEN 13 MOVE "00" TO STAT
      * TAB
      *------>WHEN  9 MOVE "??" TO STAT
      * F1
              WHEN  1 MOVE "10" TO STAT
      * F2
              WHEN  2 MOVE "12" TO STAT
      * F3
              WHEN  3 MOVE "16" TO STAT
      * F4
              WHEN  4 MOVE "18" TO STAT
      * F5
              WHEN  5 MOVE "1:" TO STAT
      * F6
              WHEN  6 MOVE "1<" TO STAT
      * F7
              WHEN  7 MOVE "1>" TO STAT
      * F8
              WHEN  8 MOVE "1 " TO STAT
      * F9
              WHEN  9 MOVE "1(" TO STAT
      * F10
              WHEN 10 MOVE "1)" TO STAT
      * F11
              WHEN 11 MOVE "1!" TO STAT
      * F12
              WHEN 12 MOVE "1#" TO STAT
      * F13
      *------>WHEN 13 MOVE "??" TO STAT
      * ^L
              WHEN 14 MOVE "^L" TO STAT
      * ^N
              WHEN 15 MOVE "^N" TO STAT
      * ^B
              WHEN 16 MOVE "^B" TO STAT
      * ^R
              WHEN 17 MOVE "^R" TO STAT
      * F18
      *       WHEN 18 MOVE "??" TO STAT
      * F19
      *       WHEN 19 MOVE "??" TO STAT
      * F20
      *       WHEN 20 MOVE "??" TO STAT
      * ESC
              WHEN 27 MOVE "1E" TO STAT
      * DO CO
      *       WHEN 40 MOVE "??" TO STAT
      * HELP
      *------>WHEN 90 MOVE "??" TO STAT
      * UP ARROW
              WHEN 52 MOVE "1U" TO STAT
      * DOWN ARROW
              WHEN 53 MOVE "1D" TO STAT
      * PAGE UP
              WHEN 67 MOVE "1P" TO STAT
      * PAGE DOWN
              WHEN 68 MOVE "1N" TO STAT
           END-EVALUATE.
           INITIALIZE VDU-KEY-DEST.

       VDU-SET-STATUS-EXIT.
           EXIT.

      ******************************************************************
      * SETUP CONTROL ATTRIBUTES FOR VDU
      ******************************************************************
       SETUP-CONTROL SECTION.
           INITIALIZE VDU-CONTROL VDU-HLD-CONTROL VDU-AT-LIST.
      *--SET ATTRIBUTES FROM OLD FORM
      *--HOLD
           MOVE VDU-FORM-ATTR(1) TO VDU-AT-GROUP1.
      *--RJUS
           IF VDU-FORM-ATTR(2) = "Y"
              CONTINUE.
      *       MOVE "R" TO VDU-AT-GROUP13.
      *--BOLD
           IF VDU-FORM-ATTR(3) = "Y"
              MOVE "B" TO VDU-AT-GROUP3.
      *   SCAN WINDOWS WILL HAVE THIS SET SO TITLE/WSEL/LEGENT SHOW AS
      *   BOLD AND REVERSE VIDEO
           IF VDU-FORM-ATTR(3) = "R"
              MOVE "B" TO VDU-AT-GROUP3
              MOVE "Y" TO VDU-AT-GROUP5.
      *--AUTO
           IF VDU-FORM-ATTR(4) = "Y"
              CONTINUE.
      *--ZFIL
           IF VDU-FORM-ATTR(5) = "Y"
              CONTINUE.
      *--VALU
           IF VDU-FORM-ATTR(6) = "Y"
              CONTINUE.
      *--MUST
           MOVE VDU-FORM-ATTR(7) TO VDU-AT-GROUP17.
      *--CONS
           IF VDU-FORM-ATTR(8) = "Y"
              CONTINUE.
      *--TOTL
           IF VDU-FORM-ATTR(9) = "Y"
              CONTINUE.
      *--PERM
           IF VDU-FORM-ATTR(10) = "Y"
              CONTINUE.
      *--ECHO
           MOVE VDU-FORM-ATTR(11) TO VDU-AT-GROUP10.

           IF VDU-DISP-ATTRIBUTES NOT = SPACES
              PERFORM VARYING VDU-SUB FROM 1 BY 1 UNTIL VDU-SUB > 8
                 IF VDU-DISP-ATTRIB(VDU-SUB) NOT = SPACE
                    EVALUATE VDU-DISP-ATTRIB(VDU-SUB)
                       WHEN 'H' MOVE "H" TO VDU-AT-GROUP3
                       WHEN 'B' MOVE "B" TO VDU-AT-GROUP3
                       WHEN 'D' MOVE "L" TO VDU-AT-GROUP3
                       WHEN 'S' MOVE "S" TO VDU-AT-GROUP3
      *                ----------------------------------
                       WHEN 'F' MOVE "Y" TO VDU-AT-GROUP4
      *                ----------------------------------
                       WHEN 'R' MOVE "Y" TO VDU-AT-GROUP5
      *                ----------------------------------
                       WHEN 'E' MOVE "Y" TO VDU-AT-GROUP10
      *                ----------------------------------
                       WHEN 'U' MOVE "Y" TO VDU-AT-GROUP12
                    END-EVALUATE
                 END-IF
              END-PERFORM.

      *-----------------------------------------------------------------
      * GROUP  1.  ERASE, NO ERASE (DEFAULT: ERASE)
      *-----------------------------------------------------------------
      *    IF VDU-AT-NO-ERASE
      *       STRING VDU-CONTROL
      *              ", " VDU-SET-NO-ERASE
      *          DELIMITED BY "  " INTO VDU-HLD-CONTROL
      *    ELSE
      *       STRING VDU-CONTROL
      *              ", " VDU-SET-ERASE
      *          DELIMITED BY "  " INTO VDU-HLD-CONTROL.
      *
      *    MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *    MOVE "ERASE EOL" TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP  2.  BEEP, NO BEEP  (DEFAULT: NO BEEP)
      *-----------------------------------------------------------------
           IF VDU-AT-BEEP
              STRING VDU-CONTROL
                     ", " VDU-SET-BEEP
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
           ELSE
              STRING VDU-CONTROL
                     ", " VDU-SET-NO-BEEP
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL.
           MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP  3.  HIGH, LOW, STANDARD, OFF (DEFAULT: HIGH)
      *-----------------------------------------------------------------
           EVALUATE TRUE
              WHEN VDU-AT-HIGHLIGHT
                      STRING VDU-CONTROL
                             ", " VDU-SET-HIGHLIGHT
                           DELIMITED BY "  " INTO VDU-HLD-CONTROL
                      MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              WHEN VDU-AT-BOLD
                      STRING VDU-CONTROL
                             ", " VDU-SET-HIGHLIGHT
                           DELIMITED BY "  " INTO VDU-HLD-CONTROL
                      MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              WHEN VDU-AT-LOW
                      STRING VDU-CONTROL
                             ", " VDU-SET-LOW
                           DELIMITED BY "  " INTO VDU-HLD-CONTROL
                      MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              WHEN VDU-AT-STANDARD
                      STRING VDU-CONTROL
                             ", " VDU-SET-STANDARD
                           DELIMITED BY "  " INTO VDU-HLD-CONTROL
                      MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              WHEN VDU-AT-OFF
                      STRING VDU-CONTROL
                             ", " VDU-SET-OFF
                           DELIMITED BY "  " INTO VDU-HLD-CONTROL
                      MOVE VDU-HLD-CONTROL TO VDU-CONTROL
           END-EVALUATE.
      *-----------------------------------------------------------------
      * GROUP  4.  BLINK, NO BLINK (DEFAULT: NO BLINK)
      *-----------------------------------------------------------------
           IF VDU-AT-BLINKING
              STRING VDU-CONTROL
                     ", " VDU-SET-BLINKING
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
           ELSE
              STRING VDU-CONTROL
                     ", " VDU-SET-NO-BLINKING
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL.
           MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP  5.  REVERSE, NO REVERSE (DEFAULT: NO REVERSE)
      *-----------------------------------------------------------------
           IF VDU-AT-REVERSED
              STRING VDU-CONTROL
                     ", " VDU-SET-REVERSED
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
           ELSE
              STRING VDU-CONTROL
                    ", " VDU-SET-NO-REVERSED
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL.
           MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP  6.  TAB, NO TAB
      *-----------------------------------------------------------------
           IF VDU-DISP-ACTION NOT = "S"
              EVALUATE TRUE
                 WHEN VDU-AT-TAB
                         STRING VDU-CONTROL
                                ", " VDU-SET-TAB
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
                 WHEN VDU-AT-NO-TAB
                         STRING VDU-CONTROL
                                ", " VDU-SET-NO-TAB
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              END-EVALUATE.
      *-----------------------------------------------------------------
      * GROUP  7.  PROMPT, NO PROMPT  (DEFAULT " ")
      *-----------------------------------------------------------------
      *    IF VDU-DISP-ACTION NOT = "S"
      *       STRING VDU-CONTROL
      *              ", " VDU-SET-PROMPT
      *          DELIMITED BY "  " INTO VDU-HLD-CONTROL
      *       MOVE VDU-HLD-CONTROL TO VDU-CONTROL
      *       MOVE SPACE TO VDU-SET-PROMPT-VAL.
      *-----------------------------------------------------------------
      * GROUP  8.  CONVERT, NO CONVERT (DEFAULT NO CONVERT)
      *-----------------------------------------------------------------
           IF VDU-DISP-ACTION NOT = "S"
              IF VDU-AT-CONVERT
                 STRING VDU-CONTROL
                        ", " VDU-SET-CONVERT
                      DELIMITED BY "  " INTO VDU-HLD-CONTROL
                 MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              ELSE
                 STRING VDU-CONTROL
                        ", " VDU-SET-NO-CONVERT
                      DELIMITED BY "  " INTO VDU-HLD-CONTROL
              END-IF
              MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP  9.  UPDATE, NO UPDATE
      *-----------------------------------------------------------------
           IF VDU-DISP-ACTION NOT = "S"
              EVALUATE TRUE
                 WHEN VDU-AT-UPDATE
                         STRING VDU-CONTROL
                                ", " VDU-SET-UPDATE
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
                 WHEN VDU-AT-NO-UPDATE
                         STRING VDU-CONTROL
                                ", " VDU-SET-NO-UPDATE
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              END-EVALUATE.
      *-----------------------------------------------------------------
      * GROUP 10.  ECHO, NO ECHO
      *-----------------------------------------------------------------
           IF VDU-DISP-ACTION NOT = "S"
              IF VDU-AT-NO-ECHO
                   STRING VDU-CONTROL
                          ", " VDU-SET-NO-ECHO
                        DELIMITED BY "  " INTO VDU-HLD-CONTROL
              ELSE
                   STRING VDU-CONTROL
                          ", " VDU-SET-ECHO
                        DELIMITED BY "  " INTO VDU-HLD-CONTROL
              END-IF
              MOVE VDU-HLD-CONTROL TO VDU-CONTROL
           ELSE
      *  NEED OVERRIDE ON DISPLAY (S) HERE.   IF THE FIELD IS
      *  DEFINED AS NO ECHO THEN WHRN YOU TRY TO CLEAR THE FIELD
      *  VIA MOVING SPACES TO WR-LIST AND PERFORMING WRFORM THE
      *  PROMPT REMAINS BECAIUSE FO THE NO ECHO ATTRIBUTE.
               IF VDU-AT-ECHO
                   STRING VDU-CONTROL
                          ", " VDU-SET-ECHO
                        DELIMITED BY "  " INTO VDU-HLD-CONTROL
              END-IF.
      *-----------------------------------------------------------------
      * GROUP 11.  UPPER, LOWER, NO CONVERSION (DEFAULT)
      *-----------------------------------------------------------------
           IF VDU-DISP-ACTION NOT = "S"
              EVALUATE TRUE
                 WHEN VDU-AT-UPPER
                         STRING VDU-CONTROL
                                ", " VDU-SET-UPPER
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
                 WHEN VDU-AT-LOWER
                         STRING VDU-CONTROL
                                ", " VDU-SET-LOWER
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              END-EVALUATE.
      *-----------------------------------------------------------------
      * GROUP 12.  UNDERLINE, NO UNDERLINE (DEFAULT:NO UNDERLINE)
      *-----------------------------------------------------------------
           IF VDU-AT-UNDERLINED
              STRING VDU-CONTROL
                     ", " VDU-SET-UNDERLINED
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
      * THIS DOES NOT WORK W/ BLINK, I DO NOT YET KNOW WHY
      *    ELSE
      *       STRING VDU-CONTROL
      *             ", "  VDU-SET-NO-UNDERLINED
      *          DELIMITED BY "  " INTO VDU-HLD-CONTROL.
           MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP 13.  LEFT, RIGHT, CENTERED, NO JUST
      *-----------------------------------------------------------------
           IF VDU-DISP-ACTION NOT = "S"
              EVALUATE TRUE
                 WHEN VDU-AT-LEFT
                         STRING VDU-CONTROL
                                ", " VDU-SET-LEFT
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
                 WHEN VDU-AT-RIGHT
                         STRING VDU-CONTROL
                                ", " VDU-SET-RIGHT
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
                 WHEN VDU-AT-CENTER
                         STRING VDU-CONTROL
                                ", " VDU-SET-CENTER
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
                 WHEN VDU-AT-NO-JUST
                         STRING VDU-CONTROL
                                ", " VDU-SET-NO-JUST
                              DELIMITED BY "  " INTO VDU-HLD-CONTROL
                         MOVE VDU-HLD-CONTROL TO VDU-CONTROL
              END-EVALUATE.
      *-----------------------------------------------------------------
      * GROUP 14.  SAME
      *-----------------------------------------------------------------
           IF VDU-AT-SAME
              STRING VDU-CONTROL
                     ", " VDU-SET-TAB
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
               MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP 15.  FCOLOR
      *-----------------------------------------------------------------
           IF VDU-AT-FCOLOR
              STRING VDU-CONTROL
                     ", " VDU-SET-FCOLOR
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
               MOVE VDU-HLD-CONTROL TO VDU-CONTROL.
      *-----------------------------------------------------------------
      * GROUP 16.  BCOLOR
      *-----------------------------------------------------------------
           IF VDU-AT-BCOLOR
              STRING VDU-CONTROL
                     ", " VDU-SET-BCOLOR
                 DELIMITED BY "  " INTO VDU-HLD-CONTROL
               MOVE VDU-HLD-CONTROL TO VDU-CONTROL.

      *-----------------------------------------------------------------
      * GROUP 17.  MUST (REQUIRED ENTRY)
      *-----------------------------------------------------------------
      *     IF VDU-AT-REQUIRED
      *        STRING VDU-CONTROL
      *               ", REQUIRED"
      *           DELIMITED BY "  " INTO VDU-HLD-CONTROL
      *         MOVE VDU-HLD-CONTROL TO VDU-CONTROL.

           INSPECT VDU-CONTROL REPLACING FIRST "," BY " ".
       SETUP-CONTROL-EXIT.
           EXIT.

      ******************************************************************
      *    EXTRACT VDU ATRIBUTES FROM VDU.
      *       EXAMPLE "E  A020PMPT:FD."  - PUTS "FD" INTO VDU-ATTRIBUTES
      ******************************************************************
       VDU-EXTRACT-ATTRIBUTES SECTION.
           MOVE 0 TO VDU-SUB2.
           MOVE SPACES TO VDU-GARBAGE VDU-ATTRIBUTES.
           INSPECT VDUFIELD TALLYING VDU-SUB2 FOR ALL ":"
           IF VDU-SUB2 > 0
              UNSTRING VDUFIELD DELIMITED BY ":"
                       INTO VDU-GARBAGE VDU-ATTRIBUTES
              MOVE VDU-GARBAGE TO VDUFIELD
              INSPECT VDU-ATTRIBUTES REPLACING ALL "." BY " "
              INSPECT VDUFIELD REPLACING ALL "." BY " "
              INSPECT VDUFIELD REPLACING FIRST " " BY ".".

      *-----------------------------------------------------------------
      * CALL-RRFORM: GETS DATA FROM SCREEN FOR ALL FIELDS IN THE LIST
      *
      *  IN : WR-LIST
      *
      *  OUT: WR-BUF
      *       STAT   (CONTAINS # OF FIELDS THAT DID NOT DISPLAY)
      *              (I.E. THEY ARE NON-DISPLAY OR THE FIELD DOES NOT EXIST)
      *
      *  CALL:   MOVE ???-LIST TO WR-LIST.
      *          PERFORM CALL-RRFORM
      *          MOVE WR-BUF TO ???-BUF.
      *
      * NOTE THIS ROUTINES CALLS SCREENIO, ALSO IT IS NOT SMART ENOUGH
      *      TO INPUT FORMATTED NUMERICS, EVERY THING COMES IN AS ALPHA
      *          
      *-----------------------------------------------------------------
      *CALL-RRFORM SECTION.
      *    MOVE 00 TO WR-STAT.
      *    CALL "C$TOUPPER" USING WR-LIST, VALUE WR-MAX.
      *    INSPECT WR-LIST REPLACING ALL "." BY " ".
      *    MOVE 1 TO WR-START WRSUB WR-PTR.
      *    SET WR-LIST-SIZE TO SIZE OF WR-LIST.
      *    MOVE "N" TO WR-LIST-EMPTY.
      *    MOVE SPACES TO WR-BUF.
      *    PERFORM RR-PROCESS-ELEMENTS UNTIL WR-NO-MORE-ELEMENTS.
      *    MOVE WR-STATX TO STAT.
      *    MOVE SPACES TO WR-LIST.
      *
      *RR-PROCESS-ELEMENTS SECTION.
      *    UNSTRING WR-LIST
      *          DELIMITED BY ALL SPACE
      *          INTO WR-ELEMENT
      *          POINTER WR-PTR
      *          ON OVERFLOW
      *             IF WR-PTR > WR-LIST-SIZE
      *                MOVE "Y" TO WR-LIST-EMPTY
      *             END-IF
      *    PERFORM RR-ACCEPT-ELEMENT
      *    MOVE SPACES TO WR-ELEMENT
      *    END-UNSTRING.
      *
      *RR-ACCEPT-ELEMENT SECTION.
      *    MOVE SPACES TO VDU VDU-ATTRIBUTES.
      *    INSPECT WR-ELEMENT REPLACING FIRST " " BY ".".
      *    UNSTRING WR-ELEMENT
      *             DELIMITED BY ":"
      *             INTO VDUFIELD
      *                  VDU-ATTRIBUTES.
      *    INSPECT VDUFIELD REPLACING ALL "." BY " ".
      *    INSPECT VDUFIELD REPLACING FIRST " " BY ".".
      *    MOVE VDUFIELD TO WR-ELEMENT.
      *    PERFORM  VDU-CONVERT-FIELD.
      *    IF STAT NOT = "00"
      *       ADD 1 TO WR-STAT
      *    ELSE
      *       MOVE VDU-SIZE TO WR-SIZEX
      *       MOVE SPACES TO VDU
      *       STRING "E  A000"
      *               WR-ELEMENT
      *               DELIMITED BY SIZE
      *               INTO VDU
      *        END-STRING
      *        PERFORM SCREENIO
      *        MOVE VDUBUF(1:WR-SIZEX) TO WR-BUF(WR-START:WR-SIZEX).
      *        ADD WR-SIZEX TO WR-START
      *        MOVE 0 TO WR-SIZEX
      *------->ADD 1 TO WRSUB.

      *-----------------------------------------------------------------
      * CALL XFER  : FOR ALL PRACTICAL PURPOSES, XFER AND WRFORM ARE
      * CALL WRFORM: EQUIVALENT WITH THIS ENVIRONMENT.  YOU CAN CALL
      *              EITHER ONE BUT THEY BOTH WILL DO THE SAME THING.
      *
      *  IN : WR-LIST
      *       WR-BUF
      *
      *  OUT: STAT   (CONTAINS # OF FIELDS THAT DID NOT DISPLAY)
      *              (I.E. THEY ARE NON-DISPLAY OR THE FIELD DOES NOT EXIST)
      *
      *  CALL:   MOVE ???-BUF TO WR-BUF.          MOVE ???-BUF TO WR-BUF.
      *          MOVE ???-LIST TO WR-LIST.  -OR-  MOVE ???-LIST TO WR-LIST.
      *          PERFORM CALL-WRFORM.             PERFORM CALL-XFER.
      *
      *          
      * MJD 121018 REMOVED CALL-XFER.   DECIDED IN LAST MEETING THAT WE WILL
      *            ONLY USE WRFORM GOING FORWARD.   XFER HAD TOO MANY TIES TO
      *            NON DISPLAYS IN THE OLD SCREEN HANDLER.
      *-----------------------------------------------------------------
      *CALL-XFER SECTION.
      *    PERFORM CALL-WRFORM.
       CALL-WRFORM SECTION.

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE "00" TO STAT
              MOVE SPACES TO WR-BUF WR-LIST
              GO TO CALL-WRFORM-EXIT.         *> SKIP DISPLAY TO SCREEN

           MOVE 00 TO WR-STAT.
           CALL "C$TOUPPER" USING WR-LIST, VALUE WR-MAX.
           INSPECT WR-LIST REPLACING ALL "." BY " ".
           MOVE 1 TO WR-START WR-PTR.
           SET WR-LIST-SIZE TO SIZE OF WR-LIST.
           MOVE "N" TO WR-LIST-EMPTY.
           PERFORM WR-PROCESS-ELEMENTS UNTIL WR-NO-MORE-ELEMENTS.
           MOVE WR-STATX TO STAT.
           MOVE SPACES TO WR-BUF WR-LIST.

       CALL-WRFORM-EXIT.
           EXIT.


       WR-PROCESS-ELEMENTS SECTION.
           UNSTRING WR-LIST
                 DELIMITED BY ALL SPACE
                 INTO WR-ELEMENT
                 POINTER WR-PTR
                 ON OVERFLOW
                    IF WR-PTR > WR-LIST-SIZE
                       MOVE "Y" TO WR-LIST-EMPTY
                    END-IF
           PERFORM WR-DISPLAY-ELEMENT
           MOVE SPACES TO WR-ELEMENT
           END-UNSTRING.

       WR-DISPLAY-ELEMENT SECTION.
           MOVE SPACES TO VDU-FIELD-TO-CONVERT VDU-DISP-ATTRIBUTES.
           INSPECT WR-ELEMENT REPLACING FIRST " " BY ".".
           UNSTRING WR-ELEMENT
                    DELIMITED BY ":"
                    INTO VDU-FIELD-TO-CONVERT
                         VDU-DISP-ATTRIBUTES.
           INSPECT VDU-FIELD-TO-CONVERT REPLACING ALL "." BY " ".
           INSPECT VDU-FIELD-TO-CONVERT REPLACING FIRST " " BY ".".
           PERFORM  VDU-CONVERT-FIELD.
      *    IF VDU-SIZEX < 1 MOVE 1 TO VDU-SIZEX.
      * 2/13/2024 ADDED OR NOT NUMERIC
           IF VDU-SIZEX < 1 OR VDU-SIZEX NOT NUMERIC
              MOVE 1 TO VDU-SIZEX.
           IF STAT NOT = "00"
              ADD 1 TO WR-STAT
           ELSE
              IF VDU-POSX = ZEROS
                 MOVE VDU-SIZEX TO WR-SIZEX
                 MOVE WR-ELEMENT TO NDIS-LIST
                 MOVE WR-BUF(WR-START:WR-SIZEX) TO NDIS-BUF
                 MOVE "W" TO NDIS-ACTION
                 PERFORM VDU-NONDISPLAY-FIELD
              ELSE
                 MOVE VDU-SIZEX TO WR-SIZEX
                 MOVE SPACES TO VDU-DISP-PROCESS
                 MOVE "S  A000" TO VDU-DISP-PROCESS
                 MOVE SPACES TO VDU-DISP-BUF
                 MOVE WR-BUF(WR-START:WR-SIZEX) TO VDU-DISP-BUF
                 MOVE VDU-POSX TO VDU-DISP-POS
                 MOVE VDU-SIZEX TO VDU-DISP-SIZE
                 PERFORM DISPLAY-FIELD
              END-IF
              ADD WR-SIZEX TO WR-START
      *       MOVE 0 TO WR-SIZEX.
              MOVE 1 TO WR-SIZEX.

      *CALL-GETFM SECTION.
      *    PERFORM CALL-CRFORM.
      *    MOVE GETFM-ND-INIT-BUF  TO WR-BUF.
      *    MOVE GETFM-ND-INIT-LIST TO WR-LIST.
      *    PERFORM CALL-WRFORM.

      *CALL-CRFORM SECTION.
      *    PERFORM SETUP-LINK-INFO.
      *    MOVE "C" TO NDIS-ACTION.
      *    PERFORM VDU-NONDISPLAY-FIELD.

      *CALL-DLFORM SECTION.
      *    MOVE "D" TO NDIS-ACTION.
      *    PERFORM VDU-NONDISPLAY-FIELD.

      *****************************************************************
      * ACCEPT/DISPLAY OF NON-DISPLAY FIELDS
      *  NDIS-ACTION:    C = CREATE NEW FILE (OPEN-OUTPUT)
      *                  D = DELETE FILE (REMOVE-WORKFILE)
      *                  W = WRITE RECORD (WRITE/REWRITE)
      *                  R = READ RECORD
      *****************************************************************
       VDU-NONDISPLAY-FIELD SECTION.
      *    MOVE "UT/NONDSP" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATHNAME STAT NDIS-LINK-BUF
      *                         EXIT-PATHNAME.
      *    CANCEL FORM-PROGX.

      *****************************************************************
      * ACTUAL DISPLAY OF FIELDS WITH AND WITHOUT ATTRIBUTES
      *****************************************************************
       DISPLAY-FIELD SECTION.

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              GO TO DISPLAY-FIELD-EXIT.

           PERFORM SETUP-CONTROL.
           IF VDU-DISP-CLASS NOT = "A"
              IF VDUCLASS = "S"
                 IF VDU-NEG-FLAG = -1
                    CALL "C$JUSTIFY"
                          USING VDU-DISP-BUF(1:VDU-DISP-SIZE) "R"
                 ELSE
      *             IF SIGNED AND NON-NEG MAKE SURE WE LEAVE TRAILING
      *            SPACE ELSE NUMNERS WILL NOT BE ALIGNED ON THE SCREEN.
                    CALL "C$JUSTIFY"
                          USING VDU-DISP-BUF(1:VDU-DISP-SIZE - 1) "R"
                 END-IF
              ELSE
                 CALL "C$JUSTIFY"
                           USING VDU-DISP-BUF(1:VDU-DISP-SIZE) "R".

      * NEED TEST OF NO-ECHO HERE ELSE WE ARE REDISPLAYING SECURE FIELDS
      * IMMEDIATELY AFTER WE ACCEPT THEM.
           IF VDU-AT-ECHO
              DISPLAY VDU-DISP-BUF(1:VDU-DISP-SIZE)
                 AT VDU-DISP-POS
                 SIZE VDU-DISP-SIZE
                 WITH NO ADVANCING
                 CONTROL VDU-CONTROL
              END-DISPLAY.
           INITIALIZE VDU-CONTROL VDU-HLD-CONTROL VDU-ATTRIBUTES
                      VDU-SET-PROMPT-VAL VDU-SET-FCOLOR-VAL
                      VDU-SET-BCOLOR-VAL.

       DISPLAY-FIELD-EXIT.
           EXIT.

      *------------------------------------------------
      * VDUBUF RJUS SHIFT
      *  LOOK AT ATTRIBUTE FROM OLD FORM.  IF THE RJUS = "Y"
      *   THEN SHIFY
      *------------------------------------------------
       VDU-SHIFT-RJUS SECTION.
           MOVE SPACES TO VDU-TEMP-BUF.
           MOVE 0 TO VDU-SUBA.
           INSPECT VDU-BUF-AREA(1:VDU-SIZEX)
                   TALLYING VDU-SUBA FOR TRAILING " ".
           COMPUTE VDU-ACTUAL-LENGTH = VDU-SIZEX - VDU-SUBA.
           COMPUTE VDU-NEW-START = VDU-SIZEX - VDU-ACTUAL-LENGTH + 1.
           IF VDU-ACTUAL-LENGTH > 0
              MOVE VDU-BUF-AREA(1:VDU-ACTUAL-LENGTH) TO
                   VDU-TEMP-BUF(VDU-NEW-START:VDU-ACTUAL-LENGTH)
           END-IF.
           MOVE VDU-TEMP-BUF TO VDU-BUF-AREA.

      ******************************************************************
      *
      *    V D U - S H I F T - M U S T
      *
      *=================================================================
      * IN  : VDU-BUF-AREA
      * OUT : VDU-BUF-AREA
      * DESC: FOR CONTROL OF FORM FIELD CASE SENSITIVITY WITH SQL IO
      *       OPERATIONS.  THE USE OF THE FORM FIELD 'MUST' ATTRIBUTE
      *       WILL CONTROL THE USER'S FIELD ENTRY. IF 'MUST' ATTRIBUTE
      *       EQUALS "Y" ALLOWS LOWERCASE ENTRY OTHERWISE "N" WILL
      *       FORCE THE USER'S ENTRY TO UPPERCASE.
      *       THIS WAS NEEDED DUE TO THE SQL DATABASE BEING
      *       CASE INSENSITIVE, SO WE MUST FORCE USER ENTRY TO UPPERCASE
      *       SO THAT DATA RECEIVED MATCHES COBOL CONDITIONAL STATEMENT.
      *       FOR EXAMPLE, IF A USER ENTERS A LOWERCASE CODE FIELD, THE
      *       SQL IO OPERATION WILL RETURN A GOOD STATUS EVENTHOUGH IT
      *       FOUND AN UPPERCASE VALUE AND NOT A LOWERCASE VALUE.
      *       USING THE VDU FORM EDITOR (UF.SH), IN THE FIELD EDIT
      *       WINDOW, SETTING THE 'MUST' VALUE TO "Y" WILL ALLOW
      *       A USER'S ENTRY TO REMAIN CASE INSENSITIVE (LOWERCASE;
      *       MUST KEEP ENTRY AS IS).  THE DEFAULT VALUE OF 'MUST' IS
      *       "N" THUS WILL FORCE ENTRIES TO UPPERCASE TO KEEP
      *       COBOL CONDITIONAL EVALUATIONS THE SAME AS IN A15.
      *       CREATED BY JKC 2023-0922.
      ******************************************************************
       VDU-SHIFT-MUST SECTION.
           CALL "C$TOUPPER" USING VDU-BUF-AREA, VALUE 80.

      ******************************************************************
       RESET-MESS SECTION.
           MOVE MESS TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
