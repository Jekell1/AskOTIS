      * A30 CONVERSION STARTED
      * COPYMEMBER: LIBLP/LPMENU
      **************************************************************************
      *
      *   L O A N - S U B M E N U
      *
      * REV:
      *  JTG 020394 ADDED LOGIC FOR MAIL MESSAGE
      *  JTG 101597 ADDED OPTIONAL DIRECT LOAN SUBMENU, RE: BR-BALLOON-LOANS-FG
      *  JTG 101298 ADDED OPTIONAL SALES FIN. SUBMENU, RE: BR-REVOLVING-SF-FG
      *             AND BR-REVOLVING-LN-FG
      *  JTG 990610 ALLOWED BR-REVOLVING-LN-FG TO WORK CORRECTLY WITH BALLOONS
      *  GLB 010702 ADDED SELECTION 7. BATCH CHECK PRINTING
      *  BAH 050715 ADDED TEST OF BR-STATE = "MX" TO GET SPANISH FORMS FROM
      *             "SM" DIRECTORY INSTEAD OF "FM", WORLD PR# 414
      *             ALSO HARD CODED WHICH FORMS TO CHECK FOR
      *   CS 081218 GP-INCL-OTH-IN-EARNED-R EXCLUDES ANTICERN(6) AND ANTICERN(4)
      *             FROM ANTICIPATED EARNINGS.  REGACC USES ANTICERN TO
      *             AMORTIZE PARTICIPATION EXPENSE. REGACC DOES NOT DO DIRECT
      *             LOAN & WE COULD MESS OTHER PEOPLE UP IF WE ALLOW THEM TO
      *             USE THIS OPTION AND THEY DO DIRECT LOAN WITH MAINTFEE THAT
      *             SHOULD UPDATE ANTICIPATED REGACC PR#210
      *  MJD 121214 ADDED TCLP-MAIN-WINDOW-HANDLE TO DISPLAYS
      *  BAH 140117 ADDED "E RA" RESET FOR CLEANER INPUT
      *  BAH 190124 REMOVED REVOLVING SELECTION FROM DIRECT AND SALES
      *  BAH 20190625 REMOVED BR-REVOLVING-SF-FG BR-REVOLVING-LN-FG PT0153
      *  BAH 20211022 REMOVED LPMNM3 MENU WHICH CONTAINED REVOLVING OPTION
      *  BAH 20220314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      **************************************************************************
      *    M E N U - S E L E C T I O N
      ******************************************************************
       MENU-SELECTION SECTION.

           MOVE "N" TO MENU-RESUME.
           MOVE 1 TO MENU-FG.

           IF NEW-SUB-MENU-FORM = " "
              PERFORM MENU-SELECTION-ENTER
           ELSE
              MOVE NEW-SUB-MENU-FORM TO MENU-NEW-PROG-FORM
              PERFORM MENU-SELECTION-ENTER-SUB
              IF ERRCD = "X"
                 PERFORM MENU-SELECTION-ENTER.

      ******************************************************************
      *    M E N U - S E L E C T I O N - E N T E R
      ******************************************************************
       MENU-SELECTION-ENTER SECTION.

           MOVE " " TO ERRCD.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LP/LPSMNU" TO FORM-NAM.
           MOVE "LPSMNU" TO VDU-FORM-NAME.

           PERFORM MENU-SELECTION-ENTER-GTFORM.

      *-----------------------------------------------------------------
       MENU-SELECTION-ENTER-NEXT.
      *    MOVE "E  A060SEL." TO VDU.
      * ADDED RESET 1/17/2014 BAH
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO MENU-SELECTION-ENTER-EXIT.

           IF VDUSTATUS NOT = "00"
              GO TO MENU-SELECTION-ENTER-NEXT.

           MOVE VDUBUF TO MENU-FIELD.

           IF MENU-5 NOT NUMERIC
              MOVE 0 TO MENU-5.

           IF MENU-NO NOT NUMERIC
              GO TO MENU-SELECTION-ENTER-NEXT.

           IF MENU-NO = 0 OR MENU-NO > MENU-MAX
              GO TO MENU-SELECTION-ENTER-NEXT.

           IF (GP-INCL-OTH-IN-EARNED-R AND MENU-NO = 3)
                MOVE "NOT ALLOWED WHEN INCLUDE OTHER IN EARNED = 'R'"
                                                          TO MESS
                PERFORM SEND-MESS
                GO TO MENU-SELECTION-ENTER-NEXT.

                    
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE VDU-FORM-NAME
              WHEN "LPLNMU"
                       DESTROY LPLNMU-SCREEN
              WHEN "LPSMNU"
                       DESTROY LPSMNU-SCREEN
              WHEN "LPCAMU"
                       DESTROY LPCAMU-SCREEN
              WHEN "LPVDMU" DESTROY LPVDMU-SCREEN
           END-EVALUATE.

           MOVE MENU-PROG-FORM(MENU-NO) TO MENU-NEW-PROG-FORM.
           MOVE " " TO NEW-SUB-MENU-FORM.

           IF ( MENU-NEW-PROG = "I" OR "N" ) OR
              ( MENU-NEW-PROG = "C" AND BR-BALLOON-LOANS-FG = "Y" ) 
              MOVE MENU-NEW-PROG-FORM TO NEW-SUB-MENU-FORM
              PERFORM MENU-SELECTION-ENTER-SUB
              IF ( ERRCD = "X" )
                 GO TO MENU-SELECTION-ENTER
              ELSE
                 MOVE " " TO ERRCD.

           MOVE MENU-NEW-PROG-FORM TO NEW-PROG-FORM.

      *-----------------------------------------------------------------
       MENU-SELECTION-ENTER-EXIT.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPSMNU-SCREEN.

      ******************************************************************
      *    M E N U - S E L E C T I O N - E N T E R - S U B
      ******************************************************************
       MENU-SELECTION-ENTER-SUB SECTION.

           MOVE " " TO ERRCD.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * DIRECT LOAN OPTIONAL MENU WITH AND/OR BALLOONS 
           IF (MENU-NEW-PROG = "C" AND BR-BALLOON-LOANS-FG = "Y")
              MOVE "LP/LPLNMU" TO FORM-NAM
              MOVE "LPLNMU"    TO VDU-FORM-NAME
           ELSE
      * VOID ACCOUNTS MENU:
           IF ( MENU-NEW-PROG = "N" )
              MOVE "LP/LPVDMU" TO FORM-NAM
              MOVE "LPVDMU"    TO VDU-FORM-NAME
           ELSE
      * CASH & CHECK WRITING MENU:
           IF ( MENU-NEW-PROG = "I" )
              MOVE "LP/LPCAMU" TO FORM-NAM
              MOVE "LPCAMU"    TO VDU-FORM-NAME.

           PERFORM MENU-SELECTION-ENTER-GTFORM.

      *-----------------------------------------------------------------
       MENU-SELECTION-ENTER-SUB-NEXT.

           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE 0 TO LOAN-KEY.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO MENU-SELECTION-ENTER-SUB-EXIT.

           IF VDUSTATUS NOT = "00"
              GO TO MENU-SELECTION-ENTER-SUB-NEXT.

           MOVE VDUBUF TO MENU-FIELD.

           IF MENU-5 NOT NUMERIC
              MOVE 0 TO MENU-5.

           IF MENU-NO NOT NUMERIC
              GO TO MENU-SELECTION-ENTER-SUB-NEXT.

           IF MENU-NO = 0
              GO TO MENU-SELECTION-ENTER-SUB-NEXT.

           IF MENU-NEW-PROG = "B"
              IF MENU-NO > 2
                 GO TO MENU-SELECTION-ENTER-SUB-NEXT
              ELSE
                 MOVE MENU-NO TO TRANS-CODES
                 GO TO MENU-SELECTION-ENTER-SUB-EXIT.

           IF MENU-NEW-PROG = "C"
              IF BR-BALLOON-LOANS-FG = "Y"
                 IF MENU-NO > 2
                    GO TO MENU-SELECTION-ENTER-SUB-NEXT
                 END-IF
              END-IF
              MOVE MENU-NO TO TRANS-CODES
              GO TO MENU-SELECTION-ENTER-SUB-EXIT.

           IF MENU-NEW-PROG = "I"
              IF MENU-NO > 7
                 GO TO MENU-SELECTION-ENTER-SUB-NEXT
              ELSE
                 SUBTRACT 1 FROM MENU-NO GIVING TRANS-CODES
                 GO TO MENU-SELECTION-ENTER-SUB-EXIT.

           IF MENU-NEW-PROG = "N"
              IF MENU-NO > 2
                 GO TO MENU-SELECTION-ENTER-SUB-NEXT
              ELSE
                 IF MENU-NO = 1
                    MOVE "J0J" TO MENU-NEW-PROG-FORM
                 ELSE
                    IF MENU-NO = 2
                       MOVE "N0N" TO MENU-NEW-PROG-FORM.

      *-----------------------------------------------------------------
       MENU-SELECTION-ENTER-SUB-EXIT.
           MOVE MENU-NEW-PROG-FORM TO NEW-PROG-FORM.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE VDU-FORM-NAME
              WHEN "LPLNMU" 
                       DESTROY LPLNMU-SCREEN
              WHEN "LPSMNU"
                       DESTROY LPSMNU-SCREEN
              WHEN "LPCAMU"
                       DESTROY LPCAMU-SCREEN
              WHEN "LPVDMU" DESTROY LPVDMU-SCREEN
           END-EVALUATE.

      ******************************************************************
      *    M E N U - S E L E C T I O N - E N T E R - G T F O R M
      ******************************************************************
       MENU-SELECTION-ENTER-GTFORM SECTION.

           IF FORM-PROG = " "
              MOVE FORM-PROGX TO FORM-PROG.

           IF VDU-FORM-NAME = "LPLNMU"
              PERFORM LPLNMU-FORM-RESET
           ELSE
           IF VDU-FORM-NAME = "LPCAMU"
              PERFORM LPCAMU-FORM-RESET
           ELSE
           IF VDU-FORM-NAME = "LPVDMU"
              PERFORM LPVDMU-FORM-RESET
           ELSE
              PERFORM LPSMNU-FORM-RESET.

      *    CALL XFER USING STAT PROG-LIST PROG-BUF.

      * COL 1 POS 1 = CURDATE
           MOVE "S  A080CURDATE." TO VDU.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "CO" TO FORM-OBJ.

       LPSMNU-FORM-RESET SECTION.
           MOVE LPLOAN-CONAME TO LPSMNU-CONAME.
           MOVE LPLOAN-SYSDATE TO LPSMNU-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LPSMNU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPSMNU-FORM-FIELDS TO WR-BUF.
           MOVE LPSMNU-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.


       LPLNMU-FORM-RESET SECTION.
           MOVE LPLOAN-CONAME TO LPLNMU-CONAME.
           MOVE LPLOAN-SYSDATE TO LPLNMU-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LPLNMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPLNMU-FORM-FIELDS TO WR-BUF.
           MOVE LPLNMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


       LPCAMU-FORM-RESET SECTION.
           MOVE LPLOAN-CONAME TO LPCAMU-CONAME.
           MOVE LPLOAN-SYSDATE TO LPCAMU-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LPCAMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPCAMU-FORM-FIELDS TO WR-BUF.
           MOVE LPCAMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       LPVDMU-FORM-RESET SECTION.
           MOVE LPLOAN-CONAME TO LPVDMU-CONAME.
           MOVE LPLOAN-SYSDATE TO LPVDMU-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LPVDMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPVDMU-FORM-FIELDS TO WR-BUF.
           MOVE LPVDMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

