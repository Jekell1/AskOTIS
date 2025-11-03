       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAKEIX.
       DATE-WRITTEN.  072991
      ******************************************************************
      *
      * DESCRIPTION: RECONSTRUCTION OF IXFILE
      *
      *=================================================================
      * REV:
      * BAH 160115 CHANGE LTFILE TO LTCFILE, PD0003
      *
      * KEC 2017.0221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED IX-BRNO TO IX-REC.
      *          ADDED CREAIX-BRNO TO CREAIX-AREA.
      * 181207 GLOBAL LTCFILE
      * 181211 GLOBAL IXFILE
      * BAH 2019.0918 REMOVED ACCESS-CALL ON IXFILE
      * BAH 2024.1002 SEND-LEGEND WASNT DISPLAYING "IN PROGRESS" DUE TO NOT
      *               USING MESS-BUF S35Q-136
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   3B.
       OBJECT-COMPUTER.   3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *************************************************
      *     WORKING STORAGE SECTION
      *************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)FX/MAKEIX S35 07/11/22-13:39:23 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".

       01  BRANCH            PIC 9999.

       01  ERRCD                   PIC X    VALUE "N".

       01  ACCEPT-BUF              PIC X.

       01  MESS-LIST               PIC X(5)  VALUE "MESS.".
       01  MESS-BUF                PIC X(49) VALUE SPACES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  MAKEIX-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/MAKEIX_DEF.CPY".
       COPY "LIBFX/MAKEIX_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/MAKEIX_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *    M A I N   P R O G R A M   M O D U L E
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MAKEIX" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS MAKEIX-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE " " TO ERRCD.
       ENTER-BRANCH.
           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16"
              MOVE 0 TO BRANCH
              MOVE "S  A040BRNO." TO VDU
              MOVE "*ALL" TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ACCEPT.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BRANCH.
           MOVE VDUNUM0 TO BRANCH.
           IF BRANCH = 0
              GO TO ENTER-BRANCH.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO END-ROUTINE.
           IF NOT (ACCEPT-BUF = "Y" OR "S")
              GO TO ENTER-ACCEPT.

           PERFORM OPEN-BR-FILE.
           IF BRANCH = 0
              MOVE 0 TO BR-NO
           ELSE
              MOVE BRANCH TO BR-NO.
           PERFORM START-BR-FILE.

       NEXT-BR.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF BRANCH NOT = 0
              IF BR-NO NOT = BRANCH
                 GO TO END-ROUTINE.

       NEXT-BR-ACCEPT.
           IF ACCEPT-BUF = "S"
              MOVE "A" TO ACCEPT-BUF
           ELSE
           IF ACCEPT-BUF = "A"
              MOVE "E  A010ACCEPT." TO VDU
              PERFORM SCREENIO
              IF VDUSTATUS = "1>"
                 GO TO END-ROUTINE
              ELSE
                 IF NOT (VDUBUF = "Y" OR "S")
                    GO TO NEXT-BR-ACCEPT.

           IF BR-DATA-PATH = " " OR BR-MACHINE = " "
              STRING "BRANCH " BR-NO
                     " MISSING DATA PATH, WILL SKIP"
                                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO NEXT-BR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO IX-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE.

           STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
                           DELIMITED BY SIZE INTO MESS-BUF.
           PERFORM SEND-LEGEND.

           PERFORM GLOBAL-UPDATE.

           GO TO NEXT-BR.

      ******************************************
       GLOBAL-UPDATE SECTION.
      ******************************************

      * REMOVE OLD IXFILE:

      *SINCE THIS IS NOW A GLOBAL FILE, YOU CAN NOT JUST NUKE IT BECAUSE
      *A SINGLE BRANCH MAY HAVE BEEN ENTERED TO RUN THE FIX AGAINST.
      *IN PRIOR RELEASES, IT NUKED THE LOCAL FILE, REBUILT IT AND MOVED
      *ON TO THE NEXT BRANCH

           PERFORM PURGE-OPEN-OUTPUT-IX.  

           PERFORM REBUILD-IXFILE.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P U R G E - O P E N - O U T P U T - I X
      *
      *=================================================================
      *    THIS FILE WAS LOCAL (A15/A30) BUT IS NOW GLOBAL.
      *    CANNOT DO AN OPEN OUTPUT ON THE FILE LIKE IS WAS LOCAL.
      *    NOW DOING THIS STANDARD ROUTINE FOR PURGE RECORDS FOR
      *    ONLY THE GIVEN BRANCH.
      ******************************************************************
       PURGE-OPEN-OUTPUT-IX SECTION.

           MOVE BR-NO                     TO IX-BRNO.
           MOVE "C"                       TO IX-CODE.
           MOVE 1                         TO IX-ACCTNO.
           MOVE 1                         TO IX-SEQNO.
           PERFORM OPEN-IX1-FILE.

           PERFORM START-IX1-FILE.

       PURGE-OPEN-OUTPUT-IX-NEXT.

           PERFORM READ-IX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IX-BRNO NOT = IX-PATH-OWNBR )
              PERFORM CLOSE-IX1-FILE
              GO TO PURGE-OPEN-OUTPUT-IX-EXIT.

           PERFORM DELETE-IX1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-IX-NEXT.
            
       PURGE-OPEN-OUTPUT-IX-EXIT.
           EXIT.

      ***********************************************
       REBUILD-IXFILE SECTION.
      ***********************************************
           PERFORM OPEN-LTC1-FILE.

           MOVE LTC-PATH-OWNBR TO LTC-BRNO.
           MOVE ZEROS          TO LTC-ACCTNO.
           MOVE ZEROS          TO LTC-SEQNO.

           PERFORM START-LTC1-FILE.
       REBUILD-IX-LOOP.
           PERFORM READ-LTC1-FILE-NEXT.
           IF (IO-FG = 9) OR (LTC-BRNO NOT = LTC-PATH-OWNBR)
              GO TO END-REBUILD-IX.

           PERFORM UPDATE-LT-C-IX.

           GO TO REBUILD-IX-LOOP.

       END-REBUILD-IX.
           PERFORM CLOSE-LTC1-FILE.

      ***********************************************
       UPDATE-LT-C-IX SECTION.
      ***********************************************
           MOVE "W" TO CREAIX-AREA.
           MOVE "E" TO CREAIX-ERRCD.
           MOVE LTC-BRNO   TO CREAIX-BRNO.
           MOVE "C"        TO CREAIX-CODE.
           MOVE LTC-ACCTNO TO CREAIX-LTC-ACCTNO.
           MOVE LTC-SEQNO  TO CREAIX-LTC-SEQNO.
           MOVE LTC-VIN(1) TO CREAIX-LTC-VIN(1).
           MOVE LTC-VIN(2) TO CREAIX-LTC-VIN(2).
           MOVE LTC-VIN(3) TO CREAIX-LTC-VIN(3).
      *DO NOT MOVE IX-SELECT-PATH HERE, WHEN YOU GET TO CL/CREAIX IT 
      *OPENS THE IX AND SETS THE PATH CORRECT. IF YOU DO IT HERE, YOU
      *ARE MUCKED UP WHEN YOU GET TO THE OPEN IS CL/CREAIX
           MOVE IX-PATH TO CREAIX-IX-PATH.
           MOVE "CL/CREAIX" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CREAIX-AREA.
           CANCEL FORM-PROGX.
           IF CREAIX-ERRCD NOT = " "
              MOVE "ERROR UPDATING LT-ID IXFILE" TO MESS
              PERFORM SEND-MESS.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO MAKEIX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO MAKEIX-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY MAKEIX-SCREEN UPON MAKEIX-WINDOW-HANDLE.
           MOVE MAKEIX-FORM-FIELDS TO WR-BUF.
           MOVE MAKEIX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE MAKEIX-HELP--FILE TO HELP-LINK-FILE.
           MOVE MAKEIX-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/MAKEIX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARAGRAPHS SECTION.
      ***********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS-BUF  TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       IO-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-IX1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPIX1IN.CPY".

       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************************
       END-ROUTINE SECTION.
      ***********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY MAKEIX-SCREEN.        
           DESTROY MAKEIX-WINDOW-HANDLE.
           EXIT PROGRAM.
