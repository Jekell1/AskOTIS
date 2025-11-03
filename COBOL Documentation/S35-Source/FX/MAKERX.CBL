       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAKERX.
       DATE-WRITTEN.  122696
      ******************************************************************
      *
      *                (FX)
      *   DESCRIPTION: RECONSTRUCTION OF RX-FILE (RESCISSION INDEX FILE)
      *
      *
      * REV:
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * BAH 181002 GLOBAL BWFILE
      * BAH 181203 GLOBAL LNFILE
      * AMM 190412 GLOBAL RXFILE
      * BAH 20190730 DO NOT OPEN OUTPUT, REMOVE SINGLE BRANCH
      * BAH 190918 REMOVED ACCESS-CALL ON BWFILE, LNFILE, RXFILE
      * BAH 2024.1003 SEND-LEGEND WASNT DISPLAYING "IN PROGRESS" DUE TO NOT
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./FX/MAKERX.CBL A32 04/12/19-13:35:34 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".


      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  ACCEPT-BUF              PIC X.
       01  BRANCH                  PIC 9999.
       01  BRANCH-ENTER            PIC X(1)    VALUE "S".
           88  BRANCH-ALL                      VALUE "A".
           88  BRANCH-SINGLE                   VALUE "S".

       01  TEST-POCD               PIC XX.
       01  FILLER                  REDEFINES TEST-POCD.
           03  TEST-POCD1            PIC X.
           03  TEST-POCD2            PIC X.

       01  ERRCD                   PIC X       VALUE "N".

       01  MESS-BUF                PIC X(55)   VALUE SPACES.
       01  MESS-LIST               PIC X(5)    VALUE "MESS.".


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
      *COPY "LIBLP/LPSETRXW.CPY".
       01  MAKERX-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/MAKERX_DEF.CPY".
       COPY "LIBFX/MAKERX_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/MAKERX_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MAKERX" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS MAKERX-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE " " TO ERRCD.

       ENTER-BRANCH.

           MOVE "S"               TO BRANCH-ENTER.
           MOVE "F3 - ALL, F7 - EXIT, ENTER BRANCH NUMBER" TO MESS-BUF.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "16" )
              MOVE 0              TO BRANCH
              MOVE "A"            TO BRANCH-ENTER
              MOVE "S  A040BRNO." TO VDU
              MOVE "****"         TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A500BR-DESC."          TO VDU
              MOVE "***** ALL BRANCHES *****" TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ACCEPT.

           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BRANCH.

           IF ( BRANCH = ZEROES )
              GO TO ENTER-BRANCH.

           IF ( BRANCH-SINGLE )
              PERFORM GET-BRANCH-DESC.

           IF ( IO-FG NOT = ZEROES )
              GO TO ENTER-BRANCH.

      *-----------------------------------------------------------------
       ENTER-ACCEPT.

           MOVE "F7 - EXIT, ^/V, ACCEPT (Y)ES/(N)O???" TO MESS-BUF.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-ACCEPT.

           MOVE VDUBUF TO ACCEPT-BUF.

           IF ( ACCEPT-BUF = "N" )
              GO TO END-ROUTINE.

           IF NOT (ACCEPT-BUF = "Y" OR "S")
              GO TO ENTER-ACCEPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-BR-FILE.

           IF ( BRANCH = ZEROES )
              MOVE 0 TO BR-NO
           ELSE
              MOVE BRANCH TO BR-NO.

           PERFORM START-BR-FILE.

       NEXT-BR.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 )
              GO TO END-ROUTINE.

           IF ( BRANCH NOT = ZEROES )
              IF ( BR-NO NOT = BRANCH )
                 GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       NEXT-BR-ACCEPT.

           IF ( ACCEPT-BUF = "S" )
              MOVE "A" TO ACCEPT-BUF
           ELSE
           IF ( ACCEPT-BUF = "A" )
              MOVE "E  A010ACCEPT." TO VDU
              PERFORM SCREENIO
              IF ( VDUSTATUS = "1>" )
                 GO TO END-ROUTINE
              ELSE
                 IF NOT (VDUBUF = "Y" OR "S")
                    GO TO NEXT-BR-ACCEPT.

           IF ( BR-DATA-PATH = SPACES ) OR ( BR-MACHINE = SPACES )
              STRING "BRANCH ",BR-KEY,
                     " MISSING DATA PATH, WILL SKIP"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO NEXT-BR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              RX-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

           GO TO NEXT-BR.

      ******************************************************************
       GLOBAL-UPDATE SECTION.
      ******************************************************************

           IF ( BRANCH-ALL )
              PERFORM SEND-BRANCH-DESC.

      *SINCE THIS IS NOW A GLOBAL FILE, YOU CAN NOT JUST NUKE IT BECAUSE
      *A SINGLE BRANCH MAY HAVE BEEN ENTERED TO RUN THE FIX AGAINST.
      *IN PRIOR RELEASES, IT NUKED THE LOCAL FILE, REBUILT IT AND MOVED
      *ON TO THE NEXT BRANCH

           PERFORM PURGE-OPEN-OUTPUT-RX.

           PERFORM REBUILD-RXFILE.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P U R G E - O P E N - O U T P U T - R X
      *
      *=================================================================
      *    THIS FILE WAS LOCAL (A15/A30) BUT IS NOW GLOBAL.
      *    CANNOT DO AN OPEN OUTPUT ON THE FILE LIKE IS WAS LOCAL.
      *    NOW DOING THIS STANDARD ROUTINE FOR PURGE RECORDS FOR
      *    ONLY THE GIVEN BRANCH.
      ******************************************************************
       PURGE-OPEN-OUTPUT-RX SECTION.

           MOVE BR-NO                      TO RX-BRNO.
           MOVE ZEROES                     TO RX-ACCTNO.

           PERFORM OPEN-RX1-FILE.

           PERFORM START-RX1-FILE.

       PURGE-OPEN-OUTPUT-RX-NEXT.

           PERFORM READ-RX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( RX-BRNO NOT = RX-PATH-OWNBR )
              PERFORM CLOSE-RX1-FILE
              GO TO PURGE-OPEN-OUTPUT-RX-EXIT.

           PERFORM DELETE-RX1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-RX-NEXT.

       PURGE-OPEN-OUTPUT-RX-EXIT.
           EXIT.

      ******************************************************************
       REBUILD-RXFILE SECTION.
      ******************************************************************

           PERFORM OPEN-RX1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO TO LN-OWNBR.
           MOVE 0     TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       REBUILD-RX-LOOP.

           PERFORM READ-LN1-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO END-REBUILD-RX.

           STRING "PROGRAM IN PROGRESS......ACCOUNT NUMBER: ",LN-ACCTNO
                  DELIMITED BY SIZE INTO MESS-BUF.

           PERFORM SEND-LEGEND.

      * CHECK IF A RESCISSION LOAN
           IF ( NOT LN-POCD-RESCIND ) AND ( NOT LN-POCD-INRESCISSION )
              GO TO REBUILD-RX-LOOP.

           PERFORM UPDATE-RX-FILE.

           GO TO REBUILD-RX-LOOP.

       END-REBUILD-RX.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-RX1-FILE.

      ******************************************************************
       UPDATE-RX-FILE SECTION.
      ******************************************************************

           MOVE SPACES        TO RX-REC.

           MOVE LN-OWNBR      TO RX-BRNO.
           MOVE LN-ACCTNO     TO RX-ACCTNO.
           MOVE LN-DLNO       TO RX-DLNO.
           MOVE LN-INTDATE    TO RX-INTDATE.
           MOVE LN-LOANDATE   TO RX-LOANDATE.
           MOVE LN-POCD       TO TEST-POCD.
      * WOULD BE "I" INRESCISSION OR "R" RESCINDED
           MOVE TEST-POCD2    TO RX-STATUS.
           MOVE LN-LNAMT      TO RX-LOANAMT.
           MOVE LN-PROCEEDS   TO RX-PROCEEDS.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE SPACES             TO RX-FNAME
              MOVE "** NOT FOUND ** " TO RX-LNAME
           ELSE
              MOVE BW-FNAME           TO RX-FNAME
              MOVE BW-LNAME           TO RX-LNAME.

           PERFORM WRITE-RX1-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

       EXIT-UPDATE-RX-FILE.
           EXIT.

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO MAKERX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO MAKERX-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY MAKERX-SCREEN UPON MAKERX-WINDOW-HANDLE.
           MOVE MAKERX-FORM-FIELDS TO WR-BUF.
           MOVE MAKERX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE MAKERX-HELP--FILE TO HELP-LINK-FILE.
           MOVE MAKERX-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/MAKERX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-BRANCH-DESC.
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE "S  A500BR-DESC." TO VDU
              MOVE "*** NOT FOUND ***" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              PERFORM SEND-BRANCH-DESC.
       SEND-BRANCH-DESC.
           MOVE "S  A500BR-DESC." TO VDU.
           STRING BR-NO," - ",BR-NAME DELIMITED BY "  " INTO VDUBUF.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE MESS-BUF  TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPRX1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MAKERX-SCREEN.         
           DESTROY MAKERX-WINDOW-HANDLE.
           EXIT PROGRAM.
