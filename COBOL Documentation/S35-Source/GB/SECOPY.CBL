       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SECOPY.
       DATE-WRITTEN.    111210.
      ******************************************************************
      *
      *                 (GB)
      *  DESCRIPTION:   USER SECURITY PROFILE COPY/SET UTILITY
      *
      *=================================================================
      * REV:
      *     101115 *NEW SECURITY*
      *
      * KEC 2015.1116 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED USE OF SE-DIR & SN-DIR
      *  BAH 20220218 CALL NEW AUDIT/CHANGE PROGRAM, AUDITW, #1542
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/SECOPY.CBL A30 11/16/15-10:33:27 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GB01SE_SQL.CPY".
       COPY "LIBGB/GBWSSE.CPY".

       COPY "LIBGB/GB01SE.CPY" REPLACING LEADING "SE" BY "SEOR".

      **********************
      *   MISC WORKERS     *
      **********************
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  SEPROFILEW           PIC X.
       01  S1                   PIC 99.
       01  S2                   PIC 99.

       01  ERRCD           PIC X.
       01  TARGET-PROF     PIC X.
       01  ACTION-TYPE     PIC 9.
       01  SOURCE-PROF     PIC X.


       COPY "LIBCL/AUDITWW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  SECOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/SECOPY_DEF.CPY".
       COPY "LIBGB/SECOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGB/SECOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
       MAIN-MODULE SECTION.
      ******************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SECOPY" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

      D    STOP MESS.
           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-SE1.
           PERFORM READ-SE1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE SE-REC TO SEOR-REC.

           IF ACTION-TYPE = 1
              MOVE SOURCE-PROF TO SEPROFILEW
              PERFORM SET-PROFILE-SUB
              MOVE SESUB TO S1
              MOVE TARGET-PROF TO SEPROFILEW
              PERFORM SET-PROFILE-SUB
              MOVE SESUB TO S2
              MOVE SE-PROF(S1) TO SE-PROF(S2)
              GO TO UPDATE-SE1.

           MOVE TARGET-PROF TO SEPROFILEW.
           PERFORM SET-PROFILE-SUB.
           MOVE SESUB TO S1.

           IF ACTION-TYPE = 2
              IF SE-PROF(S1) NOT = "Y"
                 MOVE "Y" TO SE-PROF(S1)
              ELSE
                 GO TO GET-NEXT.

           IF ACTION-TYPE = 3
              IF SE-PROF(S1) NOT = "N"
                 MOVE "N" TO SE-PROF(S1)
              ELSE
                 GO TO GET-NEXT.

       UPDATE-SE1.
           PERFORM REWRITE-SE1-FILE.
           IF SE-REC NOT = SEOR-REC
              MOVE "C" TO AUDITW-CHANGE-TYPE
              PERFORM UPDATE-AUDITW
              MOVE "N" TO AUDITW-ENTER-TICKET.
      * ONLY WANT THE TICKET ENTER ONCE FOR THIS LARGE CHANGE


       GET-NEXT.
           GO TO NEXT-SE1.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

       ENTER-TARGET.
           MOVE "E  A010TARGET." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-TARGET.
           IF F7-KEY GO TO ENDIT.
           IF NOT (DOWN-KEY OR ENTER-KEY) GO TO ENTER-TARGET.

           MOVE VDUBUF TO TARGET-PROF.
           IF TARGET-PROF  < "B" GO TO ENTER-TARGET.
           IF TARGET-PROF  > "Z" GO TO ENTER-TARGET.

       ENTER-ACTION.
           MOVE "ENNN010ACTION." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-TARGET.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO ENTER-TARGET.
           IF NOT (DOWN-KEY OR ENTER-KEY) GO TO ENTER-ACTION.

           MOVE VDUNUM0 TO ACTION-TYPE.

           IF NOT (ACTION-TYPE = 1 OR 2 OR 3)
               MOVE "ENTER 1, 2, OR 3" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ACTION.

       ENTER-SOURCE.
           IF ACTION-TYPE NOT = 1
              GO TO OK-YN.

           MOVE "E  A010SOURCE." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-TARGET.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO ENTER-ACTION.
           IF NOT (DOWN-KEY OR ENTER-KEY) GO TO ENTER-SOURCE.

           MOVE VDUBUF TO SOURCE-PROF.

           IF SOURCE-PROF = TARGET-PROF
              MOVE " SOURCE AND TARGET CANNOT BE THE SAME" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE.

           IF SOURCE-PROF  < "B" GO TO ENTER-SOURCE.
           IF SOURCE-PROF  > "Z" GO TO ENTER-SOURCE.

      *-----------------------------------------------------------------
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-SOURCE. 
           IF F1-KEY GO TO ENTER-TARGET.
           IF F7-KEY GO TO ENDIT.

           IF NOT (VDUBUF = "Y" OR "N")
               GO TO OK-YN.

           IF VDUBUF = "N" GO TO ENTER-TARGET.

           PERFORM OPEN-SE1-FILE.

           MOVE SPACES TO  SE-FORM.

           PERFORM START-SE1-FILE.

           MOVE " " TO ERRCD.

           IF ACTION-TYPE = 1
              MOVE "PROFILE COPY IN PROGRESS............." TO MESS
           ELSE
           IF ACTION-TYPE = 2
              MOVE "MOVING 'N' TO TARGET................." TO MESS
           ELSE
              MOVE "MOVING 'Y' TO TARGET..................." TO MESS.
           PERFORM DISPLAY-MESS.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SECOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SECOPY-SYSDATE.
           DISPLAY SECOPY-SCREEN UPON SECOPY-WINDOW-HANDLE.
           MOVE SECOPY-FORM-FIELDS TO WR-BUF.
           MOVE SECOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SECOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE SECOPY-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/SECOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC-PARA SECTION.
      ****************************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-PROFILE-SUB.
           EVALUATE SEPROFILEW
              WHEN 'A' MOVE 1 TO SESUB
              WHEN 'B' MOVE 2 TO SESUB
              WHEN 'C' MOVE 3 TO SESUB
              WHEN 'D' MOVE 4 TO SESUB
              WHEN 'E' MOVE 5 TO SESUB
              WHEN 'F' MOVE 6 TO SESUB
              WHEN 'G' MOVE 7 TO SESUB
              WHEN 'H' MOVE 8 TO SESUB
              WHEN 'I' MOVE 9 TO SESUB
              WHEN 'J' MOVE 10 TO SESUB
              WHEN 'K' MOVE 11 TO SESUB
              WHEN 'L' MOVE 12 TO SESUB
              WHEN 'M' MOVE 13 TO SESUB
              WHEN 'N' MOVE 14 TO SESUB
              WHEN 'O' MOVE 15 TO SESUB
              WHEN 'P' MOVE 16 TO SESUB
              WHEN 'Q' MOVE 17 TO SESUB
              WHEN 'R' MOVE 18 TO SESUB
              WHEN 'S' MOVE 19 TO SESUB
              WHEN 'T' MOVE 20 TO SESUB
              WHEN 'U' MOVE 21 TO SESUB
              WHEN 'V' MOVE 22 TO SESUB
              WHEN 'W' MOVE 23 TO SESUB
              WHEN 'X' MOVE 24 TO SESUB
              WHEN 'Y' MOVE 25 TO SESUB
              WHEN 'Z' MOVE 26 TO SESUB
              WHEN OTHER PERFORM INVALID-CODE.

       INVALID-CODE.
           MOVE " ENTER 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.

       UPDATE-AUDITW.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE FORM-PROGX     TO AUDITW-PROGRAM.
           MOVE "AUDITW"       TO FORM-NAM.
           MOVE SEOR-REC       TO AUDITW-ORIG-REC.
           MOVE SE-REC         TO AUDITW-CHG-REC.
           MOVE "SEFILE"       TO AUDITW-FILE-NAME.
           CALL FORM-PATH USING FORM-PATH AUDITW-WORKERS.
           CANCEL FORM-PATH.

      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SE1-FILE.
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GB/SEMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SECOPY-SCREEN.
           DESTROY SECOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
