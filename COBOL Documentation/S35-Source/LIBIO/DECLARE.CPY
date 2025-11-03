      * COPYMEMBER: LIBIO/DECLARE.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBIO/DECLARE
      * DESCRIPTION   : SQL DECLARE ROUTINES
      *                 NEEDED WHEN PROGRAM HAS NO VISION FILES AND
      *                 CANNOT USE STANDARD DECLARE COPY MEMBERS USED
      *                 IN PROCEDURE DIVISION DECLARATIVES.
      *
      *=================================================================
      * REV:
      * JKC 2021-0513 ***NEW***
      * JKC 2022-0222 IN SQL-IO-VALIDATION ROUTINE ADDED A CHECK TO SEE
      *               IF THE SQL CONNECTION (EXT-ACUSQL-CONNECT-STAT)
      *               IS OPEN (GOOD) THEN `PERFORM CLOSE-FILES`;
      *               NEED TO ONLY CLOSE FILES/TABLES WHEN THERE IS
      *               AN SQL CONNECTION OTHERWISE YOU CAN GET INTO
      *               AN INIFITE LOOP DUE TO THE SQL-DISCONNECT IN
      *               SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      *               IF THE CONNECTION HAS BEEN CLOSED.
      *
      ******************************************************************
       OPEN-IT.
           MOVE "OPEN ERROR" TO E-MSG.
           MOVE SPACES TO E-KEYX.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       READ-IT.
           MOVE "READ ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       WRITE-IT.
           MOVE "WRITE ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       REWRITE-IT.
           MOVE "REWRITE ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       DELETE-IT.
           MOVE "DELETE ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       START-IT.
           MOVE "START ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       CLOSE-IT.
           MOVE "CLOSE ERROR" TO E-MSG.
      *FORM-IT.
      *    MOVE "FORM ERROR" TO E-MSG.
      *    MOVE STAT TO E-STATUS-ERROR.
      *    MOVE SPACES TO E-KEYX.
      *SUSPEND.
      **    CALL SUSP.
      *    CALL "C$SLEEP" USING 1.
       ALARM.
      **    DISPLAY OMITTED WITH BEEP.
       FILE-ERRORS.
           MOVE E-FILE TO E-FILEX ERRLOGW-FILE.
           MOVE FORM-PATHNAME TO E-PROG ERRLOGW-PROGX.
           MOVE E-MSG TO ERRLOGW-TYPE.
           MOVE E-CODE TO ERRLOGW-CODE.
           MOVE E-SUBCODE TO ERRLOGW-SUBCODE.
           MOVE E-KEYX TO ERRLOGW-KEY.
           MOVE "E" TO ERRLOGW-ACTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE FORM-PATHNAME TO FORM-PATH.
           IF PIGGY-BACK-FLAG = "PIGGY"
              MOVE "SNDERR" TO PIGGY-BACK-FLAG
      *OLD    GO TO FILE-ERRORS-TERMINATE     *> OLD VISION
              PERFORM FILE-ERRORS-TERMINATE   *> NEW SQL
              GO TO EXIT-PROG.                *> NEW SQL
           PERFORM ALARM.
           IF E-MSG4 = "OPEN" OR "WRIT" OR "REWR"
              MOVE "**** STOP ****"       TO ERRMSG-HEAD
              STRING "                    CALL HOME "
                     "OFFICE OR SUPPORT             "
                     DELIMITED BY SIZE INTO ERRMSG-MSG1
              STRING "AN ERROR HAS OCCURRED         "
                     "                DO NOT PROCEED"
                     DELIMITED BY SIZE INTO ERRMSG-MSG2
              MOVE "PRESS ENTER TO CONTINUE" TO ERRMSG-MSG3
              PERFORM CALL-ERRMSG.
           MOVE SPACES TO MESS.
      * THIS MUST STAY LIKE THIS, E-MSG HAS "OPEN ERROR" OR WHATEVER
      * IN IT, IT MUST GET MOVED AS BELOW
           MOVE ERROR-MSG-HEAD TO ERRMSG-HEAD.
           MOVE ERROR-MSG-STAT TO ERRMSG-MSG1.
           MOVE ERROR-MSG-PATH TO ERRMSG-MSG2.
      *    MOVE SPACES         TO ERRMSG-MSG3.
           MOVE ERROR-MSG-KEY  TO ERRMSG-MSG3.
           PERFORM CALL-ERRMSG.

           MOVE SPACES TO MESS.
       FILE-ERRORS-TERMINATE.
      *    A30, STARRED OFF, 7/26/12, BLM:
      *    IF WINDOWS-OPEN > ZERO
      *       CALL DELWIN
      *       SUBTRACT 1 FROM WINDOWS-OPEN
      *       GO TO FILE-ERRORS-TERMINATE.
           MOVE "E" TO ERRCD.
           IF FILE-STAT = "98"
              IF (FORM-PATHNAME(16:4) = "LONP" OR "XONP")
                 MOVE "Z" TO ERRCD.
           PERFORM CLOSE-FILES.
      *OLD GO TO EXIT-PROG.     *> OLD VISION
       SEND-MESS.
           PERFORM ALARM.
           DISPLAY MESSAGE BOX MESS.
       COPY "LIBGB/TMMSG.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      *=================================================================
       SQL-IO-VALIDATION.

           IF ( E-MSG4  = "OPEN" ) AND   *> GOOD
              ( SQLCODE = 1      )
              EXIT PARAGRAPH
           ELSE
           IF ( SQLCODE = 0      )       *> GOOD
              EXIT PARAGRAPH.

           MOVE 9 TO IO-FG.

           IF ( E-MSG4  = "READ" ) AND   *> EOF OR ROW NOT FOUND
              ( SQLCODE = 100    )
              EXIT PARAGRAPH.

           IF ( E-MSG4   = "WRIT"  ) AND   *> DUPLICATE ROW
              ( SQLSTATE = "23000" )       *> LIKE VISION "22,00"
              MOVE "22"    TO FILE-STAT
              MOVE "00"    TO SECONDARY-FILE-STAT
              MOVE "22,00" TO FILE-STAT-DISP
                              E-STATUS-ERROR
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE E-FILE        TO E-FILEX ERRLOGW-FILE.
           MOVE FORM-PATHNAME TO E-PROG ERRLOGW-PROGX.
           MOVE E-MSG         TO ERRLOGW-TYPE.
           MOVE E-CODE        TO ERRLOGW-CODE.
           MOVE E-SUBCODE     TO ERRLOGW-SUBCODE.
           MOVE E-KEYX        TO ERRLOGW-KEY.
           MOVE "E"           TO ERRLOGW-ACTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CL/ERRSQL"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA
                                 SQLCA EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE FORM-PATHNAME TO FORM-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E" TO ERRCD.
           IF FILE-STAT = "98"
              IF (FORM-PATHNAME(16:4) = "LONP" OR "XONP")
                 MOVE "Z" TO ERRCD.

           PERFORM SQL-ERROR.      *> DEFINED IN LIBIO/CONNECT_SQL.CPY

      *-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
      * ONLY CLOSE FILES/TABLES WHEN THERE IS AN SQL CONNECTION
      * OTHERWISE YOU CAN GET INTO AN INIFITE LOOP DUE TO THE
      * SQL-DISCONNECT IN SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      * IF THE CONNECTION HAS BEEN CLOSED.
      * NOTE: THIS WORKS FOR TABLES; NOT SURE WHAT HAPPENS WITH VISION
      *       WORK FILES, IMPORT & EXPORT EXTRACTION FILES; BELIEVE
      *       THEY SHOULD WORK FINE IF GOING RIGHT BACK INTO THE
      *       PROGRAM (THEORY IS THE CLOSING OF A PROGRAM WILL CLOSE
      *       THE FILES).                           BAH & JKC 2022-0214
      *-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
           IF ( EXT-ACUSQL-CONNECT-STAT-GOOD )
              PERFORM CLOSE-FILES.  *> CLOSE VISION FILES

           GO TO EXIT-PROG.        *> FORCE EXIT OF PROGRAM

      *=================================================================
       CLOSE-FILES.
           MOVE "CLOSE ERROR" TO E-MSG.
