       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RCINQ0.
      *****************************************************
      *                 (SP)
      *
      *  DESCRIPTION:   DAILY RECAP STATUS INQUIRY DRIVER
      * REV:
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)SP/RCINQ0 A30 10/11/13-13:38:47 09/18/14-13:57:24 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  EXT-EOBUF-MESS PIC X(25) VALUE SPACES.
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/RCINQW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
      *    ***************************
      *    *      MAIN PROGRAM MODULE
      *    ***************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
       MAIN-MODULE.

           MOVE 0 TO RCINQ-BEG-BRANCH RCINQ-END-BRANCH
                     RCINQW-DATE1 RCINQW-DATE2.
           MOVE "Y" TO RCINQ-ENTER-RANGES.

           MOVE "WI/RCINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH RCINQ-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           GO TO END-ROUTINE.

       MISC-ROUTINES SECTION.
       COPY "LIBGB/SENDMESS.CPY".
       IO-ERROR.
           MOVE E-MSG TO MESS.
           PERFORM SEND-MESS.
           GO TO END-ROUTINE.


       END-ROUTINE SECTION.
       EXIT-PROG.
           MOVE "SP/UTMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.
