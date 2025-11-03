      ******************************************************************
      *
      * IDENTIFICATION: LIBCL/TRACE
      *
      * DESCRIPTION   : TCLP TRACE LOGGING ROUTINES
      *
      *                 USED FOR ADDING TO PROGRAMS THAT LACK
      *                 NORMAL TRACE OPERATIONS FOUND IN:
      *
      *                 - LIBGB/CONNECT_SQL.CPY
      *                 - LIBGB/*DECLARE*.CPY
      *
      *=================================================================
      * REV:
      * JKC 2024-0131 ***NEW***
      *
      ******************************************************************
      *
      *   T R A C E - P R O G R A M - F I N I S H
      *
      *=================================================================
      * IN  : EXT-TCLP-TRACE-WORKERS
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: GENERIC TRACE PROGRAM FINISH LOGGING
      ******************************************************************
       TRACE-PROGRAM-FINISH SECTION.

           IF ( EXT-TCLP-TRACE-DISABLE       ) OR
              ( EXT-TCLP-TRACE-SKIP-YN = "Y" )
              PERFORM TRACE-RESET
              GO TO TRACE-PROGRAM-FINISH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM FINISH (LIBCL/TRACE)" TO EXT-TCLP-TRACE-MESS.

           CALL "TRACE" USING EXT-TCLP-TRACE-FORM-PATH.
           CANCEL "TRACE".

      *=================================================================
       TRACE-PROGRAM-FINISH-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T R A C E - P R O G R A M - F I N I S H - S T O P 
      *
      *=================================================================
      * IN  : EXT-TCLP-TRACE-WORKERS
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: GENERIC TRACE PROGRAM FINISH LOGGING FOR A
      *       'GO TO STOP-RUN'; YOU MAY GET DOUBLE PROGRAM FINISH LOG
      *       LINES DUE TO A 'PERFORM CLOSE-FILES' THEN FOLLOWED BY
      *       A 'STOP-RUN' PARAGRAPH.
      ******************************************************************
       TRACE-PROGRAM-FINISH-STOP SECTION.

           IF ( EXT-TCLP-TRACE-DISABLE       ) OR
              ( EXT-TCLP-TRACE-SKIP-YN = "Y" )
              PERFORM TRACE-RESET
              GO TO TRACE-PROGRAM-FI-STOP-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM FINISH VIA GO TO STOP-RUN (LIBCL/TRACE)"
             TO EXT-TCLP-TRACE-MESS.

           CALL "TRACE" USING EXT-TCLP-TRACE-FORM-PATH.
           CANCEL "TRACE".

      *=================================================================
       TRACE-PROGRAM-FI-STOP-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T R A C E - P R O G R A M - R E T U R N
      *
      *=================================================================
      * IN  : EXT-TCLP-TRACE-WORKERS
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: GENERIC TRACE PROGRAM RETURN LOGGING FOR COMING BACK
      *       TO A CALLING PROGRAM; LIKE A MENU PROGRAM CALLING TO A
      *       REPORT PROGRAM THEN RETURNS BACK TO THE MENU PROGRAM
      *       WHICH NEVER FINISHED.
      ******************************************************************
       TRACE-PROGRAM-RETURN SECTION.

           IF ( EXT-TCLP-TRACE-DISABLE       ) OR
              ( EXT-TCLP-TRACE-SKIP-YN = "Y" )
              PERFORM TRACE-RESET
              GO TO TRACE-PROGRAM-RETURN-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM RETURN (LIBCL/TRACE)"
             TO EXT-TCLP-TRACE-MESS.

           CALL "TRACE" USING EXT-TCLP-TRACE-FORM-PATH.
           CANCEL "TRACE".

      *=================================================================
       TRACE-PROGRAM-RETURN-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T R A C E - P R O G R A M - S T A R T
      *
      *=================================================================
      * IN  : EXT-TCLP-TRACE-WORKERS
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: GENERIC TRACE PROGRAM START LOGGING
      ******************************************************************
       TRACE-PROGRAM-START SECTION.

           IF ( EXT-TCLP-TRACE-DISABLE       ) OR
              ( EXT-TCLP-TRACE-SKIP-YN = "Y" )
              PERFORM TRACE-RESET
              GO TO TRACE-PROGRAM-START-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM START (LIBCL/TRACE)" TO EXT-TCLP-TRACE-MESS.

           CALL "TRACE" USING EXT-TCLP-TRACE-FORM-PATH.
           CANCEL "TRACE".

      *=================================================================
       TRACE-PROGRAM-START-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T R A C E - S E N D - M E S S
      *
      *=================================================================
      * IN  : EXT-TCLP-TRACE-WORKERS
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: GENERIC TRACE MESSAGE USING EXT-TCLP-TRACE-MESS THAT IS
      *       SET PRIOR TO CALLING THIS SECTION.
      ******************************************************************
       TRACE-SEND-MESS SECTION.

           IF ( EXT-TCLP-TRACE-DISABLE       ) OR
              ( EXT-TCLP-TRACE-SKIP-YN = "Y" )
              PERFORM TRACE-RESET
              GO TO TRACE-SEND-MESS-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           CALL "TRACE" USING EXT-TCLP-TRACE-FORM-PATH.
           CANCEL "TRACE".

      *=================================================================
       TRACE-SEND-MESS-EXIT.
           EXIT.

      ******************************************************************
      *
      *   T R A C E - R E S E T
      *
      *=================================================================
      * IN  : EXT-TCLP-TRACE-WORKERS
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: RESET SPECIFIC TRACE WORKERS BACK TO DEFAULT
      ******************************************************************
       TRACE-RESET SECTION.

           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN.

      *=================================================================
       TRACE-RESET-EXIT.
           EXIT.
