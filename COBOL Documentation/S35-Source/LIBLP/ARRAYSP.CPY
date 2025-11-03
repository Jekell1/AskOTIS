      * COPYMEMBER: LIBLP/ARRAYSP
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR STATE PARAMETER RECORDS
      *
      *              USED TO LOAD *ALL* SP-REC FROM THE SPFILE SQL TABLE
      *              INTO AN INTERNAL ARRAY FOR IMPROVING PERFORMANCE.
      *
      * NEEDED     : LIBLP/LPSP1RN.CPY
      *
      *=================================================================
      * REV:
      * JKC 2023-0609 ***NEW***
      * JKC 2024-0131 ADDED OTIS TRACE LOGGING LOGIC
      *
      ******************************************************************
      *
      *    A R R A Y S P - L O A D
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : ARRAYSP-END   [NUMBER OF ELEMENTS IN FILE/ARRAY]
      *       ARRAYSP-FILE  [ALL SP-REC LOAD TO FILE/ARRAY]
      * DESC: LOAD ALL SP-REC FROM SQL SPFILE TABLES (A/B/C) INTO ARRAY.
      ******************************************************************
       ARRAYSP-LOAD SECTION.

           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYSP-LOAD START" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO ARRAYSP-FILE.

           MOVE SPACES TO SP1-KEY      ARRAYSP-KEY.
           MOVE ZEROES TO SP-SPRCLASS  ARRAYSP-SPRCLASS
                          SP-SUBCLASS  ARRAYSP-SUBCLASS
                          SP-LAWCODE   ARRAYSP-LAWCODE.

           MOVE ZEROES TO ARRAYSP-ELE  ARRAYSP-END.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.

      *=================================================================
       ARRAYSP-LOAD-NEXT.

           PERFORM READ-SP1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO ARRAYSP-LOAD-END.

           ADD 01 TO ARRAYSP-ELE  ARRAYSP-END.

           IF ( ARRAYSP-ELE > ARRAYSP-MAX )
              MOVE "READ ERROR"        TO E-MSG
              MOVE SPA-PATH-SQL        TO E-FILE
              MOVE "***MAX REACHED***" TO E-KEYX
              GO TO IO-ERROR.

           MOVE SP-REC TO ARRAYSP-REC(ARRAYSP-ELE).

           GO TO ARRAYSP-LOAD-NEXT.

      *=================================================================
       ARRAYSP-LOAD-END.

           PERFORM CLOSE-SP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYSP-LOAD FINISH" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *-----------------------------------------------------------------
       ARRAYSP-LOAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A R R A Y S P - R E A D
      *
      *=================================================================
      * IN  : ARRAYSP-KEY   [HOLD-KEY]
      *       SP1-KEY       [READ-KEY]
      * OUT : IO-FG
      *       SP-REC
      * DESC: LOAD SP-REC FROM ARRAY BASED ON SP1-KEY.
      ******************************************************************
       ARRAYSP-READ SECTION.

           MOVE ZEROES TO IO-FG.

           IF ( SP1-KEY = ARRAYSP-KEY )
              MOVE ARRAYSP-REC-READ TO SP-REC
              GO TO ARRAYSP-READ-EXIT.

           MOVE 9 TO IO-FG.

           PERFORM VARYING ARRAYSP-ELE FROM 1 BY 1
                     UNTIL ARRAYSP-ELE > ARRAYSP-END

                   MOVE ARRAYSP-REC(ARRAYSP-ELE) TO ARRAYSP-REC-READ

                   IF ( ARRAYSP-REC-READ(1:9) = SP1-KEY )
                      MOVE ARRAYSP-REC-READ TO SP-REC
                      MOVE SP1-KEY          TO ARRAYSP-KEY
                      MOVE ARRAYSP-MAX      TO ARRAYSP-ELE
                      MOVE ZEROES           TO IO-FG
                   END-IF

           END-PERFORM.

      *=================================================================
       ARRAYSP-READ-EXIT.
           EXIT.

