      * COPYMEMBER: LIBLP/ARRAYGI
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR GL INTERFACE PROCESSING (GIFILE)
      *
      *              USED TO LOAD RECORDS (GI-REC) FROM GIFILE SQL TABLE
      *              FOR BRANCH SPECIFIC ROWS.
      *
      * NEEDED     : LIBGL/GLGI1IN.CPY OR LIBGL/GLGI1RN.CPY
      *
      *=================================================================
      * REV:
      * JKC 2023-1201 ***NEW***
      * JKC 2024-0131 ADDED OTIS TRACE LOGGING LOGIC
      *
      ******************************************************************
      *
      *    A R R A Y G I - L O A D
      *
      *=================================================================
      * IN  : ARRAYGI-GI-BRANCH
      * OUT : ARRAYGI-FILE
      *       ARRAYGI-MAX
      * DESC: LOAD ARRAY WITH RECORDS (GI-REC) FOR SPECIFIC BRANCH
      *       (GI-BRANCH) ROWS IN THE SQL TABLE.
      ******************************************************************
       ARRAYGI-LOAD SECTION.


           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYGI-LOAD START" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO ARRAYGI-FILE.
           MOVE ZEROES                     TO ARRAYGI-ELE
                                              ARRAYGI-END
                                              ARRAYGI-GI-CLASS.

           MOVE ARRAYGI-GI-BRANCH          TO GI-BRANCH.
           MOVE ZEROES                     TO GI-CLASS.

           PERFORM OPEN-GI1-FILE.
           PERFORM START-GI1-FILE.

      *=================================================================
       ARRAYGI-LOAD-NEXT.

           PERFORM READ-GI1-FILE-NEXT.

           IF ( IO-BAD                            ) OR
              ( GI-BRANCH NOT = ARRAYGI-GI-BRANCH )
              GO TO ARRAYGI-LOAD-END.

           ADD 01 TO ARRAYGI-ELE  ARRAYGI-END.

           IF ( ARRAYGI-ELE > ARRAYGI-MAX )
              MOVE "READ ERROR"        TO E-MSG
              MOVE GI-PATH-SQL         TO E-FILE
              MOVE "***MAX REACHED***" TO E-KEYX
              GO TO IO-ERROR.

           MOVE GI-REC TO ARRAYGI-REC(ARRAYGI-ELE).

           GO TO ARRAYGI-LOAD-NEXT.

      *=================================================================
       ARRAYGI-LOAD-END.

           PERFORM CLOSE-GI1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYGI-LOAD FINISH" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *-----------------------------------------------------------------
       ARRAYGI-LOAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A R R A Y G I - R E A D
      *
      *=================================================================
      * IN  : ARRAYGI-GI1-KEY    [HOLD-KEY]
      *       ARRAYGI-REC-READ   [HOLD-REC]
      *       GI1-KEY            [READ-KEY]
      * OUT : ARRAYGI-GI1-KEY
      *       GI-REC
      *       IO-FG
      * DESC: LOAD GI-REC FROM ARRAY BASED ON GI1-KEY.
      ******************************************************************
       ARRAYGI-READ SECTION.

           MOVE ZEROES TO IO-FG.

           IF ( GI1-KEY = ARRAYGI-GI1-KEY )
              MOVE ARRAYGI-REC-READ TO GI-REC
              GO TO ARRAYGI-READ-EXIT.

           MOVE 9 TO IO-FG.

           PERFORM VARYING ARRAYGI-ELE FROM 1 BY 1
                     UNTIL ARRAYGI-ELE > ARRAYGI-MAX

                   MOVE ARRAYGI-REC(ARRAYGI-ELE) TO ARRAYGI-REC-READ

                   IF ( ARRAYGI-REC-READ(1:7) = GI1-KEY )
                      MOVE ARRAYGI-REC-READ TO GI-REC
                      MOVE GI1-KEY          TO ARRAYGI-GI1-KEY
                      MOVE ARRAYGI-MAX      TO ARRAYGI-ELE
                      MOVE ZEROES           TO IO-FG
                   END-IF

           END-PERFORM.

      *=================================================================
       ARRAYGI-READ-EXIT.
           EXIT.

