      * COPYMEMBER: LIBLP/ARRAYBR
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR BRANCH PROCESSING (BRFILE)
      *
      *              USED TO LOAD RECORDS (BR-REC) FROM BRFILE SQL TABLE
      *              FOR ALL ROWS.
      *
      * NEEDED     : LIBGB/GBBRIN.CPY OR LIBGB/GBBRRN.CPY
      *
      *=================================================================
      * REV:
      * JKC 2023-1205 ***NEW***
      *
      * MB  2024-0205 ADDED ARRAYBR-LOAD-ALL-SW (USED TO TELL ARRAYBR IF
      *                   IT SHOULD LOADED ALL BRANCHES INTO ARRAY IN 
      *                   INITIAL LOAD PROCESSES.  IF "N" BRANCHES ARE LOADED
      *                   INTO THE ARRAY AS THEY ARE REQUESTED IN ARRAYBR-READ
      *               ADDED ARRAYBR-BRANCHES-LOADED TO KEEP TRACK OF HOW 
      *               MANY BRANCHES HAVE BEEN LOADED INTO THE ARRAY
      ******************************************************************
      *
      *    A R R A Y B R - L O A D
      *
      *=================================================================
      * IN  : NOTHING 
      * OUT : ARRAYBR-FILE
      *       ARRAYBR-MAX
      * DESC: LOAD ARRAY WITH RECORDS (BR-REC) FOR ALL IN THE FILE.
      ******************************************************************
       ARRAYBR-LOAD SECTION.

           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYBR-LOAD START" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO ARRAYBR-BR-NO.

           PERFORM COUNT-BR-FILE.   *> SSP

           IF ( IO-BAD )
              MOVE ZEROES TO ARRAYBR-MAX
              GO TO ARRAYBR-LOAD-EXIT.

           MOVE ZEROES        TO ARRAYBR-ELE
                                 ARRAYBR-BRANCHES-LOADED.
           MOVE SPACES        TO ARRAYBR-FILE.
           MOVE BR1-ROW-COUNT TO ARRAYBR-MAX.

           MOVE ZEROES TO BR-NO.

           IF ARRAYBR-LOAD-ALL-SW = "N"
              GO TO ARRAYBR-LOAD-EXIT.

           PERFORM OPEN-BR-FILE.
           PERFORM START-BR-FILE.

      *=================================================================
       ARRAYBR-LOAD-NEXT.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD )
              IF ( ARRAYBR-ELE = ZEROES )
                 MOVE ZEROES TO ARRAYBR-MAX
              END-IF
              GO TO ARRAYBR-LOAD-END.

           ADD 01 TO ARRAYBR-ELE
                     ARRAYBR-BRANCHES-LOADED.

           IF ( ARRAYBR-ELE > ARRAYBR-MAX )
              MOVE "READ ERROR"        TO E-MSG
              MOVE BR-PATH-SQL         TO E-FILE
              MOVE "***MAX REACHED***" TO E-KEYX
              GO TO IO-ERROR.

           MOVE BR-REC TO ARRAYBR-REC(ARRAYBR-ELE).

           GO TO ARRAYBR-LOAD-NEXT.

      *=================================================================
       ARRAYBR-LOAD-END.

           PERFORM CLOSE-BR-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYBR-LOAD FINISH" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *-----------------------------------------------------------------
       ARRAYBR-LOAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A R R A Y L C - R E A D
      *
      *=================================================================
      * IN  : ARRAYBR-BR-KEY     [HOLD-KEY]
      *       ARRAYBR-REC-READ   [HOLD-REC]
      *       BR-KEY             [READ-KEY]
      * OUT : ARRAYBR-BR-KEY
      *       BR-REC
      *       IO-FG
      * DESC: LOAD BR-REC FROM ARRAY BASED ON BR-KEY.
      ******************************************************************
       ARRAYBR-READ SECTION.

           MOVE ZEROES TO IO-FG.

           IF ( BR-KEY = ARRAYBR-BR-KEY )
              MOVE ARRAYBR-REC-READ TO BR-REC
              GO TO ARRAYBR-READ-EXIT.

           MOVE 9 TO IO-FG.

           PERFORM VARYING ARRAYBR-ELE FROM 1 BY 1
                     UNTIL ARRAYBR-ELE > ARRAYBR-MAX

                   MOVE ARRAYBR-REC(ARRAYBR-ELE) TO ARRAYBR-REC-READ

                   IF ( ARRAYBR-REC-READ(1:4) = BR-KEY )
                      MOVE ARRAYBR-REC-READ TO BR-REC
                      MOVE BR-KEY           TO ARRAYBR-BR-KEY
                      MOVE ARRAYBR-MAX      TO ARRAYBR-ELE
                      MOVE ZEROES           TO IO-FG
                   END-IF

           END-PERFORM.

           IF IO-FG = 9 AND ARRAYBR-LOAD-ALL-SW = "N"
              PERFORM READ-BR-FILE
              IF IO-GOOD
                 ADD 01 TO ARRAYBR-BRANCHES-LOADED
                 IF ( ARRAYBR-BRANCHES-LOADED > ARRAYBR-MAX )
                    MOVE "READ ERROR"        TO E-MSG
                    MOVE BR-PATH-SQL         TO E-FILE
                    MOVE "***MAX REACHED***" TO E-KEYX
                    GO TO IO-ERROR
                END-IF
                MOVE BR-REC TO ARRAYBR-REC(ARRAYBR-BRANCHES-LOADED)
              END-IF
           END-IF.

      *=================================================================
       ARRAYBR-READ-EXIT.
           EXIT.

