      * COPYMEMBER: LIBLP/ARRAYLC
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR LOAN CLASS PROCESSING (LCFILE)
      *
      *              USED TO LOAD RECORDS (LC-REC) FROM LCFILE SQL TABLE
      *              FOR ALL ROWS.
      *
      * NEEDED     : LIBLP/LPLC1IN.CPY OR LIBLP/LPLC1RN.CPY
      *
      *=================================================================
      * REV:
      * MFB 2023-0926 ***NEW***
      * JKC 2023-1130 CHANGED TO MATCH UP TO THE OTHER ARRAY MEMBERS.
      * JKC 2024-0131 ADDED OTIS TRACE LOGGING LOGIC
      *
      ******************************************************************
      *
      *    A R R A Y L C - L O A D
      *
      *=================================================================
      * IN  : NOTHING 
      * OUT : ARRAYLC-FILE
      *       ARRAYLC-MAX
      * DESC: LOAD ARRAY WITH RECORDS (LC-REC) FOR ALL IN THE FILE.
      ******************************************************************
       ARRAYLC-LOAD SECTION.

           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYLC-LOAD START" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO ARRAYLC-LC-CODE.

           PERFORM COUNT-LC1-FILE.

           IF ( IO-BAD )
              MOVE ZEROES TO ARRAYLC-MAX
              GO TO ARRAYLC-LOAD-EXIT.

           MOVE ZEROES        TO ARRAYLC-ELE.
           MOVE SPACES        TO ARRAYLC-FILE.
           MOVE LC1-ROW-COUNT TO ARRAYLC-MAX.

           MOVE ZEROES TO LC-CODE.

           PERFORM OPEN-LC1-FILE.
           PERFORM START-LC1-FILE.

      *=================================================================
       ARRAYLC-LOAD-NEXT.

           PERFORM READ-LC1-FILE-NEXT.

           IF ( IO-BAD )
              IF ( ARRAYLC-ELE = ZEROES )
                 MOVE ZEROES TO ARRAYLC-MAX
              END-IF
              GO TO ARRAYLC-LOAD-END.

           ADD 01 TO ARRAYLC-ELE.

           IF ( ARRAYLC-ELE > ARRAYLC-MAX )
              MOVE "READ ERROR"        TO E-MSG
              MOVE LC-PATH-SQL         TO E-FILE
              MOVE "***MAX REACHED***" TO E-KEYX
              GO TO IO-ERROR.

           MOVE LC-REC TO ARRAYLC-REC(ARRAYLC-ELE).

           GO TO ARRAYLC-LOAD-NEXT.

      *=================================================================
       ARRAYLC-LOAD-END.

           PERFORM CLOSE-LC1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-TCLP-TRACE-UPDATE )
              MOVE "ARRAYLC-LOAD FINISH" TO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *-----------------------------------------------------------------
       ARRAYLC-LOAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A R R A Y L C - R E A D
      *
      *=================================================================
      * IN  : ARRAYLC-LC1-KEY    [HOLD-KEY]
      *       ARRAYLC-REC-READ   [HOLD-REC]
      *       LC1-KEY            [READ-KEY]
      * OUT : ARRAYLC-LC1-KEY
      *       LC-REC
      *       IO-FG
      * DESC: LOAD LC-REC FROM ARRAY BASED ON LC1-KEY.
      ******************************************************************
       ARRAYLC-READ SECTION.

           MOVE ZEROES TO IO-FG.

           IF ( LC1-KEY = ARRAYLC-LC1-KEY )
              MOVE ARRAYLC-REC-READ TO LC-REC
              GO TO ARRAYLC-READ-EXIT.

           MOVE 9 TO IO-FG.

           PERFORM VARYING ARRAYLC-ELE FROM 1 BY 1
                     UNTIL ARRAYLC-ELE > ARRAYLC-MAX

                   MOVE ARRAYLC-REC(ARRAYLC-ELE) TO ARRAYLC-REC-READ

                   IF ( ARRAYLC-REC-READ(1:3) = LC1-KEY )
                      MOVE ARRAYLC-REC-READ TO LC-REC
                      MOVE LC1-KEY          TO ARRAYLC-LC1-KEY
                      MOVE ARRAYLC-MAX      TO ARRAYLC-ELE
                      MOVE ZEROES           TO IO-FG
                   END-IF

           END-PERFORM.

      *=================================================================
       ARRAYLC-READ-EXIT.
           EXIT.

