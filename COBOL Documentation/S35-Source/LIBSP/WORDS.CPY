      * COPYMEMBER: LIBSP/WORDS
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/WORDS
      *
      * DESCRIPTION   :  WORDS ROUTINE
      *
      *                  USED TO FIND WORDS FROM A INPUT FIELD.
      *
      *=================================================================
      * REV:
      * KEC 2006.0411 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      * KEC 2007.0815 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               AT THE END, DID A LEFT JUSTIFY ON THE
      *               OUTPUT LINES (WORDS-WRAP-LINE) TO ENSURE THAT
      *               THE OUTPUT HAS NO LEADING SPACES.
      *
      ******************************************************************
      *
      *    W O R D S - R O U T I N E
      *
      *=================================================================
      *    IN  :  WORDS-INPUT
      *           WORDS-WRAP-LINES
      *           WORDS-WRAP-LINE-SIZE(01 THRU 100)
      *
      *    OUT :  WORDS-COUNT
      *           WORDS-OUTPUT   {COUNT/TEXT}
      *           WORDS-WRAP-LINE(01 THRU 100)
      *
      *    DESC:  USED TO SEARCH A INPUT FIELD TO EXTRACT ALL THE WORDS
      *           AND COUNT HOW MANY CHARACTERS ARE IN EACH WORDS.
      *           IT ALSO COUNTS HOW MANY WORDS THERE ARE.
      *           IT ALSO CREATES LINES BASED ON LINE SIZES FOR
      *           WORD WRAPPING THE INPUT FIELD.
      *
      ******************************************************************
       WORDS-ROUTINE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE ZEROES TO WORDS-COUNT.
           MOVE ZEROES TO WORDS-ELE.
           MOVE SPACES TO WORDS-PREV-BYTE.
           MOVE ZEROES TO WORDS-S1.
           MOVE ZEROES TO WORDS-S2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO WORDS-OUTPUT-WORKERS.

           PERFORM VARYING WORDS-ELE FROM 1 BY 1
                     UNTIL WORDS-ELE > WORDS---MAX

                   MOVE ZEROES TO WORDS-OUTPUT-COUNT(WORDS-ELE)
                   MOVE SPACES TO WORDS-WRAP-LINE   (WORDS-ELE)

                   IF ( WORDS-ELE > WORDS-WRAP-LINES )
                      MOVE ZEROES TO WORDS-WRAP-LINE-SIZE(WORDS-ELE)
                   END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO WORDS-ELE.

      *=================================================================
      *    FIND STARTING BYTE (NON-SPACE)
      *=================================================================
       WORDS-ROUTINE-START.

           ADD 01 TO WORDS-ELE.

           IF ( WORDS-ELE > WORDS---MAX )
              GO TO WORDS-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WORDS-INPUT-ELE(WORDS-ELE) = SPACES )
              GO TO WORDS-ROUTINE-START.

           SUBTRACT 01 FROM WORDS-ELE.

      *=================================================================
      *    SEARCH INPUT FIELD FOR ALL WORDS
      *=================================================================
       WORDS-ROUTINE-SEARCH.

           ADD 01 TO WORDS-ELE.

           IF ( WORDS-ELE > WORDS---MAX )
              GO TO WORDS-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WORDS-INPUT-ELE(WORDS-ELE) = SPACES )
              GO TO WORDS-ROUTINE-RETURN.

      *- - - - - - - - - - - - - - - - - - - - -{ INPUT IS NOT A SPACE }
           IF ( WORDS-PREV-BYTE = SPACES )
              ADD  01     TO WORDS-COUNT
              ADD  01     TO WORDS-S1
              MOVE ZEROES TO WORDS-S2.

           ADD  01 TO WORDS-S2.
           ADD  01 TO WORDS-OUTPUT-COUNT(WORDS-S1).

           MOVE WORDS-INPUT-ELE (WORDS-ELE) TO
                WORDS-OUTPUT-ELE(WORDS-S1, WORDS-S2).

      *-----------------------------------------------------------------
       WORDS-ROUTINE-RETURN.

           MOVE WORDS-INPUT-ELE(WORDS-ELE) TO WORDS-PREV-BYTE.

           GO TO WORDS-ROUTINE-SEARCH.

      *=================================================================
       WORDS-ROUTINE-END.

           PERFORM WORDS-WRAP-ROUTINE.

      *-----------------------------------------------------------------
       WORDS-ROUTINE-EXIT.
           EXIT.
      ******************************************************************
      *
      *    W O R D S - W R A P - R O U T I N E
      *
      *=================================================================
      *    OUT :  WORDS-COUNT
      *           WORDS-OUTPUT   {COUNT/TEXT}
      *           WORDS-WRAP-LINES
      *           WORDS-WRAP-LINE-SIZE(01 THRU 100)
      *
      *    OUT :  WORDS-WRAP-LINE(01 THRU 100)
      *
      *    DESC:  IT CREATES LINES BASED ON LINE SIZES FOR
      *           WORD WRAPPING THE INPUT FIELD.
      *
      ******************************************************************
       WORDS-WRAP-ROUTINE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( WORDS-WRAP-LINES = ZEROES )
              GO TO WORDS-WRAP-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WORDS-S1 = SUB FOR WRAP LINES
      *    WORDS-S2 = SIZE OF CURRENT WRAP LINE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO WORDS-ELE.
           MOVE 01     TO WORDS-S1.
           MOVE ZEROES TO WORDS-S2.

      *=================================================================
       WORDS-WRAP-ROUTINE-LOOP.

           ADD 01 TO WORDS-ELE.

           IF ( WORDS-ELE > WORDS---MAX ) OR
              ( WORDS-ELE > WORDS-COUNT )
              GO TO WORDS-WRAP-ROUTINE-END.

      *-----------------------------------------------------------------
       WORDS-WRAP-ROUTINE-CHECK.

           IF ( WORDS-S1                > WORDS-WRAP-LINES ) OR
              ( WORDS-OUTPUT(WORDS-ELE) = SPACES           )
              GO TO WORDS-WRAP-ROUTINE-LOOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SEE IF NEXT WORD WILL FIT ON CURRENT LINE.
      *    ADD SIZE OF NEXT WORD PLUS A SPACE (+ 1) TO WORDS-S2 TO SEE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE WORDS-S2 = WORDS-S2
                            + 1
                            + WORDS-OUTPUT-COUNT(WORDS-ELE).

           IF ( WORDS-S2 > WORDS-WRAP-LINE-SIZE(WORDS-S1) )
              ADD  01     TO WORDS-S1
              MOVE ZEROES TO WORDS-S2
              GO TO WORDS-WRAP-ROUTINE-CHECK.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WORDS-S2 = WORDS-OUTPUT-COUNT(WORDS-ELE) )
              MOVE WORDS-OUTPUT(WORDS-ELE) TO WORDS-WRAP-LINE(WORDS-S1)
           ELSE
              STRING WORDS-WRAP-LINE(WORDS-S1), " ",
                     WORDS-OUTPUT   (WORDS-ELE)
                     DELIMITED BY "  "   INTO WORDS-WRAP-LINE(WORDS-S1).

           GO TO WORDS-WRAP-ROUTINE-LOOP.

      *=================================================================
       WORDS-WRAP-ROUTINE-END.

           PERFORM VARYING WORDS-ELE FROM 1 BY 1
                   UNTIL   WORDS-ELE > WORDS---MAX

                   CALL "C$JUSTIFY" USING WORDS-WRAP-LINE(WORDS-ELE) "L"

           END-PERFORM.

      *-----------------------------------------------------------------
       WORDS-WRAP-ROUTINE-EXIT.
           EXIT.
