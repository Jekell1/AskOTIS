      * COPYMEMBER: LIBLP/LPOPIN
       LOAD-OP-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPEN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPEN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPEN-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPEN-PATH-MACHINE
              MOVE OPEN-PATH-FORMAT        TO OPEN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPEN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPEN-PATH-BASE
              MOVE FILPATH-MACHINE         TO OPEN-PATH-MACHINE
              MOVE OPEN-PATH-FORMAT        TO OPEN-PATH.
      *-----------------------------------------------------------------
       OPEN-OP-FILE.
           PERFORM LOAD-OP-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-PATH TO E-FILE.
           OPEN I-O OP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OP-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-OP-FILE
              GO TO OPEN-OP-FILE.
      *-----------------------------------------------------------------
       OPEN-OP-FILE-OUTPUT.
           PERFORM LOAD-OP-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-PATH TO E-FILE.
           OPEN OUTPUT OP-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-OP-FILE-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-OP-FILE
              GO TO OPEN-OP-FILE-OUTPUT.
      *-----------------------------------------------------------------
       CLOSE-OP-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE OP-FILE.
      *-----------------------------------------------------------------
