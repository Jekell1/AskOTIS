      * COPYMEMBER: LIBSP/SPOWR1RN
       LOAD-OWR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OWR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OWR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OWR-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO OWR-PATH-MACHINE
              MOVE OWR-PATH-FORMAT          TO OWR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OWR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OWR-PATH-BASE
              MOVE FILPATH-MACHINE         TO OWR-PATH-MACHINE
              MOVE OWR-PATH-FORMAT          TO OWR-PATH.

       OPEN-OWR1-FILE.
*NEW*      PERFORM LOAD-OWR1-FILE.
           PERFORM OPEN-IT.
           MOVE OWR-PATH TO E-FILE.
           OPEN INPUT OWR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OWR1-FILE.
           IF IO-FG = 7
              CLOSE OWR-FILE
              GO TO OPEN-OWR1-FILE.
       READ-OWR1-FILE.
           PERFORM READ-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           READ OWR-FILE.
           IF IO-FG = 8
              GO TO READ-OWR1-FILE.
       READ-OWR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           READ OWR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OWR1-FILE-NEXT.
       START-OWR1-FILE.
           PERFORM START-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           START OWR-FILE KEY NOT < OWR1-KEY.
       CLOSE-OWR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OWR-FILE.
