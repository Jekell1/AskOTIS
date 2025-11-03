      * COPYMEMBER: LIBLP/LPOBY1RN
       LOAD-OBY1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OBY-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OBY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OBY-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OBY-ALL-MACHINE
              MOVE OBY-ALL-PATH             TO OBY-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OBY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OBY-ALL-BASE
              MOVE FILPATH-MACHINE         TO OBY-ALL-MACHINE
              MOVE OBY-ALL-PATH             TO OBY-PATH.

       OPEN-OBY1-FILE.
           PERFORM LOAD-OBY1-FILE.
           PERFORM OPEN-IT.
           MOVE OBY-PATH TO E-FILE.
           OPEN INPUT OBY-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBY1-FILE.
           IF IO-FG = 7
              CLOSE OBY-FILE
              GO TO OPEN-OBY1-FILE.
       READ-OBY1-FILE.
           PERFORM READ-IT.
           MOVE OBY-PATH TO E-FILE.
           MOVE OBY1-KEY TO E-KEYX.
           READ OBY-FILE.
           IF IO-FG = 8
              GO TO READ-OBY1-FILE.
       READ-OBY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBY-PATH TO E-FILE.
           MOVE OBY1-KEY TO E-KEYX.
           READ OBY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBY1-FILE-NEXT.
       START-OBY1-FILE.
           PERFORM START-IT.
           MOVE OBY-PATH TO E-FILE.
           MOVE OBY1-KEY TO E-KEYX.
           START OBY-FILE KEY NOT < OBY1-KEY.
       CLOSE-OBY1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBY-FILE.
