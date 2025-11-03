      * COPYMEMBER: LIBGL/GLOGI1RN
       LOAD-OGI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OGI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OGI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OGI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OGI-ALL-MACHINE
              MOVE OGI-ALL-PATH             TO OGI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OGI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OGI-ALL-BASE
              MOVE FILPATH-MACHINE         TO OGI-ALL-MACHINE
              MOVE OGI-ALL-PATH             TO OGI-PATH.
       OPEN-OGI1-FILE.
           PERFORM LOAD-OGI1-FILE.
           PERFORM OPEN-IT.
           MOVE OGI-PATH TO E-FILE.
           OPEN INPUT OGI-FILE.
           IF IO-FG = 8
             GO TO OPEN-OGI1-FILE.
           IF IO-FG = 7
             CLOSE OGI-FILE
             GO TO OPEN-OGI1-FILE.
           UNLOCK OGI-FILE.
       READ-OGI1-FILE.
           PERFORM READ-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           READ OGI-FILE.
           IF IO-FG = 8
              GO TO READ-OGI1-FILE.
           UNLOCK OGI-FILE.
       READ-OGI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           READ OGI-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OGI1-FILE-NEXT.
           UNLOCK OGI-FILE.
       START-OGI1-FILE.
           PERFORM START-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           START OGI-FILE KEY NOT < OGI1-KEY.
           UNLOCK OGI-FILE.
       CLOSE-OGI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGI-FILE.
