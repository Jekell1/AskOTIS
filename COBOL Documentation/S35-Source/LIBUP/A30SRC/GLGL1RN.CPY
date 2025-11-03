      * COPYMEMBER: LIBGL/GLOGL1RN
       LOAD-OGL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OGL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OGL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OGL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OGL-ALL-MACHINE
              MOVE OGL-ALL-PATH             TO OGL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OGL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OGL-ALL-BASE
              MOVE FILPATH-MACHINE         TO OGL-ALL-MACHINE
              MOVE OGL-ALL-PATH             TO OGL-PATH.
       OPEN-OGL1-FILE.
           PERFORM LOAD-OGL1-FILE.
           PERFORM OPEN-IT.
           MOVE OGL-PATH TO E-FILE.
           OPEN INPUT OGL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGL1-FILE.
           IF IO-FG = 7
              CLOSE OGL-FILE
              GO TO OPEN-OGL1-FILE.
       READ-OGL1-FILE.
           PERFORM READ-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           READ OGL-FILE.
           IF IO-FG = 8
              GO TO READ-OGL1-FILE.
       READ-OGL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           READ OGL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGL1-FILE-NEXT.
       START-OGL1-FILE.
           PERFORM START-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           START OGL-FILE KEY NOT < OGL1-KEY.
       START-OGL1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           START OGL-FILE KEY > OGL1-KEY.
       CLOSE-OGL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGL-FILE.
