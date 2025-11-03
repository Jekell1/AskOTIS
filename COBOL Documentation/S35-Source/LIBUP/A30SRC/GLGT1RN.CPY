      * COPYMEMBER: LIBGL/GLOGT1RN
       LOAD-OGT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OGT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OGT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OGT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OGT-ALL-MACHINE
              MOVE OGT-ALL-PATH             TO OGT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OGT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OGT-ALL-BASE
              MOVE FILPATH-MACHINE         TO OGT-ALL-MACHINE
              MOVE OGT-ALL-PATH             TO OGT-PATH.
       OPEN-OGT1-FILE.
           PERFORM LOAD-OGT1-FILE.
           PERFORM OPEN-IT.
           MOVE OGT-PATH TO E-FILE.
           OPEN INPUT OGT-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGT1-FILE.
           IF IO-FG = 7
              CLOSE OGT-FILE
              GO TO OPEN-OGT1-FILE.
       READ-OGT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGT-PATH TO E-FILE.
           MOVE OGT1-KEY TO E-KEYX.
           READ OGT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGT1-FILE-NEXT.
       START-OGT1-FILE.
           PERFORM START-IT.
           MOVE OGT-PATH TO E-FILE.
           MOVE OGT1-KEY TO E-KEYX.
           START OGT-FILE KEY NOT < OGT1-KEY.
       CLOSE-OGT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGT-FILE.
