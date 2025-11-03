      * COPYMEMBER: LIBGL/GLOFS1RN
       LOAD-OFS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OFS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OFS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OFS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OFS-ALL-MACHINE
              MOVE OFS-ALL-PATH             TO OFS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OFS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OFS-ALL-BASE
              MOVE FILPATH-MACHINE         TO OFS-ALL-MACHINE
              MOVE OFS-ALL-PATH             TO OFS-PATH.
       OPEN-OFS1-FILE.
           PERFORM LOAD-OFS1-FILE.
           PERFORM OPEN-IT.
           MOVE OFS-PATH TO E-FILE.
           OPEN INPUT OFS-FILE.
           IF IO-FG = 8
              GO TO OPEN-OFS1-FILE.
           IF IO-FG = 7
              CLOSE OFS-FILE
              GO TO OPEN-OFS1-FILE.
       READ-OFS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OFS-PATH TO E-FILE.
           MOVE OFS1-KEY TO E-KEYX.
           READ OFS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OFS1-FILE-NEXT.
       START-OFS1-FILE.
           PERFORM START-IT.
           MOVE OFS-PATH TO E-FILE.
           MOVE OFS1-KEY TO E-KEYX.
           START OFS-FILE KEY NOT < OFS1-KEY.
       CLOSE-OFS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OFS-FILE.
