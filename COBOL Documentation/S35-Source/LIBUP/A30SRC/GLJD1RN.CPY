      * COPYMEMBER: LIBGL/GLOJD1RN
       LOAD-OJD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OJD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OJD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OJD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OJD-ALL-MACHINE
              MOVE OJD-ALL-PATH             TO OJD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OJD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OJD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OJD-ALL-MACHINE
              MOVE OJD-ALL-PATH             TO OJD-PATH.
       OPEN-OJD1-FILE.
           PERFORM LOAD-OJD1-FILE.
           PERFORM OPEN-IT.
           MOVE OJD-PATH TO E-FILE.
           OPEN INPUT OJD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OJD1-FILE.
           IF IO-FG = 7
              CLOSE OJD-FILE
              GO TO OPEN-OJD1-FILE.
           UNLOCK OJD-FILE.
       READ-OJD1-FILE.
           PERFORM READ-IT.
           MOVE OJD-PATH TO E-FILE.
           MOVE OJD1-KEY TO E-KEYX.
           READ OJD-FILE KEY IS OJD1-KEY.
           IF IO-FG = 8
              GO TO READ-OJD1-FILE.
       READ-OJD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OJD-PATH TO E-FILE.
           MOVE OJD1-KEY TO E-KEYX.
           READ OJD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OJD1-FILE-NEXT.
       START-OJD1-FILE.
           PERFORM START-IT.
           MOVE OJD-PATH TO E-FILE.
           MOVE OJD1-KEY TO E-KEYX.
           START OJD-FILE KEY NOT < OJD1-KEY.
           UNLOCK OJD-FILE.
       CLOSE-OJD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OJD-FILE.
