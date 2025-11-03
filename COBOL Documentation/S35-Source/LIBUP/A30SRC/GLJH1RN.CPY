      * COPYMEMBER: LIBGL/GLOJH1RN
       LOAD-OJH1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OJH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OJH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OJH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OJH-ALL-MACHINE
              MOVE OJH-ALL-PATH             TO OJH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OJH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OJH-ALL-BASE
              MOVE FILPATH-MACHINE         TO OJH-ALL-MACHINE
              MOVE OJH-ALL-PATH             TO OJH-PATH.
       OPEN-OJH1-FILE.
           PERFORM LOAD-OJH1-FILE.
           PERFORM OPEN-IT.
           MOVE OJH-PATH TO E-FILE.
           OPEN INPUT OJH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OJH1-FILE.
           IF IO-FG = 7
              CLOSE OJH-FILE
              GO TO OPEN-OJH1-FILE.
           UNLOCK OJH-FILE.
       READ-OJH1-FILE.
           PERFORM READ-IT.
           MOVE OJH-PATH TO E-FILE.
           MOVE OJH1-KEY TO E-KEYX.
           READ OJH-FILE KEY IS OJH1-KEY.
           IF IO-FG = 8
              GO TO READ-OJH1-FILE.
       READ-OJH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OJH-PATH TO E-FILE.
           MOVE OJH1-KEY TO E-KEYX.
           READ OJH-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OJH1-FILE-NEXT.
       START-OJH1-FILE.
           PERFORM START-IT.
           MOVE OJH-PATH TO E-FILE.
           MOVE OJH1-KEY TO E-KEYX.
           START OJH-FILE KEY NOT < OJH1-KEY.
           UNLOCK OJH-FILE.
       CLOSE-OJH1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OJH-FILE.
