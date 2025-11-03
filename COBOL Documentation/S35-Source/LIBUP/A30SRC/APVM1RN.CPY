      * COPYMEMBER: LIBAP/APOVM1RN
       LOAD-OVM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OVM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OVM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OVM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OVM-ALL-MACHINE
              MOVE OVM-ALL-PATH             TO OVM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OVM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OVM-ALL-BASE
              MOVE FILPATH-MACHINE         TO OVM-ALL-MACHINE
              MOVE OVM-ALL-PATH             TO OVM-PATH.
       OPEN-OVM1-FILE.
           PERFORM LOAD-OVM1-FILE.
           PERFORM OPEN-IT.
           MOVE OVM-PATH TO E-FILE.
           OPEN INPUT OVM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OVM1-FILE.
           IF IO-FG = 7
              CLOSE OVM-FILE
              GO TO OPEN-OVM1-FILE.
       READ-OVM1-FILE.
           PERFORM READ-IT.
           MOVE OVM-PATH TO E-FILE.
           MOVE OVM1-KEY TO E-KEYX.
           READ OVM-FILE.
           IF IO-FG = 8
              GO TO READ-OVM1-FILE.
       READ-OVM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OVM-PATH TO E-FILE.
           MOVE OVM1-KEY TO E-KEYX.
           READ OVM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OVM1-FILE-NEXT.
       START-OVM1-FILE.
           PERFORM START-IT.
           MOVE OVM-PATH TO E-FILE.
           MOVE OVM1-KEY TO E-KEYX.
           START OVM-FILE KEY NOT < OVM1-KEY.
       CLOSE-OVM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OVM-FILE.
