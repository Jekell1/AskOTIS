      * COPYMEMBER: LIBLP/LPOBW1RN
       LOAD-OBW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OBW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OBW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OBW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OBW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OBW-ALL-DATA
              MOVE OBW-ALL-PATH             TO OBW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OBW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OBW-ALL-BASE
              MOVE FILPATH-MACHINE         TO OBW-ALL-MACHINE
              MOVE FILPATH-DATA            TO OBW-ALL-DATA
              MOVE OBW-ALL-PATH             TO OBW-PATH.
       OPEN-OBW1-FILE.
           PERFORM LOAD-OBW1-FILE.
           PERFORM OPEN-IT.
           MOVE OBW-PATH TO E-FILE.
           OPEN INPUT OBW-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBW1-FILE.
           IF IO-FG = 7
              CLOSE OBW-FILE
              GO TO OPEN-OBW1-FILE.
       READ-OBW1-FILE.
           PERFORM READ-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           READ OBW-FILE.
           IF IO-FG = 8
              GO TO READ-OBW1-FILE.
       READ-OBW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           READ OBW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBW1-FILE-NEXT.
       START-OBW1-FILE.
           PERFORM START-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           START OBW-FILE KEY NOT < OBW1-KEY.
       CLOSE-OBW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBW-FILE.
