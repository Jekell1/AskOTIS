      * COPYMEMBER: LIBLP/LPOBX1RN
       LOAD-OBX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OBX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OBX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OBX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OBX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OBX-ALL-DATA
              MOVE OBX-ALL-PATH             TO OBX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OBX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OBX-ALL-BASE
              MOVE FILPATH-MACHINE         TO OBX-ALL-MACHINE
              MOVE FILPATH-DATA            TO OBX-ALL-DATA
              MOVE OBX-ALL-PATH             TO OBX-PATH.
       OPEN-OBX1-FILE.
           PERFORM LOAD-OBX1-FILE.
           PERFORM OPEN-IT.
           MOVE OBX-PATH TO E-FILE.
           OPEN INPUT OBX-FILE.
           IF IO-FG = 8
             GO TO OPEN-OBX1-FILE.
           IF IO-FG = 7
             CLOSE OBX-FILE
             GO TO OPEN-OBX1-FILE.
       READ-OBX1-FILE.
           PERFORM READ-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           READ OBX-FILE.
           IF IO-FG = 8
              GO TO READ-OBX1-FILE.
       READ-OBX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           READ OBX-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OBX1-FILE-NEXT.
       START-OBX1-FILE.
           PERFORM START-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           START OBX-FILE KEY NOT < OBX1-KEY.
           UNLOCK OBX-FILE.
       START-OBX1-FILE-LESS-THAN.
           PERFORM START-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           START OBX-FILE KEY < OBX1-KEY.
       READ-OBX1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           READ OBX-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OBX1-FILE-PREVIOUS.
       CLOSE-OBX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBX-FILE.
