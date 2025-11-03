      * COPYMEMBER: LIBLP/LPOBM1RN
       LOAD-OBM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OBM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OBM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OBM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OBM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OBM-ALL-DATA
              MOVE OBM-ALL-PATH             TO OBM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OBM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OBM-ALL-BASE
              MOVE FILPATH-MACHINE         TO OBM-ALL-MACHINE
              MOVE FILPATH-DATA            TO OBM-ALL-DATA
              MOVE OBM-ALL-PATH             TO OBM-PATH.
       OPEN-OBM1-FILE.
           PERFORM LOAD-OBM1-FILE.
           PERFORM OPEN-IT.
           MOVE OBM-PATH TO E-FILE.
           OPEN INPUT OBM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBM1-FILE.
           IF IO-FG = 7
              CLOSE OBM-FILE
              GO TO OPEN-OBM1-FILE.
       READ-OBM1-FILE.
           PERFORM READ-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           READ OBM-FILE.
           IF IO-FG = 8
              GO TO READ-OBM1-FILE.
       READ-OBM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           READ OBM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBM1-FILE-NEXT.
       START-OBM1-FILE.
           PERFORM START-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           START OBM-FILE KEY NOT < OBM1-KEY.
       CLOSE-OBM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBM-FILE.
