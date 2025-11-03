      * COPYMEMBER: LIBLP/LPOLM1RN
       LOAD-OLM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLM-ALL-DATA
              MOVE OLM-ALL-PATH             TO OLM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLM-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLM-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLM-ALL-DATA
              MOVE OLM-ALL-PATH             TO OLM-PATH.
       OPEN-OLM1-FILE.
           PERFORM LOAD-OLM1-FILE.
           PERFORM OPEN-IT.
           MOVE OLM-PATH TO E-FILE.
           OPEN INPUT OLM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLM1-FILE.
           IF IO-FG = 7
              CLOSE OLM-FILE
              GO TO OPEN-OLM1-FILE.
       READ-OLM1-FILE.
           PERFORM READ-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           READ OLM-FILE.
           IF IO-FG = 8
              GO TO READ-OLM1-FILE.
       READ-OLM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           READ OLM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLM1-FILE-NEXT.
       START-OLM1-FILE.
           PERFORM START-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           START OLM-FILE KEY NOT < OLM1-KEY.
       CLOSE-OLM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLM-FILE.
