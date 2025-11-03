      * COPYMEMBER: LIBAP/APOAIRN
       LOAD-OAI-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAI-ALL-MACHINE
              MOVE OAI-ALL-PATH             TO OAI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAI-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAI-ALL-MACHINE
              MOVE OAI-ALL-PATH             TO OAI-PATH.
       LIBAP-APOAIRN.
           MOVE OAI-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OAI-FILE.
           PERFORM LOAD-OAI-FILE.
           PERFORM OPEN-IT.
           MOVE OAI-PATH TO E-FILE.
           OPEN INPUT OAI-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAI-FILE.
           IF IO-FG = 7
              CLOSE OAI-FILE
              GO TO OPEN-OAI-FILE.
           UNLOCK OAI-FILE.
       READ-OAI-FILE.
           PERFORM READ-IT.
           MOVE OAI-PATH TO E-FILE.
           MOVE OAI-KEY TO E-KEYX.
           READ OAI-FILE.
           IF IO-FG = 8
              GO TO READ-OAI-FILE.
           UNLOCK OAI-FILE.
       READ-OAI-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAI-PATH TO E-FILE.
           MOVE OAI-KEY TO E-KEYX.
           READ OAI-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAI-FILE-NEXT.
           UNLOCK OAI-FILE.
       START-OAI-FILE.
           PERFORM START-IT.
           MOVE OAI-PATH TO E-FILE.
           MOVE OAI-KEY TO E-KEYX.
           START OAI-FILE KEY NOT < OAI-KEY.
           UNLOCK OAI-FILE.
       CLOSE-OAI-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAI-FILE.
