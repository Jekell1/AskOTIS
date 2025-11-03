      * COPYMEMBER: LIBGB/GBOPW1RN
       LOAD-OPW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPW-ALL-MACHINE
              MOVE OPW-ALL-PATH             TO OPW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPW-ALL-BASE
              MOVE FILPATH-MACHINE         TO OPW-ALL-MACHINE
              MOVE OPW-ALL-PATH             TO OPW-PATH.
       OPEN-OPW1-FILE.
           PERFORM LOAD-OPW1-FILE.
           PERFORM OPEN-IT.
           MOVE OPW-PATH TO E-FILE.
           OPEN INPUT OPW-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPW1-FILE.
           IF IO-FG = 7
              CLOSE OPW-FILE
              GO TO OPEN-OPW1-FILE.
           UNLOCK OPW-FILE.
       READ-OPW1-FILE.
           PERFORM READ-IT.
           MOVE OPW-PATH TO E-FILE.
           MOVE OPW1-KEY TO E-KEYX.
           READ OPW-FILE.
           IF IO-FG = 8
              GO TO READ-OPW1-FILE.
           UNLOCK OPW-FILE.
       READ-OPW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OPW-PATH TO E-FILE.
           MOVE OPW1-KEY TO E-KEYX.
           READ OPW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPW1-FILE-NEXT.
           UNLOCK OPW-FILE.
       START-OPW1-FILE.
           PERFORM START-IT.
           MOVE OPW-PATH TO E-FILE.
           MOVE OPW1-KEY TO E-KEYX.
           START OPW-FILE KEY NOT < OPW1-KEY.
           UNLOCK OPW-FILE.
       CLOSE-OPW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPW-FILE.
