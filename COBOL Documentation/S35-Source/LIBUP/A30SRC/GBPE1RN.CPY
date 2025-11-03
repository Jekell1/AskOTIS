      * COPYMEMBER: LIBGB/GBOPE1RN
       LOAD-OPE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPE-ALL-MACHINE
              MOVE OPE-ALL-PATH             TO OPE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPE-ALL-BASE
              MOVE FILPATH-MACHINE         TO OPE-ALL-MACHINE
              MOVE OPE-ALL-PATH             TO OPE-PATH.
       OPEN-OPE1-FILE.
           PERFORM LOAD-OPE1-FILE.
           PERFORM OPEN-IT.
           MOVE OPE-PATH TO E-FILE.
           OPEN INPUT OPE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPE1-FILE.
           IF IO-FG = 7
              CLOSE OPE-FILE
              GO TO OPEN-OPE1-FILE.
           UNLOCK OPE-FILE.
       READ-OPE1-FILE.
           PERFORM READ-IT.
           MOVE OPE-PATH TO E-FILE.
           MOVE OPE1-KEY TO E-KEYX.
           READ OPE-FILE.
           IF IO-FG = 8
              GO TO READ-OPE1-FILE.
           UNLOCK OPE-FILE.
       READ-OPE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OPE-PATH TO E-FILE.
           MOVE OPE1-KEY TO E-KEYX.
           READ OPE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPE1-FILE-NEXT.
           UNLOCK OPE-FILE.
       START-OPE1-FILE.
           PERFORM START-IT.
           MOVE OPE-PATH TO E-FILE.
           MOVE OPE1-KEY TO E-KEYX.
           START OPE-FILE KEY NOT < OPE1-KEY.
           UNLOCK OPE-FILE.
       CLOSE-OPE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPE-FILE.
