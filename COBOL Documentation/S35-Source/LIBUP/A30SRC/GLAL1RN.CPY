      * COPYMEMBER: LIBGL/GLOAL1RN
       LOAD-OAL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAL-ALL-MACHINE
              MOVE OAL-ALL-PATH             TO OAL-PATH
           ELSE
      *- - - -{ USE BRANCH VOALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAL-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAL-ALL-MACHINE
              MOVE OAL-ALL-PATH             TO OAL-PATH.
       OPEN-OAL1-FILE.
           PERFORM LOAD-OAL1-FILE.
           PERFORM OPEN-IT.
           MOVE OAL-PATH TO E-FILE.
           OPEN INPUT OAL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAL1-FILE.
           IF IO-FG = 7
              CLOSE OAL-FILE
              GO TO OPEN-OAL1-FILE.
       READ-OAL1-FILE.
           PERFORM READ-IT.
           MOVE OAL-PATH TO E-FILE.
           MOVE OAL1-KEY TO E-KEYX.
           READ OAL-FILE.
           IF IO-FG = 8
              GO TO READ-OAL1-FILE.
       READ-OAL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAL-PATH TO E-FILE.
           MOVE OAL1-KEY TO E-KEYX.
           READ OAL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAL1-FILE-NEXT.
       START-OAL1-FILE.
           PERFORM START-IT.
           MOVE OAL-PATH TO E-FILE.
           MOVE OAL1-KEY TO E-KEYX.
           START OAL-FILE KEY NOT < OAL1-KEY.
       CLOSE-OAL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAL-FILE.
