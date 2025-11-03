      * COPYMEMBER: LIBOGB/OGBOGBRN
       LOAD-OGB-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OGB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OGB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OGB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OGB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OGB-ALL-DATA
              MOVE OGB-ALL-PATH             TO OGB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OGB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OGB-ALL-BASE
              MOVE FILPATH-MACHINE         TO OGB-ALL-MACHINE
              MOVE FILPATH-DATA            TO OGB-ALL-DATA
              MOVE OGB-ALL-PATH             TO OGB-PATH.
       OPEN-OGB-FILE.
           PERFORM LOAD-OGB-FILE.
           PERFORM OPEN-IT.
           MOVE OGB-PATH TO E-FILE.
           OPEN INPUT OGB-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGB-FILE.
           IF IO-FG = 7
              CLOSE OGB-FILE
              GO TO OPEN-OGB-FILE.
           UNLOCK OGB-FILE.
       READ-OGB-FILE.
           PERFORM READ-IT.
           MOVE OGB-PATH TO E-FILE.
           MOVE OGB-KEY TO E-KEYX.
           READ OGB-FILE.
           IF IO-FG = 8
              GO TO READ-OGB-FILE.
           UNLOCK OGB-FILE.
       READ-OGB-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGB-PATH TO E-FILE.
           MOVE OGB-KEY TO E-KEYX.
           READ OGB-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGB-FILE-NEXT.
           UNLOCK OGB-FILE.
       START-OGB-FILE.
           PERFORM START-IT.
           MOVE OGB-PATH TO E-FILE.
           MOVE OGB-KEY TO E-KEYX.
           START OGB-FILE KEY NOT < OGB-KEY.
           UNLOCK OGB-FILE.
       CLOSE-OGB-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGB-FILE.
