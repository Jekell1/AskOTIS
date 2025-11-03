      * COPYMEMBER: LIBLP/LPOML1RN
       LOAD-OML1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OML-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OML-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OML-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OML-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OML-ALL-DATA
              MOVE OML-ALL-PATH             TO OML-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OML-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OML-ALL-BASE
              MOVE FILPATH-MACHINE         TO OML-ALL-MACHINE
              MOVE FILPATH-DATA            TO OML-ALL-DATA
              MOVE OML-ALL-PATH             TO OML-PATH.
       OPEN-OML1-FILE.
           PERFORM LOAD-OML1-FILE.
           PERFORM OPEN-IT.
           MOVE OML-PATH TO E-FILE.
           OPEN INPUT OML-FILE.
           IF IO-FG = 8
              GO TO OPEN-OML1-FILE.
           IF IO-FG = 7
              CLOSE OML-FILE
              GO TO OPEN-OML1-FILE.
       READ-OML1-FILE.
           PERFORM READ-IT.
           MOVE OML-PATH TO E-FILE.
           MOVE OML1-KEY TO E-KEYX.
           READ OML-FILE.
           IF IO-FG = 8
              GO TO READ-OML1-FILE.
       READ-OML1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OML-PATH TO E-FILE.
           MOVE OML1-KEY TO E-KEYX.
           READ OML-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OML1-FILE-NEXT.
       START-OML1-FILE.
           PERFORM START-IT.
           MOVE OML-PATH TO E-FILE.
           MOVE OML1-KEY TO E-KEYX.
           START OML-FILE KEY NOT < OML1-KEY.
       CLOSE-OML1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OML-FILE.
