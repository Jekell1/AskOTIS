      * COPYMEMBER: LIBLP/LPOUD1RN
       LOAD-OUD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OUD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OUD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OUD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OUD-ALL-MACHINE
              MOVE OUD-ALL-PATH             TO OUD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OUD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OUD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OUD-ALL-MACHINE
              MOVE OUD-ALL-PATH             TO OUD-PATH.
       OPEN-OUD1-FILE.
           PERFORM LOAD-OUD1-FILE.
           PERFORM OPEN-IT.
           MOVE OUD-PATH TO E-FILE.
           OPEN INPUT OUD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OUD1-FILE.
           IF IO-FG = 7
              CLOSE OUD-FILE
              GO TO OPEN-OUD1-FILE.
       READ-OUD1-FILE.
           PERFORM READ-IT.
           MOVE OUD-PATH TO E-FILE.
           MOVE OUD1-KEY TO E-KEYX.
           READ OUD-FILE.
           IF IO-FG = 8
              GO TO READ-OUD1-FILE.
       READ-OUD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OUD-PATH TO E-FILE.
           MOVE OUD1-KEY TO E-KEYX.
           READ OUD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OUD1-FILE-NEXT.
       START-OUD1-FILE.
           PERFORM START-IT.
           MOVE OUD-PATH TO E-FILE.
           MOVE OUD1-KEY TO E-KEYX.
           START OUD-FILE KEY NOT < OUD1-KEY.
       CLOSE-OUD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OUD-FILE.
