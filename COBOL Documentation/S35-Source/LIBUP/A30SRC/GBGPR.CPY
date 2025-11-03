      * COPYMEMBER: LIBGB/GBOGPR
       LOAD-OGP-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OGP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OGP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OGP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OGP-ALL-MACHINE
              MOVE OGP-ALL-PATH             TO OGP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OGP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OGP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OGP-ALL-MACHINE
              MOVE OGP-ALL-PATH             TO OGP-PATH.
       OPEN-OGP-FILE.
           PERFORM LOAD-OGP-FILE.
           PERFORM OPEN-IT.
           MOVE OGP-PATH TO E-FILE.
           OPEN INPUT OGP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGP-FILE.
           IF IO-FG = 7
              CLOSE OGP-FILE
              GO TO OPEN-OGP-FILE.
       READ-OGP-FILE.
           PERFORM READ-IT.
           MOVE OGP-PATH TO E-FILE.
           MOVE OGP-KEY TO E-KEYX.
           READ OGP-FILE.
           IF IO-FG = 8
              GO TO READ-OGP-FILE.
       CLOSE-OGP-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGP-FILE.
