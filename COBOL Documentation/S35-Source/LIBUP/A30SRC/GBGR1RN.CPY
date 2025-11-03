      * COPYMEMBER: LIBGB/GBOGR1RN
       LOAD-OGR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OGR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OGR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OGR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OGR-ALL-MACHINE
              MOVE OGR-ALL-PATH             TO OGR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *- - - -{ DO NOT KNOW WHERE THIS WOULD BE USED, }- - - - - - - - -
      *- - - -{ BUT STAYING CONSISTENT  }- - - - - - - - - - - - - - - -
           IF ( OGR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OGR-ALL-BASE
              MOVE FILPATH-MACHINE         TO OGR-ALL-MACHINE
              MOVE OGR-ALL-PATH             TO OGR-PATH.
       OPEN-OGR1-FILE.
           PERFORM LOAD-OGR1-FILE.
           PERFORM OPEN-IT.
           MOVE OGR-PATH TO E-FILE.
           OPEN INPUT OGR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGR1-FILE.
           IF IO-FG = 7
              CLOSE OGR-FILE
              GO TO OPEN-OGR1-FILE.
       READ-OGR1-FILE.
           PERFORM READ-IT.
           MOVE OGR-PATH TO E-FILE.
           MOVE OGR1-KEY TO E-KEYX.
           READ OGR-FILE.
           IF IO-FG = 8
              GO TO READ-OGR1-FILE.
       READ-OGR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGR-PATH TO E-FILE.
           MOVE OGR1-KEY TO E-KEYX.
           READ OGR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGR1-FILE-NEXT.
       START-OGR1-FILE.
           PERFORM START-IT.
           MOVE OGR-PATH TO E-FILE.
           MOVE OGR1-KEY TO E-KEYX.
           START OGR-FILE KEY NOT < OGR1-KEY.
       CLOSE-OGR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGR-FILE.
