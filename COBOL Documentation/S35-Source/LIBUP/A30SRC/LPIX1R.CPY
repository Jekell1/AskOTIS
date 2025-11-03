      * COPYMEMBER: LIBLP/LPOIX1R
       LOAD-OIX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OIX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OIX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OIX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OIX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OIX-ALL-DATA
              MOVE OIX-ALL-PATH             TO OIX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OIX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OIX-ALL-BASE
              MOVE FILPATH-MACHINE         TO OIX-ALL-MACHINE
              MOVE FILPATH-DATA            TO OIX-ALL-DATA
              MOVE OIX-ALL-PATH             TO OIX-PATH.
       OPEN-OIX1-FILE.
           PERFORM LOAD-OIX1-FILE.
           PERFORM OPEN-IT.
           MOVE OIX-PATH TO E-FILE.
           OPEN INPUT OIX-FILE.
           IF IO-FG = 8
             GO TO OPEN-OIX1-FILE.
           IF IO-FG = 7
             CLOSE OIX-FILE
             GO TO OPEN-OIX1-FILE.
       READ-OIX1-FILE.
           PERFORM READ-IT.
           MOVE OIX-PATH TO E-FILE.
           MOVE OIX1-KEY TO E-KEYX.
           READ OIX-FILE.
           IF IO-FG = 8
             GO TO READ-OIX1-FILE.
       READ-OIX1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OIX-PATH TO E-FILE.
           MOVE OIX1-KEY TO E-KEYX.
           READ OIX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIX1-FILE-NEXT.
       START-OIX1-FILE.
           PERFORM START-IT.
           MOVE OIX-PATH TO E-FILE.
           MOVE OIX1-KEY TO E-KEYX.
           START OIX-FILE KEY NOT < OIX1-KEY.

       CLOSE-OIX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIX-FILE.
