      * COPYMEMBER: LIBLP/LPLT1R
       LOAD-OLTF1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTF-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTF-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTF-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTF-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTF-ALL-DATA
              MOVE OLTF-ALL-PATH             TO OLTF-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTF-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTF-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTF-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTF-ALL-DATA
              MOVE OLTF-ALL-PATH             TO OLTF-PATH.

       OPEN-OLTF1-FILE.
           PERFORM LOAD-OLTF1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTF-PATH TO E-FILE.
           OPEN INPUT OLTF-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLTF1-FILE.
           IF IO-FG = 7
              CLOSE OLTF-FILE
              GO TO OPEN-OLTF1-FILE.
       READ-OLTF1-FILE.
           PERFORM READ-IT.
           MOVE OLTF-PATH TO E-FILE.
           MOVE OLTF1-KEY TO E-KEYX.
           READ OLTF-FILE.
           IF IO-FG = 8
              GO TO READ-OLTF1-FILE.
       START-OLTF1-FILE.
           PERFORM START-IT.
           MOVE OLTF-PATH TO E-FILE.
           MOVE OLTF1-KEY TO E-KEYX.
           START OLTF-FILE KEY NOT < OLTF1-KEY.
       READ-OLTF1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLTF-PATH TO E-FILE.
           MOVE OLTF1-KEY TO E-KEYX.
           READ OLTF-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLTF1-FILE-NEXT.
       CLOSE-OLTF1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTF-FILE.
