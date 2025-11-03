      * COPYMEMBER: LIBLP/LPOLTB1R
       LOAD-OLTB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTB-ALL-DATA
              MOVE OLTB-ALL-PATH             TO OLTB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTB-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTB-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTB-ALL-DATA
              MOVE OLTB-ALL-PATH             TO OLTB-PATH.

       OPEN-OLTB1-FILE.
           PERFORM LOAD-OLTB1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTB-PATH TO E-FILE.
           OPEN INPUT OLTB-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLTB1-FILE.
           IF IO-FG = 7
              CLOSE OLTB-FILE
              GO TO OPEN-OLTB1-FILE.
       READ-OLTB1-FILE.
           PERFORM READ-IT.
           MOVE OLTB-PATH TO E-FILE.
           MOVE OLTB1-KEY TO E-KEYX.
           READ OLTB-FILE.
           IF IO-FG = 8
              GO TO READ-OLTB1-FILE.
       READ-OLTB1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLTB-PATH TO E-FILE.
           MOVE OLTB1-KEY TO E-KEYX.
           READ OLTB-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLTB1-FILE-NEXT.
       CLOSE-OLTB1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTB-FILE.
