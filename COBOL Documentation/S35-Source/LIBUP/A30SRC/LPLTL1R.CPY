      * COPYMEMBER: LIBLP/LPOLTL1R
       LOAD-OLTL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTL-ALL-DATA
              MOVE OLTL-ALL-PATH             TO OLTL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTL-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTL-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTL-ALL-DATA
              MOVE OLTL-ALL-PATH             TO OLTL-PATH.

       OPEN-OLTL1-FILE.
           PERFORM LOAD-OLTL1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTL-PATH TO E-FILE.
           OPEN INPUT OLTL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLTL1-FILE.
           IF IO-FG = 7
              CLOSE OLTL-FILE
              GO TO OPEN-OLTL1-FILE.
       READ-OLTL1-FILE.
           PERFORM READ-IT.
           MOVE OLTL-PATH TO E-FILE.
           MOVE OLTL1-KEY TO E-KEYX.
           READ OLTL-FILE.
           IF IO-FG = 8
              GO TO READ-OLTL1-FILE.
       START-OLTL1-FILE.
           PERFORM START-IT.
           MOVE OLTL-PATH TO E-FILE.
           MOVE OLTL1-KEY TO E-KEYX.
           START OLTL-FILE KEY NOT < OLTL1-KEY.
       READ-OLTL1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLTL-PATH TO E-FILE.
           MOVE OLTL1-KEY TO E-KEYX.
           READ OLTL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLTL1-FILE-NEXT.
       CLOSE-OLTL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTL-FILE.
