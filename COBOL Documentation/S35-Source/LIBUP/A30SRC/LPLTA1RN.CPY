      * COPYMEMBER: LIBLP/LPOLTA1RN
       LOAD-OLTA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTA-ALL-DATA
              MOVE OLTA-ALL-PATH             TO OLTA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTA-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTA-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTA-ALL-DATA
              MOVE OLTA-ALL-PATH             TO OLTA-PATH.
       OPEN-OLTA1-FILE.
           PERFORM LOAD-OLTA1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTA-PATH TO E-FILE.
           OPEN INPUT OLTA-FILE.
           IF IO-FG = 8
             GO TO OPEN-OLTA1-FILE.
           IF IO-FG = 7
             CLOSE OLTA-FILE
             GO TO OPEN-OLTA1-FILE.
       READ-OLTA1-FILE.
           PERFORM READ-IT.
           MOVE OLTA-PATH TO E-FILE.
           MOVE OLTA1-KEY TO E-KEYX.
           READ OLTA-FILE.
           IF IO-FG = 8
              GO TO READ-OLTA1-FILE.
       READ-OLTA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLTA-PATH TO E-FILE.
           MOVE OLTA1-KEY TO E-KEYX.
           READ OLTA-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLTA1-FILE-NEXT.
       START-OLTA1-FILE.
           PERFORM START-IT.
           MOVE OLTA-PATH TO E-FILE.
           MOVE OLTA1-KEY TO E-KEYX.
           START OLTA-FILE KEY NOT < OLTA1-KEY.
       CLOSE-OLTA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTA-FILE.
