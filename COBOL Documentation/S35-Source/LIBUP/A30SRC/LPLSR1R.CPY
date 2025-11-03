      * COPYMEMBER: LIBLP/LPOLSR1R
       LOAD-OLSR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSR-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSR-ALL-DATA
              MOVE OLSR-ALL-PATH             TO OLSR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSR-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSR-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSR-ALL-DATA
              MOVE OLSR-ALL-PATH             TO OLSR-PATH.
       OPEN-OLSR1-FILE.
           PERFORM LOAD-OLSR1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSR-PATH TO E-FILE.
           OPEN INPUT OLSR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSR1-FILE.
           IF IO-FG = 7
              CLOSE OLSR-FILE
              GO TO OPEN-OLSR1-FILE.
       READ-OLSR1-FILE.
           PERFORM READ-IT.
           MOVE OLSR-PATH TO E-FILE.
           MOVE OLSR1-KEY TO E-KEYX.
           READ OLSR-FILE.
           IF IO-FG = 8
              GO TO READ-OLSR1-FILE.
       READ-OLSR1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSR-PATH TO E-FILE.
           MOVE OLSR1-KEY TO E-KEYX.
           READ OLSR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSR1-FILE-NEXT.
       START-OLSR1-FILE.
           PERFORM START-IT.
           MOVE OLSR-PATH TO E-FILE.
           MOVE OLSR1-KEY TO E-KEYX.
           START OLSR-FILE KEY NOT < OLSR1-KEY.
       CLOSE-OLSR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSR-FILE.
