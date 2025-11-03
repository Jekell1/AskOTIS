      * COPYMEMBER: LIBLP/LPOLSL1R
       LOAD-OLSL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSL-ALL-DATA
              MOVE OLSL-ALL-PATH            TO OLSL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSL-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSL-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSL-ALL-DATA
              MOVE OLSL-ALL-PATH            TO OLSL-PATH.
       OPEN-OLSL1-FILE.
           PERFORM LOAD-OLSL1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSL-PATH TO E-FILE.
           OPEN INPUT OLSL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSL1-FILE.
           IF IO-FG = 7
              CLOSE OLSL-FILE
              GO TO OPEN-OLSL1-FILE.
       READ-OLSL1-FILE.
           PERFORM READ-IT.
           MOVE OLSL-PATH TO E-FILE.
           MOVE OLSL1-KEY TO E-KEYX.
           READ OLSL-FILE.
           IF IO-FG = 8
              GO TO READ-OLSL1-FILE.
       READ-OLSL1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSL-PATH TO E-FILE.
           MOVE OLSL1-KEY TO E-KEYX.
           READ OLSL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSL1-FILE-NEXT.
       START-OLSL1-FILE.
           PERFORM START-IT.
           MOVE OLSL-PATH TO E-FILE.
           MOVE OLSL1-KEY TO E-KEYX.
           START OLSL-FILE KEY NOT < OLSL1-KEY.

       CLOSE-OLSL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSL-FILE.
