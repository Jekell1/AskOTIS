      * COPYMEMBER: LIBLP/LPOLSD1R
       LOAD-OLSD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSD-ALL-DATA
              MOVE OLSD-ALL-PATH             TO OLSD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSD-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSD-ALL-DATA
              MOVE OLSD-ALL-PATH             TO OLSD-PATH.
       OPEN-OLSD1-FILE.
           PERFORM LOAD-OLSD1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSD-PATH TO E-FILE.
           OPEN INPUT OLSD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSD1-FILE.
           IF IO-FG = 7
              CLOSE OLSD-FILE
              GO TO OPEN-OLSD1-FILE.
       READ-OLSD1-FILE.
           PERFORM READ-IT.
           MOVE OLSD-PATH TO E-FILE.
           MOVE OLSD1-KEY TO E-KEYX.
           READ OLSD-FILE.
           IF IO-FG = 8
              GO TO READ-OLSD1-FILE.
       START-OLSD1-FILE.
           PERFORM START-IT.
           MOVE OLSD-PATH TO E-FILE.
           MOVE OLSD1-KEY TO E-KEYX.
           START OLSD-FILE KEY NOT < OLSD1-KEY.

       READ-OLSD1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSD-PATH TO E-FILE.
           MOVE OLSD1-KEY TO E-KEYX.
           READ OLSD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSD1-FILE-NEXT.
       CLOSE-OLSD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSD-FILE.
