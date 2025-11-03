      * COPYMEMBER: LIBLP/LPOLSJ1R
       LOAD-OLSJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSJ-ALL-DATA
              MOVE OLSJ-ALL-PATH             TO OLSJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSJ-ALL-DATA
              MOVE OLSJ-ALL-PATH             TO OLSJ-PATH.
       OPEN-OLSJ1-FILE.
           PERFORM LOAD-OLSJ1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSJ-PATH TO E-FILE.
           OPEN INPUT OLSJ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSJ1-FILE.
           IF IO-FG = 7
              CLOSE OLSJ-FILE
              GO TO OPEN-OLSJ1-FILE.
       READ-OLSJ1-FILE.
           PERFORM READ-IT.
           MOVE OLSJ-PATH TO E-FILE.
           MOVE OLSJ1-KEY TO E-KEYX.
           READ OLSJ-FILE.
           IF IO-FG = 8
              GO TO READ-OLSJ1-FILE.
       READ-OLSJ1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSJ-PATH TO E-FILE.
           MOVE OLSJ1-KEY TO E-KEYX.
           READ OLSJ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSJ1-FILE-NEXT.
       START-OLSJ1-FILE.
           PERFORM START-IT.
           MOVE OLSJ-PATH TO E-FILE.
           MOVE OLSJ1-KEY TO E-KEYX.
           START OLSJ-FILE KEY NOT < OLSJ1-KEY.
       CLOSE-OLSJ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSJ-FILE.
