      * COPYMEMBER: LIBLP/LPOLSP1
       LOAD-OLSP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSP-ALL-DATA
              MOVE OLSP-ALL-PATH            TO OLSP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSP-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSP-ALL-DATA
              MOVE OLSP-ALL-PATH             TO OLSP-PATH.
       OPEN-OLSP1-FILE.
           PERFORM LOAD-OLSP1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSP-PATH TO E-FILE.
           OPEN INPUT OLSP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSP1-FILE.
           IF IO-FG = 7
              CLOSE OLSP-FILE
              GO TO OPEN-OLSP1-FILE.
       READ-OLSP1-FILE.
           PERFORM READ-IT.
           MOVE OLSP-PATH TO E-FILE.
           MOVE OLSP1-KEY TO E-KEYX.
           READ OLSP-FILE.
           IF IO-FG = 8
              GO TO READ-OLSP1-FILE.
       READ-OLSP1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSP-PATH TO E-FILE.
           MOVE OLSP1-KEY TO E-KEYX.
           READ OLSP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSP1-FILE-NEXT.
       START-OLSP1-FILE.
           PERFORM START-IT.
           MOVE OLSP-PATH TO E-FILE.
           MOVE OLSP1-KEY TO E-KEYX.
           START OLSP-FILE KEY NOT < OLSP1-KEY.
       CLOSE-OLSP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSP-FILE.
