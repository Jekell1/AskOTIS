      * COPYMEMBER: LIBLP/LPOLSF1R
       LOAD-OLSF1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSF-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSF-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSF-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSF-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSF-ALL-DATA
              MOVE OLSF-ALL-PATH             TO OLSF-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSF-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSF-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSF-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSF-ALL-DATA
              MOVE OLSF-ALL-PATH             TO OLSF-PATH.
       OPEN-OLSF1-FILE.
           PERFORM LOAD-OLSF1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSF-PATH TO E-FILE.
           OPEN INPUT OLSF-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSF1-FILE.
           IF IO-FG = 7
              CLOSE OLSF-FILE
              GO TO OPEN-OLSF1-FILE.
       READ-OLSF1-FILE.
           PERFORM READ-IT.
           MOVE OLSF-PATH TO E-FILE.
           MOVE OLSF1-KEY TO E-KEYX.
           READ OLSF-FILE.
           IF IO-FG = 8
              GO TO READ-OLSF1-FILE.
       READ-OLSF1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSF-PATH TO E-FILE.
           MOVE OLSF1-KEY TO E-KEYX.
           READ OLSF-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSF1-FILE-NEXT.
       START-OLSF1-FILE.
           PERFORM START-IT.
           MOVE OLSF-PATH TO E-FILE.
           MOVE OLSF1-KEY TO E-KEYX.
           START OLSF-FILE KEY NOT < OLSF1-KEY.
       CLOSE-OLSF1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSF-FILE.
