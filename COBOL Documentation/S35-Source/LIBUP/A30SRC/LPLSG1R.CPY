      * COPYMEMBER: LIBLP/LPOLSG1R
       LOAD-OLSG1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSG-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSG-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSG-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSG-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSG-ALL-DATA
              MOVE OLSG-ALL-PATH             TO OLSG-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSG-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSG-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSG-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSG-ALL-DATA
              MOVE OLSG-ALL-PATH             TO OLSG-PATH.
       OPEN-OLSG1-FILE.
           PERFORM LOAD-OLSG1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSG-PATH TO E-FILE.
           OPEN INPUT OLSG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSG1-FILE.
           IF IO-FG = 7
              CLOSE OLSG-FILE
              GO TO OPEN-OLSG1-FILE.
       READ-OLSG1-FILE.
           PERFORM READ-IT.
           MOVE OLSG-PATH TO E-FILE.
           MOVE OLSG1-KEY TO E-KEYX.
           READ OLSG-FILE.
           IF IO-FG = 8
              GO TO READ-OLSG1-FILE.
       READ-OLSG1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSG-PATH TO E-FILE.
           MOVE OLSG1-KEY TO E-KEYX.
           READ OLSG-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSG1-FILE-NEXT.
       START-OLSG1-FILE.
           PERFORM START-IT.
           MOVE OLSG-PATH TO E-FILE.
           MOVE OLSG1-KEY TO E-KEYX.
           START OLSG-FILE KEY NOT < OLSG1-KEY.
       CLOSE-OLSG1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSG-FILE.
