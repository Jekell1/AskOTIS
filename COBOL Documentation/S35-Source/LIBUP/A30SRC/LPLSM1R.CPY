      * COPYMEMBER: LIBLP/LPLS1R
       LOAD-OLSM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSM-ALL-DATA
              MOVE OLSM-ALL-PATH             TO OLSM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSM-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSM-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSM-ALL-DATA
              MOVE OLSM-ALL-PATH             TO OLSM-PATH.
       OPEN-OLSM1-FILE.
           PERFORM LOAD-OLSM1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSM-PATH TO E-FILE.
           OPEN INPUT OLSM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSM1-FILE.
           IF IO-FG = 7
              CLOSE OLSM-FILE
              GO TO OPEN-OLSM1-FILE.
       READ-OLSM1-FILE.
           PERFORM READ-IT.
           MOVE OLSM-PATH TO E-FILE.
           MOVE OLSM1-KEY TO E-KEYX.
           READ OLSM-FILE.
           IF IO-FG = 8
              GO TO READ-OLSM1-FILE.
       READ-OLSM1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLSM-PATH TO E-FILE.
           MOVE OLSM1-KEY TO E-KEYX.
           READ OLSM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSM1-FILE-NEXT.
       START-OLSM1-FILE.
           PERFORM START-IT.
           MOVE OLSM-PATH TO E-FILE.
           MOVE OLSM1-KEY TO E-KEYX.
           START OLSM-FILE KEY NOT < OLSM1-KEY.
       CLOSE-OLSM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSM-FILE.
