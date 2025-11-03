      * COPYMEMBER: LIBLP/LPOLSX1RN
       LOAD-OLSX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLSX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLSX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLSX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLSX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLSX-ALL-DATA
              MOVE OLSX-ALL-PATH            TO OLSX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLSX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLSX-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLSX-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLSX-ALL-DATA
              MOVE OLSX-ALL-PATH            TO OLSX-PATH.
       OPEN-OLSX1-FILE.
           PERFORM LOAD-OLSX1-FILE.
           PERFORM OPEN-IT.
           MOVE OLSX-PATH TO E-FILE.
           OPEN INPUT OLSX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLSX1-FILE.
           IF IO-FG = 7
              CLOSE OLSX-FILE
              GO TO OPEN-OLSX1-FILE.
       READ-OLSX1-FILE.
           PERFORM READ-IT.
           MOVE OLSX-PATH TO E-FILE.
           MOVE OLSX1-KEY TO E-KEYX.
           READ OLSX-FILE.
           IF IO-FG = 8
              GO TO READ-OLSX1-FILE.
       READ-OLSX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLSX-PATH TO E-FILE.
           MOVE OLSX1-KEY TO E-KEYX.
           READ OLSX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLSX1-FILE-NEXT.
       START-OLSX1-FILE.
           PERFORM START-IT.
           MOVE OLSX-PATH TO E-FILE.
           MOVE OLSX1-KEY TO E-KEYX.
           START OLSX-FILE KEY NOT < OLSX1-KEY.
       CLOSE-OLSX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLSX-FILE.
       START-OLSX1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE OLSX-PATH TO E-FILE.
           MOVE OLSX1-KEY TO E-KEYX.
           START OLSX-FILE KEY NOT > OLSX1-KEY.
       READ-OLSX1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OLSX-PATH TO E-FILE.
           MOVE OLSX1-KEY TO E-KEYX.
           READ OLSX-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OLSX1-FILE-PREVIOUS.
