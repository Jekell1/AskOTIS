      * COPYMEMBER: LIBLP/LPOCK1RN
       LOAD-OCK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCK-ALL-DATA
              MOVE OCK-ALL-PATH             TO OCK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCK-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCK-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCK-ALL-DATA
              MOVE OCK-ALL-PATH             TO OCK-PATH.
       OPEN-OCK1-FILE.
           PERFORM LOAD-OCK1-FILE.
           PERFORM OPEN-IT.
           MOVE OCK-PATH TO E-FILE.
           OPEN INPUT OCK-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCK1-FILE.
           IF IO-FG = 7
              CLOSE OCK-FILE
              GO TO OPEN-OCK1-FILE.
       READ-OCK1-FILE.
           PERFORM READ-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           READ OCK-FILE.
           IF IO-FG = 8
              GO TO READ-OCK1-FILE.
       READ-OCK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           READ OCK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCK1-FILE-NEXT.
       START-OCK1-FILE.
           PERFORM START-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           START OCK-FILE KEY NOT < OCK1-KEY.
       CLOSE-OCK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCK-FILE.
