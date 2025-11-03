      * COPYMEMBER: LIBSP/SPOCJ1RN
       LOAD-OCJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCJ-ALL-DATA
              MOVE OCJ-ALL-PATH             TO OCJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCJ-ALL-DATA
              MOVE OCJ-ALL-PATH             TO OCJ-PATH.
       OPEN-OCJ1-FILE.
*NEW*      PERFORM LOAD-OCJ1-FILE.
           PERFORM OPEN-IT.
           MOVE OCJ-PATH TO E-FILE.
           OPEN INPUT OCJ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCJ1-FILE.
           IF IO-FG = 7
              CLOSE OCJ-FILE
              GO TO OPEN-OCJ1-FILE.
       READ-OCJ1-FILE.
           PERFORM READ-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           READ OCJ-FILE.
           IF IO-FG = 8
              GO TO READ-OCJ1-FILE.
       READ-OCJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           READ OCJ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCJ1-FILE-NEXT.
       START-OCJ1-FILE.
           PERFORM START-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           START OCJ-FILE KEY NOT < OCJ1-KEY.
       CLOSE-OCJ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCJ-FILE.
