      * COPYMEMBER: LIBLP/LPOSP1RN
       LOAD-OSP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OSP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OSP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OSP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OSP-ALL-MACHINE
              MOVE OSP-ALL-PATH             TO OSP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OSP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OSP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OSP-ALL-MACHINE
              MOVE OSP-ALL-PATH             TO OSP-PATH.

       OPEN-OSP1-FILE.
           PERFORM LOAD-OSP1-FILE.
           PERFORM OPEN-IT.
           MOVE OSP-PATH TO E-FILE.
           OPEN INPUT OSP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSP1-FILE.
           IF IO-FG = 7
              CLOSE OSP-FILE
              GO TO OPEN-OSP1-FILE.
       READ-OSP1-FILE.
           PERFORM READ-IT.
           MOVE OSP-PATH TO E-FILE.
           MOVE OSP1-KEY TO E-KEYX.
           READ OSP-FILE.
           IF IO-FG = 8
              GO TO READ-OSP1-FILE.
       READ-OSP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSP-PATH TO E-FILE.
           MOVE OSP1-KEY TO E-KEYX.
           READ OSP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSP1-FILE-NEXT.
       START-OSP1-FILE.
           PERFORM START-IT.
           MOVE OSP-PATH TO E-FILE.
           MOVE OSP1-KEY TO E-KEYX.
           START OSP-FILE KEY NOT < OSP1-KEY.
       CLOSE-OSP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSP-FILE.
