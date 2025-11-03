      * COPYMEMBER: LIBLP/LPOST1RN
       LOAD-OST1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OST-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OST-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OST-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OST-ALL-MACHINE
              MOVE OST-ALL-PATH             TO OST-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OST-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OST-ALL-BASE
              MOVE FILPATH-MACHINE         TO OST-ALL-MACHINE
              MOVE OST-ALL-PATH             TO OST-PATH.

       OPEN-OST1-FILE.
           PERFORM LOAD-OST1-FILE.
           PERFORM OPEN-IT.
           MOVE OST-PATH TO E-FILE.
           OPEN INPUT OST-FILE.
           IF IO-FG = 8
              GO TO OPEN-OST1-FILE.
           IF IO-FG = 7
              CLOSE OST-FILE
              GO TO OPEN-OST1-FILE.
       READ-OST1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OST-PATH TO E-FILE.
           MOVE OST1-KEY TO E-KEYX.
           READ OST-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OST1-FILE-NEXT.
       START-OST1-FILE.
           PERFORM START-IT.
           MOVE OST-PATH TO E-FILE.
           MOVE OST1-KEY TO E-KEYX.
           START OST-FILE KEY NOT < OST1-KEY.
       CLOSE-OST1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OST-FILE.
