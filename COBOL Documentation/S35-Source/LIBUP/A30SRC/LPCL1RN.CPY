      * COPYMEMBER: LIBLP/LPOCL1RN
       LOAD-OCL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCL-ALL-MACHINE
              MOVE OCL-ALL-PATH             TO OCL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCL-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCL-ALL-MACHINE
              MOVE OCL-ALL-PATH             TO OCL-PATH.
       OPEN-OCL1-FILE.
           PERFORM LOAD-OCL1-FILE.
           PERFORM OPEN-IT.
           MOVE OCL-PATH TO E-FILE.
           OPEN INPUT OCL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCL1-FILE.
           IF IO-FG = 7
              CLOSE OCL-FILE
              GO TO OPEN-OCL1-FILE.
       READ-OCL1-FILE.
           PERFORM READ-IT.
           MOVE OCL-PATH TO E-FILE.
           MOVE OCL1-KEY TO E-KEYX.
           READ OCL-FILE.
       READ-OCL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCL-PATH TO E-FILE.
           MOVE OCL1-KEY TO E-KEYX.
           READ OCL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCL1-FILE-NEXT.
       START-OCL1-FILE.
           PERFORM START-IT.
           MOVE OCL-PATH TO E-FILE.
           MOVE OCL1-KEY TO E-KEYX.
           START OCL-FILE KEY NOT < OCL1-KEY.
       CLOSE-OCL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCL-FILE.
