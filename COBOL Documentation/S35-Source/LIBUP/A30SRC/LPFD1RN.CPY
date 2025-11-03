      * COPYMEMBER: LIBLP/LPOFD1RN
       LOAD-OFD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OFD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OFD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OFD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OFD-ALL-MACHINE
              MOVE OFD-ALL-PATH             TO OFD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OFD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OFD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OFD-ALL-MACHINE
              MOVE OFD-ALL-PATH             TO OFD-PATH.
       OPEN-OFD1-FILE.
           PERFORM LOAD-OFD1-FILE.
           PERFORM OPEN-IT.
           MOVE OFD-PATH TO E-FILE.
           OPEN INPUT OFD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OFD1-FILE.
           IF IO-FG = 7
              CLOSE OFD-FILE
              GO TO OPEN-OFD1-FILE.
       READ-OFD1-FILE.
           PERFORM READ-IT.
           MOVE OFD-PATH TO E-FILE.
           MOVE OFD1-KEY TO E-KEYX.
           READ OFD-FILE.
           IF IO-FG = 8
              GO TO READ-OFD1-FILE.
       READ-OFD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OFD-PATH TO E-FILE.
           MOVE OFD1-KEY TO E-KEYX.
           READ OFD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OFD1-FILE-NEXT.
       START-OFD1-FILE.
           PERFORM START-IT.
           MOVE OFD-PATH TO E-FILE.
           MOVE OFD1-KEY TO E-KEYX.
           START OFD-FILE KEY NOT < OFD1-KEY.
       CLOSE-OFD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OFD-FILE.
