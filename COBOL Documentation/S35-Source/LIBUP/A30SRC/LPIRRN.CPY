      * COPYMEMBER: LIBLP/LPOIRRN
       LOAD-OIR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OIR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OIR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OIR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OIR-ALL-MACHINE
              MOVE OIR-ALL-PATH             TO OIR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OIR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OIR-ALL-BASE
              MOVE FILPATH-MACHINE         TO OIR-ALL-MACHINE
              MOVE OIR-ALL-PATH             TO OIR-PATH.
       OPEN-OIR-FILE.
           PERFORM LOAD-OIR-FILE.
           PERFORM OPEN-IT.
           MOVE OIR-PATH TO E-FILE.
           OPEN INPUT OIR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OIR-FILE.
           IF IO-FG = 7
              CLOSE OIR-FILE
              GO TO OPEN-OIR-FILE.
       READ-OIR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OIR-PATH TO E-FILE.
           MOVE OIR-KEY TO E-KEYX.
           READ OIR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIR-FILE-NEXT.
       START-OIR-FILE.
           PERFORM START-IT.
           MOVE OIR-PATH TO E-FILE.
           MOVE OIR-KEY TO E-KEYX.
           START OIR-FILE KEY NOT < OIR-KEY.
       CLOSE-OIR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIR-FILE.
