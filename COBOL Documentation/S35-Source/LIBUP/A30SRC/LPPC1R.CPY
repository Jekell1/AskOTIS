      *A30 COMPLETED
      * COPYMEMBER: LIBLP/LPOPC1R
       LOAD-OPC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OPC-ALL-DATA
              MOVE OPC-ALL-PATH             TO OPC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPC-ALL-BASE
              MOVE FILPATH-MACHINE         TO OPC-ALL-MACHINE
              MOVE FILPATH-DATA            TO OPC-ALL-DATA
              MOVE OPC-ALL-PATH             TO OPC-PATH.
       OPEN-OPC1-FILE.
           PERFORM LOAD-OPC1-FILE.
           PERFORM OPEN-IT.
           MOVE OPC-PATH TO E-FILE.
           OPEN INPUT OPC-FILE.
           IF IO-FG = 8
             GO TO OPEN-OPC1-FILE.
           IF IO-FG = 7
             CLOSE OPC-FILE
             GO TO OPEN-OPC1-FILE.
           UNLOCK OPC-FILE.
       READ-OPC1-FILE.
           PERFORM READ-IT.
           MOVE OPC-PATH TO E-FILE.
           MOVE OPC1-KEY TO E-KEYX.
           READ OPC-FILE.
           IF IO-FG = 8
             GO TO READ-OPC1-FILE.
           UNLOCK OPC-FILE.
       START-OPC1-FILE.
           PERFORM START-IT.
           MOVE OPC-PATH TO E-FILE.
           MOVE OPC1-KEY TO E-KEYX.
           START OPC-FILE KEY NOT < OPC1-KEY.
       READ-OPC1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OPC-PATH TO E-FILE.
           MOVE OPC1-KEY TO E-KEYX.
           READ OPC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPC1-FILE-NEXT.
       CLOSE-OPC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPC-FILE.
