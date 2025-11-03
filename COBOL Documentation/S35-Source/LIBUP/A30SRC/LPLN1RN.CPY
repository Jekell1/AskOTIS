      * COPYMEMBER: LIBLP/LPOLN1RN
       LOAD-OLN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLN-ALL-DATA
              MOVE OLN-ALL-PATH             TO OLN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLN-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLN-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLN-ALL-DATA
              MOVE OLN-ALL-PATH             TO OLN-PATH.

       OPEN-OLN1-FILE.
           PERFORM LOAD-OLN1-FILE.
           PERFORM OPEN-IT.
           MOVE OLN-PATH TO E-FILE.
           OPEN INPUT OLN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLN1-FILE.
           IF IO-FG = 7
              CLOSE OLN-FILE
              GO TO OPEN-OLN1-FILE.
       READ-OLN1-FILE.
           PERFORM READ-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           READ OLN-FILE.
           IF IO-FG = 8
              GO TO READ-OLN1-FILE.
       READ-OLN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           READ OLN-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLN1-FILE-NEXT.
       START-OLN1-FILE.
           PERFORM START-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           START OLN-FILE KEY NOT < OLN1-KEY.
       START-OLN1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           START OLN-FILE KEY > OLN1-KEY.
       CLOSE-OLN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLN-FILE.
