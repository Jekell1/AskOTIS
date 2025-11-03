      * COPYMEMBER: LIBLP/LPOCI1RN
       LOAD-OCI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCI-ALL-DATA
              MOVE OCI-ALL-PATH             TO OCI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCI-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCI-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCI-ALL-DATA
              MOVE OCI-ALL-PATH             TO OCI-PATH.
       OPEN-OCI1-FILE.
           PERFORM LOAD-OCI1-FILE.
           PERFORM OPEN-IT.
           MOVE OCI-PATH TO E-FILE.
           OPEN INPUT OCI-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCI1-FILE.
           IF IO-FG = 7
              CLOSE OCI-FILE
              GO TO OPEN-OCI1-FILE.
       READ-OCI1-FILE.
           PERFORM READ-IT.
           MOVE OCI-PATH TO E-FILE.
           MOVE OCI1-KEY TO E-KEYX.
           READ OCI-FILE.
           IF IO-FG = 8
              GO TO READ-OCI1-FILE.
       READ-OCI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCI-PATH TO E-FILE.
           MOVE OCI1-KEY TO E-KEYX.
           READ OCI-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCI1-FILE-NEXT.
       START-OCI1-FILE.
           PERFORM START-IT.
           MOVE OCI-PATH TO E-FILE.
           MOVE OCI1-KEY TO E-KEYX.
           START OCI-FILE KEY NOT < OCI1-KEY.
       START-OCI1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OCI-PATH TO E-FILE.
           MOVE OCI1-KEY TO E-KEYX.
           START OCI-FILE KEY > OCI1-KEY.
           UNLOCK OCI-FILE.
       CLOSE-OCI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCI-FILE.
