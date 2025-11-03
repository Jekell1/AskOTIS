      * COPYMEMBER: LIBLP/LPLP1RN
       LOAD-OLP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLP-ALL-DATA
              MOVE OLP-ALL-PATH             TO OLP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLP-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLP-ALL-DATA
              MOVE OLP-ALL-PATH             TO OLP-PATH.


       OPEN-OLP1-FILE.
           PERFORM LOAD-OLP1-FILE.
           PERFORM OPEN-IT.
           MOVE OLP-PATH TO E-FILE.
           OPEN INPUT OLP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLP1-FILE.
           IF IO-FG = 7
              CLOSE OLP-FILE
              GO TO OPEN-OLP1-FILE.
       READ-OLP1-FILE.
           PERFORM READ-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           READ OLP-FILE.
           IF IO-FG = 8
              GO TO READ-OLP1-FILE.
       READ-OLP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           READ OLP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLP1-FILE-NEXT.
       START-OLP1-FILE.
           PERFORM START-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           START OLP-FILE KEY NOT < OLP1-KEY.
       START-OLP1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           START OLP-FILE KEY NOT > OLP1-KEY.
       READ-OLP1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           READ OLP-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OLP1-FILE-PREVIOUS.
       CLOSE-OLP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLP-FILE.
