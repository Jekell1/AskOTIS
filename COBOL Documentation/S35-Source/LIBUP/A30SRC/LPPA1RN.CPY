      * COPYMEMBER: LIBLP/LPOPA1RN
       LOAD-OPA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OPA-ALL-DATA
              MOVE OPA-ALL-PATH             TO OPA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPA-ALL-BASE
              MOVE FILPATH-MACHINE         TO OPA-ALL-MACHINE
              MOVE FILPATH-DATA            TO OPA-ALL-DATA
              MOVE OPA-ALL-PATH             TO OPA-PATH.
       LIBLP-LPOPA1RN.
           MOVE OPA-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OPA1-FILE.
           PERFORM LOAD-OPA1-FILE.
           PERFORM OPEN-IT.
           MOVE OPA-PATH TO E-FILE.
           OPEN INPUT OPA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPA1-FILE.
           IF IO-FG = 7
              CLOSE OPA-FILE
              GO TO OPEN-OPA1-FILE.
           UNLOCK OPA-FILE.
       READ-OPA1-FILE.
           PERFORM READ-IT.
           MOVE OPA-PATH TO E-FILE.
           MOVE OPA1-KEY TO E-KEYX.
           READ OPA-FILE KEY IS OPA1-KEY.
           IF IO-FG = 8
              GO TO READ-OPA1-FILE.
           UNLOCK OPA-FILE.
       READ-OPA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OPA-PATH TO E-FILE.
           MOVE OPA1-KEY TO E-KEYX.
           READ OPA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPA1-FILE-NEXT.
           UNLOCK OPA-FILE.
       START-OPA1-FILE.
           PERFORM START-IT.
           MOVE OPA-PATH TO E-FILE.
           MOVE OPA1-KEY TO E-KEYX.
           START OPA-FILE KEY NOT < OPA1-KEY.
           UNLOCK OPA-FILE.
       CLOSE-OPA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPA-FILE.
