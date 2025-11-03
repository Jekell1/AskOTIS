      * COPYMEMBER: LIBSP/SPOCP1RN
       LOAD-OCP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCP-ALL-DATA
              MOVE OCP-ALL-PATH             TO OCP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCP-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCP-ALL-DATA
              MOVE OCP-ALL-PATH             TO OCP-PATH.
       OPEN-OCP1-FILE.
           PERFORM LOAD-OCP1-FILE.
           PERFORM OPEN-IT.
           MOVE OCP-PATH TO E-FILE.
           OPEN INPUT OCP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCP1-FILE.
           IF IO-FG = 7
              CLOSE OCP-FILE
              GO TO OPEN-OCP1-FILE.
       READ-OCP1-FILE.
           PERFORM READ-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           READ OCP-FILE.
           IF IO-FG = 8
              GO TO READ-OCP1-FILE.
       READ-OCP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           READ OCP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCP1-FILE-NEXT.
       START-OCP1-FILE.
           PERFORM START-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           START OCP-FILE KEY NOT < OCP1-KEY.
       CLOSE-OCP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCP-FILE.
