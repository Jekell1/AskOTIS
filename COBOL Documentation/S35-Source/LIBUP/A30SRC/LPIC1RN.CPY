      * COPYMEMBER: LIBLP/LPOIC1RN
       LOAD-OIC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OIC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OIC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OIC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OIC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OIC-ALL-DATA
              MOVE OIC-ALL-PATH             TO OIC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OIC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OIC-ALL-BASE
              MOVE FILPATH-MACHINE         TO OIC-ALL-MACHINE
              MOVE FILPATH-DATA            TO OIC-ALL-DATA
              MOVE OIC-ALL-PATH             TO OIC-PATH.
       OPEN-OIC1-FILE.
           PERFORM LOAD-OIC1-FILE.
           PERFORM OPEN-IT.
           MOVE OIC-PATH TO E-FILE.
           OPEN INPUT OIC-FILE.
           IF IO-FG = 8
              GO TO OPEN-OIC1-FILE.
           IF IO-FG = 7
              CLOSE OIC-FILE
              GO TO OPEN-OIC1-FILE.
       READ-OIC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OIC-PATH TO E-FILE.
           MOVE OIC1-KEY TO E-KEYX.
           READ OIC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIC1-FILE-NEXT.
       START-OIC1-FILE.
           PERFORM START-IT.
           MOVE OIC-PATH TO E-FILE.
           MOVE OIC1-KEY TO E-KEYX.
           START OIC-FILE KEY NOT < OIC1-KEY.
       CLOSE-OIC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIC-FILE.
