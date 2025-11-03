      * COPYMEMBER: LIBLP/LPOCD1RN
       LOAD-OCD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCD-ALL-MACHINE
              MOVE OCD-ALL-PATH             TO OCD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCD-ALL-MACHINE
              MOVE OCD-ALL-PATH             TO OCD-PATH.
       OPEN-OCD1-FILE.
           PERFORM LOAD-OCD1-FILE.
           PERFORM OPEN-IT.
           MOVE OCD-PATH TO E-FILE.
           OPEN INPUT OCD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCD1-FILE.
           IF IO-FG = 7
              CLOSE OCD-FILE
              GO TO OPEN-OCD1-FILE.
       READ-OCD1-FILE.
           PERFORM READ-IT.
           MOVE OCD-PATH TO E-FILE.
           MOVE OCD1-KEY TO E-KEYX.
           READ OCD-FILE.
           IF IO-FG = 8
              GO TO READ-OCD1-FILE.
       READ-OCD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCD-PATH TO E-FILE.
           MOVE OCD1-KEY TO E-KEYX.
           READ OCD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCD1-FILE-NEXT.
       START-OCD1-FILE.
           PERFORM START-IT.
           MOVE OCD-PATH TO E-FILE.
           MOVE OCD1-KEY TO E-KEYX.
           START OCD-FILE KEY NOT < OCD1-KEY.
       CLOSE-OCD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCD-FILE.
