      * COPYMEMBER: LIBLP/LPOCA1RN
       LOAD-OCA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OCA-ALL-DATA
              MOVE OCA-ALL-PATH             TO OCA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCA-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCA-ALL-MACHINE
              MOVE FILPATH-DATA            TO OCA-ALL-DATA
              MOVE OCA-ALL-PATH             TO OCA-PATH.
       OPEN-OCA1-FILE.
           PERFORM LOAD-OCA1-FILE.
           PERFORM OPEN-IT.
           MOVE OCA-PATH TO E-FILE.
           OPEN INPUT OCA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCA1-FILE.
           IF IO-FG = 7
              CLOSE OCA-FILE
              GO TO OPEN-OCA1-FILE.
       READ-OCA1-FILE.
           PERFORM READ-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           READ OCA-FILE.
           IF IO-FG = 8
             GO TO READ-OCA1-FILE.
       READ-OCA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           READ OCA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCA1-FILE-NEXT.
       START-OCA1-FILE.
           PERFORM START-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           START OCA-FILE KEY NOT < OCA1-KEY.
       CLOSE-OCA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCA-FILE.
