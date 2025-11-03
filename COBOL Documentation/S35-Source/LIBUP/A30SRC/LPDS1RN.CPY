      * COPYMEMBER: LIBLP/LPODS1RN
       LOAD-ODS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ODS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ODS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ODS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ODS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ODS-ALL-DATA
              MOVE ODS-ALL-PATH             TO ODS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ODS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ODS-ALL-BASE
              MOVE FILPATH-MACHINE         TO ODS-ALL-MACHINE
              MOVE FILPATH-DATA            TO ODS-ALL-DATA
              MOVE ODS-ALL-PATH             TO ODS-PATH.
       OPEN-ODS1-FILE.
           PERFORM LOAD-ODS1-FILE.
           PERFORM OPEN-IT.
           MOVE ODS-PATH TO E-FILE.
           OPEN INPUT ODS-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODS1-FILE.
           IF IO-FG = 7
              CLOSE ODS-FILE
              GO TO OPEN-ODS1-FILE.
       READ-ODS1-FILE.
           PERFORM READ-IT.
           MOVE ODS-PATH TO E-FILE.
           MOVE ODS1-KEY TO E-KEYX.
           READ ODS-FILE.
           IF IO-FG = 8
              GO TO READ-ODS1-FILE.
       READ-ODS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODS-PATH TO E-FILE.
           MOVE ODS1-KEY TO E-KEYX.
           READ ODS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODS1-FILE-NEXT.
       START-ODS1-FILE.
           PERFORM START-IT.
           MOVE ODS-PATH TO E-FILE.
           MOVE ODS1-KEY TO E-KEYX.
           START ODS-FILE KEY NOT < ODS1-KEY.
       CLOSE-ODS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODS-FILE.
