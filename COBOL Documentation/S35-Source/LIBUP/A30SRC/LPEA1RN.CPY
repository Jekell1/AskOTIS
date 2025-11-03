      * COPYMEMBER: LIBLP/LPOEA1RN
       LOAD-OEA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OEA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OEA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OEA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OEA-ALL-MACHINE
              MOVE OEA-ALL-PATH             TO OEA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OEA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OEA-ALL-BASE
              MOVE FILPATH-MACHINE         TO OEA-ALL-MACHINE
              MOVE OEA-ALL-PATH             TO OEA-PATH.
       OPEN-OEA1-FILE.
           PERFORM LOAD-OEA1-FILE.
           PERFORM OPEN-IT.
           MOVE OEA-PATH TO E-FILE.
           OPEN INPUT OEA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OEA1-FILE.
           IF IO-FG = 7
              CLOSE OEA-FILE
              GO TO OPEN-OEA1-FILE.
       READ-OEA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OEA-PATH TO E-FILE.
           MOVE OEA1-KEY TO E-KEYX.
           READ OEA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OEA1-FILE-NEXT.
       START-OEA1-FILE.
           PERFORM START-IT.
           MOVE OEA-PATH TO E-FILE.
           MOVE OEA1-KEY TO E-KEYX.
           START OEA-FILE KEY NOT < OEA1-KEY.
       CLOSE-OEA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OEA-FILE.
