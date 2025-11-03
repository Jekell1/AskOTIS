      * COPYMEMBER: LIBLP/LPOTZ1RN
       LOAD-OTZ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OTZ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OTZ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OTZ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OTZ-ALL-MACHINE
              MOVE OTZ-ALL-PATH             TO OTZ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OTZ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OTZ-ALL-BASE
              MOVE FILPATH-MACHINE         TO OTZ-ALL-MACHINE
              MOVE OTZ-ALL-PATH             TO OTZ-PATH.
       OPEN-OTZ1-FILE.
           PERFORM LOAD-OTZ1-FILE.
           PERFORM OPEN-IT.
           MOVE OTZ-PATH TO E-FILE.
           OPEN INPUT OTZ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTZ1-FILE.
           IF IO-FG = 7
              CLOSE OTZ-FILE
              GO TO OPEN-OTZ1-FILE.
       READ-OTZ1-FILE.
           PERFORM READ-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           READ OTZ-FILE.
           IF IO-FG = 8
              GO TO READ-OTZ1-FILE.
       READ-OTZ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           READ OTZ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTZ1-FILE-NEXT.
       START-OTZ1-FILE.
           PERFORM START-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           START OTZ-FILE KEY NOT < OTZ1-KEY.
       START-OTZ1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           START OTZ-FILE KEY > OTZ1-KEY.
           UNLOCK OTZ-FILE.
       CLOSE-OTZ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTZ-FILE.
