      * COPYMEMBER: LIBSP/SPOEP1RN
       LOAD-OEP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OEP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OEP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OEP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OEP-ALL-MACHINE
              MOVE OEP-ALL-PATH             TO OEP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OEP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OEP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OEP-ALL-MACHINE
              MOVE OEP-ALL-PATH             TO OEP-PATH.
       OPEN-OEP1-FILE.
*NEW*      PERFORM LOAD-OEP1-FILE.
           PERFORM OPEN-IT.
           MOVE OEP-PATH TO E-FILE.
           OPEN INPUT OEP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OEP1-FILE.
           IF IO-FG = 7
              CLOSE OEP-FILE
              GO TO OPEN-OEP1-FILE.
       READ-OEP1-FILE.
           PERFORM READ-IT.
           MOVE OEP-PATH TO E-FILE.
           MOVE OEP1-KEY TO E-KEYX.
           READ OEP-FILE.
           IF IO-FG = 8
              GO TO READ-OEP1-FILE.
       READ-OEP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OEP-PATH TO E-FILE.
           MOVE OEP1-KEY TO E-KEYX.
           READ OEP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OEP1-FILE-NEXT.
       START-OEP1-FILE.
           PERFORM START-IT.
           MOVE OEP-PATH TO E-FILE.
           MOVE OEP1-KEY TO E-KEYX.
           START OEP-FILE KEY NOT < OEP1-KEY.
       CLOSE-OEP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OEP-FILE.
