      * COPYMEMBER: LIBSP/SPOEW1RN
       LOAD-OEW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OEW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OEW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OEW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OEW-ALL-MACHINE
              MOVE OEW-ALL-PATH             TO OEW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OEW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OEW-ALL-BASE
              MOVE FILPATH-MACHINE         TO OEW-ALL-MACHINE
              MOVE OEW-ALL-PATH             TO OEW-PATH.
       OPEN-OEW1-FILE.
*NOEW*      PERFORM LOAD-OEW1-FILE.
           PERFORM OPEN-IT.
           MOVE OEW-PATH TO E-FILE.
           OPEN INPUT OEW-FILE.
           IF IO-FG = 8
              GO TO OPEN-OEW1-FILE.
           IF IO-FG = 7
              CLOSE OEW-FILE
              GO TO OPEN-OEW1-FILE.
       READ-OEW1-FILE.
           PERFORM READ-IT.
           MOVE OEW-PATH TO E-FILE.
           MOVE OEW1-KEY TO E-KEYX.
           READ OEW-FILE.
           IF IO-FG = 8
              GO TO READ-OEW1-FILE.
       READ-OEW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OEW-PATH TO E-FILE.
           MOVE OEW1-KEY TO E-KEYX.
           READ OEW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OEW1-FILE-NEXT.
       START-OEW1-FILE.
           PERFORM START-IT.
           MOVE OEW-PATH TO E-FILE.
           MOVE OEW1-KEY TO E-KEYX.
           START OEW-FILE KEY NOT < OEW1-KEY.
       CLOSE-OEW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OEW-FILE.
