      * COPYMEMBER: LIBLP/LPOPD1RN
       LOAD-OPD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OPD-ALL-DATA
              MOVE OPD-ALL-PATH             TO OPD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OPD-ALL-MACHINE
              MOVE FILPATH-DATA            TO OPD-ALL-DATA
              MOVE OPD-ALL-PATH             TO OPD-PATH.
       OPEN-OPD1-FILE.
           PERFORM LOAD-OPD1-FILE.
           PERFORM OPEN-IT.
           MOVE OPD-PATH TO E-FILE.
           OPEN INPUT OPD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPD1-FILE.
           IF IO-FG = 7
              CLOSE OPD-FILE
              GO TO OPEN-OPD1-FILE.
       READ-OPD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OPD-PATH TO E-FILE.
           MOVE OPD1-KEY TO E-KEYX.
           READ OPD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPD1-FILE-NEXT.
       START-OPD1-FILE.
           PERFORM START-IT.
           MOVE OPD-PATH TO E-FILE.
           MOVE OPD1-KEY TO E-KEYX.
           START OPD-FILE KEY NOT < OPD1-KEY.
       CLOSE-OPD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPD-FILE.
