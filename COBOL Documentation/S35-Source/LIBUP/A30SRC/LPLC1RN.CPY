      * COPYMEMBER: LIBLP/LPOLC1RN
      * 121022 ADD LOAD-OLC1-FILE, FULL PATH NAMES A30 COMPLETE
       LOAD-OLC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLC-ALL-MACHINE
              MOVE OLC-ALL-PATH             TO OLC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLC-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLC-ALL-MACHINE
              MOVE OLC-ALL-PATH             TO OLC-PATH.
       OPEN-OLC1-FILE.
           PERFORM LOAD-OLC1-FILE.
           PERFORM OPEN-IT.
           MOVE OLC-PATH TO E-FILE.
           OPEN INPUT OLC-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLC1-FILE.
           IF IO-FG = 7
              CLOSE OLC-FILE
              GO TO OPEN-OLC1-FILE.
       READ-OLC1-FILE.
           PERFORM READ-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           READ OLC-FILE.
           IF IO-FG = 8
              GO TO READ-OLC1-FILE.
       READ-OLC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           READ OLC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLC1-FILE-NEXT.
       START-OLC1-FILE.
           PERFORM START-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           START OLC-FILE KEY NOT < OLC1-KEY.
       START-OLC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           START OLC-FILE KEY > OLC1-KEY.
           UNLOCK OLC-FILE.

       CLOSE-OLC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLC-FILE.
