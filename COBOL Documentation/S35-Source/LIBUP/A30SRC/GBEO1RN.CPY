      * COPYMEMBER: LIBGB/GBOEO1RN
       LOAD-OEO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OEO-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OEO-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OEO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OEO-ALL-MACHINE
              MOVE OEO-ALL-PATH             TO OEO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OEO-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OEO-ALL-BASE
              MOVE FILPATH-MACHINE         TO OEO-ALL-MACHINE
              MOVE OEO-ALL-PATH             TO OEO-PATH.
       OPEN-OEO1-FILE.
           PERFORM LOAD-OEO1-FILE.
           PERFORM OPEN-IT.
           MOVE OEO-PATH TO E-FILE.
           OPEN INPUT OEO-FILE.
           IF IO-FG = 8
              GO TO OPEN-OEO1-FILE.
           IF IO-FG = 7
              CLOSE OEO-FILE
              GO TO OPEN-OEO1-FILE.
       READ-OEO1-FILE.
           PERFORM READ-IT.
           MOVE OEO-PATH TO E-FILE.
           MOVE OEO1-KEY TO E-KEYX.
           READ OEO-FILE KEY IS OEO1-KEY.
           IF IO-FG = 8
              GO TO READ-OEO1-FILE.
       READ-OEO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OEO-PATH TO E-FILE.
           MOVE OEO1-KEY TO E-KEYX.
           READ OEO-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OEO1-FILE-NEXT.
       START-OEO1-FILE.
           PERFORM START-IT.
           MOVE OEO-PATH TO E-FILE.
           MOVE OEO1-KEY TO E-KEYX.
           START OEO-FILE KEY NOT < OEO1-KEY.
       START-OEO1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OEO-PATH TO E-FILE.
           MOVE OEO1-KEY TO E-KEYX.
           START OEO-FILE KEY > OEO1-KEY.
       CLOSE-OEO1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OEO-FILE.
