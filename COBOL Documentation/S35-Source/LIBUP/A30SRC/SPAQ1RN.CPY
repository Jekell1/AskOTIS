      * COPYMEMBER: LIBSP/SPOAQ1RN
       LOAD-OAQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAQ-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAQ-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO OAQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OAQ-ALL-DATA
              MOVE OAQ-ALL-PATH             TO OAQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAQ-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO OAQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO OAQ-ALL-DATA
              MOVE OAQ-ALL-PATH             TO OAQ-PATH.
       OPEN-OAQ1-FILE.
           PERFORM LOAD-OAQ1-FILE.
           PERFORM OPEN-IT.
           MOVE OAQ-PATH TO E-FILE.
           OPEN INPUT OAQ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAQ1-FILE.
           IF IO-FG = 7
              CLOSE OAQ-FILE
              GO TO OPEN-OAQ1-FILE.
       READ-OAQ1-FILE.
           PERFORM READ-IT.
           MOVE OAQ-PATH TO E-FILE.
           MOVE OAQ1-KEY TO E-KEYX.
           READ OAQ-FILE.
           IF IO-FG = 8
              GO TO READ-OAQ1-FILE.
       READ-OAQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAQ-PATH TO E-FILE.
           MOVE OAQ1-KEY TO E-KEYX.
           READ OAQ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAQ1-FILE-NEXT.
       START-OAQ1-FILE.
           PERFORM START-IT.
           MOVE OAQ-PATH TO E-FILE.
           MOVE OAQ1-KEY TO E-KEYX.
           START OAQ-FILE KEY NOT < OAQ1-KEY.
       CLOSE-OAQ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAQ-FILE.
