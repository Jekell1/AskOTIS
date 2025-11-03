      * COPYMEMBER: LIBAP/APOAD1RN
       LOAD-OAD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAD-ALL-MACHINE
              MOVE OAD-ALL-PATH             TO OAD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAD-ALL-MACHINE
              MOVE OAD-ALL-PATH             TO OAD-PATH.
       LIBAP-APOAD1RN.
           MOVE OAD-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OAD1-FILE.
           PERFORM LOAD-OAD1-FILE.
           PERFORM OPEN-IT.
           MOVE OAD-PATH TO E-FILE.
           OPEN INPUT OAD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAD1-FILE.
           IF IO-FG = 7
              CLOSE OAD-FILE
              GO TO OPEN-OAD1-FILE.
           UNLOCK OAD-FILE.
       READ-OAD1-FILE.
           PERFORM READ-IT.
           MOVE OAD-PATH TO E-FILE.
           MOVE OAD1-KEY TO E-KEYX.
           READ OAD-FILE KEY IS OAD1-KEY.
           IF IO-FG = 8
              GO TO READ-OAD1-FILE.
           UNLOCK OAD-FILE.
       READ-OAD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAD-PATH TO E-FILE.
           MOVE OAD1-KEY TO E-KEYX.
           READ OAD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAD1-FILE-NEXT.
           UNLOCK OAD-FILE.
       START-OAD1-FILE.
           PERFORM START-IT.
           MOVE OAD-PATH TO E-FILE.
           MOVE OAD1-KEY TO E-KEYX.
           START OAD-FILE KEY NOT < OAD1-KEY.
           UNLOCK OAD-FILE.
       CLOSE-OAD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAD-FILE.
