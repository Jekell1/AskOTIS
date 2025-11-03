      * COPYMEMBER: LIBAP/APOAK1RN
       LOAD-OAK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAK-ALL-MACHINE
              MOVE OAK-ALL-PATH             TO OAK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAK-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAK-ALL-MACHINE
              MOVE OAK-ALL-PATH             TO OAK-PATH.
       LIBAP-APOAK1RN.
           MOVE OAK-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OAK1-FILE.
           PERFORM LOAD-OAK1-FILE.
           PERFORM OPEN-IT.
           MOVE OAK-PATH TO E-FILE.
           OPEN INPUT OAK-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAK1-FILE.
           IF IO-FG = 7
              CLOSE OAK-FILE
              GO TO OPEN-OAK1-FILE.
           UNLOCK OAK-FILE.
       READ-OAK1-FILE.
           PERFORM READ-IT.
           MOVE OAK-PATH TO E-FILE.
           MOVE OAK1-KEY TO E-KEYX.
           READ OAK-FILE KEY IS OAK1-KEY.
           IF IO-FG = 8
              GO TO READ-OAK1-FILE.
           UNLOCK OAK-FILE.
       READ-OAK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAK-PATH TO E-FILE.
           MOVE OAK1-KEY TO E-KEYX.
           READ OAK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAK1-FILE-NEXT.
           UNLOCK OAK-FILE.
       START-OAK1-FILE.
           PERFORM START-IT.
           MOVE OAK-PATH TO E-FILE.
           MOVE OAK1-KEY TO E-KEYX.
           START OAK-FILE KEY NOT < OAK1-KEY.
           UNLOCK OAK-FILE.
       CLOSE-OAK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAK-FILE.
