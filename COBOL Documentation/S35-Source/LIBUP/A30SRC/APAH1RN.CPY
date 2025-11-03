      * COPYMEMBER: LIBAP/APOAH1RN
       LOAD-OAH1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAH-ALL-MACHINE
              MOVE OAH-ALL-PATH             TO OAH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAH-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAH-ALL-MACHINE
              MOVE OAH-ALL-PATH             TO OAH-PATH.
       LIBAP-APOAH1RN.
           MOVE OAH-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OAH1-FILE.
           PERFORM LOAD-OAH1-FILE.
           PERFORM OPEN-IT.
           MOVE OAH-PATH TO E-FILE.
           OPEN INPUT OAH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAH1-FILE.
           IF IO-FG = 7
              CLOSE OAH-FILE
              GO TO OPEN-OAH1-FILE.
           UNLOCK OAH-FILE.
       READ-OAH1-FILE.
           PERFORM READ-IT.
           MOVE OAH-PATH TO E-FILE.
           MOVE OAH1-KEY TO E-KEYX.
           READ OAH-FILE KEY IS OAH1-KEY.
           IF IO-FG = 8
              GO TO READ-OAH1-FILE.
           UNLOCK OAH-FILE.
       READ-OAH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAH-PATH TO E-FILE.
           MOVE OAH1-KEY TO E-KEYX.
           READ OAH-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAH1-FILE-NEXT.
           UNLOCK OAH-FILE.
       START-OAH1-FILE.
           PERFORM START-IT.
           MOVE OAH-PATH TO E-FILE.
           MOVE OAH1-KEY TO E-KEYX.
           START OAH-FILE KEY NOT < OAH1-KEY.
           UNLOCK OAH-FILE.
       CLOSE-OAH1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAH-FILE.
