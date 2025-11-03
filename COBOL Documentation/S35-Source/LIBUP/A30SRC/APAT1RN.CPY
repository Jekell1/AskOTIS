      * COPYMEMBER: LIBAP/APOAT1RN
       LOAD-OAT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH OAT ALL  }- -
           IF ( OAT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAT-ALL-MACHINE
              MOVE OAT-ALL-PATH             TO OAT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAT-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAT-ALL-MACHINE
              MOVE OAT-ALL-PATH             TO OAT-PATH.
       LIBAP-APOAT1RN.
           MOVE OAT-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OAT1-FILE.
           PERFORM LOAD-OAT1-FILE.
           PERFORM OPEN-IT.
           MOVE OAT-PATH TO E-FILE.
           OPEN INPUT OAT-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAT1-FILE.
           IF IO-FG = 7
              CLOSE OAT-FILE
              GO TO OPEN-OAT1-FILE.
           UNLOCK OAT-FILE.
       READ-OAT1-FILE.
           PERFORM READ-IT.
           MOVE OAT-PATH TO E-FILE.
           MOVE OAT1-KEY TO E-KEYX.
           READ OAT-FILE KEY IS OAT1-KEY.
           IF IO-FG = 8
              GO TO READ-OAT1-FILE.
           UNLOCK OAT-FILE.
       READ-OAT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAT-PATH TO E-FILE.
           MOVE OAT1-KEY TO E-KEYX.
           READ OAT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAT1-FILE-NEXT.
           UNLOCK OAT-FILE.
       START-OAT1-FILE.
           PERFORM START-IT.
           MOVE OAT-PATH TO E-FILE.
           MOVE OAT1-KEY TO E-KEYX.
           START OAT-FILE KEY NOT < OAT1-KEY.
           UNLOCK OAT-FILE.
       CLOSE-OAT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAT-FILE.
