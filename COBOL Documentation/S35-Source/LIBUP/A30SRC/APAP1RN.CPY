      * COPYMEMBER: LIBAP/APOAP1RN
       LOAD-OAP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OAP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OAP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OAP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OAP-ALL-MACHINE
              MOVE OAP-ALL-PATH             TO OAP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OAP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OAP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OAP-ALL-MACHINE
              MOVE OAP-ALL-PATH             TO OAP-PATH.
       LIBAP-APOAP1RN.
      *    PERFORM FALL-THRU-IT.
           MOVE OAP-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OAP1-FILE.
           PERFORM LOAD-OAP1-FILE.
           PERFORM OPEN-IT.
           MOVE OAP-PATH TO E-FILE.
           OPEN INPUT OAP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAP1-FILE.
           IF IO-FG = 7
              CLOSE OAP-FILE
              GO TO OPEN-OAP1-FILE.
           UNLOCK OAP-FILE.
       READ-OAP1-FILE.
           PERFORM READ-IT.
           MOVE OAP-PATH TO E-FILE.
           MOVE OAP1-KEY TO E-KEYX.
           READ OAP-FILE KEY IS OAP1-KEY.
           IF IO-FG = 8
              GO TO READ-OAP1-FILE.
           UNLOCK OAP-FILE.
       READ-OAP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAP-PATH TO E-FILE.
           MOVE OAP1-KEY TO E-KEYX.
           READ OAP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAP1-FILE-NEXT.
           UNLOCK OAP-FILE.
       START-OAP1-FILE.
           PERFORM START-IT.
           MOVE OAP-PATH TO E-FILE.
           MOVE OAP1-KEY TO E-KEYX.
           START OAP-FILE KEY NOT < OAP1-KEY.
           UNLOCK OAP-FILE.
       CLOSE-OAP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAP-FILE.
