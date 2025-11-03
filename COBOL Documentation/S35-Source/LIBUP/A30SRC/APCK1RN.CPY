      * COPYMEMBER: LIBAP/APOCKAP1RN
       LOAD-OCKAP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OCKAP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OCKAP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OCKAP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OCKAP-ALL-MACHINE
              MOVE OCKAP-ALL-PATH             TO OCKAP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OCKAP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OCKAP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OCKAP-ALL-MACHINE
              MOVE OCKAP-ALL-PATH             TO OCKAP-PATH.
       LIBAP-APOCKAP1RN.
           MOVE OCKAP-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OCKAP1-FILE.
           PERFORM LOAD-OCKAP1-FILE.
           PERFORM OPEN-IT.
           MOVE OCKAP-PATH TO E-FILE.
           OPEN INPUT OCKAP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCKAP1-FILE.
           IF IO-FG = 7
              CLOSE OCKAP-FILE
              GO TO OPEN-OCKAP1-FILE.
           UNLOCK OCKAP-FILE.
       READ-OCKAP1-FILE.
           PERFORM READ-IT.
           MOVE OCKAP-PATH TO E-FILE.
           MOVE OCKAP1-KEY TO E-KEYX.
           READ OCKAP-FILE KEY IS OCKAP1-KEY.
           IF IO-FG = 8
              GO TO READ-OCKAP1-FILE.
           UNLOCK OCKAP-FILE.
       READ-OCKAP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCKAP-PATH TO E-FILE.
           MOVE OCKAP1-KEY TO E-KEYX.
           READ OCKAP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCKAP1-FILE-NEXT.
           UNLOCK OCKAP-FILE.
       START-OCKAP1-FILE.
           PERFORM START-IT.
           MOVE OCKAP-PATH TO E-FILE.
           MOVE OCKAP1-KEY TO E-KEYX.
           START OCKAP-FILE KEY NOT < OCKAP1-KEY.
           UNLOCK OCKAP-FILE.
       CLOSE-OCKAP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCKAP-FILE.
