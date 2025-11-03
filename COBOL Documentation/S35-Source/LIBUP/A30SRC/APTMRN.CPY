      * COPYMEMBER: LIBAP/APOTMRN
       LOAD-OTM-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OTM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OTM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OTM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OTM-ALL-MACHINE
              MOVE OTM-ALL-PATH             TO OTM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OTM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OTM-ALL-BASE
              MOVE FILPATH-MACHINE         TO OTM-ALL-MACHINE
              MOVE OTM-ALL-PATH             TO OTM-PATH.
       LIBAP-APOTMRN.
      *    PERFORM FALL-THRU-IT.
           MOVE OTM-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OTM-FILE.
           PERFORM LOAD-OTM-FILE.
           PERFORM OPEN-IT.
           MOVE OTM-PATH TO E-FILE.
           OPEN INPUT OTM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTM-FILE.
           IF IO-FG = 7
              CLOSE OTM-FILE
              GO TO OPEN-OTM-FILE.
           UNLOCK OTM-FILE.
       READ-OTM-FILE.
           PERFORM READ-IT.
           MOVE OTM-PATH TO E-FILE.
           MOVE OTM-KEY TO E-KEYX.
           READ OTM-FILE.
           IF IO-FG = 8
              GO TO READ-OTM-FILE.
           UNLOCK OTM-FILE.
       READ-OTM-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTM-PATH TO E-FILE.
           MOVE OTM-KEY TO E-KEYX.
           READ OTM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTM-FILE-NEXT.
           UNLOCK OTM-FILE.
       START-OTM-FILE.
           PERFORM START-IT.
           MOVE OTM-PATH TO E-FILE.
           MOVE OTM-KEY TO E-KEYX.
           START OTM-FILE KEY NOT < OTM-KEY.
           UNLOCK OTM-FILE.
       CLOSE-OTM-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTM-FILE.
