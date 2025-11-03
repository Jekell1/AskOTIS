      * COPYMEMBER: LIBAP/APORI1RN
       LOAD-ORI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ORI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ORI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ORI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ORI-ALL-MACHINE
              MOVE ORI-ALL-PATH             TO ORI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ORI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ORI-ALL-BASE
              MOVE FILPATH-MACHINE         TO ORI-ALL-MACHINE
              MOVE ORI-ALL-PATH             TO ORI-PATH.
       LIBAP-APORI1RN.
           MOVE ORI-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-ORI1-FILE.
           PERFORM LOAD-ORI1-FILE.
           PERFORM OPEN-IT.
           MOVE ORI-PATH TO E-FILE.
           OPEN INPUT ORI-FILE.
           IF IO-FG = 8
              GO TO OPEN-ORI1-FILE.
           IF IO-FG = 7
              CLOSE ORI-FILE
              GO TO OPEN-ORI1-FILE.
           UNLOCK ORI-FILE.
       READ-ORI1-FILE.
           PERFORM READ-IT.
           MOVE ORI-PATH TO E-FILE.
           MOVE ORI1-KEY TO E-KEYX.
           READ ORI-FILE KEY IS ORI1-KEY.
           IF IO-FG = 8
              GO TO READ-ORI1-FILE.
           UNLOCK ORI-FILE.
       READ-ORI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ORI-PATH TO E-FILE.
           MOVE ORI1-KEY TO E-KEYX.
           READ ORI-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ORI1-FILE-NEXT.
           UNLOCK ORI-FILE.
       START-ORI1-FILE.
           PERFORM START-IT.
           MOVE ORI-PATH TO E-FILE.
           MOVE ORI1-KEY TO E-KEYX.
           START ORI-FILE KEY NOT < ORI1-KEY.
           UNLOCK ORI-FILE.
       CLOSE-ORI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ORI-FILE.
