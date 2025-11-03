      * COPYMEMBER: LIBLP/LPOLH1RN
       LOAD-OLH1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLH-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLH-ALL-DATA
              MOVE OLH-ALL-PATH             TO OLH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLH-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLH-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLH-ALL-DATA
              MOVE OLH-ALL-PATH             TO OLH-PATH.
       OPEN-OLH1-FILE.
           PERFORM LOAD-OLH1-FILE.
           PERFORM OPEN-IT.
           MOVE OLH-PATH TO E-FILE.
           OPEN INPUT OLH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLH1-FILE.
           IF IO-FG = 7
              CLOSE OLH-FILE
              GO TO OPEN-OLH1-FILE.
       READ-OLH1-FILE.
           PERFORM READ-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           READ OLH-FILE.
           IF IO-FG = 8
              GO TO READ-OLH1-FILE.
       READ-OLH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           READ OLH-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLH1-FILE-NEXT.
       START-OLH1-FILE.
           PERFORM START-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           START OLH-FILE KEY NOT < OLH1-KEY.
       START-OLH1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           START OLH-FILE KEY NOT > OLH1-KEY.
       READ-OLH1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           READ OLH-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OLH1-FILE-PREVIOUS.
       CLOSE-OLH1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLH-FILE.
