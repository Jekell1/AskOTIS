      * COPYMEMBER: LIBLP/LPOLX1R
       LOAD-OLX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLX-ALL-DATA
              MOVE OLX-ALL-PATH             TO OLX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLX-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLX-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLX-ALL-DATA
              MOVE OLX-ALL-PATH             TO OLX-PATH.
       OPEN-OLX1-FILE.
           PERFORM LOAD-OLX1-FILE.
           PERFORM OPEN-IT.
           MOVE OLX-PATH TO E-FILE.
           OPEN INPUT OLX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLX1-FILE.
           IF IO-FG = 7
              CLOSE OLX-FILE
              GO TO OPEN-OLX1-FILE.
       READ-OLX1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           READ OLX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLX1-FILE-NEXT.
       START-OLX1-FILE.
           PERFORM START-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           START OLX-FILE KEY NOT < OLX1-KEY.
       CLOSE-OLX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLX-FILE.
