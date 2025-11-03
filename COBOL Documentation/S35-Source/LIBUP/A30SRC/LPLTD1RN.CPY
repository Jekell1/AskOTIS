      * COPYMEMBER: LIBLP/LPOLTD1R
       LOAD-OLTD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTD-ALL-DATA
              MOVE OLTD-ALL-PATH             TO OLTD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTD-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTD-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTD-ALL-DATA
              MOVE OLTD-ALL-PATH             TO OLTD-PATH.

       OPEN-OLTD1-FILE.
           PERFORM LOAD-OLTD1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTD-PATH TO E-FILE.
           OPEN INPUT OLTD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLTD1-FILE.
           IF IO-FG = 7
              CLOSE OLTD-FILE
              GO TO OPEN-OLTD1-FILE.
       READ-OLTD1-FILE.
           PERFORM READ-IT.
           MOVE OLTD-PATH TO E-FILE.
           MOVE OLTD1-KEY TO E-KEYX.
           READ OLTD-FILE.
           IF IO-FG = 8
              GO TO READ-OLTD1-FILE.
       START-OLTD1-FILE.
           PERFORM START-IT.
           MOVE OLTD-PATH TO E-FILE.
           MOVE OLTD1-KEY TO E-KEYX.
           START OLTD-FILE KEY NOT < OLTD1-KEY.

       READ-OLTD1-FILE-NEXT.          *> ONLY IN A32
           PERFORM READ-IT.
           MOVE OLTD-PATH TO E-FILE.
           MOVE OLTD1-KEY TO E-KEYX.
           READ OLTD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLTD1-FILE-NEXT.
       CLOSE-OLTD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTD-FILE.
