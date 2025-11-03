      * COPYMEMBER: LIBLP/LPOTY1RN
      ***************************************************
      *                (LPOTY1RN)
      * REV:
      *  BLK 041194 BROUGH COPY MEMBER FROM A90 WHICH
      *             INCLUDES READ, READ-PREV, START NOT >
      ***************************************************
       LOAD-OTY1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OTY-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OTY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OTY-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OTY-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OTY-ALL-DATA
              MOVE OTY-ALL-PATH             TO OTY-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OTY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OTY-ALL-BASE
              MOVE FILPATH-MACHINE         TO OTY-ALL-MACHINE
              MOVE FILPATH-DATA            TO OTY-ALL-DATA
              MOVE OTY-ALL-PATH             TO OTY-PATH.
       OPEN-OTY1-FILE.
           PERFORM LOAD-OTY1-FILE.
           PERFORM OPEN-IT.
           MOVE OTY-PATH TO E-FILE.
           OPEN INPUT OTY-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTY1-FILE.
           IF IO-FG = 7
              CLOSE OTY-FILE
              GO TO OPEN-OTY1-FILE.
       READ-OTY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           READ OTY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTY1-FILE-NEXT.
       READ-OTY1-FILE.
           PERFORM READ-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           READ OTY-FILE.
           IF IO-FG = 8
              GO TO READ-OTY1-FILE.
       START-OTY1-FILE.
           PERFORM START-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           START OTY-FILE KEY NOT < OTY1-KEY.
       START-OTY1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           START OTY-FILE KEY NOT > OTY1-KEY.
       READ-OTY1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           READ OTY-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OTY1-FILE-PREVIOUS.
       CLOSE-OTY1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTY-FILE.
