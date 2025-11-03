      * COPYMEMBER: LIBLP/LPOLTI1RN
       LOAD-OLTI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTI-ALL-DATA
              MOVE OLTI-ALL-PATH             TO OLTI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTI-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTI-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTI-ALL-DATA
              MOVE OLTI-ALL-PATH             TO OLTI-PATH.
       OPEN-OLTI1-FILE.
           PERFORM LOAD-OLTI1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTI-PATH TO E-FILE.
           OPEN INPUT OLTI-FILE.
           IF IO-FG = 8
             GO TO OPEN-OLTI1-FILE.
           IF IO-FG = 7
             CLOSE OLTI-FILE
             GO TO OPEN-OLTI1-FILE.
       READ-OLTI1-FILE.
           PERFORM READ-IT.
           MOVE OLTI-PATH TO E-FILE.
           MOVE OLTI1-KEY TO E-KEYX.
           READ OLTI-FILE.
           IF IO-FG = 8
              GO TO READ-OLTI1-FILE.
       READ-OLTI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLTI-PATH TO E-FILE.
           MOVE OLTI1-KEY TO E-KEYX.
           READ OLTI-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLTI1-FILE-NEXT.
       START-OLTI1-FILE.
           PERFORM START-IT.
           MOVE OLTI-PATH TO E-FILE.
           MOVE OLTI1-KEY TO E-KEYX.
           START OLTI-FILE KEY NOT < OLTI1-KEY.
       CLOSE-OLTI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTI-FILE.
