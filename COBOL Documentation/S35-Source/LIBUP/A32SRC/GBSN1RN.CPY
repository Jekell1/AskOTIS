      * COPYMEMBER: LIBGB/GBOSN1RN
       LOAD-OSN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OSN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OSN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OSN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OSN-ALL-MACHINE
              MOVE OSN-ALL-PATH             TO OSN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OSN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OSN-ALL-BASE
              MOVE FILPATH-MACHINE         TO OSN-ALL-MACHINE
              MOVE OSN-ALL-PATH             TO OSN-PATH.
       OPEN-OSN1-FILE.
           PERFORM LOAD-OSN1-FILE.
           PERFORM OPEN-IT.
           MOVE OSN-PATH TO E-FILE.
           OPEN INPUT OSN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSN1-FILE.
           IF IO-FG = 7
              CLOSE OSN-FILE
              GO TO OPEN-OSN1-FILE.
       READ-OSN1-FILE.
           PERFORM READ-IT.
           MOVE OSN-PATH TO E-FILE.
           MOVE OSN1-KEY TO E-KEYX.
           READ OSN-FILE KEY IS OSN1-KEY.
           IF IO-FG = 8
              GO TO READ-OSN1-FILE.
       READ-OSN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSN-PATH TO E-FILE.
           MOVE OSN1-KEY TO E-KEYX.
           READ OSN-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSN1-FILE-NEXT.
       START-OSN1-FILE.
           PERFORM START-IT.
           MOVE OSN-PATH TO E-FILE.
           MOVE OSN1-KEY TO E-KEYX.
           START OSN-FILE KEY NOT < OSN1-KEY.
       START-OSN1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OSN-PATH TO E-FILE.
           MOVE OSN1-KEY TO E-KEYX.
           START OSN-FILE KEY > OSN1-KEY.
       CLOSE-OSN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSN-FILE.
