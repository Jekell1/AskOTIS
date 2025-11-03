      * COPYMEMBER: LIBGB/GBOSE1RN
      * MJD 20170522 CHANGED "OPEN INPOSE OSE-FILE" TO "OPEN INPUT OSE-FILE"
       LOAD-OSE1-FILE.
      *- - - -{ OSET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OSE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OSE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OSE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OSE-ALL-MACHINE
              MOVE OSE-ALL-PATH             TO OSE-PATH
           ELSE
      *- - - -{ UOSE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OSE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OSE-ALL-BASE
              MOVE FILPATH-MACHINE         TO OSE-ALL-MACHINE
              MOVE OSE-ALL-PATH             TO OSE-PATH.
       OPEN-OSE1-FILE.
           PERFORM LOAD-OSE1-FILE.
           PERFORM OPEN-IT.
           MOVE OSE-PATH TO E-FILE.
           OPEN INPUT OSE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSE1-FILE.
           IF IO-FG = 7
              CLOSE OSE-FILE
              GO TO OPEN-OSE1-FILE.
       READ-OSE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSE-PATH TO E-FILE.
           MOVE OSE1-KEY TO E-KEYX.
           READ OSE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSE1-FILE-NEXT.
       START-OSE1-FILE.
           PERFORM START-IT.
           MOVE OSE-PATH TO E-FILE.
           MOVE OSE1-KEY TO E-KEYX.
           START OSE-FILE KEY NOT < OSE1-KEY.
       CLOSE-OSE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSE-FILE.
