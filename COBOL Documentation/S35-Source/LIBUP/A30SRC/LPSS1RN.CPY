      * COPYMEMBER: LIBLP/LPOSS1RN
       LOAD-OSS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OSS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROOSSING BRANCHES }- - - - - -
           IF ( OSS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OSS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OSS-ALL-MACHINE
              MOVE OSS-ALL-PATH             TO OSS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OSS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OSS-ALL-BASE
              MOVE FILPATH-MACHINE         TO OSS-ALL-MACHINE
              MOVE OSS-ALL-PATH             TO OSS-PATH.
       OPEN-OSS1-FILE.
           PERFORM LOAD-OSS1-FILE.
           PERFORM OPEN-IT.
           MOVE OSS-PATH TO E-FILE.
           OPEN INPUT OSS-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSS1-FILE.
           IF IO-FG = 7
              CLOSE OSS-FILE
              GO TO OPEN-OSS1-FILE.
       READ-OSS1-FILE.
           PERFORM READ-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           READ OSS-FILE.
           IF IO-FG = 8
              GO TO READ-OSS1-FILE.
       READ-OSS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           READ OSS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSS1-FILE-NEXT.
       START-OSS1-FILE.
           PERFORM START-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           START OSS-FILE KEY NOT < OSS1-KEY.
       START-OSS1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           START OSS-FILE KEY > OSS1-KEY.
           UNLOCK OSS-FILE.
       CLOSE-OSS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSS-FILE.
