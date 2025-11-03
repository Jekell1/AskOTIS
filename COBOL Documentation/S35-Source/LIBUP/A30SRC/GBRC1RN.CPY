      * COPYMEMBER: LIBGB/GBORC1RN
      ***************************************************
      *                (GBORC1RN)
      * REV:
      ***************************************************
       LOAD-ORC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ORC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ORC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ORC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ORC-ALL-MACHINE
              MOVE ORC-ALL-PATH            TO ORC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ORC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ORC-ALL-BASE
              MOVE FILPATH-MACHINE         TO ORC-ALL-MACHINE
              MOVE ORC-ALL-PATH            TO ORC-PATH.
       OPEN-ORC1-FILE.
           PERFORM LOAD-ORC1-FILE.
           PERFORM OPEN-IT.
           MOVE ORC-PATH TO E-FILE.
           OPEN INPUT ORC-FILE.
           IF IO-FG = 8
              GO TO OPEN-ORC1-FILE.
           IF IO-FG = 7
              CLOSE ORC-FILE
              GO TO OPEN-ORC1-FILE.
       READ-ORC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           READ ORC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ORC1-FILE-NEXT.
       READ-ORC1-FILE.
           PERFORM READ-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           READ ORC-FILE.
           IF IO-FG = 8
              GO TO READ-ORC1-FILE.
       START-ORC1-FILE.
           PERFORM START-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           START ORC-FILE KEY NOT < ORC1-KEY.
       START-ORC1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           START ORC-FILE KEY NOT > ORC1-KEY.
       READ-ORC1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           READ ORC-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-ORC1-FILE-PREVIOUS.
       CLOSE-ORC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ORC-FILE.
