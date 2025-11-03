      * COPYMEMBER: LIBLP/LPOMP1IN
       LOAD-OMP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OMP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OMP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OMP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OMP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OMP-ALL-DATA
              MOVE OMP-ALL-PATH             TO OMP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OMP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OMP-ALL-BASE
              MOVE FILPATH-MACHINE         TO OMP-ALL-MACHINE
              MOVE FILPATH-DATA            TO OMP-ALL-DATA
              MOVE OMP-ALL-PATH             TO OMP-PATH.
       OPEN-OMP1-FILE.
           PERFORM LOAD-OMP1-FILE.
           PERFORM OPEN-IT.
           MOVE OMP-PATH TO E-FILE.
      *KC  OPEN I-O OMP-FILE.
           OPEN INPUT OMP-FILE.     *> A32
           IF IO-FG = 8
             GO TO OPEN-OMP1-FILE.
           IF IO-FG = 7
             CLOSE OMP-FILE
             GO TO OPEN-OMP1-FILE.
           UNLOCK OMP-FILE.
       READ-OMP1-FILE.
           PERFORM READ-IT.
           MOVE OMP-PATH TO E-FILE.
           MOVE OMP1-KEY TO E-KEYX.
           READ OMP-FILE.
           IF IO-FG = 8
              GO TO READ-OMP1-FILE.
       READ-OMP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OMP-PATH TO E-FILE.
           MOVE OMP1-KEY TO E-KEYX.
           READ OMP-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OMP1-FILE-NEXT.
       START-OMP1-FILE.
           PERFORM START-IT.
           MOVE OMP-PATH TO E-FILE.
           MOVE OMP1-KEY TO E-KEYX.
           START OMP-FILE KEY NOT < OMP1-KEY.
           UNLOCK OMP-FILE.
       START-OMP1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE OMP-PATH TO E-FILE.
           MOVE OMP1-KEY TO E-KEYX.
           START OMP-FILE KEY NOT > OMP1-KEY.
       READ-OMP1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE OMP-PATH TO E-FILE.
           MOVE OMP1-KEY TO E-KEYX.
           READ OMP-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-OMP1-FILE-PREVIOUS.
      *WRITE-OMP1-FILE.     *> A32
      *    PERFORM WRITE-IT.
      *    MOVE OMP-PATH TO E-FILE.
      *    MOVE OMP1-KEY TO E-KEYX.
      *    WRITE OMP-REC.
      *    IF IO-FG = 8
      *      GO TO WRITE-OMP1-FILE.
      *    UNLOCK OMP-FILE.
      *REWRITE-OMP1-FILE.     *> A32
      *    PERFORM REWRITE-IT.
      *    MOVE OMP-PATH TO E-FILE.
      *    MOVE OMP1-KEY TO E-KEYX.
      *    REWRITE OMP-REC.
      *    IF IO-FG = 8
      *      GO TO REWRITE-OMP1-FILE.
      *    UNLOCK OMP-FILE.
      *DELETE-OMP1-FILE.     *> A32
      *    PERFORM DELETE-IT.
      *    MOVE OMP-PATH TO E-FILE.
      *    MOVE OMP1-KEY TO E-KEYX.
      *    DELETE OMP-FILE.
      *    IF IO-FG = 8
      *      GO TO DELETE-OMP1-FILE.
      *    UNLOCK OMP-FILE.
       CLOSE-OMP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OMP-FILE.
