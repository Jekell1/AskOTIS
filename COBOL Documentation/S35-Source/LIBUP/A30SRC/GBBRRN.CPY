      * COPYMEMBER: LIBGB/GBOBRRN
       LOAD-OBR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OBR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING OBRANCHES }- - - - - -
           IF ( OBR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OBR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OBR-ALL-MACHINE
              MOVE OBR-ALL-PATH            TO OBR-PATH
           ELSE
      *- - - -{ USE OBRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OBR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OBR-ALL-BASE
              MOVE FILPATH-MACHINE         TO OBR-ALL-MACHINE
              MOVE OBR-ALL-PATH            TO OBR-PATH.
       OPEN-OBR-FILE.
           PERFORM LOAD-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.
       READ-OBR-FILE.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE.
           IF IO-FG = 8
              GO TO READ-OBR-FILE.
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.
       START-OBR-FILE.
           PERFORM START-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           START OBR-FILE KEY NOT < OBR-KEY.
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.
