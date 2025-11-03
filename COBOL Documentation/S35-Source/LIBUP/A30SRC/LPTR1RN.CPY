      * COPYMEMBER: LIBLP/LPOTR1RN
       LOAD-OTR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *KC  IF ( OTRANS-PATH-OVERRIDE = "Y" )
           IF ( OTR-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *KC  IF ( OTRANS-PATH-OVERRIDE = SPACES )
           IF ( OTR-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO OTR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OTR-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OTR-ALL-DATA
      *KC     MOVE OTR-ALL-PATH            TO OTRANS-PATH
              MOVE OTR-ALL-PATH            TO OTR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *KC  IF ( OTRANS-PATH-OVERRIDE = "B" )
           IF ( OTR-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO OTR-ALL-BASE
              MOVE FILPATH-MACHINE         TO OTR-ALL-MACHINE
              MOVE FILPATH-DATA            TO OTR-ALL-DATA
      *KC     MOVE OTR-ALL-PATH            TO OTRANS-PATH.
              MOVE OTR-ALL-PATH            TO OTR-PATH.
       OPEN-OTR1-FILE.
           PERFORM LOAD-OTR1-FILE.
           PERFORM OPEN-IT.
      *KC  MOVE OTRANS-PATH TO E-FILE.
           MOVE OTR-PATH TO E-FILE.
           OPEN INPUT OTR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTR1-FILE.
           IF IO-FG = 7
              CLOSE OTR-FILE
              GO TO OPEN-OTR1-FILE.
       READ-OTR1-FILE-NEXT.
           PERFORM READ-IT.
      *KC  MOVE OTRANS-PATH TO E-FILE.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTR1-FILE-NEXT.
       READ-OTR1-FILE.
           PERFORM READ-IT.
      *KC  MOVE OTRANS-PATH TO E-FILE.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE.
           IF IO-FG = 8
              GO TO READ-OTR1-FILE.
       START-OTR1-FILE.
           PERFORM START-IT.
      *KC  MOVE OTRANS-PATH TO E-FILE.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           START OTR-FILE KEY NOT < OTR1-KEY.
       CLOSE-OTR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTR-FILE.
