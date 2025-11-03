      * COPYMEMBER: LIBLP/LPOLTC1RN
       LOAD-OLTC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OLTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OLTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OLTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OLTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OLTC-ALL-DATA
              MOVE OLTC-ALL-PATH            TO OLTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OLTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OLTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO OLTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO OLTC-ALL-DATA
              MOVE OLTC-ALL-PATH            TO OLTC-PATH.
       OPEN-OLTC1-FILE.
           PERFORM LOAD-OLTC1-FILE.
           PERFORM OPEN-IT.
           MOVE OLTC-PATH TO E-FILE.
           OPEN INPUT OLTC-FILE.
           IF IO-FG = 8
             GO TO OPEN-OLTC1-FILE.
           IF IO-FG = 7
             CLOSE OLTC-FILE
             GO TO OPEN-OLTC1-FILE.
       READ-OLTC1-FILE.
           PERFORM READ-IT.
           MOVE OLTC-PATH TO E-FILE.
           MOVE OLTC1-KEY TO E-KEYX.
           READ OLTC-FILE.
           IF IO-FG = 8
              GO TO READ-OLTC1-FILE.
       READ-OLTC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLTC-PATH TO E-FILE.
           MOVE OLTC1-KEY TO E-KEYX.
           READ OLTC-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLTC1-FILE-NEXT.
       START-OLTC1-FILE.
           PERFORM START-IT.
           MOVE OLTC-PATH TO E-FILE.
           MOVE OLTC1-KEY TO E-KEYX.
           START OLTC-FILE KEY NOT < OLTC1-KEY.
       CLOSE-OLTC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLTC-FILE.
