      * COPYMEMBER: LIBLP/LPBP1IN
       LOAD-BP-WK-GB-PATH.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BP-WK-GB-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BP-WK-GB-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO BP-WK-GB-BASE
              MOVE EXT-FILPATH-MACHINE     TO BP-WK-GB-MACHINE
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BP-WK-GB-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO BP-WK-GB-BASE
              MOVE FILPATH-MACHINE         TO BP-WK-GB-MACHINE.

      *-----------------------------------------------------------------
       LOAD-BP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BP-ALL-DATA
              MOVE BP-ALL-PATH             TO BP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BP-ALL-BASE
              MOVE FILPATH-MACHINE         TO BP-ALL-MACHINE
              MOVE FILPATH-DATA            TO BP-ALL-DATA
              MOVE BP-ALL-PATH             TO BP-PATH.

      *-----------------------------------------------------------------
       OPEN-BP1-FILE.
           PERFORM LOAD-BP1-FILE.
           PERFORM OPEN-IT.
           MOVE BP1-IFN TO E-FILE.
           OPEN I-O BP-FILE.
           IF IO-FG = 8
              GO TO OPEN-BP1-FILE.
           IF IO-FG = 7
              CLOSE BP-FILE
              GO TO OPEN-BP1-FILE.
           UNLOCK BP-FILE.
       READ-BP1-FILE.
           PERFORM READ-IT.
           MOVE BP1-IFN TO E-FILE.
           MOVE BP1-KEY TO E-KEYX.
           READ BP-FILE.
           IF IO-FG = 8
              GO TO READ-BP1-FILE.
       READ-BP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BP1-IFN TO E-FILE.
           MOVE BP1-KEY TO E-KEYX.
           READ BP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-BP1-FILE-NEXT.
       START-BP1-FILE.
           PERFORM START-IT.
           MOVE BP1-IFN TO E-FILE.
           MOVE BP1-KEY TO E-KEYX.
           START BP-FILE KEY NOT < BP1-KEY.
           UNLOCK BP-FILE.
       WRITE-BP1-FILE.
           PERFORM WRITE-IT.
           MOVE BP1-IFN TO E-FILE.
           MOVE BP1-KEY TO E-KEYX.
           WRITE BP-REC.
           IF IO-FG = 8
              GO TO WRITE-BP1-FILE.
           UNLOCK BP-FILE.
       REWRITE-BP1-FILE.
           PERFORM REWRITE-IT.
           MOVE BP1-IFN TO E-FILE.
           MOVE BP1-KEY TO E-KEYX.
           REWRITE BP-REC.
           IF IO-FG = 8
              GO TO REWRITE-BP1-FILE.
           UNLOCK BP-FILE.
       DELETE-BP1-FILE.
           PERFORM DELETE-IT.
           MOVE BP1-IFN TO E-FILE.
           MOVE BP1-KEY TO E-KEYX.
           DELETE BP-FILE.
           IF IO-FG = 8
              GO TO DELETE-BP1-FILE.
           UNLOCK BP-FILE.
       OPEN-BP1-FILE-OUTPUT.
           PERFORM LOAD-BP1-FILE.
           PERFORM OPEN-IT.
           MOVE BP1-IFN TO E-FILE.
           OPEN OUTPUT BP-FILE.
           IF IO-FG = 8
              GO TO OPEN-BP1-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE BP-FILE
              GO TO OPEN-BP1-FILE-OUTPUT.
           UNLOCK BP-FILE.
       CLOSE-BP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE BP-FILE.
