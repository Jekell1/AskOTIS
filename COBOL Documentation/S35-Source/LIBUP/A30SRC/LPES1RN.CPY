      * COPYMEMBER: LIBLP/LPOES1RN
      *=================================================================
      *    IO-ROUTINOES FOR OES-FILE
      *=================================================================
       LOAD-OES1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OES-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHOES }- - - - - -
           IF ( OES-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OES-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OES-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO OES-ALL-DATA
              MOVE OES-ALL-PATH             TO OES-PATH
           ELSE
      *- - - -{ USE BRANCH VALUOES }- - - - - - - - - - - - - - - - - - -
           IF ( OES-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OES-ALL-BASE
              MOVE FILPATH-MACHINE         TO OES-ALL-MACHINE
              MOVE FILPATH-DATA            TO OES-ALL-DATA
              MOVE OES-ALL-PATH             TO OES-PATH.
       OPEN-OES1-FILE.
           PERFORM LOAD-OES1-FILE.
           PERFORM OPEN-IT.
           MOVE OES-PATH TO E-FILE.
           OPEN INPUT OES-FILE.
           IF ( IO-FG = 8 )
             GO TO OPEN-OES1-FILE.
           IF ( IO-FG = 7 )
             CLOSE OES-FILE
             GO TO OPEN-OES1-FILE.
           UNLOCK OES-FILE.
      *-----------------------------------------------------------------
       READ-OES1-FILE.
           PERFORM READ-IT.
           MOVE OES-PATH TO E-FILE.
           PERFORM SET-OES1-E-KEYX.
           READ OES-FILE.
           IF ( IO-FG = 8 )
              GO TO READ-OES1-FILE.
      *-----------------------------------------------------------------
       READ-OES1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OES-PATH TO E-FILE.
           PERFORM SET-OES1-E-KEYX.
           READ OES-FILE NEXT.
           IF ( IO-FG = 8 )
             GO TO READ-OES1-FILE-NEXT.
      *-----------------------------------------------------------------
       START-OES1-FILE.
           PERFORM START-IT.
           MOVE OES-PATH TO E-FILE.
           PERFORM SET-OES1-E-KEYX.
           START OES-FILE KEY NOT < OES1-KEY.
           UNLOCK OES-FILE.
      *-----------------------------------------------------------------
       CLOSE-OES1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OES-FILE.
      *-----------------------------------------------------------------
       SET-OES1-E-KEYX.
           MOVE SPACES    TO E-KEYX.
           MOVE OES-ACCTNO TO E-KEYX-ACCTNO.
