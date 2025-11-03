      *================================================================*
      * END COPYBOOK: LIBLP\LPLTP1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLP1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLP1RN
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
