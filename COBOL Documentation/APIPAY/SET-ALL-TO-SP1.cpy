      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( UPDATE-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( UPDATE-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO UPDATE-PATH-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO UPDATE-PATH-MACHINE
      *       MOVE UPDATE-PATH-FORMAT      TO UPDATE-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( UPDATE-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO UPDATE-PATH-BASE
      *       MOVE FILPATH-MACHINE         TO UPDATE-PATH-MACHINE
      *       MOVE UPDATE-PATH-FORMAT      TO UPDATE-PATH.
      *================================================================*
      * END COPYBOOK: LIBLP\LPEDPATHIO.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPSP1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPSP1RN
      ******************************************************************
       SET-ALL-TO-SP1.
           MOVE SPA1-KEY   TO SP1-KEY.
           MOVE SPA-AREA-A TO SP-AREA-A.
           MOVE SPB-AREA-B TO SP-AREA-B.
           MOVE SPC-AREA-C TO SP-AREA-C.

