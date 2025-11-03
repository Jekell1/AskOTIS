      *================================================================*
      * END COPYBOOK: LIBGB\GBGBGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\GBBRRN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/GBBRRN
      * BAH 2022-0923 BAH ADDED BR_VERITEC_LICENSE #1570
      * BAH 2023-0419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      * BAH 2023-0713 BAH ADDED BR-USE-NEW-WFARGO-ACCT #1616
      * BAH 2023-1113 BAH ADDED BR-ALLOW-F3-ALL-DOCS #1630
      * JKC 2024-0124 ADDED OTIS TRACE LOGGING LOGIC
      *
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *-----------------------------------------------------------------
