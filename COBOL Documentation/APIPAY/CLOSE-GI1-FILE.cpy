      *-----------------------------------------------------------------
       CLOSE-GI1-FILE.
           PERFORM CLOSE-IT.
           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GI1-CURSOR-STAT.

      *================================================================*
      * END COPYBOOK: LIBGL\GLGI1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPCD1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPCD1RN
