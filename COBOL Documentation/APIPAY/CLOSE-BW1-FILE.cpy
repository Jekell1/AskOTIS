      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPBW1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGL\GLGI1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGL/GLGI1RN
