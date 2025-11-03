      *-----------------------------------------------------------------
       CLOSE-LXG1-FILE.

           PERFORM CLOSE-IT.
           IF ( LXG1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LXG1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LXG1-CURSOR-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPLXG1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPWKI.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPWKI
