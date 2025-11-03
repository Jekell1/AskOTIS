      *-----------------------------------------------------------------
       CLOSE-LXE1-FILE.

           PERFORM CLOSE-IT.
           IF ( LXE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LXE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LXE1-CURSOR-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPLXE1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLXG1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLXGE1RN
