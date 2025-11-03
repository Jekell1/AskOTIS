      *-----------------------------------------------------------------
       CLOSE-RC2-FILE.
           PERFORM CLOSE-IT.
           IF ( RC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RC2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RC2-CURSOR-STAT.

      *================================================================*
      * END COPYBOOK: LIBGB\GBRC2RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPOPIN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPOPIN
