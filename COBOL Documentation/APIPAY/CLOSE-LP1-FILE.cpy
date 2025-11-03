      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.

           IF ( LP1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-SC-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPLP1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLXE1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLXE1RN
