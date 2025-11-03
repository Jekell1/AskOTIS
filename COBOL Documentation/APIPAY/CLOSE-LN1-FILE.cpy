      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-GR-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPLN1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLTI1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLTI1RN
