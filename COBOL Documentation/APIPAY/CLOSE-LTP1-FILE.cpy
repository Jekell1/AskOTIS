      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTP1-CURSOR-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPLTP1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLP1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLP1RN
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
