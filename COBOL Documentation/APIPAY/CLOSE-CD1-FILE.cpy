      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           PERFORM CLOSE-IT.
           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-STAT.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-NG-STAT.

      *================================================================*
      * END COPYBOOK: LIBLP\LPCD1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLN1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLN1RN
      * REV:
      * JKC 2024-0404 ADDED LN-LOS-APP-ID
      ******************************************************************
