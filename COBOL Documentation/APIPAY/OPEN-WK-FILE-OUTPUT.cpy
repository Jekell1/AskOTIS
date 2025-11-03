      *================================================================*
      * END COPYBOOK: LIBLP\LPLXG1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPWKI.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPWKI
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TEMP-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
           UNLOCK WK-FILE.
