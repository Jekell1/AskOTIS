      *================================================================*
      * END COPYBOOK: LIBGB\CONNECT_SQL.CPY                                *
      *================================================================*
       OPEN-LOG-FILE-EXTEND.
           OPEN EXTEND LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-EXTEND.

