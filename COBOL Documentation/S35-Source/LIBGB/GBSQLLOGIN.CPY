      * COPYMEMBER: LIBGB/GBSQLLOGIN
       LOAD-SQLLOG-FILE.
           MOVE EXT-FILPATH                TO SQLLOG-PATH.
           MOVE "LG"                       TO SQLLOG-PATH-LG.
           MOVE "/"                        TO SQLLOG-PATH-SLASH.
           MOVE "SQLLOG."                  TO SQLLOG-PATH-FILENAME.
           ACCEPT SQLLOG-PATH-DATE FROM DATE.

      *-----------------------------------------------------------------
       OPEN-SQLLOG-FILE-EXTEND.
           OPEN EXTEND SQLLOG-FILE.

           IF ( IO-FG = 8 )
             GO TO OPEN-SQLLOG-FILE-EXTEND.

           IF ( IO-FG = 7 )
             CLOSE SQLLOG-FILE
             GO TO OPEN-SQLLOG-FILE-EXTEND.

      *-----------------------------------------------------------------
       OPEN-SQLLOG-FILE-OUTPUT.
           OPEN OUTPUT SQLLOG-FILE.

           IF ( IO-FG = 8 )
             GO TO OPEN-SQLLOG-FILE-OUTPUT.

           IF ( IO-FG = 7 )
             CLOSE SQLLOG-FILE
             GO TO OPEN-SQLLOG-FILE-OUTPUT.

      *-----------------------------------------------------------------
       WRITE-SQLLOG-FILE.
           WRITE SQLLOG-REC.

           IF ( IO-FG = 8 )
             GO TO WRITE-SQLLOG-FILE.

      *-----------------------------------------------------------------
       CLOSE-SQLLOG-FILE.
           CLOSE SQLLOG-FILE.

