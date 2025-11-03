      ******************************************************************
      *
      *    S Q L - T E X T - F I N D - A S C I I
      *
      *=================================================================
      * IN  : SQL-TEXT-CURRENT
      * OUT : SQL-TEXT-AFTER
      *       SQL-TEXT-BEFORE
      * DESC: FIND THE CURRENT CHARACTER IN SQL-TEXT-ASCII THEN RETURN
      *       BACK THE CHARACTER BEFORE & AFTER THE CURRENT CHARACTER.
      ******************************************************************
       SQL-TEXT-FIND-ASCII.

           MOVE ZEROES TO SQL-TEXT-S4.

           PERFORM VARYING SQL-TEXT-S3 FROM 1 BY 1
                     UNTIL SQL-TEXT-S3 > SQL-TEXT-ASCII-MAX

                  IF ( SQL-TEXT-ASCIIX(SQL-TEXT-S3) = SQL-TEXT-CURRENT )
                     MOVE SQL-TEXT-S3        TO SQL-TEXT-S4
                     MOVE SQL-TEXT-ASCII-MAX TO SQL-TEXT-S3
                  END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-S4 = ZEROES )
              MOVE SPACES TO SQL-TEXT-AFTER
                             SQL-TEXT-BEFORE
              EXIT PARAGRAPH.

           IF ( SQL-TEXT-S4 = 1 )
              MOVE SQL-TEXT-ASCIIX(2)               TO SQL-TEXT-AFTER
              MOVE SPACES                           TO SQL-TEXT-BEFORE
           ELSE
           IF ( SQL-TEXT-S4 = SQL-TEXT-ASCII-MAX )
              MOVE SPACES                           TO SQL-TEXT-AFTER
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 - 1) TO SQL-TEXT-BEFORE
           ELSE
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 + 1) TO SQL-TEXT-AFTER
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 - 1) TO SQL-TEXT-BEFORE.

      *================================================================*
      * END COPYBOOK: LIBGB\CONNECT_SQL.CPY                                *
      *================================================================*
