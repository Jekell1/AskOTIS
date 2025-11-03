      *COPY MEMBER LIBGB/GRBRLS_SQL
      ******************************************************************
      * DESCRIPTION: I-O FOR CALLING STORED PROCEDURE
      *                  SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *  ALSO NEED WORKING STORAGE COPY BOOK LIBGB/GRBRLSW
      *  CALL STORED PROCEDURE PER FOLLOWING USING :BGLS-PASS-BEG-BRGRP,
      *                                            :BGLS-PASS-END-BRGRP
      *                     :BGLS-PASS-BEG-BRGRP   :BGLS-PASS-END-BRGRP
      *  INDIVIDUAL BRANCH      BRANCH NUMBER          BRANCH NUMBER
      *  BRANCH RANGE           BEG BRANCH             END BRANCH
      *  GROUP                  GROUP NUMBER           SPACES
      *=================================================================
      * REV:
      * MB  2023-08-21 *NEW*
      ******************************************************************
       LOAD-BGLS-BRANCH-LIST.
           PERFORM START-BGLS-FILE.

           PERFORM UNTIL IO-FG NOT = 0
              PERFORM READ-BGLS-FILE-NEXT
              IF IO-FG = 0
                ADD 1 TO BGLS-BRANCHES-RETR-CTR
                MOVE BGLS-RET-MEMBER-ID TO 
                      BGLS-BRANCH-BRNO(BGLS-BRANCHES-RETR-CTR)
              END-IF
           END-PERFORM.

           PERFORM CLOSE-BGLS-FILE.

       START-BGLS-FILE.
           PERFORM START-IT.
           MOVE "SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT" TO E-FILE.
           MOVE SPACES                               TO E-KEYX.

           IF ( BGLS-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BGLS-FILE.

           EXEC SQL
            DECLARE BGLS_CURSOR CURSOR FOR
                :BGLS-RETURN-CODE = 
                 EXEC SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
                  ( :BGLS-PASS-BEG-BRGRP, :BGLS-PASS-END-BRGRP)
                END-EXEC.

           EXEC SQL
               OPEN BGLS_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO BGLS-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BGLS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE "SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT" TO E-FILE.
           MOVE SPACES                               TO E-KEYX.

           IF ( BGLS-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BGLS_CURSOR INTO 
                     :BGLS-RET-GRID,
                     :BGLS-RET-KEY,
                     :BGLS-RET-MEMBER-ID
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BGLS-FILE-NEXT.

      *-----------------------------------------------------------------
       CLOSE-BGLS-FILE.
           PERFORM CLOSE-IT.

           IF ( BGLS-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BGLS_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BGLS-CURSOR-STAT.
