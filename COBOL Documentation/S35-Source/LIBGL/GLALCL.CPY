
      ************************************************
       CLEAR-AL-FILE SECTION.
      ************************************************
           MOVE SPACES TO AL-REC.

           MOVE 0 TO AL-NO-ACCTS AL-LTOUCH-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE 0 TO AL-ACCT(SUB) AL-PCT(SUB)
           END-PERFORM.
