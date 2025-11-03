      ******************************************
       CREATE-LOG SECTION.
      ******************************************
           MOVE LOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-LOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-LOG-FILE-OUTPUT.

           IF RETURN-STATUS = 99
              MOVE SPACES     TO LOG-STATUS
           ELSE
           IF RETURN-STATUS = 0
              MOVE "PASS"     TO LOG-STATUS
           ELSE
              MOVE "FAIL"     TO LOG-STATUS.

           MOVE RETURN-STATUS TO LOG-RETURN.
           MOVE TRANS-DATE    TO LOG-DATE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT     TO LOG-TIME.
           MOVE BT-BRANCH     TO LOG-BRNO.
           MOVE BP-LNNO       TO LOG-NUMBER.
           PERFORM WRITE-LOG-FILE.
           PERFORM CLOSE-LOG-FILE.

      ******************************************************************
