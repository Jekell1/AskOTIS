      *================================================================*
      * END COPYBOOK: LIBLP\LPLTI1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLTP1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLTP1RN
      *=================================================================
       GET-LT-P-REC.
           MOVE 0 TO IO-FG.
           IF NOT (LN-ACCTNO = LTP-ACCTNO)
              MOVE LN-OWNBR  TO LTP-BRNO
              MOVE LN-ACCTNO TO LTP-ACCTNO
              MOVE 1         TO LTP-SEQNO
              PERFORM OPEN-LTP1-FILE
              PERFORM READ-LTP1-FILE
              PERFORM CLOSE-LTP1-FILE.

      *-----------------------------------------------------------------
