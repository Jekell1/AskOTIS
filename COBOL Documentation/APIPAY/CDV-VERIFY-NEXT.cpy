       CDV-VERIFY-NEXT.
           ADD 1 TO CDV-SUB.
           MOVE CDV-CD(CDV-SUB) TO STAT.
           IF STAT = LP-TRCD
              GO TO CDV-VERIFY-EXIT.
           IF STAT NOT = " "
              GO TO CDV-VERIFY-NEXT.
           MOVE 0 TO CDV-SUB.
