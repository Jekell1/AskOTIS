       CALC-HOLD-LN-ACCERN.
           MOVE LN-EARN-ORDER(1) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(1) = LN-ACCERN(1) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(1) = LN-ACCERN(1) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(2) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(2) = LN-ACCERN(2) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(2) = LN-ACCERN(2) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(3) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(3) = LN-ACCERN(3) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(3) = LN-ACCERN(3) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(4) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(4) = LN-ACCERN(4) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(4) = LN-ACCERN(4) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(5) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(5) = LN-ACCERN(5) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(5) = LN-ACCERN(5) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(6) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(6) = LN-ACCERN(6) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(6) = LN-ACCERN(6) + LP-EARNED(SUB).

