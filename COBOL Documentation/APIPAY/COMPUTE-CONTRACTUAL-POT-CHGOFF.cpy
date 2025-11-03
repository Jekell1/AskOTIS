      * TEST FOR POTENTIAL CHARGE OFF'S
       COMPUTE-CONTRACTUAL-POT-CHGOFF.

           IF SP-BANKRULE = "F"
              IF CSUB > 2
                 DIVIDE CSUB BY 2 GIVING AGEING-SUB-WORKER
                                  REMAINDER AGEING-SUB-REMAIN
                 IF AGEING-SUB-REMAIN NOT = 0
                    ADD 1 TO AGEING-SUB-WORKER
                 END-IF
                 MOVE AGEING-SUB-WORKER TO CSUB.

      *****************************************************
      *  POTENTIAL CHARGE OFF'S CODE = 'A'
      *    ALL ACCOUNTS THAT ARE 150 DAYS OR MORE
      *    (USED TO BE MERCURY'S CODE)
      *****************************************************

           IF SP-CPOTCHGOFF-CD = "A"
              IF CSUB > 5
                 IF LN-PLCD = " "
                    IF CSUB > 6
                       MOVE "M" TO CPOTCHGOFF
                    ELSE
                       MOVE "Y" TO CPOTCHGOFF
                    END-IF
                 END-IF
              END-IF
           ELSE
      * REGENCY POTENTIAL CHARGE OFF'S CODE = 'B'
      * ALL ACCOUNTS THAT ARE 180 DAYS OR MORE
           IF SP-CPOTCHGOFF-CD = "B"
              IF CSUB > 6
                 IF LN-PLCD = " "
                    IF CSUB > 7
                       MOVE "M" TO CPOTCHGOFF
                    ELSE
                       MOVE "Y" TO CPOTCHGOFF
                    END-IF
                 END-IF
              END-IF
      * MERCURY POTENTIAL CHARGE OFF'S CODE = 'C'
      * ALL ACCOUNTS THAT ARE 120 DAYS OR MORE
           ELSE
           IF SP-CPOTCHGOFF-CD = "C"
              IF CSUB > 4
                 IF LN-PLCD = " "
                    IF CSUB > 5
                       MOVE "M" TO CPOTCHGOFF
                    ELSE
                       MOVE "Y" TO CPOTCHGOFF.

