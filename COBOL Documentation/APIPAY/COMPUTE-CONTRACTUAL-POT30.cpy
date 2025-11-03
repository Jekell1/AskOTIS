      *    TEST FOR POTENTIAL 30'S AND MISSPAY'S
       COMPUTE-CONTRACTUAL-POT30.

      *************************************************************
      *   MERCURY POTENTIAL 30 CODE = 'A'
      *      ALL CURRENT ACCOUNTS THAT ARE NOT PAID AHEAD
      *      ARE CONSIDERED POTENTIAL 30
      *************************************************************

           IF SP-CPOT30-CD = "A"
      * CURRENT PAID AHEAD'S ARE NOT POTENTIAL 30:
              IF CSUB = 1
                 MOVE "Y" TO CPOT30.

      * TEST FOR POTENTIAL CHARGE OFF'S
      
