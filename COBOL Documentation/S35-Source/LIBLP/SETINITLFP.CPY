      * COPYMEMBER: LIBLP/SETINITLFP
      ******************************************************************
      *        (LIBLP)
      *
      *  NAME: SETINITLFP
      *  DESC: SET INITIAL AMORTIZED INTEREST SINCE
      *        LAST FULL PAYMENT (LFP) FIELDS
      *  USED: LONPB2 LONPC2
      *
      *  IN  :
      *  OUT :
      *
      * REV:
      *  JTG 010720 CHANGED TO TEST FOR SP-CAL-RATETYPE(1) = 'Z'
      ******************************************************************
       SET-INITIAL-AMORT-LFP SECTION.
           MOVE 0 TO LN-EXTRA-PRIN-LFP
                     LN-REMAIN-INT-SINCE-LFP
                     LN-REMAIN-PRIN-SINCE-LFP.

           IF SP-CAL-RATETYPE(1) NOT = "Z"
              GO TO SET-INITIAL-AMORT-LFP-EXIT.

           MOVE LN-1STPYDATE  TO RATE-DATE.
           MOVE LN-MAKERCD(1) TO RATE-MAKERCD.
           MOVE "PY"          TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.
           MOVE INDU-INTEREST TO LN-REMAIN-INT-SINCE-LFP.

           COMPUTE LN-REMAIN-PRIN-SINCE-LFP =
              LN-1STPYAMT - INDU-INTEREST.

       SET-INITIAL-AMORT-LFP-EXIT.
           EXIT.

