      * COPYMEMBER: LIBLP/LPALECL
      ******************************************************************
      *
      *    C L E A R - L E - F I L E
      *
      ******************************************************************
       CLEAR-ALE-FILE SECTION.

           MOVE ALL X"09" TO ALE-REC.

           MOVE 0 TO ALE-BRNO
               ALE-ACCTNO                      ALE-SSNO
               ALE-OSACCTNO                    ALE-LTOUCH-DATE
               ALE-E-OSRATE                    ALE-E-OSAMT
               ALE-E-OSDATE                    ALE-E-AMT
               ALE-E-DATE               
               ALE-R-LOANDATE                  ALE-R-LNAMT
               ALE-R-POFFDATE                  ALE-R-RESTRICTED-MONEY
               ALE-R-BALANCE.

 
           MOVE SPACES TO ALE-TYPE ALE-FLAG ALE-E-OSMAKERCD
                          ALE-R-SALEFIN.
