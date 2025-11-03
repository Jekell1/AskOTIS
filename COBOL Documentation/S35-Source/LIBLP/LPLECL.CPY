      * COPYMEMBER: LIBLP/LPLECL
      ******************************************************************
      *
      *    C L E A R - L E - F I L E
      *
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-LE-FILE SECTION.

           MOVE SPACES TO LE-REC.
           MOVE ZEROES TO LE-BRNO
               LE-ACCTNO                      LE-SSNO
               LE-OSACCTNO                    LE-LTOUCH-DATE
               LE-E-OSRATE                    LE-E-OSAMT
               LE-E-OSDATE                    LE-E-AMT
               LE-E-DATE               
               LE-R-LOANDATE                  LE-R-LNAMT
               LE-R-POFFDATE                  LE-R-RESTRICTED-MONEY
               LE-R-BALANCE.

       CLEAR-LE-FILE-EXIT.
           EXIT.
