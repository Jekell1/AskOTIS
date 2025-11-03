      * COPYMEMBER: LIBLP/LPLSCLP

      ***********************************************************************
      *    LPLSCL    LOAN STATUS INFORMATION CLEAR ROUTINE. PDTH
      *
      * REV:
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ***********************************************************************
       CLEAR-LSP-FILE SECTION.
           MOVE SPACES TO LSP-REC.
           MOVE 0 TO LSP-BRNO LSP-ACCTNO LSP-SEQNO LSP-LTOUCH-DATE.
           MOVE 0 TO LSP-PERIODS-TO-DEFER LSP-MORTGAGE-BALANCE
                     LSP-INFO-DATE LSP-BR-MANAGER-DATE
                     LSP-AVP-DATE LSP-VP-DATE.

