      * COPYMEMBER: LIBLP/LPLSDCL

      ***********************************************************************
      *    LPLSDCL    LOAN STATUS INFORMATION CLEAR ROUTINE. DEFICIENCY
      *
      * REV:
      *  BAH 060724 ADDED CLEAR OF LS-D-DEF-LETTER-SENT
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ***********************************************************************
       CLEAR-LSD-FILE SECTION.
      * DEFICIENCY RECORD
           MOVE SPACES TO LSD-REC.
           MOVE 0 TO LSD-BRNO LSD-ACCTNO LSD-SEQNO LSD-LTOUCH-DATE.
           MOVE 0 TO LSD-STOLEN-DATE LSD-GAP-INS-PAID
                        LSD-OTHER-EXPENSE LSD-MISC-AMT-EXPENSE
                        LSD-INT-REB
                        LSD-CPP-REB LSD-GAP-REB LSD-WARRANTY-REB
                        LSD-MISC-AMT-CREDIT LSD-PAID-TO-OTHER
                        LSD-AMT-FROM-OWING LSD-AMT-OF-SURPLUS
                        LSD-AMT-OF-DEFICIENCY LSD-SEND-FMNO
                        LSD-INSURANCE-PROCEEDS LSD-DATE-CHECK-RECV
                        LSD-INSURANCE-CHECK LSD-DEF-LETTER-SENT-DATE
                        LSD-TOT-EXPENSE LSD-TOT-CREDITS
                        LSD-SALES-DATE
                        LSD-AUCTN-TIME
                        LSD-AUCTN-PHONE LSD-REPO-CHARGE
                        LSD-STORAGE-CHARGE LSD-STORAGE-RATE
                        LSD-REPAIR-CHARGE LSD-OTHER-CHARGE
                        LSD-EST-REBATE LSD-MINIMUM-BID.
