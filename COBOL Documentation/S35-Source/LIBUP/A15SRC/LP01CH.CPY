      * COPYMEMBER: LIBLP/LP01CH
      ******************************************************************
      *
      *  IDENTIFICATION:  LIBLP/LP01CH
      *
      *  DESCRIPTION   :  DAILY CHANGES FILE                  RSZ = 0091
      *
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "B"  (BANKRUPTCY  )
      *    CH-CODE = "C"  (CONFIDENTIAL FLAG CHANGE)
      *                   (MODERN CEASE&DESIST)
      *    CH-CODE = "D"  (DEBT-AID    )
      *    CH-CODE = "E"  (FROZEN ACCOUNT; ACCOUNT HAS BEEN LOCKED)
      *    CH-CODE = "F"  (CONFIRMATION)  
      *    CH-CODE = "G"   (MAKEUP OF CHARGES ON REVOLVING ACCOUNTS)
      *    CH-CODE = "H"  (CREDIT LIMIT)  
      *    CH-CODE = "I"  (REVOLVING LOANS RATE INDEX ADJUSTMENTS)
      *    CH-CODE = "J"  (REVOLVING INSURANCE CHANGES)
      *    CH-CODE = "K"  (TRW; CREDIT BUREAU CHANGES)
      *    CH-CODE = "L"  (LEGAL       )
      *    CH-CODE = "M"  (MAINTENANCE CHANGES) (BORROWER)
      *    CH-OCDE = "N"  (LOAN ORIGIN)
      *    CH-CODE = "O"  (ESCROW FLAG - CHANGE TO 'I', INACTIVE)
      *    CH-CODE = "P"  (P&L) (CHARGE OFF REASON CODE)
      *    CH-CODE = "Q"  (INVESTOR CHANGES)
      *    CH-CODE = "R"  (NOTE MASTER CHANGES)
      *    CH-CODE = "S"  (SOCIAL SECURITY)
      *    CH-CODE = "T"   (COLLATERAL TRAILER CHANGES)
      *    CH-CODE = "W"  (WCBC CREDIT LIMIT CHANGE)
      *    CH-CODE = "X"  (REPO)
      *    CH-CODE = "Y"  (ALLOTCD/LBOX  )
      *    CH-CODE = "Z"  (DEFICIENCY TRAILER (LS-D) AND LS-P-) PAID THRU
      *    CH-CODE = "1"  (IL GROSS INCOME)
      *    CH-CODE = "2"  (REGACC TDRS DEBT RESTRUCTURE) NEW-LAYOUT
      *-----------------------------------------------------------------
      * REV:
      * MJD 1999.0201 ADDED ACCOUNT FROZEN REDEF.
      * CS  1999.0202 ADDED REVOLVING CHARGES REDEF. AND CH-CODE "G"
      * CS  1999.0212 ADDED REVOLVING RATE INDEX ADJUSTMENT AND
      *                CH-CODE "I"
      * CS  1999.0220 ADDED REVOLVING INSURANCE CODE CHANGES "J"
      * KEC 1999.0527 ADDED 'CH-TRW-TYPE' FOR DIFFERENT TYPES OF
      *                TRW CHANGES; CLEANED-UP THE LAYOUT (ALPHABETIZED
      *                THE 'CH-CODE' REDEFINITIONS)
      * MJD 000915 ADDED "Y" REDEFINITION FOR ALLOTMENT/LOCK BOX CHANGES
      *            (MFC PR#227)
      * BAH 020626 ADDED CH-CODE "P" FOR REGACC P&L REASON CODE PR# 1775
      * CS  020906 ADDED CH-CODE "N" FOR MODERN LN-ORIGN PR# 1876
      * BAH 050908 ADDED CH-CODE "Z" FOR REGACC DEFICIENCY TRAILER PR# 112
      * BLF 060307 ADDED REDEF TO BORROWER CHANGES FOR NO DECIMAL, PR# 2711
      * BAH 060616 REMOVED "O" INTERIM, NOT NECESSARY
      * BLM 100226 ADDED "O", CHANGE LN-ESCROW-FG TO "I", INACTIVE, 
      *            REGWAN PR# 3506
      * BAH 111107 ADDED CH-CODE "1" FOR LN-IL-GROSS-INCOME, WORLD PR# 776
      *  CS 130425 ADDED CH-CODE "2" TDRS (DEBT RESTRUCTURE) NEW LAYOUT
      *                                                REGACC PR#338
      * BAH 160415 ADDED CH-ORG-6DEC2 AND CH-ORG-RATE REDEFINES FOR
      *            BANKRUPT CHANGES (CHBNK2) NEEDED FIELD LENGTHS THAT
      *            WOULD FIT IN A MEMO, WORLD 1150
      *  CS 170328 ADD CH-CODE '3' LOAN SOURCE CODE CHANGES WORLD PR#1238
      ******************************************************************
       78  CH-SIZE                                             VALUE 91.
      *-----------------------------------------------------------------
       01  CH-REC.
           03  CH-DAY                             PIC 9(02)        COMP.
           03  CH-BRNO                            PIC 9(04)        COMP.
           03  CH-CODE                            PIC X(01).
           03  CH-USERID                          PIC X(10).

           03  CH-TIME.
               05  CH-HR                          PIC 9(02)        COMP.
               05  CH-MIN                         PIC 9(02)        COMP.
               05  CH-SEC                         PIC 9(02)        COMP.

           03  CH-DESCRIPT                        PIC X(20).
           03  CH-INFO                            PIC X(54).

      *=================================================================
      *=================================================================
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "S"  (SOCIAL SECURITY)
      *    CH-CODE = "Y"  (ALLOTCD/LBOX  )
      *=================================================================
           03  CH-SSNO-INFO                       REDEFINES CH-INFO.
               05  CH-OLD-SSNO                    PIC 9(10)        COMP.
               05  CH-NEW-SSNO                    PIC 9(10)        COMP.
               05  FILLER                         PIC X(44).
           03  FILLER                             REDEFINES CH-INFO.
               05  FILLER                         PIC X(04).
               05  CH-OLD-ALLOTCD                 PIC X(01).
               05  CH-NEW-ALLOTCD                 PIC X(01).
               05  CH-OLD-LBOX                    PIC X(01).
               05  CH-NEW-LBOX                    PIC X(01).
               05  FILLER                         PIC X(46).

      *=================================================================
      *    CH-CODE = "B"  (BANKRUPTCY  )
      *    CH-CODE = "D"  (DEBT-AID    )
      *    CH-CODE = "F"  (CONFIRMATION)  
      *    CH-CODE = "H"  (CREDIT LIMIT)  
      *    CH-CODE = "L"  (LEGAL       )
      *    CH-CODE = "X"  (REPOSSIONS  )
      *    CH-CODE = "1"  (GROSS INCOME)
      *=================================================================
           03  CH-CREDIT-INFO                     REDEFINES CH-INFO.
               05  CH-CREDIT-ACCTNO               PIC 9(08)        COMP.
               05  CH-OLD-CREDIT                  PIC 9(06)        COMP.
               05  CH-NEW-CREDIT                  PIC 9(06)        COMP.
               05  FILLER                         PIC X(44).

      *=================================================================
      *    CH-CODE = "C"  (CONFIDENTIAL FLAG CHANGE)
      *    MODERN CEASE&DESIST SHARES CH-CODE 'C'
      *=================================================================
           03  CH-CONFID-INFO                     REDEFINES CH-INFO.
               05  CH-CONFID-ACCTNO               PIC 9(08)        COMP.
               05  CH-OLD-CONFIDENT               PIC X(01).
               05  CH-NEW-CONFIDENT               PIC X(01).
               05  FILLER                         PIC X(48).

      *=================================================================
      *    CH-CODE = "E"  (FROZEN ACCOUNT; ACCOUNT HAS BEEN LOCKED)
      *=================================================================
           03  CH-ACCT-FROZEN-INFO                REDEFINES CH-INFO.
               05  CH-FROZEN-ACCTNO               PIC 9(08)        COMP.
               05  CH-OLD-FROZEN                  PIC X(01).
               05  CH-NEW-FROZEN                  PIC X(01).
               05  FILLER                         PIC X(48).

      *=================================================================
      *    CH-CODE = "G"   (MAKEUP OF CHARGES ON REVOLVING ACCOUNTS)
      *=================================================================
           03  CH-REVOLV-CHARGES-INFO             REDEFINES CH-INFO.
               05  CH-REVOLV-ACCTNO               PIC 9(08)        COMP.
               05  CH-REVOLV-INT-OLD              PIC X(01).
               05  CH-REVOLV-INT-NEW              PIC X(01).
               05  CH-REVOLV-LC-OLD               PIC X(01).
               05  CH-REVOLV-LC-NEW               PIC X(01).
               05  CH-REVOLV-MEMSHIP-OLD          PIC X(01).
               05  CH-REVOLV-MEMSHIP-NEW          PIC X(01).
               05  FILLER                         PIC X(44).

      *=================================================================
      *    CH-CODE = "I"  (REVOLVING LOANS RATE INDEX ADJUSTMENTS)
      *=================================================================
           03 CH-REVOLV-INDEX-ADJ                 REDEFINES CH-INFO.
              05  CH-REVOLV-INDEX-ACCTNO          PIC 9(08)        COMP.
              05  CH-REVOLV-IDXADJ1-OLD           PIC S999V999     COMP.
              05  CH-REVOLV-IDXADJ1-NEW           PIC S999V999     COMP.
              05  CH-REVOLV-IDXADJ2-OLD           PIC S999V999     COMP.
              05  CH-REVOLV-IDXADJ2-NEW           PIC S999V999     COMP.
              05  FILLER                          PIC X(34).

      *=================================================================
      *    CH-CODE = "J"  (REVOLVING INSURANCE CHANGES)
      *=================================================================
           03  CH-REVOLV-INSURANCE-CHANGES        REDEFINES CH-INFO.
               05  CH-REVOLV-INS-ACCTNO           PIC 9(08)        COMP.
               05  CH-REVOLV-INSCODES-OLD         PIC X(03).
               05  CH-REVOLV-INSCODES-NEW         PIC X(03).
               05  FILLER                         PIC X(44).
             
      *=================================================================
      *    CH-CODE = "K"  (TRW; CREDIT BUREAU CHANGES)
      *    CH-CODE = "P"  (P&L REASON CODE CHANGES)
      *=================================================================
           03  CH-TRW-INFO                        REDEFINES CH-INFO.
               05  CH-TRW-ACCTNO                  PIC 9(08)        COMP.
               05  CH-OLD-TRW                     PIC X(02).
               05  CH-NEW-TRW                     PIC X(02).
               05  CH-TRW-TYPE                    PIC X(01).
                   88  CH-TRW-TYPE-SKIP-TRW           VALUE "1".
                   88  CH-TRW-TYPE-TRW-STATUS         VALUE "2".
                   88  CH-TRW-TYPE-TRW-SPECIAL-CODE   VALUE "3".
                   88  CH-TRW-TYPE-TRW-B-CONSUMER     VALUE "4".
                   88  CH-TRW-TYPE-TRW-J-CONSUMER     VALUE "5".
                   88  CH-TRW-TYPE-TRW-COMPLIANCE     VALUE "6".
               05  FILLER                         PIC X(45).
      *=================================================================
      *    CH-CODE = "N"  (CHANGE OF LOAN ORIGIN)      
      *=================================================================
           03  CH-ACCT-LOAN-ORIGIN-INFO          REDEFINES CH-INFO.
               05  CH-LOAN-ORIGIN-ACCTNO          PIC 9(08)     COMP.
               05  CH-OLD-LOAN-ORIGIN             PIC X(04).
               05  CH-NEW-LOAN-ORIGIN             PIC X(04).
               05  FILLER                         PIC X(42).

      *=================================================================
      *    CH-CODE = "O"  CHANGE ESCROW FG FROM "Y" TO "I"(INACTIVE)
      *=================================================================
           03  CH-ACCT-ESCROW-INFO                REDEFINES CH-INFO.
               05  CH-ESCROW-ACCTNO               PIC 9(08)        COMP.
               05  CH-OLD-ESCROW-FG               PIC X(01).
               05  CH-NEW-ESCROW-FG               PIC X(01).
               05  FILLER                         PIC X(48).

      *=================================================================
      *    CH-CODE = "2"  CHANGE TDR1 (DEBT RESTRUCTURE)               
      *=================================================================

           03  CH-TDR1-INFO                       REDEFINES CH-INFO.
               05  CH-TDRS-ACCTNO              PIC 9(08)        COMP.
               05  CH-TDR-1-2                  PIC X.
               05  CH-OLD-TDR                  PIC X(01).               
               05  CH-OLD-TDR-DATE             PIC 9(6)         COMP. 
               05  CH-NEW-TDR                  PIC X(01).               
               05  CH-NEW-TDR-DATE             PIC 9(6)         COMP. 
               05  CH-TDRS-SOURCE              PIC X.
               05  FILLER                      PIC X(40).

      *=================================================================
      *    CH-CODE = "3"  CHANGE LOAN SOURCE CODE                       
      *=================================================================
           03  CH-ACCT-SRCD-INFO                  REDEFINES CH-INFO.
               05  CH-LOAN-SRCD-ACCTNO             PIC 9(08)      COMP.
               05  CH-OLD-LOAN-SRCD                PIC X(02).
               05  CH-NEW-LOAN-SRCD                PIC X(02).
               05  FILLER                          PIC X(46).
      *-----------------------------------------------------------------
      *    CH-CODE = "B"  (BANKRUPTCY  ) USING REDEFINES BUT NOT THE CH-M-SSN
      *    CH-CODE = "Z"   (DEFICIENCY TRAILER CHANGES)
      *    CH-CODE = "T"   (COLLATERAL TRAILER CHANGES)
      *    CH-CODE = "R"   (NOTE MASTER CHANGES)
      *=================================================================
      *    CH-CODE = "M"  (MAINTENANCE CHANGES, BORROWER)
      *    CH-CODE = "Q"  (INVESTOR CHANGES)
      *=================================================================
           03  FILLER                             REDEFINES CH-INFO.
               05  CH-M-SSN                       PIC 9(10)        COMP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  CH-ORG-MAINT.
                   07  CH-ORG-CHAR                PIC X(24).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-SNUM            PIC ZZZ/ZZ/ZZZZ.
                       09  FILLER                 PIC X(11).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-TNUM            PIC ZZZ/ZZZ/ZZZZ.
                       09  FILLER                 PIC X(11).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-NUM             PIC Z,ZZZ,ZZZ,ZZ9.
                       09  FILLER                 PIC X(11).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-2DEC            PIC Z,ZZZ,ZZZ,ZZ9.99.
                       09  FILLER                 PIC X(08).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-3DEC            PIC Z,ZZZ,ZZZ,ZZ9.999.
                       09  FILLER                 PIC X(07).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-DATE            PIC 99/99/99.
                       09  FILLER                 PIC X(16).

                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-NODEC           PIC ZZZZZZZZZ9.
                       09  FILLER                 PIC X(14).
                        
                   07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-6DEC2           PIC ZZZZZ9.99.
                       09  FILLER                 PIC X(15).
                       
                  07  FILLER                     REDEFINES CH-ORG-CHAR.
                       09  CH-ORG-RATE           PIC ZZ9.999.
                       09  FILLER                PIC X(17).
                       
  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  CH-NEW-MAINT.
                   07  CH-NEW-CHAR                PIC X(24).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-SNUM            PIC ZZZ/ZZ/ZZZZ.
                       09  FILLER                 PIC X(11).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-TNUM            PIC ZZZ/ZZZ/ZZZZ.
                       09  FILLER                 PIC X(11).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-NUM             PIC Z,ZZZ,ZZZ,ZZ9.
                       09  FILLER                 PIC X(11).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-2DEC            PIC Z,ZZZ,ZZZ,ZZ9.99.
                       09  FILLER                 PIC X(08).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-3DEC            PIC Z,ZZZ,ZZZ,ZZ9.999.
                       09  FILLER                 PIC X(07).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-DATE            PIC 99/99/99.
                       09  FILLER                 PIC X(16).

                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-NODEC           PIC ZZZZZZZZZ9.
                       09  FILLER                 PIC X(14).
                       
                   07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-6DEC2           PIC ZZZZZ9.99.
                       09  FILLER                 PIC X(15).
                       
                  07  FILLER                     REDEFINES CH-NEW-CHAR.
                       09  CH-NEW-RATE           PIC ZZ9.999.
                       09  FILLER                PIC X(17).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  FILLER                         PIC X(01).

               
