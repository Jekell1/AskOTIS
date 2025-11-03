      * COPYMEMBER: LIBLP/LP01CH
      ******************************************************************
      *
      *  IDENTIFICATION:  LIBLP/LP01CH
      *
      *  DESCRIPTION   :  DAILY CHANGES FILE       
      *
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "B"  (BANKRUPTCY  )
      *    CH-CODE = "C"  (CONFIDENTIAL FLAG CHANGE)
      *                   (MODERN CEASE&DESIST)
      *                   (WORLD DUAL LOAN OVERRIDE (LN-GIFTNET))
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
      *    CH-CODE = "Z"  (DEFICIENCY TRAILER (LS-D) AND LS-P-) PAID THRU
      *    CH-CODE = "1"  (IL GROSS INCOME)
      *    CH-CODE = "2"  (REGACC TDRS DEBT RESTRUCTURE) NEW-LAYOUT
      *    CH-CODE = "3"  (LN SOURCE CODE CHANGES)
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
       78  CH-SIZE                                           VALUE 200.
      *-----------------------------------------------------------------
       01  CH-REC.
           03  CH-KEY.
               05  CH-BRNO                        PIC 9(4).
               05  CH-DAY                         PIC 9(2).
               05  CH-CODE                        PIC X(3).
               05  CH-USERID                      PIC X(10).
               05  CH-TIME                        PIC 9(6).
               05  FILLER REDEFINES CH-TIME.
                   07  CH-HR                      PIC 99.
                   07  CH-MIN                     PIC 99.
                   07  CH-SEC                     PIC 99.
               05  CH-SEQ                         PIC 9(2).

           03  CH-DESCRIPT                        PIC X(25).
           03  CH-ACCTNO                          PIC 9(8)    COMP-3.
           03  CH-SSNO                            PIC 9(9)    COMP-3.
      * TYPE-? IS CH-CODE IT IS TIED TO
           03  CH-TYPE                            PIC 99.
               88  CH-TYPE-K-TRW-SKIP           VALUE 1.
               88  CH-TYPE-K-TRW-STATUS         VALUE 2.
               88  CH-TYPE-K-TRW-SPECIAL-CODE   VALUE 3.
               88  CH-TYPE-K-TRW-B-CONSUMER     VALUE 4.
               88  CH-TYPE-K-TRW-J-CONSUMER     VALUE 5.
               88  CH-TYPE-K-TRW-COMPLIANCE     VALUE 6.
           03  CH-INFO                            PIC X(100).
      *******************************************************************

      *=================================================================
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "S"  (SOCIAL SECURITY)
      *    CH-CODE = "Z"  (DEFICIENCY TRAILER CHANGES)
      *    CH-CODE = "T"  (COLLATERAL TRAILER CHANGES)
      *    CH-CODE = "R"  (NOTE MASTER CHANGES)
      *    CH-CODE = "M"  (MAINTENANCE CHANGES, BORROWER)
      *    CH-CODE = "Q"  (INVESTOR CHANGES)
      *    CH-CODE = "B"  (BANKRUPTCY  )
      *    CH-CODE = "D"  (DEBT-AID    )
      *    CH-CODE = "F"  (CONFIRMATION)
      *    CH-CODE = "H"  (CREDIT LIMIT)
      *    CH-CODE = "L"  (LEGAL       )
      *    CH-CODE = "X"  (REPOSSIONS  )
      *    CH-CODE = "1"  (GROSS INCOME)
      *    CH-CODE = "C"  (CONFIDENTIAL FLAG CHANGE) (MODERN CEASE&DESIST)
      *                   (DUAL LOAN OVERRIDE LN-GIFTNET)
      *    CH-CODE = "E"  (FROZEN ACCOUNT; ACCOUNT HAS BEEN LOCKED)
      *    CH-CODE = "J"  (REVOLVING INSURANCE CHANGES)
      *    CH-CODE = "K"  (TRW; CREDIT BUREAU CHANGES)
      *    CH-CODE = "P"  (P&L REASON CODE CHANGES)
      *    CH-CODE = "N"  (CHANGE OF LOAN ORIGIN)
      *    CH-CODE = "O"  CHANGE ESCROW FG FROM "Y" TO "I"(INACTIVE)
      *=================================================================
           03  FILLER                    REDEFINES CH-INFO.

      ***********
               05  CH-ORG-CHAR                 PIC X(50).
      ***********
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-SSNO             PIC 9(10).
                   07  FILLER                  PIC X(40).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-PHONE            PIC ZZZ/ZZZ/ZZZZ.
                   07  FILLER                  PIC X(38).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-NUM              PIC Z,ZZZ,ZZZ,ZZ9.
                   07  FILLER                  PIC X(37).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-2DEC             PIC Z,ZZZ,ZZZ,ZZ9.99.
                   07  FILLER                  PIC X(34).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-3DEC             PIC Z,ZZZ,ZZZ,ZZ9.999.
                   07  FILLER                  PIC X(33).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-DATE             PIC 99/99/99.
                   07  FILLER                  PIC X(42).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-NODEC            PIC ZZZZZZZZZ9.
                   07  FILLER                  PIC X(40).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-6DEC2            PIC ZZZZZ9.99.
                   07  FILLER                  PIC X(41).
               05  FILLER                   REDEFINES CH-ORG-CHAR.
                   07  CH-ORG-RATE             PIC ZZ9.999.
                   07  FILLER                  PIC X(43).


      ***********
               05  CH-NEW-CHAR                 PIC X(50).
      ***********
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-SSNO             PIC 9(10).
                   07  FILLER                  PIC X(40).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-PHONE            PIC ZZZ/ZZZ/ZZZZ.
                   07  FILLER                  PIC X(38).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-NUM              PIC Z,ZZZ,ZZZ,ZZ9.
                   07  FILLER                  PIC X(37).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-2DEC             PIC Z,ZZZ,ZZZ,ZZ9.99.
                   07  FILLER                  PIC X(34).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-3DEC             PIC Z,ZZZ,ZZZ,ZZ9.999.
                   07  FILLER                  PIC X(33).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-DATE             PIC 99/99/99.
                   07  FILLER                  PIC X(42).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-NODEC            PIC ZZZZZZZZZ9.
                   07  FILLER                  PIC X(40).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-6DEC2            PIC ZZZZZ9.99.
                   07  FILLER                  PIC X(41).
               05  FILLER                   REDEFINES CH-NEW-CHAR.
                   07  CH-NEW-RATE             PIC ZZ9.999.
                   07  FILLER                  PIC X(43).

      *=================================================================
      *    CH-CODE = "Y"  (ALLOTCD/LBOX  )
      *=================================================================
           03  CH-ALLOT-LBOX-INFO           REDEFINES CH-INFO.
               05  CH-ORG-ALLOTCD                 PIC X.
               05  CH-NEW-ALLOTCD                 PIC X.
               05  CH-ORG-LBOX                    PIC X.
               05  CH-NEW-LBOX                    PIC X.
               05  FILLER                         PIC X(96).

      *=================================================================
      *    CH-CODE = "G"   (MAKEUP OF CHARGES ON REVOLVING ACCOUNTS)
      *=================================================================
           03  CH-REVOLV-CHARGES-INFO        REDEFINES CH-INFO.
               05  CH-REVOLV-INT-OLD              PIC X(01).
               05  CH-REVOLV-INT-NEW              PIC X(01).
               05  CH-REVOLV-LC-OLD               PIC X(01).
               05  CH-REVOLV-LC-NEW               PIC X(01).
               05  CH-REVOLV-MEMSHIP-OLD          PIC X(01).
               05  CH-REVOLV-MEMSHIP-NEW          PIC X(01).
               05  FILLER                         PIC X(94).

      *=================================================================
      *    CH-CODE = "I"  (REVOLVING LOANS RATE INDEX ADJUSTMENTS)
      *=================================================================
           03 CH-REVOLV-INDEX-ADJ                 REDEFINES CH-INFO.
              05  CH-REVOLV-IDXADJ1-OLD           PIC S999V999.
              05  CH-REVOLV-IDXADJ1-NEW           PIC S999V999.
              05  CH-REVOLV-IDXADJ2-OLD           PIC S999V999.
              05  CH-REVOLV-IDXADJ2-NEW           PIC S999V999.
              05  FILLER                          PIC X(76).

      *=================================================================
      *    CH-CODE = "2"  CHANGE TDR1 (DEBT RESTRUCTURE)
      *=================================================================

           03  CH-TDR1-INFO                       REDEFINES CH-INFO.
               05  CH-TDR-1-2                  PIC X.
               05  CH-ORG-TDR                  PIC X.
               05  CH-ORG-TDR-DATE             PIC 9(8).
               05  CH-NEW-TDR                  PIC X.
               05  CH-NEW-TDR-DATE             PIC 9(8).
               05  CH-TDRS-SOURCE              PIC X.
               05  FILLER                      PIC X(80).
      *=================================================================
      *    CH-CODE = "3"  CHANGE LOAN SOURCE CODE                       
      *=================================================================
           03  CH-ACCT-SRCD-INFO                  REDEFINES CH-INFO.
               05  CH-LOAN-SRCD-ACCTNO             PIC 9(08)      COMP.
               05  CH-OLD-LOAN-SRCD                PIC X(02).
               05  CH-NEW-LOAN-SRCD                PIC X(02).
               05  FILLER                          PIC X(46).

           03  FILLER                          PIC X(36).


