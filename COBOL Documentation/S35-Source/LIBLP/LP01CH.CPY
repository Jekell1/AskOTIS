      * COPYMEMBER: LIBLP/LP01CH
      ******************************************************************
      *
      *  IDENTIFICATION:  LIBLP/LP01CH
      *
      *  DESCRIPTION   :  DAILY CHANGES FILE       
      *
      *  CH-KEY = CH-BRNO, CH-DATE, CH-CODE, CH-USERID, CH-TIME, CH-SEQ
      *
      *
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "B"  (BANKRUPTCY  )
      *    CH-CODE = "C"  (CONFIDENTIAL FLAG CHANGE)
      *                   (MODERN CEASE&DESIST)
      *                   (WORLD DUAL LOAN OVERRIDE (LN-GIFTNET))
      *    CH-CODE = "D"  (DEBT-AID    )
      *    CH-CODE = "E"  (FROZEN ACCOUNT; ACCOUNT HAS BEEN LOCKED)
      *    CH-CODE = "F"  (CONFIDENTIAL)
      *    CH-CODE = "G"   ** OPEN ** - WAS REVOLVING
      *    CH-CODE = "H"  (CREDIT LIMIT)
      *    CH-CODE = "I"   ** OPEN ** - WAS REVOLVING RATE INDEX ADJ)
      *    CH-CODE = "J"   ** OPEN ** - WAS REVOLVING INSURANCE
      *    CH-CODE = "K"  (TRW; CREDIT BUREAU CHANGES)
      *    CH-CODE = "L"  (LEGAL       )
      *    CH-CODE = "M"  (MAINTENANCE CHANGES) (BORROWER)
      *    CH-OCDE = "N"  (LOAN ORIGIN)
      *    CH-CODE = "O"  (ESCROW FLAG - CHANGE TO 'I', INACTIVE)
      *    CH-CODE = "P"  (P&L) (CHARGE OFF REASON CODE)
      *    CH-CODE = "Q"  (CHLOAN; CHANGE SNSO'S)
      *    CH-CODE = "R"  (REVERSALS)
      *    CH-CODE = "S"  (SOCIAL SECURITY)
      *    CH-CODE = "T"   (COLLATERAL TRAILER CHANGES)
      *    CH-CODE = "W"  (WCBC CREDIT LIMIT CHANGE)
      *    CH-CODE = "X"  (REPO)
      *    CH-CODE = "Y"  (LOCK BOX/ALLOTMENT)
      *    CH-CODE = "Z"  (DEFICIENCY TRAILER (LS-D) AND LS-P-) PAID THRU
      *    CH-CODE = "1"  (IL GROSS INCOME)
      *    CH-CODE = "2"  (REGACC TDRS DEBT RESTRUCTURE) NEW-LAYOUT
      *    CH-CODE = "3"  (CHSRCD, LN SOURCE CODE CHANGES)
      *    CH-CODE = "4"  (CHCOCD, LN COLLATERAL CODE CHANGES)
      *    CH-CODE = "5"  (CHACTN, CHANGE ACTIONCD2 SECURITIZATION CODE)
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
      *  CS 170328 ADD CH-CODE "3" LOAN SOURCE CODE CHANGES WORLD PR#1238
      *  CS 190517 REMOVED 'CH-INFO PIC X(100)'; MADE
      *            'CH-ORG-CHAR PIC X(50)' AND 'CH-NEW-CHAR PIC X(50)'
      *            PRIMARY DEFINITIONS INSTEAD OF REDEFINES OF CH-INFO.
      * BAH 201016 ADDED XFD DATE TO CH-DATE
      * BAH 2022.0114 ADD CH-CODE "4" COLLATERAL CODE CHANGE #1536
      * BAH 2022.0426 ADD CH-CODE "5" ACTIONCD 2 SECURITIZATION #1551
      * BAH 20230600 ADDED FILLER
      ******************************************************************
       78  CH-SIZE                                           VALUE 225.
      *-----------------------------------------------------------------
       01  CH-REC.
           03  CH-KEY.
               05  CH-BRNO                        PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  CH-DATE                        PIC 9(8).
               05  CH-CODE                        PIC X(3).
               05  CH-USERID                      PIC X(10).
               05  CH-TIME                        PIC 9(6).
               05  FILLER REDEFINES CH-TIME.
                   07  CH-HR                      PIC 99.
                   07  CH-MIN                     PIC 99.
                   07  CH-SEC                     PIC 99.
               05  CH-SEQ                         PIC 9(3).

      $XFD DATE=YYYYMMDD
           03  CH-LTOUCH-DATE                     PIC 9(8) COMP-3.

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

      *******************************************************************

      *=================================================================
      *    CH-CODE = "A"  (DEALER NUMBER  )
      *    CH-CODE = "S"  (SOCIAL SECURITY)
      *    CH-CODE = "Z"  (DEFICIENCY TRAILER CHANGES)
      *    CH-CODE = "T"  (COLLATERAL TRAILER CHANGES)
      *    CH-CODE = "R"  (NOTE MASTER CHANGES)
      *    CH-CODE = "M"  (MAINTENANCE CHANGES, BORROWER)
      *    CH-CODE = "Q"  (CHLOAN; CHANGE SNSO'S)
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
      *    CH-CODE = "J"  
      *    CH-CODE = "K"  (TRW; CREDIT BUREAU CHANGES)
      *    CH-CODE = "P"  (P&L REASON CODE CHANGES)
      *    CH-CODE = "N"  (CHANGE OF LOAN ORIGIN)
      *    CH-CODE = "O"  CHANGE ESCROW FG FROM "Y" TO "I"(INACTIVE)
      *    CH-CODE = "4"  (CHCOCD, CHANGE OF LOAN COLLATERAL CODE)
      *    CH-CODE = "5"  (CHACTN, CHANGE OF LOAN ACTIONCD2 SECURITIZATION
      *=================================================================
           03  CH-ORG-CHAR                 PIC X(50).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-SSNO             PIC 9(10).
               05  FILLER                  PIC X(40).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-PHONE            PIC ZZZ/ZZZ/ZZZZ.
               05  FILLER                  PIC X(38).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-NUM              PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                  PIC X(37).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-2DEC             PIC Z,ZZZ,ZZZ,ZZ9.99.
               05  FILLER                  PIC X(34).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-3DEC             PIC Z,ZZZ,ZZZ,ZZ9.999.
               05  FILLER                  PIC X(33).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-DATE             PIC 99/99/99.
               05  FILLER                  PIC X(42).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-NODEC            PIC ZZZZZZZZZ9.
               05  FILLER                  PIC X(40).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-6DEC2            PIC ZZZZZ9.99.
               05  FILLER                  PIC X(41).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-RATE             PIC ZZ9.999.
               05  FILLER                  PIC X(43).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-ORG-ALLOTCD          PIC X.
               05  CH-ORG-LBOX             PIC X.
               05  FILLER                  PIC X(48).
           03  FILLER                   REDEFINES CH-ORG-CHAR.
               05  CH-TDR-1-2              PIC X.
               05  CH-ORG-TDR              PIC X.
               05  CH-ORG-TDR-DATE         PIC 9(8).
               05  FILLER                  PIC X(40).

      ***********
           03  CH-NEW-CHAR                 PIC X(50).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-SSNO             PIC 9(10).
               05  FILLER                  PIC X(40).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-PHONE            PIC ZZZ/ZZZ/ZZZZ.
               05  FILLER                  PIC X(38).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-NUM              PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                  PIC X(37).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-2DEC             PIC Z,ZZZ,ZZZ,ZZ9.99.
               05  FILLER                  PIC X(34).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-3DEC             PIC Z,ZZZ,ZZZ,ZZ9.999.
               05  FILLER                  PIC X(33).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-DATE             PIC 99/99/99.
               05  FILLER                  PIC X(42).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-NODEC            PIC ZZZZZZZZZ9.
               05  FILLER                  PIC X(40).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-6DEC2            PIC ZZZZZ9.99.
               05  FILLER                  PIC X(41).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-RATE             PIC ZZ9.999.
               05  FILLER                  PIC X(43).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-ALLOTCD          PIC X.
               05  CH-NEW-LBOX             PIC X.
               05  FILLER                  PIC X(48).
           03  FILLER                   REDEFINES CH-NEW-CHAR.
               05  CH-NEW-TDR              PIC X.
               05  CH-NEW-TDR-DATE         PIC 9(8).
               05  CH-TDRS-SOURCE          PIC X.
               05  FILLER                  PIC X(40).

           03  FILLER                      PIC X(49).
