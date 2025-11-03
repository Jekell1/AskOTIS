      * COPYMEMBER: LIBLP/LP01PC
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01PC
      *
      * DESCRIPTION   :  LOAN SCRATCH PAD FILE (PC-FILE)
      *                  --- FOR F3 EXPANDED COLLATERAL
      *  PC-ID = "A" FOR SAVED AUTO INSURANCE INFO
      *  PC-ID = "C" FOR SAVED AUTO 1, AUTO 2, HHGS 1 AND HHGS2
      *  PC-ID = "5" FOR SAVED HHGS 3 (LT-SEQ 5) FOR UNITED ACCEPTANCE
      *
      *=================================================================
      * REV:
      * MJD 1999.0127 ----- UPDATED IN FLEXGEN -----
      *
      * KEC 2006.0621 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ADDED PC-SIZE.
      * BAH 100128 ADDED PC-ID = "5" FOR UNITED ACCEPTANCE TO STORE ANOTHER
      *            LINE OF HHGS, PR# 3524
      *
      ******************************************************************
       78  PC-SIZE                                   VALUE 435.
      *=================================================================
       01  PC-REC.
           03  PC1-KEY.
               05  PC-ID               PIC X.
               05  PC-BRANCH           PIC 9(4)  COMP-3.
               05  PC-SOC-SEC          PIC 9(10) COMP-3.
           03  PC-LT-RECORD            PIC X(370).
           03  PC-LOANDATE             PIC 9(6).
           03  PC-ORGTERM              PIC 9(3).
           03  PC-REGPYAMT             PIC 9(6)V99.
           03  PC2-KEY.
               05  PC2-ID              PIC X.
               05  PC2-BRANCH          PIC 9(4).
               05  PC2-BWNAME          PIC X(33).
 
