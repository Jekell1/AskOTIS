      * COPYMEMBER: LIBLP/LP01PC
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01PC
      *
      * DESCRIPTION   :  LOAN SCRATCH PAD FILE (PC-FILE)
      *                  --- FOR F3 EXPANDED COLLATERAL
      *  PC-ID = "A" FOR SAVED AUTO INSURANCE INFO
      *  PC-ID = "C" FOR SAVED AUTO 1, AUTO 2, HHGS 1, HHGS 2, HHGS 3
      *
      *=================================================================
      * REV:
      * MJD 1999.0127 ----- UPDATED IN FLEXGEN -----
      *
      * KEC 2006.0621 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ADDED PC-SIZE.
      * BAH 100128 ADDED PC-ID = "5" FOR UNITED ACCEPTANCE TO STORE ANOTHER
      *            LINE OF HHGS, PR# 3524
      * BAH 160114 MADE SURE PC-LT-RECORD WAS CORRECT SIZE TO HOLD
      *            WK-LT-RECORD AND/OR WK-LT-A-RECORD PD0003
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED COMP-3 FROM PC1-KEY.
      *
      ******************************************************************
       78  PC-SIZE                                   VALUE 5060.
      *=================================================================
       01  PC-REC.
           03  PC1-KEY.
               05  PC-ID               PIC X.
               05  PC-BRANCH           PIC 9(4).
               05  PC-SOC-SEC          PIC 9(10).
      * PC-LT-RECORD HOLDS WK-LT-RECORD AND WK-LT-A-RECORD
           03  PC-LT-RECORD            PIC X(4800).
      *>>XDD DATE=YYYYMMDD
           03  PC-LOANDATE             PIC 9(8)    COMP-3.
           03  PC-ORGTERM              PIC 9(3).
           03  PC-REGPYAMT             PIC 9(6)V99 COMP-3.
           03  PC2-KEY.
               05  PC2-ID              PIC X.
               05  PC2-BRANCH          PIC 9(4).
               05  PC2-BWNAME          PIC X(33).

           03  FILLER                  PIC X(194).
