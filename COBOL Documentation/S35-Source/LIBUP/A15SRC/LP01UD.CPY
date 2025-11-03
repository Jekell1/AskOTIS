      * COPYMEMBER: LIBLP/LP01UD
      *************************************************************
      *                     'UD'
      *            ( UNEARNED DELINQUENCY STATISTICS FILE )                  
      *            RSZ - 
      * REV:
      * BLV 010302 ADDED UD-VOL-AMT, MTD & YTD BY SOURCE CODE
      *************************************************************
       78  UD-SIZE                        VALUE 6399.
       01  UD-REC.
           03  UD1-KEY.
               05  UD-DATE                PIC 9(5).
               05  UD-BRNO                PIC 9(4).
               05  UD-CLASS               PIC 9(2).
           03  UD-DATA.
      *    -------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE NON P&L, NON REPO ACCOUNTS
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *
      *    UD-NOW-BMO
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *
      *    UD-DEL-BUCKET
      *        1- 30
      *        2- 60
      *        3- 90        
      *        4- 120        
      *        5- 150        
      *        6- 180        
      *        7- 210+        
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *    -------------------------------------------------
               05  UD-NOW-BMO              OCCURS 2.
                   07  UD-DEL-BUCKET   OCCURS 10.
      *            DELINQUENT UNITS:                                 
                       09  UD-UNITS    PIC S9(7)    COMP.
      *            OUTSTANDING BALANCE:
                       09  UD-NETAMT   PIC S9(9)V99 COMP.
      *    -------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *
      *    UD-NOW-BMO-BNKRPT
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *
      *    UD-DEL-BUCKET-BNKRPT
      *        1- 30
      *        2- 60
      *        3- 90        
      *        4- 120        
      *        5- 150        
      *        6- 180        
      *        7- 210+        
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *    -------------------------------------------------
               05  UD-NOW-BMO-BNKRPT          OCCURS 2.
                   07  UD-DEL-BUCKET-BNKRPT   OCCURS 10.
      *            DELINQUENT UNITS:                                 
                       09  UD-UNITS-BNKRPT    PIC S9(7)    COMP.
      *            OUTSTANDING BALANCE:
                       09  UD-NETAMT-BNKRPT   PIC S9(9)V99 COMP.
               05  UD-MTD-YTD                 OCCURS 2.
                   07  UD-TYPE-TABLE          OCCURS 11.
                       09  UD-VOL-AMT         PIC S9(9)V99 COMP.
      *    -------------------------------------------------
           03  FILLER                             PIC X(30).
