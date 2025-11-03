      * COPYMEMBER: LIBLP/LP01DS
      **************************************************************************
      *          ( DEALER STATISTICS FILE )
       78  DS-SIZE                           VALUE 2136.
      *
      *    KEY 1 - DATE, BRANCH, DLNO, CLASS                
      *    KEY 2 - DLNO, CLASS                                
      *
      * REV:
      *  MJD 100295 CHANGED PROMPTS FROM 90 TO 90-120 AND 120+ TO 150+.
      *  JTG 042996 ADDED DS-AFPDUEDL-MEMO RAC
      *  JTG 101096 CHANGED DS-SUMWGTTM FROM PIC 9(6) TO  PIC S9(6)
      *  KEC 030797 ***REVISED***
      *              RECORD SIZE: NOW = 2326; WAS = 2150;
      *              REMOVED OLD APPLICATION FIELDS;
      *              ADDED   NEW APPLICATION FIELDS (TABLE).
      *  KEC 051297 ***REVISED***
      *              ADDED DS-NO-MADE.
      *  MJD 070297 ADDED DS-DLDISC-VOL (MERCURY REQ#160)
      *  BLV 091197 NO RECORD CHANGE, BUT DELINQUENCY TABLE ORDER
      *             REVISED & WILL NOW USE ALL 10 BUCKETS
      *  MJD 100897 CHANGED DELINQUENCY TABLE TO OCCUR 10 TIMES INSTEAD OF 18.
      *  MJD 031198 CHANGED DS-SUMWGTTM FROM PIC S9(6) TO  PIC S9(8)
      *             CHANGED DS-NO-REPO TO PIC 9(4)
      *  MJD 051898 ADDED DELINQ TABLE FOR BANKRUPTCY
      *             EXPANDED RECORD SIZE TO 2023 BYTES
      *  JTG 062998 CHANGED DS-CNVT.... TO DS-RNEW....
      *                     DS-SVOS.... TO DS-TODL....
      *  JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      *             EXPANDED RECORD SIZE TO 2136 BYTES
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BLV 990818 OCCURANCES 4 & 5 OF REPO TABLE NOW WILL BE REPO ON-
      *             HAND & SOLD IN P&L RECEIVABLES, USED TO BE P4 ACT
      *             & P4 INACTIVE, NO CHANGE TO RECORD SIZE OR FORMAT
      *  BLV 000321 ADDED UNEARED TABLE, REMOVED OLD UNDEARNED FIELDS,
      *             MERC PR# 223
      *  MJD 000330 ADDED DS-PROCEEDS (MERC PR#223)
      *  KEC 2000.0411 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  GLB 000724  ADD DS-UNPAID-MANUAL MANUAL CHECK VOIDS
      **************************************************************************
       01  DS-REC.
           03  DS1-KEY.
      * DS-DATE = YYYMM        2/23/04 = 10402    10/15/99 = 09910
               05  DS-DATE          PIC 9(5).
               05  DS-BRANCH        PIC 9(4).
               05  DS2-KEY.
                   07  DS-NUM       PIC 9(6).
                   07  DS-NUM-RED  REDEFINES DS-NUM.
                       09  DS-NO    PIC 9(4).
                       09  DS-BR    PIC 9(2).
                   07  DS-DLNO     REDEFINES DS-NUM
                                    PIC 9(4)V99.
                   07  DS-CLASS     PIC 99.
      *---------------------------------------------------------------
           03  DS-DATA.
               05  DS-DLRVPRIN      OCCURS 3
                                    PIC S9(8)V99  COMP.
               05  DS-DLRVCHGS      OCCURS 3
                                    PIC S9(8)V99  COMP.
               05  DS-DLPARVHELD    PIC S9(8)V99  COMP.
               05  DS-UNPAIDCK      PIC S9(8)V99  COMP.
               05  DS-DLDISC        PIC S9(8)V99  COMP.
      *        THIS FIELD  (DS-DLPARVPAID) REPRESENTS TOTAL MONIES
      *        PAID TO DEALER IN PARTICIPATION AT CONTRACT PURCHASE.
               05  DS-DLPARVPAID    PIC S9(8)V99  COMP.
      *---------------------------------------------------------------
               05  DS-SUMWGTTM      PIC S9(8)     COMP.
               05  DS-SUMWGTAV      PIC S9(9)V99  COMP.
               05  DS-SUMWGTTA      PIC S9(11)V99 COMP.

      *---------------------------------------------------------------
      * RECOURSE ACCOUNTS (FULL, PARTIAL & NON)
               05  DS-RCOURSE-UNITS PIC S9(5)     COMP   OCCURS 3.
               05  DS-RCOURSE-AMT   PIC S9(9)V99  COMP   OCCURS 3.
      *---------------------------------------------------------------
      *     VOLUME TABLES:
      *        1 - MTD    [DSMTD]
      *        2 - YTD    [DSYTD]
      *        3 - LYR    [DSLYR]
      *---------------------------------------------------------------
      * NEW PURCHASED VOLUME
               05  DS-NPURUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-NPURAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * OTHER PURCHASED VOLUME
               05  DS-OPURUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-OPURAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * RENEWED ACCOUNT VOLUME
      *      THESE FIELDS REFLECT THE NO. & $DOLLAR NEW VOLUME
      *      OF ALL RENEWED ACCOUNTS WITHER THE RENEWAL WAS:
      *      --DIRECT RENEWED INTO DIRECT
      *      --SALES  RENEWED INTO DIRECT
      *      --SALES  RENEWED INTO SALES      
               05  DS-RNEWUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-RNEWAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * PAID-OFF
               05  DS-PAIDUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-PAIDAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * CONVERTED TO A DIRECT LOAN
      *      THESE FIELDS REFLECT THE NO. & $DOLLARS RENEWED FOR THIS 
      *      SALE FINANCE CLASS THAT WERE PAID OFF AND CONVERTED VIA 'SC' 
      *      TO A DIRECT LOAN CLASS.
               05  DS-TODLUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-TODLAMT       PIC S9(8)V99  COMP   OCCURS 3.

      *---------------------------------------------------------------
      *      NET TABLES:
      *        1 - CURRENT           [DSCUR]
      *        2 - END LAST MONTH    [DSELM]
      *        3 - END LAST YEAR     [DSELY]
      *---------------------------------------------------------------
      * OUTSTANDING
               05  DS-OUTSUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-OUTSAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * CHARGED-OFF (P&L) EXCLUSIVE OF P4'S
               05  DS-PLOSUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-PLOSAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * RECOVERED  P & L EXCLUSIVE OF P4'S WHERE ARE P4 RECOVERIES <BAD>??
               05  DS-RCPLUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-RCPLAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      * JUDGMENTS
               05  DS-JDOSUNIT      PIC S9(5)     COMP   OCCURS 3.
               05  DS-JDOSAMT       PIC S9(8)V99  COMP   OCCURS 3.
 
      *---------------------------------------------------------------
      *     CONTRACTUAL EARNINGS:
      *        1 - CURRENT           [DSCUR]
      *        2 - END LAST MONTH    [DSELM]
      *        3 - END LAST YEAR     [DSELY]
      *---------------------------------------------------------------
               05  DS-DELUNEDT      PIC 9(6)      COMP.
      *------->05  DS-CNEARN        PIC S9(8)V99  COMP   OCCURS 3.
      *------->05  DS-EARNTAKN      PIC S9(8)V99  COMP   OCCURS 3.
      *------->05  DS-EARNDUE       PIC S9(8)V99  COMP   OCCURS 3.
      *------->05  DS-CNUNEARN      PIC S9(8)V99  COMP   OCCURS 3.
      *------->05  DS-ERNOTTAK      PIC S9(8)V99  COMP   OCCURS 3.
      *    ------------------------------------------------------
      *    UNEARNED PERIOD TABLE: (PART 1 REMAINDER OF TABLE LOCATED AT 
      *                                   END OF RECORD)
      *                   ___________________________________________
      *                   1 - CURRENT                |  1 - MTD               
      *                                              |
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME        
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST                
      *                    2 - ADDON
      *                    3 - SERVICE CHARGE    
      *                    4 - MAINTENANCE FEES             
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *    ------------------------------------------------------
               05  DS-UNEARNED-ACTIVITY           OCCURS 2.
                   07  DS-UNEARNED-TYPE           OCCURS 6.
                       09  DS-UNEARNED-AMT        PIC S9(8)V99 COMP.

      *-----------------------------------------------------------------
      * DS-PROCEEDS IS A VOLUME AMOUNT THAT OCCURS MYD/YTD/LYR
      *-----------------------------------------------------------------
               05  DS-PROCEEDS     OCCURS 2       PIC S9(8)V99 COMP.
               05  FILLER                         PIC X(05).

      *-------------------------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE REPO ACCOUNTS ONLY
      *
      *    REPO TABLE:
      *    PERIOD TABLE:  ________________________________________________
      *                   1 - CURRENT        [DSCUR] | 1 - MTD     [DSMTD]
      *                   2 - END LAST MONTH [DSELM] | 2 - YTD     [DSYTD]
      *                   3 - END LAST YEAR  [DSELY] | 3 - LYR     [DSLYR]
      *
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [DSNET]   2 - NEW VOL [DSVOL]
      *
      *    TYPE TABLE  :  1 - REPO ON HAND
      *                   2 - REPO REDEEMED
      *                   3 - REPO SOLD LIVE RECEIVABLE
      *                   4 - REPO ON HAND P&L RECEIVABLE
      *                   5 - REPO SOLD P&L RECEIVABLE
      *
      *-------------------------------------------------------------------
               05  DS-REPO-PERIOD-TBL         OCCURS 3.
                   07  DS-REPO-ACTIVITY-TBL   OCCURS 2.
                       09  DS-REPO-TYPE-TBL   OCCURS 5.
                           11  DS-NO-REPO     PIC S9(4)    COMP.
                           11  DS-AMT-REPO    PIC S9(7)V99 COMP.
      *---------------------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE NON P&L, NON REPO ACCOUNTS
      * 
      *    DS-NOW-BMO-LME
      *        1- NOW   (CURRENT)         [DSNOW]
      *        2- BEGINNING OF MONTH      [DSBMO]
      *        3- LAST MONTH END          [DSLME]
      *
      *    DS-REC-CON
      *        1- RECENCY                 [DSREC]
      *        2- CONTRACTUAL             [DSCON]
      *
      *    DS-DEL-BUCKET
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
      *
      *    NOTE:
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *    -------------------------------------------------
               05  DS-NOW-BMO-LME          OCCURS 3.
                   07  DS-REC-CON          OCCURS 2.
                       09  DS-DEL-BUCKET   OCCURS 10.
      *                DELINQUENT UNITS:                                 
                           11  DS-UNITS    PIC S9(7)    COMP.
      *                OUTSTANDING BALANCE:
                           11  DS-OUTBAL   PIC S9(9)V99 COMP.
      *---------------------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      * 
      *    DS-NOW-BMO-LME-BNKRPT
      *        1- NOW   (CURRENT)         [DSNOW]
      *        2- BEGINNING OF MONTH      [DSBMO]
      *        3- LAST MONTH END          [DSLME]
      *
      *    DS-REC-CON-BNKRPT
      *        1- RECENCY                 [DSREC]
      *        2- CONTRACTUAL             [DSCON]
      *
      *    DS-DEL-BUCKET-BNKRPT
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
      *
      *    NOTE:
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *    -------------------------------------------------
               05  DS-NOW-BMO-LME-BNKRPT        OCCURS 3.
                   07  DS-REC-CON-BNKRPT        OCCURS 2.
                       09  DS-DEL-BUCKET-BNKRPT OCCURS 10.
      *                DELINQUENT UNITS:                                 
                           11  DS-UNITS-BNKRPT  PIC S9(7)    COMP.
      *                OUTSTANDING BALANCE:
                           11  DS-OUTBAL-BNKRPT PIC S9(9)V99 COMP.
    
           03  DS-AFPDUEDL-MEMO            PIC S9(8)V99  COMP.

      *-------------------- APPLICATIONS -------------------------------
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [DSCUR] |  1 - MTD        [DSMTD]
      *                   2 - END LAST MONTH [DSELM] |  2 - YTD        [DSYTD]
      *                   3 - END LAST YEAR  [DSELY] |  3 - LYR        [DSLYR]
      *                                              |
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [DSNET] |  2 - NEW VOLUME [DSVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB
      *                   2 - OTH   +   RENEWALS 
      *                   3 - NOT USD FOR RENEWALS NO ROOM ON SCREENS TO 
      *                       DISPLAY A THIRD LINE
      *
      *    NOTE: STATISTICS FOR:
      *            APPS MADE IS THE ACTUAL LOAN VOLUME CARRIED UNDER BY-NO
      *            APPS NOT TAKEN IS NOT EASILY MAINTAINED
      *         
      *-----------------------------------------------------------------
           03  DS-APP-PERIOD-TBL              OCCURS 3.
               05  DS-APP-ACTIVITY-TBL        OCCURS 2.
                   07  DS-APP-TYPE-TBL        OCCURS 3.
                       09  DS-NO-PENDING      PIC S9(5)    COMP.
                       09  DS-NO-COND-APP     PIC S9(5)    COMP.
                       09  DS-NO-APPROVED     PIC S9(5)    COMP.
                       09  DS-NO-MADE         PIC S9(5)    COMP.
                       09  DS-NO-TURNDOWN     PIC S9(5)    COMP.
                       09  DS-NO-TURNUSDOWN   PIC S9(5)    COMP.

      *-----------------------------------------------------------------
      *    DLDISC-VOL-TBL: 1 - MTD                  NEW VOLUMES
      *                    2 - YTD
      *                    3 - LYR
      *-----------------------------------------------------------------
           03  DS-DLDISC-VOL-TBL                  OCCURS 3.
               05 DS-DLDISC-VOL                   PIC S9(9)V99 COMP.
      *-----------------------------------------------------------------
      *    ------------------------------------------------------
      *    UNEARNED PERIOD TABLE: (PART 2 CURRENT/MTD LOCATED ABOVE)
      *                   ___________________________________________
      *                   1 - END LAST MONTH         |  1 - YTD               
      *                   2 - END LAST YEAR          |  2 - LYR                
      *                                              |
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME        
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST                
      *                    2 - ADDON (1ST PAY EXTENSION CHARGES)
      *                    3 - SERVICE CHARGES              
      *                    4 - MAINTENANCE FEES, OTHER FEES             
      *                    5 - DEALER DISCOUNTS
      *                    6 - OTHER
      *    ------------------------------------------------------
           03  DS-UNEARNED-PERIOD2                OCCURS 2.
               05  DS-UNEARNED-ACTIVITY2          OCCURS 2.
                   07  DS-UNEARNED-TYPE2          OCCURS 6.
                       09  DS-UNEARNED-AMT2       PIC S9(8)V99 COMP.
      * GLB 07/24/00
           03  DS-UNPAID-MANUAL                   PIC S9(8)V99  COMP.
           03  FILLER                             PIC X(7).
      *    03  FILLER                             PIC X(12).
