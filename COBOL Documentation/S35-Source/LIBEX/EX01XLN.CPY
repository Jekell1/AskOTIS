      * COPYMEMBER: LIBEX/EX01XLN
      ********************************************************************
      * LOAN EXTRACTION FILE       3597  BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *  
      ********************************************************************
      * REV:
      *  020715 BAH SIGNED XLN-TRADEIN, REMOVED 1 DIGIT TO MAKE ROOM,
      *             AS PER KEVIN, PR#1808
      *  021004 BLV UPDATED FIELDS FOR REGACC, ADDED & EXPANDED
      *  021007 BLV CHANGE EXPANDED FIELDS BACK TO SMALLER SIZES; KEVIN
      *             SAYS THIS WOULD BE A LOT OF WORK FOR THEM TO CHANGE
      *  040903 CS   ADD XLN-REGION-NUMBER PR#76 REGACC    
      *  050429 BAH ADDED XLN-ACH, REGACC PR# 102
      *  051019 BAH ADDED XLN-UNWIND-REASON-CODE, REGACC PR# 105
      *  051025 BAH RENAMED XLN-NOODDPY-PD-JUNK TO FILLER, NOT USED!
      *  051025 BAH RENAMED LN-LEDGERCNT, NOT USED....
      *  051025 BAH RENAMED LN-LPCNT, NOT USED....
      *  051028 BAH ADDED XLN-FROZEN-REASON, REGACC PR# 114
      *  061205 BAH ADDED XLN-COUPON-STATEMENT, REGACC PR# 158
      *
      *  070406 KEC {PR#00169} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *             TOOK XLN-ZERO-BAL-CNT  [9(3)] AND REDEFINED IT AS
      *                  XLN-NEXIS-CRSCORE [9(3)].
      *                  XLN-NEXIS-CRSCORE IS FOR CMSI.
      *             XLN-ZERO-BAL-CNT IS A REVOLVING FIELD WHICH IS NOT
      *             USED FOR CMSI.
      *
      *  100824 BAH ADDED XLN-NETBAL, REGACC PR# 271
      *  151007 BAH ADDED XLN-TRW-B-CONSUMER-IND,XLN-TRW-J-CONSUMER-IND,
      *             XLN-TRW-COMPLIANCE-CODE,  SOAUTO PL#906
      *  161221 MJD ADDED XNO-DEFERMENT2>XNO-DEFERMENT5 TO END OF
      *             EXTRACTION THIS ADDS 44 BYTES TO THE EXTRACTION(3533)
      *                                                       71225 PR#384
      *  170331 MJD REMOVED XNO-DEFERMENT 2-5 FROM END OF EXTRACT AND 
      *             REPLACED WITH XNO-DEF-AMOUNT (OCCURS 9) TO HOLD 
      *             DF-D9                                        71225#386
      *
      * KEC 2017.0403 {PR#05271} HONOR FINANCE <A15> [DAVID SINCLAIR]
      *        ADDED XXX-TO-ROLL WHICH IS A CALCULATED FIELD (WI/COLAST)
      *
      ********************************************************************
       78  XLN-SIZE                VALUE 3597.
       01  XLN-REC.
           03  XLN-BRACCTNO        PIC 9(10).
           03  XLN-OWNBR           PIC 9(04).
           03  XLN-ACCTNO          PIC 9(06).
           03  XLN-SEQNO           PIC 9(02).
           03  XLN-CLASS           PIC 9(02).
           03  XLN-SUBCLASS        PIC 9(02).                 
           03  XLN-LAWCODE         PIC 9(02).                 
      *----------------------------------------------------
      *    LOAN BORROWERS
      *----------------------------------------------------
           03  XLN-SSNO1           PIC 9(09).
           03  XLN-MAKERCD1        PIC X(01).
           03  XLN-SSNO2           PIC 9(09).
           03  XLN-MAKERCD2        PIC X(01).
           03  XLN-SSNO3           PIC 9(09).
           03  XLN-MAKERCD3        PIC X(01).
           03  XLN-SSNO4           PIC 9(09).
           03  XLN-MAKERCD4        PIC X(01).
      *----------------------------------------------------
      *    LOAN STATUS CODES
      *----------------------------------------------------
           03  XLN-ORGST           PIC X(02).
           03  XLN-ORGBR           PIC 9(04).
           03  XLN-OWNST           PIC X(02).
           03  XLN-SPRCLASS        PIC 9(02).
           03  XLN-MORELN          PIC X(01).
           03  XLN-CONVERCD        PIC X(01).                  
           03  XLN-MIDSTREAM       PIC X(01).                  
           03  XLN-PURCD           PIC X(03).
           03  XLN-SRCD            PIC X(02).
           03  XLN-COLLCD          PIC X(02).
           03  XLN-COLLID          PIC X(20).
           03  XLN-POCD            PIC X(02).
           03  XLN-PLCD            PIC X(01).
           03  XLN-PLTYPE          PIC X(01).
           03  XLN-CONFIDENTCD     PIC X(01).                  
           03  XLN-ALLOTCD         PIC X(01).                  
           03  XLN-RECASTCD        PIC X(01).                  
           03  XLN-SKIP-TRW        PIC X(01).                  
           03  XLN-STATUSFG        PIC X(01).                  
           03  XLN-SMALLOAN        PIC X(01).
           03  XLN-SALEFIN         PIC X(01).
           03  XLN-SERVACCT        PIC X(01).
           03  XLN-LOANTYPE        PIC X(01).
           03  XLN-ADDON-FG        PIC X(01).
           03  XLN-PAY-SCHLD-FG    PIC X(01).
           03  XLN-LC-CODE         PIC 9(02).
           03  XLN-CAN-LOAN-NO     PIC X(02).
           03  XLN-STATFLAGS       PIC X(10).
      *----------------------------------------------------
      *    LOAN DATES
      *----------------------------------------------------
           03  XLN-ENTDATE         PIC 9(08).
           03  XLN-LOANDATE        PIC 9(08).
           03  XLN-PURDATE         PIC 9(08).
           03  XLN-INTDATE         PIC 9(08).
           03  XLN-CASHDATE        PIC 9(08).
           03  XLN-FILEDATE        PIC 9(08).
           03  XLN-ORIG-1STPAY     PIC 9(08).
           03  XLN-1STPYDATE       PIC 9(08).
           03  XLN-JDDATE          PIC 9(08).
           03  XLN-PLDATE          PIC 9(08).
           03  XLN-ACCELDATE       PIC 9(08).
           03  XLN-REDUDATE        PIC 9(08).
           03  XLN-POFFDATE        PIC 9(08).
           03  XLN-LEGALDATE       PIC 9(08).
           03  XLN-BNKRPTDATE      PIC 9(08).
           03  XLN-DEBTAIDATE      PIC 9(08).
           03  XLN-REPODATE        PIC 9(08).
           03  XLN-REDEEMDATE      PIC 9(08).
           03  XLN-SOLDDATE        PIC 9(08).
           03  XLN-INS-CONFIRMATION       PIC 9(08).
      *----------------------------------------------------
      *    RATE INFORMATION
      *----------------------------------------------------
           03  XLN-APRATE          PIC 9(03).999.
           03  XLN-EFFRATE         PIC 9(03).999.
           03  XLN-SMPRATE         PIC 9(03).999.
           03  XLN-YLDRATE         PIC 9(03).999.
           03  XLN-OVRDRATE        PIC 9(03).999.
           03  XLN-ENDORATE        PIC 9(03).999.
           03  XLN-JDRATE          PIC 9(03).999.
           03  XLN-ACCRATE         PIC 9(03).999.
           03  XLN-REDURATE        PIC 9(03).999.
           03  XLN-SERVPER         PIC 9(03).99.
      *----------------------------------------------------
      *    LOAN DISCLOSURE INFORMATION
      *----------------------------------------------------
           03  XLN-SELLPR          PIC 9(06).99.
           03  XLN-DOWNPY          PIC 9(06).99.
           03  XLN-TRADEIN         PIC +9(05).99.
           03  XLN-LNAMT           PIC 9(06).99.
           03  XLN-FINCHG          PIC 9(06).99.
           03  XLN-INTCHG          PIC 9(06).99.
           03  XLN-SERCHG          PIC 9(05).99.
           03  XLN-EXTCHG          PIC 9(04).99.
           03  XLN-MAINTFEE        PIC 9(02).99.
           03  XLN-POINTS          PIC 9(02).99.
           03  XLN-PROCEEDS        PIC +9(06).99.
           03  XLN-ORGTERM         PIC 9(03).
           03  XLN-1STPYAMT        PIC 9(06).99.
           03  XLN-NOREGPY         PIC 9(03).
           03  XLN-REGPYAMT        PIC 9(06).99.
           03  XLN-LASTPYAMT       PIC +9(06).99.
           03  XLN-PROCASH         PIC +9(07).99.
           03  XLN-TILLNO          PIC X(01).
           03  XLN-STATE-FLAG      PIC X(01).
           03  XLN-STATE-WORKER    PIC +9(07).99.
           03  XLN-STATE-WORKER2   PIC +9(05).99.
      *----------------------------------------------------
      *    FEE INFORMATION
      *----------------------------------------------------
           03  XLN-FEE-TAB.
               05  XLN-FEECODE1    PIC X(02).                 
               05  XLN-FEEAMT1     PIC 9(04).99.
               05  XLN-FEEOURS1    PIC X(01).                  
               05  XLN-FEEFINCHG1  PIC X(01).
               05  XLN-FEECODE2    PIC X(02).                 
               05  XLN-FEEAMT2     PIC 9(04).99.
               05  XLN-FEEOURS2    PIC X(01).                  
               05  XLN-FEEFINCHG2  PIC X(01).
               05  XLN-FEECODE3    PIC X(02).                 
               05  XLN-FEEAMT3     PIC 9(04).99.
               05  XLN-FEEOURS3    PIC X(01).                  
               05  XLN-FEEFINCHG3  PIC X(01).
               05  XLN-FEECODE4    PIC X(02).                 
               05  XLN-FEEAMT4     PIC 9(04).99.
               05  XLN-FEEOURS4    PIC X(01).                  
               05  XLN-FEEFINCHG4  PIC X(01).
               05  XLN-FEECODE5    PIC X(02).                 
               05  XLN-FEEAMT5     PIC 9(04).99.
               05  XLN-FEEOURS5    PIC X(01).                  
               05  XLN-FEEFINCHG5  PIC X(01).
               05  XLN-FEECODE6    PIC X(02).                 
               05  XLN-FEEAMT6     PIC 9(04).99.
               05  XLN-FEEOURS6    PIC X(01).                  
               05  XLN-FEEFINCHG6  PIC X(01).
               05  XLN-FEECODE7    PIC X(02).                 
               05  XLN-FEEAMT7     PIC 9(04).99.
               05  XLN-FEEOURS7    PIC X(01).                  
               05  XLN-FEEFINCHG7  PIC X(01).
               05  XLN-FEECODE8    PIC X(02).                 
               05  XLN-FEEAMT8     PIC 9(04).99.
               05  XLN-FEEOURS8    PIC X(01).                  
               05  XLN-FEEFINCHG8  PIC X(01).
               05  XLN-FEECODE9    PIC X(02).                 
               05  XLN-FEEAMT9     PIC 9(04).99.
               05  XLN-FEEOURS9    PIC X(01).                  
               05  XLN-FEEFINCHG9  PIC X(01).
               05  XLN-FEECODE10   PIC X(02).                 
               05  XLN-FEEAMT10    PIC 9(04).99.
               05  XLN-FEEOURS10   PIC X(01).                  
               05  XLN-FEEFINCHG10 PIC X(01).
           03  XLN-FEE-TABLE REDEFINES XLN-FEE-TAB OCCURS 10 TIMES.
               05  XLN-FEECODE     PIC X(02).                 
               05  XLN-FEEAMT      PIC 9(04).99.
               05  XLN-FEEOURS     PIC X(01).                  
               05  XLN-FEEFINCHG   PIC X(01).
      *--------------------------------------------------------
      *    INSURANCE INFORMATION
      *--------------------------------------------------------
           03  XLN-INSURED         PIC X(01).
           03  XLN-INSTYPES        PIC X(01).
           03  XLN-INSELIGIBLE     PIC X(01).                  
           03  XLN-INSURANCE-TAB.
               05  XLN-INSCOMP1      PIC 9(02).                 
               05  XLN-INSOURS1      PIC X(01).
               05  XLN-INSEFF1       PIC 9(08).
               05  XLN-INSEXP1       PIC 9(08).
               05  XLN-INSPREM1      PIC 9(04).99.
               05  XLN-INSCOVR1      PIC 9(06).99.
               05  XLN-INSCOMM1      PIC 9(04).99.
               05  XLN-INSANTICERN1  PIC +9(06).99.
               05  XLN-INSANTICADJ1  PIC +9(06).99.
               05  XLN-INSUFERN1     PIC +9(06).99.
               05  XLN-INSACCERN1    PIC +9(06).99.
               05  XLN-INSEARN-ORDER1      PIC 9(01).
               05  XLN-INSFINCHG1    PIC X(01).
               05  XLN-INSCOMP2      PIC 9(02).                 
               05  XLN-INSOURS2      PIC X(01).
               05  XLN-INSEFF2       PIC 9(08).
               05  XLN-INSEXP2       PIC 9(08).
               05  XLN-INSPREM2      PIC 9(04).99.
               05  XLN-INSCOVR2      PIC 9(06).99.
               05  XLN-INSCOMM2      PIC 9(04).99.
               05  XLN-INSANTICERN2  PIC +9(06).99.
               05  XLN-INSANTICADJ2  PIC +9(06).99.
               05  XLN-INSUFERN2     PIC +9(06).99.
               05  XLN-INSACCERN2    PIC +9(06).99.
               05  XLN-INSEARN-ORDER2      PIC 9(01).
               05  XLN-INSFINCHG2    PIC X(01).
               05  XLN-INSCOMP3      PIC 9(02).                 
               05  XLN-INSOURS3      PIC X(01).
               05  XLN-INSEFF3       PIC 9(08).
               05  XLN-INSEXP3       PIC 9(08).
               05  XLN-INSPREM3      PIC 9(04).99.
               05  XLN-INSCOVR3      PIC 9(06).99.
               05  XLN-INSCOMM3      PIC 9(04).99.
               05  XLN-INSANTICERN3  PIC +9(06).99.
               05  XLN-INSANTICADJ3  PIC +9(06).99.
               05  XLN-INSUFERN3     PIC +9(06).99.
               05  XLN-INSACCERN3    PIC +9(06).99.
               05  XLN-INSEARN-ORDER3      PIC 9(01).
               05  XLN-INSFINCHG3    PIC X(01).
               05  XLN-INSCOMP4      PIC 9(02).                 
               05  XLN-INSOURS4      PIC X(01).
               05  XLN-INSEFF4       PIC 9(08).
               05  XLN-INSEXP4       PIC 9(08).
               05  XLN-INSPREM4      PIC 9(04).99.
               05  XLN-INSCOVR4      PIC 9(06).99.
               05  XLN-INSCOMM4      PIC 9(04).99.
               05  XLN-INSANTICERN4  PIC +9(06).99.
               05  XLN-INSANTICADJ4  PIC +9(06).99.
               05  XLN-INSUFERN4     PIC +9(06).99.
               05  XLN-INSACCERN4    PIC +9(06).99.
               05  XLN-INSEARN-ORDER4      PIC 9(01).
               05  XLN-INSFINCHG4    PIC X(01).
               05  XLN-INSCOMP5      PIC 9(02).                 
               05  XLN-INSOURS5      PIC X(01).
               05  XLN-INSEFF5       PIC 9(08).
               05  XLN-INSEXP5       PIC 9(08).
               05  XLN-INSPREM5      PIC 9(04).99.
               05  XLN-INSCOVR5      PIC 9(06).99.
               05  XLN-INSCOMM5      PIC 9(04).99.
               05  XLN-INSANTICERN5  PIC +9(06).99.
               05  XLN-INSANTICADJ5  PIC +9(06).99.
               05  XLN-INSUFERN5     PIC +9(06).99.
               05  XLN-INSACCERN5    PIC +9(06).99.
               05  XLN-INSEARN-ORDER5      PIC 9(01).
               05  XLN-INSFINCHG5    PIC X(01).
               05  XLN-INSCOMP6      PIC 9(02).                 
               05  XLN-INSOURS6      PIC X(01).
               05  XLN-INSEFF6       PIC 9(08).
               05  XLN-INSEXP6       PIC 9(08).
               05  XLN-INSPREM6      PIC 9(04).99.
               05  XLN-INSCOVR6      PIC 9(06).99.
               05  XLN-INSCOMM6      PIC 9(04).99.
               05  XLN-INSANTICERN6  PIC +9(06).99.
               05  XLN-INSANTICADJ6  PIC +9(06).99.
               05  XLN-INSUFERN6     PIC +9(06).99.
               05  XLN-INSACCERN6    PIC +9(06).99.
               05  XLN-INSEARN-ORDER6      PIC 9(01).
               05  XLN-INSFINCHG6    PIC X(01).
               05  XLN-INSCOMP7      PIC 9(02).                 
               05  XLN-INSOURS7      PIC X(01).
               05  XLN-INSEFF7       PIC 9(08).
               05  XLN-INSEXP7       PIC 9(08).
               05  XLN-INSPREM7      PIC 9(04).99.
               05  XLN-INSCOVR7      PIC 9(06).99.
               05  XLN-INSCOMM7      PIC 9(04).99.
               05  XLN-INSANTICERN7  PIC +9(06).99.
               05  XLN-INSANTICADJ7  PIC +9(06).99.
               05  XLN-INSUFERN7     PIC +9(06).99.
               05  XLN-INSACCERN7    PIC +9(06).99.
               05  XLN-INSEARN-ORDER7      PIC 9(01).
               05  XLN-INSFINCHG7    PIC X(01).
               05  XLN-INSCOMP8      PIC 9(02).                 
               05  XLN-INSOURS8      PIC X(01).
               05  XLN-INSEFF8       PIC 9(08).
               05  XLN-INSEXP8       PIC 9(08).
               05  XLN-INSPREM8      PIC 9(04).99.
               05  XLN-INSCOVR8      PIC 9(06).99.
               05  XLN-INSCOMM8      PIC 9(04).99.
               05  XLN-INSANTICERN8  PIC +9(06).99.
               05  XLN-INSANTICADJ8  PIC +9(06).99.
               05  XLN-INSUFERN8     PIC +9(06).99.
               05  XLN-INSACCERN8    PIC +9(06).99.
               05  XLN-INSEARN-ORDER8      PIC 9(01).
               05  XLN-INSFINCHG8    PIC X(01).
           03  XLN-INSURANCE-TABLE REDEFINES XLN-INSURANCE-TAB
                                   OCCURS 8 TIMES.
               05  XLN-INSCOMP     PIC 9(02).                 
               05  XLN-INSOURS     PIC X(01).
               05  XLN-INSEFF      PIC 9(08).
               05  XLN-INSEXP      PIC 9(08).
               05  XLN-INSPREM     PIC 9(04).99.
               05  XLN-INSCOVR     PIC 9(06).99.
               05  XLN-INSCOMM     PIC 9(04).99.
               05  XLN-INSANTICERN PIC +9(06).99.
               05  XLN-INSANTICADJ PIC +9(06).99.
               05  XLN-INSUFERN    PIC +9(06).99.
               05  XLN-INSACCERN   PIC +9(06).99.
               05  XLN-INSEARN-ORDER      PIC 9(01).
               05  XLN-INSFINCHG   PIC X(01).
           03  XLN-PYMMYY-RECDEL   PIC 9(04).
           03  XLN-MTD-PAYS-RECDEL PIC +9(6).99.
           03  XLN-GIFTNET-EXTR    PIC X(01).
           03  XLN-SHIP-SAME-FG    PIC X(01).
           03  XLN-AD-D-INS-COVR   PIC 9(06).
           03  XLN-TOT-PARTIAL-CHGOFF 
                                   PIC +9(06).99.
           03  XLN-LPO             PIC X(04).
           03  XLN-REP-CD          PIC X(08).
           03  XLN-PROCESS-CD      PIC X(01).
           03  XLN-PRIOR-RN-FOR10  PIC 9(08).
           03  XLN-LAST-RN-FOR10   PIC 9(08).
           03  XLN-WCBC-CREDITLIM  PIC +9(05).
           03  XLN-FOLLOWUP-CD     PIC X(04).
      *----------------------------------------------------
      *    SALES FINANCE PURCHASE INFORMATION
      *----------------------------------------------------
           03  XLN-DLNO            PIC 9(04).99.
           03  XLN-DLPOOL          PIC 9(05).
           03  XLN-DLRECOURSE      PIC X(01).                  
           03  XLN-DLPARTRCAMT     PIC 9(06).99.
           03  XLN-ORIGPURAMT      PIC 9(06).99.
           03  XLN-PURBAL          PIC +9(06).99.
           03  XLN-DLRVPRIN-TAB.
               05  XLN-DLRVPRIN1   PIC 9(06).99.
               05  XLN-DLRVPRIN2   PIC 9(06).99.
               05  XLN-DLRVPRIN3   PIC 9(06).99.
           03  XLN-DLRVPRIN REDEFINES XLN-DLRVPRIN-TAB OCCURS 3
                                   PIC 9(06).99.
           03  XLN-DLRVCHGS-TAB.
               05  XLN-DLRVCHGS1   PIC 9(06).99.
               05  XLN-DLRVCHGS2   PIC 9(06).99.
               05  XLN-DLRVCHGS3   PIC 9(06).99.
           03  XLN-DLRVCHGS REDEFINES XLN-DLRVCHGS-TAB OCCURS 3
                                   PIC 9(06).99.
           03  XLN-DLPARVHELD      PIC 9(06).99.
           03  XLN-DLPARVPAID      PIC 9(06).99.
           03  XLN-DLDISC          PIC 9(06).99.
      *---------------------------------------------------------------------
      *    CURRENT LOAN INFORMATION 
      *---------------------------------------------------------------------
           03  XLN-LCPDTH          PIC 9(08).
           03  XLN-INTPDTH         PIC 9(08).
           03  XLN-CURBAL          PIC +9(06).99.
           03  XLN-INTBAL          PIC +9(05).99.
           03  XLN-LCBAL           PIC +9(05).99.
           03  XLN-OTHBAL          PIC +9(05).99.
           03  XLN-OT2BAL          PIC +9(05).99.
           03  XLN-LCPAID          PIC +9(05).99.
           03  XLN-LCPARTIALS      PIC +9(05).99.
      * STATEMENT COUNTER CONTAINS SEQNO OF LAST PRINTED PAYMENT RECORD.
           03  XLN-STMT-COUNTER    PIC 9(04).
           03  FILLER              PIC X(03).
           03  FILLER              PIC 9(03).
           03  XLN-MONTHEND-BAL    PIC 9(06).99.
           03  XLN-ANTICTERM       PIC 9(3).
           03  XLN-ANTICTBL-TAB.
               05  XLN-ANTICERN1   PIC +9(06).99.
               05  XLN-ANTICADJ1   PIC +9(06).99.
               05  XLN-UFERN1      PIC +9(06).99.
               05  XLN-ACCERN1     PIC +9(06).99.
               05  XLN-EARN-ORDER1 PIC 9(01).
               05  XLN-ANTICERN2   PIC +9(06).99.
               05  XLN-ANTICADJ2   PIC +9(06).99.
               05  XLN-UFERN2      PIC +9(06).99.
               05  XLN-ACCERN2     PIC +9(06).99.
               05  XLN-EARN-ORDER2 PIC 9(01).
               05  XLN-ANTICERN3   PIC +9(06).99.
               05  XLN-ANTICADJ3   PIC +9(06).99.
               05  XLN-UFERN3      PIC +9(06).99.
               05  XLN-ACCERN3     PIC +9(06).99.
               05  XLN-EARN-ORDER3 PIC 9(01).
               05  XLN-ANTICERN4   PIC +9(06).99.
               05  XLN-ANTICADJ4   PIC +9(06).99.
               05  XLN-UFERN4      PIC +9(06).99.
               05  XLN-ACCERN4     PIC +9(06).99.
               05  XLN-EARN-ORDER4 PIC 9(01).
               05  XLN-ANTICERN5   PIC +9(06).99.
               05  XLN-ANTICADJ5   PIC +9(06).99.
               05  XLN-UFERN5      PIC +9(06).99.
               05  XLN-ACCERN5     PIC +9(06).99.
               05  XLN-EARN-ORDER5 PIC 9(01).
               05  XLN-ANTICERN6   PIC +9(06).99.
               05  XLN-ANTICADJ6   PIC +9(06).99.
               05  XLN-UFERN6      PIC +9(06).99.
               05  XLN-ACCERN6     PIC +9(06).99.
               05  XLN-EARN-ORDER6 PIC 9(01).
           03  XLN-ANTICTBL REDEFINES XLN-ANTICTBL-TAB OCCURS 6 TIMES.
               05  XLN-ANTICERN    PIC +9(06).99.
               05  XLN-ANTICADJ    PIC +9(06).99.
               05  XLN-UFERN       PIC +9(06).99.
               05  XLN-ACCERN      PIC +9(06).99.
               05  XLN-EARN-ORDER  PIC 9(01).
           03  XLN-LMO-PRIOR-INTEREST     
                                   PIC +9(06).99.
           03  XLN-PRIOR-UNEARNED  PIC +9(06).99.
           03  XLN-ERNDELQ-STOP    PIC X(01).
           03  XLN-NONACRERN       PIC +9(06).99.
           03  XLN-PRIOR-NONACRERN PIC +9(06).99.
           03  XLN-ERN-IB-PDTH     PIC 9(08).
           03  XLN-ERN-IB-PRINBAL  PIC +9(06).99.
           03  XLN-ERN-IB-INTOWE   PIC +9(06).99.
      *----------------------------------------------------
      *    LOAN TOTALS
      *----------------------------------------------------
           03  XLN-TOTERN          PIC +9(07).99.
           03  XLN-TOTOTH          PIC +9(06).99.
           03  XLN-TOTINT          PIC +9(06).99.
           03  XLN-TOTLCHG         PIC +9(06).99.
           03  XLN-TOTDEF          PIC +9(06).99.
           03  XLN-TOTSERACCT      PIC +9(06).99.
           03  XLN-LCWAIVERS       PIC +9(05).99.
           03  XLN-INTWAIVERS      PIC +9(05).99.
           03  XLN-TOTPAYMNTD      PIC +9(06).99.
           03  XLN-TOTEXCPAYMNTD   PIC +9(06).99.
           03  XLN-TOTNODEF        PIC +9(02).
           03  XLN-YTDNODEF        PIC +9(02).
           03  XLN-TOTNOALDEL      PIC +9(02).
           03  XLN-YTDINTPD        PIC +9(05).99.
           03  XLN-LYRINTPD        PIC +9(05).99.
           03  XLN-TOTINTPD        PIC +9(05).99.
           03  XLN-RECASTINT       PIC 9(06).99.
           03  XLN-RECASTPRIN      PIC 9(06).99.
           03  XLN-TRW-DOFD        PIC 9(08).
      *----------------------------------------------------
      *    REBATE INFORMATION
      *----------------------------------------------------
           03  XLN-REBATE-TAB.
               05  XLN-CL-REBATE   PIC X(01).
               05  XLN-AH-REBATE   PIC X(01).
               05  XLN-PP-REBATE   PIC X(01).
               05  XLN-INT-REBATE  PIC X(01).
               05  XLN-SC-REBATE   PIC X(01).
               05  XLN-MF-REBATE   PIC X(01).
               05  XLN-DF-REBATE   PIC X(01).
               05  XLN-EX-REBATE   PIC X(01).
               05  XLN-O1-REBATE   PIC X(01).
               05  XLN-O2-REBATE   PIC X(01).
               05  XLN-O3-REBATE   PIC X(01).
               05  XLN-O4-REBATE   PIC X(01).
               05  XLN-O5-REBATE   PIC X(01).
           03  XLN-REBATE-TABX     REDEFINES XLN-REBATE-TAB.
               05  XLN-REBATE      PIC X(01) OCCURS 13.
      *----------------------------------------------------
      *    COLLECTION INFORMATION
      *----------------------------------------------------
           03  XLN-COLLECTOR       PIC X(03).               
           03  XLN-ACTIONCD        PIC X(04).               
           03  XLN-ACTIONCD2       PIC X(04).               
           03  XLN-ATCODE          PIC X(03).
           03  XLN-RECODE          PIC X(03).
           03  XLN-PROMID          PIC X(03).
           03  XLN-PROMDATE        PIC 9(08).
           03  XLN-PROMAMT         PIC 9(06).99.
           03  XLN-DATE-PAID-LAST  PIC 9(08).
           03  XLN-AMT-PAID-LAST   PIC 9(06).99.
           03  XLN-LASTCFORMNO     PIC 9(04).
           03  XLN-LASTCFORMDATE   PIC 9(08).
           03  XLN-DELCNT          PIC 99.                 
           03  XLN-DELCNT2         PIC 99.                 
           03  XLN-CREDITLIM       PIC 9(06).
           03  XLN-CRSCORE         PIC +9(03).
      *----------------------------------------------------
      *    SOLICIT INFORMATION
      *----------------------------------------------------
           03  XLN-LASTSFORMNO     PIC 9(04).
           03  XLN-LASTSFORMDATE   PIC 9(08).
           03  XLN-SOLCNT          PIC 99.
           03  XLN-MESSAGE         PIC X(30).
           03  XLN-PROMDATE2       PIC 9(08).
      *----------------------------------------------------
      *    PRIOR LOAN INFORMATION
      *----------------------------------------------------
           03  XLN-OLDACCTNO       PIC 9(08).
           03  XLN-PRLNNO          PIC 9(08).
           03  XLN-PRBAL           PIC +9(06).99.
           03  XLN-PRINTDUE        PIC +9(05).99.
           03  XLN-PRFINCHGRB      PIC +9(05).99.
           03  XLN-PRINSRB         PIC +9(05).99.
           03  XLN-PRPOCD          PIC X(02).                 
           03  XLN-PRPODATE        PIC 9(08).
           03  XLN-PRCONPROFILE    PIC 9(12).
           03  XLN-PRDELINQAMT     PIC +9(07).99.
      *----------------------------------------------------
      *    LOAN ADDON INSURANCE CONSOLIDATED INFORMATION
      *----------------------------------------------------
           03  XLN-SUM-ORIGPURAMT  PIC 9(06).99.
           03  XLN-SUM-LNAMT       PIC 9(06).99.
           03  XLN-SUM-FINCHG      PIC 9(06).99.
           03  XLN-SUM-INTCHG      PIC 9(06).99.
           03  XLN-SUM-ORGTERM     PIC 9(03).
           03  XLN-SUM-REGPYAMT    PIC 9(06).99.
           03  XLN-SUM-EXTCHG      PIC 9(04).99.
           03  XLN-SUM-SERCHG      PIC 9(05).99.
           03  XLN-SUM-MAINTFEE    PIC 9(04).99.
      *----------------------------------------------------
      *    UNIT PERIOD INFORMATION
      *----------------------------------------------------
           03  XLN-UNITPER-CD      PIC X(01).
           03  XLN-UNITPER-FREQ    PIC 9(03).
           03  XLN-DAYS-IN-UNITPER PIC 9(03).
           03  XLN-UNITPER-PER-YEAR       PIC 9(3).9(12).
      *---------------------------------------------------
           03  XLN-REPOCD          PIC X(01).
           03  XLN-BR-TFR-FG       PIC X(01).
           03  XLN-O3INS-DLPROC    PIC 9(06).99.
           03  XLN-INSPREM-DLADJ   PIC +9(05).99.
           03  XLN-PRLNAMT         PIC 9(06).99.
           03  XLN-PRPDUE-DAYS     PIC +9(05).
           03  XLN-PLAMT           PIC 9(06).99.
           03  XLN-MTD-PRIN        PIC +9(06).99.
           03  XLN-MTD-LC          PIC +9(05).99.
           03  XLN-MTD-INT         PIC +9(05).99.
           03  XLN-MTD-OT          PIC +9(05).99.
           03  XLN-MTD-EX          PIC +9(05).99.
           03  XLN-BNKRPT-CHAPTER  PIC 9(02).
           03  XLN-BMO-CON-STATUS  PIC X(01).
           03  XLN-BMO-REC-STATUS  PIC X(01).
           03  XLN-APPNO           PIC 9(06).
           03  XLN-GIFTNET         PIC X(01).
           03  XLN-GIFTNET-OUR-PO  PIC 9(06).
           03  XLN-PROC-DISBURSED  PIC X(01).
           03  FILLER              PIC X(01).
           03  XLN-CONPROFILE-TAB.
               05  XLN-CONPROFILE1 PIC 9(12).
               05  XLN-CONPROFILE2 PIC 9(12).
               05  XLN-CONPROFILE3 PIC 9(12).
               05  XLN-CONPROFILE4 PIC 9(12).
               05  XLN-CONPROFILE5 PIC 9(12).
           03  XLN-CONPROFILE REDEFINES XLN-CONPROFILE-TAB OCCURS 5
                                   PIC 9(12).
           03  XLN-RECPROFILE-TAB.
               05  XLN-RECPROFILE1 PIC 9(12).
               05  XLN-RECPROFILE2 PIC 9(12).
               05  XLN-RECPROFILE3 PIC 9(12).
               05  XLN-RECPROFILE4 PIC 9(12).
               05  XLN-RECPROFILE5 PIC 9(12).
           03  XLN-RECPROFILE REDEFINES XLN-RECPROFILE-TAB OCCURS 5
                                   PIC 9(12).
           03  XLN-TRW-STATUS      PIC X(02).
           03  XLN-ORIGIN          PIC X(04).
           03  XLN-FOLLOWUP-DATE   PIC 9(08).
           03  XLN-PO-TRCD         PIC X(02).
           03  XLN-PRDLDISC-UNEARN PIC +9(07).99.
           03  XLN-YTDNOALDEL      PIC +99.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIELDS SPECIFIC TO REVOLVING LOAN CLASSES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  XLN-REVOLVING       PIC X(01).
           03  XLN-PREV-STMT-DATE  PIC 9(08).
           03  XLN-LAST-STMT-DATE  PIC 9(08).

           03  XLN-ZERO-BAL-CNT    PIC 9(03).
           03  XLN-NEXIS-CRSCORE   REDEFINES XLN-ZERO-BAL-CNT
                                   PIC 9(03).

           03  XLN-ACTIVE-FG       PIC X(01).
           03  XLN-AGEING-AMT-TAB.
               05  XLN-AGEING-AMT1 PIC +9(06).99.
               05  XLN-AGEING-AMT2 PIC +9(06).99.
               05  XLN-AGEING-AMT3 PIC +9(06).99.
               05  XLN-AGEING-AMT4 PIC +9(06).99.
               05  XLN-AGEING-AMT5 PIC +9(06).99.
               05  XLN-AGEING-AMT6 PIC +9(06).99.
               05  XLN-AGEING-AMT7 PIC +9(06).99.
               05  XLN-AGEING-AMT8 PIC +9(06).99.
           03  XLN-AGEING-AMT REDEFINES XLN-AGEING-AMT-TAB OCCURS 8
                                   PIC +9(06).99.
           03  XLN-CYCLE-DATE      PIC 9(08).
           03  XLN-CHARGE-INTEREST-FG     PIC X(01).
           03  XLN-CHARGE-LATECHG-FG      PIC X(01).
           03  XLN-CHARGE-MEMBERSHIP-FG   PIC X(01).
           03  XLN-INDEX-ADJUST-FACTOR-TAB.
               05  XLN-INDEX-ADJUST-FACTOR1 PIC +9(03).999.
               05  XLN-INDEX-ADJUST-FACTOR2 PIC +9(03).999.
           03  XLN-INDEX-ADJUST-FACTOR 
               REDEFINES XLN-INDEX-ADJUST-FACTOR-TAB OCCURS 2
                                   PIC +9(03).999.
           03  XLN-OTHBAL-CL       PIC +9(05).99.
           03  XLN-OTHBAL-AH       PIC +9(05).99.
           03  XLN-OTHBAL-PP       PIC +9(05).99.
           03  XLN-OTHBAL-O1       PIC +9(05).99.
           03  XLN-OTHBAL-MEMBERSHIP      
                                   PIC +9(05).99.
           03  XLN-OTHBAL-NSF      PIC +9(05).99.
           03  XLN-OTHBAL-MISC1    PIC +9(05).99.
           03  XLN-OTHBAL-MISC2    PIC +9(05).99.
           03  XLN-OTHBAL-MISC3    PIC +9(05).99.
           03  XLN-OTHBAL-MISC4    PIC +9(05).99.
           03  XLN-OTHBAL-MISC5    PIC +9(05).99.
           03  XLN-OTHBAL-MISC6    PIC +9(05).99.
      * 2898
           03  XLN-RCURPRIN        PIC +9(06).99.
           03  XLN-10DAY-PAYOFF REDEFINES XLN-RCURPRIN PIC +9(06).99.
           03  XLN-RCURINT         PIC +9(06).99.
           03  XLN-RCURLC          PIC +9(06).99.
           03  XLN-RCUROTH         PIC +9(06).99.
           03  XLN-RPDUEPRIN       PIC +9(06).99.
           03  XLN-RPDUEINT        PIC +9(06).99.
           03  XLN-RPDUELC         PIC +9(06).99.
           03  XLN-RPDUEOTH        PIC +9(06).99.
      *-----------------------------------------------------------------
           03  XLN-ACCT-FROZEN     PIC X(01).
           03  XLN-LAST-ACCRUAL-DATE      PIC 9(08).
           03  XLN-TRW-SPECIAL-CODE       PIC X(02).
           03  XLN-CREATION-STATUS PIC X(01).
           03  XLN-TOTNODEFACT     PIC +9(02).
           03  XLN-YTDNODEFACT     PIC +9(02).
           03  XLN-TOTNOLCHG       PIC +9(03).
           03  XLN-PRIOR-STMT-COUNTER     PIC 9(04).
           03  XLN-CONRATE         PIC 9(03).999.
           03  XLN-LBOX            PIC X(01).
           03  XLN-EXTRA-PRIN-LFP  PIC +9(07).99.
           03  XLN-REMAIN-INT-SINCE-LFP   
                                   PIC +9(07).99.
           03  XLN-REMAIN-PRIN-SINCE-LFP  
                                   PIC +9(07).99.
      *-----------------------------------------------------------------
           03  XLN-INTERIM-COUPON  PIC X(01).
      *-----------------------------------------------------------------
      *  VENDOR APP # FROM MAFILE:
      *-----------------------------------------------------------------
           03  XLN-VENDOR-APPNO    PIC 9(08).
      *-----------------------------------------------------------------
      *  NOTICE FIELDS:
      *-----------------------------------------------------------------
           03  XNO-TDUE            PIC 9(08).
           03  XNO-TDUELC          PIC +9(07).99.
           03  XNO-TDUEAMT         PIC +9(07).99.
           03  XNO-TDUEPRIN        PIC +9(07).99.
           03  XNO-TDUETBAL        PIC +9(07).99.
           03  XNO-TDUEPOFF        PIC +9(07).99.
           03  XNO-LDUE            PIC 9(08).
           03  XNO-LDUELC          PIC +9(07).99.
           03  XNO-LDUEAMT         PIC +9(07).99.
           03  XNO-DATE-PAID-LAST  PIC 9(08).
           03  XNO-AMT-PAID-LAST   PIC +9(07).99.
           03  XNO-LSTLPKEY        PIC X(12).
           03  XNO-NXDUE           PIC 9(08).
           03  XNO-NXDUEAMT        PIC +9(07).99.
           03  XNO-NXDUELC         PIC +9(07).99.
           03  XNO-TRUCONTR        PIC 9(04).
           03  XNO-RECENCY         PIC 9(04).
           03  XNO-PDTH            PIC 9(08).
           03  XNO-LCPDTH          PIC 9(08).
           03  XNO-PRINTED         PIC X.
           03  XNO-LCHG-INTDUE     PIC +9(07).99.
           03  XNO-AVAILCREDIT     PIC +9(07).99.
           03  XNO-TDUEDELINQ      PIC +9(07).99.
           03  XNO-DEFERMENT       PIC +9(07).99.
           03  XNO-TRUE-DUE-DATE   PIC 9(08).
           03  XNO-SCHED-PAYMENT   PIC +9(07).99.
           03  XNO-TDUECONTR       PIC +9(07).99.
           03  XNO-LDUECONTR       PIC +9(07).99.
           03  XNO-NXDUECONTR      PIC +9(07).99.
           03  XNO-CURRENT-AVAIL   PIC +9(07).99.
           03  XNO-LATE-CHG        PIC +9(07).99.
           03  XNO-WAGE-ASSIGN-DELINQ 
                                   PIC +9(07).99.
           03  XNO-PAST-DUE-DAYS   PIC 9(04).
           03  XLN-ORIG-MATURE-DATE PIC 9(08).
           03  XLN-UPD-MATURE-DATE PIC 9(08).

      * UPDATED FIELDS FOR REGACC, 10/4/02, BLV:
           03  XLN-MIP-FG                PIC X.
           03  XLN-MIP-MTD-CLAHPAID      PIC +9(5).99.
           03  XLN-MIP-CLPDTH            PIC 9(8).
           03  XLN-MIP-TOT-CLPAID        PIC +9(7).99.
           03  XLN-MIP-AHPDTH            PIC 9(8).
           03  XLN-MIP-TOT-AHPAID        PIC +9(7).99.
           03  XLN-RESCISSION            PIC X(1).
           03  XLN-PLREASON              PIC X(2).
           03  XLN-LAST-PYPA-DATE        PIC 9(8).
           03  XLN-PROMDAYS-PASTDUE      PIC +9(3).
           03  XLN-DLCOMM-O3-CMSI        PIC 9(4).99.
           03  XLN-DLCOMM-PP-CMSI        PIC 9(4).99.
           03  XLN-REGION-NUMBER         PIC 9(2).
           03  XLN-ACH                   PIC X.
           03  XLN-UNWIND-REASON-CODE    PIC XX.
           03  XLN-FROZEN-REASON         PIC XX.
           03  XLN-COUPON-STATEMENT      PIC X.
           03  XLN-NETBAL                PIC +9(7).99.
      * 10/7/15 SOAUTO
           03  XLN-TRW-B-CONSUMER-IND    PIC X(2).
           03  XLN-TRW-J-CONSUMER-IND    PIC X(2).
           03  XLN-TRW-COMPLIANCE-CODE   PIC X(2).
           03  XLN-UNUSED          PIC X(4).
           03  XNO-ALT-DEF-AMOUNT        PIC +9(07).99 OCCURS 9.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    XXX-TO-ROLL IS A CALCULATED FIELD (WI/COLAST) PR#5271
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  XXX-TO-ROLL               PIC 9(6).99.

           03  XLN-CR              PIC X(01).
           03  XLN-LF              PIC X(01).
