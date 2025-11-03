      * COPYMEMBER: LIBLP/LP01WK
      **************************************************************************
      * USED IN WI/LTSCAN.C
      *                                                
      *          LOAN PROCESSING WORK RECORD    (RSZ=2190)
      *                                                
      *  WK-KEY =                                      
      *         =    1  SALES FINANCE INPUT            
      *         =    2  DIRECT LOAN INPUT              
      *         =    3  TRANSACTION POSTING            
      *         =    4  CHECK DISBURSEMENT (DIRECT)    
      *         =    5  SALES FINANCE MODIFY           
      *         =    6  COLLATERAL TRAILER             
      *         =    7  DIRECT LOAN MODIFY             
      *         =    8  ADDONS
      *         =    9  ADDONS (USED TO HOLD ORIGINAL LOAN RECORD)
      * REV:
      *  BAH 061996 ADDED WK-ADR1 WK-ADR2 WK-CITY WK-STATE 
      *             WK-ZIP TO PASS DEALER ADDRESS TO PRINT ON CHECK
      *  JTG 021197 RESERVED WK-KEY = 8 & 9 FOR ADDONS
      *  BLK 081997 EXPANDED WK-SAVE-REC TO MATCH LN-REC
      *  MJD 101797 ADDED WK-UPDATE-BUF. WAS PREVIOUSLY DEFINED IN EACH LONP*
      *  JTG 112097 EXPANDED WK-SAVE-REC BY 230 TO 1800 TO MATCH LN-REC
      *  MJD 112597 EXPANDED LX-REC TO 73 BYTES (ADDED 10)
      *  MJD 060498 EXPANDED LP-REC TO 239 BYTES
      *  BLV 030225 DEFINE APPROVAL LIMIT & CREDIT LIMIT & THEIR USER ID'S
      *             THESE ARE IN PD BEYOND LOAN RECORD PART; NEED APPROVAL
      *             INFO TO WRITE MEMO IN LONPC2, LONPB2, WORLD PR# 315
      *  JTG 051004 ADDED WK-UNWIND-REASON-CD AND WK-UNWIND-DRAWER
      *             LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *                   AND LN-UNWIND-REASON-CODE                 REGACC #0105
      *
      *  KEC 060621 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *             ADDED "03  WK-LT-A-RECORD".
      *  BAH 100128 ADDED WK-FREE-TYPE-3 FOR UNITED ONLY TO ENTER ANOTHER
      *             LINE OF COLLATERAL IN LTCREA, UNITED PR# 3524
      *  BAH 180301 ADDED WK-DEBIT-CARD-NUMBER WORLD #1306
      **************************************************************************
       01  WK-REC.
           03  WK-ELE                    PIC 99.
           03  WK-OLD-PROG-FORM          PIC X(3).
      *-------------------------------------------------
      *    WK-ELE AND WK-OLD-PROG-FORM ARE COMMON TO ALL
      *    PROGRAMS; WK-SAVE-RECORD MAY BE REDEFINED.
      *-------------------------------------------------
      * WK-SAVE-RECORD SIZE 2185
           03  WK-SAVE-RECORD.
               05  WK-SAVE-REC           PIC X(1800).
               05  WK-MEMO.
                   07  WK-MSG1           PIC X(30).
                   07  WK-MSG2           PIC X(30).
               05  WK-DLPAID             PIC X.
               05  WK-CKNO               PIC 9(6)    COMP-3.
               05  WK-FREE1              PIC X(70).
               05  WK-FREE2              PIC X(70).
               05  FILLER                PIC X(24).
0105           05  WK-UNWIND-REASON-CD   PIC XX.
               05  WK-UNWIND-DRAWER      PIC XX.
               05  WK-APPROVAL-LIMIT     PIC 9(7)    COMP.
               05  WK-APPROVAL-USERID    PIC X(3).
               05  WK-MIN-CRED-SCORE     PIC 9(4)    COMP.
               05  WK-CRED-SCORE-USERID  PIC X(3).
               05  FILLER                PIC X(140).

      *************************************************************
      *    REDEFINITION FOR PASSING LTFILE INFO FROM 
      *    ADDPROGED PROGRAM TO LOAN INPUT PROGRAM.
      *************************************************************
           03  WK-LT-RECORD             REDEFINES WK-SAVE-RECORD.
               05  WK-FREE-TYPE-1        PIC X(80).
               05  WK-FREE-1            REDEFINES WK-FREE-TYPE-1.
                   07  WK-BUF-1          PIC X OCCURS 80.
               05  WK-FREE-1X           REDEFINES WK-FREE-TYPE-1.
                   07  WK-FREE-TYPE1A    PIC X(20).
                   07  WK-FREE-TYPE1B    PIC X(20).
                   07  WK-FREE-TYPE1C    PIC X(20).
                   07  WK-FREE-TYPE1D    PIC X(20).
                   
               05  WK-FREE-TYPE-2        PIC X(80).
               05  WK-FREE-2X           REDEFINES WK-FREE-TYPE-2.
                   07  WK-FREE-TYPE2A    PIC X(20).
                   07  WK-FREE-TYPE2B    PIC X(20).
                   07  WK-FREE-TYPE2C    PIC X(20).
                   07  WK-FREE-TYPE2D    PIC X(20).
               05  WK-AUTO-1.
                   07  WK-AUTO-ALP-1.
                       09  WK-DESCRIPTION-1  PIC X(10).
                       09  WK-MAKE-1         PIC X(15).
                       09  WK-MODEL-1        PIC X(15).
                       09  WK-YEAR-1         PIC X(4).
                       09  WK-STYLE-1        PIC X(8).
                       09  WK-CLASS-1        PIC X(8).
                       09  WK-ID-1           PIC X(20).
                       09  WK-TITLE-NO-1     PIC X(8).
                   07  WK-MILEAGE-1          PIC S9(7)    COMP-3.
                   07  WK-VALUE-1            PIC S9(5)V99 COMP-3.
                   07  WK-SELLPR-1           PIC S9(5)V99 COMP-3.
                   07  WK-MISC-AMT-1         PIC S9(5)V99 COMP-3.
                   07  FILLER                PIC X.
               05  WK-AUTO-2.
                   07  WK-AUTO-ALP-2.
                       09  WK-DESCRIPTION-2  PIC X(10).
                       09  WK-MAKE-2         PIC X(15).
                       09  WK-MODEL-2        PIC X(15).
                       09  WK-YEAR-2         PIC X(4).
                       09  WK-STYLE-2        PIC X(8).
                       09  WK-CLASS-2        PIC X(8).
                       09  WK-ID-2           PIC X(20).
                       09  WK-TITLE-NO-2     PIC X(8).
                   07  WK-MILEAGE-2          PIC S9(7)    COMP-3.
                   07  WK-VALUE-2            PIC S9(5)V99 COMP-3.
                   07  WK-SELLPR-2           PIC S9(5)V99 COMP-3.
                   07  WK-MISC-AMT-2         PIC S9(5)V99 COMP-3.
                   07  FILLER                PIC X.

               05  WK-FREE-TYPE-3            PIC X(80).
               05  WK-FREE-3X           REDEFINES WK-FREE-TYPE-3.
                   07  WK-FREE-TYPE3A    PIC X(20).
                   07  WK-FREE-TYPE3B    PIC X(20).
                   07  WK-FREE-TYPE3C    PIC X(20).
                   07  WK-FREE-TYPE3D    PIC X(20).

               05  FILLER                    PIC X(1735).

      *************************************************************
      *    REDEFINITION FOR PASSING LTFILE INFO FROM 
      *    ADDPROGED PROGRAM TO LOAN INPUT PROGRAM.
      *************************************************************
           03  WK-LT-A-RECORD                  REDEFINES WK-SAVE-RECORD.
               05  WK-LT-A-RECORD1.
                   07  WK-LT-A-DLR-REQ         PIC X.
                   07  WK-LT-A-INS-TYPE        PIC X.
                   07  WK-LT-A-INS-CO          PIC X(25).
                   07  WK-LT-A-POLICY-NO       PIC X(25).
                   07  WK-LT-A-AGENCY          PIC X(25).
                   07  WK-LT-A-AGENT           PIC X(25).
                   07  FILLER                  PIC X(33).
               05  WK-LT-A-RECORD2.
                   07  WK-LT-A-ADR1            PIC X(25).
                   07  WK-LT-A-ADR2            PIC X(25).
                   07  WK-LT-A-CITY            PIC X(15).
                   07  WK-LT-A-STATE           PIC XX.
                   07  WK-LT-A-ZIP             PIC X(10).
                   07  WK-LT-A-PHONE           PIC 9(10)    COMP-3.
                   07  WK-LT-A-EFF-DATE        PIC 9(6)     COMP-3.   
                   07  WK-LT-A-EXP-DATE        PIC 9(6)     COMP-3.   
                   07  WK-LT-A-CAN-DATE        PIC 9(6)     COMP-3.   
                   07  WK-LT-A-CONF-DATE       PIC 9(6)     COMP-3.   
                   07  WK-LT-A-LAST-FORM       PIC 9(3)     COMP-3.   
                   07  FILLER                  PIC X(34).
               05  WK-LT-A-RECORD3.
                   07  WK-LT-A-COV-TYPE        PIC X.
                   07  FILLER                  PIC X(24).
                   07  WK-LT-A-BIL-COV         PIC S9(7)V99 COMP-3.   
                   07  WK-LT-A-PDL-COV         PIC S9(7)V99 COMP-3.   
                   07  WK-LT-A-PIP-COV         PIC S9(7)V99 COMP-3.   
                   07  WK-LT-A-UMB-COV         PIC S9(7)V99 COMP-3.   
                   07  WK-LT-A-COL-COV         PIC S9(7)V99 COMP-3.   
                   07  WK-LT-A-LOSS-PAYEE      PIC X(25).
                   07  WK-LT-A-COM-COV         PIC S9(7)V99 COMP-3.   
                   07  FILLER                  PIC X(55).
               05  FILLER                    PIC X(1780).

      *************************************************************
      *    REDEFINITION FOR PASSING LTFILE INFO FROM 
      *    ADDPROGED PROGRAM TO LOAN INPUT PROGRAM.
      *************************************************************
           03  WK-CHECK-RECORD              REDEFINES WK-SAVE-RECORD.
               05  WK-STATUS                 PIC X.
               05  WK-PAYEE                  PIC X(30).
               05  WK-PYAMT                  PIC 9(6)V99.
               05  WK-CKNUM                  PIC 9(6).
               05  WK-BWPROC                 PIC X.
               05  WK-CK-FREE1               PIC X(70).
               05  WK-CK-FREE2               PIC X(70).
               05  WK-ADR1                   PIC X(25).
               05  WK-ADR2                   PIC X(25).
               05  WK-CITY                   PIC X(15).
               05  WK-STATE                  PIC XX.
               05  WK-ZIP                    PIC X(10).
               05  WK-DEBIT-CARD-NUMBER      PIC 9(10).
               05  FILLER                    PIC X(1912).
      *************************************************************
      *    REDEFINITION FOR PAYMENT POSTING
      *************************************************************
           03  WK-UPDATE-BUF     REDEFINES WK-SAVE-RECORD.
               05  FILLER         PIC X(1800).
               05  WK-LP-REC      PIC X(239).
               05  WK-LX-REC      PIC X(73).
               05  WK-LX2-REC     PIC X(73).
      *************************************************************
      *    USED IN LP/LONPD0 AND LP/SFDISB.C
      *************************************************************
           03  WK-SAVE-TR1-KEY   REDEFINES WK-SAVE-RECORD.
               05  WK-TR1-DAY     PIC 99       COMP.
               05  WK-TR1-BRNO    PIC 9(4)     COMP.
               05  WK-TR1-CODE    PIC X.
               05  WK-TR1-CLASS   PIC 99       COMP.
               05  WK-TR1-SUBKEY  PIC X(9).
               05  FILLER         PIC X(2171).
