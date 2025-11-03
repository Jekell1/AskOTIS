      * COPYMEMBER: LIBLP/LP01WK
      **************************************************************************
      * USED IN WI/LTSCAN.C, LONPD0.C
      *
      *          LOAN PROCESSING WORK RECORD  
      *
      *  WK-KEY =
      *         =    1  SALES FINANCE INPUT
      *         =    2  DIRECT LOAN INPUT
      *         =    3  TRANSACTION POSTING
      *         =    4  CHECK DISBURSEMENT (DIRECT)
      *         =    5  SALES FINANCE MODIFY
      *         =    5  WK-SAVE-RECORD HOLDS CI-REC (LONPC2)
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
      *             ADDED "03  WK-LTA-RECORD".
      *  BAH 100128 ADDED WK-FREE-TYPE-3 FOR UNITED ONLY TO ENTER ANOTHER
      *             LINE OF COLLATERAL IN LTCREA, UNITED PR# 3524
      *   CS 160215 CHANGED WK-LP-REC TO LP-SIZE, WK-LX-REC TO USE LX-SIZE
      *             SAVED VERSION LP01WK.160215
      *             CHANGED SIZE OF WK-REC TO 2815            
      *   CS 160216 SINCE MOST PROGRAMS DONT HAVE THE LP, HARD-CODED THE 
      *             SIZE FOR WK-LP-REC, WK-LX-REC
      *  BLM 160831 ADDED WK-LN-LTR-REC & WK-PMT-LTR-REC SINCE REVOLVING
      *             FIELDS NO LONGER IN LN-REC.  INCREASE FILLER &
      *             ADJUSTED FILLER TO MATCH IN ALL REDEFS, PD0003
      *  BAH 171128 ADDED WK-LXE-REC BACK INTO THIS RECORD FOR THE EARNINGS
      *             IT WAS WK-LX2-REC IN A15..TO PASS THE LXE STUFF. CINDY/BETH
      *  BAH 171129 REMOVED WK-LXE AND PUT BACK INTO OWN COPY MEMBER, THE
      *             EARNINGS GET WIPED OUT WHEN WK-FILE IS READ IN LONPF PROGS
      *  BAH 180123 CHANGED WK-ADR1, WK-ADR2, ANC WK-CITY TO X(30)
      *  BAH 180215 ADDED WK-DEBIT-CARD-NUMBER WORLD #1306
      *             INCREASED WK-CKNUM FROM 9(6) TO 9(10)
      *  BAH 180418 REMOVED WK-PMT-LTR-REC AND MOVED WK-LN-LTR-REC DOWN TO
      *             WHERE WK-PMT-LTR-REC WAS, IT WAS WIPING OUT THE SAVED LXE
      *             RECORD HAVING IT UP IN THE WK-SAVE-RECORD AREA!
      *  BAH 190116 CHANGED SIZES ON LX-REC AND LXE-REC
      *  BAH 20190703 REMOVED WK-LN-LTR-REC, REVOLVING
      *  BAH 20211123 REMOVED WK-DEBIT-CARD-NUMBER #1527
      *  BAH 20230612 ADJUSTED ALL THE SIZED FOR ADDING FILLER
      *  BAH 20240109 ADDED SECOND BENEFICIARY INFO #1637
      **************************************************************************
      * WK-REC (4805) WAS (4105)
       01  WK-REC.
           03  WK-ELE                    PIC 99.
           03  WK-OLD-PROG-FORM          PIC X(3).
      *-------------------------------------------------
      *    WK-ELE AND WK-OLD-PROG-FORM ARE COMMON TO ALL
      *    PROGRAMS; WK-SAVE-RECORD MAY BE REDEFINED.
      *******************************************************
      *DO NOT USE LP-SIZE OR LX-SIZE AS SEVERAL PROGRAMS DO NOT
      *HAVE IN THEM. HARD CODE THE VALUES
      ******************************************************
      * WK-SAVE-RECORD (4800)
           03  WK-SAVE-RECORD.
      * WK-SAVE-REC SHOULD ALWAYS BE SAME SIZE AS LN-REC AND PD-REC
               05  WK-SAVE-REC           PIC X(2800).

      ***************************************************************
      * THIS PORTION MUST MAP TO PD-REST-OF-REC (SIZE 497)
               05  WK-MEMO.
                   07  WK-MSG1           PIC X(30).
                   07  WK-MSG2           PIC X(30).
               05  WK-DLPAID             PIC X.
               05  WK-CKNO               PIC 9(6)    COMP-3.
               05  WK-FREE1              PIC X(70).
               05  WK-FREE2              PIC X(70).
               05  FILLER                PIC X(44).
               05  WK-POSTING-BR         PIC 9(4)    COMP-3.
               05  WK-OTHBR-FG           PIC X.
               05  WK-APPROVAL-LIMIT     PIC 9(7)    COMP-3.
               05  WK-APPROVAL-USERID    PIC X(3).
               05  WK-MIN-CRED-SCORE     PIC 9(4)    COMP-3.
               05  WK-CRED-SCORE-USERID  PIC X(3).
               05  WK-SECOND-BEN-NAME    PIC X(32).
               05  WK-SECOND-BEN-ADR     PIC X(32).
               05  WK-SECOND-BEN-CITY    PIC X(16).
               05  WK-SECOND-BEN-STATE   PIC X(2).
               05  WK-SECOND-BEN-ZIP     PIC X(10).
               05  WK-SECOND-BEN-PHONE   PIC 9(10)   COMP.
               05  WK-SECOND-BEN-RELATION PIC X(15).
               05  FILLER                PIC X(119).

      ***************************************************************
      * WK-UNWIND ONLY USED IN LONPJ0 AND LONPJ2 TO PASS FIELDS
               05  WK-UNWIND-REASON-CD   PIC XX.
               05  WK-UNWIND-DRAWER      PIC 99.
               05  FILLER                PIC X(1499).

      *************************************************************
      *    REDEFINITION FOR PASSING LTFILE INFO FROM INPUT PROGRAM
      *     TO LOAN INPUT PROGRAM.
      *************************************************************
      * THIS WK-LT-RECORD MUST FIT IN PC-LTC-RECORD!!!! 
      * PC-LTC-RECORD IS 1000 BYTES, BUT ONLY USING PC-LTC-COLLATERAL-INFO (720)
      * NOT USING ANY OF THE FILLER AS INDIVIDUAL MOVES ARE DONE

      * PC-LTC-COLLATERAL-INFO =  720
      * WK-LT-RECORD  = 4800

           03  WK-LT-RECORD REDEFINES WK-SAVE-RECORD.
               05  WK-HHGS-1             PIC X(80).
               05  WK-HHGSX-1            REDEFINES WK-HHGS-1.
                   07  WK-HHGS-BUF-1     PIC X OCCURS 80.
               05  WK-HHGS-1X            REDEFINES WK-HHGS-1.
                   07  WK-HHGS-1A        PIC X(20).
                   07  WK-HHGS-1B        PIC X(20).
                   07  WK-HHGS-1C        PIC X(20).
                   07  WK-HHGS-1D        PIC X(20).

               05  WK-HHGS-2             PIC X(80).
               05  WK-HHGS-2X            REDEFINES WK-HHGS-2.
                   07  WK-HHGS-2A        PIC X(20).
                   07  WK-HHGS-2B        PIC X(20).
                   07  WK-HHGS-2C        PIC X(20).
                   07  WK-HHGS-2D        PIC X(20).

               05  WK-HHGS-3            PIC X(80).
               05  WK-HHGS-3X           REDEFINES WK-HHGS-3.
                   07  WK-HHGS-3A        PIC X(20).
                   07  WK-HHGS-3B        PIC X(20).
                   07  WK-HHGS-3C        PIC X(20).
                   07  WK-HHGS-3D        PIC X(20).

               05  WK-AUTO-1.
                   07  WK-AUTO-ALP-1.
                       09  WK-DESC-1         PIC X(15).
                       09  WK-MAKE-1         PIC X(15).
                       09  WK-MODEL-1        PIC X(15).
                       09  WK-YEAR-1         PIC X(4).
                       09  WK-STYLE-1        PIC X(8).
                       09  WK-CLASS-1        PIC X(8).
                       09  WK-VIN-1          PIC X(20).
                       09  WK-TITLE-NO-1     PIC X(20).
                   07  WK-MILEAGE-1          PIC S9(7)    COMP-3.
                   07  WK-VALUE-1            PIC S9(7)V99 COMP-3.
                   07  WK-SELLPR-1           PIC S9(7)V99 COMP-3.
                   07  WK-MISC-AMT-1         PIC S9(7)V99 COMP-3.
                   07  WK-SENT-FLAG-1        PIC X.
                   07  WK-VIN-CHG-DATE-1     PIC 9(8)     COMP-3.
                   07  FILLER                PIC X(30).
               05  WK-AUTO-2.
                   07  WK-AUTO-ALP-2.
                       09  WK-DESC-2         PIC X(15).
                       09  WK-MAKE-2         PIC X(15).
                       09  WK-MODEL-2        PIC X(15).
                       09  WK-YEAR-2         PIC X(4).
                       09  WK-STYLE-2        PIC X(8).
                       09  WK-CLASS-2        PIC X(8).
                       09  WK-VIN-2          PIC X(20).
                       09  WK-TITLE-NO-2     PIC X(20).
                   07  WK-MILEAGE-2          PIC S9(7)    COMP-3.
                   07  WK-VALUE-2            PIC S9(7)V99 COMP-3.
                   07  WK-SELLPR-2           PIC S9(7)V99 COMP-3.
                   07  WK-MISC-AMT-2         PIC S9(7)V99 COMP-3.
                   07  WK-SENT-FLAG-2        PIC X.
                   07  WK-VIN-CHG-DATE-2     PIC 9(8)     COMP-3.
                   07  FILLER                PIC X(30).
               05  WK-AUTO-3.
                   07  WK-AUTO-ALP-3.
                       09  WK-DESC-3         PIC X(15).
                       09  WK-MAKE-3         PIC X(15).
                       09  WK-MODEL-3        PIC X(15).
                       09  WK-YEAR-3         PIC X(4).
                       09  WK-STYLE-3        PIC X(8).
                       09  WK-CLASS-3        PIC X(8).
                       09  WK-VIN-3          PIC X(20).
                       09  WK-TITLE-NO-3     PIC X(20).
                   07  WK-MILEAGE-3          PIC S9(7)    COMP-3.
                   07  WK-VALUE-3            PIC S9(7)V99 COMP-3.
                   07  WK-SELLPR-3           PIC S9(7)V99 COMP-3.
                   07  WK-MISC-AMT-3         PIC S9(7)V99 COMP-3.
                   07  WK-SENT-FLAG-3        PIC X.
                   07  WK-VIN-CHG-DATE-3     PIC 9(8)     COMP-3.
                   07  FILLER                PIC X(30).

               05  FILLER                    PIC X(4080).

      *************************************************************
      *    REDEFINITION FOR PASSING LTFILE INFO FROM
      *    ADDED PROGRAM TO LOAN INPUT PROGRAM.
      *************************************************************
      * THIS WK-LTA-RECORD MUST FIT IN PC-LTA-RECORD!!!!
      * MOVES ARE DONE!

      * PC-LTA-AUTO-RECORD =  480 WAS 302
      * WK-LTA-RECORD      = 4800

           03  WK-LTA-RECORD              REDEFINES WK-SAVE-RECORD.
               05  WK-LTA-DLR-REQ         PIC X.
               05  WK-LTA-INS-TYPE        PIC X.
               05  WK-LTA-INS-CO          PIC X(25).
               05  WK-LTA-POLICY-NO       PIC X(25).
               05  WK-LTA-AGENCY          PIC X(25).
               05  WK-LTA-AGENT           PIC X(25).
      * 103
               05  WK-LTA-ADR1            PIC X(25).
               05  WK-LTA-ADR2            PIC X(25).
               05  WK-LTA-CITY            PIC X(15).
               05  WK-LTA-STATE           PIC XX.
               05  WK-LTA-ZIP             PIC X(10).
      * 180
               05  WK-LTA-PHONE           PIC 9(10)    COMP-3.
               05  WK-LTA-EFF-DATE        PIC 9(8)     COMP-3.
               05  WK-LTA-EXP-DATE        PIC 9(8)     COMP-3.
               05  WK-LTA-CAN-DATE        PIC 9(8)     COMP-3.
               05  WK-LTA-CONF-DATE       PIC 9(8)     COMP-3.
               05  WK-LTA-LAST-FORM       PIC 9(4)     COMP-3.
      * 209
               05  WK-LTA-COV-TYPE        PIC X.
               05  WK-LTA-BIL-COV         PIC S9(7)V99 COMP-3.
               05  WK-LTA-PDL-COV         PIC S9(7)V99 COMP-3.
               05  WK-LTA-PIP-COV         PIC S9(7)V99 COMP-3.
               05  WK-LTA-UMB-COV         PIC S9(7)V99 COMP-3.
               05  WK-LTA-COL-COV         PIC S9(7)V99 COMP-3.
               05  WK-LTA-LOSS-PAYEE      PIC X(25).
               05  WK-LTA-COM-COV         PIC S9(7)V99 COMP-3.
               05  WK-LTA-TRACK-ONLY      PIC X.
               05  WK-LTA-INSURANCE       PIC X.
               05  WK-LTA-TRACK-MONTHS    PIC X(12).
               05  WK-LTA-INS-MONTHS      PIC X(12).
               05  WK-LTA-CPP-MONTHS      PIC X(12).
               05  FILLER                 PIC X(178).

               05  FILLER                 PIC X(4320).

      *************************************************************
      *    REDEFINITION FOR PASSING INFO FROM
      *    ADDPROGED PROGRAM TO LOAN INPUT PROGRAM.
      *************************************************************
      * 4800
       78  WK-CHECK-RECORD-SIZE             VALUE 4800.
        
           03  WK-CHECK-RECORD              REDEFINES WK-SAVE-RECORD.
               05  WK-STATUS                 PIC X.
               05  WK-PAYEE                  PIC X(60).
               05  WK-PYAMT                  PIC 9(6)V99.
               05  WK-CKNUM                  PIC 9(10).
               05  WK-BWPROC                 PIC X.
               05  WK-CK-FREE1               PIC X(70).
               05  WK-CK-FREE2               PIC X(70).
               05  WK-ADR1                   PIC X(30).
               05  WK-ADR2                   PIC X(30).
               05  WK-CITY                   PIC X(30).
               05  WK-STATE                  PIC XX.
               05  WK-ZIP                    PIC X(10).
               05  FILLER                    PIC X(4478).

      *************************************************************
      *    REDEFINITION FOR PAYMENT POSTING
      *************************************************************
      * 4800
      * 2800 IS LN-REC AND PD-REC
           03  WK-UPDATE-BUF     REDEFINES WK-SAVE-RECORD.

               05  FILLER            PIC X(2800). 
               05  WK-LP-REC         PIC X(410).
               05  WK-LX-REC         PIC X(530).
               05  WK-LXE-REC        PIC X(270).
               05  WK-LXG-REC        PIC X(300).
               05  FILLER            PIC X(490).

      *************************************************************
      *    USED IN LP/LONPD0 (TRL) AND LP/SFDISB.C (TRS) 
      *************************************************************
      * 4800
           03  WK-SAVE-TR-KEY   REDEFINES WK-SAVE-RECORD.
               05  WK-TR-BRNO    PIC 9(4).
               05  WK-TR-DATE    PIC 9(8).
               05  WK-TR-CLASS   PIC 9(3).
               05  WK-TR-ACCTNO  PIC 9(8).
               05  WK-TR-LAWCODE PIC 9(2).
               05  WK-TR-LINE    PIC 9(3).
               05  FILLER        PIC X(4772).

