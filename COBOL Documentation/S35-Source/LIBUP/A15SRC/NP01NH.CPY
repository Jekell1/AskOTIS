      * COPYMEMBER: LIBNP/NP01NH
      ******************************************************************
      *
      *       NH-FILE - NOTE TRANSACTION HISTORY
      *       MUST MATCH WK-FILE, SO ALSO CHANGE NP01WK
      *
      *=================================================================
      *
      *  ***ATTENTION*** ***ATTENTION*** ***ATTENTION*** ***ATTENTION***
      *
      *  *** DATE CREATED: 2016-0520 JAY
      *  *** DATE UPDATED: 2016-0520 JAY
      *
      *  THE FOLLOWING ARE LINKED TOGETHER:
      *  - 03  HOLD-AREA.        NP/NOTPF1 & NP/NOTPF2
      *  - 03  NH-DATA.          LIBNP/NP01NH 
      *  - 03  TRT-DATA.         LIBNP/NPFDTRT
      *  - 03  WK-DATA.          LIBNP/NP01WK 
      *  - 03  WK2-REC-COMMON.   NP/NITACC
      *
      *  WHEN CHANGING (FIELDS AND/OR SIZE) ANY OF THIS ITEMS
      *  BE ***VERY CAREFUL*** TO PROPERLY UPDATING THE OTHER ITEMS
      *  SINCE THEY ARE ALL LINKED TOGETHER.
      *
      *  HOLD-AREA IS USED AS A HEADER RECORD FOR WK-FILE [WK-KEY=1].
      *  IT DOES *NOT* MATCH THE LAYOUT OF WK-REC.  IT IS USED TO PASS
      *  INFORMATION FROM NP/NOTPF1 TO NP/NOTPF2.  THE REST OF WK-FILE
      *  [WK-KEY=2 & HIGHER] DOES MATCH THE LAYOUT OF WK-REC.
      *
      *  NH-DATA, TRT-DATA AND WK-DATA ARE IDENTICAL & IN USE TOGETHER.
      *
      *  TRT-EOM-ACCURAL-FG [X(1)] WAS IN THE WK-NEW-PAY WAS IN THE
      *  MIDDLE FILLER NH-REC [X(6); UNDER NH-BATCH-ID] FOR A15.
      *  IT REMAINS IN THE SAME SPOT IN A30 AS OF NOW [2016-0520 JAY].
      *
      *  WK-REC IS IDENTICAL TO NH-REC BUT HAS ADDITIONAL FIELDS AT THE
      *  BOTTOM OF THE LAYOUT.
      *
      *  WK-NEW-PAY WAS IN THE MIDDLE FILLER NH-REC
      *  [X(6); UNDER NH-BATCH-ID] FOR A15.  WK-NEW-PAY WAS MOVED TO
      *  THE BOTTOM OF THE LAYOUT [OUTSIDE OF NH-REC DEFINITION] FOR A30
      *
      *  WK-REC IS IDENTICAL TO WK2-REC-COMMON (WK-REC IS MOVED INTO
      *  WK2-REC-COMMON AND THEN MOVED BACK INTO WK-REC LATER).
      *
      *  WK-FILE USED IN NP/NOTPF1 & NP/NOTPF2 USES NP-TEMP-PATH AND IS
      *  REMOVED AT THE END OF NP/NOTP.
      *
      *=================================================================
      * REV:
      *  BLV 980923 ADDED NH-CASH-DRAWER-NO
      *  BLV 981107 ADDED NH-TRANSFER-ACCTNO, NH-OTHBR-FG
      *                   NH-POST-BRNO
      *  BLV 981217 EXPANDED RECORD FROM 128 TO 175
      *  GLF 981217 ADD NH-CHECKNO, NH-REL-BRNO, NH-CK-STATUS
      *  MJD 981230 ADDED NH-USERID, NH-POSTDATE, NH-POSTTIME,
      *             NH-REVERSED. ADDED 10 BYTES TO RECORD
      *  MJD 990105 ADDED NH-REV-SEQNO AND NH-BATCH-ID.
      *  MJD 990122 ----- IMPORTED INTO FLEXGEN -----
      *  CS  1999.0309 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BLV 990927 ADDED NH-DAYS-ACCRUED, FEB FIX
      *  GLB 991004 RENAME NH-DAYS-ACCURED TO NH-DAYS-BAL
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BLF 070122 ADD NH-CASH-AMT, NH-CHECK-AMT, NH-TRANSFER-AMT, 
      *             TO MONITOR FUNDS SOURCE, REGENCY PR# 2893
      ******************************************************************
       78  NH-SIZE                     VALUE 185.
       01  NH-REC.
           03  NH1-KEY.
               05  NH-ACCTNO           PIC 9(6)      COMP-3.
               05  NH-SEQNO            PIC 9(4)      COMP-3.
      **************************************************
      *        CURRENT TRANSACTION
      **************************************************
           03  NH-DATA.
               05  NH-TRDATE           PIC 9(6)      COMP-3.
               05  NH-TRCD             PIC X(2).
               05  NH-REFNO            PIC X(5).
               05  NH-TRAMT            PIC S9(9)V99  COMP-3.
               05  NH-NOTERATE         PIC 99V99     COMP-3.
               05  NH-ACTIVE-CD        PIC X.
      **************************************************
      *        PRIOR TO TRANSACTION
      **************************************************
               05  NH-PREVBAL          PIC S9(9)V99  COMP-3.
               05  NH-ACCRUAL          PIC S9(9)V99  COMP-3.
               05  NH-ACCRDATE         PIC 9(6)      COMP-3.
               05  NH-LASTPMT          PIC 9(6)      COMP-3.
      **************************************************
      *        AFTER TRANSACTION
      **************************************************
               05  NH-NOTEBAL          PIC S9(9)V99  COMP-3.
               05  NH-GLCASH           PIC S9(9)V99  COMP-3.
               05  NH-GLACCRUE         PIC S9(9)V99  COMP-3.
               05  NH-GLEXPENSE        PIC S9(9)V99  COMP-3.
               05  NH-GLNTPAY          PIC S9(9)V99  COMP-3.
               05  NH-GL-INFO.
                   07  NH-GLMISC           OCCURS 2.
                       09  NH-GLNO         PIC 9(11)     COMP-3.
                       09  NH-GLAMT        PIC S9(9)V99  COMP-3.
               05  NH-INTPYCD-INFO REDEFINES NH-GL-INFO.
                   07  NH-INTPYCD      PIC X.
                   07  NH-PARTAMT      PIC 9(6)V99       COMP-3.
                   07  NH-PARTPCT      PIC 99V99         COMP-3.
                   07  NH-LAST-RN-DATE PIC 9(8)      COMP-3.
                   07  NH-YRLYPMTS     PIC 9(2)      COMP-3.
                   07  NH-ISSDATE      PIC 9(8)      COMP-3.
                   07  FILLER          PIC X(3).
                   
               05  NH-REALTRCD         PIC X(2).
               05  NH-PAYDATE          PIC 9(6)      COMP-3.
               05  NH-CASH-DRAWER-NO   PIC X.
               05  NH-TRANSFER-ACCTNO  PIC 9(6)      COMP-3.
               05  NH-CHECKNO          PIC 9(6)      COMP-3.
               05  NH-REL-BRNO         PIC 9(4)      COMP-3.
               05  NH-CK-STATUS        PIC XX.
               05  NH-POST-BRNO        PIC 9(4)      COMP-3.
               05  NH-OTHBR-FG         PIC X.
               05  NH-USERID           PIC X(10).
               05  NH-POSTDATE         PIC 9(8)      COMP-3.
               05  NH-POSTTIME         PIC 9(6)      COMP-3.
               05  NH-REVERSED         PIC X.
               05  NH-REV-SEQNO        PIC 9(4)      COMP-3.
               05  NH-BATCH-ID         PIC 9(5)      COMP-3.
               05  FILLER              PIC X(6).
               05  NH-DAYS-ACCRUED     PIC S9(3)     COMP-3.
               05  NH-DAYS-BAL         PIC S9(3)     COMP-3.
               05  NH-CASH-AMT         PIC S9(9)V99  COMP-3.
               05  NH-CHECK-AMT        PIC S9(9)V99  COMP-3.
               05  NH-TRANSFER-AMT     PIC S9(9)V99  COMP-3.
               05  FILLER              PIC X(5).
