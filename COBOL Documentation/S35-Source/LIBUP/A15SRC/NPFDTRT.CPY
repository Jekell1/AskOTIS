      * COPYMEMBER: LIBNP/NPFDTRT
      ******************************************************************
      *                                                
      *     (DAILY NOTES PAYABLE TRANSACTION RECORD)   
      *
      *           CODE = "T"
      *
      *            --- RECORD LENGTH 218 ---           
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
      *  REV:
      *  MJD 091698 ADDED POSTTIME, POSTDATE, POST-BR AND USERID.
      *  BLV 980923 ADDED TRT-CASH-DRAWER-NO
      *  BLV 980929 ADDED TRT-OTHBR-FG
      *  MJD 981103 ADDED TRT-TRANSFER-ACCTNO.
      *  BLV 981107 MOVE TRANSFER-ACCTNO, OTHBR-FG, POST-BRNO;
      *             ALIGNING FIELDS WITH WK-REC, NH-REC
      *  GLF 981217 ADD TRT-CHECKNO,TRT-REL-BRNO,TRT-CK-STATUS
      *  GLB 981221 ADD TRT-NC-SEQNO, ADJUSTED REC SIZE
      *  MJD 981230 ADDED TRT-REVERSED
      *  MJD 990105 ADDED TRT-REV-SEQNO AND TRT-BATCH-ID.
      *  GLB 020124 PR#1714 REGWAN;ADDED TRT-EOM-ACCRUAL-FG
      *             TEST IN EOD AND FORCE DAY "16" IN GL-TRDATE
      *  BLM 081023 ADDED FIELDS, SINCE HOLD-REC GETS MOVED TO
      *             NH-REC & TR-REC IN NOTPF2, THESE FIELDS WERE
      *             GETTING WRITTEN TO THE TR REC BUT NOT DEFINED,
      *             NEEDED TO CHECK TRT-CASH-AMT IN NTCASH. ADDED:
      *             05  TRT-DAYS-ACCRUED    PIC S9(3)     COMP-3.
      *             05  TRT-DAYS-BAL        PIC S9(3)     COMP-3.
      *             05  TRT-CASH-AMT        PIC S9(9)V99  COMP-3.
      *             05  TRT-CHECK-AMT       PIC S9(9)V99  COMP-3.
      *             05  TRT-TRANSFER-AMT    PIC S9(9)V99  COMP-3.
      *             REGWAN PR# 3252
      **************************************************
       01  TRT-REC.
           03  TRT-KEY.
               05  TRT-DAY              PIC 99.
               05  TRT-BRNO             PIC 9(4).
               05  TRT-COMMON-KEY.
                   07  TRT-CODE             PIC X.
                   07  TRT-CLASS            PIC 99.
                   07  TRT-NC-SEQNO         PIC 99.
                   07  TRT-ACCTNO           PIC 9(6).
                   07  TRT-LINE             PIC 9(6).
                   07  FILLER               PIC X(6).
           03  TRT-UPD-FLAG             PIC X.     
           03  TRT-DATA.
               05  TRT-TRDATE          PIC 9(6)      COMP-3.
               05  TRT-TRCD            PIC X(2).
               05  TRT-REFNO           PIC X(5).
               05  TRT-TRAMT           PIC S9(9)V99  COMP-3.
               05  TRT-NOTERATE        PIC 99V99     COMP-3.
               05  TRT-CURFG           PIC X.
      **************************************************
      *        PRIOR TO TRANSACTION
      **************************************************
               05  TRT-PREVBAL         PIC S9(9)V99  COMP-3.
               05  TRT-ACCRUAL         PIC S9(9)V99  COMP-3.
               05  TRT-ACCRDATE        PIC 9(6)      COMP-3.
               05  TRT-LASTPMT         PIC 9(6)      COMP-3.
      **************************************************
      *        AFTER TRANSACTION
      **************************************************
               05  TRT-NOTEBAL         PIC S9(9)V99  COMP-3.
               05  TRT-GLCASH          PIC S9(9)V99  COMP-3.
               05  TRT-GLACCRUE        PIC S9(9)V99  COMP-3.
               05  TRT-GLEXPENSE       PIC S9(9)V99  COMP-3.
               05  TRT-GLNTPAY         PIC S9(9)V99  COMP-3.
               05  TRT-GLMISC          OCCURS 2.
                   07  TRT-GLNO        PIC 9(11)     COMP-3.
                   07  TRT-GLAMT       PIC S9(9)V99  COMP-3.
               05  TRT-REALTRCD        PIC X(2).
               05  TRT-PAYDATE         PIC 9(6)      COMP-3.
               05  TRT-CASH-DRAWER-NO  PIC X.
               05  TRT-TRANSFER-ACCTNO PIC 9(6)      COMP-3.
               05  TRT-CHECKNO         PIC 9(6)      COMP-3.
               05  TRT-REL-BRNO        PIC 9(4)      COMP-3.
               05  TRT-CK-STATUS       PIC XX.
               05  TRT-POST-BRNO       PIC 9(4)      COMP-3.
               05  TRT-OTHBR-FG        PIC X.
               05  TRT-USERID          PIC X(10).
               05  TRT-POSTDATE        PIC 9(8)      COMP-3.
               05  TRT-POSTTIME        PIC 9(6)      COMP-3.
               05  TRT-REVERSED        PIC X.
               05  TRT-REV-SEQNO       PIC 9(4)      COMP-3.
               05  TRT-BATCH-ID        PIC 9(5)      COMP-3.
               05  TRT-EOM-ACCRUAL-FG  PIC X.
               05  FILLER              PIC X(5).
               05  TRT-DAYS-ACCRUED    PIC S9(3)     COMP-3.
               05  TRT-DAYS-BAL        PIC S9(3)     COMP-3.
               05  TRT-CASH-AMT        PIC S9(9)V99  COMP-3.
               05  TRT-CHECK-AMT       PIC S9(9)V99  COMP-3.
               05  TRT-TRANSFER-AMT    PIC S9(9)V99  COMP-3.
               05  FILLER              PIC X(15).
