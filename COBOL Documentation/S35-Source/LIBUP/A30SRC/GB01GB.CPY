      * COPYMEMBER: LIBGB/GB01GB
      **************************************************************************
      *    GLOBAL FILE       550
      *
      * REV:
      *  KEC 011797 ADDED GB-NEXT-APPNO.
      *  JTG 082097 ADDED GB-NEXT-GIFTNET-PO
      *  JTG 101598 ADDED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL REGENCY REVOLVING
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  CS  1999.0315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BAH 990622 ADDED GB-INS-PRODUCER FOR ANY TYPE OF INSURANCE NUMBER
      *             NEED FOR REGMGM VOYAGER PRODUCER NUMBER, PR# 891
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 001120 {PR#00000} FINANCIAL MANAGEMENT [JUDY]
      *             ADDED GB-FEDERAL-TAXID PIC X(10) AS PER BETH.
      *  BAH 051004 RENAMED GB-NEXT-GIFTNET-PO TO GB-NEXT-RECEIPT
      *             GB-NEXT-GIFTNET-PO WAS ONLY BEING USED BY 2 GL EXTRACTIONS
      *             AND GB-NEXT-NOTE IS BEING USED BY GIFTNET AT WORLD !!!
      *             WORLD PR# 436, MEXICO RECEIPT
      *  BAH 150930 MADE FILE INDEXED, ADDED GB-KEY TO RECORD, PD0005
      *  BAH 160216 EXPANDED RECORD SIZE, EXPANDED CLASSES, ADDED BRNO PD0003
      *
      **************************************************************************
       78  GB-SIZE                     VALUE 550.
       01  GB-REC.
           03  GB-BRNO                 PIC 9(4).
           03  GB-KEY                  PIC 9.

      *>>XDD DATE=YYYYMMDD
           03  GB-LTOUCH-DATE          PIC 9(8)   COMP-3.
 
           03  GB-COMPANY-NAME         PIC X(40).
           03  GB-ADDR-1               PIC X(25).
           03  GB-ADDR-2               PIC X(25).
           03  GB-CITY                 PIC X(15).
           03  GB-STATE                PIC XX.
           03  GB-ZIP                  PIC X(10).
           03  GB-PHONE                PIC 9(10)   COMP-3.

           03  GB-NEXT-SF              PIC 9(8)    COMP-3.
           03  GB-NEXT-SMALL           PIC 9(8)    COMP-3.
           03  GB-NEXT-OTHER           PIC 9(8)    COMP-3.
      * NOTE: THIS IS USED BY WORLD FOR NEXT GIFTNET PO#!!!!
           03  GB-NEXT-NOTE            PIC 9(8)    COMP-3.

           03  GB-PROGRP-1ST-LN-CLASS  PIC 9(3)      COMP-3.
           03  GB-PROGRP-2ND-LN-CLASS  PIC 9(3)      COMP-3.

           03  GB-TRW-CONTROL          PIC X(4).
           03  GB-TRW-SUBSCRIBER       PIC X(7).
           03  GB-HOLIDAY-TAB.
               05  GB-HOLIDAY-TABLE    OCCURS 16 TIMES
                                       INDEXED BY GBHOIX.
                   07  GB-HOLIDAY      PIC 9999    COMP-3.

           03  GB-RPT-LINES            PIC 99.
           03  GB-CHECK-PROG           PIC XX.
           03  GB-OTHER-BRANCH-POST    PIC X.
           03  GB-LAST-ACCRUE          PIC 9(4)    COMP-3.
           03  GB-AUTO-DEPOSIT         PIC X.
           03  GB-UUCP-HOME            PIC X(8).
           03  GB-NEXT-APPNO           PIC 9(8)    COMP-3.
      * NOTE: NEXT-RECEIPT IS BEING USED BY GL/DBEXTR AND PLATEX FOR CONTROL #
           03  GB-NEXT-RECEIPT         PIC 9(8)    COMP-3.
           03  GB-NEXT-SF-REVOL        PIC 9(8)    COMP-3.
           03  GB-NEXT-LN-REVOL        PIC 9(8)    COMP-3.
           03  GB-INS-PRODUCER         PIC X(8).
           03  GB-FEDERAL-TAXID        PIC X(10).
           03  FILLER                  PIC X(279).
