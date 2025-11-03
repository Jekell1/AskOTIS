      * COPYMEMBER: LIBGB/GB01GB
      **************************************************************************
      *      GLOBAL FILE
      *
      *    GB-KEY = GB-BRNO, GB-NO
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
      *             ADDED GB-FEDERAL-TAXID      PIC X(10) AS PER BETH.
      *  BAH 051004 RENAMED GB-NEXT-GIFTNET-PO TO GB-NEXT-RECEIPT
      *             GB-NEXT-GIFTNET-PO WAS ONLY BEING USED BY 2 GL EXTRACTIONS
      *             AND GB-NEXT-NOTE IS BEING USED BY GIFTNET AT WORLD !!!
      *             WORLD PR# 436, MEXICO RECEIPT
      *  BAH 150930 MADE FILE INDEXED, ADDED GB-NO TO RECORD, PD0005
      *  BAH 160216 EXPANDED RECORD SIZE, EXPANDED CLASSES, ADDED BRNO PD0003
      *  BAH 181030 GLOBAL GBFILE, MOVED GB-BRNO UNDER GB-NO 
      *   CS 190408 ADD FUTURE FIELDS.SIZE WAS 550
      *   CS 190617 ADD GB-EOM-YYYYMM REPLACING END OF MONTH TAG FILE
      *  BAH 20190625 REMOVED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL PT0153
      *  BAH 20190705 REMOVED GB-NEXT-NOTE, GB-NEXT-APPNO, GB-NEXT-RECEIPT
      *  CS 20190708 ADD GB-EOM-UPDATE-DATE-TIME, GB-EOY-UPDATE-DATE-TIME, 
      *              GB-EOM-UPDATE-USERID,GB-EOY-UPDATE-USERID
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20200519 REMOVED GB-PROGRP-1ST-LN-CLASS, GB-PROGRP-2ND-LN-CLASS,
      *               GB-FEDERAL-TAXID, GB-INS-PRODUCER, GB-TRW-CONTROL,
      *               GB-TRW-SUBSCRIBER
      *  BAH 20210909 REMOVED XFD DATE=HHNNSS ON TIMES
      *  20230600 BAH ADDED FILLER
      **************************************************************************
       78  GB-SIZE                     VALUE 500.
       01  GB-REC.
           03  GB-KEY.
               05  GB-BRNO                  PIC 9(4).
               05  GB-NO                    PIC 9.

      $XFD DATE=YYYYMMDD
           03  GB-LTOUCH-DATE               PIC 9(8)   COMP-3.
 
           03  GB-COMPANY-NAME              PIC X(40).
           03  GB-ADDR-1                    PIC X(25).
           03  GB-ADDR-2                    PIC X(25).
           03  GB-CITY                      PIC X(15).
           03  GB-STATE                     PIC XX.
           03  GB-ZIP                       PIC X(10).
           03  GB-PHONE                     PIC 9(10)  COMP-3.

           03  GB-NEXT-SF                   PIC 9(8)   COMP-3.
           03  GB-NEXT-SMALL                PIC 9(8)   COMP-3.
           03  GB-NEXT-OTHER                PIC 9(8)   COMP-3.

           03  GB-HOLIDAY-TAB.
               05  GB-HOLIDAY-TABLE  OCCURS 16 
                                     INDEXED BY GBHOIX.
                   07  GB-HOLIDAY           PIC 9(4)   COMP-3.

           03  GB-RPT-LINES                 PIC 99.
           03  GB-CHECK-PROG                PIC XX.
           03  GB-OTHER-BRANCH-POST         PIC X.
           03  GB-LAST-ACCRUE               PIC 9(4)   COMP-3.
           03  GB-AUTO-DEPOSIT              PIC X.
           03  GB-UUCP-HOME                 PIC X(8).
           03  GB-EOM-TAG-CCYYMM            PIC 9(6).
           03  GB-EOM-TAG-WORK     REDEFINES GB-EOM-TAG-CCYYMM.
               05  GB-EOM-TAG-CCYY          PIC 9999.
               05  GB-EOM-TAG-MM            PIC 99.
           03  GB-EOM-UPDATE-DATE-TIME.
      $XFD DATE=YYYYMMDD
               05  GB-EOM-UPDATE-DATE       PIC 9(8)   COMP-3.
               05  GB-EOM-UPDATE-TIME       PIC 9(6)   COMP-3.
           03  GB-EOM-UPDATE-USERID         PIC X(10).

           03  GB-FISCAL-EOY-TAG-CCYYMM     PIC 9(6).
           03  GB-FISCAL-EOY-TAG-WORK REDEFINES 
                         GB-FISCAL-EOY-TAG-CCYYMM.
               05  GB-FISCAL-EOY-TAG-CCYY   PIC 9999.
               05  GB-FISCAL-EOY-TAG-MM     PIC 99.

      *    EOYF = END OF YEAR FISCAL
           03  GB-EOYF-UPDATE-DATE-TIME.
      $XFD DATE=YYYYMMDD
               05  GB-EOYF-UPDATE-DATE      PIC 9(8)   COMP-3.
               05  GB-EOYF-UPDATE-TIME      PIC 9(6)   COMP-3.
           03  GB-EOYF-UPDATE-USERID        PIC X(10).
                
           03  GB-CALENDAR-EOY-TAG-CCYYMM   PIC 9(6).
           03  GB-CALENDAR-EOY-TAG-WORK REDEFINES
                           GB-CALENDAR-EOY-TAG-CCYYMM.
               05  GB-CALENDAR-EOY-TAG-CCYY PIC 9999.
               05  GB-CALENDAR-EOY-TAG-MM   PIC 99.

      *    EOYC = END OF YEAR CALENDAR
           03  GB-EOYC-UPDATE-DATE-TIME.
      $XFD DATE=YYYYMMDD
               05  GB-EOYC-UPDATE-DATE      PIC 9(8)   COMP-3.
               05  GB-EOYC-UPDATE-TIME      PIC 9(6)   COMP-3.
           03  GB-EOYC-UPDATE-USERID        PIC X(10).

           03  FILLER                       PIC X(212).
