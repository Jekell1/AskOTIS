      * COPYMEMBER: LIBGL/GL01GI
      ******************************************************************
      *              (GL)  GI INTERFACE RECORD
      *
      *    GI1-KEY =  GI-BRANCH, GI-CLASS
      *
      *  NOTE:
      *       IF THIS RECORD CHANGES,
      *       CHANGE UNSUB AND ERSUB IN LPEOD5.C!!!!!!!!
      *       ALSO, FIX WORK RECORD IN SP/GICOPY.C!!!
      * REV:
      *  JTG 051995 ADDED GI-REPO-AR, GI-REPO-INT
      *  JTG 111095 ADDED GI-ACR-INT-AR, IB ACCRUED INT INCOME
      *  BAH 011696 ADDED GI-LN-APPROVAL-LIMIT, E022
      *  JTG 111296 ADDED GI-CONV-OFFSET (ACCOUNT CONVERSIONS)
      *  JTG 111197 DEFINED 78'S FOR GI-SIZE 750 AND GI-ACCT-IX-MAX 103
      *  JTG 043098 ADDED GI-PARTIAL-PLEXP (PARTIAL CHARGEOFF), MERCURY #E175
      *             AND SET GI-ACCT-IX-MAX TO 104
      *  BAH 062998 RENAMED GI-EREXTCHG TO GI-ERADDON AND CHANGED
      *                     GI-UNEXTCHG TO GI-UNADDON
      *  JTG 111998 ADDED REVOLVING OTHBAL PAYMENT GL TABLE
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  KEC 990315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 000410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 000929 ADDED GI-PAYLBOX (MFC PR#237)
      *  MJD 001113 ADDED GI-PENALTY-INT (NEWS PR#1264)
      *  JTG 010605 ADDED GP-TRUE-NEXT-BATCH           (MERC PR#243/251)
      *             GI-PAYTPAY
      *             GI-PAYTCOLL
      *             GI-PAYWESTU
      *  JTG 010614 ADDED NEW TRCD "OP" FOR AUTOMATIC OT/AP COMBINATION
      *             ADDED NEW TRCD "RE" FOR REPO EXENSE ADDON,   REGACC PR#1478
      *             GI-REPO-ADDON-EXP
      *  JTG 010814 CHANGED GI-PAYWESTU TO GI-PAYMGRAM           MERCURY PR#258
      *  GLB 020507 ADDED GI-ACH-BANK; REGACC PR#1797
      *  JTG 040517 ADDED GL CONTROL FIELDS FOR:
      *                 - UNEARNED INTEREST REPO
      *                 - UNEARNED ADDON INTEREST REPO
      *             THE UNEARNED WILL MOVE TO THESE ACCOUNTS POSTING A 'SX'
      *             AND REVERSING A 'SV'
      *             THE UNEARNED WILL MOVE (BACK) FROM THESE ACCOUNTS POSTING
      *             A 'SV' AND REVERSING A 'SX'    
      *                                                            REGACC #0049
      * 160122 CS  ADD 250 FILLER
      * 160129 CS  ADDED GI-UNO4, GI-UNO5, MOVED GI-UNITREPO TO END OF GI-UNOT
      *            TABLE. ADD GI-UN1,GI-UN2,GI-UN3,GI-UN4 TO UNEARNED CHARGES
      *            ADDED GI-UNO6, GI-UNO7(UNEARN INSURANCE) AND MOVED GI-UNINT
      *            REPO, GI-UNADDONINTREPO.  ADDED GI-EARN1,GI-EARN2,GI-EARN3
      *            GI-EARN4(NEW EARNINGS COMPONENTS) 
      * 160119 CS  INCREASED GI-CLASS TO 9(3).
      * 160204 CS  FOR THE MOMENT LEFT THE GI-ACCOUNT TABLE INTACT FOR THE 
      *            CLEAR
      * 190321 BAH RENAMED GI-ACCT TO GI-BR-ACCOUNT
      * 190514  CS ADD FUTURE FIELDS, FIXED WK-REC IN SP/GICOY
      * 20190701 BAH REMOVED GI-OTHBAL-* REVOLVING FIELDS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210427 REMOVED FUTURE FIELDS
      * JKC 20210427 GI-BR-ACCOUNT TO USE 'OCCURS 121' INSTEAD OF
      *              'OCCURS GI-ACCT-IX-MAX'; WAS THE ONLY 01 COPY
      *              MEMBER USING A 78 IN AN OCCURS; THIS CHANGE
      *              MAKES IT MORE CONSISTENT WITH OTHER LAYOUTS.
      *  20230600 BAH ADDED FILLER
      ******************************************************************
       78  GI-SIZE                VALUE  990.
       01  GI-REC.
           03  GI1-KEY.
               05  GI-BRANCH      PIC 9(4).
               05  GI-CLASS       PIC 999.
      $XFD DATE=YYYYMMDD
           03  GI-LTOUCH-DATE     PIC 9(8) COMP-3.

           03  GI-ACCOUNTS.       
      *----BANK AND CASH---------------------------
               05  GI-CASH        PIC 9(11) COMP-3.
               05  GI-BANK        PIC 9(11) COMP-3.
               05  GI-NSF         PIC 9(11) COMP-3.
      *----RECEIVABLES----------------------------- 4
               05  GI-AR          PIC 9(11) COMP-3.
               05  GI-PL          OCCURS 5
                                  PIC 9(11) COMP-3.
               05  GI-DLINSREB    PIC 9(11) COMP-3.
               05  GI-ACR-INT-AR-AR  PIC 9(11) COMP-3.
      *----INTER BRANCH---------------------------- 12
               05  GI-OTHBR-REC   PIC 9(11) COMP-3.
               05  GI-OTHBR-PAY   PIC 9(11) COMP-3.
      *----UNEARNED CHARGES------------------------- 14
               05  GI-UNEARN-CHG-TBL. 
                   07  GI-UNINTCHG           PIC 9(11) COMP-3.
                   07  GI-UNADDON            PIC 9(11) COMP-3.
                   07  GI-UNSERCHG           PIC 9(11) COMP-3.
                   07  GI-UNMAINTFEE         PIC 9(11) COMP-3.
                   07  GI-UNDLDISC           PIC 9(11) COMP-3.
                   07  GI-UNOTHER            PIC 9(11) COMP-3.
                   07  GI-UN7                PIC 9(11) COMP-3.
                   07  GI-UN8                PIC 9(11) COMP-3.
                   07  GI-UN9                PIC 9(11) COMP-3.
                   07  GI-UN10               PIC 9(11) COMP-3.

               05  GI-UNEARN-CHG-TBLX  REDEFINES GI-UNEARN-CHG-TBL.
                   07  GI-UNEARN         OCCURS 10
                                         PIC 9(11) COMP-3.

               05  GI-UNCL1              PIC 9(11) COMP-3.
               05  GI-UNCL2              PIC 9(11) COMP-3.
               05  GI-UNAH               PIC 9(11) COMP-3.
               05  GI-UNPP               PIC 9(11) COMP-3.
               05  GI-UNOT-TBL.
                   07  GI-UNO1           PIC 9(11) COMP-3.
                   07  GI-UNO2           PIC 9(11) COMP-3.
                   07  GI-UNO3           PIC 9(11) COMP-3.
                   07  GI-UNO4           PIC 9(11) COMP-3.
                   07  GI-UNO5           PIC 9(11) COMP-3.
                   07  GI-UNO6           PIC 9(11) COMP-3.
                   07  GI-UNO7           PIC 9(11) COMP-3.
               05  GI-UNOT-TBLX   REDEFINES GI-UNOT-TBL.
                   07  GI-UNOT           OCCURS 7
                                         PIC 9(11) COMP-3.
 
               05  GI-UNINTREPO      PIC 9(11) COMP-3.
               05  GI-UNADDONINTREPO PIC 9(11) COMP-3.


      *----EXPENSES-------------------------------- 37
               05  GI-PLEXP       OCCURS 5
                                  PIC 9(11) COMP-3.
               05  GI-DLPARVEX    PIC 9(11) COMP-3.
      *----OFFSETS--------(SEE BELOW)-------------- 43
               05  GI-PLOFFSET    OCCURS 5
                                  PIC 9(11) COMP-3.
               05  GI-SERVOFF     PIC 9(11) COMP-3.
      *----PAYABLES-------------------------------- 49
               05  GI-PAYCL1      PIC 9(11) COMP-3.
               05  GI-PAYCL2      PIC 9(11) COMP-3.
               05  GI-PAYAH       PIC 9(11) COMP-3.
               05  GI-PAYPP       PIC 9(11) COMP-3.
               05  GI-PAYOT-TBL.
                   07  GI-PAYO1   PIC 9(11) COMP-3.
                   07  GI-PAYO2   PIC 9(11) COMP-3.
                   07  GI-PAYO3   PIC 9(11) COMP-3.
                   07  GI-PAYO4   PIC 9(11) COMP-3.
                   07  GI-PAYO5   PIC 9(11) COMP-3.
                   07  GI-PAYO6   PIC 9(11) COMP-3.
                   07  GI-PAYO7   PIC 9(11) COMP-3.
               05  GI-PAYOT-TBLX  REDEFINES GI-PAYOT-TBL.
                   07  GI-PAYOT   OCCURS 7
                                  PIC 9(11) COMP-3.
               05  GI-DLPAYABLE        PIC 9(11) COMP-3.
      *----RESERVES---------------------------------------- 61
               05  GI-DLRVPRINX.
                   07  GI-DLRVPRIN-1  PIC 9(11) COMP-3.
                   07  GI-DLRVPRIN-2  PIC 9(11) COMP-3.
                   07  GI-DLRVPRIN-3  PIC 9(11) COMP-3.
               05  GI-DLRVPRINX-TBL  REDEFINES GI-DLRVPRINX.
                   07  GI-DLRVPRIN    OCCURS 3
                                      PIC 9(11) COMP-3.
               05  GI-DLRVCHGSX.
                   07  GI-DLRVCHGS-1  PIC 9(11) COMP-3.
                   07  GI-DLRVCHGS-2  PIC 9(11) COMP-3.
                   07  GI-DLRVCHGS-3  PIC 9(11) COMP-3.
               05  GI-DLRVCHGSX-TBL  REDEFINES GI-DLRVCHGSX.
                   07  GI-DLRVCHGS    OCCURS 3
                                      PIC 9(11) COMP-3.
      *---------------------------------------------------- 67
               05  GI-DLPARV          PIC 9(11) COMP-3.
               05  GI-PAYALLOT        PIC 9(11) COMP-3.
               05  GI-DIRECT-PAY      PIC 9(11) COMP-3.
      *------------INCOME---------------------------------- 70
               05  GI-EARN-CHG-TBL. 
                   07  GI-ERINTCHG    PIC 9(11) COMP-3.
                   07  GI-ERADDON     PIC 9(11) COMP-3.
                   07  GI-ERSERCHG    PIC 9(11) COMP-3.
                   07  GI-ERMAINTFEE  PIC 9(11) COMP-3.
                   07  GI-ERDLDISC    PIC 9(11) COMP-3.
                   07  GI-EROTHER     PIC 9(11) COMP-3.
                   07  GI-EARN7       PIC 9(11) COMP-3.
                   07  GI-EARN8       PIC 9(11) COMP-3.
                   07  GI-EARN9       PIC 9(11) COMP-3.
                   07  GI-EARN10      PIC 9(11) COMP-3.
               05  GI-EARN-CHG-TBLX REDEFINES GI-EARN-CHG-TBL.
                   07  GI-EARN    OCCURS 10 PIC 9(11) COMP-3.

               05  GI-LATECHG     PIC 9(11) COMP-3.
               05  GI-INTEREST    PIC 9(11) COMP-3.
               05  GI-DEFERMENTS  PIC 9(11) COMP-3.
               05  GI-SAPRINCIPAL PIC 9(11) COMP-3.
      *---------------------------------------------------- 84
               05  GI-PLINT       OCCURS 5
                                  PIC 9(11) COMP-3.
               05  GI-PLREC       OCCURS 5
                                  PIC 9(11) COMP-3.
      *---------------------------------------------------- 94
               05  GI-CL1             PIC 9(11) COMP-3.
               05  GI-CL2             PIC 9(11) COMP-3.
               05  GI-AH              PIC 9(11) COMP-3.
               05  GI-PP              PIC 9(11) COMP-3.
               05  GI-O1              PIC 9(11) COMP-3.
               05  GI-O2              PIC 9(11) COMP-3.
               05  GI-O3              PIC 9(11) COMP-3.
               05  GI-O4              PIC 9(11) COMP-3.
               05  GI-O5              PIC 9(11) COMP-3.
               05  GI-O6              PIC 9(11) COMP-3.
               05  GI-O7              PIC 9(11) COMP-3.
      *---------------------------        ----------------- 105 
      * NOTE:
      *       GI-PLNEW-AR WAS INTENDED FOR ADDITIONAL NEED
      *       FOR EACH LEVEL (PL/P2/P3/P4/P5).
      *       THE FIRST LEVEL IS NOT NEEDED/USED.
      *       THE SECOND & THIRD LEVELS ARE USED AS INTENDED.
      *       THE FOURTH & FIFTH LEVELS ARE NOT AS INTENDED;
      *       THEY WERE USED BY JOHN GENTILE IN 2002-0916 FOR
      *       REGACC PR#1846 WHICH ARE TRIGGERED BY USING
      *       GP-DLCOMM-CMSI-FG = "Y".
      *
      *       GI-PLNEW-AR(1) IS NOT USED; ON P2
      *       GI-PLNEW-AR(2) IS TRCD "SE" ON P2
      *       GI-PLNEW-AR(3) IS TRCD "SE" ON P3
      *       GI-PLNEW-AR(4) IS DLCOMM-PP-CMSI  GAP
      *       GI-PLNEW-AR(5) IS DLCOMM-O3-CMSI  WARRANTY
      *
               05  GI-PLNEW-AR    OCCURS 5
                                  PIC 9(11) COMP-3.
      *-------------------------------------------- 110
               05  GI-REPO-AR     PIC 9(11) COMP-3.
               05  GI-REPO-INT    PIC 9(11) COMP-3.
               05  GI-ACR-INT-AR  PIC 9(11) COMP-3.
      *----OFFSETS--------(SEE ABOVE)-------------- 113
               05  GI-CONV-OFFSET PIC 9(11) COMP-3.
      *----PL EXPENSE-----(SEE ABOVE)-------------- 114
               05  GI-PARTIAL-PLEXP PIC 9(11) COMP-3.

      *-------------------------------------------- 115
               05  GI-PAYLBOX             PIC 9(11)   COMP-3.
               05  GI-PENALTY-INT         PIC 9(11)   COMP-3.
               05  GI-PAYTPAY             PIC 9(11)   COMP-3.
               05  GI-PAYTCOLL            PIC 9(11)   COMP-3.
               05  GI-PAYMGRAM            PIC 9(11)   COMP-3.
               05  GI-REPO-ADDON-EXP      PIC 9(11)   COMP-3.   
               05  GI-ACH-BANK            PIC 9(11)   COMP-3.
      *--------------------------------------------------
       78  GI-ACCT-IX-MAX         VALUE 121.
           03  GI-ACCTS REDEFINES GI-ACCOUNTS.
               05  GI-BR-ACCOUNT          OCCURS 121
                                          PIC 9(11)   COMP-3.
            
           03  GI-LN-APPROVAL-LIMIT       PIC 9(7)    COMP-3.

           03  FILLER                     PIC X(248).
