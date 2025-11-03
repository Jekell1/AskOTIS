      * COPYMEMBER: LIBGB/GB01GP
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GB01GP                            RSZ=0512
      *
      * DESCRIPTION   : GLOBAL PARAMETER FILE (GP)
      *
      * FILE KEY #'S  : 1 = STANDARD GLOBAL PARAMETER OPTIONS
      *                 2 = ACCOUNTS PAYABLE
      *                 3 = LOAN APPLICATIONS (PROMPTS)
      *                 4 = REVOLVING RATES
      *                 5 = LOAN APPLICATIONS (OPTIONS)
      *                 6 = ADDITIONAL GP INFO (BANK2 INFO)
      *
      *=================================================================
      * REV:
      * JTG 072095 ADDED GP-BANK-DEPOSIT-GL
      * JTG 072595 ADDED GP-AGE-BUCKET-CD
      * JTG 081095 ADDED GP-THE-ONLY-SF-SRCD
      * BAH 081495 ADDED GP-ALLOW-BATCH-CHECKS
      * MJD 081695 ADDED GP-DLRVPRIN-3-FG AND GP-DRRVCHGS-3-FG.
      * MJD 082395 ADDED GP-CASH-METHOD-ONLY AND GP-IB-ACCRUALS-DONE.
      * KEC 082995 ADDED GP-NO-LC-IN-ARREARAGE-FG
      * JTG 092995 ADDED GP-DEFAULT-O3-INS-COMPANY
      * MJD 121195 ADDED GP-STOP-ON-PRIOR-FG.
      * MJD 032196 ADDED GP-APPLICATION-FG
      * BAH 032796 ADDED GP-DEFAULT-ACCRATE
      * MJD 041996 ADDED GP-DLR-DISP-TYPE
      * MJD 042396 ADDED GP-ALLOW-RLOGINS
      * MJD 050196 ADDED RECORD KEY #2, ACCOUNTS PAYABLE (AP)
      * MJD 050296 ADDED GP-AUTO-REPO-WINDOW.
      * JTG 052396 ADDED GP-ALLOW-PI-WITH-SE-BAL
      * BAH 071596 ADDED GP-ALLOW-MORE-COLLECT TO ALLOW MORE THAN 1
      *            COLLECTION TYPE FORM TO BE EXTRACTED AT ONCE
      * BAH 072696 ADDED GP-BATCH-MORE-THAN-DAY TO NOT FORCE YOU TO RELEASE
      *            BATCH CHECKS DAILY LIKE MERCURY (INSTANT)
      * BAH 072696 ADDED GP-PRINT-GL-RECAP TO PRINT FROM BATCH PRINT
      *            MERCURY DID NOT WANT THESE TO PRINT IN THE BRANCHES !
      * JTG 080696 CHANGED GB-BEG-FISCALX TO X(6)
      * JTG 121196 ADDED GP-LOAN-CONVERSION-UPD-FG
      * JTG 021897 ADDED GP-FORM-CHG-CD, BHFC
      * BLV 031097 ADDED GP-PROGRP-PRINT-SOLD
      * BAH 031397 ADDED GP-BW-HOUSING-CODE
      * GLF 031397 CHANGED GP-SPL-RETAIN-MONTHS TO GP-EOD-RETAIN-MONTHS
      * MJD 050997 ADDED GP-BW-CREDIT-SCORE . "Y" REQUIRES ENTRY OF
      *            CREDIT SCORE  FIELD IN BW. (REQ#E155)
      *  CS 051297 CHANGED GP-PROGRP-PRINT-SOLD TO GP-PROGRP-PRINT-REDEEM
      * BAH 060997 ADDED GP-DISPLAY-BANK-BAL TO DISPLAY BANK BALANCES
      *            WHEN CLOSING DAY FOR COLONIAL, TN
      * MJD 071197 ADDED GP-PROGRP-DL-CONT. ON PROGRESS REPORT, IF "Y"
      *            THEN PRINT CONTRACTUAL FOR DIRECT LOAN 150+ POT'S
      *            CUMBERLAND REQ#525
      *  CS 071997 ADDED GP-DLRVPRIN-2-FG AND GP-DLRVPRIN-1-FG FOR RESERVE
      *            FROM PRINCIPAL.            
      * KEC 072897 ADDED 'KEY 3': FOR LOAN APPLICATIONS.
      * JTG 082097 ADDED GP-ADV-TO-1ST-NO-CORR WORLD A90
      * JTG 082697 ADDED GP-I-AM-WORLD, WORLD A90
      * JTG 082697 REMOVED COMP-3'S SO SET ENVIRONMENT ON GPFILE WORKS
      * MJD 082997 ADDED GP-COMPANY-FLAG AND MADE GP-I-AM-WORLD AN 88 LEVEL.
      *            ALSO ADDED GP-I-AM-MERC AND GP-I-AM-RAC AS 88 LEVELS
      * KEC 091097 ADDED 'USERDEFINED PROMPTS' UNDER 'KEY 3' RE: JTG
      * JTG 092497 ADDED TO GP-INTERBR-AR-AP-FULLGL TO ALLOW DETAIL FROM/TO
      *            BRANCH ON INTERBRANCH PAYMENTS
      * MJD 122997 ADDED GP-SKIP-BI-UPDATE
      * BAH 032098 ADDED GP-ASK-HARD-COPY-MEMOS SO LONPP0 WILL ASK IF YOU
      *            WANT TO PRINT MEMOS, WORLD # 183
      * MJD 032698 ADDED GP-RLOGIN-LOCAL-PROFILE AND GP-RLOGIN-REMOTE-PROFILE.
      *            THESE REPRESENT THE LOWEST SECUTITY LEVEL THAT CAN ACCESS
      *            RLOGINS.
      * BAH 040198 ADDED GP-REPODATE-PAYDATE FOR FEDERATED #663 AND RAC #518
      * BAH 040298 ADDED GP-POST-INTO-FUTURE FROM A90 GLOBAL
      * BAH 040798 ADDED GP-RENEWAL-COLLATERAL FOR WORLD TO BRING COLLATERAL
      *             FORWARD FROM PRIOR LOAN
      * BLV 052198 ADDED GP-PROGRP-CLASSES FOR PRIORITIZING CLASSES TO
      *            PRINT ON PROGRESS REPORT (CASTLE, PR# 622)
      * JTG 082198 ADDED GP-RENEW-DISC-UNERN FOR MERCURY TO INDICATED IF 
      *            THE NEW LOAN SHOULD RE-ESTABLISH THE UNEARNED DISCOUNT
      *            OF THE LOAN RENEWED
      * BLV 091698 ADDED GP-NOTES-EOD-BATCH-ID FOR REWRITE OF NOTES EOD
      * JTG 101298 ADDED GP-ALLOW-REVOLVING-FG, REVOL REGENCY 
      * KEC 103098 ADDED GP-RATES-REC (RECORD KEY #4), REVOL REGENCY
      * MJD 110398 ADDED GP-NEXT-NOTE FOR NOTES PAYABLE.
      * MJD 990105 ADDED GP-NEXT-BATCH-ID.
      * MJD 990126 ----- UPDATED IN FLEXGEN -----
      * MJD 990304 ADDED GP-REFNO-VERIFY
      * CS  1999.0315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLF 990322 ADDED GP-ACTIONCD-VERIFY TO DETERMINE IF TO TEST ENTERED
      *            OR CHANGED ACTION CODE FOR VALIDITY
      * MJD 990412 ADDED GP-BP-NEXT-BATCH
      *  CS 990414 ADDED GP-IB-ACCRUE-EARN-ON-REPO (WHEN ACC UPGRADED THEY
      *            WANTEDTO CONTINUE ACCRUAL UNTIL ACCOUNT CHARGED OFF
      * MJD 990416 ADDED GP-DELINQ-WITH-O2
      * BAH 990503 ADDED GP-I-AM-REGACC REGIONAL ACCEPTANCE, NC
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * JTG 990618 ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC   (RGMG)
      * BLV 990624 ADDED GP-ALLOW-REPO-ON-PL, TFC PR#20, GR SUBURBAN PR#809
      * BAH 990701 ADDED GP-BK-PRIOR-TO-POSTING, TFC PR# 26
      * MJD 990719 ADDED GP-AMT-CASH-PROCEEDS. (REGIOAL MANAGEMENT #899)
      * MJD 990730 ADDED GP-INCL-OTH-IN-EARNED. (Y/N) REGENCY PR# 48
      * GLF 990804 ADDED GP-VALID-BDATE. (Y/N)
      * KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLF 990818 ADDED GP-ARCHIVE-DIRECTORY
      * BLV 990921 ADDED GP-DL-MEMO-TOGGLE
      * MJD 990924 ADDED GP-I-AM-MULL
      * MJD 991005 ADDED GP-RN-FOR10-LN (REGMGM PR#942)
      * KEC 991130 ADDED GP-LI-APPLICATION-FG; FOR CMSI/CSI PROJECT
      * JTG 000125 ADDED GP-I-AM-WISC, WISCONSION FINANCE, WI
      * MJD 000201 ADDED GP-BULLETIN-BOARD (WORLD)
      * MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48)
      * MJD 000209 EXPANDED GP-BULLETIN-BOARD  TO X(15)
      * MJD 000322 ADDED GP-UNEARNED-STATS (MERC PR#223)
      * JTG 000329 ADDED GP-PDTH-GT-SCHED-PAY MERCURY #J001
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLV 000417 ADDED GP-I-AM-REGENCY, REGWAN PR# 1141
      * MJD 000420 ADDED GP-LB-NEXT-BATCH FOR LOCK BOX PROCESSING
      *            (MERC PR#227)
      * GLB 000421 ADDED GP-I-AM-LIBERAL,  NEEDED FOR NOTES LIBERAL FIN
      * BAH 000503 ADDED GP-I-AM-NEWSOUTH, SPR FLAG TO BACKDATE LOANS, PR# 1179
      * BLV 000524 ADDED GP-RECDEL-REVERSAL-FG - WHETHER TO ALLOW
      *            LN-MTD-PAYS-RECDEL TO GO NEGATIVE
      * MJD 000608 ADDED GP-REF-PHONE (PR#37)
      *            ADDED GP-PL-SX-ONLLY (PR#77)
      * MJD 000619 ADDED GP-USE-CONRATE TO ALLOW/DISALLOW ENTRY USE
      *            OF CONTRACT RATE FIELD (REGACC PR#1220)
      * MJD 000628 ADDED GP-CPCORP-CURBAL (NEWS PR#1230)
      * BLV 000712 RENAMED GP-PL-SX-ONLY TO GP-ALLOW-PL-ON-SX
      * MJD 000816 ADDED GP-TIME-ZONE-Q. IF YES THEN COLLECTION QUEUE BY
      *            TIME ZONE  (TFC PR#25)
      * MJD 000817 ADDED GP-TIME-ZONE-FACTOR AND GP-TIME-ZONE-DLS.
      *            THE FACTOR REFERS TO HOW FAR WE ARE OFF FROM
      *            GREENWICH MEAN TIME (GMT) EX.
      *              EX:   EASTERN STD. TIME  = GMT -5
      *                    CENTRAL STD. TIME  = GMT -6
      *                    MOUNTAIN STD. TIME = GMT -7
      *                    PACIFIC STD. TIME  = GMT -8
      *                    ALASKA STD. TIME   = GMT -9
      *                    HAWAII STD. TIME   = GMT -10
      *            GP-TIME-ZONE-DLS TELLS WHETHER OR NOT DAYLIGHT SAVING TIME
      *            IS OBSERVED AT THE LOCATION OF THE MACHINE. (HAWAII,
      *            MOST OF ARIZONA AND THE EAST PORTION OF INDIANA
      *            DO NOT OBSERVE DAYLIGHT SAVING TIME)
      * MJD 000821 ADDED GP-CALL-START-TIME AND GP-CALL-END-TIME.
      *            THESE REFER TO THE TIMES BETWEEN WHICH IT IS LEGAL
      *            TO MAKE CALLS. USUALLY 0800 TO 2100. (TFC PR#25)
      * MJD 001002 ADDED GP-RV-MEMO. (WORLD AS PER JTG)
      * BLV 110900 ADDED GP-I-AM-WDT, WDT PR# 1340
      * MJD 001113 ADDED GP-PENALTY-INT (NEWS PR#1264)
      * JTG 001215 ADDED GP-ALLOW-SET-FOR-NONCASH, MERCURY #239
      * BAH 001222 ADDED GP-I-AM-SUNBELT, PR# 1387
      * BAH 010112 ADDED GP-I-AM-MODERN, ONLY NEEDED FOR UP ARROW IN PRIORITY
      *            MEMOS, 48 TO 15 UPGRADE WILMEMO
      * BAH 010305 ADDED GP-IGNORE-LCHG-NXDUE, WORLD PR# 254, TO IGNORE
      *            LATE CHARGES FOR NEXT DUE AMT (WHEN PAID AHEAD DUE TO LCHG)
      * MG  010404 ADDED GP-LA-OPTIONS-REC ( GP-KEY #5 )
      *
      * KEC 010405 {PR#00000} PARADATA FINANCIAL [KEN CISZEWSKI]
      *             CLEANED UP (COMMENTS) THE FILE LAYOUT.  ADDING
      *             '78' LEVELS DESCRIBING THE KEYS AND THE FILE MAX
      *             SIZE. DONE THIS BECAUSE WAS ADDING GP-LA-OPTIONS-REC
      *             ( GP-KEY #5 ).
      *
      * GLB 010502 ADDED GP-REVOLVE-MULTI(PLANS);GP-REVOLVE-SPECIAL
      * JTG 010507 ADDED GP-ALLOW-ENTRY-TRADEIN
      *            ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * JTG 010530 ADDED GP-TRUE-NEXT-BATCH                      MERC PR#243/251
      * CS  010607 ADDED GP-REPO-CODE     REGIONAL PR#1478
      * JTG 010720 ADDED GP-ALLOW-MORTGAGE-INT-Z      LENDMARK PR#1045 & PR#1380
      * BLV 010723 ADDED GP-PROM-AMT-MIN-PCT, PR# 1489, DEVELOPMENT
      * CS  010822 ADDED GP-BW-PRIVACY-OPT-OUT  PR#1571 SUPPORT
      * JTG 010912 CHANGED ENTRY OF 'O2' ON P&L TO STOP UPDATE OF GL MERC #260
      *            ADDED GP-NO-GL-ON-O2-WHEN-PL
      * BAH 011102 ADDED GP-INTERIM-COUPONS FOR REGACC PR# 1632
      * JTG 011112 ADDED MONEYGRAM FILE OPTION GP-MGRAM-NEXT-BATCH   MERC PR#263
      * BLV 011128 ADDED GP-RESCHDULE-MEMO, UNITED PR# 1661
      * JTG 011219 ADDED OPTION FOR 'SS' GP-DONT-COLL-INT-ON-SS   REGACC PR#1684
      * GLB 020104 ADDED GP-DL-ACH-FG PER REGACC PR# 1691 DIRECT DEPOSIT
      * BLV 020215 ADDED GP-GL-NEW-JE OPTION FOR NEW JOURNAL ENTRY MODULE
      * BAH 020227 ADDED GP-CLEAR-PROM-PDTH TO NOT CLEAR PROMISE DATE FIELDS
      *            UNLESS PDTH ADVANCES, REGACC PR# 1750
      * BAH 020311 ADDED GP-I-AM-SOAUTO SOUTHER AUTOMOTIVE FINANCE
      * BAH 020319 ADDED GP-CREATE-TP-FORMS FOR MIDATL PR# 1757
      * BAH 020416 ADDED GP-CHECK-RECONCILIATION FOR REGMGM PR# 1726
      * BAH 020515 ADDED GP-CORP-BRNO FOR REGACC PR# 1800
      * BAH 020604 ADDED GP-I-AM-MIDATL MID ATLANTIC , KEVIN
      * BLV 020618 ADDED GP-I-AM-FFG, SOAC PR# 1834
      * BAH 020626 ADDED GP-PL-REPO-REASON , REGACC PR# 1755
      * JTG 020805 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *            ADDED GP-ALLOW-MIP-INSURANCE
      * BLV 020813 ADDED GP-ENTER-LPO-FG, SOAUTO PR# 1885, FLAGS WHETHER TO
      *            STOP FOR LPO, REP-CD & PROCESS CD IN LOAN ENTRY PROGRAMS
      * JTG 020905 ADDED LN-DLCOMM-03-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      *            ADDED GP-DLCOMM-CMSI-FG          
      * JTG 021011 ADDED GP-BATCH-PAY-REVISIONS FOR NEW BATCH PAY REVISIONS
      *            REGACC #0006
      * JTG 021018 ADDED GP-REFCD-NEXT-BATCH                        REGACC #0006
      * JTG 021028 CHANGED TO GP-REFCD-NEXT-BATCH 700-899           REGACC #0006
      * JTG 021202 REMOVED GP-BATCH-PAY-REVISIONS FOR NEW BATCH PAY REVISIONS
      *            REGACC #0006
      * BLV 030211 ADDED GP-EARLIEST-CLOSE-TIME, REGACC PR# 0018
      * BLV 030225 ADDED GP-GROUP-APPROVAL-FG AND GP-APPROVAL-MEMO, 
      *            WORLD PR# 315
      * JTG 030311 ADDED GP-FORCE-INPUT-PP-COVERAGE                   WORLD #319
      * JTG 030318 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      * BAH 030328 ADDED GP-LR-TRAILER-TRIAD, NEW LOAN TRAILER FOR REGACC PR#26
      *  CS 030411 ADDED GP-NF-UPD-REFNO (CUSTOMER CHECK/MO NUMBER ENTRY IN
      *            PAYMENT POSTING [CASHCO PR#2034]
      * BLV 030421 ADDED GP-EMPLOYEE-TIME-CARDS, CASHCO PR# 2045
      * BAH 031022 ADDED GP-DEFAULT-O3-INSOURS, LENDMARK PR# 2180
      * BLV 031107 REDEFINE FISCAL TABLE WITH MONTHS, DAYS & YEARS BROKEN OUT
      * BAH 031114 ADD GP-USE-BW-BAD-FLAGS, PR# 2155
      * BAH 031119 ADD GP-NON-FILE-FEECODE FOR WORLD SMITH REPLACEMENT
      * BAH 031120 ADD GP-CHBORR-PAST-DUE-DAYS FOR WORLD PL#337
      *            ENTER # DAYS, IF PAST DUE >, FORCE BORROWER CHANGES
      *            TO OPEN IN PAYMENT POSTING
      * JTG 031215 CHANGED GL UPDATE TO TEST NEW GLOBAL PARAMETER FLAG
      *            GP-CLOSED-FISCAL-FG, TO DETERMINE IF GL FISCAL PERIOD
      *            HAS BEEN CLOSED. IF CLOSED STOP UPDATE.           WORLD #J040
      * BAH 040311 ADD GP-BACKDATE-MEMO FOR A MEMO WINDOW WHEN BACKDATING
      *            ANY PAYMENT OR PAYOFF, WORLD 344
      * JTG 040407 ADDED GP-OTREF-NEXT-BATCH  900-909              CENTRIX #2254
      * JTG 040519 ADDED GP-NEW-REPO-UNEARNED-ACCTS
      *            NEW GL CONTROL FIELDS FOR:
      *                - UNEARNED INTEREST REPO
      *                - UNEARNED ADDON INTEREST REPO
      *            THE UNEARNED WILL MOVE TO THESE ACCOUNTS POSTING A 'SX'
      *            AND REVERSING A 'SV'
      *            THE UNEARNED WILL MOVE (BACK) FROM THESE ACCOUNTS POSTING
      *            A 'SV' AND REVERSING A 'SX'                    REGACC #0049
      * BAH 040824 ADDED GP-BK-FLAG-CONFIDENTIAL TO AUTOMATICALLY FLAG 
      *            LOAN AS CONFIDENTIAL WHEN POST A "BK", AND FORCE IT BACK
      *            TO "N" WHEN A "BD" OR REVERSE "BK" IS DONE, LENDMARK PR#2364
      * JTG 041014 ADDED GP-AMORTIZE-ACQUIST-FEE, THIS OPTION WILL TAKE THE
      *            AMOUNT ENTERED IN RESERVE PRINCIPAL 3 AND MOVE IT TO
      *            THE LOANS UNEARNED SERVICE CHARGE               REGACC #0081 
      * BAH 041026 ADDED GP-I-AM-MIDWEST, PR# 2397, FOR O2MORP CHANGE
      *
      * KEC 041206 {PR#02333} THRIFT INVESTMENT <A15> [KEVIN MULLEN]
      *            ADDED GP-I-AM-THRIFT
      * BAH 050119 ADDED GP-DENY-FORM-NO-ADDRESS, REGACC PR# 94
      * KEC 050323 {PR#02489} PREMIER ADVISORY <A15> [JUDY]
      *             ADDED GP-I-AM-PREMIER-ADVISORY   VALUE "PADV".
      * BLF 050418 ADDED GP-I-AM-WAGNER, WAGNER PL#478
      *  CS 050502 ADDED GP-I-AM-ANNARBOR, ANNARBOR PR#2522 
      * BAH 050713 ADDED GP-HIDE-FIRST-5-SSNO FOR REGACC PR# 109
      * KEC 050726 {PR#02567} BAY COUNTRY <A15> [JUDY]
      *            ADDED GP-I-AM-BAY-COUNTRY
      * BAH 050914 ADDED GP-DEFERMENT-APPROVAL, LENDMARK PR# 2568
      * BLF 051024 ADDED GP-INCLUDE-O2-IN-BAL, PR# 2623
      * BAH 060214 ADDED GP-I-AM-AUTOMOTIVE-CREDIT "ACC ", PR# 2704
      * BLF 060224 ADDED GP-LOAN-ACH-PROCESSING LENDMARK PR# 2711
      * BAH 060313 ADDED GP-I-AM-FUTURE PR# 2735
      * BAH 060407 ADDED GP-I-AM-UNION PR# 2756, UNION FINANCE
      * BAH 060510 ADDED GP-I-AM-SUN-FINANCE PR# 2785, SUN FINANCE, LA
      *  CS 060522 ADDED GP-I-AM-SECURITY    PR#2503 SECURITY
      * BLF 060711 ADDED GP-PAYEE-PROCESSING-FG, DEVELOPMENT PR# 2633
      *  CS 060815 ADDED GP-I-AM-COSMO       PR#549 COSMO FRESNO
      * BAH 060911 ADDED GP-I-AM-PCCREDIT  
      * BAH 060920 ADDED GP-AUTO-BANKRUPT-WINDOW, WORLD PR# 493.
      * KEC 060921 {PR#00149} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED 'GP-BW-SCORES-QUARTER-SUB'
      *  CS 061005 ADD GP-POST-XX-BF-PAYMENTS(CREATE PRE-BALANCE FORWARD)
      *            PAYMENT HISTORY ON CONVERTED/BULK ACCOUNTS [PR#2659]
      *                                                      SERVICE FINANCE 
      * BAH 061113 ADDED GP-VALIDATE-ENDORSER-AGE FOR LOAN INPUT, COLTN PL#558
      * KEC 061211 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *            REMOVED REFERENCE OF KEY #05 FOR NEW LOAN APPLICATION
      *            SETTINGS.  CREATED NEW AO-FILE FOR THIS.
      * BAH 070104 ADDED GP-ALLOW-MANUAL-CHECKS FOR LOANS, WORLD PR#513
      * BAH 070307 ADDED GP-EXPANDED-FOLLOW-UP FOR TRITON, PR# 2937
      * BLF 070322 ADDED GP-NEXT-EMPLOYEE-NO FOR WORLD, PR# 488
      * BLF 070411 ADDED GP-USE-RESULT-WORKED-FG, SUPPORT PL# 584
      * BAH 070418 ADDED GP-I-AM-UNITED, UNITED PR# 2959
      *  CS 070625 ADDED GP-MAX-PROM-DAYS TRITION PR#2992
      * BLF 070808 ADDED GP-PURGE-TO-ARCHIVE-FG, WORLD PR# 547
      * BLF 080111 ADDED GP-EDIT-BORROWER-NAME-FG, WORLD PR# 564
      * JTG 080129 ADDED GP-I-AM-FINAUTO,  PR# 3141
      * BAH 080212 ADDED GP-RECAP-CLASS-TOTALS, HAMPTON PR# 3150
      *  CS 080325 ADDED GP-I-AM-CASHCO PER JR MEMO       
      *  CS 080623 ADDED GP-BRING-AUTO-INS-ON-RENEWAL PR#3177 CITIZENS FIN
      * MJD 080922 ADDED GP-ALLOW-MF-ON-IB            PR#590 WORLD
      *  CS 081104 ADD 'R' TO GP-INCL-OTH-IN-EARNED. IF 'R', WILL NOT 
      *            INCLUDE OTHER(6) OR MAINTFEE(4) IN EARNED. PR#210 REGACC
      * BAH 090805 ADDED GP-USE-COLLECT-LETTER-DAYS FOR REGACC PR# 251
      * BAH 091005 ADDED GP-DISPLAY-BAD-ADR-FLAG FOR REGACC PR# 256
      * BAH 091209 ADDED GP-DAYS-IDENT-NEW-PAPER FOR REGACC TO TEST WHAT TO
      *            DISPLAY ON COLLECT WORKSCREEN PR# 262 REGACC
      * BLM 100203 ADDED GP-ESCROW-FG, REGWAN PR# 3506
      * BLM 101012 ADDED GP-SECURITY-FG, "Y" = NEW SECURITY, "N" OR " " =
      *            OLD SECURITY, PR# NEWSECURITY
      * BAH 110211 ADDED GP-GPS-INFORMATION FOR GPS PROCESSING PCCREDIT PR# 3699
      * BLM 110216 ADDED GP-CUST-INFO-FG, WORLD PR# 739
      *  CS 110218 ADDED GP-USE-ADDRESS-BW-SCAN MODERN PR#3720
      * BAH 110223 ADDED GP-EXPANDED-LEGAL-WINDOW UNITED ACC PR# 3707
      * BAH 110328 ADDED GP-USE-SCRA (SERVICE MEMBER CIVIL RELIEF ACT)
      *                                                        REGACC PR# 289
      *  CS 111012 ADDED GP-USE-ACCRUAL-EARN-FILE REGACC PR#299
      * BAH 111017 ADDED GP-I-AM-CARFIN, CAROLINA FINANCE PR# 3887
      * BLM 120111 ADDED GP-I-AM-CITIZENS, CITIZENS FINANCE, IOWA (CITI),
      *            TRIST PR# 3944
      * BLM 120201 ADDED GP-I-AM-PEOPLES, PEOPLES CREDIT, ILLINOIS (PEPL),
      *            PEOPLES PR# 3963
      * KEC 120223 {PR#00000} PARADATA <A15> [JIM KRUSE]
      *            ADDED GP-MESSAGE-ATTR SO TO GET ATTRIBUTES ON THE
      *            MESSAGE FIELD IN ACCOUNT INQUIRY, PAYMENT POSTING AND
      *            COLLECTIONS WORK SCREEEN.
      * AMM 120316 {PR#00000} PARADATA <A15> [JAYKEN]
      *            ADDED KEY#5 LOAN-APPS-OPTIONS
      * BAH 120404 ADDED GP-JUDGMENT-WINDOW CITIZENS PR# 3990
      * BAH 121005 ADDED GP-I-AM-AMER-ACC "AMAC" AMERICAN ACCEPT PR# 4117
      * BLM 130401 ADDED GP-ADD-INFO-REC, KEY 6, FOR ADDITIONAL INFO, BANK 2
      *            FIELDS FOR ACH, CITIZENS PR# 4244
      * BAH 130521 ADDED GP-DUE-29-30-31 FOR SOAUTO PR# 4239
      * BLM 130618 ADDED GP-I-AM-GLFIN, GL FINANCIAL PR# 4308
      *  CS 131202 ADDED VALUE OF 'R' TO GP-USE-ACCRUAL-EARN-FILE. IF 'R', A
      *            PREPROCESSOR (CL/RAC347) WILL RUN THAT WILL UPDATE AEFILE
      *            AND LRFILE.  ONLY REGACC SHOULD RUN THE PREPROCESSOR
      *                                               REGACC  PR#347
      * BAH 131213 ADDED GP-I-AM-KSTREET,  PR# 4428
      * BAH 131216 ADDED GP-I-AM-BLACKHAWK,  PR# 4414
      *  CS 140115 ADDED GP-I-AM COLT, COLONIAL TN, PR#4453
      * BAH 140310 ADDED "MULL" AS GP-I-AM-KSTREET, THIS IS KSTREET DOING
      *            BUSINESS AS MULLEN, CANT LOSE OLD CUSTOM PROGRAMMING
      *            FOR MULLEN, SO I COMBINED THEM, PR#4492
      * BAH 141008 ADDED GP-DEFAULT-SKIP-TRW FOR NEW LOANS/SALES, COSMO PR#4629
      * BAH 150331 ADDED GP-I-AM-FIRST-METRO, PR#4671
      *  CS 150529 ADD GP-EARLIEST-OPEN-TIME REGACC PR#369
      * BAH 150810 ADD GP-TRANSFER-CONPROFILE REGMGM PR#4799
      *  CS 150812 ADD GP-I-AM-EXCEL  PR#4834 EXCEL
      * BAH 151030 ADD GP-I-AM-HONOR  PR#4998 HONOR
      * BAH 151208 ADD GP-I-AM-BARNETT PR#4914 BARNETT
      * BLM 160415 ADD GP-BATCH-REFCD-DUP-DAYS PR#5048 SUPPORT
      * BAH 171114 ADDED GP-I-AM-CFSC FOR GAPEXT, PR#5328
      * BAH 230302 ADDED GP-TEST-PP-AUTO-INSCOVR #1594
      * BAH 230419 REMOVED GP-TEST-PP-AUTO-INSCOVR, PUT IN BRANCH  #1594A
      * BAH 20231019 ADDED GP-CREATE-PAY-XTRP-XNO FOR REPAY PAYMENTS
      *              ALWAYS CREATE XTRP AND XNO IN FTP/OUT/PAYS FOR EVERY
      *              TRANSACTION #1629
      **************************************************************************
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       01  GP-REC.

      ******************************************************************
      *
      *    K E Y # 0 1 --- O P T I O N S
      *
      *-----------------------------------------------------------------
      *    DESC: STANDARD GLOBAL PARAMETER OPTIONS
      *    KEY#: 01   [78  GP-KEY-OPTIONS   VALUE 1.]
      ******************************************************************
           03  GP-GLOBAL-REC.
               05  GP-BEG-FISCALX.
                   07  GP-BEG-FISCAL     PIC 9(6).
               05  FILLER               REDEFINES GP-BEG-FISCALX.
                   07  GP-BEG-FISCAL-MO  PIC 99.
                   07  FILLER            PIC X(4).
               05  GP-END-FISCAL-TABLE.
                   07  GP-END-FISCAL    OCCURS 12
                                         PIC 9(6).
               05  FILLER               REDEFINES GP-END-FISCAL-TABLE.
                   07  GP-END-FISCAL-MMDDYY OCCURS 12.
                       09  GP-END-FISCAL-MM PIC 99.
                       09  GP-END-FISCAL-DD PIC 99.
                       09  GP-END-FISCAL-YY PIC 99.
               05  GP-BANK-GL               PIC 9(11).
               05  GP-BANK-CLEARING-GL      PIC 9(11).
               05  GP-BANK-ROUTING-NO       PIC 9(9).
               05  GP-BANK-ACCTNO           PIC 9(15).
               05  GP-TR-RETAIN-MONTHS      PIC 99.
               05  GP-FED-TAX-ID            PIC 9(9).
               05  GP-CORP-NAME             PIC X(25).
               05  GP-SHORT-NAME            PIC X(3).
               05  GP-BANK-NAME             PIC X(23).
               05  GP-BANK-DEPOSIT-GL       PIC 9(11).
               05  GP-AGE-BUCKET-CD         PIC X.
                   88  GP-AGE-BUCKET-CD-VALID      VALUE " " "A".
               05  GP-THE-ONLY-SF-SRCD      PIC XX.
               05  GP-ALLOW-BATCH-CHECKS    PIC X.
               05  GP-DLRVPRIN-3-FG         PIC X.
               05  GP-DLRVCHGS-3-FG         PIC X.
               05  GP-CASH-METHOD-ONLY      PIC X.
               05  GP-IB-ACCRUALS-DONE      PIC X.
               05  GP-EOD-RETAIN-MONTHS     PIC 99.
               05  GP-NO-LC-IN-ARREARAGE-FG  PIC X.
                   88 GP-NO-LC-IN-ARREARAGE VALUE " " "Y".
               05  GP-DEFAULT-O3-INS-COMPANYX.
                   07  GP-DEFAULT-O3-INS-COMPANY   PIC 99.
               05  GP-STOP-ON-PRIOR-FG      PIC X.
               05  GP-APPLICATION-FG        PIC X.
               05  GP-DEFAULT-ACCRATE       PIC 999V999.
               05  GP-DLR-DISP-TYPE         PIC X.
               05  GP-ALLOW-RLOGINS         PIC X.
               05  GP-AUTO-REPO-WINDOW      PIC X.
               05  GP-ALLOW-PI-WITH-SE-BAL  PIC X.
               05  GP-ALLOW-MORE-COLLECT    PIC X.
               05  GP-BATCH-MORE-THAN-DAY   PIC X.
               05  GP-PRINT-GL-RECAP        PIC X.
               05  GP-LOAN-CONVERSION-UPD-FG  PIC X.
               05  GP-FORM-CHG-CD           PIC X.
               05  GP-PROGRP-PRINT-REDEEM   PIC X.
               05  GP-BW-HOUSING-CODE       PIC X.
               05  GP-BW-CREDIT-SCORE       PIC X.
               05  GP-DISPLAY-BANK-BAL      PIC X.
               05  GP-PROGRP-DL-CONT        PIC X.
               05  GP-DLRVPRIN-2-FG         PIC X.
               05  GP-DLRVPRIN-1-FG         PIC X.
               05  GP-ADV-TO-1ST-NO-CORR    PIC X.
               05  GP-COMPANY-FLAG          PIC X(4).
                   88  GP-COMPANY-VALID    VALUE "    " "WRLD" "MERC"
                                                 "RAC " " RAC" "TFC"
                                                 "RGAC" "RGMG" "MULL"
                                                 "WISC" "AMTH" "REGC"
                                                 "LIBL" "NEWS" "WDT "
                                                 "SUNB" "MODN" "SOAU"
                                                 "MIDA" "FFG " "CREA"
                                                 "PACE" "MIDW" "TINV"
                                                 "PADV" "WAGN" "AA  "
                                                 "BAYC" "ACC " "FUTR"
                                                 "UFIN" "SUNL" "SAL "
                                                 "NSA " "COSM" "PEOP"
                                                 "UNIT" "FIAU" "CASH"
                                                 "CARF" "CITI" "PEPL"
                                                 "AMAC" "GLF " "KSTR"
                                                 "BHWK" "COLT" "FMET" 
                                                 "EXCL" "HONR" "BARN"
                                                 "MAS " "CFSC".
                   88  GP-I-AM-WORLD       VALUE "WRLD".
                   88  GP-I-AM-MERC        VALUE "MERC".
                   88  GP-I-AM-WDT         VALUE "WDT ".
      * MULLEN FINANCE, CALIFORNIA
                   88  GP-I-AM-MULL        VALUE "MULL" "KSTR".
      * RELIANCE ACCEPTANCE, TEXAS
                   88  GP-I-AM-RAC         VALUE "RAC " " RAC".
                   88  GP-I-AM-TFC         VALUE "TFC".
      * REGIONAL ACCEPTANCE, GREENVILLE NC
                   88  GP-I-AM-REGACC      VALUE "RGAC".
      * REGIONAL MANAGEMENT, GREENVILLE SC
                   88  GP-I-AM-REGMGM      VALUE "RGMG".
      * WISCONSIN ACCEPTANCE, WI
                   88  GP-I-AM-WISC        VALUE "WISC".
      * AMERICAN THRIFT, LA
                   88  GP-I-AM-AMERICAN    VALUE "AMTH".
      * REGENCY FINANCE, PA
                   88  GP-I-AM-REGENCY     VALUE "REGC".
      * LIBERAL FINANCE, NJ
                   88  GP-I-AM-LIBERAL     VALUE "LIBL".
      * LENDMARK, GA (NEW SOUTH)
                   88  GP-I-AM-LENDMARK    VALUE "NEWS".
      * NEW SOUTH, GA
                   88  GP-I-AM-NEWSOUTH    VALUE "NEWS".
      * SUNBELT, SC
                   88  GP-I-AM-SUNBELT     VALUE "SUNB".
      * MIDWEST, MO
                   88  GP-I-AM-MIDWEST     VALUE "MIDW".
      * MODERN, MI
                   88  GP-I-AM-MODERN      VALUE "MODN".
      * SOUTHERN AUTOMOTIVE FINANCE, FL
                   88  GP-I-AM-SOAUTO      VALUE "SOAU".
      * MIDATLANTIC
                   88  GP-I-AM-MIDATL      VALUE "MIDA".
      * FLORIDA FINANCE GROUP
                   88  GP-I-AM-FFG         VALUE "FFG ".
      * CREATIVE FINANCE
                   88  GP-I-AM-CREATIVE    VALUE "CREA".
      * PACESETTER FINANCE
                   88  GP-I-AM-PACESETTER   VALUE "PACE".
      * THRIFT INVESTMENT
                   88  GP-I-AM-THRIFT       VALUE "TINV".
      * PREMIER ADVISORY
                   88  GP-I-AM-PREMIER-ADVISORY            VALUE "PADV".
      * WAGNER 
                   88  GP-I-AM-WAGNER       VALUE "WAGN".
      * ANN ARBOR
                   88  GP-I-AM-ANNARBOR     VALUE "AA  ".
      * BAY COUNTRY
                   88  GP-I-AM-BAY-COUNTRY  VALUE "BAYC".
      * AUTOMOTIVE CREDIT CORP
                   88  GP-I-AM-AUTOMOTIVE-CREDIT  VALUE "ACC ".
                   88  GP-I-AM-FUTURE       VALUE "FUTR".
                   88  GP-I-AM-UNION        VALUE "UFIN".
      * SUN FINANCE, LA
                   88  GP-I-AM-SUN-FINANCE  VALUE "SUNL".
      * SECURITY AUTO LOAN
                   88  GP-I-AM-SECURITY     VALUE "SAL ".
      * NORTHSTATE ACCEPTANCE
                   88  GP-I-AM-NORTHSTATE   VALUE "NSA ".
      * COSMO (FRESNO)
                   88  GP-I-AM-COSMO        VALUE "COSM".
      * PEOPLES CREDIT, OR PCCREDIT
                   88  GP-I-AM-PCCREDIT     VALUE "PEOP".
      * UNITED
                   88  GP-I-AM-UNITED       VALUE "UNIT".
      * FINANCIAL AUTO SERVICES
                   88  GP-I-AM-FINAUTO      VALUE "FIAU".
      * CASHCO
                   88  GP-I-AM-CASHCO       VALUE "CASH".
      * CAROLINA FINANCE
                   88  GP-I-AM-CARFIN       VALUE "CARF".
      * CITIZENS FINANCE IOWA
                   88  GP-I-AM-CITIZENS     VALUE "CITI".
      * PEOPLES CREDIT, ILLINOIS
                   88  GP-I-AM-PEOPLES      VALUE "PEPL".
      * AMERICAN ACCEPTANCE
                   88  GP-I-AM-AMER-ACC     VALUE "AMAC".
      * GL FINANCIAL
                   88  GP-I-AM-GLFIN        VALUE "GLF ".
      * KSTREET DBA MULLEN
                   88  GP-I-AM-KSTREET      VALUE "KSTR" "MULL".
      * BLACKHAWK FINANCE
                   88  GP-I-AM-BLACKHAWK    VALUE "BHWK".
      * COLONIAL, TN
                   88  GP-I-AM-COLTN        VALUE "COLT".
      * FIRST METRO, TN
                   88  GP-I-AM-FIRST-METRO  VALUE "FMET".
                   88  GP-I-AM-EXCEL        VALUE "EXCL".
                   88  GP-I-AM-HONOR        VALUE "HONR".
                   88  GP-I-AM-BARNETT      VALUE "BARN".
                   88  GP-I-AM-MAS          VALUE "MAS ".
                   88  GP-I-AM-CFSC         VALUE "CFSC".

               05  GP-INTERBR-AR-AP-FULLGL  PIC X.
               05  GP-SKIP-BI-UPDATE        PIC X.
               05  GP-ASK-HARD-COPY-MEMOS   PIC X.
               05  GP-RLOGIN-LOCAL-PROFILE  PIC X.
               05  GP-RLOGIN-REMOTE-PROFILE  PIC X.
               05  GP-REPODATE-PAYDATE      PIC X.
               05  GP-POST-INTO-FUTURE      PIC X.
               05  GP-RENEWAL-COLLATERAL    PIC X.
               05  GP-PROGRP-CLASSES.
                   07  GP-PROGRP-LN-CLASS   PIC 99 OCCURS 3.
                   07  GP-PROGRP-SF-CLASS   PIC 99 OCCURS 4.
               05  GP-RENEW-DISC-UNERN      PIC X.
               05  GP-NOTES-EOD-BATCH-ID    PIC X(3).
               05  GP-ALLOW-REVOLVING-FG    PIC X.
               05  GP-NEXT-NOTE             PIC 9(6).
               05  GP-NEXT-BATCH-ID         PIC 9(5).
               05  GP-REFNO-VERIFY          PIC X.
               05  GP-ACTIONCD-VERIFY       PIC X.
               05  GP-BP-NEXT-BATCH         PIC 9(3).
                   88 GP-BP-NEXT-BATCH-VALID   VALUE 100 THRU 199.
      *            -----------------------------------------------
               05  GP-IB-ACCRUE-EARN-ON-REPO   PIC X.
               05  GP-DELINQ-WITH-O2        PIC X.
               05  GP-ALLOW-REPO-ON-PL      PIC X.
               05  GP-BK-PRIOR-TO-POSTING   PIC X.
               05  GP-AMT-CASH-PROCEEDS     PIC 9(6)V99.
               05  GP-INCL-OTH-IN-EARNED    PIC X.
                   88  GP-INCL-OTH-IN-EARNED-N VALUE "N".
                   88  GP-INCL-OTH-IN-EARNED-R VALUE "R".
               05  GP-VALID-BDATE           PIC X.
               05  GP-ARCHIVE-DIRECTORY     PIC X(4).
               05  GP-DL-MEMO-TOGGLE        PIC X.
               05  GP-RN-FOR10-LN           PIC X.
               05  GP-LI-APPLICATION-FG     PIC X(01).
               05  GP-BULLETIN-BOARD        PIC X(15).
               05  GP-OT-INS-TERM           PIC X(01).
               05  GP-UNEARNED-STATS        PIC X(01).
               05  GP-PDTH-GT-SCHED-PAY     PIC X.
               05  GP-LB-NEXT-BATCH         PIC 9(3).
                   88 GP-LB-NEXT-BATCH-VALID   VALUE 200 THRU 499.
      *            -----------------------------------------------
               05  GP-RECDEL-REVERSAL-FG    PIC X.
               05  GP-REF-PHONE             PIC X(01).
               05  GP-ALLOW-PL-ON-SX        PIC X(01).
               05  GP-USE-CONRATE           PIC X(01).
               05  GP-CPCORP-CURBAL         PIC X(01).
               05  GP-TIME-ZONE-Q           PIC X(01).
               05  GP-TIME-ZONE-FACTOR      PIC S9(02).
               05  GP-TIME-ZONE-DLS         PIC X(01).
               05  GP-CALL-START-TIME       PIC 9(04).
               05  GP-CALL-END-TIME         PIC 9(04).
               05  GP-RV-MEMO               PIC X(01).
               05  GP-PENALTY-INT           PIC X(01).
               05  GP-ALLOW-SET-FOR-NONCASH PIC X.
               05  GP-IGNORE-LCHG-NXDUE     PIC X.
               05  GP-REVOLVE-MULTI         PIC X.
               05  GP-REVOLVE-SPECIAL       PIC X.
               05  GP-ALLOW-ENTRY-TRADEIN   PIC X.
               05  GP-TRUE-NEXT-BATCH            PIC 9(3).
                   88 GP-TRUE-NEXT-BATCH-VALID   VALUE 500 THRU 599.
      *            -----------------------------------------------
               05  GP-REPO-CODE             PIC X.
                   88  GP-REPO-CODE-VALID        VALUE " " "A".
               05  GP-ALLOW-MORTGAGE-INT-Z  PIC X.
               05  GP-PROM-AMT-MIN-PCT      PIC 9(3).
               05  GP-BW-PRIVACY-OPT-OUT    PIC X.
               05  GP-NO-GL-ON-O2-WHEN-PL   PIC X.
               05  GP-INTERIM-COUPONS       PIC X.
               05  GP-MGRAM-NEXT-BATCH           PIC 9(3).
                   88 GP-MGRAM-NEXT-BATCH-VALID  VALUE 600 THRU 699.
      *            -----------------------------------------------

               05  GP-RESCHEDULE-MEMO       PIC X.
               05  GP-DONT-COLL-INT-ON-SS   PIC X.
               05  GP-DL-ACH-FG             PIC X.
               05  GP-GL-NEW-JE             PIC X.
               05  GP-CLEAR-PROM-PDTH       PIC X.
               05  GP-CREATE-TP-FORMS       PIC X.
               05  GP-CHECK-RECONCILIATION  PIC X.
               05  GP-CORP-BRNO             PIC 9(4).
               05  GP-PL-REPO-REASON        PIC X.
1801           05  GP-ALLOW-MIP-INSURANCE   PIC X.
1885           05  GP-ENTER-LPO-FG          PIC X.
1846           05  GP-DLCOMM-CMSI-FG        PIC X.
0006           05  FILLER                   PIC X.
0006           05  GP-REFCD-NEXT-BATCH          PIC 9(3).
0006               88 GP-REFCD-NEXT-BATCH-VALID VALUE 700 THRU 899.
0006  *            -----------------------------------------------
               05  GP-EARLIEST-CLOSE-TIME       PIC 9(4).
               05  GP-GROUP-APPROVAL-FG         PIC X.
               05  GP-APPROVAL-MEMO             PIC X.
319            05  GP-FORCE-INPUT-PP-COVERAGE   PIC X.
307            05  GP-NO-SC-USE-DM              PIC X.
               05  GP-LR-TRAILER-TRIAD          PIC X.
               05  GP-NF-UPD-REFNO              PIC X(5) OCCURS 2.
               05  GP-EMPLOYEE-TIME-CARDS       PIC X.
               05  GP-DEFAULT-O3-INSOURS        PIC X.
               05  GP-USE-BW-BAD-FLAGS          PIC X.
               05  GP-NON-FILE-FEECODE          PIC XX.
               05  GP-CHBORR-PAST-DUE-DAYS      PIC 99.
J040           05  GP-CLOSED-FISCAL-TABLE.
J040               07  GP-CLOSED-FISCAL-FG     OCCURS 12
J040                                            PIC X.
               05  GP-BACKDATE-MEMO             PIC X.
2254           05  GP-OTREF-NEXT-BATCH          PIC 9(3).
2254               88 GP-OTREF-NEXT-BATCH-VALID VALUE 900 THRU 909.
2254  *            -----------------------------------------------

0049           05  GP-NEW-REPO-UNEARNED-ACCTS   PIC X.
               05  GP-BK-FLAG-CONFIDENTIAL      PIC X.
               05  GP-AMORTIZE-ACQUIST-FEE      PIC X.
               05  GP-DENY-FORM-NO-ADDRESS      PIC X.
               05  GP-HIDE-FIRST-5-SSNO         PIC X.
               05  GP-DEFERMENT-APPROVAL        PIC X.
               05  GP-INCLUDE-O2-IN-BAL         PIC X.
               05  GP-LOAN-ACH-PROCESSING       PIC X.
               05  GP-PAYEE-PROCESSING-FG       PIC X.
               05  GP-AUTO-BANKRUPT-WINDOW      PIC X.
               05  GP-BW-SCORES-QUARTER-SUB     PIC 9(01).
               05  GP-POST-XX-BF-PAYMENTS       PIC X.
               05  GP-VALIDATE-ENDORSER-AGE     PIC X.
               05  GP-ALLOW-MANUAL-CHECKS       PIC X.

               05  GP-EXPANDED-FOLLOW-UP        PIC X.
               05  GP-NEXT-EMPLOYEE-NO          PIC 9(6).
               05  GP-USE-RESULT-WORKED-FG      PIC X.
               05  GP-MAX-PROMISE-DAYS          PIC 9(2).
               05  GP-PURGE-TO-ARCHIVE-FG       PIC X.
               05  GP-EDIT-BORROWER-NAME-FG     PIC X.
               05  GP-RECAP-CLASS-TOTALS        PIC X.
               05  GP-BRING-AUTO-INS-ON-RENEWAL PIC X.
               05  GP-ALLOW-MF-ON-IB            PIC X.
               05  GP-USE-MIN-FORM-SENT-DAYS    PIC X.
               05  GP-DISPLAY-BAD-ADR-FLAG      PIC X.
               05  GP-DAYS-IDENT-NEW-PAPER      PIC 9(3).
               05  GP-ESCROW-FG                 PIC X.
               05  GP-SECURITY-FG               PIC X.
               05  GP-GPS-INFORMATION           PIC X.
               05  GP-CUST-INFO-FG              PIC X.
               05  GP-USE-ADDRESS-BW-SCAN       PIC X.
               05  GP-EXPANDED-LEGAL-WINDOW     PIC X.
               05  GP-USE-SCRA                  PIC X.
               05  GP-USE-ACCRUAL-EARN-FILE     PIC X.
                   88 GP-USE-ACCRUAL-EARN-FILE-Y VALUE "Y".
                   88 GP-USE-ACCRUAL-EARN-FILE-R VALUE "R".
                   88 GP-USE-ACCRUAL-EARN-FILE-N VALUE "N" " ".
               05  GP-MESSAGE-ATTR              PIC X.
                   88 GP-MESSAGE-ATTR-NONE      VALUE "N" " ".
                   88 GP-MESSAGE-ATTR-FLASH     VALUE "F".
                   88 GP-MESSAGE-ATTR-REVERSE   VALUE "R".
                   88 GP-MESSAGE-ATTR-BOTH      VALUE "B".
               05  GP-JUDGMENT-WINDOW           PIC X.
               05  GP-DUE-29-30-31              PIC X.
               05  GP-DEFAULT-SKIP-TRW          PIC X.
               05  GP-EARLIEST-OPEN-TIME        PIC 9(4).
               05  GP-TRANSFER-CONPROFILE       PIC X.
               05  GP-BATCH-REFCD-DUP-DAYS      PIC 9(3).
               05  GP-CREATE-PAY-XTRP-XNO       PIC X.
               05  FILLER                       PIC X(34).

      ******************************************************************
      *
      *    K E Y # 0 2 --- A C C O U N T S - P A Y A B L E
      *
      *-----------------------------------------------------------------
      *    DESC: ACCOUNTS PAYABLE SETTINGS
      *    KEY#: 02   [78  GP-KEY-ACCOUNTS-PAYABLE   VALUE 2.]
      ******************************************************************
           03  GP-AP-REC                  REDEFINES GP-GLOBAL-REC.
               05  GP-AP-CHECK-ID          PIC 99.
               05  GP-AP-PO-FLAG           PIC X.
               05  GP-AP-CHECK-ADDR-SKIP   PIC 9.
               05  GP-AP-CHECK-LINES       PIC 99.
               05  GP-AP-PASSWORD1         PIC X(6).
               05  GP-AP-CHECK-FLAG        PIC X.
               05  GP-AP-CHECKNO           PIC 9(6).
               05  GP-AP-POST-TS           PIC X.
               05  GP-AP-LOC-PAY           PIC X.
               05  GP-AP-FORCE-GL          PIC X.
               05  GP-AP-NEXT-VENDORNO     PIC 9(6).
               05  GP-AP-DEFAULT-LOC       PIC 9(4).
               05  GP-AP-DEFAULT-TERMS     PIC 99.
               05  GP-AP-FORCE-FISCAL      PIC X.
               05  GP-AP-NEXT-MISCNO       PIC 9(6).
               05  GP-AP-GL-POST-FG        PIC X.
               05  GP-AP-CUTSHEET          PIC X.
               05  FILLER                  PIC X(469).

      ******************************************************************
      *
      *    K E Y # 0 3 --- L O A N - A P P L I C A T I O N S
      *
      *-----------------------------------------------------------------
      *    DESC: LOAN APPLICATION INFORMATION FOR DISPLAYING PROMPTS
      *    KEY#: 03   [78  GP-KEY-LOAN-APPLICATIONS   VALUE 3.]
      ******************************************************************
           03  GP-LOAN-APPLICATIONS-REC    REDEFINES GP-GLOBAL-REC.
               05  GP-CONTRACT-CHECKLIST-PROMPTS            PIC X(15)
                                                            OCCURS 14.
               05  GP-USERDEFINED-PROMPTS                   PIC X(15)
                                                            OCCURS 20.
                   88  GP-USERDEFINED-PROMPT-SPACES         VALUE
                                                      "               ".
                   88  GP-USERDEFINED-PROMPT-EMPTY          VALUE
                                                      "...............".
               05  FILLER                                   PIC X(2).

      ******************************************************************
      *
      *    K E Y # 0 4 --- R E V O L V I N G - R A T E S
      *
      *-----------------------------------------------------------------
      *    DESC: REVOLVING RATE INFORMATION FOR REVOLVING LOANS
      *    KEY#: 04   [78  GP-KEY-REVOLVING-RATES   VALUE 4.]
      ******************************************************************
           03  GP-RATES-REC                     REDEFINES GP-GLOBAL-REC.
               05  GP-RATES-TABLE               OCCURS 2 TIMES.
                   07  GP-RATES-DATE            PIC 9(6)      OCCURS 21.
                   07  GP-RATES-RATE            PIC 999V999   OCCURS 21.
               05  FILLER                       PIC X(8).

      ******************************************************************
      *
      *    K E Y # 0 5 --- L O A N - A P P S - O P T I O N S
      *
      *-----------------------------------------------------------------
      *    DESC: LOAN APPLICATION INFORMATION FOR DISPLAYING OPTIONS
      *    KEY#: 05   [78  GP-KEY-LOAN-APPLICATIONS   VALUE 5.]
      ******************************************************************
           03  GP-LOAN-APPS-OPTIONS-REC         REDEFINES GP-GLOBAL-REC.
               05  GP-LA-PAGES-TABLE            OCCURS 25 TIMES.
                   07  GP-LA-PAGE-NEW           PIC X(01).
                   07  GP-LA-PAGE-ACCESS        PIC X(01).
               05  FILLER                       PIC X(462).
      ******************************************************************
      *    KEY # 6 --- ADDITIONAL GP INFO, 2ND BANK INFO
      ******************************************************************
           03  GP-ADD-INFO-REC                  REDEFINES GP-GLOBAL-REC.
               05  GP-BANK2-INFO.
                   07  GP-BANK2-ROUTING-NO      PIC 9(9).
                   07  GP-BANK2-ACCTNO          PIC 9(15).
                   07  GP-FED-TAX-ID2           PIC 9(9).
                   07  GP-BANK2-NAME            PIC X(23).
                   07  GP-CORP-NAME2            PIC X(25).
               05  FILLER                       PIC X(431).
