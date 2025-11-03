      * COPYMEMBER: LIBLA/LA01LA
      ******************************************************************
      *
      *  MEMBER NAME: LIBLA/LA01LA                             RSZ=4500
      *
      *  DESCRIPTION: LOAN APPLICATION FILE
      *
      *-----------------------------------------------------------------
      *  REV:
      *  KEC 020397 REVISED KEY STRUCTURE OF LA-FILE.
      *  KEC 041697 REVISED LA-YARD-AMT FROM S9(3)V9(3) TO S9(7)V99.
      *  KEC 052297 ADDED EXTRA FILLER (560) RSZ=3500(WAS-2940)
      *  BLV 090597 ADDED LA-LICENSE-STAT, LA-CO-LICENSE-STAT
      *  KEC 090597 ADDED LA-AUTO-VALUATION-PROMPTS AND
      *                   LA-ACCT-TYPE.
      *             REVISED LA-YARD-* (YARDSTICK) TO LA-AUTO-VALUATION.
      *  KEC 091097 REVISED:
      *              CHANGED LA-VERIFY-FORMER TO LA-VERIFY-OTHER;
      *              CHANGED LA-AV-ITEM-TOTAL TO FILLER;
      *              CHANGED LA-BR-MGR-COMM3  TO LA-USERDEFINED-A;
      *              CHANGED LA-SUP-COMM3     TO LA-USERDEFINED-B;
      *              CHANGED LA-SUP-APP-COND3 TO LA-USERDEFINED-C/FILLER
      *  MJD 111297 CHANGED LA-CK-* TO LA-CHECKLIST(1-14)
      *  KEC 022598 REVISED THE ENTIRE FILE LAYOUT BEFORE THE WORLD
      *              CONVERSION.  REMOVED THE MIDDLE FILLER, GROUPED THE
      *              FIELDS TOGETHER, ADDED TABLES FOR PREVIOUS ADDRESS
      *              AND EMPLOYMENT, REVISED THE CREDITORS TABLE.
      *              RSZ=4500 (WAS=3473)
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MG  2001.1203 ADDED LA-APR AND LA-DL-FEE PR#1670.
      * MG  2001.0606 ADDED LA-ETHNIC AND LA-CO-ETHNIC PR#2670.
      *
      * KEC 2003.1013 {PR#02142} LENDMARK <A15> [KEVIN MULLEN]
      *               {PR#02171} LENDMARK <A15> [KEVIN MULLEN]
      *               {PR#02179} LENDMARK <A15> [KEVIN MULLEN]
      *               REDEFINED LA-CREDITORS (OCCURS 11) AREA TO ADDED
      *               NEW FIELDS FOR THESE REQUESTS.
      *
      * KEC 2003.1130 {PR#00088} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ADDED 78 LA-SIZE   VALUE 4500            RSZ=4500
      *                     03  LA-LICENSE-STATE      PIC X(02).
      *                     03  LA-CO-LICENSE-STATE   PIC X(02).
      *
      ******************************************************************
       78  LA-SIZE                                           VALUE 4500.
      *=================================================================
       01  LA-REC.
           03  LA1-KEY.
               05  LA-APPNO                     PIC 9(06).
           03  LA2-KEY.
               05  LA-SSNO                      PIC 9(09).
               05  LA-APPDATE                   PIC 9(07)        COMP-3.
           03  LA4-KEY.
               05  LA-DLNO                      PIC 9(04)V99     COMP-3.
               05  LA3-KEY.
                   07  LA-LNAME                 PIC X(16).
                   07  LA-FNAME                 PIC X(16).

      *=================================================================
      *    
      *    BORROWER INFORMATION (INFO. NOT IN 'BW' FILE)
      *    
      *=================================================================
           03  LA-BANK                          PIC X(10).
           03  LA-CHECK-SAVINGS                 PIC X(01).
           03  LA-CO-DRIVERLIC                  PIC X(15).
           03  LA-CO-LICENSE-STAT               PIC X(01).
           03  LA-DEPENDENTS                    PIC 9(02).
           03  LA-DRIVERLIC                     PIC X(15).
           03  LA-LICENSE-STAT                  PIC X(01).
           03  LA-MARITAL-STAT                  PIC X(01).

      *------------------------------------------------ EXPENSE ANALYSIS
           03  LA-NET-MONTHLY-INCOME            PIC S9(7)V99     COMP-3.
           03  LA-TOTAL-MONTHLY-INDEBT          PIC S9(7)V99     COMP-3.

      *------------------------------------------------------ REFERENCES
           03  LA-REFERENCES                    OCCURS 5 TIMES.
               05  LA-REF-HOME-PHONE            PIC 9(10)        COMP-3.
               05  LA-REF-NAME                  PIC X(25).
               05  LA-REF-RELATIONSHIP          PIC X(10).
               05  LA-REF-WORK-PHONE            PIC 9(10)        COMP-3.

      *=================================================================
      *    
      *    EMPLOYMENT INFORMATION
      *    
      *=================================================================
           03  LA-EMPLOYERS                     OCCURS 6 TIMES.
               05  LA-EMP-ADR1                  PIC X(24).
               05  LA-EMP-ADR2                  PIC X(24).
               05  LA-EMP-CITY                  PIC X(15).

               05  LA-EMP-DATE.
                   07  LA-EMP-BEG-DATE          PIC 9(04).
                   07  LA-EMP-BEG-DATEX         REDEFINES
                                                LA-EMP-BEG-DATE.
                       09  LA-EMP-BEG-DATE-MM   PIC 9(02).
                       09  LA-EMP-BEG-DATE-YY   PIC 9(02).

                   07  LA-EMP-END-DATE          PIC 9(04).
                   07  LA-EMP-END-DATEX         REDEFINES
                                                LA-EMP-END-DATE.
                       09  LA-EMP-END-DATE-MM   PIC 9(02).
                       09  LA-EMP-END-DATE-YY   PIC 9(02).

               05  LA-EMP-EMPLOYER              PIC X(24).
               05  LA-EMP-PAY                   PIC 9(06)V99     COMP-3.
               05  LA-EMP-PAY-WEEK-MONTH        PIC X(01).

               05  LA-EMP-PHONE-INFO.
                   07  LA-EMP-PHONE             PIC 9(10)        COMP-3.
                   07  LA-EMP-EXT               PIC 9(04)        COMP-3.

               05  LA-EMP-POSITION              PIC X(24).
               05  LA-EMP-STATE                 PIC X(02).
               05  LA-EMP-SUPERVISOR            PIC X(24).

               05  LA-EMP-TYPE                  PIC X(01).
                   88  LA-EMP-TYPE-CURRENT      VALUE "C".
                   88  LA-EMP-TYPE-EMPTY        VALUE " ".
                   88  LA-EMP-TYPE-JOINT        VALUE "J".
                   88  LA-EMP-TYPE-PREVIOUS     VALUE "P".
                   88  LA-EMP-TYPE-VALID        VALUE "C" " " "J" "P".

               05  LA-EMP-ZIP                   PIC X(10).

      *=================================================================
      *    
      *    GENERAL LOAN APPLICATION INFORMATION
      *    
      *=================================================================
           03  LA-COVR-VERIF-BY                 PIC X(54).
           03  LA-COVR-VERIF-DATE               PIC 9(06).
           03  LA-DLR-CASH-DOWN                 PIC S9(7)V99     COMP-3.
           03  LA-GROSS-CHARGE                  PIC S9(7)V99     COMP-3.
           03  LA-INSTALLMENTS                  PIC S9(7)V99     COMP-3.
           03  LA-INSUR-COMPANY                 PIC X(25).
           03  LA-INSUR-COVR                    PIC S9(7)V99     COMP-3.
           03  LA-NET-ALLOWANCE                 PIC S9(7)V99     COMP-3.
           03  LA-NOTE-AMOUNT                   PIC S9(7)V99     COMP-3.
           03  LA-OWE-ON-TRADE                  PIC S9(7)V99     COMP-3.
           03  LA-PAYABLE-IN                    PIC S9(3)        COMP-3.
           03  LA-RATE-CHART-USED               PIC X(01).
           03  LA-SELL-PRICE                    PIC S9(7)V99     COMP-3.
           03  LA-TOTAL-DOWN                    PIC S9(7)V99     COMP-3.
           03  LA-TRADE-IN                      PIC S9(7)V99     COMP-3.
           03  LA-UNPAID-BALANCE                PIC S9(7)V99     COMP-3.

      *------------------------------------------ CO-MAKER INVESTIGATION
           03  LA-CO-VERIFY-COMMENTS            PIC X(40).
           03  LA-CO-VERIFY-DATE-HIRED          PIC 9(06).
           03  LA-CO-VERIFY-POSITION            PIC X(24).
           03  LA-CO-VERIFY-SALARY              PIC S9(7)V99     COMP-3.

      *--------------------------------------------- MAKER INVESTIGATION
           03  LA-VERIFY-ADDR-1                 PIC X(79).
           03  LA-VERIFY-ADDR-2                 PIC X(79).
           03  LA-VERIFY-COMMENTS               PIC X(40).
           03  LA-VERIFY-DATE-HIRED             PIC 9(06).
           03  LA-VERIFY-POSITION               PIC X(24).
           03  LA-VERIFY-OTHER                  PIC X(79).
           03  LA-VERIFY-SALARY                 PIC S9(7)V99     COMP-3.

      *-------------------------------------------- CREDITOR INFORMATION
      *
      *    IF ( LA-CREDITOR-TYPE-CLOSED ) THEN USE LA-CREDITOR-DATE-PAID
      *    IF ( LA-CREDITOR-TYPE-OPEN   ) THEN USE LA-CREDITOR-LAST-PAID
      *    IF ( LA-CREDITOR-TYPE-OPEN   ) THEN USE LA-CREDITOR-NEXT-DUE
      *    IF ( LA-CREDITOR-TYPE-OPEN   ) THEN USE LA-CREDITOR-SAME-ADD
      *
      *-----------------------------------------------------------------
           03  LA-CREDITORS-TABLE.
               05  LA-CREDITORS                 OCCURS 11 TIMES.
      * 1903 1997 2091 2185
                   07  LA-CREDITOR-ACCT-TYPE    PIC X(01).
                   07  LA-CREDITOR-BALANCE      PIC S9(7)V99     COMP-3.
                   07  LA-CREDITOR-DATE-OPEN    PIC 9(06).
                   07  LA-CREDITOR-HI-CREDIT    PIC 9(06).
      * 1921 2015 2109 2203 2297
                   07  LA-CREDITOR-HOW-PAID     PIC X(10).

                   07  LA-CREDITOR-DATE-PAID    PIC 9(06).
                   07  LA-CREDITOR-LAST-PAID    REDEFINES
                                                LA-CREDITOR-DATE-PAID
                                                PIC 9(06).

                   07  LA-CREDITOR-NAME         PIC X(24).
                   07  LA-CREDITOR-NEXT-DUE     PIC 9(06).
                   07  LA-CREDITOR-PAYAMT       PIC S9(7)V99     COMP-3.
                   07  LA-CREDITOR-PHONE        PIC 9(10).
                   07  LA-CREDITOR-SAME-ADD     PIC X(01).
                   07  LA-CREDITOR-SECURITY     PIC X(10).
                   07  LA-CREDITOR-TERM         PIC 9(03).
                   07  LA-CREDITOR-TYPE         PIC X(01).
                       88  LA-CREDITOR-TYPE-OPEN              VALUE "O".
                       88  LA-CREDITOR-TYPE-CLOSED            VALUE "C".

      *-----------------------------------------------------{ LA-LEI-* }
      *    THE FOLLOWING IS A REDEFINE OF LA-CREDITOR-TABLE FOR
      *    LENDMARK (NEWS) NEEDS.
      *-----------------------------------------------------------------
       78  SET-LA-LEI-ID-APPLICANT-MAX          VALUE 2.
       78  SET-LA-LEI-ID-TYPE-MAX               VALUE 2.
       78  SET-LA-LEI-ORIGIN-APPL-MAX           VALUE 2.
       78  SET-LA-LEI-ORIGIN-MAX                VALUE 5.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LA-LENDMARK-EXTRA-INFO REDEFINES LA-CREDITORS-TABLE.

               05  LA-LEI-ORIGIN-TABLE.
                   07  LA-LEI-ORIGIN-APPLICANT  OCCURS
                                             SET-LA-LEI-ORIGIN-APPL-MAX.
                       09  LA-LEI-ORIGIN-ELE    OCCURS
                                                SET-LA-LEI-ORIGIN-MAX
                                                PIC X(04).

               05  LA-LEI-ID-VERIFICATION-TABLE.
                   07  LA-LEI-ID-VERIFY-APPLICANT OCCURS
                                            SET-LA-LEI-ID-APPLICANT-MAX.
                       09  LA-LEI-ID-VERIFY-TYPE  OCCURS
                                                 SET-LA-LEI-ID-TYPE-MAX.
                           11  LA-LEI-ID-VERIFY-DOC     PIC X(10).
                           11  LA-LEI-ID-VERIFY-NUMBER  PIC X(15).
                           11  LA-LEI-ID-VERIFY-EFFDATE PIC 9(6) COMP-3.
                           11  LA-LEI-ID-VERIFY-EXPDATE PIC 9(6) COMP-3.
                           11  LA-LEI-ID-VERIFY-ISSUED  PIC X(10).

               05  FILLER                       PIC X(822).

      *-------------------------------------------- CONTRACT INFORMATION
           03  LA-CONTRACT-INFORMATION.
               05  LA-AMT-FINANCED              PIC S9(7)V99     COMP-3.
               05  LA-CASH-DOWN                 PIC S9(7)V99     COMP-3.
               05  LA-CASH-PRICE                PIC S9(7)V99     COMP-3.
               05  LA-FINANCE-CHG               PIC S9(7)V99     COMP-3.
               05  LA-OTHER                     PIC S9(7)V99     COMP-3.
               05  LA-TOTAL-ADD-CHGS            PIC S9(7)V99     COMP-3.
               05  LA-TOTAL-PAYMENTS            PIC S9(7)V99     COMP-3.
               05  LA-TRADE-DOWN                PIC S9(7)V99     COMP-3.
               05  LA-TT-L                      PIC S9(7)V99     COMP-3.
               05  LA-UNPAID-BAL                PIC S9(7)V99     COMP-3.

           03  LA-CONTRACT-INFO-FEES.
               05  LA-AUTO-CLUB                 PIC S9(7)V99     COMP-3.
               05  LA-DOC-FEES                  PIC S9(7)V99     COMP-3.
               05  LA-WARRANTY                  PIC S9(7)V99     COMP-3.

           03  LA-CONTRACT-INFO-PREMIUMS.
               05  LA-AH-PREM                   PIC S9(7)V99     COMP-3.
               05  LA-CL-PREM                   PIC S9(7)V99     COMP-3.

      *------------------------------------------- APPROVAL AND COMMENTS
           03  LA-BR-MGR-APPROVAL               PIC X(10).
           03  LA-BR-MGR-COMM1                  PIC X(70).
           03  LA-BR-MGR-COMM2                  PIC X(70).
           03  LA-BR-MGR-TURNDOWN               PIC X(10).
           03  LA-CRED-BUREAU-SUMM1             PIC X(30).
           03  LA-CRED-BUREAU-SUMM2             PIC X(30).
           03  LA-CRED-BUREAU-SUMM3             PIC X(30).
           03  LA-SUP-APP-COND1                 PIC X(70).
           03  LA-SUP-APP-COND2                 PIC X(70).
           03  LA-SUP-APPROVAL                  PIC X(10).
           03  LA-SUP-APPROVAL-AMT              PIC S9(7)V99     COMP-3.
           03  LA-SUP-COMM1                     PIC X(70).
           03  LA-SUP-COMM2                     PIC X(70).
           03  LA-SUP-TURNDOWN                  PIC X(10).

      *------------------------------------ AUTO SALES FINANCE WORKSHEET
           03  LA-SFW-AMT-FINANCED              PIC S9(7)V99     COMP-3.
           03  LA-SFW-BR-PLAN                   PIC S9(3)V99     COMP-3.
           03  LA-SFW-COMM-INS                  PIC S9(7)V99     COMP-3.
           03  LA-SFW-COMM-WARR                 PIC S9(7)V99     COMP-3.
           03  LA-SFW-DISCOUNT                  PIC S9(7)V99     COMP-3.
           03  LA-SFW-DLR-CHK                   PIC S9(7)V99     COMP-3.
           03  LA-SFW-DOC-FEE                   PIC S9(7)V99     COMP-3.
           03  LA-SFW-FIN1                      PIC S9(5)V99     COMP-3.
           03  LA-SFW-FIN2                      PIC S9(3)V99     COMP-3.
           03  LA-SFW-FIN3                      PIC S9(7)V99     COMP-3.
           03  LA-SFW-MFC-INS                   PIC S9(7)V99     COMP-3.
           03  LA-SFW-MOTOR-CLUB                PIC S9(7)V99     COMP-3.
           03  LA-SFW-OTHER                     PIC S9(7)V99     COMP-3.
           03  LA-SFW-WARRANTY                  PIC S9(7)V99     COMP-3.
           03  LA-SFW-WARRANTY-CO               PIC X(20).

      *--------------------------------------------- AUTO VALUATION (AV)
           03  LA-AUTO-VALUATION.
               05  LA-AV-ADJUST-PERCT           PIC S9(3)V9(3)   COMP-3.
               05  LA-AV-ADJUST-VALUE           PIC S9(7)V99     COMP-3.
               05  LA-AV-BREAKDOWN.
                   07  LA-AV-ITEM-1             PIC S9(7)V99     COMP-3.
                   07  LA-AV-BASE     REDEFINES LA-AV-ITEM-1
                                                PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-2             PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-3             PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-4             PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-5             PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-6             PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-7             PIC S9(7)V99     COMP-3.
                   07  LA-AV-ITEM-8             PIC S9(7)V99     COMP-3.
               05  LA-AV-BREAKDOWN-TABLE        REDEFINES
                                                LA-AV-BREAKDOWN.
                   07  LA-AV-ITEMS              PIC S9(7)V99     COMP-3
                                                OCCURS 8 TIMES.

           03  LA-AUTO-VALUATION-PROMPTS.
               05  LA-AV-PROMPT                 PIC X(15)     OCCURS 7.

      *------------------------------------------------------- CHECKLIST
           03  LA-CHECKLIST-TABLE               OCCURS 14.
               05  LA-CHECKLIST                 PIC X(01).

      *--------------------------------------------- USER-DEFINED FIELDS
           03  LA-USERDEFINED-FIELDS.
               05  LA-USERDEFINED-01            PIC X(10).
               05  LA-USERDEFINED-02            PIC X(10).
               05  LA-USERDEFINED-03            PIC X(10).
               05  LA-USERDEFINED-04            PIC X(10).
               05  LA-USERDEFINED-05            PIC X(10).
               05  LA-USERDEFINED-06            PIC X(10).
      * 7 = PARTICIPATION GRADE
               05  LA-USERDEFINED-07            PIC X(10).
               05  LA-USERDEFINED-08            PIC X(10).
               05  LA-USERDEFINED-09            PIC X(10).
               05  LA-USERDEFINED-10            PIC X(10).
               05  LA-USERDEFINED-11            PIC X(10).
               05  LA-USERDEFINED-12            PIC X(10).
               05  LA-USERDEFINED-13            PIC X(10).
               05  LA-USERDEFINED-14            PIC X(10).
               05  LA-USERDEFINED-15            PIC X(10).
               05  LA-USERDEFINED-16            PIC X(10).
               05  LA-USERDEFINED-17            PIC X(10).
               05  LA-USERDEFINED-18            PIC X(10).
               05  LA-USERDEFINED-19            PIC X(10).
               05  LA-USERDEFINED-20            PIC X(10).

           03  LA-USERDEFINED-TABLE   REDEFINES LA-USERDEFINED-FIELDS.
               05  LA-USERDEFINED-TABLE2        OCCURS 20 TIMES.
                   07  LA-USERDEFINED-FIELD     PIC X(10).

      *=================================================================
      *    
      *    MORTGAGE INFORMATION
      *    
      *=================================================================
           03  LA-MORTGAGE-ADDR                 PIC X(40).
           03  LA-MORTGAGE-BAL                  PIC S9(7)V99     COMP-3.
           03  LA-MORTGAGE-HOLDER               PIC X(40).
           03  LA-MORTGAGE-PHONE                PIC 9(10)        COMP-3.

      *=================================================================
      *    
      *    RESIDENCE (ADDRESS) INFORMATION
      *    
      *=================================================================
           03  LA-EXPENSE-MTG-RENT              PIC X(01).
           03  LA-HOME-VALUE                    PIC S9(7)V99     COMP-3.
           03  LA-LIVE-WITH                     PIC X(10).
           03  LA-MO-HOME-EXPENSE               PIC S9(7)V99     COMP-3.
           03  LA-OWN-BUY-RENT-WHAT             PIC X(10).
           03  LA-TRAILER-OTHER                 PIC X(01).
           03  LA-YEARS-IN-AREA                 PIC 9(02).

      *-------------------------------------------- PREVIOUS INFORMATION
           03  LA-PREVIOUS-ADDRESS              OCCURS 3 TIMES.
               05  LA-PREVIOUS-ADR1             PIC X(24).
               05  LA-PREVIOUS-ADR2             PIC X(24).
               05  LA-PREVIOUS-CITY             PIC X(15).
               05  LA-PREVIOUS-HOW-LONG         PIC X(04).
               05  LA-PREVIOUS-STATE            PIC X(02).
               05  LA-PREVIOUS-ZIP              PIC X(10).

      *=================================================================
      *    
      *    MISCELLANEOUS INFORMATION
      *    
      *=================================================================
           03  LA-BRANCH                        PIC 9(04).
           03  LA-DATE                          PIC 9(06).
           03  LA-INDIV-JOINT                   PIC X(01).

           03  LA-APR                           PIC 9(03)V999    COMP-3.
           03  LA-DL-FEE                        PIC 9(04)V99     COMP-3.

           03  LA-ETHNIC                        PIC 9(01).
               88  LA-ETHNIC-NEWS-VALID         VALUE 1 THRU 5.
           03  LA-CO-ETHNIC                     PIC 9(01).
               88  LA-CO-ETHNIC-NEWS-VALID      VALUE 1 THRU 5.
      *-----------------------------------------------------------------
           03  LA-LICENSE-STATE                 PIC X(02).
           03  LA-CO-LICENSE-STATE              PIC X(02).
           03  FILLER                           PIC X(94).
