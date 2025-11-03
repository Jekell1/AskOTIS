      * COPYMEMBER: LIBSP/SPFDMETRO2
      **************************************************
      *               METRO 2 FORMAT
      *                                                
      *       (TRW CREDIT REPORTING FILE)         
      *
      *               TRWFILE RECORD                   
      *       STANDARD 426 CHARACTER RECORD LAYOUT     
      *
      *       
      * REV:
      *  040617 BAH REDEFINED TRW-B-IDENT-NO FOR LENDMARK PR# 2328
      *  2024.0823 BAH REDEFINED TRW-L1-NEW-IDENT FOR TRANSFERRED ACCOUNTS
      *                NEW BRANCH #1675 S35Q-122
      *  2025.0304 BAH ADDED TRW-H-INNOVIS-ID #1702 S35Q-169
      **************************************************
      $XFD  FILE=XMETRO2
       FD  TRW-FILE
           LABEL RECORDS ARE STANDARD.
       01  TRW-REC.
           03  FILLER                     PIC X(1364).
 
      *------------------------------------------------*
      *   HEADER RECORD - CHARACTER FORMAT
      *------------------------------------------------*
       01  TRW-HEADER-REC.
 
      * (1) RECORD DESCRIPTOR WORD (RDW)                           1 - 4
      * CONTAINS A VALUE EQUAL TO THE LENGTH OF 
      * THE LOGICAL RECORD
           03  TRW-H-RDW                    PIC 9(4).
 
      * (2) RECORD IDENTIFIER                                      5 - 10
      * CONTAINS THE CONSTANT = "HEADER"
           03  TRW-H-ID                     PIC X(6).
 
      * (3) CYCLE NUMBER                                          11 - 12
      * CONTAINS THE CYCLE NUMBER FOR THE INFO BEING
      * REPORTED, IF BY CYCLES. FOR GMAC WHO REPORTS DAILY
           03  TRW-H-CYCLE-NO               PIC X(2).

      * (4) INNOVIS PROGRAM IDENTIFIER                            13 - 22
           03  TRW-H-INNOVIS-ID             PIC X(10).
 
      * (5) EQUIFAX PROGRAM IDENTIFIER                            23 - 32
      * CONTAINS A UNIQUE IDENTIFICATION NUMBER
      * ASSIGNED BY THIS CREDIT AGENCY
           03  TRW-H-EQUIFAX-ID             PIC X(10).

      * (6) EXPERIAN PROGRAM IDENTIFIER                           33 - 37
      * CONTAINS A UNIQUE IDENTIFICATION NUMBER
      * ASSIGNED BY THIS CREDIT AGENCY
           03  TRW-H-EXPERIAN-ID            PIC X(5).

      * (7) TRANS UNION PROGRAM IDENTIFIER                        38 - 47
      * CONTAINS A UNIQUE IDENTIFICATION NUMBER
      * ASSIGNED BY THIS CREDIT AGENCY
           03  TRW-H-TRANSUNION-ID          PIC X(10).

      * (8) ACTIVITY DATE                                         48 - 55
      * SIGNIFIES DATE OF MOST RECENT UPDATE TO BALANCES
      * ON ACCOUNTS. FORMAT IS MMDDYYYY
           03  TRW-H-ACTIVITY-DATE          PIC 9(8).

      * (9) DATE CREATED                                          56 - 63
      * CONTAINS THE DATE THE MEDIA WAS GENERATED.
      * FORMAT IS MMDDYYYY
           03  TRW-H-DATE-CREATED           PIC 9(8).
 
      * (10) PROGRAM DATE                                          64 - 71
      * CONTAINS THE DATE YOUR REPORTING FORMAT WAS
      * DEVELOPED. FORMAT IS MMDDYYYY.
           03  TRW-H-PROGRAM-DATE           PIC 9(8).

      * (11) PROGRAM REVISION DATE                                 72 - 79
      * CONTAINS THE LAST DATE YOUR REPOTING FORMAT WAS
      * REVISED. FORMAT IS MMDDYYYY.
           03  TRW-H-REVISION-DATE          PIC 9(8).
 
      * (12) REPORTER NAME                                         80 - 119
      * CONTAINS THE NAME OF THE PROCESSING COMPANY SENDING
      * THE DATA. TOLD TO USE CUSTOMER
           03  TRW-H-REPORTER-NAME          PIC X(40).

      * (13) REPORTER ADDRESS                                     120 - 215
      * CONTAINS THE COMPLETE MAILING ADDRESS OF THE PROCESSING
      * COMPANY; STREET, CITY, STATE AND ZIP
           03  TRW-H-REPORTER-ADR           PIC X(96).
 
      * (14) REPORTER TELEPHONE NUMBER                            216 - 225
      * CONTAINS THE TELEPHONE NUMBER (AREA CODE +
      * NUMBER) OF THE COMPNAY SENDING THE DATA
           03  TRW-H-REPORTER-PHONE         PIC 9(10).

      * (15) SOFTWARE VENDOR NAME                                 226 - 265
      * CONTAINS THE NAME OF THE SOFTWARE VENDOR THAT
      * PROVIDED THE METRO 2 FORMAT SOFTWARE.
           03  TRW-H-SOFTWARE-VENDOR        PIC X(40).

      * (16) SOFTWARE VERSION NUMBER                              266 - 270
      * CONTAINS THE VERSION NUMBER OF THE METRO 2
      * FORMAT SOFTWARE
           03  TRW-H-SOFTWARE-VERSION       PIC X(5).

      * (17) RESERVED                                             271 - 426
      *                                                            RSZ 1364
           03  FILLER                       PIC X(1094).

      *------------------------------------------------*
      *        TRW BASE SEGMENT
      *------------------------------------------------*
       01  TRW-BASE-REC.
 
      * (1) RECORD DESCRIPTOR WORD (RDW)                           1 - 4
      * CONTAINS A VALUE EQUAL TO THE LENGTH OF 
      * THE LOGICAL RECORD
           03  TRW-B-RDW                 PIC 9(4).
 
      * (2) PROCESSING INDICATOR                                   5 - 5
      * CONTAINS A CODE THAT INDICATES A PROCESSING
      * INSTRUCTION FOR THE CREDIT AGENCIES.
      *   1 = FULL RECORD ( ALL FIELDS AVAILABLE)
      *   2 = ABBREVIATED RECORD (LIMITED FIELDS)
           03  TRW-B-PROC-INDICATOR      PIC 9.

      * (3) TIME STAMP                                            6 - 19
      * CONTAINS DATE AND TIME OF ACTUAL ACCOUNT 
      * INFO UPDATE   MMDDYYYYHHMMSS
           03  TRW-B-DATE-STAMP          PIC 9(8).
           03  TRW-B-TIME-STAMP          PIC 9(6).

      * (4) CORRECTION INDICATOR                                 20 - 20
      * USED TO REPLACE THE MOST RECENTLY REPORTED
      * UPDATE FOR THE SAME REPORTING TIME PERIOD.
      *  0 = NOT A REPLACEMENT (NORMAL UPDATE)
      *  1 = REPLACEMENT (CORRECTION)
      * ONLY THE RECORDS TO BE CORRECTED SHOULD BE
      * REPORTED ON THE MEDIA WHEN SET TO 1.
      * DO NOT MIX NORMAL UPDATES WITH CORRECTIONS.
      * MUST RECEIVE CORRECTIONS PRIOR TO NEXT UPDATE.
           03  TRW-B-CORRECTION          PIC 9.

      * (5) IDENTIFICATION NUMBER                                21 - 40
      * UNIQUELY IDENTIFY A CREDIT GRANTOR. REPORT YOUR
      * INTERNAL CODE TO IDENTIFY EACH BRANCH, OFFICE, AND/OR
      * CREDIT CENTRAL WHERE INFO IS VERIFIED. MUST BE
      * UNIQUE AND AT LEAST 5 DIGITS LONG. NEVER BE ZERO, 
      * BLANK OR 9 FILLED.
      * MUST BE CONSISTENT MONTH-TO-MONTH
           03  TRW-B-IDENT-NO            PIC X(20).
           03  TRW-B-IDENT-NOX REDEFINES TRW-B-IDENT-NO.
               05  TRW-B-IDENT-NO1       PIC 9(10).
               05  TRW-B-IDENT-NO2       PIC 9(10).
 
      * (6) CYCLE IDENTIFIER                                    41 - 42
      * REPORT THE INTERNAL CYCLE CODE FOR THIS ACCOUNT.
      * ONLY REQUIRED IF REPORTING BY CYCLES, OTHERWISE
      * BLANK FILL.
           03  TRW-B-CYCLE               PIC X(2).

      * (7) CONSUMER ACCOUNT NUMBER                             43 - 72
      * THE INDIVIDUAL'S COMPLETE AND UNIQUE ACCOUNT NUMBER
      * AS EXTRACTED FROM YOUR FILE. DO NOT INCLUDE EMBEDDED
      * BLANKS OR SPECIAL CHARACTERS.
           03  TRW-B-ACCTNOX             PIC X(15).
           03  TRW-B-ACCTNO              PIC 9(15).
           03  TRW-B-WORLD-REDEF REDEFINES TRW-B-ACCTNO.
               05  FILLER                  PIC X(3).
               05  TRW-B-W-BRNO            PIC 9(4).
               05  TRW-B-W-ACCT            PIC 9(8).
 
      * (8) PORTFOLIO TYPE                                      73 - 73
      * ABBREVIATION FOR TYPE OF PORTFOLIO
      * C = LINE OF CREDIT   I = INSTALLMENT
      * M = MORTGAGE         O = OPEN ACCOUNT
      * R = REVOLVING
           03  TRW-B-PTYPE                PIC X.
 
      * (9) ACCOUNT TYPE                                        74 - 75
      * IDENTIFIES THE ACCOUNT CLASSIFICATION.
      * EXHIBIT 1 PRVIDES A LISTING OF TYPE CODES
      * EXHIBIT 2 PROVIDES A DESCRIPTION IN NUMERIC SEQ
           03  TRW-B-ATYPE                PIC XX.

      * (10) DATE OPENED                                         76 - 83
      * DATE THE ACCOUNT WAS ORIGINALLY OPENED.
      * RETAIN THE ORIGINAL DATE OPENED REGARDLESS OF
      * FUTURE ACTIVITY SUCH AS TRANSFER OR REFINANCE
      * FORMAT IS MMDDYYYY
           03  TRW-B-DATE-OPENED         PIC 9(8).

      * (11) CREDIT LIMIT                                        84 - 92
      * LINE OF CREDIT = ASSIGNED CREDIT LIMIT
      * INSTALLMENT    = ZERO FILL
      * MORTGAGE       = ZERO FILL
      * OPEN           = ZERO FILL
      * REVOLVING      = ASSIGNED CREDIT LIMIT
      * WHOLE DOLLARS ONLY
           03  TRW-B-CREDITLIMIT         PIC 9(9).

      * (12) HIGHEST CREDIT OR ORIGINAL LOAN AMOUNT              93 - 101
      * LINE OF CREDIT = HIGHEST BALANCE EVER ATTAINED 
      * INSTALLMENT    = ORIGINAL AMT OF LOAN
      * MORTGAGE       = ORIG AMT OF LOAN LESS INTEREST PAYS
      * OPEN           = HIGHEST BALANCE EVER ATTAINED
      * REVOLVING      = HIGHEST BALANCE EVER ATTAINED
      * WHOLE DOLLARS ONLY
           03  TRW-B-AMT                 PIC 9(9).

      * (13) TERMS DURATION                                     102 - 104
      * LINE OF CREDIT = CONSTANT OF "LOC"
      * INSTALLMENT    = NUMBER OF MONTHS
      * MORTGAGE       = NUMBER OF YEARS
      * OPEN           = CONSTANT OF "001"
      * REVOLVING      = CONSTANT OF "REV"
      * EXHIBIT 3 PROVIDES THE CALCULATIONS NECESSARY
      * TO CONVERT TERMS DIRATION TO MONTHLY
           03  TRW-B-TERMS               PIC 9(3).
           03  TRW-B-TERMSX REDEFINES TRW-B-TERMS PIC X(3).

      * (14) TERMS FREQUENCY                                   105 - 105
      *  D = DEFERRED      P = SINGLE PAY LOAN
      *  W = WEEKLY        B = BIWEEKLY
      *  E = SEMIMONTHLY   M = MONTHLY
      *  L = BIMONTHLY     Q = QUARTERLY
      *  T = TRIANNUALLY   S = SEMIANNUALLY
      *  Y = ANNUALLY
      * EXHIBIT 3 PROVIDES DEFINITIONS OF THE TERMS
      * FREQ CODES
      * WHEN REPORTING DEFERRED, REPORT DEFERRED PAY
      * START DATE IN THE K4 SEGMENT
           03  TRW-B-TERMS-FREQ          PIC X.

      * (15) SCHEDULED MONTHLY PAYMENT AMOUNT                 106 - 114
      * DOLLAR AMOUNT OF THE SCHEDULED MONTHLY PAYMENT 
      * IN WHOLE DOLLARS ONLY.
      * LINE OF CREDIT = MIN AMT DUE BASED ON BALANCE, NOT
      *                  INCLUDING ANY AMTS PAST DUE
      * INSTALLMENT    = REGULAR MONTHLY PAYMENT
      * MORTGAGE       = REGULAR MONTHLY PAYMENT, INCLUDING
      *                  INTEREST, PRINCIPAL, AND ESCROW
      * OPEN           = ZERO FILL
      * REVOLVING      = MIN AMT DUE BASED ON BALANCE, NOT
      *                  INCLUDING ANY AMTS PAST DUE
           03  TRW-B-REGPYAMT            PIC 9(9).

      * (16) ACTUAL PAYMENT AMOUNT                            115 - 123
      * REPORT THE DOLLAR AMOUNT OF THE MONTHLY PAY
      * ACTUALLY RECEIVED FOR THIS REPORTING PERIOD IN
      * WHOLE DOLLARS ONLY
           03  TRW-B-ACTUAL-PAID         PIC 9(9).

      * (17A) ACCOUNT STATUS                                   124 - 125
      * CONTAINS THE STATUS CODE THAT PROPERLY IDENTIFIES
      * THE CURRENT CONDITION OF THE ACCOUNT AS OF THE
      * BILLING DATE
      * EXHIBIT 4 PROVIDES A DESCRIPTION
      * THE PAYMENT RATING MUST ALSO BE REPORTED WHEN
      * THIS STATUS IS 05, 13, 65, 88, 89, 94 OR 95.
      * SPECIAL COMMENTS MAY ALSO BE USED TO FURTHER DEFINE.
           03  TRW-B-STATUS              PIC X(2).
 
      * (17B) PAYMENT RATING                                   126 - 126
      * WHEN THE ACCOUNT STATUS CONTAINS 05, 13, 65,
      * 88, 89, 94 OR 95, THIS FIELD MUST ALSO BE REPORTED.
      * CONTAINS A CODE THAT PROPERLY IDENTIFIES WHETHER
      * THE ACCOUNT IS CURRENT OR PAST DUE WITHIN THE
      * ACTIVITY PERIOD BEING REPORTED.
      *  0 =  CURRENT             1 =  30 - 59 DAYS PAST
      *  2 =  60 - 89 DAYS PAST   3 =  90 - 119 DAYS PAST DUE
      *  4 = 120 - 149 DAYS PAST  5 = 150 - 179 DAYS PAST
      *  6 = 180 OR MORE
           03  TRW-B-PAY-RATING          PIC X.
 
 
      * (18) PAYMENT HISTORY PROFILE                          127 - 150
      * CONTAINS UP TO 24 MONTHS OF CONSECUTIVE PAYMENT
      * ACTIVITY FOR THE PREVIOUS 24 CALENDAR MONTHS 
      * PRIOR TO THE ACTIVITY DATE BEING REPORTED. REPORT ONE
      * MONTH'S PAYMENT RECORD IN EACH BYTE FROM THE LEFT TO RIGHT
      * IN MOST RECENT TO LEAST RECENT ORDER. THE FIRST BYTE
      * SHOULD REPRESENT THE PREVIOUS MONTH'S STATUS. 
      *  0 = 0 PAYMENTS PAST DUE (CURRENT)    1 =  30 - 59 DAYS
      *  2 =  60 - 89 DAYS                    3 =  90 - 119 DAYS
      *  4 = 129 - 149 DAYS                   5 = 150 - 179 DAYS
      *  6 = 180 OR MORE                      B = NO PAY HISTORY AVAIL
      *  D = NO PAY HISTORY AVAIL THIS MONTH  E = ZERO BAL AND CURRENT
      *  G = COLLECTION                       H = FORECLOSURE
      *  J = VOLUNTARY SURRENDER              K = REPO
      *  L = CHARGE-OFF
      * IF A FULL 24 MONTHS OF HISTORY ARE NOT AVAILABLE FOR REPORTING
      * THE ENDING POSITIONS OF THIS FIELD SHOULD BE FILLED WITH B'S.
      * EXHIBIT 5 HAS EXAMPLES
           03  TRW-B-HIST-PROFILE             PIC X(24).

      * (19) SPECIAL COMMENT                                    151 - 152
      * IN CONJUNCTION WITH ACCOUNT STATUS AND PAY RATING
      * EXHIBIT 6 PROVIDES A LIST OF AVAILABLE COMMENTS
      * EXHIBIT 7 IS DESCRIPTIONS 
           03  TRW-B-COMMENT                  PIC X(2).
 
      * (20) COMPLIANCE CONDITION CODE                          153 - 154
      * ALLOWS A CONDITION THAT IS REQUIRED FOR LEGAL COMPLIANCE
      * ACCORDING TO THE FCRA OR FCBA
      *  XA = ACCOUNT CLOSED AT CONSUMER'S REQUEST
      *  XB = ACCOUNT INFO DISPUTED BY CONSUMER - MEETS FCRA REQUIREMENTS
      *  XC = COMPLETED INVESTIGATION OF FCRA DISPUTE - CUSTOMER DISAGREES
      *  XD = ACCNT CLOSED AT CONSUMER'S REQUEST AND IN DISPUTE UNDER FCRA
      *  XE = ACCNT CLOSED/CONSUM REQUEST/DISPUTE INVEST DONE/CONSUMER DISAGREE
      *  XF = ACCOUNT IN DISPUTE UNDER FCBA (FAIR CREDIT BILLING ACT)
      *  XG = FCBA DISPUTE RESOLVED - CONSUMER DISAGREES
      *  XH = ACCNT PREVIOUSLY IN DISPUTE/RESOLVED, REPORTED BY CREDIT GRANTOR
      *  XJ = ACCNT CLOSED AT CONSUMER'S REQUEST AND IN DISPUTE UNDER FCBA
      *  XR = REMOVES THEMOST RECENTLY REPORTED COMPLIANCE CONDITION CODE
      * THIS CODE SHOULD BE REPORTED ONE TIME AND WILL BE DELETED ONLY WHEN
      * ANOTHER CODE OR THE "XR" IS REPORTED.
           03  TRW-B-CONDITION                PIC X(2).

      * (21) CURRENT BALANCE                                    155 - 163
      * REPORT THE TOTAL CURRENT BALANCE OF THE ACCOUNT IN
      * WHOLE DOLLARS ONLY. CREDIT BALANCES ARE ZERO.
           03  TRW-B-CURBAL              PIC 9(9).

      * (22) AMOUNT PAST DUE                                    164 - 172
      * REPORT THE AMOUNT PAST DUE IN WHOLE DOLLARS ONLY
      * THIS FIELD CAN INCLUDE LATE CHARGES AND FEES.
      * DO NOT INCLUDE CURRENT AMOUNT DUE IN THIS FIELD !!
      * IF STATUS IS CURRENT (11), THIS FIELD SHOULD BE ZERO.
           03  TRW-B-AMT-PASTDUE         PIC 9(9).

      * (23) ORIGINAL CHARGE-OFF AMOUNT                        173 - 181
      * FOR STATUS CODES 64 AND 97, REPORT THE ORIGINAL
      * AMT CHARGED TO LOSS,  REGARDLESS OF THE DECLINING
      * BALANCE. WHOLE DOLLARS ONLY
           03  TRW-B-ORIG-PLAMT          PIC 9(9).

      * (24) BILLING DATE                                      182 - 189
      * FOR ALL STATUS CODES, REPORT THE BILLING DATE
      * FOR THE ACCOUNT. MMDDYYYY
           03  TRW-B-BILL-DATE           PIC 9(8).

      * (25) FCRA COMPLIANCE/DATE OF FIRST DELINQUENCY         190 - 197
      * FOR STATUS CODES 61-65, 71, 78, 80, 82-84, 88-89,
      * 93-97, REPORT THE DATE OF THE FIRST DELINQUENCY THAT
      * LED TO THE STATUS BEING REPORTED. IF DELINQ THEN CURRENT, 
      * THEN DELINQ AGAIN, THE DATE OF FIRST DELINQ STARTS OVER
      * WITH THE NEW FIRST DELINQ DATE.
      * FOR STATUS CODES 05, 13, 65, 88, 89, 94, OR 95, IF THE
      * PAY RATING IS 1,2,3,4,5 OR 6, REPORT THE DATE OF THE FIRST
      * DELINQ THAT LED TO THE PAY RATING BEGING REPORTED.
      * MMDDYYYY
      * EXHIBIT 8 HAS DETAILED REPORTING INSTRUCTIONS
           03  TRW-B-DATE-DELINQ         PIC 9(8).
 
      * (26) DATE CLOSED                                       198 - 205
      * FOR ALL PORTFOLIO TYPES, CONTAINS THE DATE THE ACCOUNT WAS
      * CLOSED OR PAID. MMDDYYYY
      * IF NOT AVAILABLE, ZERO FILL
           03  TRW-B-DATE-CLOSED         PIC 9(8).

      * (27) DATE OF LAST PAYMENT                              206 - 213
      * REPORT THE DATE OF THE LAST CONSUMER PAYMENT. MMDDYYYY
      * IF NOT AVAILABLE, USE 01.
           03  TRW-B-DATE-LAST-PAY       PIC 9(8).
 
 
      * (28)                                                   214 - 230
           03  FILLER                    PIC X(17).
 
      * (29) CONSUMER TRANSACTION TYPE                         231 - 231
      * USED TO INDICATE A NEW RECORD, A NEW BORROWER, OR A CHANGE
      * IN CONSUMER IDENTIFICATION;
      *   1 = NEWLY OPENED ACCOUNT OR NEW BORROWER ASSOCIATED WITH
      *       EXISTING ACCOUNT
      *   2 = NAME CHANGE
      *   3 = ADDRESS CHANGE
      *   5 = SSNO CHANGE
      *   6 = NAME & ADDRESS CHANGE
      *   8 = NAME & SSNO CHANGE
      *   9 = ADDRESS & SSNO CHANGE
      *   A = NAME, ADDRESS AND/OR SSNO CHANGE

           03  TRW-B-CONSUMER-TRAN-TYPE  PIC X.
 
      * (30) SURNAME                                          232 - 256
      * REPORT THE LAST NAME OF THE PRMARY CONSUMER.
      * TITLES AND PREFIXES SHOULD NOT BE REPORTED

           03  TRW-B-LNAME               PIC X(25).
      * (31) FIRST NAME                                       257 - 276
           03  TRW-B-FNAME               PIC X(20).
      * (32) MIDDLE NAME                                      277 - 296
           03  TRW-B-MNAME               PIC X(20).
 
      * (33) GENERATION CODE                                  297 - 297
      * J = JUNIOR     5 = V     S = SENIOR     6 = VI
      * 2 = II         7 = VII   3 = III        8 = V88
      * 4 = IV         9 = IX
           03  TRW-B-GENERATION          PIC X.
 
      * (34) SOCIAL SECURITY NUMBER                           298 - 306
      * OF THE PRIMARY CONSUMER, ZERO OR 9 FILL
           03  TRW-B-SSNO                PIC 9(9).

      * (35) DATE OF BIRTH                                    307 - 314
      * FOR PRIMARY CONSUMER. MMDDYYYY, IF MONTH AND YEAR ARE
      * NOT AVAILABLE, USER 0101. 4 DIGIT YEAR IS REQUIRED
           03  TRW-B-BDATE               PIC 9(8).

      * (36) TELEPHONE NUMBER                                 315 - 324
      * ARE CODE + 7 DIGITS, ZERO FILL THE FIRST 3 IF NO AREA CODE
           03  TRW-B-PHONE               PIC 9(10).

      * (37) ECOA CODE                                        325 - 325
      * DEFINES THE RELATIONSHIP OF THE PRIMARY CONSUMER TO THE ACCOUNT
      * AND DESIGNATES THE ACOCUNT AS JOINT, INDIVIDUAL, ETC.
      * EQUAL CREDIT OPPORTUNITY ACT
      *   0 = UNDESIGNATED, DO NOT USE ON ACCOUNTS OPENED AFTER 6/1977
      *   1 = INDIVIDUAL - HAS PRIMARY RESPONSIBLE FOR PAYMENTS
      *   2 = JOINT CONTRACTUAL LIABILITY - BOTH CUSTOMER AND JOINT 
      *   4 = JOINT - SHARED BUT CANNOT BE MORE NARROWLY DEFINED BY CODE 2.
      *   6 = ON BEHALF OF - SECURE CREDIT FOR ANOTHER INDIVIDUAL
      *   7 = MAKER - ACCOUNT FOR WHICH SUBJECT IS LIABLE BUT A CO-MAKER 
      *       IS LIABLE IF MAKER DEFAULTS
      *   T = ASSOCIATION WITH ACOCUNT TERMINATED
      *   W = BUSINESS/COMMERCIAL
      *   X = CONSUMER DECEASED
      *   Z = DELETE BORROWER (ONLY INACCURATELY REPORTED CONSUMERS SHOULD
      *                        BE DELETED).
           03  TRW-B-ECOA-CODE           PIC X.

      * (38) CONSUMER INFORMATION INDICATOR              326 - 327
      * CONTAINS A VALUE THAT INDICATES A SPECIAL CONDITION OF THE ACCOUNT
      * THAT APPLIES TO THE PRIMARY CONSUMER.
      * THIS COULD BE A BANKRUPTCY WAS FILES, DISCHARGED, DISMISSED OR 
      * WITHDRAWN; A DEBT WAS REAFFIRMED; OR THE CONSUMER CANNOT BE LOCATED
      * OR IS NOW LOCATED.
      * THIS INDICATOR SHOULD BE REPORTED ONE TIME AND WILL BE DELETED ONLY
      * WHEN THE APPROPRIATE REMOVEL CODE (Q,S,U) IS REPORTED.
      * SEE EXHIBIT 9 FOR A LIST OF THESE INDICATORS.
           03  TRW-B-CONSUMER-INFO-IND   PIC X(2).

      * (39) COUNTRY CODE                               328 - 329
      * STANDARD 2 CHARACTER COUNTRY ABBREVIATION.
      * EXHIBIT 10
           03  TRW-B-COUNTRY             PIC X(2).

      * (40) FIRST LINE OF ADDRESS                      330 - 361
      * MAILING ADDRESS FOR THE PRIMARY CONSUMER
           03  TRW-B-ADR1                PIC X(32).
 
      * (41) SECOND LINE OF ADDRESS                     362 - 393
      * IF NEEDED, SUCH AS APARTMENT OR UNIT NUMBER
           03  TRW-B-ADR2                PIC X(32).
 
      * (42) CITY                                       394 - 413
      * CITY FOR ADDRESS OF PRIMARY CONSUMER
           03  TRW-B-CITY                PIC X(20).
 
      * (43) STATE                                      414 - 415
      * STANDARD U.S. POSTAL SERVICE STATE ABBREVIATION
      * EXHIBIT 12
           03  TRW-B-STATE               PIC X(2).
 
 
      * (44) POSTAL/ZIP CODE                            416 - 424
      * USE ENTIRE FIELD IF REPORTING 9 DIGIT ZIP, OR LEFT JUSTIFY
           03  TRW-B-ZIP                 PIC X(9).
 
      * (45) ADDRESS INDICATOR                          425 - 425
      * Y = KNOWN TO BE ADDRESS OF PRIMARY CONSUMER
      * N = NOT CONFIRMED ADDRESS
      * B = BUSINESS ADDRESS
      * U = NON-DELIVERABLE ADDRESS/RETURNED MAIL
      * D = DATA REPORTER'S DEFAULT ADDRESS
      * M = MILITARY ADDRESS
           03  TRW-B-ADD-IND             PIC X.
 
 
      *(46) RESIDENCE CODE                              426 - 426
      * O = OWNS    R = RENTS
           03  TRW-B-RESIDENCE           PIC X.
 
      *------------------------------------------------*
      * TRW APPENDAGE SEGMENT J1                       *
      *     ASSOCIATED CONSUMER - SAME ADDRESS         *
      *------------------------------------------------*
           03  TRW-J1-RECS             OCCURS 2 TIMES.
       
      *(1) SEGMENT IDENTIFIER                                  1 - 2
      *    CONSTANT = "J1"
     
               05  TRW-J1-ID                  PIC X(2).
 
      * (2) CONSUMER TRANSACTION TYPE                          3 - 3
      *  1 = NEWLY OPENED ACCOUNT OR NEW BORROWER ASSOCIATED WITH EXISTING
      *      ACCOUNT
      *  2 = NAME CHANGE
      *  3 = ADDRESS CHANGE
      *  5 = SSNO
      *  6 = NAME & ADDRESS CHANGE
      *  8 = NAME & SSNO CHANGE
      *  9 = ADDRESS & SSNO CHANGE
      *  A = NAME, ADDRESS AND/OR SSNO CHANGE
              05  TRW-J1-CONSUMER-TRAN-TYPE     PIC X.
 
      * (3)  SURNAME                                           4 - 28
      * LAST NAME OF THE ASSOCIATED CONSUMER.
              05  TRW-J1-LNAME                PIC X(25).

      * (4)  FIRST NAME                                        29 - 48
      * FIRST NAME OF THE ASSOCIATED CONSUMER.
              05  TRW-J1-FNAME                PIC X(20).

      * (5)  MIDDLE NAME                                       49 - 68
      * MIDDLE NAME OF THE ASSOCIATED CONSUMER.
              05  TRW-J1-MNAME                PIC X(20).
 
      * (6) GENERATION CODE                                    69 - 69
      * J = JUNIOR     5 = V     S = SENIOR     6 = VI
      * 2 = II         7 = VII   3 = III        8 = V88
      * 4 = IV         9 = IX
              05  TRW-J1-GENERATION          PIC X.

      * (7) SOCIAL SECURITY NUMBER                             70 - 78
              05  TRW-J1-SOCSEC              PIC 9(9).
 
      * (8) DATE OF BIRTH                                      79 - 86
      *      MMDDYYYY   IF MONTH AND DAY ARE NOT AVAILABLE, USE 0101
      *                 4 DIGIT YEAR IS REQUIRED
              05  TRW-J1-BDATE               PIC 9(8).
   
      * (9) PHONE                                              87 - 96
      *   AREA CODE + 7
              05  TRW-J1-PHONE               PIC 9(10).

      * (10) ECOA CODE                                        97 - 97
      * DEFINES THE RELATIONSHIP OF THE ASSOCIATED CONSUMER TO THE ACCOUNT
      * AND DESIGNATES THE ACOCUNT AS JOINT, INDIVIDUAL, ETC.
      * EQUAL CREDIT OPPORTUNITY ACT
      *   0 = UNDESIGNATED, DO NOT USE ON ACCOUNTS OPENED AFTER 6/1977
      *   1 = INDIVIDUAL - HAS PRIMARY RESPONSIBLE FOR PAYMENTS
      *   2 = JOINT CONTRACTUAL LIABILITY - BOTH CUSTOMER AND JOINT 
      *   4 = JOINT - SHARED BUT CANNOT BE MORE NARROWLY DEFINED BY CODE 2.
      *   6 = ON BEHALF OF - SECURE CREDIT FOR ANOTHER INDIVIDUAL
      *   7 = MAKER - ACCOUNT FOR WHICH SUBJECT IS LIABLE BUT A CO-MAKER 
      *       IS LIABLE IF MAKER DEFAULTS
      *   T = ASSOCIATION WITH ACOCUNT TERMINATED
      *   W = BUSINESS/COMMERCIAL
      *   X = CONSUMER DECEASED
      *   Z = DELETE BORROWER (ONLY INACCURATELY REPORTED CONSUMERS SHOULD
      *                        BE DELETED).
              05  TRW-J1-ECOA-CODE           PIC X.
 
      * (11) CONSUMER INFORMATION INDICATOR              98 - 99
      * CONTAINS A VALUE THAT INDICATES A SPECIAL CONDITION OF THE ACCOUNT
      * THAT APPLIES TO THE ASSOCIATED CONSUMER.
      * THIS COULD BE A BANKRUPTCY WAS FILES, DISCHARGED, DISMISSED OR 
      * WITHDRAWN; A DEBT WAS REAFFIRMED; OR THE CONSUMER CANNOT BE LOCATED
      * OR IS NOW LOCATED.
      * THIS INDICATOR SHOULD BE REPORTED ONE TIME AND WILL BE DELETED ONLY
      * WHEN THE APPROPRIATE REMOVEL CODE (Q,S,U) IS REPORTED.
      * SEE EXHIBIT 9 FOR A LIST OF THESE INDICATORS.
              05  TRW-J1-CONSUMER-INFO-IND   PIC X(2).
 
      * (12)                                             100 - 100
              05  FILLER                   PIC X.
 
 
      *------------------------------------------------*
      * TRW APPENDAGE SEGMENT J2                       *
      *     ASSOCIATED CONSUMER - DIFFERENT ADDRESS    *
      *------------------------------------------------*

           03  TRW-J2-RECS             OCCURS 2 TIMES.
      *(1) SEGMENT IDENTIFIER                                  1 - 2
      *    CONSTANT = "J2"
              05  TRW-J2-ID                  PIC X(2).
 
      * (2) CONSUMER TRANSACTION TYPE                          3 - 3
      *  1 = NEWLY OPENED ACCOUNT OR NEW BORROWER ASSOCIATED WITH EXISTING
      *      ACCOUNT
      *  2 = NAME CHANGE
      *  3 = ADDRESS CHANGE
      *  5 = SSNO
      *  6 = NAME & ADDRESS CHANGE
      *  8 = NAME & SSNO CHANGE
      *  9 = ADDRESS & SSNO CHANGE
      *  A = NAME, ADDRESS AND/OR SSNO CHANGE
              05  TRW-J2-CONSUMER-TRAN-TYPE     PIC X.
 
      * (3)  SURNAME                                           4 - 28
      * LAST NAME OF THE ASSOCIATED CONSUMER.
              05  TRW-J2-LNAME                PIC X(25).

      * (4)  FIRST NAME                                        29 - 48
      * FIRST NAME OF THE ASSOCIATED CONSUMER.
              05  TRW-J2-FNAME                PIC X(20).

      * (5)  MIDDLE NAME                                       49 - 68
      * MIDDLE NAME OF THE ASSOCIATED CONSUMER.
              05  TRW-J2-MNAME                PIC X(20).
 
      * (6) GENERATION CODE                                    69 - 69
      * J = JUNIOR     5 = V     S = SENIOR     6 = VI
      * 2 = II         7 = VII   3 = III        8 = V88
      * 4 = IV         9 = IX
              05  TRW-J2-GENERATION          PIC X.

      * (7) SOCIAL SECURITY NUMBER                             70 - 78
              05  TRW-J2-SOCSEC              PIC 9(9).
 
      * (8) DATE OF BIRTH                                      79 - 86
      *      MMDDYYYY   IF MONTH AND DAY ARE NOT AVAILABLE, USE 0101
      *                 4 DIGIT YEAR IS REQUIRED
              05  TRW-J2-BDATE               PIC 9(8).
   
      * (9) PHONE                                              87 - 96
      *   AREA CODE + 7
              05  TRW-J2-PHONE               PIC 9(10).

      * (10) ECOA CODE                                        97 - 97
      * DEFINES THE RELATIONSHIP OF THE ASSOCIATED CONSUMER TO THE ACCOUNT
      * AND DESIGNATES THE ACOCUNT AS JOINT, INDIVIDUAL, ETC.
      * EQUAL CREDIT OPPORTUNITY ACT
      *   0 = UNDESIGNATED, DO NOT USE ON ACCOUNTS OPENED AFTER 6/1977
      *   1 = INDIVIDUAL - HAS PRIMARY RESPONSIBLE FOR PAYMENTS
      *   2 = JOINT CONTRACTUAL LIABILITY - BOTH CUSTOMER AND JOINT 
      *   4 = JOINT - SHARED BUT CANNOT BE MORE NARROWLY DEFINED BY CODE 2.
      *   6 = ON BEHALF OF - SECURE CREDIT FOR ANOTHER INDIVIDUAL
      *   7 = MAKER - ACCOUNT FOR WHICH SUBJECT IS LIABLE BUT A CO-MAKER 
      *       IS LIABLE IF MAKER DEFAULTS
      *   T = ASSOCIATION WITH ACOCUNT TERMINATED
      *   W = BUSINESS/COMMERCIAL
      *   X = CONSUMER DECEASED
      *   Z = DELETE BORROWER (ONLY INACCURATELY REPORTED CONSUMERS SHOULD
      *                        BE DELETED).
              05  TRW-J2-ECOA-CODE           PIC X.
 
      * (11) CONSUMER INFORMATION INDICATOR              98 - 99
      * CONTAINS A VALUE THAT INDICATES A SPECIAL CONDITION OF THE ACCOUNT
      * THAT APPLIES TO THE ASSOCIATED CONSUMER.
      * THIS COULD BE A BANKRUPTCY WAS FILES, DISCHARGED, DISMISSED OR 
      * WITHDRAWN; A DEBT WAS REAFFIRMED; OR THE CONSUMER CANNOT BE LOCATED
      * OR IS NOW LOCATED.
      * THIS INDICATOR SHOULD BE REPORTED ONE TIME AND WILL BE DELETED ONLY
      * WHEN THE APPROPRIATE REMOVEL CODE (Q,S,U) IS REPORTED.
      * SEE EXHIBIT 9 FOR A LIST OF THESE INDICATORS.
              05  TRW-J2-CONSUMER-INFO-IND   PIC X(2).
 
      * (12) COUNTRY                                   100 - 101
              05  TRW-J2-COUNTRY            PIC X(2).
      * (13) FIRST LINE OF ADDRESS                     102 - 133
              05  TRW-J2-ADR1               PIC X(32).
      * (14) SECOND LINE OF ADDRESS                    134 - 165
              05  TRW-J2-ADR2               PIC X(32).
      * (15) CITY                                      166 - 185
              05  TRW-J2-CITY               PIC X(20).
      * (16) STATE                                     186 - 187
              05  TRW-J2-STATE              PIC X(2).
      * (17) POSTAL/ZIP CODE                           188 - 196
              05  TRW-J2-ZIP                PIC X(9).
      * (18) ADDRESS INDICATOR                         197 - 197
      * Y = KNOWN TO BE ADDRESS OF PRIMARY CONSUMER
      * N = NOT CONFIRMED ADDRESS
      * B = BUSINESS ADDRESS
      * U = NON-DELIVERABLE ADDRESS/RETURNED MAIL
      * D = DATA REPORTER'S DEFAULT ADDRESS
      * M = MILITARY ADDRESS
              05  TRW-J2-ADD-IND             PIC X.

      *(19) RESIDENCE CODE                             198 - 198
      * O = OWNS    R = RENTS
              05  TRW-J2-RESIDENCE           PIC X.
 
      * (20)                                           199 - 200
              05  FILLER                     PIC X(2).
 
      *------------------------------------------------*
      *  TRW APPENDAGE SEGMENT K1                      
      * ORIGINAL CREDITOR NAME
      * THE K1 SEGMENT MUST BE PRESENT EACH TIME THE ACCOUNT
      * IS REPORTED BY COLLECTION AGENCIES, DEBT COLLECTORS, 
      * FACTORING COMPANIES, STUDENT LOAN GUARANTY AGENCIES, AND THE
      * U.S. DEPT OF EDUCATION.
      *------------------------------------------------*

      *(1) SEGMENT IDENTIFIER                          1 - 2
      *    CONSTANT = "K1"
           03  TRW-K1-ID                  PIC X(2).

      * (2) ORIGINAL CREDITOR NAME                     3 - 32
      * THE ORIGINAL CREDITOR NAME IS THE NAME OF THE COMPANY
      * OR AGENT WHO ORIGINALLY OPENED THE ACCOUNT FOR THE CONSUMER.
           03  TRW-K1-CREDITOR            PIC X(30).

      * (3) CREDITOR CLASSIFICATION                    33 - 34
      * INDICATES GENERAL TYPE OF BUSINESS FOR THE ORIGINAL
      * CREDITOR.
      * 01-RETAIL, 02-MEDICAL, 03-OIL COMP, 04-GOVRNMENT,
      * 05-PERSONAL SERV, 06-INSURANCE, 07-EDUCATIONAL,
      * 08-BANKING, 09-RENTAL, 10-UTILITIES, 11-CABLE/CELLULAR,
      * 12-FINANCIAL, 13-CREDIT UNION, 14-AUTO, 15-CHECK GUARANTEE
           03  TRW-K1-CLASSIFICATION      PIC X(2).


      *------------------------------------------------*
      *  TRW APPENDAGE SEGMENT K2                      
      * PURCHASED PORTFOLIO/SOLD TO
      * TO REPORT THE NAME OF THE COMPANY FROM WHICH THE PORTFOLIO
      * OR PARTIAL PORTFOLIO WAS PURCHASED OR THE NAME TO WHICH
      * IT WAS SOLD
      *------------------------------------------------*
      *(1) SEGMENT IDENTIFIER                          1 - 2
      *    CONSTANT = "K2"
           03  TRW-K2-ID                  PIC X(2).

      *(2) PORTFOLIO INDICATOR                         3 - 3
      * CODE REPRESENTING THE TYPE OF INFO BEING REPORTED
      * 1=PORTFOLIO PURCHASED FROM NAME
      * 2=SOLD TO NAME
      * 9=REMOVE PREVIOUSLY REPORTED K2 SEGMENT INFO
           03  TRW-K2-INDICATOR           PIC X(1).

      *(3) PURCHASED PORTFOLIO OR SOLD TO NAME        4 - 33
      * CONTAINS A FREE-FORM DESCRIPTION OF THE NAME OF THE 
      * COMPANY FROM WHICH WAS PURCHASED OR SOLD.
      * IF FIELD 2 = 9, THIS SHOULD BE BLANK
           03  TRW-K2-NAME                PIC X(30).
 
           03  FILLER                     PIC X.

      *------------------------------------------------*
      *  TRW APPENDAGE SEGMENT K3                      
      * MORTGAGE INFO
      * TO INDICATE A SECONDARY MARKETING AGENCY'S INTEREST IN A LOAN
      * OR TO REPORT THE MORTGAGE INDENT NUMBER (MIN)
      *------------------------------------------------*
      *(1) SEGMENT IDENTIFIER                          1 - 2
      *    CONSTANT = "K3"
           03  TRW-K3-ID                  PIC X(2).

      *(2) AGENCY IDENTIFIER                           3 - 4
      * INDICATING WHICH SECONDARY MARKETING AGENCY HAS INTEREST
      * IN THIS LOAN.
      *  00=AGENCY IDENTIFIER NOT APPLICABLE (MIN ONLY)
      *  01=FANNIE MAE     02 = FREDDIE MAC
           03  TRW-K3-IDENT               PIC 9(2).

      *(3) ACCOUNT NUMBER                              5 - 22
      * ACCOUNT NUMBER AS ASSIGNED BY THE SECONDARY MARKETING
      * AGENCY.
      * IF FIELD 2 = 00, THIS SHOULD BE BLANK
           03  TRW-K3-ACCOUNT             PIC X(18).

      *(4) MORTGAGE INDENTIFICATION NUMBER (MIN)      23 - 40
      *REGISTERED WITH THE MORTGAGE ELECTRONIC REGISTRACTION
      * SYSTEM, INC. (MERS).
           03  TRW-K3-MIN                PIC X(18).

      *------------------------------------------------*
      *  TRW APPENDAGE SEGMENT K4
      * SPECIALIZED PAYMENT INFO
      * TO REPORT INFO ON SPECIALIZED PAYMENT SCHEDULES FOR
      * EITHER DEFERRED PAYMENTS OR BALLOON PAYMENTS.
      * MUST BE PRESENT EACH TIME THE ACCOUNT IS REPORTED.
      *------------------------------------------------*
      *(1) SEGMENT IDENTIFIER                          1 - 2
      *    CONSTANT = "K4"
           03  TRW-K4-ID                  PIC X(2).

      *(2) SPECIALIZED PAYMENT INDICATOR               3 - 4
      *    01 = BALLOON PAYMENT     02 = DEFERRED PAYMENT
           03  TRW-K4-INDICATOR            PIC 9(2).

      *(3) DEFERRED PAYMENT START DATE                 5 - 12
      * DATE THE FIRST PAYMENT IS DUE FOR DEFERRED LOANS.
      * MMDDYYYY, IF DAY IS NOT AVAIL, USE 01.
           03  TRW-K4-START-DATE           PIC 9(8).

      *(4) PAYMENT DUE DATE                            13 - 20
      * DATE THE BALLOON PAYMENT IS DUE
      * MMDDYYYY, IF DAY IS NOT AVAIL, USE 01.
           03  TRW-K4-DUE-DATE             PIC 9(8).
  
      *(5) PAYMENT AMOUNT                            21 - 29
      * AMT OF THE BALLOON PAYMENT IN WHOLE DOLLARS ONLY
           03  TRW-K4-PAY-AMT             PIC 9(9).
      
      * (6)                                           30 - 30
           03  FILLER                      PIC X.

      *------------------------------------------------*
      *  TRW APPENDAGE SEGMENT L1                      *
      * ACCOUNT NUMBER/IDENTIFICATION NUMBER CHANGE    *
      *------------------------------------------------*

      *(1) SEGMENT IDENTIFIER                          1 - 2
      *    CONSTANT = "L1"
           03  TRW-L1-ID                  PIC X(2).

      *(2) CHANGE INDICATOR                            3 - 3
      *    1 =  CONSUMER ACCOUNT NUMBER CHANGE ONLY
      *    2 = IDENTIFICATION NUMBER CHANGE ONLY
      *    3 = CONSUMER ACCOUNT NUMBER AND IDENT CHANGE
           03  TRW-L1-CHANGE-IND          PIC 9.

      *(3) NEW CONSUMER ACCOUNT NUMBER                 4 - 33
      *    CONTAINS THE NEW ACCOUNT NUMBER ASSIGNED TO THIS ACCOUNT.
      * DO NOT INCLUDE EMBEDDED BLANKS OR SPCIAL CHARACTERS.
      * IF FIELD 2 = 2, THIS FIELD SHOULD BE BLANK FILLED
           03  TRW-L1-NEW-ACCTNO1         PIC 9(15).
           03  TRW-L1-NEW-ACCTNO2         PIC 9(15).
           03  TRW-L1-NEW-BRNO-ACCTNO REDEFINES TRW-L1-NEW-ACCTNO2.
               05  FILLER                   PIC X(3).
               05  TRW-L1-W-BRNO            PIC 9(4).
               05  TRW-L1-W-ACCT            PIC 9(8).

      *(4) NEW IDENTIFICATION NUMBER                  34 - 53
      *    CONTAINS THE NEW IDENTIFICATION NUMBER ASSIGNED TO
      *    THIS ACCOUNT.
      * IF FIELD 2 = 1, THIS FIELD SHOULD BE BLANK FILLED
           03  TRW-L1-NEW-IDENT           PIC X(20).
           03  TRW-L1-NEW-IDENTX REDEFINES TRW-L1-NEW-IDENT.
               05  TRW-L1-NEW-IDENT1       PIC 9(10).
               05  TRW-L1-NEW-IDENT2       PIC 9(10).

      * (5)                                          54 - 54
           03  FILLER                     PIC X.

      *------------------------------------------------*
      *  TRW APPENDAGE SEGMENT N1                      *
      * EMPLOYMENT
      *------------------------------------------------*

      *(1) SEGMENT IDENTIFIER                          1 - 2
      *    CONSTANT = "N1"
           03  TRW-N1-ID                  PIC X(2).

      *(2) EMPLOYER NAME                              3 - 32
      * NAME OF THE EMPLOYER FOR THE CONSUMER REPORTED IN THE BASE
           03  TRW-N1-EMP-NAME            PIC X(30).

      *(3) FIRST LINE OF EMPLOYER ADDRESS             33 - 64
           03  TRW-N1-EMP-ADR1            PIC X(32).
      *(4) SECOND LINE OF EMPLOYER ADDRESS            65 - 96
           03  TRW-N1-EMP-ADR2            PIC X(32).
      *(5) EMPLOYER CITY                              97 - 116
           03  TRW-N1-EMP-CITY            PIC X(20).
      *(6) EMPLOYER STATE                            117 - 118
           03  TRW-N1-EMP-STATE           PIC X(2).
      *(7) EMPLOYER ZIPCODE                          119 - 127
           03  TRW-N1-EMP-ZIPCODE         PIC X(9).
      *(8) OCCUPATION                                128 - 145
           03  TRW-N1-OCCUPATION          PIC X(18).
      *(9)                                           146 - 146
           03  FILLER                     PIC X.

      *------------------------------------------------*
      *       TRW SUBSCRIBER TRAILER RECORD            
      * MUST ALWAYS BE THE LAST RECORD PROVIDED ON THE FILE
      *------------------------------------------------*
       01  TRW-T-REC.
 
      * (1) RECORD DESCRIPTOR WORD (RDW)               1 - 4
           03  TRW-T-RDW                 PIC 9(4).
 
      * (2) RECORD IDENTIFIER                          5 - 11
      *        CONSTANT = "TRAILER"
           03  TRW-T-ID                  PIC X(7).
 
      * (3)  TOTAL BASE RECORDS                       12 - 20
           03  TRW-T-RECORDS             PIC 9(9).

      * (4) RESERVED                                  21 - 29
           03  FILLER                    PIC X(9).

      * (5) RESERVED                                  30 - 38
           03  FILLER                    PIC X(9).
 
      * (6) TOTAL ASSOCIATED CONSUMER SEGMENTS (J1)   39 - 47
      *      CONTAINS THE TOTAL NUMBER OF J1 SEGMENTS BEING REPORTED.
      *      DO NOT COUNT BLANK OR 9-FILLED SEGMENTS
           03  TRW-T-TOT-J1              PIC 9(9).

      * (7) TOTAL ASSOCIATED CONSUMER SEGMENTS (J2)   48 - 56
      *      CONTAINS THE TOTAL NUMBER OF J2 SEGMENTS BEING REPORTED.
      *      DO NOT COUNT BLANK OR 9-FILLED SEGMENTS
           03  TRW-T-TOT-J2              PIC 9(9).

      * (8) BLOCK COUNT                               57 - 65
      *     CURRENTLY N/A
           03  TRW-T-BLOCKS              PIC 9(9).
 
      * (9) TOTAL OF STATUS CODE "DA"                 66 - 74
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "DA"
      *     IN FIELD 17A.
           03  TRW-T-TOT-DA              PIC 9(9).

      * (10) TOTAL OF STATUS CODE "05"                75 - 83
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "05"
      *     IN FIELD 17A.
           03  TRW-T-TOT-05              PIC 9(9).
      * (11) TOTAL OF STATUS CODE "11"                84 - 92
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "11"
      *     IN FIELD 17A.
           03  TRW-T-TOT-11              PIC 9(9).
      * (12) TOTAL OF STATUS CODE "13"                93 - 101
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "13"
      *     IN FIELD 17A.
           03  TRW-T-TOT-13              PIC 9(9).
      * (13) TOTAL OF STATUS CODE "61"                102 - 110
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "61"
      *     IN FIELD 17A.
           03  TRW-T-TOT-61              PIC 9(9).
      * (14) TOTAL OF STATUS CODE "62"                111 - 119
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "62"
      *     IN FIELD 17A.
           03  TRW-T-TOT-62              PIC 9(9).
      * (15) TOTAL OF STATUS CODE "63"                120 - 128
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "63"
      *     IN FIELD 17A.
           03  TRW-T-TOT-63              PIC 9(9).
      * (16) TOTAL OF STATUS CODE "64"                129 - 137
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "64"
      *     IN FIELD 17A.
           03  TRW-T-TOT-64              PIC 9(9).
      * (17) TOTAL OF STATUS CODE "65"                138 - 146
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "65"
      *     IN FIELD 17A.
           03  TRW-T-TOT-65              PIC 9(9).
      * (18) TOTAL OF STATUS CODE "71"                147 - 155
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "71"
      *     IN FIELD 17A.
           03  TRW-T-TOT-71              PIC 9(9).
      * (19) TOTAL OF STATUS CODE "78"                156 - 164
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "78"
      *     IN FIELD 17A.
           03  TRW-T-TOT-78              PIC 9(9).
      * (20) TOTAL OF STATUS CODE "80"                165 - 173
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "80"
      *     IN FIELD 17A.
           03  TRW-T-TOT-80              PIC 9(9).
      * (21) TOTAL OF STATUS CODE "82"                174 - 182
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "82"
      *     IN FIELD 17A.
           03  TRW-T-TOT-82              PIC 9(9).
      * (22) TOTAL OF STATUS CODE "83"                183 - 191
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "83"
      *     IN FIELD 17A.
           03  TRW-T-TOT-83              PIC 9(9).
      * (23) TOTAL OF STATUS CODE "84"                192 - 200
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "84"
      *     IN FIELD 17A.
           03  TRW-T-TOT-84              PIC 9(9).
      * (24) TOTAL OF STATUS CODE "88"                201 - 209
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "88"
      *     IN FIELD 17A.
           03  TRW-T-TOT-88              PIC 9(9).
      * (25) TOTAL OF STATUS CODE "89"                210 - 218
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "89"
      *     IN FIELD 17A.
           03  TRW-T-TOT-89              PIC 9(9).
      * (26) TOTAL OF STATUS CODE "93"                219 - 227
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "93"
      *     IN FIELD 17A.
           03  TRW-T-TOT-93              PIC 9(9).
      * (27) TOTAL OF STATUS CODE "94"                228 - 236
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "94"
      *     IN FIELD 17A.
           03  TRW-T-TOT-94              PIC 9(9).
      * (28) TOTAL OF STATUS CODE "95"                237 - 245
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "95"
      *     IN FIELD 17A.
           03  TRW-T-TOT-95              PIC 9(9).
      * (29) TOTAL OF STATUS CODE "96"                246 - 254
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "96"
      *     IN FIELD 17A.
           03  TRW-T-TOT-96              PIC 9(9).
      * (30) TOTAL OF STATUS CODE "97"                255 - 263
      *     CONTAINS THE TOTAL NUMBER OF BASE SEGMENTS WITH STATUS CODE "97"
      *     IN FIELD 17A.
           03  TRW-T-TOT-97              PIC 9(9).
      * (31) TOTAL OF ECOA CODE "Z"                   264 - 272
      *     CONTAINS THE TOTAL NUMBER OF RECORDS WITH ECOA CODE "Z"
      *      BEING REPORTED IN THE BASE SEGMENT FIELD 37, IN THE J1
      *       SEGMENT FIELD 10 AND IN THE J2 SEGMENT FIELD 10.
           03  TRW-T-TOT-ECOA-Z          PIC 9(9).
      * (32) TOTAL OF EMPLOYMENT SEGMENTS  "N1"       273 - 281
      *     CONTAINS THE TOTAL NUMBER OF RECORDS WITH EMPLOYMENT BEING REPORTED
      *     IN THE "N1" SEGMENT
           03  TRW-T-TOT-N1              PIC 9(9).
      * (33) TOTAL ORIGINAL CREDITOR SEGMENTS "K1"    282 - 290
      *     CONTAINS THE TOTAL NUMBER OF RECORDS WITH ORIGINAL CREDITORS
      *     BEING REPORTED IN THE "K1" SEGMENT
           03  TRW-T-TOT-K1              PIC 9(9).
      * (34) TOTAL PURCHASED PORTFOLIO/SOLD TO SEGMENTS "K2" 291 - 299
      *     CONTAINS THE TOTAL NUMBER OF RECORDS WITH PURCHASED PORTFOLIO/SOLD
      *     TO BEING REPORTED IN THE K2 SEGMENT
           03  TRW-T-TOT-K2              PIC 9(9).
      * (35) TOTAL MORTGAGE INFORMATION SEGMENTS "K3"  300 - 308
      *     CONTAINS THE TOTAL NUMBER OF RECORDS WITH MORTGAGE INFO BEING
      *     REPORTED IN THE K3 SEGMENT
           03  TRW-T-TOT-K3              PIC 9(9).
      * (36) TOTAL SPECIALIZED PAYMENT INFO SEGMENTS "K4"  309 - 317
      *     CONTAINS THE TOTAL NUMBER OF RECORDS WITH SPECIALIZED PAYMENT
      *    INFO BEING REPORTED IN THE K4 SEGMENT
           03  TRW-T-TOT-K4              PIC 9(9).
      * (37) TOTAL CHANGE SEGMENTS "L1"                318 - 326
      *     CONTAINS THE TOTAL NUMBER OF CONSUMER ACCOUNT NUMBER AND/OR
      *     IDENTIFICATION NUMBER CHANGES BEING REPORTED IN THE L1 SEGMENT.
           03  TRW-T-TOT-L1              PIC 9(9).
      * (38) TOTAL SOCIAL SECURITY NUMBERS ALL SEGMENTS  327 - 335
      *     CONTAINS THE TOTAL NUMBER OF VALID SOCIAL SECURITY NUMBERS REPORTED
      *      IN THE BASE FIELD 34, THE J1 FIELD 7, AND THE J2 FIELD 7.
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-SSNO-ALL         PIC 9(9).
      * (39) TOTAL SOCIAL SECURITY NUMBERS BASE SEGMENTS  336 - 344
      *     CONTAINS THE TOTAL NUMBER OF VALID SOCIAL SECURITY NUMBERS REPORTED
      *      IN THE BASE SEGMENT FIELD 34 
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-SSNO-BASE        PIC 9(9).
      * (40) TOTAL SOCIAL SECURITY NUMBERS J1 SEGMENTS  345 - 353
      *     CONTAINS THE TOTAL NUMBER OF VALID SOCIAL SECURITY NUMBERS REPORTED
      *      IN THE J1 SEGMENT FIELD 7 
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-SSNO-J1          PIC 9(9).
      * (41) TOTAL SOCIAL SECURITY NUMBERS J2 SEGMENTS  354 - 362
      *     CONTAINS THE TOTAL NUMBER OF VALID SOCIAL SECURITY NUMBERS REPORTED
      *      IN THE J2 SEGMENT FIELD 7 
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-SSNO-J2          PIC 9(9).
      * (42) TOTAL DATES OF BIRTH ALL SEGMENTS          363 - 371
      *     CONTAINS THE TOTAL NUMBER OF VALID DATES OF BIRTH REPORTED
      *      IN THE BASE FIELD 35, J1 FIELD 8 AND J2 FIELD 8.
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-BDATE-ALL          PIC 9(9).
      * (43) TOTAL DATES OF BIRTH BASE SEGMENTS          372 - 380
      *     CONTAINS THE TOTAL NUMBER OF VALID DATES OF BIRTH REPORTED
      *      IN THE BASE FIELD 35
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-BDATE-BASE         PIC 9(9).
      * (44) TOTAL DATES OF BIRTH J1 SEGMENTS          381 - 389
      *     CONTAINS THE TOTAL NUMBER OF VALID DATES OF BIRTH REPORTED
      *      IN THE J1 FIELD 8
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-BDATE-J1         PIC 9(9).
      * (45) TOTAL DATES OF BIRTH J2 SEGMENTS          390 - 398
      *     CONTAINS THE TOTAL NUMBER OF VALID DATES OF BIRTH REPORTED
      *      IN THE J2 FIELD 8
      *      DO NOT COUNT 0 OR 9 FILLED SSNOS.
           03  TRW-T-TOT-BDATE-J2         PIC 9(9).
      * (46) TOTAL TELEPHONE NUMBERS ALL SEGMENTS      399 - 407
      *     CONTAINS THE TOTAL NUMBER OF VALID TELEPHONE NUMBERS 
      *      REPORTED IN THE BASE FIELD 36, J1 FIELD 9, AND J2 FIELD 9.
      *      DO NOT COUNT 0 FILLED TELEPHONE NUMBERS.
           03  TRW-T-TOT-PHONE         PIC 9(9).
 
 
           03  FILLER                     PIC X(957).
      * RSZ 1364
