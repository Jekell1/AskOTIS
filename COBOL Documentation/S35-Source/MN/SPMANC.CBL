       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANC.
      **************************************************************************
      *   DESC: PAGE 1 OF REFUNDS - PROG 12
      * REV:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  SLC 051995 ADDED REBATE FORMULA "U" FOR MERCURY A&H.
      *  JTG 061095 ADDED SP-RBEARLY = 9  (MERCURY) FULL REFUND (REFDATE - 30)
      *  JTG 062795 ADDED SP-RBEARLY = 10 (MERCURY) 1/30 R78 BF 1ST PAYDATE
      *  SLC 070295 ADDED REBATE FORMULAS "V" AND "W" FOR MERCURY DEFERMENTS
      *  JTG 072195 RETURNED ABILITY TO USE SP-RBFRMLA = 1 WHEN EXTFRMLA EXISTS
      *  BAH 092595 ADDED SP-RBEARLY = 11 (MERCURY) "MS"
      *  JTG 101695 ADDED SP-RBFRMLA = 'X', MERCURY 'WI' DEFERMENT REFUND
      *  BAH 101795 ADDED SP-RBMIN = 999.02, MERCURY 'OK' PP MINIMUM TEST #L022
      *  BAH 110995 ADDED SP-RBEARLY = 12, MERCURY 'AL' ACTION SPREAR=1
      *  BAH 111695 ADDED SP-RBFRMLA = 'Y', MERCURY 'SC' DEFERMENT REFUND
      *  BAH 111695 ADDED SP-RBFRMLA = 'Z', MERCURY 'AZ' DEFERMENT REFUND
      *  BAH 111795 ADDED SP-RBFRMLA2 OCCURS 18 TIMES
      *  JTG 112195 ADDED SP-RBOPT1 = 13, ACTUARIAL 'F' NEAREST DUE DATE OPTION
      *             MERCURY ARIZONA ACTION 24
      *  JTG 112495 ADDED SP-RBEARLY = 13 MERCURY 'NV' PP REFUND ACTION 8
      *  JFF 120495 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  JTG 011096 ADDED SP-RBFRMLA2 = 1, MERCURY 'AZ' FIRE REFUND (PP)
      *  JTG 021196 CHANGED SP-RBSPOPT1 = 13, TO ALSO BASE THE INTEREST START
      *             DATE FROM ONE MONTH PRIOR TO ORIGINAL FIRST PAY DATE
      *             ADDED SP-RBSPOPT1 = 14, TO  BASE THE INTEREST START DATE
      *             FROM ONE MONTH PRIOR TO ORIGINAL FIRST PAY DATE
      *  JTG 021296 ADDED SP-RBSPOPT1 = 15, MERCURY 'OREGON' RULE OF
      *             ANTICIPATION OPTION
      *  BAH 021197 ADDED SP-RBEARLY 15, AL, MERCURY, LIKE 12, EXCEPT 120 DAYS
      *  BAH 021197 ADDED SP-RBEARLY 16, "AL", SUN, LIKE 3, EXCEPT 120 DAYS
      *  JTG 051397 ADDED SP-RBEARLY = 17, CLAY FINANCIAL SINGLE PAY LOANS ONLY
      *  JTG 051997 ADDED REFUND FORMULAS FOR O3 INSURANCE
      *  MJD 060397 MOVED "14. COMBINE" TO "15. COMBINE" AND ADDED "14. OTH 3
      *             INS".
      *  MJD 072997 ADDED SP-RBEARLY = 21: EARLY PROVISION FOR ACQUISITION
      *             CHARGES (REQ #163 - WORLD)
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  BAH 102198 ADDED SP-RBEARLY 22 FOR FREEDOM, AL #775, WITHIN 90 DAYS
      *             DO PRORATA, OTHERWISE, NO REFUND
      *  JTG 122198 ADDED SP-RBSPOPT1 = 16 OHIO PREPAY PENALTY, REGENCY #001
      *  JTG 990127 INSURED THAT "FORMULA 3 IS FOR INTEREST ONLY!"
      *  JTG 990519 ADDED SP-RBFRMLA2 = '3', MEAN RULE 78THS       REGACC   #N/A
      *  BAH 990603 ADDED SP-RBEARLY = 23, WORLD (MO) #206
      *  BAH 990603 ADDED SP-RBSPOPT1 = 17, REGACC  #875
      *  JTG 000104 ADDED SP-RBREDUC = 9999.04 WISCONSIN IB REAL ESTATE
      *             PREPAY PENALTY, WISCONSIN #1031
      *  JTG 000119 ADDED SP-RBFRMLA2 '4', TN PROPERTY REDUND (SHORT RATE
      *             METHOD) CONSUMER FINANCIAL #1062
      *  BAH 000124 ADDED SP-RBCOMCD VALUE "B" FOR REGENCY, PR# 1048 FOR NOT
      *             ALLOWING COMBINED CL & AH REBATE OF LESS THAN $10.00, STATE
      *             OF "PA"
      *  JTG 000225 ADDED SP-RBREDUC = 9999.05 NEW SOUTH FLAT $75.00
      *             PREPAY PENALTY, (NEWS PR#1101)
      *  JTG 000322 ALLOWED LOGIC FOR SP-RBEARLY = 10, EP10 TO WORK FOR
      *             SERVICE CHARGE REFUNDS ALSO, RE: WORLD BR 588 #01-006564
      *             WRITTEN UP BY AUDITORS
      *  BAH 000407 ADDED RBSPOPT1 = 18, SAME AS 7 PLUS MORE, MULLEN PR# 1118
      *  JTG 000822 ADDED LOGIC FOR KENTUCKY 60 DAY NO CHARGE LAW, ADDED
      *             SP-RBSPOPT1() = 19, WORLD #233
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  JTG 011024 NEW DEFERMENT REFUND SP-RBFRMLA2 = 5 SMALL LOANS
      *                                                           CITIZENS #1622
      *  JTG 011026 CHANGED RBACT-APRATE TO RBACT-RATE FOR NEW REFUND FORMULA
      *             '0  6'                                       REGENCY PR#1631
      *  JTG 011107 PLUS NEW SP-RBFRMLA (0 7) CL & AH REFUNDS     MULLEN PR#1641
      *  JTG 020919 ADDED NEW SHORT RATE FORMULA SP-RBEARLY = 24 (PP)    TFC #96
      *  JTG 030304 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      *  JTG 030513 ADDED SP-RBFRMLA2 (0 8) WISCONSIN CPI REFUND FORMULA
      *                                                           CITIZENS #2065
      *  JTG 030709 NEW INTEREST REFUND FORMULA (0 9) STATE OF TEXAS BASED
      *             ON DAILY INTEREST USING 365 DAYS AND SIMPLE      EXCEL #2111
      *  JTG 030728 NEW INSURANCE REFUND 2ND FORMULAS:
      *             (0 A) MEAN OF PRORATA & RULE 78
      *             (0 B) RULE 78 FOR NET + 1                        WORLD #326
      *  BAH 040304 ADDED SP-DEFAULT-O2-INS-COMP FOR LENDMARK PR# 2258
      *  BAH 040526 ADDED SP-RBSPOPT1 = 20, DOLLAR 'OREGON'
      *  JTG 040608 ADDED NEW SP-RBFRMLA (0 C) SHORT RATE FORMULA   MULLEN #2302
      *  BAH 040622 ADDED SP-RBSPOPT1 = 21, THRIFT "NEW JERSEY" PR# 2334
      *  JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS
      *             ADDED AERN-LN-INSEFF & AERN-LN-INSEXP           REGACC #0074
      *  CS  041014 ADD CALL TO CL/SPMEMO WRITES A FIELD BY FIELD SM MEMO
      *                                                       [WORLD PR#362]
      *  JTG 041102 ADDED ALAMAMA REFUND FRMLA ACTUARIAL (0 E)      WORLD  #J050
      *  BAH 050405 ADDED SP-RBSPOPT1 = 22, SUN "LA" PR# 2504
      *             IF PAID BY "PB" OR "RN" REFUND RULE OF 78TH'S, IF PAID
      *             OFF WITH "PO", NO REFUND
      *  JTG 050418 NEW REFUND FORMULA '0 F' TRUE DAILY PRORATA REFUND FORMULA
      *             FOR STATE OF COLORADO                            WORLD #0401
      *   CS 050520 NEW REFUND FORMULA 2 [0 H] SAME AS [0 9] EXCEPT IT USES
      *             SPLIT TABLE (PG 4) INSTEAD OF SIMPLE RATE PR#2527 REGMGMT
      * JTG 050714 ADDED NEW FORMULAS FOR MEXICO
      *            SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)   WORLD #0408
      *  CS 060203 ADD SP-RBSPOPT1 23 DONT ROUND TO WHOLE DOLLARS USED IN REBATE
      *            SHORT-RATE-0C  [PEOPLE PR#2670]
      *  CS 060215 ADD SP-RBSPOPT1 24 USE TIM360 IN CALCULATION [PR#2680 PEOPL]
      *            REFUND ENTIRE PREMIUM IF REFUNDED WITHIN 30 DAYS, TIED TO
      *            REBATE FRMLA 0F, CAN ONLY USE WITH INSURANCE
      *  MJD 061208 ADDED SP-RBSPOPT1 = 25 OHIO PREPAY PENALTY, REGENCY #2878
      *  BAH 070814 ADDED SP-RBSPOPT1 = 26 TO USE APR INSTEAD OF SMPRATE
      *             ONLY IN RBFRMLA 'Q', LENDMARK PR# 3026
      *  JTG 071030 NEW INSURANCE REFUND FORMULA (0 I) FOR CPI POLICYS
      *             WHICH ARE ALWAYS 1 (ONE) YEAR TERMS          CITIZENS #3097
      *  BAH 080910 NEW INSURANCE REFUND FORMULA2 (0 K) FOR MULLEN PR# 3250
      *   CS 100805 PERMITTED WORLD-MEXICO TO USE SP-RBUINITPER-CD OF 'B' WITH
      *             ACTUARIAL FORMULA 'B', PREV COULD ONLY USE SP-RBUNITPER-CD
      *             REFUND FRMLA WAS PRORATA OR 78'S.  NOT ALLOWING A CHANGE OF
      *             REBATE FORMULA(7) TO ANYTHING OTHER THAN 78'S,PRO-RATA OR
      *             'B' IF SP-RBUNITPER-CD IS NOT SPACES.   WORLD PL#692
      *  BAH 100908 INSURANCE RBFRMLA2 (0 M) WORLD TX ANTICPATED PR# 711
      *  BAH 110307 ADDED SP-RBCOMCD VALUE "C" FOR REGENCY, PR# 3743 FOR NOT
      *             ALLOWING COMBINED CL & AH REBATE OF LESS THAN $5.00
      *   CS 110907 ADD SP-RBEARLY 98, IF ELAPSED TIME < 11 REFUND REB-TOTCHG
      *                                           REGMGM PR#3865
      *  MJD 120126 ADDED SP-RBFRMLA2 = "N" FOR CREDIT LIFE        CFSC PR#3795
      *  MJD 120502 ADDED SP-RBFRMLA2 = "O" FOR CREDIT LIFE         REGMGT #3938
      * BAH 130912 ADDED SP-RBFRMLA2 = "P".  "IN" DECREASING LIFE, CFSC PR# 4367
      *  CS 160728 CHANGED SAVEKEY FROM X(8) TO X(9), ORIGINAL-REC TO
      *            ORIGINAL-SP-REC, CHANGED-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      *  CS 191204 ADDED SP-RBEARLY 27 INDIANA EARLY PROVISION PR# 1406
      *  BAH 2020.01.28 ADDED SP-RBEARLY = 28, IL #1417
      *  BAH 2021.0617 ADDED SP-RBEARLY = 97 WORLD TN #1500 FOR CLOSING FEE AND
      *                ALSO INCLUDE SAME LOGIC AS 99 FULL SERVICE CHG REFUND
      *  BAH 2023.0801 ADDED SP-RBFRMLA2 "Q" FOR "AL" CLOSING FEE #1619
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/SPMANC.CBL S35 04/11/22-12:10:18 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01RB.CPY".
       COPY "LIBLP/LP01RB_SQL.CPY".
       COPY "LIBLP/LPWSRB.CPY".

       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  S                 PIC 99     COMP-3.
       01  ELE               PIC 99     VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE SPACES.
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE SPACES.


       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  REB-TABLE       OCCURS 14.
               05  F-RBFRMLA   PIC X.
               05  F-RBFRMLA2  PIC X.
               05  F-RBMINRET  PIC ZZZ9.99  BLANK ZERO.
               05  F-RBNOMON   PIC ZZ.
               05  F-RBDEDCHG  PIC ZZZ9.99  BLANK ZERO.
               05  F-RBSPOPT1  PIC ZZ.
               05  F-RBEARLY   PIC ZZ.
               05  F-RBDEFER   PIC ZZ.
               05  F-RBDEFERX  REDEFINES F-RBDEFER PIC Z9.
               05  F-RBPCT     PIC ZZ9.9    BLANK ZERO.
               05  F-RBREDUC   PIC ZZZ9.99  BLANK ZERO.
               05  F-RBMIN     PIC ZZ9.99   BLANK ZERO.
           03  F-RBCOMCD       PIC X.
           03  F-160           PIC 99.

       01  DISPLAY-LISTS.
           03  FILLER        PIC X(45)  VALUE
           "010 010A 011 012 013 014 015 016 017 018 019 ".
           03  FILLER        PIC X(45)  VALUE
           "020 020A 021 022 023 024 025 026 027 028 029 ".
           03  FILLER        PIC X(45)  VALUE
           "030 030A 031 032 033 034 035 036 037 038 039 ".
           03  FILLER        PIC X(45)  VALUE
           "040 040A 041 042 043 044 045 046 047 048 049 ".
           03  FILLER        PIC X(45)  VALUE
           "050 050A 051 052 053 054 055 056 057 058 059 ".
           03  FILLER        PIC X(45)  VALUE
           "060 060A 061 062 063 064 065 066 067 068 069 ".
           03  FILLER        PIC X(45)  VALUE
           "070 070A 071 072 073 074 075 076 077 078 079 ".
           03  FILLER        PIC X(45)  VALUE
           "080 080A 081 082 083 084 085 086 087 088 089 ".
           03  FILLER        PIC X(45)  VALUE
           "090 090A 091 092 093 094 095 096 097 098 099 ".
           03  FILLER        PIC X(45)  VALUE
           "100 100A 101 102 103 104 105 106 107 108 109 ".
           03  FILLER        PIC X(45)  VALUE
           "110 110A 111 112 113 114 115 116 117 118 119 ".
           03  FILLER        PIC X(45)  VALUE
           "120 120A 121 122 123 124 125 126 127 128 129 ".
           03  FILLER        PIC X(45)  VALUE
           "130 130A 131 132 133 134 135 136 137 138 139 ".
           03  FILLER        PIC X(49)  VALUE
           "140 140A 141 142 143 144 145 146 147 148 149 150 ".
           03  FILLER        PIC X(4)   VALUE
           "160.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                             PIC X(638).

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX              PIC 99 COMP-3 VALUE 16.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 16. SP-DEFAULT-O2-INS-COMP
           03  FILLER        PIC X(8)   VALUE "EN N020 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 16 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANC_DEF.CPY".
       COPY "LIBMN/SPMANC_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANC_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANC" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.
       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.
           PERFORM REC-SP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P2
              MOVE 13 TO FORM-SEL2.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE SP1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE SP1-KEY TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      *****************************************
       REC-SP-MODULE SECTION.
      *****************************************
       REC-SP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO SP1-KEY
              MOVE " " TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-SPFILE.
           MOVE 0 TO NEW-FLAG.
           MOVE "SPCLASS SUBCLASS LCDESC CLASS STDESC SPNO." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-SP-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-SP-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-MODULE-NO.
           MOVE VDUBUF TO SP-ORGST.
           PERFORM VERIFY-STATE-CODE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
       REC-SP-CLASS.
           MOVE "LCDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1:"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SN N020CLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY TO VDUNUM0.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE 0 TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SNNN020CLASS." TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0
                                            SP-SPRCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-CLASS.
           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-LOAN-CLASS.
           IF IO-FG NOT = 0
              GO TO REC-SP-CLASS.
       REC-SP-SUBCLASS.
           MOVE "EN N020SUBCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-CLASS.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-SUBCLASS.
           MOVE VDUNUM0 TO SP-SUBCLASS.
       REC-SP-LAWCODE.
           MOVE "EN N020SPCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0 TO SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-LAWCODE
           ELSE
              MOVE "SNNN020SPCLASS." TO VDU
              MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                        SP-LAWCODE
              PERFORM SCREENIO
              GO TO READ-SPFILE.

           IF VDUSTATUS = "1U"
              GO TO REC-SP-SUBCLASS.
           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-LAWCODE.
           MOVE VDUNUM0 TO SP-LAWCODE.
       READ-SPFILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
           MOVE SP-REC TO ORIGINAL-SP-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-SP-MODULE.
           EXIT.

      *****************************************
       CLASS-SCAN SECTION.
      *****************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *****************************************
       ENTER-SCANKEY SECTION.
      *****************************************
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE FIRST-SCAN.
           MOVE " ENTER STATE CODE, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       ENTER-KEY.
           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE SP-REC TO ORIGINAL-SP-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF TO SP-ORGST.
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK SPA-FILE.
      *    UNLOCK SPB-FILE.
      *    UNLOCK SPC-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "S  A020SPNO." TO VDU.
           MOVE SP-ORGST TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM VERIFY-STATE-CODE.
           MOVE "SN N020CLASS." TO VDU.
           MOVE SP-SPRCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020SUBCLASS." TO VDU.
           MOVE SP-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-LOAN-CLASS.
           PERFORM DISPLAY-SUBCLASS-DESC.
           MOVE "SN N020SPCLASS." TO VDU.
           MOVE SP-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF F2-KEY
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10 THRU 14
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS OR FORM-SEL2 = 12
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                   GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10 F-11
                 F-12 F-13 F-14 F-15 F-16
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ***************************
      *    SET RECORD ELEMENTS
      ***************************
       F-01.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-10.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-11.
           GO TO CREATE-ROUTINE.
       F-12.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-13.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-14.
           PERFORM ENTER-REBATE.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO SP-RBCOMCD.
           IF NOT SP-RBCOMCD-VALID
              MOVE "ENTER: SPACE, 'A', 'B', OR 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM0 TO SP-DEFAULT-O2-INS-COMP.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       ENTER-REBATE SECTION.
      ************************************************
           MOVE 0 TO IO-FG.

      * ADJUST ELE TO SUBSCRIPT TABLE
           SUBTRACT 0 FROM ELE GIVING S.
           MOVE VDUBUF TO SP-RBFRMLA(S).
           IF NOT SP-RBFRMLA-VALID(S)
              MOVE "ENTER: SPACE, '1' THRU 'Z'" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "3"
              IF NOT (S = 7)
                 MOVE "FORMULA 3 IS FOR INTEREST ONLY!" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "Q" OR "S"
              IF NOT (S = 1 OR 2 OR 3 OR 4 OR 5)
                 MOVE "FORMULA (Q & S) ARE FOR LIFE & A&H INSURANCE"
                                                             TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "G" OR "L" OR "P" OR "R"
              IF S NOT = 5
                 MOVE "THIS FORMULA IS FOR A & H ONLY" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "8"
              IF NOT (S = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 12)
                 MOVE "THIS FORMULA IS FOR COMPUTED INSURANCES ONLY"
                                                          TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "N"
              IF NOT (S = 7 OR 8 OR 9)
                 MOVE "FORMULA N IS FOR INT, SC OR MF ONLY" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "H" OR "I"
              IF S NOT = 8
                 MOVE "FORMULA # H AND # I ARE FOR SERVICE CHG ONLY"
                                                               TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "9" OR "E" OR "K" OR "M" OR "O" OR
                              "V" OR "W" OR "X" OR "Y" OR "Z"
              IF S NOT = 10
                 MOVE "FORMULA IS FOR DEFERMENTS ONLY" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF SP-RBFRMLA(S) = "A" OR "B" OR "C" OR "D" OR "F"
              IF S NOT = 7
                 MOVE
                 "FORMULA'S A, B, C, D AND F ARE FOR INTEREST ONLY"
                                                             TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF S = 7
              IF SP-RBFRMLA(S) = "D" OR "F"
                 IF SP-RBFRMLA(8) NOT = SPACE
                    MOVE "NOT ALLOWED! REBATE D AND F INCLUDE SCHG"
                                                                TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                    GO TO ENTER-REBATE-TABLE-EXIT
                 ELSE
                 IF SP-ERNFRMLA(1) = 1 OR 2 OR 3 OR 4 OR 59
                    IF (SP-ERNFRMLA(1) NOT = SP-ERNFRMLA(2)
                             AND SP-ERNFRMLA(2) NOT = 0)
                          OR
                             (SP-ERNFRMLA(1) NOT = SP-ERNFRMLA(3)
                                   AND SP-ERNFRMLA(3) NOT = 0)
                       MOVE
            "INT, EXT, AND SCH EARN FORMULAS MUST BE THE SAME OR BLANK."
                                                                TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG
                       GO TO ENTER-REBATE-TABLE-EXIT.

           IF S = 7
              IF SP-RBFRMLA(S) = "A"
                 IF SP-ERNFRMLA(1) = 1 OR 2 OR 3 OR 4 OR 59
                    IF SP-ERNFRMLA(1) NOT = SP-ERNFRMLA(2) AND
                       SP-ERNFRMLA(2) NOT = 0
                       MOVE
                  "INT AND EXT EARN FORMULAS MUST BE THE SAME OR BLANK."
                                                                TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG
                       GO TO ENTER-REBATE-TABLE-EXIT.

      *    IF S = 7
      *       IF SP-RBFRMLA(S) = "1" AND
      *          SP-PYEXFRMLA NOT = 0
      *          MOVE "CAN NOT USE REB FRM. 1 IF THERE ARE EXT CHGS."
      *                                                         TO MESS
      *          PERFORM SEND-MESS
      *          MOVE 9 TO IO-FG
      *          GO TO ENTER-REBATE-TABLE-EXIT.

           IF S = 8
              IF SP-RBFRMLA(7) = "D" OR "F"
                 IF SP-RBFRMLA(S) NOT = SPACE
                 MOVE "NOT ALLOWED! REBATE D AND F INCLUDE SCHG"
                                                             TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF S = 10
              IF SP-RBFRMLA(S) NOT = SPACE
                            AND NOT = "1"
                             AND NOT = "9"
                              AND NOT = "E"
                               AND NOT = "K"
                                AND NOT = "M"
                                 AND NOT = "O"
                                  AND NOT = "V"
                                   AND NOT = "W"
                                    AND NOT = "X"
                                     AND NOT = "Y"
                                      AND NOT = "Z"
                                       AND NOT = "0"
                 MOVE
                 "ENTER: SPACE, 1, 9, E, K, M, O, V, W, X, Y, Z, 0"
                                                            TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.


           IF SP-ORGST = "MX"
              IF S = 7
                IF SP-RBUNITPER-CD(S) = "B"
                 IF NOT(SP-RBFRMLA(S) = "B" OR 1 OR 3 OR 5 OR "A")
                    MOVE
                   "PAY FREQ 'B'(PG 13) NOT VALID WITH REBATE CODE"
                                                TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                    GO TO ENTER-REBATE-TABLE-EXIT.


       ENTER-RBFRMLA2.
           IF SP-RBFRMLA(S) NOT = "0"
              MOVE SPACES TO SP-RBFRMLA2(S) VDUBUF
              MOVE "S  A010  0A." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              GO TO ENTER-RBMINRET.

           MOVE "E  A010  0A." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-RBFRMLA2.
           MOVE VDUBUF TO SP-RBFRMLA2(S).
           IF NOT SP-RBFRMLA2-VALID(S)
              MOVE "ENTER: 1 THRU 9, A THRU Q" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "Q"
              IF S NOT = 8
                 MOVE "REBATE FORMULA 'Q' IS FOR CLOSING FEE(SCHG)"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "5"
              IF S NOT = 10
                 MOVE "SECOND REBATE FORMULA '5' IS FOR DEFERMENTS"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "6"
              IF S NOT = 7
                 MOVE "SECOND REBATE FORMULA '6' IS FOR INTEREST"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "7"
              IF S NOT < 6
                 MOVE "2ND REBATE FORMULA 7 IS FOR CREDIT LIFE & A&H"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "8" OR "M"
              IF NOT ( (S < 7) OR (S = 12 OR 13 OR 14)
                     )
                 MOVE "THIS REBATE FORMULA IS FOR INSURANCE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "9"
              IF (S NOT = 7)
                 MOVE "2ND REBATE FORMULA 9 IS FOR INTEREST ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
            IF SP-RBFRMLA2(S) = "H"
              IF (S NOT = 7)
                 MOVE "2ND REBATE FORMULA H IS FOR INTEREST ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.


           IF SP-RBFRMLA2(S) = "A"
              IF NOT ( (S < 7) OR (S = 12 OR 13 OR 14)
                     )
                 MOVE "2ND REBATE FORMULA 'A' IS FOR INSURANCE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "B"
              IF NOT ( (S < 7) OR (S = 12 OR 13 OR 14)
                     )
                 MOVE "2ND REBATE FORMULA 'B' IS FOR INSURANCE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "C" OR "K"
              IF NOT ( (S < 7) OR (S = 12 OR 13 OR 14)
                     )
               MOVE "2ND REBATE FRMLA 'C' & 'K' ARE FOR INSURANCE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "D"
              IF NOT ( (S < 7) OR (S = 12 OR 13 OR 14)
                     )
                 MOVE "2ND REBATE FORMULA 'D' IS FOR INSURANCE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "E"
              IF NOT ( (S < 7) OR (S = 12 OR 13 OR 14)
                     )
                 MOVE "2ND REBATE FORMULA 'D' IS FOR INSURANCE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "G"
              IF NOT ( (S = 9) )
                 MOVE "2ND REBATE FORMULA 'G' IS FOR MAINT. FEES ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "N" OR "P"
              IF ( S > 4 )
                 MOVE "2ND REBATE FORMULA 'N' IS FOR CREDIT LIFE ONLY"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF SP-RBFRMLA2(S) = "O"
              IF ( S > 4 )
                 MOVE "2ND REBATE FORMULA 'O' IS FOR CREDIT LIFE ONLY"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBFRMLA2.
           IF VDUBUF = " "
              GO TO ENTER-RBFRMLA2.

       ENTER-RBMINRET.
           MOVE "ENNN042  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              IF SP-RBFRMLA(S) = "0"
                 GO TO ENTER-RBFRMLA2
              ELSE
                 MOVE 9 TO IO-FG
                 GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBMINRET.
           MOVE VDUNUM2 TO SP-RBMINRET(S).
       ENTER-RBNOMON.
           MOVE "ENNN020  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBMINRET.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBNOMON.
           MOVE VDUNUM0 TO SP-RBNOMON(S).
       ENTER-RBDEDCHG.
           MOVE "ENNN042  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBNOMON.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBDEDCHG.
           MOVE VDUNUM2 TO SP-RBDEDCHG(S).
       ENTER-RBSPOPT1.
           MOVE "ENNN020  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBDEDCHG.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBSPOPT1.
           MOVE VDUNUM0 TO SP-RBSPOPT1(S).
           IF SP-RBFRMLA(S) = "T"
              IF SP-RBSPOPT1(S) = 0
                 MOVE "A CANCEL/REFUND TABLE NO. IS REQUIRED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1
              ELSE
                 PERFORM VERIFY-RBFILE-TABLE
                 IF IO-FG = 9
                    MOVE "INVALID CANCEL/REFUND TABLE NO" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-RBSPOPT1
                 ELSE
                    GO TO ENTER-RBEARLY.

           IF SP-RBSPOPT1(S) = 2 AND S > 6
              MOVE "ENTER: 0 OR 1" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 1 OR 9
            IF LC-LOANTYPE NOT = "I"
               MOVE
              "OPTION 1 AND 9 ARE FOR INTEREST BEARING CLASSES ONLY"
                                                             TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 1 OR 5 OR 6 OR 8 OR 9 OR 12 OR 21
            IF S NOT = 7
               MOVE "OPTION 1, 5, 6, 8 AND 9 ARE FOR INTEREST ONLY"
                                                             TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 6
            IF LC-SALEFIN NOT = "Y"
               MOVE "OPTION 6 IS FOR SALES FINANCE ONLY" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 7 OR 18 OR 24
            IF S = 7 OR 8 OR 9 OR 10 OR 17
               MOVE "OPTION IS FOR INSURANCE ONLY" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 10  OR 11
              IF SP-RBFRMLA(S) NOT = "L"
                 MOVE
                 "THIS OPTION IS ONLY ALLOWED WITH REBATE FRMLA 'L'"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 13 OR 14
              IF SP-RBFRMLA(S) NOT = "F"
                 MOVE
                 "THIS OPTION IS ONLY ALLOWED WITH REBATE FRMLA 'F'"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 15
              IF SP-RBFRMLA(S) NOT = "U"
                 MOVE
                 "THIS OPTION IS ONLY ALLOWED WITH REBATE FRMLA 'U'"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 26
              IF SP-RBFRMLA(S) NOT = "Q"
                 MOVE
                 "THIS OPTION IS ONLY ALLOWED WITH REBATE FRMLA 'Q'"
                                                              TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 19
              IF S NOT = 7
                 MOVE
                 "(19) KENTUCKY 60 NO CHARGE LAW IS FOR INTEREST ONLY"
                                                               TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.
           IF SP-RBSPOPT1(S) = 20
              IF SP-RBFRMLA(S) NOT = "2"
                 MOVE
                 "THIS OPTION IS ONLY ALLOWED WITH REBATE FRMLA '2'"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.

           IF SP-RBSPOPT1(S) = 23
              IF SP-RBFRMLA(S) NOT = 0 OR
                (SP-RBFRMLA2(S) NOT = "C" AND SP-RBFRMLA2(S) NOT = "K")
                   MOVE
               "ONLY ALLOWED WITH REBATE FRMLA 0, FRMLA2 'C' OR 'K'"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.

      *PEOPLES USE 360 YR IN TIME CALCULATIONS, REFUND ENTIRE PREMIUM
      *IF REFUNDED WITHIN 30 DAYS.
           IF SP-RBSPOPT1(S) = 24
              IF SP-RBFRMLA(S) NOT = 0 OR SP-RBFRMLA2(S) NOT = "F"
                   MOVE
               "OPTION ONLY ALLOWED WITH REBATE FRMLA 0, FRMLA 'F'"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT1.

            IF SP-RBSPOPT1(S) = 24
                 IF SP-RBYRTYPE(S,1) NOT = 360 OR
                    SP-RBYRTYPE(S,2) NOT = 360
                    MOVE "SPECIAL OPTION 24 REQUIRES YEAR TYPE OF 360"
                                                             TO MESS
                    PERFORM SEND-MESS

                 GO TO ENTER-RBSPOPT1.

           IF NOT SP-RBSPOPT1-VALID(S)
              MOVE "ENTER: 0 THRU 25" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBSPOPT1.
       ENTER-RBEARLY.
           MOVE "ENNN020  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBSPOPT1.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 1 OR 2 OR 3 OR 4 OR 7 OR 11 OR 12
                           OR 15 OR 16 OR 17 OR 23 OR 28
              IF S NOT = 7
                 MOVE "ENTRY ALLOWED FOR INTEREST ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 10
              IF NOT (S = 7 OR 8)
                 MOVE "ENTRY ALLOWED FOR INTEREST AND SERCHG ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 4
              IF SP-RBFRMLA(S) NOT = 1
                 MOVE "MUST HAVE REBATE FORMULA OF 1 [78THS]" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 5 OR 6
              IF S NOT = 9
                 MOVE "ENTRY ALLOWED FOR MAINT. FEES ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY
              ELSE
              IF SP-RBFRMLA(S) NOT = 5
                 MOVE "MUST HAVE REBATE FORMULA OF 5 [PRORATA]" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 7
              IF NOT (SP-RBFRMLA(S) = 3 OR "N")
                 MOVE "MUST HAVE REBATE FORMULA OF 3 OR N [78THS]"
                                                             TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 8
              IF (NOT (SP-RBFRMLA(S) = 1 OR 3 OR 5 OR "N") )
                 MOVE "MUST HAVE REBATE FORMULA 1, 3, 5 OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 99
              IF S = 10
                 MOVE "NOT ALLOWED FOR DEFERMENTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.

           IF VDUNUM0 = 98
              IF S = 10
                 MOVE "NOT ALLOWED FOR DEFERMENTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.


           IF VDUNUM0 = 13
              IF S NOT = 6
                 MOVE "ENTRY ALLOWED FOR PERSONAL PROPERTY ONLY"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 17
              IF SP-RBFRMLA(S) = "N"
                 MOVE "DOES'T WORK WITH REBATE FORMULA 'N'"
                                                             TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 97
              IF S NOT = 8
                 MOVE "ENTRY ALLOWED FOR SERVICE CHARGE ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.
           IF VDUNUM0 = 21
              IF S NOT = 8
                 MOVE "ENTRY ALLOWED FOR SERVICE CHARGE ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY
              ELSE
                 MOVE "THIS WILL OVERRIDE THE REBATE FORMULA" TO MESS
                 PERFORM SEND-MESS.
           MOVE VDUNUM0 TO SP-RBEARLY(S).

            IF VDUNUM0 = 27
              IF S NOT = 7
                 MOVE "ENTRY ALLOWED FOR INTEREST ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY.

           IF VDUNUM0 = 26
              IF S NOT = 8
                 MOVE "ENTRY ALLOWED FOR SERVICE CHARGE ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBEARLY
              ELSE
                 MOVE "THIS WILL OVERRIDE THE REBATE FORMULA" TO MESS
                 PERFORM SEND-MESS.
           MOVE VDUNUM0 TO SP-RBEARLY(S).

           IF NOT SP-RBEARLY-VALID(S)
              MOVE "ENTER: 0 THRU 25, 27, 28, 97, 98 OR 99" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBEARLY.
       ENTER-RBDEFER.
           MOVE "ENNN020  6." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBEARLY.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBDEFER.
           MOVE VDUNUM0 TO SP-RBDEFER(S).
           IF NOT (S = 7 OR (S = 10 AND
                  (SP-RBFRMLA(S) = 1 OR "V" OR "W")))
              IF SP-RBDEFER(S) NOT = 0
                 MOVE "ENTER: 0 ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBDEFER.
           IF NOT SP-RBDEFER-VALID(S)
              MOVE "ENTER: 0, 1 OR 2" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBDEFER.
       ENTER-RBPCT.
           MOVE "ENNN031  7." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBDEFER.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBPCT.
           MOVE VDUNUM1 TO SP-RBPCT(S).
       ENTER-RBREDUC.
           MOVE "ENNN042  8." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBPCT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBREDUC.
           MOVE VDUNUM2 TO SP-RBREDUC(S).
       ENTER-RBMIN.
           MOVE "ENNN032  9." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBREDUC.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-RBMIN.
           MOVE VDUNUM2 TO SP-RBMIN(S).
           IF SP-RBMIN(S) > 998.99
      * INTEREST AND PERSONAL PROPERTY
              IF S NOT = 7 AND NOT = 6
                 GO TO ENTER-RBMIN
              ELSE
                 IF S = 7
                    IF SP-RBMIN(S) NOT = 999.01
                       GO TO ENTER-RBMIN
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF SP-RBMIN(S) NOT = 999.02
                       GO TO ENTER-RBMIN.

       ENTER-REBATE-TABLE-EXIT.
           EXIT.

      ************************************************
       VERIFY-RBFILE-TABLE SECTION.
      ************************************************
           PERFORM OPEN-RB1-FILE.
           MOVE SP-RBSPOPT1(S) TO RB-TBL-NO.
           MOVE 0 TO RB-TERM.
           PERFORM START-RB1-FILE.
           PERFORM READ-RB1-FILE-NEXT.
           IF IO-FG NOT = 0
              MOVE 9 TO IO-FG
           ELSE
              IF RB-TBL-NO NOT = SP-RBSPOPT1(S)
                 MOVE 9 TO IO-FG.

           PERFORM CLOSE-RB1-FILE.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-SP1-FILE.
           MOVE SP-REC TO CHANGED-SP-REC.
           IF ORIGINAL-SP-REC = CHANGED-SP-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF SP-REC NOT = ORIGINAL-SP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-SP-REC TO SP-REC.
           PERFORM REWRITE-SP1-FILE.
      *CREATE SPR MEMO WITH CHANGES
           IF IO-GOOD
              MOVE FORM-PATHNAME TO FORM-PATH
              MOVE "CL/SPMEMO"   TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   ORIGINAL-SP-REC
                                   SP-REC
              CANCEL FORM-PROGX.

      * CODE TO WRITE CHANGE FILE.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SP1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

      ************************************************
       SPSCAN-SCAN SECTION.
      ************************************************
           MOVE SPACES TO SPSCAN-SP1-KEY.
           MOVE SPACES TO SPSCAN-LAST-LAW.
           MOVE "WI/SPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SPSCAN-STATUS NOT = "00" OR SPSCAN-SP1-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-SPSCAN-SCAN
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SPSCAN-SCAN.
           EXIT.

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE 0 TO SUB.
       NEXT-INS-RATE.
           ADD 1 TO SUB.
           MOVE SP-RBFRMLA(SUB)    TO F-RBFRMLA(SUB).
           MOVE SP-RBFRMLA2(SUB)   TO F-RBFRMLA2(SUB).
           MOVE SP-RBMINRET(SUB)   TO F-RBMINRET(SUB).
           MOVE SP-RBNOMON(SUB)    TO F-RBNOMON(SUB).
           MOVE SP-RBDEDCHG(SUB)   TO F-RBDEDCHG(SUB).
           MOVE SP-RBSPOPT1(SUB)   TO F-RBSPOPT1(SUB).
           MOVE SP-RBEARLY(SUB)    TO F-RBEARLY(SUB).
           IF NOT (SUB = 7 OR (SUB = 10 AND SP-RBFRMLA(SUB) = 1))
              MOVE SP-RBDEFER(SUB) TO F-RBDEFER(SUB)
           ELSE
              MOVE SP-RBDEFER(SUB) TO F-RBDEFERX(SUB).
           MOVE SP-RBPCT(SUB)      TO F-RBPCT(SUB).
           MOVE SP-RBREDUC(SUB)    TO F-RBREDUC(SUB).
           MOVE SP-RBMIN(SUB)      TO F-RBMIN(SUB).
           IF SUB < 14
              GO TO NEXT-INS-RATE.
           MOVE SP-RBCOMCD TO F-RBCOMCD.
           MOVE SP-DEFAULT-O2-INS-COMP TO F-160.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANC-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANC-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANC-FORM-FIELDS TO WR-BUF.
           MOVE SPMANC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANC-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANC-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO ST-DESC.
           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 0
              MOVE "S  A160LCDESC." TO VDU
              MOVE LC-DESC TO VDUBUF
              PERFORM SCREENIO.
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-RB1-FILE.
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPRBGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPRB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANC-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
