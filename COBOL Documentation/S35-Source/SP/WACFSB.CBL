       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WACFSB.
      ******************************************************************
      *                 WORLD ACCEPTANCE AUTOMATED CONVERSION
      *
      *  DESCRIPTION:   CONVERTS ON ACCOUNTS FROM TAB DELIMITED FILE
      *                  LOCATED IN GLOBAL SP/FSB.TAB
      *   FORCES BRANCH ENTRY OF 8000 - 8099 OR 9961
      *
      *
      * 03/25/2019 BAH NEW PR#1365
      * 04/15/2019 BAH SET LN-PRIOR-UNEARNED
      * 04/19/2019 BAH CLEAR-FILES WAS STILL CHECKING FOR BRANCH > 9900,
      *                CHANGE TO > 7999, #1365A
      * 04/23/2019 BAH SET INSURANCE REBATE FLAGS  #1365C
      * 04/25/2019 BAH SET LN-POINTS FROM IMPORT FILE, NEEDED FOR
      *                GILA SERCHG REFUNDS,  #1365D
      * 05/31/2019 BAH SET BW-PRIVACY-OPT-OUT = "N"                 #1375
      *                CONVERT 20 CHARACTERS FOR TAB-REF (ORIG ACCT #)
      *                TEST FOR CURBAL < 0, SIGN THE FIELD
      *                SET LN-STATE-WORKER WITH 8% PORTION OF SERCHG
      *                IF SP-NON-LOAN-FLAG = "Y", MOVE "X" TO LN-GIFTNET
      *                IF SP-SKIP-TRW = "Y", MOVE "X" TO LN-SKIP-TRW
      *                ADD CLEAR LM-FILE
      *                ONLY CREATE COLLATERAL IF MAKE, MODEL, VIN EXISTS
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * 7/08/2019 BAH CURBAL < 0 WAS NOT WORKING, HAD TO ADD SIGN LEADING
      *               SEPARATE, PLDATE AND DOFD WERE WRONG DATE FORMAT #1375
      * 7/09/2019 BAH SET LP-POSTDATE #1375
      * 9/16/2019 BAH SET LN-MESSAGE WITH TAB-MESSAGE FROM FILE, #1389
      * 2/07/2020 BAH SET LN-ACTIONCD WITH TAB-ACTIONCD FROM FILE, SET
      *               LN-ORIGPURAMT WITH CURBAL AT TIME OF RUN, #1419
      * 8/14/2020 BAH KEVIN ASKS FOR TRFILE RECORDS FOR DUMMY BRANCH ALL
      *               GET DELETED AS HE USED TO MOVE THE SINGLE TRFILE
      *               THERE SHOULD NEVER BE ANY TR RECORDS.
      * 9/23/2020 BAH ADD FIELDS TO INPUT FILE TO SET COLLATERAL CODE,
      *               SINGLE/JOINT INSURED FLAG, ODD FIRST PAYMENT FIELD, LPO 
      *               PROCESS CODE. ADJUST DOFD, FIRST PAYMENT DATE CHANGE
      *               LOGIC. #1462
      * 12/01/2020 BAH FIX LN-ORIG-1STPYDATE, IF DAY OF ORIG-1STPAY AND DAY
      *                OF NEXTDUE ARE 28,29,30 OR 31, DO NOT ADJUST THEM.
      *                                                            #1478
      * 01/07/2021 BAH CAN NOT START A FILE WITH 0 IN THE DATE, USE 19000101
      *                WONT WORK IN A DATABASE, CHANGED ALL 
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 20220803 HARD CODED START
      * BAH 20220921 CLEAR-LSM-FILE #1570
      * BAH 20231207 VALIDATE NUMERIC IMPORT FIELDS
      * BAH 2024.0131 CALL CRNOR2 AND WRITE NOTICE RECORD
      * BAH 20240315 CHANGE ALL INSCOVR TO TOTAL-REPAY, NOT LNAMT #1648
      * BAH 20240906 WASNT CONVERTING DLNO CORRECTLY, TRYING TO MOVE ALPHA
      *              TO A 9999V99 FIELD. CHANGED TO FIND NUMERIC VALUE S35Q-124
      * BAH 20241004 ONLY PRINT LAST 4 OF SSNO #1673 S35Q-118
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TAB-FILE ASSIGN TO TAB-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
           SELECT TAB-LP-FILE ASSIGN TO TAB-LP-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  TAB-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 3000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  TAB-REC.
           03  TAB-BYTE          PIC X OCCURS 3000 TIMES.

      * PAYMENTS
       FD  TAB-LP-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 500 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  TAB-LP-REC.
           03  TAB-LP-BYTE          PIC X OCCURS 500 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/WACFSB S35 09/06/24-10:08:39 >".

       01  TAB-IFN              PIC X(3) VALUE "FSB".
       01  TAB-LP-IFN           PIC X(6) VALUE "FSB LP".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

       01  TAB-PATH.
           03  TAB-PATH-BASE          PIC X(09).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TAB-PATH-MACHINE       PIC X(02).
           03  FILLER                 PIC X(28)   VALUE "/SP/FSB.TAB".

       01  TAB-LP-PATH.
           03  TAB-LP-PATH-BASE       PIC X(09).
           03  FILLER                 PIC X(01) VALUE "/".
           03  TAB-LP-PATH-MACHINE    PIC X(02).
           03  FILLER                 PIC X(28) VALUE "/SP/FSB.LP.TAB".

       01  TAB-RECORD.
           03  TAB-ACCT                        PIC 9(6).
      * THEIR ACCOUNT #
           03  TAB-REF                         PIC X(20).
           03  TAB-ORGST                       PIC X(2).
           03  TAB-CLASS                       PIC 9(2).
           03  TAB-SUBCLASS                    PIC 9(2).
           03  TAB-POOLID                      PIC 9(5).
           03  TAB-DLNO                        PIC 9(9)V99.
           03  TAB-DLNOX REDEFINES TAB-DLNO.
               05  TAB-DLNO-WHOLE           PIC 9(9).
               05  TAB-DLNO-DEC             PIC 9(2).
           03  TAB-NAME                        PIC X(16).
           03  TAB-FNAME                       PIC X(16).
           03  TAB-LNAME                       PIC X(16).
           03  TAB-ADR1                        PIC X(25).
           03  TAB-ADR2                        PIC X(25).
           03  TAB-CITY                        PIC X(16).
           03  TAB-STATE                       PIC X(2).
           03  TAB-ZIPCODE                     PIC X(10).
           03  TAB-SSNO                        PIC 9(9).
           03  TAB-CRSCORE                     PIC 9(4).
           03  TAB-HPHONE                      PIC 9(10).
           03  TAB-WPHONE                      PIC 9(10).
           03  TAB-CELLPHONE                   PIC 9(10).
           03  TAB-EMAIL                       PIC X(40).
           03  TAB-BDATE                       PIC 9(8).
           03  TAB-EMPLOYER                    PIC X(24).
           03  TAB-SALARY                      PIC 9(6)V99.
           03  TAB-SALARYX REDEFINES TAB-SALARY.
               05  TAB-SALARY-WHOLE            PIC 9(6).
               05  TAB-SALARY-DEC              PIC 9(2).
           03  TAB-YEARS-EMP                   PIC 99.
           03  TAB-EMP-POSITION                PIC X(24).
           03  TAB-ENTERDATE                   PIC 9(8).
           03  TAB-OPENDATE                    PIC 9(8).
           03  TAB-FSTDUEDATE                  PIC 9(8).
           03  TAB-LSTPAIDDATE                 PIC 9(8).
           03  TAB-NEXTDUEDATE                 PIC 9(8).
           03  TAB-NOPAYS                      PIC 9(3).
           03  TAB-NOEXT                       PIC 9(3).
           03  TAB-PAYOS                       PIC 9(6)V99.
           03  TAB-PAYOSX REDEFINES TAB-PAYOS.
               05  TAB-PAYOS-WHOLE             PIC 9(6).
               05  TAB-PAYOS-DEC               PIC 9(2).
           03  TAB-PAYFREQ                     PIC X.
           03  TAB-OFINCHG                     PIC 9(6)V99.
           03  TAB-OFINCHGX REDEFINES TAB-OFINCHG.
               05  TAB-OFINCHG-WHOLE           PIC 9(6).
               05  TAB-OFINCHG-DEC             PIC 9(2).
           03  TAB-INTMETHOD                   PIC X.
           03  TAB-PAYAMT                      PIC 9(6)V99.
           03  TAB-PAYAMTX REDEFINES TAB-PAYAMT.
               05  TAB-PAYAMT-WHOLE            PIC 9(6).
               05  TAB-PAYAMT-DEC              PIC 9(2).
           03  TAB-FINAL-PAYAMT                PIC 9(6)V99.
           03  TAB-FINAL-PAYAMTX REDEFINES TAB-FINAL-PAYAMT.
               05  TAB-FINAL-PAYAMT-WHOLE      PIC 9(6).
               05  TAB-FINAL-PAYAMT-DEC        PIC 9(2).
           03  TAB-CURBAL                      PIC S9(7)V99 SIGN LEADING
                                               SEPARATE.
           03  TAB-CURBALX REDEFINES TAB-CURBAL.
               05  TAB-CURBAL-WHOLE            PIC S9(7) SIGN LEADING
                                               SEPARATE.
               05  TAB-CURBAL-DEC              PIC 9(2).
           03  TAB-OPENBAL                     PIC 9(6)V99.
           03  TAB-OPENBALX REDEFINES TAB-OPENBAL.
               05  TAB-OPENBAL-WHOLE           PIC 9(6).
               05  TAB-OPENBAL-DEC             PIC 9(2).
           03  TAB-ODISB                       PIC 9(6)V99.
           03  TAB-ODISBX REDEFINES TAB-ODISB.
               05  TAB-ODISB-WHOLE             PIC 9(6).
               05  TAB-ODISB-DEC               PIC 9(2).
           03  TAB-DISCOUNT                    PIC 9(6)V99.
           03  TAB-DISCOUNTX REDEFINES TAB-DISCOUNT.
               05  TAB-DISCOUNT-WHOLE          PIC 9(6).
               05  TAB-DISCOUNT-DEC            PIC 9(2).
           03  TAB-TOTPMTS                     PIC 9(6)V99.
           03  TAB-TOTPMTSX REDEFINES TAB-TOTPMTS.
               05  TAB-TOTPMTS-WHOLE           PIC 9(6).
               05  TAB-TOTPMTS-DEC             PIC 9(2).
           03  TAB-TOTPAID                     PIC 9(6)V99.
           03  TAB-TOTPAIDX REDEFINES TAB-TOTPAID.
               05  TAB-TOTPAID-WHOLE           PIC 9(6).
               05  TAB-TOTPAID-DEC             PIC 9(2).
           03  TAB-ORIGRATE                    PIC 9(3)V9(3).
           03  TAB-ORIGRATEX REDEFINES TAB-ORIGRATE.
               05  TAB-ORIGRATE-WHOLE          PIC 9(3).
               05  TAB-ORIGRATE-DEC            PIC 9(3).
           03  TAB-CURRATE                     PIC 9(3)V9(3).
           03  TAB-CURRATEX REDEFINES TAB-CURRATE.
               05  TAB-CURRATE-WHOLE           PIC 9(3).
               05  TAB-CURRATE-DEC             PIC 9(3).
           03  TAB-ORIGTERM                    PIC 9(3).
           03  TAB-YEAR                        PIC X(4).
           03  TAB-MAKE                        PIC X(10).
           03  TAB-MODEL                       PIC X(10).
           03  TAB-VIN                         PIC X(20).
           03  TAB-MILEAGE                     PIC 9(8).
           03  TAB-CO-NAME                     PIC X.
           03  TAB-CO-FNAME                    PIC X(16).
           03  TAB-CO-LNAME                    PIC X(16).
           03  TAB-CO-ADR1                     PIC X(25).
           03  TAB-CO-ADR2                     PIC X(25).
           03  TAB-CO-CITY                     PIC X(16).
           03  TAB-CO-STATE                    PIC XX.
           03  TAB-CO-ZIP                      PIC X(10).
           03  TAB-CO-HPHONE                   PIC 9(10).
           03  TAB-CO-WPHONE                   PIC 9(10).
           03  TAB-CO-CELLPHONE                PIC 9(10).
           03  TAB-CO-EMAIL                    PIC X(40).
           03  TAB-CO-BDATE                    PIC 9(8).
           03  TAB-CO-SSNO                     PIC 9(9).
           03  TAB-CO-EMPLOYER                 PIC X(24).
           03  TAB-CO-SALARY                   PIC 9(6)V99.
           03  TAB-CO-SALARYX REDEFINES TAB-CO-SALARY.
               05  TAB-CO-SALARY-WHOLE         PIC 9(6).
               05  TAB-CO-SALARY-DEC           PIC 9(2).
           03  TAB-CO-YEARS-EMP                PIC 99.
           03  TAB-LCBAL                       PIC S9(7)V99 SIGN LEADING
                                                   SEPARATE.
           03  TAB-LCBALX REDEFINES TAB-LCBAL.
               05  TAB-LCBAL-WHOLE             PIC S9(7) SIGN LEADING
                                                   SEPARATE.
               05  TAB-LCBAL-DEC               PIC 9(2).
           03  TAB-OTHBAL                      PIC S9(7)V99 SIGN LEADING
                                                   SEPARATE.
           03  TAB-OTHBALX REDEFINES TAB-OTHBAL.
               05  TAB-OTHBAL-WHOLE            PIC S9(7) SIGN LEADING
                                                   SEPARATE.
               05  TAB-OTHBAL-DEC              PIC 9(2).
           03  TAB-INTBAL                      PIC S9(7)V99 SIGN LEADING
                                                   SEPARATE.
           03  TAB-INTBALX REDEFINES TAB-INTBAL.
               05  TAB-INTBAL-WHOLE            PIC S9(7) SIGN LEADING
                                                    SEPARATE.
               05  TAB-INTBAL-DEC              PIC 9(2).
           03  TAB-ORIGIN                      PIC X(4).
           03  TAB-LOAN-COST                   PIC 9(6)V99.
           03  TAB-LOAN-COSTX REDEFINES TAB-LOAN-COST.
               05  TAB-LOAN-COST-WHOLE         PIC 9(6).
               05  TAB-LOAN-COST-DEC           PIC 9(2).
           03  TAB-PURCD                       PIC X(3).
           03  TAB-DEALER-LCHG-CODE            PIC 9(2).
           03  TAB-TOTINT                      PIC 9(6)V99.
           03  TAB-TOTINTX REDEFINES TAB-TOTINT.
               05  TAB-TOTINT-WHOLE            PIC 9(6).
               05  TAB-TOTINT-DEC              PIC 9(2).
           03  TAB-AMT-LAST-PAID               PIC 9(6)V99.
           03  TAB-AMT-LAST-PAIDX REDEFINES TAB-AMT-LAST-PAID.
               05  TAB-AMT-LAST-PAID-WHOLE     PIC 9(6).
               05  TAB-AMT-LAST-PAID-DEC       PIC 9(2).
           03  TAB-CITY-TAX-CODE               PIC X(3).
           03  TAB-CONTRACT-RATE               PIC 9(3)V9(3).
           03  TAB-CONTRACTX REDEFINES TAB-CONTRACT-RATE.
               05  TAB-CONTRACT-WHOLE          PIC 9(3).
               05  TAB-CONTRACT-DEC            PIC 9(3).
           03  TAB-MISC-FIELDS.
                05  TAB-MISC1                  PIC X(20).
                05  TAB-MISC2                  PIC X(20).
                05  TAB-MISC3                  PIC X(20).
                05  TAB-MISC4                  PIC X(20).
                05  TAB-MISC5                  PIC X(20).
           03  TAB-PLDATE                      PIC 9(8).
           03  TAB-PLAMT                       PIC 9(6)V99.
           03  TAB-PLAMTX REDEFINES TAB-PLAMT.
               05  TAB-PLAMT-WHOLE             PIC 9(6).
               05  TAB-PLAMT-DEC               PIC 9(2).
           03  TAB-MESSAGE                     PIC X(30).
           03  TAB-SERCHG                      PIC 9(6)V99.
           03  TAB-SERCHGX REDEFINES TAB-SERCHG.
               05  TAB-SERCHG-WHOLE            PIC 9(6).
               05  TAB-SERCHG-DEC              PIC 9(2).
           03  TAB-MFEE                        PIC 9(6)V99.
           03  TAB-MFEEX REDEFINES TAB-MFEE.
               05  TAB-MFEE-WHOLE              PIC 9(6).
               05  TAB-MFEE-DEC                PIC 9(2).
           03  TAB-CLTYPE                      PIC X.
           03  TAB-CLPREM                      PIC 9(6)V99.
           03  TAB-CLPREMX REDEFINES TAB-CLPREM.
               05  TAB-CLPREM-WHOLE            PIC 9(6).
               05  TAB-CLPREM-DEC              PIC 9(2).
           03  TAB-AHPREM                      PIC 9(6)V99.
           03  TAB-AHPREMX REDEFINES TAB-AHPREM.
               05  TAB-AHPREM-WHOLE            PIC 9(6).
               05  TAB-AHPREM-DEC              PIC 9(2).
           03  TAB-PPPREM                      PIC 9(6)V99.
           03  TAB-PPPREMX REDEFINES TAB-PPPREM.
               05  TAB-PPPREM-WHOLE            PIC 9(6).
               05  TAB-PPPREM-DEC              PIC 9(2).
           03  TAB-O1PREM                      PIC 9(6)V99.
           03  TAB-O1PREMX REDEFINES TAB-O1PREM.
               05  TAB-O1PREM-WHOLE            PIC 9(6).
               05  TAB-O1PREM-DEC              PIC 9(2).
           03  TAB-O3PREM                      PIC 9(6)V99.
           03  TAB-O3PREMX REDEFINES TAB-O3PREM.
               05  TAB-O3PREM-WHOLE            PIC 9(6).
               05  TAB-O3PREM-DEC              PIC 9(2).
           03  TAB-TOTLCHG                     PIC 9(6)V99.
           03  TAB-TOTLCHGX REDEFINES TAB-TOTLCHG.
               05  TAB-TOTLCHG-WHOLE           PIC 9(6).
               05  TAB-TOTLCHG-DEC             PIC 9(2).
           03  TAB-POFFAMT                     PIC 9(6)V99.
           03  TAB-POFFAMTX REDEFINES TAB-POFFAMT.
               05  TAB-POFFAMT-WHOLE           PIC 9(6).
               05  TAB-POFFAMT-DEC             PIC 9(2).
           03  TAB-INT-REBATE-FLAG             PIC X.
           03  TAB-DOFD                        PIC 9(8).
           03  TAB-CREDIT-LIMIT-FLAG           PIC X.
           03  TAB-POINTS                      PIC 9(2)V9(2).
           03  TAB-POINTSX REDEFINES TAB-POINTS.
               05  TAB-POINTS-WHOLE            PIC 9(2).
               05  TAB-POINTS-DEC              PIC 9(2).
           03  TAB-8-PERCENT                   PIC 9(6)V99.
           03  TAB-8-PERCENTX REDEFINES TAB-8-PERCENT.
               05  TAB-8-PERCENT-WHOLE         PIC 9(6).
               05  TAB-8-PERCENT-DEC           PIC 9(2).
           03  TAB-ACTIONCD                    PIC X(4).
           03  TAB-COLLCD                      PIC X(2).
           03  TAB-INSURED                     PIC X.
           03  TAB-FIRST-PAYAMT                PIC 9(6)V99.
           03  TAB-FIRST-PAYAMTX REDEFINES TAB-FIRST-PAYAMT.
               05  TAB-FIRST-PAYAMT-WHOLE      PIC 9(6).
               05  TAB-FIRST-PAYAMT-DEC        PIC 9(2).
           03  TAB-LPO-PROCESS                 PIC X.

       01  TAB-LP-RECORD.
           03  TAB-LP-ACCTNO         PIC X(6).
           03  TAB-LP-SEQNO          PIC X(4).
           03  TAB-LP-TRDATE         PIC 9(8).
           03  TAB-LP-TRCD           PIC XX.
           03  TAB-LP-REFNO          PIC X(5).
           03  TAB-LP-TRAMT          PIC X(12).
           03  TAB-LP-APINT          PIC X(12).
           03  TAB-LP-APLC           PIC X(12).
           03  TAB-LP-APOTH          PIC X(12).
           03  TAB-LP-APCUR          PIC X(12).
           03  TAB-LP-LCBAL          PIC X(12).
           03  TAB-LP-OTHBAL         PIC X(12).
           03  TAB-LP-INTBAL         PIC X(12).
           03  TAB-LP-CURBAL         PIC X(12).


       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  CREATE-LP            PIC X VALUE "Y".
       01  OUT-OF-BALANCE       PIC 9(6) VALUE 0.
       01  NEGATIVE-BAL-COUNT   PIC 9(6) VALUE 0.
       01  ACCT-ALREADY-EXIST   PIC 9(6) VALUE 0.
       01  RECORD-COUNTER       PIC 9(6)   VALUE 0.
       01  WRITTEN-COUNTER      PIC 9(6)   VALUE 0.
       01  NEGATIVE-FLAG        PIC X.
       01  SET-ACCRUAL-FIELDS   PIC X.
       01  SET-LAST-ACCRUAL-DATE PIC 9(8).
       01  SET-ERNDELQ-STOP      PIC X.
 
       01  SAVE-SSNO            PIC 9(9)  VALUE 999000000.
       01  SS-SSNO              PIC 9(10) VALUE 0.
       01  WK-STATUS            PIC X     VALUE SPACES.
       01  WORK-AMT             PIC S9(9)V99.
       01  BAD-NUMERIC          PIC X     VALUE SPACES.
 
       01  OUT-FIELD            PIC X(40).
       01  OUT-FIELD-X REDEFINES OUT-FIELD.
           03  OUT-BYTE         PIC X OCCURS 40 TIMES.
       01  OUT-NUMBER-X REDEFINES OUT-FIELD.
           03  FILLER           PIC X(25).
           03  OUT-NUMBER       PIC 9(15).
       01  OUT-DECIMAL-X REDEFINES OUT-FIELD.
           03  FILLER           PIC X(29).
           03  OUT-DOLLARS.
               05  OUT-WHOLE        PIC 9(9).
               05  OUT-DECIMAL      PIC 99.
           03  OUT-RATE REDEFINES OUT-DOLLARS.
               05  FILLER           PIC X(5).
               05  OUT-RATE3        PIC 9(3).
               05  OUT-DEC3         PIC 9(3).
               
       01  OUT-SUB          PIC S9(6) COMP-3.
       01  IN-SUB           PIC S9(6) COMP-3.
       01  START-SUB        PIC S9(6) COMP-3.
       01  HOLD-END-SUB     PIC S9(6) COMP-3.
       01  END-SUB          PIC S9(6) COMP-3.
       01  SUB             PIC S9(6) COMP-3.

       01  TOTAL-BALANCES   PIC S9(9)V99 VALUE 0.
 
       01  ERRCD            PIC X.
       01  BEG-BRANCH       PIC 9(4) VALUE 0.
       01  DEALER-NO        PIC 9(4)V99 VALUE 0.
       01  CLEAR-FILES      PIC X    VALUE SPACES.
       01  ACCRUAL-DATE     PIC 9(8) VALUE 0.
       01  INS-COMPANY      PIC 9(2)    VALUE 0.
       01  LOAN-LPO         PIC X(4) VALUE SPACES.
       01  ACCEPT-BUF       PIC X    VALUE SPACES.
       01  HOLD-LN1-KEY     PIC 9(12) VALUE 0.
       01  HOLD-SYSDATE     PIC 9(8).
       01  HOLD-SYS        REDEFINES HOLD-SYSDATE.
           03  HOLD-SYS-CC PIC 99.
           03  HOLD-SYS-YR PIC 99.
           03  HOLD-SYS-MO PIC 99.
           03  HOLD-SYS-DA PIC 99.


       01  LOAN-DISPLAY-NAME    PIC X(4) VALUE "MES.".
       01  LOAN-DISPLAY.
           03  FILLER           PIC X(21)  VALUE
               "PROCESSING.....:".
           03  LOAN-NAME        PIC X(15) VALUE " ".
           03  LOAN-COUNTER     PIC Z(9).
           03  FILLER           PIC X VALUE " ".
      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(10) VALUE SPACES.
           03  FILLER               PIC X(40) VALUE
               "WORLD AUTOMATED CONVERSION LISTING".
       01  HEAD3.
           03  FILLER               PIC X(15) VALUE "ACCTNO".
           03  FILLER               PIC X(13) VALUE "SSNO".
           03  FILLER               PIC X(18) VALUE "NAME".
           03  FILLER               PIC X(15) VALUE "PRINCIPAL".
           03  FILLER               PIC X(12) VALUE "MESSAGE".
 
       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-ACCTNO             PIC Z(6).
           03  FILLER               PIC X(2).
           03  FILLER               PIC X(7).
           03  D-SSNO               PIC 9999.
           03  FILLER               PIC XX.
           03  D-NAME               PIC X(15).
           03  FILLER               PIC X(2).
           03  D-PRINBAL            PIC Z(7).99.
           03  FILLER               PIC X(2).
           03  D-MESSAGE            PIC X(25).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  R-COUNTERX    PIC X(20).
           03  R-COUNTER     PIC Z(8)9.
           03  FILLER        PIC X(2).
           03  R-BALANCES    PIC Z(9).99-.
           03  R-ALPHA REDEFINES R-BALANCES PIC X(13).
 
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPAERNW.CPY".
       COPY "LIBLP/LPUERNW.CPY".
       COPY "LIBLP/LPCERNW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPEADJW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  WACFSB-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WACFSB_DEF.CPY".
       COPY "LIBSP/WACFSB_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/WACFSB_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               TAB-FILE PR-FILE TAB-LP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IX1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               TAB-FILE PR-FILE TAB-LP-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      *****************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *****************************
       COPY "LIBGB/CHKSEC.CPY".
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WACFSB" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS WACFSB-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF CLEAR-FILES = "Y"
      * KEVIN ASKED TO ADD THIS SINCE HE USED TO MOVE OLD TR OUT OF THE WAY
      * THIS IS DONE IN A DUMMY BRANCH, SHOULD NEVER HAVE ANY TR'S
              PERFORM PURGE-OPEN-OUTPUT-TR
              PERFORM PURGE-OPEN-OUTPUT-BW
              PERFORM PURGE-OPEN-OUTPUT-LP
              PERFORM PURGE-OPEN-OUTPUT-LN
              PERFORM PURGE-OPEN-OUTPUT-LM
              PERFORM PURGE-OPEN-OUTPUT-NO
              PERFORM PURGE-OPEN-OUTPUT-LTC
              PERFORM PURGE-OPEN-OUTPUT-LSM
              PERFORM PURGE-OPEN-OUTPUT-IX.

           PERFORM GET-GPENV.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-NO1-FILE.
           PERFORM OPEN-LTC1-FILE.
           PERFORM OPEN-LSM1-FILE.
           PERFORM OPEN-LP1-FILE.

           PERFORM OPEN-TAB-FILE.

       NEXT-RECORD.
           MOVE SPACES TO TAB-REC TAB-RECORD.
           PERFORM READ-TAB-FILE.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           IF TAB-BYTE(1) = X"1A"
              GO TO END-ROUTINE.

      * HEADER RECORD
           IF TAB-BYTE(1) = "A"
              GO TO NEXT-RECORD.

           PERFORM CONVERT-TAB-RECORD.

           MOVE "Y" TO CREATE-LP.
           ADD 1    TO RECORD-COUNTER.

      *  SCREEN DISPLAY
           MOVE TAB-LNAME      TO LOAN-NAME.
           MOVE RECORD-COUNTER TO LOAN-COUNTER.
           MOVE LOAN-DISPLAY      TO WR-BUF.
           MOVE LOAN-DISPLAY-NAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM VALIDATE-NUMERIC-FIELDS.
           IF BAD-NUMERIC = "Y"
              MOVE TAB-ACCT           TO D-ACCTNO
              MOVE TAB-SSNO           TO D-SSNO
              MOVE TAB-LNAME          TO D-NAME
              MOVE TAB-CURBAL         TO D-PRINBAL
              MOVE "NON NUMERICS"     TO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.
             

      * SOME OLD CHARGE OFFS IN THE SELLERS FILE HAD NEGATIVE CURRENT
      * BALANCES, NEED TO SKIP THESE AND PRINT EXCEPTION
           IF TAB-CURBAL < 0
              ADD 1                   TO NEGATIVE-BAL-COUNT
              MOVE TAB-ACCT           TO D-ACCTNO
              MOVE TAB-SSNO           TO D-SSNO
              MOVE TAB-LNAME          TO D-NAME
              MOVE TAB-CURBAL         TO D-PRINBAL
              MOVE "NEGATIVE BALANCE" TO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.

           IF TAB-SSNO NOT = 0
              MOVE TAB-SSNO TO SS-SSNO
           ELSE
      *  MAKE DUMMY SSNO IF NOT FOUND
              ADD 1 TO SAVE-SSNO
              MOVE SAVE-SSNO TO SS-SSNO TAB-SSNO.
 
      * NEED CLASS FIELDS FOR BORROWER LIABILITY
           MOVE TAB-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO LC-SERVACCT LC-SALEFIN
              MOVE "Y" TO LC-SMALLOAN
              MOVE "P" TO LC-LOANTYPE
              MOVE "INVALID CLASS" TO MESS PERFORM SEND-MESS
              MOVE TAB-ACCT        TO MESS PERFORM SEND-MESS
              MOVE TAB-ACCT        TO D-ACCTNO
              MOVE TAB-SSNO        TO D-SSNO
              MOVE TAB-LNAME       TO D-NAME
              MOVE TAB-CURBAL      TO D-PRINBAL
              MOVE "INVALID CLASS" TO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE.

           PERFORM CREATE-BW-RECORD.
 
           PERFORM CREATE-LN-RECORD.

           PERFORM CREATE-NOTICE-RECORD.

           IF CREATE-LP = "N"
              GO TO NEXT-RECORD.

           MOVE BR-NO    TO LN-OWNBR.
           MOVE TAB-ACCT TO LN-ACCTNO.
           MOVE LN1-KEY  TO HOLD-LN1-KEY.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-RECORD.
      *    UNLOCK LN-FILE.

           PERFORM CREATE-LT-RECORD.

           IF TAB-MISC-FIELDS NOT = SPACES
              PERFORM CREATE-LS-RECORD.

           PERFORM CREATE-BF-RECORD.

           GO TO NEXT-RECORD.
 
       END-ROUTINE.

           PERFORM PRINT-TOTALS.

           MOVE "TOTAL" TO MESS.
           MOVE RECORD-COUNTER TO MESS-X-MON-1.
           PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE " " TO ERRCD.

           PERFORM OPEN-BR-FILE.

       GETPR.
           MOVE WACFSB-QUEUE-POS TO Q-POS.
           MOVE WACFSB-COPIES-POS TO C-POS.
           MOVE WACFSB-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.


           MOVE EXT-FILPATH-BASE      TO TAB-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE   TO TAB-PATH-MACHINE.
           MOVE TAB-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "FILE: FSB.TAB DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE 999000000 TO SAVE-SSNO.

       ENTER-BEG-BRANCH.
           MOVE "ENNN040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO GETPR.
           IF (        F7-KEY ) GO TO ENDIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-BEG-BRANCH.
           MOVE VDUNUM0 TO BEG-BRANCH.
           IF BEG-BRANCH < 8000 OR > 8099
              IF BEG-BRANCH NOT = 9961
              MOVE "INVALID BRANCH" TO MESS PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           MOVE BEG-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           PERFORM SET-FILE-PATHS.

       ENTER-DLNO.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO GETPR.
           IF (        F7-KEY ) GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-BRANCH.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DLNO.
           MOVE VDUNUM2 TO DEALER-NO.
           IF VDUNUM2 = 0
              GO TO ENTER-DLNO.

           PERFORM OPEN-DL1-FILE.
           MOVE DEALER-NO TO DL-DLNO.
           PERFORM READ-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "DEALER NOT ON FILE" TO MESS PERFORM SEND-MESS
              GO TO ENTER-DLNO.
           PERFORM CLOSE-DL1-FILE.

       SS-NO.
           MOVE "ENNN090SSNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENTER-DLNO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SS-NO.
           MOVE VDUNUM0 TO SAVE-SSNO.

       ENTER-CLEAR.
           MOVE "E  A010CLEAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO SS-NO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-CLEAR.
           MOVE VDUBUF TO CLEAR-FILES.
           IF CLEAR-FILES = "Y"
              IF (BEG-BRANCH NOT > 7999) OR
                  (BR-LR-GROUP-COLLID NOT = "X")
                 MOVE "CLEAR NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-CLEAR.

       
       ENTER-ACCRUAL-DATE.
           MOVE "ERNM060ACCRUAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENTER-CLEAR.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ACCRUAL-DATE.
           MOVE VDU-DATE-YYYYMMDD TO ACCRUAL-DATE.
           IF ACCRUAL-DATE = 0
              GO TO ENTER-ACCRUAL-DATE.

       ENTER-INS-COMPANY.
           MOVE "ENNN020INSCOMP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENTER-ACCRUAL-DATE
           IF VDUSTATUS = "10" GO TO GETPR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-INS-COMPANY.
           MOVE VDUNUM0 TO INS-COMPANY.

       ENTER-LPO.
           MOVE "E  A040LPO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENTER-INS-COMPANY.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-LPO.
           MOVE VDUBUF TO LOAN-LPO.

       OK-YN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           MOVE SPACES             TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      ****************************
       CONVERT-TAB-RECORD SECTION.
      ****************************

           MOVE 1 TO IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-ACCT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-ACCT.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-ACCT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-ACCT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-ACCT.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-REF.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-REF
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-REF.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ORGST.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-ORGST
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-ORGST.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-CLASS.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-CLASS.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CLASS.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CLASS.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CLASS.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-SUBCLASS.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-SUBCLASS.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-SUBCLASS.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-SUBCLASS.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-SUBCLASS.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-POOLID.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-POOLID.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-POOLID.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-POOLID.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-POOLID.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       CONVERT-FIND-DLNO.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-DLNO.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-DLNO.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-DLNO.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-DLNO-WHOLE.
           MOVE OUT-DECIMAL TO TAB-DLNO-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-NAME.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-NAME
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-NAME.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-FNAME.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-FNAME
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-FNAME.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-LNAME.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-LNAME
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-LNAME.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ADR1.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-ADR1
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-ADR1.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ADR2.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-ADR2
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-ADR2.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CITY.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CITY
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CITY.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-STATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-STATE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-STATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ZIPCODE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-ZIPCODE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-ZIPCODE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-SSNO.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-SSNO.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-SSNO.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-SSNO.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-SSNO.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-CRSCORE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-CRSCORE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CRSCORE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CRSCORE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CRSCORE.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-HPHONE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-HPHONE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-HPHONE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-HPHONE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-HPHONE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-WPHONE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-WPHONE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-WPHONE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-WPHONE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-WPHONE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-CELLPHONE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-CELLPHONE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CELLPHONE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CELLPHONE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CELLPHONE.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-EMAIL.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-EMAIL
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-EMAIL.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-BDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-BDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-BDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-BDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-BDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-BDATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-EMPLOYER.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-EMPLOYER
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-EMPLOYER.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-SALARY.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-SALARY.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-SALARY.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-SALARY.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-SALARY-WHOLE.
           MOVE OUT-DECIMAL TO TAB-SALARY-DEC.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-YEARS-EMP.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-YEARS-EMP.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-YEARS-EMP.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-YEARS-EMP.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-YEARS-EMP.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-EMP-POSITION.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-EMP-POSITION
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-EMP-POSITION.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-ENTERDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-ENTERDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-ENTERDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-ENTERDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-ENTERDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-ENTERDATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-OPENDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-OPENDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-OPENDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-OPENDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-OPENDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-OPENDATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-FSTDUEDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-FSTDUEDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-FSTDUEDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-FSTDUEDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-FSTDUEDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-FSTDUEDATE.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-LSTPAIDDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-LSTPAIDDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-LSTPAIDDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-LSTPAIDDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-LSTPAIDDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-LSTPAIDDATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-NEXTDUEDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-NEXTDUEDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-NEXTDUEDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-NEXTDUEDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-NEXTDUEDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-NEXTDUEDATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-NOPAYS.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-NOPAYS.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-NOPAYS.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-NOPAYS.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-NOPAYS.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-NOEXT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-NOEXT.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-NOEXT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-NOEXT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-NOEXT.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-PAYOS.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-PAYOS.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-PAYOS.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-PAYOS.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-PAYOS-WHOLE.
           MOVE OUT-DECIMAL TO TAB-PAYOS-DEC.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-PAYFREQ.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-PAYFREQ
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-PAYFREQ.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-OFINCHG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-OFINCHG.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-OFINCHG.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-OFINCHG.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-OFINCHG-WHOLE.
           MOVE OUT-DECIMAL TO TAB-OFINCHG-DEC.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-INTMETHOD.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-INTMETHOD
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-INTMETHOD.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-PAYAMT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-PAYAMT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-PAYAMT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-PAYAMT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-PAYAMT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-PAYAMT-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-FINAL-PAYAMT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-FINAL-PAYAMT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-FINAL-PAYAMT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-FINAL-PAYAMT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-FINAL-PAYAMT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-FINAL-PAYAMT-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE "N" TO NEGATIVE-FLAG.
       CONVERT-FIND-CURBAL.
           IF TAB-BYTE(IN-SUB) = "-"
              MOVE "Y" TO NEGATIVE-FLAG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-CURBAL.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CURBAL.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CURBAL.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           IF NEGATIVE-FLAG = "Y"
              COMPUTE TAB-CURBAL-WHOLE = OUT-WHOLE * -1
              MOVE OUT-DECIMAL TO TAB-CURBAL-DEC
           ELSE
              MOVE OUT-WHOLE TO TAB-CURBAL-WHOLE
              MOVE OUT-DECIMAL TO TAB-CURBAL-DEC.

           MOVE "N" TO NEGATIVE-FLAG.
           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-OPENBAL.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-OPENBAL.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-OPENBAL.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-OPENBAL.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-OPENBAL-WHOLE.
           MOVE OUT-DECIMAL TO TAB-OPENBAL-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-ODISB.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-ODISB.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-ODISB.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-ODISB.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-ODISB-WHOLE.
           MOVE OUT-DECIMAL TO TAB-ODISB-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-DISCOUNT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-DISCOUNT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-DISCOUNT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-DISCOUNT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-DISCOUNT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-DISCOUNT-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-TOTPMTS.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-TOTPMTS.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-TOTPMTS.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-TOTPMTS.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-TOTPMTS-WHOLE.
           MOVE OUT-DECIMAL TO TAB-TOTPMTS-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-TOTPAID.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-TOTPAID.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-TOTPAID.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-TOTPAID.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-TOTPAID-WHOLE.
           MOVE OUT-DECIMAL TO TAB-TOTPAID-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-ORIGRATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-ORIGRATE.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-ORIGRATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-ORIGRATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-RATE3 TO TAB-ORIGRATE-WHOLE.
           MOVE OUT-DEC3 TO TAB-ORIGRATE-DEC.


           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-CURRATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-CURRATE.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CURRATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CURRATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-RATE3 TO TAB-CURRATE-WHOLE.
           MOVE OUT-DEC3 TO TAB-CURRATE-DEC.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-ORIGTERM.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-ORIGTERM.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-ORIGTERM.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-ORIGTERM.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-ORIGTERM.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-YEAR.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-YEAR
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-YEAR.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MAKE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MAKE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MAKE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MODEL.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MODEL
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MODEL.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-VIN.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-VIN
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-VIN.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-MILEAGE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-MILEAGE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-MILEAGE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-MILEAGE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-MILEAGE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-NAME.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-NAME
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-NAME.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-FNAME.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-FNAME
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-FNAME.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-LNAME.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-LNAME
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-LNAME.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-ADR1.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-ADR1
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-ADR1.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-ADR2.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-ADR2
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-ADR2.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-CITY.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-CITY
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-CITY.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-STATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-STATE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-STATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-ZIPCODE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-ZIPCODE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-ZIP.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-CO-TAB-HPHONE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-CO-TAB-HPHONE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-CO-TAB-HPHONE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-CO-TAB-HPHONE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CO-HPHONE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-CO-TAB-WPHONE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-CO-TAB-WPHONE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-CO-TAB-WPHONE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-CO-TAB-WPHONE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CO-WPHONE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-CO-TAB-CELLPHONE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-CO-TAB-CELLPHONE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-CO-TAB-CELLPHONE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-CO-TAB-CELLPHONE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CO-CELLPHONE.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-EMAIL.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-EMAIL
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-EMAIL.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-CO-TAB-BDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-CO-TAB-BDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-CO-TAB-BDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-CO-TAB-BDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CO-BDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-CO-BDATE.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-CO-SSNO.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-CO-SSNO.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CO-SSNO.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CO-SSNO.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CO-SSNO.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CO-EMPLOYER.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CO-EMPLOYER
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CO-EMPLOYER.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       FIND-CO-SALARY.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-CO-SALARY.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CO-SALARY.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CO-SALARY.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-CO-SALARY-WHOLE.
           MOVE OUT-DECIMAL TO TAB-CO-SALARY-DEC.


           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-CO-YEARS-EMP.
           IF TAB-BYTE(IN-SUB) NOT = X"09" AND NOT = " "
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-CO-YEARS-EMP.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CO-YEARS-EMP.
           IF TAB-BYTE(END-SUB) NOT = X"09" AND NOT = " "
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CO-YEARS-EMP.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-CO-YEARS-EMP.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE "N" TO NEGATIVE-FLAG.
       CONVERT-FIND-LCBAL.
           IF TAB-BYTE(IN-SUB) = "-"
              MOVE "Y" TO NEGATIVE-FLAG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-LCBAL.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-LCBAL.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-LCBAL.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           IF NEGATIVE-FLAG = "Y"
              COMPUTE TAB-LCBAL-WHOLE = OUT-WHOLE * -1
              MOVE OUT-DECIMAL TO TAB-LCBAL-DEC
           ELSE
              MOVE OUT-WHOLE TO TAB-LCBAL-WHOLE
              MOVE OUT-DECIMAL TO TAB-LCBAL-DEC.


           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE "N" TO NEGATIVE-FLAG.
       CONVERT-FIND-OTHBAL.
           IF TAB-BYTE(IN-SUB) = "-"
              MOVE "Y" TO NEGATIVE-FLAG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-OTHBAL.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-OTHBAL.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-OTHBAL.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           IF NEGATIVE-FLAG = "Y"
              COMPUTE TAB-OTHBAL-WHOLE = OUT-WHOLE * -1
              MOVE OUT-DECIMAL TO TAB-OTHBAL-DEC
           ELSE
              MOVE OUT-WHOLE TO TAB-OTHBAL-WHOLE
              MOVE OUT-DECIMAL TO TAB-OTHBAL-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE "N" TO NEGATIVE-FLAG.
       CONVERT-FIND-INTBAL.
           IF TAB-BYTE(IN-SUB) = "-"
              MOVE "Y" TO NEGATIVE-FLAG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-INTBAL.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-INTBAL.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-INTBAL.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           IF NEGATIVE-FLAG = "Y"
              COMPUTE TAB-INTBAL-WHOLE = OUT-WHOLE * -1
              MOVE OUT-DECIMAL TO TAB-INTBAL-DEC
           ELSE
              MOVE OUT-WHOLE TO TAB-INTBAL-WHOLE
              MOVE OUT-DECIMAL TO TAB-INTBAL-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       CONVERT-TAB-ORIGIN.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO OUT-SUB IN-SUB
              GO TO CONVERT-TAB-ORIGIN
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-ORIGIN.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-LOANCOST.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-LOANCOST.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-LOANCOST.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-LOANCOST.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-LOAN-COST-WHOLE.
           MOVE OUT-DECIMAL TO TAB-LOAN-COST-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       CONVERT-PURCD.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO CONVERT-PURCD
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-PURCD.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       FIND-TAB-DEALER-LCHG.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-DEALER-LCHG.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-DEALER-LCHG.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-DEALER-LCHG.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-DEALER-LCHG-CODE.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE "N" TO NEGATIVE-FLAG.

       CONVERT-FIND-TOTINT.
           IF TAB-BYTE(IN-SUB) = "-"
              MOVE "Y" TO NEGATIVE-FLAG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-TOTINT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.

       CONVERT-TAB-TOTINT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-TOTINT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           IF NEGATIVE-FLAG = "Y"
              COMPUTE TAB-TOTINT-WHOLE = OUT-WHOLE * -1
              MOVE OUT-DECIMAL TO TAB-TOTINT-DEC
           ELSE
              MOVE OUT-WHOLE TO TAB-TOTINT-WHOLE
              MOVE OUT-DECIMAL TO TAB-TOTINT-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE "N" TO NEGATIVE-FLAG.
       CONVERT-FIND-AMT-LAST-PAID.
           IF TAB-BYTE(IN-SUB) = "-"
              MOVE "Y" TO NEGATIVE-FLAG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-AMT-LAST-PAID.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-AMT-LAST-PAID.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-AMT-LAST-PAID.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           IF NEGATIVE-FLAG = "Y"
              COMPUTE TAB-AMT-LAST-PAID-WHOLE = OUT-WHOLE * -1
              MOVE OUT-DECIMAL TO TAB-AMT-LAST-PAID-DEC
           ELSE
              MOVE OUT-WHOLE TO TAB-AMT-LAST-PAID-WHOLE
              MOVE OUT-DECIMAL TO TAB-AMT-LAST-PAID-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CITY-TAX-CODE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CITY-TAX-CODE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CITY-TAX-CODE.


           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-CONTRACT-RATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-CONTRACT-RATE.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CONTRACT-RATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CONTRACT-RATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-RATE3 TO TAB-CONTRACT-WHOLE.
           MOVE OUT-DEC3 TO TAB-CONTRACT-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MISC1.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MISC1
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MISC1.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MISC2.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MISC2
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MISC2.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MISC3.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MISC3
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MISC3.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MISC4.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MISC4
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MISC4.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MISC5.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MISC5
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MISC5.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-PLDATE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-PLDATE.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-PLDATE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-PLDATE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-PLDATE DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-PLDATE.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-PLAMT.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-PLAMT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-PLAMT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-PLAMT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-PLAMT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-PLAMT-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-MESSAGE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-MESSAGE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-MESSAGE.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-SERCHG.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-SERCHG.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-SERCHG.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-SERCHG.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-SERCHG-WHOLE.
           MOVE OUT-DECIMAL TO TAB-SERCHG-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-MFEE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-MFEE.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-MFEE.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-MFEE.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-MFEE-WHOLE.
           MOVE OUT-DECIMAL TO TAB-MFEE-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CLTYPE.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CLTYPE
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CLTYPE.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-CLPREM.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-CLPREM.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-CLPREM.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-CLPREM.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-CLPREM-WHOLE.
           MOVE OUT-DECIMAL TO TAB-CLPREM-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-AHPREM.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-AHPREM.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-AHPREM.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-AHPREM.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-AHPREM-WHOLE.
           MOVE OUT-DECIMAL TO TAB-AHPREM-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-PPPREM.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-PPPREM.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-PPPREM.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-PPPREM.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-PPPREM-WHOLE.
           MOVE OUT-DECIMAL TO TAB-PPPREM-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-O1PREM.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-O1PREM.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-O1PREM.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-O1PREM.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-O1PREM-WHOLE.
           MOVE OUT-DECIMAL TO TAB-O1PREM-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-O3PREM.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-O3PREM.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-O3PREM.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-O3PREM.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-O3PREM-WHOLE.
           MOVE OUT-DECIMAL TO TAB-O3PREM-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-TOTLCHG.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-TOTLCHG.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-TOTLCHG.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-TOTLCHG.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-TOTLCHG-WHOLE.
           MOVE OUT-DECIMAL TO TAB-TOTLCHG-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-POFFAMT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-POFFAMT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-POFFAMT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-POFFAMT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-POFFAMT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-POFFAMT-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-INTREB.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-INTREB
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-INT-REBATE-FLAG.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-TAB-DOFD.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              ADD 1 TO IN-SUB
              GO TO FIND-TAB-DOFD.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-DOFD.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-DOFD.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO TAB-DOFD DATE-MMDDYYYY.
           IF DATE-MMDDYYYY-YYYY = 1900
              MOVE 0 TO TAB-DOFD.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-CREDIT-LIMIT.
           IF TAB-BYTE(IN-SUB) NOT = X"09" 
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-CREDIT-LIMIT
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-CREDIT-LIMIT-FLAG.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-POINTS.
           IF TAB-BYTE(IN-SUB) NOT = X"09" AND NOT = X"0D" AND NOT = " "
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-POINTS.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-POINTS.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-POINTS.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-POINTS-WHOLE.
           MOVE OUT-DECIMAL TO TAB-POINTS-DEC.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-8-PERCENT.
           IF TAB-BYTE(IN-SUB) NOT = X"09" AND NOT = X"0D" AND NOT = " "
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-8-PERCENT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-8-PERCENT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-8-PERCENT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE   TO TAB-8-PERCENT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-8-PERCENT-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ACTIONCD.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-ACTIONCD
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-ACTIONCD.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-COLLCD.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-COLLCD
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-COLLCD.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-INSURED.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-INSURED
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-INSURED.

           MOVE SPACES TO OUT-FIELD.
           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
       CONVERT-FIND-FIRST-PAYAMT.
           IF TAB-BYTE(IN-SUB) NOT = X"09"
              ADD 1 TO IN-SUB
              GO TO CONVERT-FIND-FIRST-PAYAMT.

           MOVE IN-SUB TO HOLD-END-SUB.
           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-TAB-FIRST-PAYAMT.
           IF TAB-BYTE(END-SUB) NOT = X"09"
              IF TAB-BYTE(END-SUB) NUMERIC
                 MOVE TAB-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-TAB-FIRST-PAYAMT.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-WHOLE TO TAB-FIRST-PAYAMT-WHOLE.
           MOVE OUT-DECIMAL TO TAB-FIRST-PAYAMT-DEC.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-LPO.
           IF TAB-BYTE(IN-SUB) NOT = X"09" AND NOT = X"0D" AND NOT = " "
              MOVE TAB-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              GO TO FIND-LPO
           ELSE
              MOVE IN-SUB TO HOLD-END-SUB.

           MOVE OUT-FIELD TO TAB-LPO-PROCESS.

      ****************************
       CREATE-BW-RECORD SECTION.
      ****************************

           MOVE BR-NO   TO BW-OWNBR.
           MOVE SS-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              PERFORM CHECK-INCREMENT-LOANS
              PERFORM REWRITE-BW1-FILE
              MOVE TAB-ACCT          TO D-ACCTNO
              MOVE TAB-SSNO          TO D-SSNO
              MOVE TAB-LNAME         TO D-NAME
              MOVE TAB-CURBAL        TO D-PRINBAL
              MOVE "BORROWER ALREADY EXISTS" TO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE
              GO TO CHECK-CREATE-COMAKER.
             
           PERFORM CLEAR-BW-FILE.

           MOVE BR-NO   TO BW-OWNBR.
           MOVE SS-SSNO TO BW-SSNO.
 
           MOVE TAB-LNAME   TO BW-LNAME.
           MOVE TAB-FNAME   TO BW-FNAME.
           MOVE TAB-ADR1    TO BW-ADR1.
           MOVE TAB-ADR2    TO BW-ADR2.
           MOVE TAB-CITY    TO BW-CITY.
           MOVE TAB-STATE   TO BW-STATE.
           MOVE TAB-ZIPCODE TO BW-ZIP.
           MOVE TAB-HPHONE  TO BW-PHONE.
 
           MOVE HOLD-SYSDATE TO BW-REFDATE.
 
           MOVE "Y" TO BW-SOLICIT.
           MOVE "N" TO BW-CONFIDENTIAL.

           MOVE BR-NO TO BW-ORGBR.

           MOVE TAB-CITY-TAX-CODE TO BW-CITY-TAX-CD.
 
      * BW-PRLOANS OR BW-LOANS, BW-LIAOTH OR BW-LIASMLN
           PERFORM CHECK-INCREMENT-LOANS.

           MOVE TAB-BDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BW-BDATE.

           MOVE TAB-CRSCORE   TO BW-CREDIT-SCORE.
           MOVE TAB-WPHONE    TO BW-EMPPHONE.
           MOVE TAB-CELLPHONE TO BW-CELL-PHONE.

           MOVE TAB-EMPLOYER     TO BW-EMPLOYER.
           MOVE TAB-SALARY       TO BW-EMPPAY.
           MOVE TAB-YEARS-EMP    TO BW-EMPYEARS.
           MOVE TAB-EMP-POSITION TO BW-EMPLPOS.

           MOVE TAB-ORIGIN       TO BW-ORIGIN.
           MOVE TAB-EMAIL        TO BW-EMAIL.
           MOVE "N"              TO BW-PRIVACY-OPT-OUT.

           PERFORM WRITE-BW1-FILE.
 
       CHECK-CREATE-COMAKER.
           IF TAB-CO-SSNO = 0 AND TAB-CO-LNAME = SPACES
              GO TO END-CREATE-BW-RECORD.

           IF TAB-CO-SSNO = 0 OR TAB-CO-SSNO = 999999999
              ADD 1 TO SAVE-SSNO
              MOVE SAVE-SSNO TO TAB-CO-SSNO.

           MOVE BR-NO       TO BW-OWNBR.
           MOVE TAB-CO-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              PERFORM CHECK-INCREMENT-LOANS
              PERFORM REWRITE-BW1-FILE
              MOVE TAB-ACCT          TO D-ACCTNO
              MOVE TAB-SSNO          TO D-SSNO
              MOVE TAB-LNAME         TO D-NAME
              MOVE TAB-CURBAL        TO D-PRINBAL
              MOVE "COBORROWER ALREADY EXISTS" TO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE
              GO TO END-CREATE-BW-RECORD.

           PERFORM CLEAR-BW-FILE.

           MOVE BR-NO       TO BW-OWNBR.
           MOVE TAB-CO-SSNO TO BW-SSNO.
 
           MOVE TAB-CO-LNAME  TO BW-LNAME.
           MOVE TAB-CO-FNAME  TO BW-FNAME.
           MOVE TAB-CO-ADR1   TO BW-ADR1.
           MOVE TAB-CO-ADR2   TO BW-ADR2.
           MOVE TAB-CO-CITY   TO BW-CITY.
           MOVE TAB-CO-STATE  TO BW-STATE.
           MOVE TAB-CO-ZIP    TO BW-ZIP.
           MOVE TAB-CO-HPHONE TO BW-PHONE.
 
           MOVE HOLD-SYSDATE TO BW-REFDATE.
 
           MOVE "Y" TO BW-SOLICIT.
           MOVE "N" TO BW-CONFIDENTIAL.

           MOVE BR-NO TO BW-ORGBR.
 
      * BW-PRLOANS OR BW-LOANS, BW-LIAOTH OR BW-LIASMLN
           PERFORM CHECK-INCREMENT-LOANS.

           MOVE TAB-CO-BDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BW-BDATE.

           MOVE TAB-CO-WPHONE    TO BW-EMPPHONE.
           MOVE TAB-CO-CELLPHONE TO BW-CELL-PHONE.     

           MOVE TAB-CO-EMPLOYER  TO BW-EMPLOYER.
           MOVE TAB-CO-SALARY    TO BW-EMPPAY.
           MOVE TAB-CO-YEARS-EMP TO BW-EMPYEARS.
           MOVE TAB-CO-EMAIL     TO BW-EMAIL.

           PERFORM WRITE-BW1-FILE.

       END-CREATE-BW-RECORD.
           EXIT.
 
      *************************************
       CREATE-LN-RECORD SECTION.
      *************************************
   
           IF TAB-ACCT = 0
              PERFORM GET-NEXT-ACCT-NO
              IF TAB-ACCT = 0
                 MOVE TAB-ACCT TO D-ACCTNO
                 MOVE TAB-SSNO TO D-SSNO
                 MOVE TAB-LNAME TO D-NAME
                 MOVE "BAD NEXT GB NUMBER" TO D-MESSAGE
                 PERFORM WRITE-DETAIL-LINE
                 MOVE "N" TO CREATE-LP
                 GO TO END-CREATE-LN-RECORD.

           MOVE BR-NO    TO LN-OWNBR.
           MOVE TAB-ACCT TO LN-ACCTNO.
           MOVE LN1-KEY  TO HOLD-LN1-KEY.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 0
      *       UNLOCK LN-FILE
              MOVE TAB-ACCT   TO D-ACCTNO
              MOVE TAB-SSNO   TO D-SSNO
              MOVE TAB-LNAME  TO D-NAME
              MOVE TAB-CURBAL TO D-PRINBAL
              MOVE "ACCOUNT ALREADY EXISTS" TO D-MESSAGE
              ADD 1 TO ACCT-ALREADY-EXIST
              PERFORM WRITE-DETAIL-LINE
              MOVE "N"        TO CREATE-LP
              GO TO END-CREATE-LN-RECORD.
            
           PERFORM CLEAR-LN-FILE.

           MOVE BR-NO TO LN-OWNBR LN-ORGBR.

           MOVE TAB-ORGST TO LN-OWNST LN-ORGST.

           MOVE TAB-CLASS TO LN-CLASS LN-SPRCLASS.
           MOVE TAB-SUBCLASS TO LN-SUBCLASS LN-LAWCODE.

           MOVE TAB-OPENDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-LOANDATE.

           PERFORM GET-EFFECTIVE-SPR.
           IF IO-FG = 9
              MOVE TAB-ACCT TO D-ACCTNO
              MOVE TAB-SSNO TO D-SSNO
              MOVE TAB-LNAME TO D-NAME
              MOVE TAB-CURBAL TO D-PRINBAL
              MOVE SPACES     TO D-MESSAGE
              STRING "NO VALID SPR LAW CODE ", SP1-KEY
                            DELIMITED BY "  " INTO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE
              MOVE "N" TO CREATE-LP
              GO TO END-CREATE-LN-RECORD
           ELSE
              MOVE SP-LAWCODE TO LN-LAWCODE.

           MOVE LOAN-LPO        TO LN-LPO.
           MOVE TAB-LPO-PROCESS TO LN-PROCESS-CD.

           MOVE TAB-SSNO TO LN-SSNO(1) BW-SSNO.
           MOVE "M"      TO LN-MAKERCD(1).
          
           IF TAB-CO-SSNO NOT = 0
              IF TAB-CO-SSNO NOT = TAB-SSNO
                 MOVE TAB-CO-SSNO TO LN-SSNO(2)
                 MOVE "J" TO LN-MAKERCD(2).

           IF TAB-PLAMT NOT = 0
              MOVE TAB-PLAMT TO LN-PLAMT
              MOVE "P"       TO LN-PLCD LN-PLTYPE WK-STATUS
              PERFORM SET-STATUS-FG
              MOVE TAB-PLDATE TO DATE-MMDDYYYY
              PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD  TO LN-PLDATE
              IF LN-PLDATE = 0
                 MOVE LN-LOANDATE TO LN-PLDATE.

           MOVE "Y" TO LN-CONVERCD LN-MIDSTREAM.

           IF TAB-PURCD = SPACES
              MOVE "00 " TO LN-PURCD
           ELSE
              MOVE TAB-PURCD TO LN-PURCD.

           MOVE "CP" TO LN-SRCD.

           IF TAB-COLLCD NOT = SPACES
              MOVE TAB-COLLCD TO LN-COLLCD
           ELSE
              MOVE "00" TO LN-COLLCD.

      * CLASS READ BEFORE CREATE-BORROWER
           MOVE LC-SMALLOAN  TO LN-SMALLOAN.
           MOVE LC-SERVACCT  TO LN-SERVACCT.
           MOVE LC-SALEFIN   TO LN-SALEFIN.
           MOVE LC-LOANTYPE  TO LN-LOANTYPE.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 MOVE TAB-ACCT   TO D-ACCTNO
                 MOVE TAB-SSNO   TO D-SSNO
                 MOVE TAB-LNAME  TO D-NAME
                 MOVE TAB-CURBAL TO D-PRINBAL
                 MOVE "INVALID SPR #&&&&&&&&" TO D-MESSAGE
                 INSPECT D-MESSAGE REPLACING FIRST "&&&&&&&&"
                                             BY LN-ACCTNO
                 PERFORM WRITE-DETAIL-LINE
                 MOVE "N"    TO CREATE-LP
                 MOVE SPACES TO LPWSSP-SP1-KEY
                 GO TO END-CREATE-LN-RECORD
              ELSE
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY.

      * CHECK IF ACCOUNT IS A NON-LOAN FOR DUAL LOAN PURPOSES
           IF SP-NON-LOAN-FLAG = "Y"
              MOVE "X" TO LN-GIFTNET.

           IF SP-SKIP-TRW = "Y"
              MOVE "Y" TO LN-SKIP-TRW.

           IF TAB-CURBAL = 0 
              MOVE "PO" TO LN-POCD.

           MOVE "N" TO LN-CONFIDENTCD.

           MOVE TAB-ENTERDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-ENTDATE.

           MOVE LN-LOANDATE TO LN-INTDATE.

           IF LN-SALEFIN = "Y"
              MOVE LN-ENTDATE TO LN-PURDATE.

           MOVE 12 TO LN-UNITPER-PER-YEAR.
           MOVE 30 TO LN-DAYS-IN-UNITPER.
           MOVE "M" TO LN-UNITPER-CD.
           MOVE 1 TO LN-UNITPER-FREQ.

           IF TAB-PAYFREQ = "W "
              MOVE 52 TO LN-UNITPER-PER-YEAR
              MOVE 7 TO LN-DAYS-IN-UNITPER
              MOVE "W" TO LN-UNITPER-CD
           ELSE
              IF TAB-PAYFREQ = "B "
                 MOVE 26 TO LN-UNITPER-PER-YEAR
                 MOVE 14 TO LN-DAYS-IN-UNITPER
                 MOVE "B" TO LN-UNITPER-CD 
              ELSE
              IF TAB-PAYFREQ = "S"
                 MOVE 24 TO LN-UNITPER-PER-YEAR
                 MOVE 15 TO LN-DAYS-IN-UNITPER
                 MOVE "S" TO LN-UNITPER-CD.
     
           MOVE TAB-FSTDUEDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-ORIG-1STPYDATE
                                 LN-1STPYDATE.

           IF LN-ORIG-1STPYDATE = 0
               MOVE BR-NO TO D-BRNO
               MOVE LN-ACCTNO TO D-ACCTNO
               MOVE "NO FIRST PAY" TO D-NAME
               MOVE 1 TO LN-FEED
               PERFORM WRITE-DETAIL-LINE

               MOVE LN-INTDATE      TO NDTE-DATE
               MOVE 1               TO NDTE-HOLD
               MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD
               MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
               PERFORM INCREMENT-UNITPER
               MOVE NDTE-DATE       TO LN-ORIG-1STPYDATE
                                       LN-1STPYDATE.

    
      * EXAMPLE NEXT DUE 3/05/2020  1STPYDATE  5/25/2019, SHOULD BE  6/05/2019
      *                 12/15/2018  1STPYDATE 10/01/2017, SHOULD BE 10/15/2017

      * IF DAY OF ORIG-1STPYDATE AND DAY OF NEXTDUE ARE 28,29,30 OR 31 
      * DO NOT CHANGE THEM.

           IF LN-UNITPER-CD = "M"
              IF TAB-NEXTDUEDATE NOT = 0
                 MOVE TAB-NEXTDUEDATE TO DATE-MMDDYYYY
                 PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
                 IF (LN-1STPYDA = 28 OR 29 OR 30 OR 31) AND
                      (DATE-YYYYMMDD-DD = 28 OR 29 OR 30 OR 31)
                     NEXT SENTENCE
                 ELSE
                    IF DATE-YYYYMMDD-DD < LN-1STPYDA
                       MOVE DATE-YYYYMMDD-DD TO LN-1STPYDA
                       MOVE LN-1STPYDATE     TO NDTE-DATE
                       MOVE 1                TO NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE        TO LN-1STPYDATE
                    ELSE
                       IF DATE-YYYYMMDD-DD NOT = LN-1STPYDA
                          MOVE DATE-YYYYMMDD-DD TO LN-1STPYDA.

           MOVE TAB-LSTPAIDDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-DATE-PAID-LAST.

           MOVE TAB-AMT-LAST-PAID TO LN-AMT-PAID-LAST.

           MOVE TAB-DOFD TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-TRW-DOFD-DATE.
            
      * IF DOFD IN THE INPUT FILE IS BLANK AND THE SELLERS DUE
      * DATE (NEXTDUE) IS 60 OR MORE DAYS LESS THAN ACQUISITION PURCHASE
      * DATE (ENTERDATE), THEN FORCE DOFD TO SELLERS FIRST PAYMENT DATE
      * PLUS 2 MONTHS (LOGICAL FIRST DATE ACCOUNT CAN BE REPORTED
      * DELINQUENT AT WAC).
           IF LN-TRW-DOFD-DATE = 0
              MOVE LN-ENTDATE TO NDTE-DATE
              MOVE -60        TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE  TO NUM-DATE

              MOVE TAB-NEXTDUEDATE TO DATE-MMDDYYYY
              PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO SYS-DATE

              IF SYS-DATE NOT > NUM-DATE
                 MOVE TAB-FSTDUEDATE TO DATE-MMDDYYYY
                 PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO NDTE-DATE
                 MOVE 2             TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO LN-TRW-DOFD-DATE.

           MOVE TAB-CRSCORE  TO LN-CRSCORE.

           IF LN-POCD NOT = " "
              MOVE LN-DATE-PAID-LAST TO LN-POFFDATE
              IF LN-POFFDATE = 0
                 MOVE LN-LOANDATE    TO LN-POFFDATE.

           MOVE TAB-ODISB TO LN-PROCEEDS.

           MOVE TAB-ORIGTERM TO LN-ORGTERM LN-SUM-ORGTERM.

           MOVE TAB-PAYAMT TO LN-REGPYAMT LN-1STPYAMT.
           IF TAB-FIRST-PAYAMT NOT = 0
              MOVE TAB-FIRST-PAYAMT TO LN-1STPYAMT.
           COMPUTE LN-NOREGPY = LN-ORGTERM - 1.
           IF LN-ORGTERM NOT = 1
              MOVE LN-REGPYAMT TO LN-LASTPYAMT
              IF LN-ORGTERM > 2
                 SUBTRACT 2 FROM LN-ORGTERM GIVING LN-NOREGPY.
           IF TAB-FINAL-PAYAMT NOT = 0
              MOVE TAB-FINAL-PAYAMT TO LN-LASTPYAMT.

           MOVE LN-REGPYAMT TO LN-SUM-REGPYAMT.

           MOVE TAB-OPENBAL TO LN-LNAMT LN-SUM-LNAMT.

           MOVE TAB-OFINCHG TO LN-INTCHG LN-SUM-INTCHG.
           MOVE TAB-SERCHG  TO LN-SERCHG LN-SUM-SERCHG.
           MOVE TAB-MFEE    TO LN-SUM-MAINTFEE.
           COMPUTE LN-MAINTFEE ROUNDED = LN-SUM-MAINTFEE / LN-ORGTERM.

           COMPUTE LN-FINCHG = LN-INTCHG + LN-SERCHG + LN-SUM-MAINTFEE.
           MOVE LN-FINCHG   TO LN-SUM-FINCHG.

      * CLOSING FEE
           MOVE TAB-POINTS    TO LN-POINTS.
      * 8% PORTION OF SERCHG
           MOVE TAB-8-PERCENT TO LN-STATE-WORKER.

      * USING THIS FIELD FOR CHARGE OFF REASON (LEGAL, BK, DSC)
           MOVE TAB-MESSAGE   TO LN-MESSAGE.

      * USING  THIS FIELD TO IDENTIFY BANKRUPT/LEGAL ACCOUNTS
           MOVE TAB-ACTIONCD TO LN-ACTIONCD.

           MOVE 1 TO LN-TILLNO.

           MOVE "N" TO LN-INSURED LN-INSTYPES LN-INSELIGIBLE.

      * ABILITY TO SET JOINT/SINGLE INSURANCE FLAG. IN 'NM', THE RATES
      * USED FOR REBATE ARE DIFFERENT FOR SINGLE AND JOINT DECREASING LIFE.
      * DEFAULT TO SINGLE 'M'.
           IF TAB-INSURED NOT = "M" AND NOT = "J"
              MOVE "M" TO TAB-INSURED.

           COMPUTE TOTAL-REPAY = LN-1STPYAMT
                     + (LN-NOREGPY * LN-REGPYAMT) + LN-LASTPYAMT.

      * INSURANCE
      * CREDIT LIFE
           MOVE TAB-CLPREM       TO LN-INSPREM(1).
           IF LN-INSPREM(1) NOT = 0
              MOVE TOTAL-REPAY   TO LN-INSCOVR(1)
              MOVE "Y"           TO LN-INSOURS(1)
              MOVE LN-LOANDATE   TO LN-INSEFF-DATE(1) NDTE-DATE
              MOVE LN-ORGTERM    TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE     TO LN-INSEXP-DATE(1)
              MOVE 0             TO LN-INSCOMM(1)
              MOVE TAB-INSURED   TO LN-INSURED
              MOVE INS-COMPANY   TO LN-INSCOMP(1).

      * A&H
           MOVE TAB-AHPREM       TO LN-INSPREM(2).
           IF LN-INSPREM(2) NOT = 0
              MOVE TOTAL-REPAY   TO LN-INSCOVR(2)
              MOVE "Y"           TO LN-INSOURS(2)
              MOVE LN-LOANDATE   TO LN-INSEFF-DATE(2) NDTE-DATE
              MOVE LN-ORGTERM    TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE     TO LN-INSEXP-DATE(2)
              MOVE 0             TO LN-INSCOMM(2)
              MOVE INS-COMPANY   TO LN-INSCOMP(2)
      * DONT CHANGE IF LIFE SET IT TO JOINT
              IF LN-INSURED NOT = "J"
                 MOVE "M"           TO LN-INSURED.

      * PROPERTY
           MOVE TAB-PPPREM       TO LN-INSPREM(3).
           IF LN-INSPREM(3) NOT = 0
              MOVE TOTAL-REPAY   TO LN-INSCOVR(3)
              MOVE "Y"           TO LN-INSOURS(3)
              MOVE LN-LOANDATE   TO LN-INSEFF-DATE(3) NDTE-DATE
              MOVE LN-ORGTERM    TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE     TO LN-INSEXP-DATE(3)
              MOVE 0             TO LN-INSCOMM(3)
              MOVE INS-COMPANY   TO LN-INSCOMP(3)
      * DONT CHANGE IF LIFE SET IT TO JOINT
              IF LN-INSURED NOT = "J"
                 MOVE "M"           TO LN-INSURED.

      * OTHER 1
           MOVE TAB-O1PREM       TO LN-INSPREM(4).
           IF LN-INSPREM(4) NOT = 0
              MOVE TOTAL-REPAY   TO LN-INSCOVR(4)
              MOVE "Y"           TO LN-INSOURS(4)
              MOVE LN-LOANDATE   TO LN-INSEFF-DATE(4) NDTE-DATE
              MOVE LN-ORGTERM    TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE     TO LN-INSEXP-DATE(4)
              MOVE 0             TO LN-INSCOMM(4)
              MOVE INS-COMPANY   TO LN-INSCOMP(4)
      * DONT CHANGE IF LIFE SET IT TO JOINT
              IF LN-INSURED NOT = "J"
                 MOVE "M"           TO LN-INSURED.

      * OTHER 3
           MOVE TAB-O3PREM       TO LN-INSPREM(6).
           IF LN-INSPREM(6) NOT = 0
              MOVE TOTAL-REPAY   TO LN-INSCOVR(6)
              MOVE "Y"           TO LN-INSOURS(6)
              MOVE LN-LOANDATE   TO LN-INSEFF-DATE(6) NDTE-DATE
              MOVE LN-ORGTERM    TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE     TO LN-INSEXP-DATE(6)
              MOVE 0             TO LN-INSCOMM(6)
              MOVE INS-COMPANY   TO LN-INSCOMP(6)
      * DONT CHANGE IF LIFE SET IT TO JOINT
              IF LN-INSURED NOT = "J"
                 MOVE "M"           TO LN-INSURED.

      * DECREASING LIFE, DUAL PROPERTY
           IF LN-INSPREM(1) NOT = 0
              IF LN-INSPREM(2) = 0
                 IF LN-INSPREM(3) = 0
                    IF LN-INSPREM(4) = 0
      * DL
                       MOVE "C" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * DL,O1
                       MOVE "I" TO LN-INSTYPES LN-INSELIGIBLE
                 ELSE
                    IF LN-INSPREM(4) = 0
      * DL,P2
                       MOVE "T" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * DL,P2,O1
                       MOVE "5" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
                 IF LN-INSPREM(3) NOT = 0
                    IF LN-INSPREM(4) NOT = 0
      * DL,AH,P2,O1
                       MOVE "7" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * DL,AH,P2
                       MOVE "X" TO LN-INSTYPES LN-INSELIGIBLE
                 ELSE
                    IF LN-INSPREM(4) NOT = 0
      * DL,AH,O1
                       MOVE "P" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * DL,AH
                       MOVE "G" TO LN-INSTYPES LN-INSELIGIBLE
           ELSE
              IF LN-INSPREM(2) NOT = 0
                 IF LN-INSPREM(3) NOT = 0
                    IF LN-INSPREM(4) NOT = 0
      * AH,P2,O1
                       MOVE "0" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * AH,P2
                       MOVE "V" TO LN-INSTYPES LN-INSELIGIBLE
                 ELSE
                    IF LN-INSPREM(4) NOT = 0
      * AH,O1
                       MOVE "Q" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * AH
                       MOVE "D" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
      * (2) IS ZERO
                 IF LN-INSPREM(3) = 0
                    IF LN-INSPREM(4) = 0
      * NONE
                       MOVE "N" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * O1
                       MOVE "1" TO LN-INSTYPES LN-INSELIGIBLE
                 ELSE
                    IF LN-INSPREM(4) = 0
      * P2
                       MOVE "R" TO LN-INSTYPES LN-INSELIGIBLE
                    ELSE
      * P2,O1
                       MOVE "8" TO LN-INSTYPES LN-INSELIGIBLE.

      * LEVEL LIFE - NOT DECREASING
           IF TAB-CLTYPE = "L"
              IF LN-INSTYPES = "C"
                 MOVE "B" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "G"
                 MOVE "F" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "J"
                 MOVE "H" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "T"
                 MOVE "S" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "I"
                 MOVE "A" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "M"
                 MOVE "L" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "P"
                 MOVE "O" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "Y"
                 MOVE "U" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "6"
                 MOVE "3" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "X"
                 MOVE "W" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "5"
                 MOVE "2" TO LN-INSTYPES LN-INSELIGIBLE
              ELSE
              IF LN-INSTYPES = "7"
                 MOVE "4" TO LN-INSTYPES LN-INSELIGIBLE.

           MOVE 9999.99 TO LN-DLNO.
           IF LN-SALEFIN = "Y"
               MOVE DEALER-NO TO LN-DLNO.

           MOVE TAB-POOLID    TO LN-DLPOOL.

           MOVE TAB-DISCOUNT  TO LN-DLDISC.

           MOVE "N" TO LN-DLRECOURSE.


           MOVE TAB-NEXTDUEDATE TO DATE-MMDDYYYY.
           PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-LASTSFORMDATE.
           IF DATE-YYYYMMDD NOT = 0
              MOVE DATE-YYYYMMDD TO NDTE-DATE
              MOVE -1            TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              IF LN-UNITPER-CD = "M"
                 MOVE LN-1STPYDA TO NDTE-DD
              END-IF
              MOVE NDTE-DATE TO LN-LCPDTH-DATE.

      ***********************************
      * DO NOT CALCULATE MONIES TO PAID THROUGH, USE AMOUNT IN TOTPAID
      ***********************************
      *    MOVE LN-1STPYDATE TO CEPP1-DATE.
      *    MOVE NDTE-DATE TO CEPP2-DATE.
      *    MOVE "Y" TO CEPP-NOCAP-FG.
      *    PERFORM CONTRACTUAL-ELAPSED-PAYPER.
      *    MOVE 0 TO LN-TOTPAYMNTD.

      *    IF CEPP-PERIODS NOT < 0
      *       IF CEPP-PERIODS NOT = 0
      *          ADD LN-1STPYAMT TO LN-TOTPAYMNTD
      *          SUBTRACT 1 FROM CEPP-PERIODS
      *       END-IF
      *       SUBTRACT TAB-NOEXT FROM CEPP-PERIODS
      *       COMPUTE LN-TOTPAYMNTD =
      *         LN-TOTPAYMNTD + (CEPP-PERIODS * LN-REGPYAMT)
      * PAYMENT OVER/SHORT
      *              + TAB-PAYOS.

      ******
      *    IF TAB-NOPAYS NOT = 0
      *       COMPUTE LN-TOTPAYMNTD =
      *        ( (TAB-NOPAYS - TAB-NOEXT) * LN-REGPYAMT)
      * PAYMENT OVER/SHORT
      *                     + TAB-PAYOS .
      ******
      ***********************************
      * DO NOT CALCULATE MONIES TO PAID THROUGH, USE AMOUNT IN TOTPAID
      ***********************************

           MOVE TAB-TOTPAID TO LN-TOTPAYMNTD.

           MOVE TAB-ORIGIN TO LN-ORIGIN.

           MOVE TAB-CURBAL TO LN-CURBAL LN-PURBAL.
           MOVE TAB-LCBAL  TO LN-LCBAL.
           MOVE TAB-INTBAL TO LN-INTBAL.
           MOVE TAB-OTHBAL TO LN-OTHBAL.

           IF LN-POCD NOT = SPACES
              MOVE 0 TO LN-CURBAL LN-INTBAL LN-LCBAL LN-OTHBAL.

           MOVE LN-CURBAL TO LN-ORIGPURAMT LN-SUM-ORIGPURAMT.

           MOVE 1                 TO LN-STMT-COUNTER.

           MOVE LN-DATE-PAID-LAST TO LN-INTPDTH-DATE.

           MOVE LN-ORGTERM        TO LN-ANTICTERM.

           MOVE SP-EARN-ORDER(1) TO LN-EARN-ORDER(1).
           MOVE SP-EARN-ORDER(2) TO LN-EARN-ORDER(2).
           MOVE SP-EARN-ORDER(3) TO LN-EARN-ORDER(3).
           MOVE SP-EARN-ORDER(4) TO LN-EARN-ORDER(4).
           MOVE SP-EARN-ORDER(5) TO LN-EARN-ORDER(5).
           MOVE SP-EARN-ORDER(6) TO LN-EARN-ORDER(6).

           MOVE TAB-NOEXT TO LN-TOTNODEF.

           ADD LN-CURBAL TO TOTAL-BALANCES.

      *******************************************
      * RATES
      *******************************************

           PERFORM APRS-RATE.
           MOVE APRS-EFFRATE TO LN-EFFRATE.
           MOVE APRS-SMPRATE TO LN-SMPRATE.

           COMPUTE APR-FINANCED = LN-1STPYAMT
                                + (LN-REGPYAMT * LN-NOREGPY)
                                + LN-LASTPYAMT
                                - LN-EXTCHG - LN-SERCHG - LN-INTCHG
                                - (LN-MAINTFEE * LN-ORGTERM).
           MOVE LN-INTDATE      TO APR-INTDATE
           MOVE LN-1STPYDATE    TO APR-1STPYDATE
           MOVE LN-ORGTERM      TO APR-ORGTERM
           MOVE LN-1STPYAMT     TO APR-1STPYAMT
           MOVE LN-REGPYAMT     TO APR-REGPYAMT
           MOVE LN-LASTPYAMT    TO APR-LASTPYAMT
           MOVE LN-UNITPER-CD   TO APR-UNITPER-CD
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ
           MOVE SP-DISFRMLA     TO APR-DISFRMLA
           PERFORM APR-REGZ
           MOVE APR-APR TO LN-APRATE.

           IF LN-LOANTYPE = "I"
              MOVE TAB-CURRATE TO LN-OVRDRATE
           ELSE
              MOVE 999.999     TO LN-OVRDRATE.

           MOVE 999.999           TO LN-ENDORATE.
           MOVE 999.999           TO LN-JDRATE.
           MOVE 999.99            TO LN-SERVPER.
           MOVE TAB-CONTRACT-RATE TO LN-CONRATE.

           PERFORM EXPIRATION-DATE-CALCULATION.
           IF LN-DATE-PAID-LAST NOT = 0 AND LN-POCD = SPACES
              IF LN-DATE-PAID-LAST >= XDTE-DATE
                 MOVE "E" TO WK-STATUS
                 PERFORM SET-STATUS-FG.

      * TAB-REF IS THE VENDOR ACCOUNT #
           MOVE TAB-REF TO LN-COLLID.

           IF LN-LCPDTH-DATE = 0
              MOVE LN-1STPYDATE TO PDTH-DATE-WORK
              PERFORM PAID-THRU-CALCULATION
              MOVE PDTH-DATE-FULL TO LN-LCPDTH-DATE.

           MOVE TAB-TOTINT  TO LN-TOTINT.
           MOVE TAB-TOTLCHG TO LN-TOTLCHG.

      *************************************************
      * EARNINGS
      *************************************************

           IF LN-LOANTYPE = "P"
              MOVE LN-INTCHG TO LN-ANTICERN(1).

           MOVE LN-SERCHG       TO LN-ANTICERN(3).
           MOVE LN-SUM-MAINTFEE TO LN-ANTICERN(4).

           IF SP-ERNFRMLA(5) NOT = 0
              IF (LN-PLDATE = 0 AND LN-ACCELDATE = 0)
                 MOVE LN-DLDISC     TO LN-ANTICERN(5).

           IF SP-ERNFRMLA(6) NOT = 0
              IF (LN-PLDATE = 0 AND LN-ACCELDATE = 0)
                 MOVE TAB-LOAN-COST TO LN-ANTICERN(6).

           PERFORM COMPUTE-UERNINGS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF UERN-EARNINGS(SUB) < 0
                 MOVE 0 TO UERN-EARNINGS(SUB)
              END-IF
              MOVE UERN-EARNINGS(SUB) TO LN-UFERN(SUB)
                                         LN-ACCERN(SUB)
              MOVE SP-EARN-ORDER(SUB) TO LN-EARN-ORDER(SUB)
              MOVE 0 TO AERN-EARNINGS(SUB)
                        CERN-EARNINGS(SUB)
           END-PERFORM.

           MOVE ACCRUAL-DATE TO AERN-ENDDATE.
           MOVE " "          TO AERN-ERNDELQ-STOP
                                SET-ACCRUAL-FIELDS
                                SET-ERNDELQ-STOP.
           MOVE 0            TO SET-LAST-ACCRUAL-DATE.

      * ALL REBATE FLAGS BASED UPON ONE INPUT REBATE FLAG
      * SET INSURANCE REBATE FLAGS ALSO #1365C 4/22/2019

           IF TAB-INT-REBATE-FLAG = "Y"
              PERFORM SET-REBATE-FLAGS.

           IF ((LN-JDDATE NOT = 0) AND SP-ERNJD-CD = " ") OR
              ((LN-ACCELDATE NOT = 0) AND SP-ERNACCEL-CD = " ")
      * CLOSE OUT ALL EARNINGS:
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                 MOVE LN-ANTICERN(SUB) TO LN-ACCERN(SUB)
              END-PERFORM
           ELSE
      * TEST FOR REBATED COMPONENT
              IF LN-INT-REBATE NOT = " "
                COMPUTE LN-ANTICERN(1) = LN-ANTICERN(1) - LN-ANTICADJ(1)
                MOVE LN-ANTICERN(1) TO LN-ACCERN(1)
              END-IF
              IF LN-SC-REBATE NOT = " "
                COMPUTE LN-ANTICERN(3) = LN-ANTICERN(3) - LN-ANTICADJ(3)
                MOVE LN-ANTICERN(3) TO LN-ACCERN(3)
              END-IF
              IF LN-MF-REBATE NOT = " "
                COMPUTE LN-ANTICERN(4) = LN-ANTICERN(4) - LN-ANTICADJ(4)
                MOVE LN-ANTICERN(4) TO LN-ACCERN(4)
              END-IF
              MOVE 1 TO CERN AERN
              PERFORM WHICH-METHOD 6 TIMES.

           MOVE 0 TO LN-PRIOR-UNEARNED.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              COMPUTE LN-PRIOR-UNEARNED = LN-PRIOR-UNEARNED +
                  (LN-ANTICERN(SUB) - LN-ACCERN(SUB)).

           MOVE ACCRUAL-DATE TO LN-LAST-ACCRUAL-DATE.

           IF SET-ACCRUAL-FIELDS = "Y"
              MOVE SET-LAST-ACCRUAL-DATE TO LN-LAST-ACCRUAL-DATE
              MOVE SET-ERNDELQ-STOP      TO LN-ERNDELQ-STOP.


           COMPUTE LN-TOTERN = LN-ACCERN(1) + LN-ACCERN(2) +
                               LN-ACCERN(3) + LN-ACCERN(4) +
                               LN-ACCERN(5) + LN-ACCERN(6) +
                               LN-TOTLCHG   + LN-TOTINT.

           MOVE BR-COUPON-STATEMENT TO LN-COUPON-STATEMENT.

           MOVE TAB-DEALER-LCHG-CODE TO LN-LC-CODE.
      * WANT TO SAVE VENDOR PAYOFF AMOUNT FOR COMPARISON DOWN THE ROAD
           MOVE TAB-POFFAMT          TO LN-PRBAL.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM       TO LN-PY-YYYYMM-RECDEL.

      * IF DATE OF LAST PAYMENT IS THE SAME MONTH AS THE BOARDING DATE
           IF NUM-MO = HOLD-SYS-MO AND NUM-YR = HOLD-SYS-YR
              MOVE LN-REGPYAMT TO LN-MTD-PAYS-RECDEL.

           PERFORM LOAN-CALCULATIONS.

      * IF LOAN DOESNT BALANCE OUT, WRITE AN EXCEPTION
      * ODISB + INT + SER + MFEE + INS
           COMPUTE WORK-AMT = LN-PROCEEDS + LN-INTCHG + LN-SERCHG
                            + LN-SUM-MAINTFEE + LN-INSPREM(1)
                            + LN-INSPREM(2) + LN-INSPREM(3)
                            + LN-INSPREM(4) + LN-INSPREM(5)
                            + LN-INSPREM(6).

           IF WORK-AMT NOT = TOTAL-REPAY
              MOVE TAB-ACCT TO D-ACCTNO
              MOVE TAB-SSNO TO D-SSNO
              MOVE TAB-LNAME TO D-NAME
              MOVE "AMOUNTS DO NOT BALANCE" TO D-MESSAGE
              ADD 1 TO OUT-OF-BALANCE
              PERFORM WRITE-DETAIL-LINE.

           IF TAB-CREDIT-LIMIT-FLAG = "N"
              MOVE 0 TO LN-CREDITLIM
           ELSE
              MOVE HIGH-CREDIT TO LN-CREDITLIM.


           PERFORM WRITE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE TAB-ACCT TO D-ACCTNO
              MOVE TAB-SSNO TO D-SSNO
              MOVE TAB-LNAME TO D-NAME
              MOVE "BAD WRITE LN-FILE" TO D-MESSAGE
              PERFORM WRITE-DETAIL-LINE
              MOVE "N" TO CREATE-LP
           ELSE
              ADD 1 TO WRITTEN-COUNTER.
       
       END-CREATE-LN-RECORD.
           EXIT.

      ***********************************************
       CREATE-NOTICE-RECORD SECTION.
      ***********************************************
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE HOLD-SYSDATE   TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ********************************
       VALIDATE-NUMERIC-FIELDS SECTION.
      ********************************
           MOVE "N" TO BAD-NUMERIC.

           IF TAB-ACCT NOT NUMERIC OR
           TAB-CLASS                        NOT NUMERIC OR
           TAB-SUBCLASS                     NOT NUMERIC OR
           TAB-POOLID                       NOT NUMERIC OR
           TAB-DLNO                         NOT NUMERIC OR
           TAB-SSNO                         NOT NUMERIC OR
           TAB-CRSCORE                      NOT NUMERIC OR
           TAB-HPHONE                       NOT NUMERIC OR
           TAB-WPHONE                       NOT NUMERIC OR
           TAB-CELLPHONE                    NOT NUMERIC OR
           TAB-BDATE                        NOT NUMERIC OR
           TAB-SALARY                       NOT NUMERIC OR
           TAB-YEARS-EMP                    NOT NUMERIC OR
           TAB-ENTERDATE                    NOT NUMERIC OR
           TAB-OPENDATE                     NOT NUMERIC OR
           TAB-FSTDUEDATE                   NOT NUMERIC OR
           TAB-LSTPAIDDATE                  NOT NUMERIC OR
           TAB-NEXTDUEDATE                  NOT NUMERIC OR
           TAB-NOPAYS                       NOT NUMERIC OR
           TAB-NOEXT                        NOT NUMERIC OR
           TAB-PAYOS                        NOT NUMERIC OR
           TAB-OFINCHG                      NOT NUMERIC OR
           TAB-PAYAMT                       NOT NUMERIC OR
           TAB-FINAL-PAYAMT                 NOT NUMERIC OR
           TAB-CURBAL                       NOT NUMERIC OR
           TAB-OPENBAL                      NOT NUMERIC OR
           TAB-ODISB                        NOT NUMERIC OR
           TAB-DISCOUNT                     NOT NUMERIC OR
           TAB-TOTPMTS                      NOT NUMERIC OR
           TAB-TOTPAID                      NOT NUMERIC OR
           TAB-ORIGRATE                     NOT NUMERIC OR
           TAB-CURRATE                      NOT NUMERIC OR
           TAB-ORIGTERM                     NOT NUMERIC OR
           TAB-MILEAGE                      NOT NUMERIC OR
           TAB-CO-HPHONE                    NOT NUMERIC OR
           TAB-CO-WPHONE                    NOT NUMERIC OR
           TAB-CO-CELLPHONE                 NOT NUMERIC OR
           TAB-CO-BDATE                     NOT NUMERIC OR
           TAB-CO-SSNO                      NOT NUMERIC OR
           TAB-CO-SALARY                    NOT NUMERIC OR
           TAB-CO-YEARS-EMP                 NOT NUMERIC OR
           TAB-LCBAL                        NOT NUMERIC OR
           TAB-OTHBAL                       NOT NUMERIC OR
           TAB-INTBAL                       NOT NUMERIC OR
           TAB-LOAN-COST                    NOT NUMERIC OR
           TAB-DEALER-LCHG-CODE             NOT NUMERIC OR
           TAB-TOTINT                       NOT NUMERIC OR
           TAB-AMT-LAST-PAID                NOT NUMERIC OR
           TAB-CONTRACT-RATE                NOT NUMERIC OR
           TAB-PLDATE                       NOT NUMERIC OR
           TAB-PLAMT                        NOT NUMERIC OR
           TAB-SERCHG                       NOT NUMERIC OR
           TAB-MFEE                         NOT NUMERIC OR
           TAB-CLPREM                       NOT NUMERIC OR
           TAB-AHPREM                       NOT NUMERIC OR
           TAB-PPPREM                       NOT NUMERIC OR
           TAB-O1PREM                       NOT NUMERIC OR
           TAB-O3PREM                       NOT NUMERIC OR
           TAB-TOTLCHG                      NOT NUMERIC OR
           TAB-POFFAMT                      NOT NUMERIC OR
           TAB-DOFD                         NOT NUMERIC OR
           TAB-POINTS                       NOT NUMERIC OR
           TAB-8-PERCENT                    NOT NUMERIC OR
           TAB-FIRST-PAYAMT                 NOT NUMERIC 
               MOVE "Y" TO BAD-NUMERIC.

      ********************************
       GET-EFFECTIVE-SPR SECTION.
      ********************************
           MOVE LN-ORGST    TO SP-ORGST
                               QSP1-WBEG-ORGST
                               QSP1-WEND-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS
                               QSP1-WBEG-SPRCLASS
                               QSP1-WEND-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS 
                               QSP1-WBEG-SUBCLASS
                               QSP1-WEND-SUBCLASS.
           MOVE 0           TO SP-LAWCODE
                               QSP1-WBEG-LAWCODE.
           MOVE ALL "9"     TO QSP1-WEND-LAWCODE.
           PERFORM START-SP1-FILE.
       GET-EFF-SPR-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO GET-EFF-SPR-EXIT.

           IF NOT (SP-ORGST = LN-ORGST AND
                    SP-SPRCLASS = LN-SPRCLASS AND
                     SP-SUBCLASS = LN-SUBCLASS)
              MOVE 9 TO IO-FG
              GO TO GET-EFF-SPR-EXIT.

           IF SP-EFFDATE > LN-LOANDATE
              GO TO GET-EFF-SPR-NEXT
           ELSE
              IF SP-EXPDATE = 0
                 GO TO GET-EFF-SPR-EXIT.

           IF SP-EXPDATE <= LN-LOANDATE
              GO TO GET-EFF-SPR-NEXT.

       GET-EFF-SPR-EXIT.
           EXIT.


      *************************
       CREATE-LT-RECORD SECTION.
      *************************

      * ONLY CREATE LT IF SOMETHING IS THERE
           IF TAB-MAKE = SPACES AND
               TAB-MODEL = SPACES AND
                 TAB-VIN = SPACES 
              GO TO EXIT-CREATE-LT-RECORD.

           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 0
              GO TO EXIT-CREATE-LT-RECORD.

           PERFORM CLEAR-LTC-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.

           MOVE "AUTO"        TO LTC-DESC(1).
           MOVE TAB-MAKE      TO LTC-MAKE(1).
           MOVE TAB-MODEL     TO LTC-MODEL(1).
           MOVE TAB-YEAR      TO LTC-YEAR(1).
           MOVE TAB-VIN       TO LTC-VIN(1).
           MOVE TAB-MILEAGE   TO LTC-MILEAGE(1).

           MOVE 0 TO LTC-SELLPR(1) LTC-MISC-AMT(1) LTC-VIN-CHG-DATE(1)
                     LTC-VALUE(1).

           MOVE IX-PATH TO CREAIX-IX-PATH.
           PERFORM WRITE-LTC1-FILE.

       EXIT-CREATE-LT-RECORD.
           EXIT.

      *************************
       CREATE-LS-RECORD SECTION.
      *************************

           PERFORM CLEAR-LSM-FILE.

           MOVE LN-OWNBR  TO LSM-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1         TO LSM-SEQNO.

           MOVE TAB-MISC1 TO LSM-MISC-FIELD(1).
           MOVE TAB-MISC2 TO LSM-MISC-FIELD(2).
           MOVE TAB-MISC3 TO LSM-MISC-FIELD(3).
           MOVE TAB-MISC4 TO LSM-MISC-FIELD(4).
           MOVE TAB-MISC5 TO LSM-MISC-FIELD(5).

           PERFORM WRITE-LSM1-FILE.

      **************************
       CREATE-BF-RECORD SECTION.
      **************************

           PERFORM CLEAR-LP-FILE.
 
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
 
           MOVE LN-DATE-PAID-LAST TO LP-TRDATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE LN-LOANDATE TO LP-TRDATE.
 
           MOVE LP-TRDATE    TO LP-PAYDATE.
           MOVE HOLD-SYSDATE TO LP-POSTDATE.
 
           MOVE "BF"     TO LP-TRCD.
           MOVE "BAL FW" TO LP-REFNO.
 
           MOVE LN-AMT-PAID-LAST TO LP-TRAMT.
           MOVE LN-LCPDTH-DATE   TO LP-PDTH-DATE LP-LCPDTH-DATE.
 
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-CURBAL TO LP-CURBAL.
 
           MOVE 1 TO LP-TILLNO.
 
           MOVE LN-SSNO(1) TO LP-SSNO.
           MOVE LN-OWNBR   TO LP-BRNO.
 
           PERFORM WRITE-LP1-FILE.

       EXIT-CREATE-BF-RECORD.
           EXIT.
 
      ************************************
       GET-NEXT-ACCT-NO SECTION.
      ************************************
           MOVE 1 TO GB-KEY.
           PERFORM OPEN-GB-FILE.
           PERFORM READ-GB-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO TAB-ACCT
              GO TO EXIT-GET-NEXT-ACCT-NO.

           IF GB-NEXT-SF = 0
              ADD 1 TO GB-NEXT-SF.

       GET-NEXT-ACCT-AGAIN.
           MOVE BR-NO      TO LN-OWNBR.
           MOVE GB-NEXT-SF TO LN-ACCTNO.

           ADD 1 TO GB-NEXT-SF.
           IF GB-NEXT-SF > 98999999
              MOVE 0 TO TAB-ACCT
              GO TO EXIT-GET-NEXT-ACCT-NO.

           PERFORM READ-LN1-FILE.
           IF IO-FG = 0
              GO TO GET-NEXT-ACCT-AGAIN.
      *    UNLOCK LN-FILE.

      * HERE IF IO-FG IS BAD, WHICH MEANS A LOAN DOES NOT EXIST
      * WITH THIS NUMBER SO YOU ARE GOOD TO CONTINUE WITH THIS NUMBER

           MOVE LN-ACCTNO TO TAB-ACCT.

           PERFORM REWRITE-GB-FILE.

       EXIT-GET-NEXT-ACCT-NO.
           PERFORM CLOSE-GB-FILE.

      ***********************************************
       PURGE-OPEN-OUTPUT-IX SECTION.
      ***********************************************

           PERFORM OPEN-IX1-FILE.

           MOVE IX-PATH-OWNBR TO IX-BRNO.
           MOVE SPACES        TO IX-CODE.
           MOVE ZEROES        TO IX-ACCTNO.
           MOVE ZEROES        TO IX-SEQNO.

           PERFORM START-IX1-FILE.

       PURGE-OPEN-OUTPUT-IX-NEXT.
           PERFORM READ-IX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IX-BRNO NOT = IX-PATH-OWNBR )
              PERFORM CLOSE-IX1-FILE
              GO TO PURGE-OPEN-OUTPUT-IX-EXIT.

           PERFORM DELETE-IX1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-IX-NEXT.
       PURGE-OPEN-OUTPUT-IX-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-TR SECTION.
      ***********************************************
           PERFORM REMOVE-TRB-RECORDS.
           PERFORM REMOVE-TRC-RECORDS.
           PERFORM REMOVE-TRD-RECORDS.
           PERFORM REMOVE-TRI-RECORDS.
           PERFORM REMOVE-TRJ-RECORDS.
           PERFORM REMOVE-TRK-RECORDS.
           PERFORM REMOVE-TRL-RECORDS.
           PERFORM REMOVE-TRP-RECORDS.
           PERFORM REMOVE-TRS-RECORDS.
           PERFORM REMOVE-TRW-RECORDS.
           PERFORM REMOVE-TRX-RECORDS.

       PURGE-OPEN-OUTPUT-TR-EXIT.
           EXIT.

      *****************************************
       REMOVE-TRB-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE BR-NO    TO TB-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TB-DATE.
           MOVE 0        TO TB-CLASS TB-ACCTNO
                            TB-LAWCODE TB-LINE.

           PERFORM START-TRB1-FILE.
       NEXT-TRB1-DELETE.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-BAD) OR (TB-BRNO NOT = TRB-PATH-OWNBR)
              GO TO REMOVE-TRB-RECORDS-EXIT.

           PERFORM DELETE-TRB1-FILE.

           GO TO NEXT-TRB1-DELETE.

       REMOVE-TRB-RECORDS-EXIT.
           PERFORM CLOSE-TRB1-FILE.

      *****************************************
       REMOVE-TRC-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO    TO TC-BRNO.
           MOVE 19000101 TO TC-KEY-DATE.
           MOVE 0        TO TC-CKNO TC-CLASS TC-SEQ.

           PERFORM START-TRC1-FILE.
       NEXT-TRC1-DELETE.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-BAD) OR (TC-BRNO NOT = TRC-PATH-OWNBR)
              GO TO REMOVE-TRC-RECORDS-EXIT.

           PERFORM DELETE-TRC1-FILE.

           GO TO NEXT-TRC1-DELETE.

       REMOVE-TRC-RECORDS-EXIT.
           PERFORM CLOSE-TRC1-FILE.

      *****************************************
       REMOVE-TRD-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO    TO TD-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TD-DATE.

           PERFORM START-TRD1-FILE.
       NEXT-TRD1-DELETE.
           PERFORM READ-TRD1-FILE-NEXT.
           IF (IO-BAD) OR (TD-BRNO NOT = TRD-PATH-OWNBR)
              GO TO REMOVE-TRD-RECORDS-EXIT.

           PERFORM DELETE-TRD1-FILE.

           GO TO NEXT-TRD1-DELETE.

       REMOVE-TRD-RECORDS-EXIT.
           PERFORM CLOSE-TRD1-FILE.

      *****************************************
       REMOVE-TRI-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRI1-FILE.

           MOVE BR-NO    TO TI-BRNO.
           MOVE 19000101 TO TI-KEY-DATE.
           MOVE 0        TO TI-SEQ.

           PERFORM START-TRI1-FILE.
       NEXT-TRI1-DELETE.
           PERFORM READ-TRI1-FILE-NEXT.
           IF (IO-BAD) OR (TI-BRNO NOT = TRI-PATH-OWNBR)
              GO TO REMOVE-TRI-RECORDS-EXIT.

           PERFORM DELETE-TRI1-FILE.

           GO TO NEXT-TRI1-DELETE.

       REMOVE-TRI-RECORDS-EXIT.
           PERFORM CLOSE-TRI1-FILE.

      *****************************************
       REMOVE-TRJ-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE BR-NO    TO TJ-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TJ-DATE.
           MOVE 0        TO TJ-CLASS TJ-ACCTNO
                            TJ-LAWCODE TJ-LINE.

           PERFORM START-TRJ1-FILE.
       NEXT-TRJ1-DELETE.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-BAD) OR (TJ-BRNO NOT = TRJ-PATH-OWNBR)
              GO TO REMOVE-TRJ-RECORDS-EXIT.

           PERFORM DELETE-TRJ1-FILE.

           GO TO NEXT-TRJ1-DELETE.

       REMOVE-TRJ-RECORDS-EXIT.
           PERFORM CLOSE-TRJ1-FILE.

      *****************************************
       REMOVE-TRK-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE BR-NO    TO TK-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TK-DATE.
           MOVE 0        TO TK-CLASS TK-ACCTNO
                            TK-LAWCODE TK-LINE.

           PERFORM START-TRK1-FILE.
       NEXT-TRK1-DELETE.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-BAD) OR (TK-BRNO NOT = TRK-PATH-OWNBR)
              GO TO REMOVE-TRK-RECORDS-EXIT.

           PERFORM DELETE-TRK1-FILE.

           GO TO NEXT-TRK1-DELETE.

       REMOVE-TRK-RECORDS-EXIT.
           PERFORM CLOSE-TRK1-FILE.

      *****************************************
       REMOVE-TRL-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO    TO TL-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TL-DATE.
           MOVE 0        TO TL-CLASS TL-ACCTNO
                            TL-LAWCODE TL-LINE.

           PERFORM START-TRL1-FILE.
       NEXT-TRL1-DELETE.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-BAD) OR (TL-BRNO NOT = TRL-PATH-OWNBR)
              GO TO REMOVE-TRL-RECORDS-EXIT.

           PERFORM DELETE-TRL1-FILE.

           GO TO NEXT-TRL1-DELETE.

       REMOVE-TRL-RECORDS-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      *****************************************
       REMOVE-TRP-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO    TO TP-BRNO.
      *    MOVE " "      TO TP-CODE.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TP-DATE.
           MOVE 0        TO TP-POOLID TP-CLASS TP-ACCTNO TP-LINE.

           PERFORM START-TRP1-FILE.
       NEXT-TRP1-DELETE.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-BAD) OR (TP-BRNO NOT = TRP-PATH-OWNBR)
              GO TO REMOVE-TRP-RECORDS-EXIT.

           PERFORM DELETE-TRP1-FILE.

           GO TO NEXT-TRP1-DELETE.

       REMOVE-TRP-RECORDS-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
       REMOVE-TRS-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO    TO TS-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TS-DATE.
           MOVE 0        TO TS-CLASS TS-ACCTNO TS-LAWCODE TS-LINE.

           PERFORM START-TRS1-FILE.
       NEXT-TRS1-DELETE.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-BAD) OR (TS-BRNO NOT = TRS-PATH-OWNBR)
              GO TO REMOVE-TRS-RECORDS-EXIT.

           PERFORM DELETE-TRS1-FILE.

           GO TO NEXT-TRS1-DELETE.

       REMOVE-TRS-RECORDS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      *****************************************
       REMOVE-TRW-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRW1-FILE.

           MOVE BR-NO    TO TW-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TW-DATE.
           MOVE 0        TO TW-TILLNO.

           PERFORM START-TRW1-FILE.
       NEXT-TRW1-DELETE.
           PERFORM READ-TRW1-FILE-NEXT.
           IF (IO-BAD) OR (TW-BRNO NOT = TRW-PATH-OWNBR)
              GO TO REMOVE-TRW-RECORDS-EXIT.

           PERFORM DELETE-TRW1-FILE.

           GO TO NEXT-TRW1-DELETE.

       REMOVE-TRW-RECORDS-EXIT.
           PERFORM CLOSE-TRW1-FILE.

      *****************************************
       REMOVE-TRX-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRX1-FILE.

           MOVE BR-NO    TO TX-BRNO.
           MOVE 19000101 TO TX-DATE.
           MOVE 0        TO TX-CLASS TX-ACCTNO
                            TX-LAWCODE TX-LINE.

           PERFORM START-TRX1-FILE.
       NEXT-TRX1-DELETE.
           PERFORM READ-TRX1-FILE-NEXT.
           IF (IO-BAD) OR (TX-BRNO NOT = TRX-PATH-OWNBR)
              GO TO REMOVE-TRX-RECORDS-EXIT.

           PERFORM DELETE-TRX1-FILE.

           GO TO NEXT-TRX1-DELETE.

       REMOVE-TRX-RECORDS-EXIT.
           PERFORM CLOSE-TRX1-FILE.

      ***********************************************
       PURGE-OPEN-OUTPUT-BW SECTION.
      ***********************************************

           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO         TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.

       PURGE-OPEN-OUTPUT-BW-NEXT.
           PERFORM READ-BW1-FILE-NEXT.
           IF ( IO-BAD ) OR ( BW-OWNBR NOT = BW-PATH-OWNBR )
              PERFORM CLOSE-BW1-FILE
              GO TO PURGE-OPEN-OUTPUT-BW-EXIT.

           PERFORM DELETE-BW1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-BW-NEXT.


       PURGE-OPEN-OUTPUT-BW-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LN SECTION.
      ***********************************************

           PERFORM OPEN-LN1-FILE.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       PURGE-OPEN-OUTPUT-LN-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              PERFORM CLOSE-LN1-FILE
              GO TO PURGE-OPEN-OUTPUT-LN-EXIT.

           PERFORM DELETE-LN1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LN-NEXT.

       PURGE-OPEN-OUTPUT-LN-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LP SECTION.
      ***********************************************

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE 0             TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.

           PERFORM START-LP1-FILE.

       PURGE-OPEN-OUTPUT-LP-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LP-BRNO NOT = LP-PATH-OWNBR )
              PERFORM CLOSE-LP1-FILE
              GO TO PURGE-OPEN-OUTPUT-LP-EXIT.

           PERFORM DELETE-LP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LP-NEXT.
       PURGE-OPEN-OUTPUT-LP-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-NO SECTION.
      ***********************************************

           PERFORM OPEN-NO1-FILE.

           MOVE BR-NO         TO NO-BRNO.
           MOVE 0             TO NO-ACCTNO.

           PERFORM START-NO1-FILE.

       PURGE-OPEN-OUTPUT-NO-NEXT.
           PERFORM READ-NO1-FILE-NEXT.
           IF ( IO-BAD ) OR ( NO-BRNO NOT = NO-PATH-OWNBR )
              PERFORM CLOSE-NO1-FILE
              GO TO PURGE-OPEN-OUTPUT-NO-EXIT.

           PERFORM DELETE-NO1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-NO-NEXT.

       PURGE-OPEN-OUTPUT-NO-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LM SECTION.
      ***********************************************

           PERFORM OPEN-LM1-FILE.
           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE 0             TO LM-ACCTNO LM-DATE-REVERSED LM-SEQ.

           PERFORM START-LM1-FILE.

       PURGE-OPEN-OUTPUT-LM-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LM-BRNO NOT = LM-PATH-OWNBR )
              PERFORM CLOSE-LM1-FILE
              GO TO PURGE-OPEN-OUTPUT-LM-EXIT.

           PERFORM DELETE-LM1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LM-NEXT.

       PURGE-OPEN-OUTPUT-LM-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LTC SECTION.
      ***********************************************

           PERFORM OPEN-LTC1-FILE.
           MOVE LTC-PATH-OWNBR TO LTC-BRNO.
           MOVE ZEROES         TO LTC-ACCTNO.
           MOVE ZEROES         TO LTC-SEQNO.

           PERFORM START-LTC1-FILE.

       PURGE-OPEN-OUTPUT-LTC-NEXT.
           PERFORM READ-LTC1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LTC-BRNO NOT = LTC-PATH-OWNBR )
              PERFORM CLOSE-LTC1-FILE
              GO TO PURGE-OPEN-OUTPUT-LTC-EXIT.

           PERFORM DELETE-LTC1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LTC-NEXT.

       PURGE-OPEN-OUTPUT-LTC-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LSM SECTION.
      ***********************************************

           PERFORM OPEN-LSM1-FILE.
           MOVE LSM-PATH-OWNBR TO LSM-BRNO.
           MOVE ZEROES         TO LSM-ACCTNO.
           MOVE ZEROES         TO LSM-SEQNO.

           PERFORM START-LSM1-FILE.

       PURGE-OPEN-OUTPUT-LSM-NEXT.
           PERFORM READ-LSM1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSM-BRNO NOT = LSM-PATH-OWNBR )
              PERFORM CLOSE-LSM1-FILE
              GO TO PURGE-OPEN-OUTPUT-LSM-EXIT.

           PERFORM DELETE-LSM1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LSM-NEXT.

       PURGE-OPEN-OUTPUT-LSM-EXIT.
           EXIT.

      **************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************
       HEAD-PAGE SECTION.
      **************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       END-HEAD-PAGE.
           MOVE 4 TO LN-FEED.
           MOVE 7 TO LN-COUNT.

      ****************************************
       PRINT-TOTALS SECTION.
      ****************************************
           MOVE 4 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           MOVE "TOTALS..............." TO R-COUNTERX.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECORDS READ........." TO R-COUNTERX.
           MOVE RECORD-COUNTER TO R-COUNTER.
           MOVE TOTAL-BALANCES TO R-BALANCES.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECORDS WRITTEN......" TO R-COUNTERX.
           MOVE WRITTEN-COUNTER TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "OUT OF BALANCE ACCTS." TO R-COUNTERX.
           MOVE OUT-OF-BALANCE TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DUPLICATE ACCOUNT #.." TO R-COUNTERX.
           MOVE ACCT-ALREADY-EXIST      TO R-COUNTER.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NEGATIVE BAL ACCTS..." TO R-COUNTERX.
           MOVE NEGATIVE-BAL-COUNT      TO R-COUNTER.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BRANCH"   TO R-COUNTERX.
           MOVE BEG-BRANCH TO R-ALPHA.
           MOVE 5          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER"   TO R-COUNTERX.
           MOVE DEALER-NO  TO R-BALANCES.
           MOVE 1          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CLEAR FILES" TO R-COUNTERX.
           MOVE CLEAR-FILES   TO R-ALPHA.
           MOVE 1          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCRUAL DATE" TO R-COUNTERX.
           MOVE ACCRUAL-DATE   TO R-ALPHA.
           MOVE 1          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INSURANCE CO" TO R-COUNTERX.
           MOVE INS-COMPANY    TO R-ALPHA.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LPO" TO R-COUNTERX.
           MOVE LOAN-LPO   TO R-ALPHA.
           MOVE 1          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ********************************************************
      *    COMPUTE ACCRUAL EARNINGS THRU END OF MONTH:
      ********************************************************
       ACCRUAL-METHOD SECTION.

      * JUST USE SAME ACCRUAL DATE THAT WAS ENTERED ON THE SCREEN, THIS IS
      * USED TO STORE THE LAST REAL ACCRUAL, IN CASE IT WAS STOPPED, IT WILL
      * KEEP THIS DATE AS LAST TRUE DATE ACCRUAL WAS RAN.
           MOVE ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE.
           PERFORM COMPUTE-AERNINGS.
           MOVE CERN TO AERN.
           ADD AERN-EARNINGS(AERN) TO LN-ACCERN(AERN).

      * CANT SET THESE LOAN FLAGS HERE BECUASE IT WILL AFFECT THE SERVICE
      * AND MFEE
           MOVE "Y" TO SET-ACCRUAL-FIELDS.

           MOVE AERN-LAST-ACCRUAL-DATE TO SET-LAST-ACCRUAL-DATE.
           IF AERN-ERNDELQ-STOP = "R"
              MOVE "C" TO SET-ERNDELQ-STOP.

      ********************************************************
      *    COMPUTE CASH EARNINGS
      ********************************************************
       CASH-METHOD SECTION.
           IF LN-DATE-PAID-LAST NOT = 0
              MOVE LN-DATE-PAID-LAST TO CERN-PAYDATE
              COMPUTE CERN-APCUR = LN-LNAMT - LN-CURBAL
              MOVE LN-TOTINT         TO CERN-TOTINT
              MOVE LN-TOTLCHG        TO CERN-TOTLCHG
              MOVE 0                 TO LN-TOTPAYMNTD CERN-PAYMNTD
              MOVE 0                 TO CERN-APINT CERN-APLC
              MOVE LN-LNAMT TO LN-PURBAL LN-CURBAL CERN-PRINBAL
              MOVE "PY"     TO CERN-TRCD
              PERFORM COMPUTE-CERNINGS
              MOVE AERN TO CERN
              ADD CERN-EARNINGS(CERN) TO LN-ACCERN(CERN)
                                         LN-NONACRERN
              MOVE TAB-TOTPAID        TO LN-TOTPAYMNTD
              MOVE TAB-CURBAL         TO LN-CURBAL LN-PURBAL
              IF SP-DLDISC-CONV-BF-PCNT NOT = 0
                 IF CERN = 5
                    MOVE 0 TO LN-ACCERN(5).

       EXIT-CASH-METHOD.
           EXIT.



      ****************************************
       SET-STATUS-FG SECTION.
      ****************************************
           SET STFG TO 1.
       NEXT-STAT.
           IF LN-STATFG(STFG) NOT = SPACE
              SET STFG UP BY 1
              GO TO NEXT-STAT.
           MOVE WK-STATUS TO LN-STATFG(STFG).
           

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPREGZ.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPAERN.CPY".
       COPY "LIBLP/LPCERN.CPY".
       COPY "LIBLP/LPUERN.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPEADJ.CPY".
       COPY "LIBLP/LPLSMCL.CPY".
       COPY "LIBLP/LPLNCL.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPBWCL.CPY".
       COPY "LIBLP/LPLTCCL.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO WACFSB-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WACFSB-SYSDATE.
           DISPLAY WACFSB-SCREEN UPON WACFSB-WINDOW-HANDLE.
           MOVE WACFSB-FORM-FIELDS TO WR-BUF.
           MOVE WACFSB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE WACFSB-HELP--FILE TO HELP-LINK-FILE.
           MOVE WACFSB-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WACFSB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       CHECK-INCREMENT-LOANS.
           IF TAB-CURBAL = 0
              ADD 1 TO BW-PRLOANS
           ELSE
              ADD 1 TO BW-LOANS.
           IF LN-SMALLOAN = "Y"
              ADD TAB-CURBAL TO BW-LIASMLN
           ELSE
              ADD TAB-CURBAL TO BW-LIAOTH.
       WHICH-METHOD.
           IF SP-ERNFRMLA(CERN) > 50
              PERFORM ACCRUAL-METHOD
           ELSE
              PERFORM CASH-METHOD.
           ADD 1 TO AERN CERN.

       SET-FILE-PATHS.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO BW-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                              DL-PATH-OVERRIDE
                              IX-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              NO-PATH-OVERRIDE
                              LM-PATH-OVERRIDE
                              LSM-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LTC-PATH-OVERRIDE
                              TRB-PATH-OVERRIDE
                              TRC-PATH-OVERRIDE
                              TRD-PATH-OVERRIDE
                              TRI-PATH-OVERRIDE
                              TRJ-PATH-OVERRIDE
                              TRK-PATH-OVERRIDE
                              TRL-PATH-OVERRIDE
                              TRP-PATH-OVERRIDE
                              TRS-PATH-OVERRIDE
                              TRW-PATH-OVERRIDE
                              TRX-PATH-OVERRIDE.

       SET-REBATE-FLAGS.
           MOVE TAB-INT-REBATE-FLAG TO LN-INT-REBATE.
           IF LN-SERCHG NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-SC-REBATE.
           IF LN-SUM-MAINTFEE NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-MF-REBATE.
           IF LN-INSPREM(1) NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-CL-REBATE.
           IF LN-INSPREM(2) NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-AH-REBATE.
           IF LN-INSPREM(3) NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-PP-REBATE.
           IF LN-INSPREM(4) NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-O1-REBATE.
           IF LN-INSPREM(6) NOT = 0
              MOVE TAB-INT-REBATE-FLAG TO LN-O3-REBATE.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       OPEN-TAB-FILE.
           PERFORM OPEN-IT.
           MOVE TAB-IFN TO E-FILE.
           OPEN INPUT TAB-FILE.
           IF IO-FG = 8
              GO TO OPEN-TAB-FILE.
           IF IO-FG = 7
              CLOSE TAB-FILE
              GO TO OPEN-TAB-FILE.
       READ-TAB-FILE.
           PERFORM READ-IT.
           MOVE TAB-IFN TO E-FILE.
           READ TAB-FILE.
           IF IO-FG = 8
              GO TO READ-TAB-FILE.
       CLOSE-TAB-FILE.
           PERFORM CLOSE-IT.
           CLOSE TAB-FILE.
      *OPEN-TAB-LP-FILE.
      *    PERFORM OPEN-IT.
      *    MOVE TAB-LP-IFN TO E-FILE.
      *    OPEN INPUT TAB-LP-FILE.
      *    IF IO-FG = 8
      *       GO TO OPEN-TAB-LP-FILE.
      *    IF IO-FG = 7
      *       CLOSE TAB-LP-FILE
      *       GO TO OPEN-TAB-LP-FILE.
      *READ-TAB-LP-FILE.
      *    PERFORM READ-IT.
      *    MOVE TAB-LP-IFN TO E-FILE.
      *    READ TAB-LP-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-TAB-LP-FILE.
      *CLOSE-TAB-LP-FILE.
      *    PERFORM CLOSE-IT.
      *    CLOSE TAB-LP-FILE.
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1IN_NOBI.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPIX1IN.CPY".
       COPY "LIBLP/LPTRB1IN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTRD1IN.CPY".
       COPY "LIBLP/LPTRI1IN.CPY".
       COPY "LIBLP/LPTRJ1IN.CPY".
       COPY "LIBLP/LPTRK1IN.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPTRS1IN.CPY".
       COPY "LIBLP/LPTRW1IN.CPY".
       COPY "LIBLP/LPTRX1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "SP/EXTRMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY WACFSB-WINDOW-HANDLE.
           EXIT PROGRAM.
