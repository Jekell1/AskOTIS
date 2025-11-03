       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFVERI.
       DATE-WRITTEN.    012484.
      ******************************************************************
      *                 (CL)
      *  DESCRIPTION:   SALES FINANCE LOAN - CALLED FROM LONPB0
      *                 VERIFY CONTRACT
      *
      *  !!!! HEY   !!!!   THIS PROGRAM HAS BEEN ALTERED SO IT CAN BE CALLED
      *  !!!!!!!!!!!!!!!   FROM MERCURY'S BATCH PROGRAM TO CREATE LOANS
      *  !!!!!!!!!!!!!!!   FROM CMSI'S SAVED LOANS.  I HAD TO CHANGE
      *  !!!!!!!!!!!!!!!   ALL OCCURANCES OF PERFORM SEND-MESS TO
      *  !!!!!!!!!!!!!!!   PERFORM CONDITIONAL-SEND-MESS SO ERRORS
      *  !!!!!!!!!!!!!!!   MIGHT NOT GO TO THE SCREEN, SO PLEASE DON'T
      *  !!!!!!!!!!!!!!!   PERFORM SEND-MESS, PERFORM CONDITIONAL-SEND-MESS
      *
      * REV:
      * JTG 070588 ADDED TEST FOR MEMO DOWN PAYMENT
      * JTG 100588 ADDED VERIFICATION ON PURCHASE DATE
      * SLC 120888 CHANGED LN-ORGBR TO LN-OWNBR
      * JTG 011190 STOPPED EDIT ON DL-EARNCD IF I/B
      * JTG 040590 SKIPPED BIG APPLE EARN EDITS FOR SERVACCTS
      * JTG 081390 STOPPED PARTICIPATION AND INS. ADDONS
      * BAH 061992 REMOVED PROCEEDS VERIFICATION, DO IN "B0"
      * JTG 032593 REVISED TO PASS GB-REC, BR-REC & SP-REC
      * JTG 042293 REVISED TO WORK WITH NEW DATE ROUTINES
      * JTG 052193 REVISED FOR BALLOONS AND ALLOWED EDIT ON EFFRATE
      * JTG 111593 ALLOWED ENTRY OF BR-DOWNPY-FLAG = 'F' FAIRCO,
      *            WHICH WILL ALLOW SELL PRICE TO BE LESS THAN
      *            THE DOWN PAYMENT
      * SLC 123093 ADDED CODE TO DISALLOW BOOKING A LOAN OF DL-BRANCH NOT =
      *            LN-OWNBR, WHEN BR-BOOK-SF-BR-ONLY = 'Y'.
      * JTG 010294 ADDED GB-DLALLOC-GL  (FAIRCO)
      * BAH 090994 ADDED MOVE OF 999999999.99 TO N-WK2 IF RECOURSE
      *            CEILINGS IN DL RECORD ARE 9999999.99, NOT BIG ENOUGH!
      * JTG 082395 ADDED TEST FOR BR-AH-AND-CL
      * MJD 091495 ADDED LOGIC FOR DS-DATE.
      * JFF 031196 ADDED TEST FOR VALID PURPOSE CODE.
      * JTG 040296 ADDED TEST FOR  DEALER CHECK NEGATIVE
      * MJD 081696 ADDED TEST OF LN-SSNO(2) THRU LN-SSNO(4).
      * CLS 021197 DON'T GO TO IO-ERROR ON BAD DL READ!
      * JTG 051297 ADDED USE OF 'LIBLP/SETCTBLW' & 'LIBLP/SETCTBL'
      * GLF 082997 REMOVED ALL USE OF GLOBAL
      * BAH 012698 IF INSURED IS "S", TEST SECOND MAKER'S AGE EDITS, PR#641
      * JTG 060598 ADDED LOGIC FOR SF RENEWALS INTO SF, MERCURY #176
      * BAH 101298 DONT TEST PURDATE WITHING ACCTNG PERIOD IF JUST DOING A
      *            SAVE, TEST ACTION, NATIONS FUNDING, PR# 768
      * JTG 990126 REMOVED ENTRIES REFERENCING DL-EARNCD = 1, WHICH WAS TESTING
      *            EARNFRMLA(6) WHICH NOW IS USED FOR LOAN COSTS
      * MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *            NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * BAH 010723 VERIFY THAT INS PREMIUMS MATCH INS TYPES FLAG, PL# 308
      * BLV 010727 ADD LOGIC TO ALLOW THIS PROGRAM TO BE CALLED FROM LN/LNCREA,
      *            MERCURY AUTOMATIC UPDATE OF SAVED CMSI LOANS, MERC PR# 253
      * BAH 011126 ADDED TEST FOR SP-CL-AH-MAXIMUMS, THEY WERE CHANGING THE
      *            LOAN TERM AFTER THE INSURANCE WAS CALCULATED !!
      *            REGENCY PR# 1643
      * BAH 011203 ADDED SP-INSEXCEPT 14, REGENCY  (SP-ORGTERM-MAXIMUM) PR#1657
      * CS  020619 ADDED DISPLAY OF A VERIFICATION INPUT WINDOW [LIBWI/POPVFY]
      *            THAT ALLOWS USER 1 MORE CHANCE TO OPT OUT OF UPDATE
      * JTG 020628 ADDED RP-RATE-ALLOW-OVERRIDE                  LENDMARK #1841
      *  CS 060608 ADDED TEST FOR GP-I-AM-SECURITY VERIFY RESERVES TO DEALER
      *            NUMBERS PR#2503 SECURITY AUTO
      * BAH 060728 PASSWORD PROTECT A VIN THAT IS NOT 17 CHAR, REGACC PR# 141
      * BAH 070913 ADDED VALIDATION OF SPECIFIC BORROWER FIELDS AS A MUST
      *            FILL IN FOR GP-I-AM-REGENCY ONLY, REGWAN PR# 3031
      *  CS 090728 ADDED TO GP-I-AM-SECURITY VERIFY RESERVES TO DEALER NUMBER
      *            AND CLASS SECURITY AUTO PR#3402
      * BAH 110429 DO NOT ALLOW 0 LN-LNAMT UNLESS REVOLVING, REGWAN PL# 738
      * BAH 130214 PASSWORD COLLCD "US" OR "UN" WITH PP OR O2 INSPREM
      *                                                     REGMGM, PR# 4210
      * BAH 130509 MUST HAVE INSCOVR FOR ANY INSURANCE WRITTEN, WORLD PR# 880
      * BAH 130529 PASSWORD WRITING PP AND O2 INSURANCE, REGMGM PR# 4282
      * BAH 160304 PASSWORD USING DLNO 9999.99, REGMGM PR#5007
      * KEC 2016.0309 CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *               CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      * BLM 2016.0511 FIX FOR NEW DS-DATE FORMAT, PD0003
      * KEC 2017.0103 CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *               WITH THE DATE FIELDS IN BY & UD FILE.
      * BAH 2019.0625 REMOVED LN-REVOLVING, PT0153
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/SFVERI A30 08/13/18-13:43:13 >".
      **********************************
      *       F I L E   P A T H S      *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  WK-IFN          PIC X(2)  VALUE "WK".
       01  WK-KEY          PIC 9(3).

       01  HOLD-VIN.
           03  HOLD-VIN-16 PIC X(16).
           03  HOLD-VIN-17 PIC X.
           03  HOLD-VIN-3  PIC X(3).

       01  AMT-HOLDER      PIC S9(9)V99    COMP-3.
       01  EXTEND-VERIFY   PIC X     VALUE "Y".
       01  MESS1           PIC X(61) VALUE " ".
       01  MSEL-LIST       PIC X(5)  VALUE "MSEL.".
       01  N-WK2           PIC S9(9)V99 COMP-3 VALUE 0.
       01  SUB             PIC 9(6)     COMP-3.
       01  SUB1            PIC 9(6)     COMP-3.
       01  BW-ERRORS       PIC X     VALUE "N".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78 VIN-17-SEQ            VALUE 0.
       78 BW-SEQ                VALUE 1.
       78 US-UN-SEQ             VALUE 3.
       78 PP-O2-SEQ             VALUE 4.
       78 DLNO-SEQ              VALUE 5.
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  ERRORS-EXIST    PIC X.
       01  ACTION          PIC X(6).
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.

       PROCEDURE DIVISION
           USING FORM-PATHNAME ERRORS-EXIST ACTION PROG-BUF
                 EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           PERFORM GET-GPENV.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE USER-PID TO TEMP-PID.

      *    IF LPLOAN-CALLED-FROM = "LNCREA"
      *       PERFORM GET-BR
      *       PERFORM SETUP-LOCAL-PATHS.

           MOVE "N" TO ERRORS-EXIST.
           PERFORM VERIFY-MESSAGE.

           MOVE 1 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           MOVE WK-SAVE-REC TO LN-REC.

      * GET COLLATERAL WORK RECORD
           MOVE 6 TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              MOVE WK-VIN-1 TO HOLD-VIN
              PERFORM VERIFY-VIN
              IF WK-VIN-2 NOT = SPACES
                 MOVE WK-VIN-2 TO HOLD-VIN
                 PERFORM VERIFY-VIN.

           PERFORM CLOSE-WK-FILE.

           PERFORM GET-BR.

           PERFORM GET-SP.

           IF LN-INTDATE < LN-LOANDATE
              MOVE "INTEREST DATE MAY NOT BE < LOAN DATE" TO MESS
              PERFORM ERROR-MESSAGE.

           IF LN-1STPYDATE < LN-INTDATE
              MOVE "1ST PMT DATE MAY NOT BE < INTEREST DATE" TO MESS
              PERFORM ERROR-MESSAGE.

           IF LN-CASHDATE NOT = 0
              IF LN-CASHDATE < LN-LOANDATE
                 MOVE "CASH DATE MAY NOT BE < LOAN-DATE" TO MESS
                 PERFORM ERROR-MESSAGE.

           IF LN-PURDATE < LN-LOANDATE
              MOVE "PURCHASE DATE MAY NOT BE < LOAN DATE" TO MESS
              PERFORM ERROR-MESSAGE.

           MOVE TRANS-DATE TO NUM-DATE.
           MOVE LN-PURDATE TO SYS-DATE.
           IF (NUM-MO NOT = S-MM) OR (NUM-YR NOT = S-YY)
              IF ACTION NOT = "     S"
                 MOVE "PURCHASE DATE MUST BE WITHIN ACCOUNTING MONTH"
                                                             TO MESS
                 PERFORM ERROR-MESSAGE.

           IF LN-LNAMT = 0
              MOVE "LOAN AMOUNT MAY NOT BE ZERO" TO MESS
              PERFORM ERROR-MESSAGE.

      * VERIFY DEALER CHECK NOT NEGATIVE:
           IF LN-PROCEEDS < 0
              MOVE "DEALER CHECK CAN NOT BE NEGATIVE" TO MESS
              PERFORM ERROR-MESSAGE.

      * VERIFY-PURPOSE-CODE:
           MOVE LN-PURCD TO CD-CODE.
           MOVE "PU"     TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE "INVALID PURPOSE CODE" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "Y" TO ERRORS-EXIST.

      * ASK FOR PASSWORD IF THEY TRY TO WRITE BOTH TYPES OF INSURANCE
           IF LN-INSPREM(3) NOT = 0 AND LN-INSPREM(5) NOT = 0
               MOVE "  " TO PASSWDW-STAT
               MOVE PP-O2-SEQ TO PASSWDW-SEQ
               MOVE "TO WRITE PP AND O2 INSURANCE" TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  MOVE "Y" TO ERRORS-EXIST.

      * PASSWORD USING THE DIRECT DEALER 9999.99
      * NEEDS TO BE IN HERE BECAUSE THEY ARE ALLOWED TO USE 9999.99 IN THE
      * QUOTE AND THEN WHEN THEY RECALL IN SALES INPUT, IT DOESN'T GET
      * CAUGHT AT DEALER INPUT, SO CATCH IT WHEN THEY UPDATE
           IF LN-DLNO = 9999.99
              MOVE "  "                  TO PASSWDW-STAT
              MOVE DLNO-SEQ              TO PASSWDW-SEQ
              MOVE "TO USE DLNO 9999.99" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "Y" TO ERRORS-EXIST. 

      * VERIFY LOAN AMOUNT VIA TOTAL OF PAYMENTS:
           COMPUTE N-WK1 = LN-1STPYAMT
                         + (LN-NOREGPY * LN-REGPYAMT)
                                 + LN-LASTPYAMT.
           IF LN-LOANTYPE = "I"
              COMPUTE N-WK1 = N-WK1 - LN-INTCHG - LN-EXTCHG
                              - (LN-ORGTERM * LN-MAINTFEE).
           IF N-WK1 NOT = LN-LNAMT
              MOVE "PAYMENTS DO NOT EQUAL LOAN AMOUNT" TO MESS
              MOVE "Y" TO EXTEND-VERIFY
              PERFORM ERROR-MESSAGE.

           MOVE LN-SELLPR TO N-WK1.
           IF BR-DOWNPY-FLAG NOT = "Y"
              SUBTRACT LN-DOWNPY LN-TRADEIN FROM N-WK1.

           IF BR-DOWNPY-FLAG NOT = "F"
              IF N-WK1 < 0
                 MOVE "AMOUNT FINANCED MAY NOT BE < 0" TO MESS
                 PERFORM ERROR-MESSAGE.

           IF LN-FINCHG < SP-MINSF-CHG
              MOVE "FINANCE CHARGE BELOW MINIMUM" TO MESS
              PERFORM ERROR-MESSAGE.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF LN-INSPREM(SUB1) NOT = 0
                 IF LN-INSCOVR(SUB1) = 0
                    MOVE "MISSING INSURANCE COVERAGE" TO MESS
                    PERFORM ERROR-MESSAGE
                 END-IF
              END-IF
           END-PERFORM.

           IF ( LN-INSURED NOT = "N" )
              GO TO VERIFY-AH-AND-CL.

           MOVE 0 TO IO-FG.

           IF ( LN-INSCOMP(1) NOT = 0 ) OR
              ( LN-INSPREM(1) NOT = 0 )
              MOVE 9 TO IO-FG.

           IF ( LN-INSCOMP(2) NOT = 0 ) OR
              ( LN-INSPREM(2) NOT = 0 )
              MOVE 9 TO IO-FG.

           IF ( LN-INSCOMP(3) NOT = 0 ) OR
              ( LN-INSPREM(3) NOT = 0 )
              MOVE 9 TO IO-FG.

           IF ( LN-INSCOMP(4) NOT = 0 ) OR
              ( LN-INSPREM(4) NOT = 0 )
              MOVE 9 TO IO-FG.

           IF ( IO-BAD )
              MOVE "INS CO/PREMIUM MUST BE 0 FOR INS CD 'N'" TO MESS
              PERFORM ERROR-MESSAGE.


           GO TO VERIFY-DEALER-RECOURSE.

       VERIFY-AH-AND-CL.

           IF BR-AH-AND-CL = "Y"
              IF (LN-INSPREM(2) NOT = 0)
                         AND (LN-INSOURS(2) = "Y")
                 IF LN-INSPREM(1) = 0
                    MOVE "MUST WRITE CREDIT LIFE TO WRITE A&H"
                                                        TO MESS
                    PERFORM ERROR-MESSAGE.

           IF ( LN-INSURED = "S" OR "J" )
               IF ( LN-SSNO(2) = ZEROES )
                  MOVE "JOINT COVERAGE NOT ALLOWED" TO MESS
                  PERFORM ERROR-MESSAGE.

      * VERIFY INSURANCE CODES MATCH PREMIUMS, PR# 308  7/23/01 BAH
           IF LN-INSTYPES-LL  PERFORM LEVEL-INSURANCE.
           IF LN-INSTYPES-DL  PERFORM DECREASING-INSURANCE.
           IF LN-INSTYPES-AH  PERFORM ACCIDENT-INSURANCE.
           IF LN-INSTYPES-PP  PERFORM PROPERTY-INSURANCE.
           IF LN-INSTYPES-PP2 PERFORM PROPERTY-INSURANCE.
           IF LN-INSTYPES-O1  PERFORM OTHER1-INSURANCE.

       VERIFY-DEALER-RECOURSE.
           MOVE LN-DLNO TO DL-DLNO DS-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-BAD
              MOVE "DEALER RECORD NOT FOUND" TO MESS
              PERFORM ERROR-MESSAGE.

           IF LN-DLRECOURSE = "F"
              MOVE 1 TO SUB
              MOVE DL-FULLCEIL TO N-WK2.
           IF LN-DLRECOURSE = "P"
              MOVE 2 TO SUB
              MOVE DL-PARTCEIL TO N-WK2.
           IF LN-DLRECOURSE = "N"
              MOVE 3 TO SUB
              MOVE DL-NONCEIL TO N-WK2.
           IF N-WK2 = 9999999.99
              MOVE 999999999.99 TO N-WK2.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR TO DS-BRANCH.
           MOVE LN-CLASS TO DS-CLASS.
           PERFORM OPEN-DS1-FILE.
           PERFORM READ-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-BAD
              MOVE 0 TO DS-RCOURSE-AMT(SUB).

           IF LN-DLRECOURSE = "P"
              ADD LN-DLPARTRCAMT DS-RCOURSE-AMT(SUB) GIVING N-WK1
            ELSE
              ADD LN-LNAMT DS-RCOURSE-AMT(SUB) GIVING N-WK1.

           IF N-WK1 > N-WK2
              MOVE "DEALER RECOURSE CEILING EXCEEDED" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-DLRECOURSE = "P"
              IF LN-DLPARTRCAMT > LN-LNAMT
                 MOVE "PARTIAL RECOURSE AMOUNT EXCEEDS LOAN AMOUNT"
                                                     TO MESS
                 PERFORM ERROR-MESSAGE.


           IF LN-LOANTYPE = "I" OR LN-SERVACCT = "Y"
              GO TO VERIFY-AGAINST-SPEDIT.

           IF DL-EARNCD = 0 AND SP-ERNFRMLA(1) = 0
              MOVE "INT. EARN FORMULA IN SPR MUST NOT BE 0." TO MESS
              PERFORM ERROR-MESSAGE.

           IF DL-EARNCD = 1
                   AND (SP-ERNFRMLA(1) NOT = 0 OR
                        SP-ERNFRMLA(2) NOT = 0 OR
                        SP-ERNFRMLA(3) NOT = 0 OR
                        SP-ERNFRMLA(4) NOT = 0 OR
                        SP-ERNFRMLA(5) NOT = 0)
              MOVE "EARN FORMULAS 1 - 5 IN SPR MUST BE 0." TO MESS
              PERFORM ERROR-MESSAGE.

       VERIFY-AGAINST-SPEDIT.

           MOVE LN-COLLCD TO CD-CODE.
           MOVE "CO"      TO CD-TYPE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE "CD RECORD NOT FOUND" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "Y" TO ERRORS-EXIST.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "MAKER BORROWER RECORD NOT FOUND" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "Y" TO ERRORS-EXIST
              MOVE 0   TO BW-BDATE BW-REFDATE.

           MOVE BW-BDATE   TO VERIFY-AGE.
           MOVE BW-REFDATE TO VERIFY-REFDATE.

      * TEST BORROWERS SOCIAL SECURITY NUMBERS
      
           PERFORM VARYING SUB1 FROM 2 BY 1 UNTIL SUB1 > 4
              IF LN-SSNO(SUB1) NOT = 0
                 MOVE LN-SSNO(SUB1) TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 IF IO-BAD
                    MOVE "JOINT/ENDORSER BORROWER RECORD NOT FOUND"
                                                           TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "Y" TO ERRORS-EXIST
                 ELSE
                    IF ( SUB1 = 2 ) AND ( LN-INSURED = "S" )
                       MOVE BW-BDATE    TO VERIFY-AGE
                       MOVE BW-REFDATE  TO VERIFY-REFDATE
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

           PERFORM CLOSE-BW1-FILE.

           PERFORM APRS-RATE.
           MOVE APRS-SMPRATE TO LN-SMPRATE.
           MOVE APRS-EFFRATE TO LN-EFFRATE.

           MOVE LN-APRATE  TO VERIFY-TERMCEIL-APR.
           MOVE LN-EFFRATE TO VERIFY-TERMCEIL-EFF.
           MOVE "Y"        TO VERIFY-TERMCEIL-EFF-FG.
           MOVE " "        TO VERIFY-REJECT-TERM-FG.
           PERFORM VERIFY-TERMCEIL.
           IF VALID-LOAN = "N"
              MOVE "Y" TO ERRORS-EXIST.

      * VERIFY ANTICIPATED
      *    THAT RESERVE FROM CHARGES AND/OR PARTICIPATING RESERVES 
      *    WILL NOT EXCEED INTEREST CHARGE
      
           COMPUTE AMT-HOLDER =
              LN-DLRVCHGS(1) + LN-DLRVCHGS(2) + LN-DLRVCHGS(3)
                    + LN-DLPARVHELD + LN-DLPARVPAID.
           IF LN-INTCHG < AMT-HOLDER
              MOVE
              "PARTIC AND/OR RESERVE FROM CHGS EXCEEDS INTEREST CHARGE"
                                                        TO MESS
              PERFORM ERROR-MESSAGE.

      * VERIFY PRIOR LOAN
      
           IF LN-SRCD = "RN" OR "PB" OR "RB" OR "RO"
              IF LN-PRLNNO = 0
                 MOVE "PRIOR LOAN NO. REQUIRED FOR LOAN RENEWALS"
                                                     TO MESS
                 PERFORM ERROR-MESSAGE.

           MOVE MENU-PROG-FORM(2) TO NEW-PROG-FORM.
           MOVE "P" TO ERRCD.
           IF ERRORS-EXIST = "Y"
              MOVE "CORRECT ENTRIES AND UPDATE !" TO MESS
              PERFORM ERROR-MESSAGE
              MOVE SPACES TO MESS
              PERFORM CALL-TMMSG
              MOVE "1" TO ROUTE-CD
              GO TO END-ROUTINE.

           MOVE "2" TO NEW-PROG-SUB.
           GO TO END-ROUTINE.

      ***********************************************
       VERIFY-VIN SECTION.
      ***********************************************

           IF HOLD-VIN-17 = SPACES OR HOLD-VIN-3 NOT = SPACES
              MOVE " " TO PASSWDW-STAT
              MOVE VIN-17-SEQ TO PASSWDW-SEQ
              MOVE "TO ACCEPT VIN NOT 17 LONG :" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "VIN MUST BE 17 CHARACTERS LONG" TO MESS
                 PERFORM ERROR-MESSAGE.

       EXIT-VERIFY-VIN.
           EXIT.

      ***********************************************
       LEVEL-INSURANCE SECTION.
      ***********************************************
           IF LN-INSPREM(1) = 0
              MOVE "CL PREMIUM DOES NOT MATCH INS TYPE FLAG" TO MESS
              PERFORM ERROR-MESSAGE
           ELSE
              IF ( LN-INSURED = "J" )
                 IF ( SP-INSCOM(4) = ZEROES )
                    MOVE "JOINT LEVEL LIFE NOT ALLOWED" TO MESS
                    PERFORM ERROR-MESSAGE
                 ELSE
                    IF SP-CL-AH-MAXIMUMS(4) OR SP-ORGTERM-MAXIMUM(4)
                      IF LN-ORGTERM > SP-INSMAXTERM(4)
                        MOVE "JOINT LEVEL LIFE EXCEEDS MAX TERM" TO MESS
                        PERFORM ERROR-MESSAGE.

           IF ( LN-INSURED NOT = "J" )
              IF ( SP-INSCOM(3) = ZEROES )
                 MOVE "LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE
              ELSE
                 IF SP-CL-AH-MAXIMUMS(3) OR SP-ORGTERM-MAXIMUM(3)
                    IF LN-ORGTERM > SP-INSMAXTERM(3)
                       MOVE "LEVEL LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM ERROR-MESSAGE.

           IF ( BR-AH-AND-CL = "Y" )
              IF ( LN-INSTYPES-AH )
                 IF NOT ( LN-INSTYPES-DL OR LN-INSTYPES-LL )
                    MOVE "MUST WRITE LIFE TO WRITE A&H" TO MESS
                    PERFORM ERROR-MESSAGE.

      ***********************************************
       DECREASING-INSURANCE SECTION.
      ***********************************************
           IF LN-INSPREM(1) = 0
              MOVE "CL PREMIUM DOES NOT MATCH INS TYPE FLAG" TO MESS
              PERFORM ERROR-MESSAGE
           ELSE
              IF ( LN-INSURED = "J" )
                 IF ( SP-INSCOM(2) = ZEROES )
                    MOVE "JOINT DECLINING LIFE NOT ALLOWED" TO MESS
                    PERFORM ERROR-MESSAGE
                 ELSE
                  IF SP-CL-AH-MAXIMUMS(2) OR SP-ORGTERM-MAXIMUM(2)
                    IF LN-ORGTERM > SP-INSMAXTERM(2)
                      MOVE "JNT DECLINING LIFE EXCEEDS MAX TERM" TO MESS
                      PERFORM ERROR-MESSAGE.

           IF ( LN-INSURED NOT = "J" )
              IF ( SP-INSCOM(1) = ZEROES )
                 MOVE "DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE
              ELSE
                 IF SP-CL-AH-MAXIMUMS(1) OR SP-ORGTERM-MAXIMUM(1)
                    IF LN-ORGTERM > SP-INSMAXTERM(1)
                       MOVE "DECLINING LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM ERROR-MESSAGE.

           IF ( BR-AH-AND-CL = "Y" )
              IF ( LN-INSTYPES-AH )
                 IF NOT ( LN-INSTYPES-DL OR LN-INSTYPES-LL )
                    MOVE "MUST WRITE LIFE TO WRITE A&H" TO MESS
                    PERFORM ERROR-MESSAGE.

      ***********************************************
       ACCIDENT-INSURANCE SECTION.
      ***********************************************

           IF LN-INSPREM(2) = 0
              MOVE "A&H PREMIUM DOES NOT MATCH INS TYPE FLAG" TO MESS
              PERFORM ERROR-MESSAGE
           ELSE
              IF ( SP-INSCOM(5) = ZEROES )
                 MOVE "A & H NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE
              ELSE
                 IF SP-CL-AH-MAXIMUMS(5) OR SP-ORGTERM-MAXIMUM(5)
                    IF LN-ORGTERM > SP-INSMAXTERM(5)
                       MOVE "A & H EXCEEDS MAX TERM" TO MESS
                       PERFORM ERROR-MESSAGE.

            IF ( BR-AH-AND-CL = "Y" )
               IF ( LN-INSTYPES-AH )
                 IF NOT ( LN-INSTYPES-DL OR LN-INSTYPES-LL )
                    MOVE "MUST WRITE LIFE TO WRITE A&H" TO MESS
                    PERFORM ERROR-MESSAGE.

      ***********************************************
       PROPERTY-INSURANCE SECTION.
      ***********************************************
           IF LN-INSPREM(3) = 0
              MOVE "PROP PREMIUM DOES NOT MATCH INS TYPE FLAG" TO MESS
              PERFORM ERROR-MESSAGE
           ELSE
              IF ( LN-INSTYPES-PP )
                 IF ( SP-INSCOM(6) = ZEROES )
                    MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
                    PERFORM ERROR-MESSAGE.

           IF ( LN-INSTYPES-PP2 )
              IF ( SP-INSCOM(7) = ZEROES )
                 MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE.

      ***********************************************
       OTHER1-INSURANCE SECTION.
      ***********************************************

           IF LN-INSPREM(4) = 0
              MOVE "O1 PREMIUM DOES NOT MATCH INS TYPE FLAG" TO MESS
              PERFORM ERROR-MESSAGE
           ELSE
              IF ( SP-INSCOM(8) = ZEROES )
                 MOVE "OTHER 1 INSURANCE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE.


      ***********************************************
       VERIFY-PASSWORD SECTION.
      ***********************************************
           MOVE "SFVERI"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPSPEDL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".


      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
       GET-BR.
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-BAD
              MOVE "INVALID LAW RECORD" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.
       VERIFY-MESSAGE.
           MOVE "CONTRACT VERIFICATION IN PROGRESS......." TO MESS1.
           PERFORM SEND-LEGEND.
       ERROR-BW-MESSAGE.
           MOVE SPACES TO MESS1.
           PERFORM SEND-LEGEND.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "Y" TO BW-ERRORS.
       ERROR-MESSAGE.
           MOVE " " TO MESS1.
           PERFORM SEND-LEGEND.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "Y" TO ERRORS-EXIST.
           IF EXTEND-VERIFY = "Y"
              PERFORM VERIFY-MESSAGE.
       SEND-LEGEND.
           MOVE MESS1 TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CONDITIONAL-SEND-MESS.
      *    IF LPLOAN-CALLED-FROM NOT = "LNCREA"
              PERFORM SEND-MESS.
      *    ELSE
      *       IF LPLOAN-RETURN-ERR-BUF = SPACES
      *          MOVE MESS TO LPLOAN-RETURN-ERR-BUF.

      *************************************
       IO-ROUTINE SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPWKR.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
