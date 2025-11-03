       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNVERI.
       DATE-WRITTEN.  082996.
      ******************************************************************
      *
      * IDENTIFICATION:  CL/LNVERI.C
      *
      * DESCRIPTION   :  DIRECT LOANS (NON-SALES FINANCE)
      *                  VERIFY CONTRACT
      *                   CALLED FROM LONPC0, XONPC0, ZONPC0
      * !!!! HEY   !!!!  THIS PROGRAM HAS BEEN ALTERED SO IT CAN BE CALLED
      * !!!!!!!!!!!!!!!  FROM WORLD LIVE CHECK PROGRAM (LNCRE2) TO CREATE LOANS
      * !!!!!!!!!!!!!!!  I HAD TO CHANGE
      * !!!!!!!!!!!!!!!  ALL OCCURANCES OF PERFORM SEND-MESS TO
      * !!!!!!!!!!!!!!!  PERFORM CONDITIONAL-SEND-MESS SO ERRORS
      * !!!!!!!!!!!!!!!  MIGHT NOT GO TO THE SCREEN, SO PLEASE DON'T
      * !!!!!!!!!!!!!!!  PERFORM SEND-MESS, PERFORM CONDITIONAL-SEND-MESS

      *
      *  ---------------------------------------------------------
      *  |                    N  O  T  E:                        |
      *  |                                                       |
      *  |       THIS PROGRAM USES A48 PAYMENT SCHEDULE LOGIC    |
      *  |        ....UNLESS CALLED FROM ZONPC0.C, BALLOONS      |
      *  ---------------------------------------------------------
      *
      *-----------------------------------------------------------------
      * REV:
      * JTG 950331 REMOVED REF: NO ODD PAYMENTS
      * JFF 960311 ADDED TEST FOR VALID PURPOSE CODE.
      * MJD 960816 ADDED TEST FOR LN-SSNO(2) THRU LN-SSNO(4).
      * MJD 960829 COPIED FROM LP/LONPC1.C AND MADE INTO A CALLED
      *            PROGRAM. ADDED LOGIC TO TEST INSURANCE CODES.
      * JTG 961112 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * JTG 971023 CHANGED TO USE A60/A10 PAYMENT SCHEDULE LOGIC FOR
      *            CALLS FROM ZONPC0.C
      * BAH 980126 IF INSURED IS "S", TEST SECOND MAKER'S AGE EDITS,
      *            PR#641
      *  CS 980427 ADD TEST OF NEW RENEWAL CODES "RB" AND "RO"
      *  MG 990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE
      *            WRITING NO. OF MONTHS INSURANCE NOT IN VIOLATION,
      *            WISCONSIN #1032
      *
      * KEC 2000.0524 {PR#00227} WORLD ACCEPTANCE [JUDY BRANIFF]
      *               ADDED SECTION 'VERIFY-PB-DELINQUENT'.
      *               THIS IS FOR THE PURPOSE OF HAVING A VERIFICATION
      *               OF DELINQUENT PAYOFF (PBD) FOR WORLD ACCEPTANCE.
      *
      * KEC 2000.0718 {PR#00227} WORLD ACCEPTANCE [JUDY BRANIFF]
      *               ADDED THE FOLLOWING LINE FOR WE WERE HAVING A
      *               PROBLEM WITH 'GET-SRCD-SUB'.  BARB BELIEVES THAT
      *               LN-PRPDUE-DAYS WILL BE RESET IN THE UPDATE.
      *
      *                  MOVE PD-PRPDUE-DAYS TO LN-PRPDUE-DAYS.
      *
      *               ALSO REVISED THE '{ VERIFY: PRIOR LOAN }' FOR IT
      *               WAS NOT READING THE 'PD' FILE.
      * BLV 010810 ADDED CONDITIONAL-SEND-MESS FOR CHANGE TO LPSPEDP,
      *            MERC PR# 253
      * BAH 011126 ADDED TEST FOR SP-CL-AH-MAXIMUMS, THEY WERE CHANGING THE
      *            LOAN TERM AFTER THE INSURANCE WAS CALCULATED !!
      *            REGENCY PR# 1643
      * BAH 011203 ADDED SP-INSEXCEPT 14, REGENCY  (SP-ORGTERM-MAXIMUM) PR#1657
      * JTG 020712 ADDED RP-RATE-ALLOW-OVERRIDE                  LENDMARK #1841
      * BAH 070816 ADDED VALIDATION OF SPECIFIC BORROWER FIELDS AS A MUST
      *            FILL IN FOR GP-I-AM-REGENCY ONLY, REGWAN PR# 3031
      * BAH 070822 ADDED VALIDATION OF SPECIFIC AUTO COLLATERAL FIELDS IF
      *            COLLCD "AU" OR "AP" FOR WORLD ONLY, WORLD PR# 550
      * BAH 071016 ADDED VERIFY OF SRCD PER SP-RENEWABLE-DAYS, REGMGM PR#3082
      * MJD 081024 ADDED LOGIC TO ALLOW MFEES ON IB ACCOUNTS.  WORLD PR#590
      *  CS 090911 ADDED VERIFY FOR REGENCY OF PURPOSE CODE. IF REGENCY & CLASS
      *            6, 1ST CHARACTER OF PURPOSE MUST BE 'T' REGENCY PR#3450
      * BAH 110118 ADDED CL/RENEWA TO RESTRICT WORLD "IL" TO NO NEW LOAN ON
      *            SAME DAY AS A PAYOFF, PR# 734
      * BLM 110211 SEND ERROR IF RENEWAL AND LN-SSNO(1) DOESN'T MATCH
      *            PRIOR LOAN SSNO, PL # 725
      * BAH 110318 CHANGED ABOVE CHANGE TO MAKE SURE NEW SSNO MATCHES AT LEAST
      *            ONE SSNO ON THE PRIOR LOAN, PL# 725
      * BAH 130214 PASSWORD COLLCD "US" OR "UN" WITH PP OR O2 INSPREM
      *                                                     REGMGM, PR# 4210
      * BAH 130509 MUST HAVE INSCOVR FOR ANY INSURANCE WRITTEN, WORLD PR# 880
      * BAH 130529 PASSWORD WRITING PP AND O2 INSURANCE, REGMGM PR# 4282
      * BAH 150504 ADD CLASS 5 TO PURPOSE CODE PASSWORD FOR REGENCY PR#4748
      * MJD 160608 REPLACED SEND-MESS WITH CONDITIONAL-SEND-MESS. WORLD PR#1133
      *            ADDED GET-BR AND SETUP-LOCAL-PATHS.
      * MJD 160726 CHANGED TO NOT PERFORM SEND-LEGEND IF CALLED FROM LNCRE2.
      *            WRFORM WITHOUT LIST ONLY WAS WAS DISPLAYING GARBAGE IN THE
      *            SYSDATE/CONAME FIELD                            61000 PR#1148
      * BAH 170404 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20210716 STATE OF ALABAMA REQUIRES BORROWER TO BE 19 TO MAKE A
      *              LOAN. CHECK BORROWER AGE IF LN-ORGST "AL"  #1505
      * BAH 20210914 ALL BORROWERS MUST HAVE AN AGE OR REJECT LOAN, #1513
      * BAH 20220209 VERIFY-TERMCEIL WAS BEING PERFORMED WITH A COMAKER
      *              IN THE BW-REC DUE TO ALABAMA AGE TESTING. NEEDS TO
      *              HAVE MAKER IN THERE FOR INSURANCE AGE EDITS. ADDED
      *              A READ OF MAKER IF COMAKERS EXIST. #1541
      * BAH 20221116 ADDED VALIDATION OF AUTO COLLATERAL FIELDS IF "WA"
      *              OR "WB" #1577A
      * BAH 20221205 VALIDATE UMC SALES TAX FOR KENTUCKY. ROAD GARD #1582
      * BAH 20240214 ADDED VALIDATION OF SPECIFIC AUTO COLLATERAL FIELDS IF
      *              (AU,AP,WA,WB) YOU CAN NOT HAVE ANY VEHICLE INFO #1643
      * BAH 20241111 AD&D TERM VERIFY, CALL CL/FEEADD #1685 S35Q-144
      * BAH 20250102 ADDED COLLCD "AS" TO SAME TESTS AS "AP" #1695 S35Q-154
      * BAH 20250106 UMC TERM VERIFY, CAN ONLY SELL UP TO 12 MONTHS AND
      *              MUST CHECK PRIOR LOANS #1687 S35Q-146
      *************************************************************************
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
           "@(#)CL/LNVERI S35 02/24/25-15:31:40 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  WK-KEY                      PIC 9(02)     VALUE ZEROES.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  AMT-HOLDER                  PIC S9(9)V99  COMP-3.
       01  MESS1                       PIC  X(70)    VALUE SPACES.
       01  MSEL-LIST                   PIC  X(05)    VALUE "MSEL.".

       01  OUR-CHGS                    PIC S9(05)V99 COMP-3  VALUE 0.
       01  SUB                         PIC  9(06)    COMP-3.
       01  SUB1                        PIC  9(06)    COMP.
       01  FEE-SUB                     PIC  9(06)    COMP-3.
       01  CAN-SELL-ADD                PIC  X        VALUE " ".
       01  STATE-UMC-MAX               PIC 9(3).
       01  UMC-ACTIVE-LEFT             PIC 9(3).
       01  UMC-REMAINDER               PIC  X        VALUE " ".
       01  CAN-SELL-UMC                PIC  X        VALUE " ".
       01  UMC-TERM               PIC 9(3)V99.
       01  UMC-TERMX REDEFINES UMC-TERM.
           03  UMC-WHOLE          PIC 9(3).
           03  UMC-DEC1           PIC 9.
           03  UMC-DEC2           PIC 9.
       01  NEW-UMC-TERM           PIC 9(3).
       01  DISP-UMC-TERM          PIC Z(3).
       01  UPDATE-APPROVAL             PIC  X(01)    VALUE "Y".
       01  BW-ERRORS                   PIC  X        VALUE "N".

       01  ROAD-GARD                   PIC S9(7)V99 COMP-3 VALUE 0.
       01  ROAD-GARD-TAX               PIC S9(7)V99 COMP-3 VALUE 0.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/XONPC_DEF.CPY".
       COPY "LIBLP/LONPC_DEF.CPY".
       COPY "LIBLP/ZONPC_DEF.CPY".
       COPY "LIBLP/XONPC_WKS.CPY".
       COPY "LIBLP/LONPC_WKS.CPY".
       COPY "LIBLP/ZONPC_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/FEEADDW.CPY".
       COPY "LIBCL/RENEWAW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".

       COPY "LIBGB/PASSWDW.CPY".
      *77  BW-SEQ             PIC 99 VALUE 00.
      *77  PURCD-SEQ          PIC 99 VALUE 01.
      *77  STOPLN-SEQ         PIC 99 VALUE 02.
      *77  US-UN-SEQ          PIC 99 VALUE 03.
       77  PP-O2-SEQ          PIC 99 VALUE 04.

       COPY "LIBGB/GETFMW.CPY".
       01   ERRORS-EXIST               PIC X(01).
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/XONPC_SCN.CPY".
       COPY "LIBLP/LONPC_SCN.CPY".
       COPY "LIBLP/ZONPC_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME ERRORS-EXIST PROG-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
      D    STOP MESS.

           IF LPLOAN-CALLED-FROM = "XONPC0"
              MOVE "XONPC" TO VDU-FORM-NAME
           ELSE
           IF LPLOAN-CALLED-FROM = "LONPC0"
              MOVE "LONPC" TO VDU-FORM-NAME
           ELSE
           IF LPLOAN-CALLED-FROM = "ZONPC0"
              MOVE "ZONPC" TO VDU-FORM-NAME
           ELSE
           IF LPLOAN-CALLED-FROM = "LNCRE2" OR "API   "
              MOVE "LNCRE2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE USER-PID TO TEMP-PID.

           IF LPLOAN-CALLED-FROM = "LNCRE2" OR "API   "
              PERFORM GET-BR
              PERFORM SETUP-LOCAL-PATHS.

           MOVE "N" TO ERRORS-EXIST.
           PERFORM VERIFY-MESSAGE.

           MOVE 2 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE WK-SAVE-REC TO LN-REC.

      * GET COLLATERAL WORK RECORD TO VERIFY ENTRY FOR WORLD
           MOVE 6 TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF ( IO-BAD )
              MOVE SPACES TO WK-LT-RECORD.

           PERFORM CLOSE-WK-FILE.

           PERFORM GET-BR.

           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              MOVE "INVALID STATE PARAMETER" TO MESS
              PERFORM ERROR-MESSAGE
              GO TO CONTINUE-VERIFICATION.

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

           MOVE 1 TO SUB.

           MOVE "PU"     TO CD-TYPE.
           MOVE LN-PURCD TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              MOVE "INVALID PURPOSE CODE" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "Y" TO ERRORS-EXIST.

      *
      *    IF (GP-I-AM-REGENCY)
      *        MOVE LN-SSNO(1) TO STOPLN-SSNO(1)
      *        MOVE LN-SSNO(2) TO STOPLN-SSNO(2)
      *        MOVE LN-SSNO(3) TO STOPLN-SSNO(3)
      *        MOVE LN-SSNO(4) TO STOPLN-SSNO(4)
      *        MOVE LN-CLASS TO STOPLN-CLASS
      *        MOVE "CL/STOPLN" TO FORM-NAM
      *        CALL FORM-PROGX USING FORM-PATH STOPLN-WORKERS
      *        CANCEL FORM-PROGX
      *        IF STOPLN-STOP-LOAN = "Y"
      *           MOVE STOPLN-SEQ TO PASSWDW-SEQ
      *           MOVE "TO OVERRIDE CLASS 14 EDIT" TO PASSWDW-PROMPT2
      *           PERFORM VERIFY-PASSWORD
      *           IF PASSWDW-STAT NOT = "OK"
      *              MOVE "Y" TO ERRORS-EXIST.

           IF SP-RENEWABLE-DAYS = 9999
              IF LN-SRCD = "PB" OR "RN"
                 MOVE "'PB' OR 'RN' SOURCE CODE NOT ALLOWED" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "Y" TO ERRORS-EXIST.

           IF SP-RENEWAL-EDIT = "A"
              IF LN-SRCD = "FB" OR "NB"
                 MOVE LN-SSNO(1) TO RENEWA-SSNO(1)
                 MOVE LN-SSNO(2) TO RENEWA-SSNO(2)
                 MOVE LN-SSNO(3) TO RENEWA-SSNO(3)
                 MOVE LN-SSNO(4) TO RENEWA-SSNO(4)
                 MOVE LN-LOANDATE TO RENEWA-LOANDATE
                 MOVE "CL/RENEWA" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH RENEWA-WORKERS
                 CANCEL FORM-PROGX
      * FOUND A LOAN WITH A PAYOFF DATE ON SAME DAY AS THIS LOANDATE
                 IF RENEWA-STOP-LOAN = "Y"
                    MOVE "LOAN NOT ALLOWED SAME DAY AS PAYOFF" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "Y" TO ERRORS-EXIST.

      *    IF GP-I-AM-REGMGM
      *       IF LN-COLLCD = "US" OR "UN"
      *          IF LN-INSPREM(3) NOT = 0 OR LN-INSPREM(5) NOT = 0
      *              MOVE "  " TO PASSWDW-STAT
      *              MOVE US-UN-SEQ TO PASSWDW-SEQ
      *              MOVE "TO USE 'US' OR 'UN' COLLCD" TO
      *                                               PASSWDW-PROMPT2
      *              PERFORM VERIFY-PASSWORD
      *              IF PASSWDW-STAT NOT = "OK"
      *                 MOVE "Y" TO ERRORS-EXIST.

      * ASK FOR PASSWORD IF THEY TRY TO WRITE BOTH TYPES OF INSURANCE
           IF LN-INSPREM(3) NOT = 0 AND LN-INSPREM(5) NOT = 0
               MOVE "  " TO PASSWDW-STAT
               MOVE PP-O2-SEQ TO PASSWDW-SEQ
               MOVE "TO WRITE PP AND O2 INSURANCE" TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  MOVE "Y" TO ERRORS-EXIST.

       VERIFY-PAYMENTS.

           COMPUTE N-WK1 = LN-REGPYAMT + LN-EXTCHG +
                           (LN-ORGTERM - 1) / 100.
           IF ( LN-1STPYAMT > N-WK1 )
              MOVE "INVALID ODD PAYMENT AMOUNT" TO MESS
              PERFORM ERROR-MESSAGE.

       VERIFY-LOAN-AMOUNT.

           IF (LN-ORGTERM = 1 AND LN-NOREGPY = 1)
                    OR (LN-ORGTERM > 1 AND LN-LASTPYAMT = 0)
      *  IF NOT COMING FROM ZONPC0.C, MUST USE A48 PAY SCHEDULE:
              MOVE LN-ORGTERM TO LN-NOREGPY
              IF LN-1STPYAMT NOT = 0
                 SUBTRACT 1 FROM LN-NOREGPY
              END-IF
              COMPUTE N-WK1 = LN-1STPYAMT + (LN-NOREGPY * LN-REGPYAMT)
           ELSE
              COMPUTE N-WK1 =
                     LN-1STPYAMT + (LN-NOREGPY * LN-REGPYAMT)
                                                       + LN-LASTPYAMT.

           IF ( LN-LOANTYPE = "I" )
              IF GP-ALLOW-MF-ON-IB = "Y"
                 COMPUTE N-WK1 = N-WK1 - LN-EXTCHG - LN-INTCHG
              ELSE
                 COMPUTE N-WK1 = N-WK1 - LN-EXTCHG - LN-INTCHG
                                 - (LN-ORGTERM * LN-MAINTFEE).

           IF ( N-WK1 NOT = LN-LNAMT )
              MOVE "PAYMENTS DO NOT EQUAL LOAN AMOUNT" TO MESS
              PERFORM ERROR-MESSAGE.

      * VERIFY YOU CAN SELL AD&D ON NEW LOAN.
      * FIRST CHECK IF THEY TRY TO SELL AD&D ON NEW LOAN
      * IF ANY LOAN IS WITHIN 12 MONTHS, NEW LOAN CAN NOT HAVE AD&D

           MOVE " " TO CAN-SELL-ADD.

           PERFORM VARYING FEE-SUB FROM 1 BY 1 UNTIL FEE-SUB > 10
              IF LN-FEECODE(FEE-SUB) = BR-ADD-INS-FEECODE AND
                     BR-ADD-INS-FEECODE NOT = SPACES
                 PERFORM CHECK-LOANS-FOR-ADD
              END-IF
           END-PERFORM.

           IF CAN-SELL-ADD = "N"
              MOVE "AD&&D TERM NOT EXPIRED ON PRIOR, CAN'T SELL" TO MESS
              PERFORM ERROR-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    N-WK1 AT THIS POINT CONTAINS LOAN AMOUNT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO SUB.

           MOVE 0 TO ROAD-GARD ROAD-GARD-TAX.

       NEXT-FEE.

      * VALIDATE THAT KY UNITED MOTOR CLUB HAS SALES TAX FEECODE ENTERED!
      * AND THAT IT IS 6% OF PREMIUM
           IF (BR-FORCE-VALID-CITY-COUNTY = "Y" AND LN-ORGST = "KY")
              IF LN-FEECODE(SUB) = "TM"
                 ADD LN-FEEAMT(SUB) TO ROAD-GARD-TAX
              ELSE
              IF LN-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE OR
                    LN-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE
                 ADD LN-FEEAMT(SUB) TO ROAD-GARD.

           SUBTRACT LN-FEEAMT(SUB) FROM N-WK1.
           ADD LN-FEEAMT(SUB) TO OUR-CHGS.

           IF ( SUB < 10 )
              ADD 1 TO SUB
              GO TO NEXT-FEE.

      * VALIDATE THAT PREIUM EXIST IF THEY ENTERED TAX AMOUNT
           IF (BR-FORCE-VALID-CITY-COUNTY = "Y" AND LN-ORGST = "KY")
              IF ROAD-GARD = 0
                 IF ROAD-GARD-TAX NOT = 0
                    MOVE "MUST ENTER UMC PREMIUM FEECODE" TO MESS
                    PERFORM ERROR-MESSAGE.

      * VALIDATE THAT KY UNITED MOTOR CLUB HAS SALES TAX FEECODE ENTERED!
      * AND THAT IT IS 6% OF PREMIUM
           IF (BR-FORCE-VALID-CITY-COUNTY = "Y" AND LN-ORGST = "KY")
              IF ROAD-GARD NOT = 0
                 IF ROAD-GARD-TAX = 0
                    MOVE "MUST ENTER UMC SALES TAX 'TM' FEECODE" TO MESS
                    PERFORM ERROR-MESSAGE
                 ELSE
                    IF (ROAD-GARD * .06) NOT = ROAD-GARD-TAX
                       MOVE "UMC TAX MUST BE 6%" TO MESS
                       PERFORM ERROR-MESSAGE.

           MOVE 1 TO SUB.

       NEXT-INS.

           IF ( LN-INSOURS(SUB) NOT = "M" )
              ADD LN-INSPREM(SUB) TO OUR-CHGS
              SUBTRACT LN-INSPREM(SUB) FROM N-WK1.

           IF ( SUB < 8 )
              ADD 1 TO SUB
              GO TO NEXT-INS.

           SUBTRACT LN-SERCHG FROM N-WK1.

           IF ( N-WK1 < ZEROES )
              MOVE "AMOUNT FINANCED MAY NOT BE < 0" TO MESS
              PERFORM ERROR-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - -{ VERIFY: LN-FINCHG  }
           IF ( LN-FINCHG < SP-MINSF-CHG )
              MOVE "FINANCE CHARGE BELOW MINIMUM" TO MESS
              PERFORM ERROR-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VERIFY: PROCEEDS }
           COMPUTE N-WK1 =
                    LN-1STPYAMT + LN-NOREGPY * LN-REGPYAMT
                       + LN-LASTPYAMT - LN-EXTCHG
                        - (LN-MAINTFEE * LN-ORGTERM)
                         - LN-SERCHG - OUR-CHGS - LN-INTCHG.

           IF ( N-WK1 < ZEROES )
              MOVE "PROCEEDS MAY NOT BE < 0" TO MESS
              PERFORM ERROR-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - -{ VERIFY: PRIOR LOAN }
           MOVE SPACES TO PD-PRPOCD.
           MOVE ZEROES TO PD-PRBAL        PD-PRINTDUE
                          PD-PRFINCHGRB   PD-PRINSRB
                          PD-PRPODATE     PD-PRCONPROFILE
                          PD-PRPDUE-DAYS.

           IF ( LN-SRCD NOT = "PB" ) AND
              ( LN-SRCD NOT = "RB" ) AND
              ( LN-SRCD NOT = "RN" ) AND
              ( LN-SRCD NOT = "RO" ) AND
              ( LN-SRCD NOT = "SC" )
              GO TO VERIFY-INSURANCE.

           IF ( LN-PRLNNO = ZEROES )
              MOVE "PRIOR LOAN NO. REQUIRED FOR LOAN RENEWALS" TO MESS
              PERFORM ERROR-MESSAGE
              GO TO VERIFY-INSURANCE.

           IF ( LN-POCD-INRESCISSION )
              GO TO VERIFY-INSURANCE.

      *- - - - - - - - - - - - - - - - -{ GET 'PD' RECORD (PRIOR LOAN) }
           MOVE "R"       TO PD-ID.
           MOVE BRANCH    TO PD-BRANCH.
           MOVE LN-PRLNNO TO PD-SOC-SEC.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.

           IF ( IO-BAD )
              MOVE " PRIOR LOAN NO. IS NOT ON FILE" TO MESS
              PERFORM ERROR-MESSAGE
              GO TO VERIFY-INSURANCE.

      * NEW SSNO MUST MATCH AT LEAST 1 OF THE PRIOR LOAN SSNO'S
           IF LN-SSNO(1) NOT = PD-SSNO(1) AND
                         NOT = PD-SSNO(2) AND
                         NOT = PD-SSNO(3) AND
                         NOT = PD-SSNO(4)
              MOVE " PRIOR LOAN SSNO DOES NOT MATCH LOAN SSNO" TO MESS
              PERFORM ERROR-MESSAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    N-WK1 HAS PROCEEDS IN IT AT THIS POINT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SUBTRACT PD-PRBAL PD-PRINTDUE FROM N-WK1.
           ADD PD-PRFINCHGRB PD-PRINSRB TO N-WK1.

           IF ( N-WK1 < ZEROES )
              MOVE "FINANCED LESS PRIOR LOAN MAY NOT BE < 0" TO MESS
              PERFORM ERROR-MESSAGE.


       VERIFY-INSURANCE.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF LN-INSPREM(SUB1) NOT = 0
                 IF LN-INSCOVR(SUB1) = 0
                    MOVE "MISSING INSURANCE COVERAGE" TO MESS
                    PERFORM ERROR-MESSAGE
                 END-IF
              END-IF
           END-PERFORM.

           PERFORM VALID-INS-TYPES-ELIG.

           IF ( LN-INSURED NOT = "N" )
              GO TO VERIFY-AGAINST-SPEDIT.

           MOVE ZEROES TO IO-FG.

           IF ( LN-INSCOMP(1) NOT = ZEROES ) OR
              ( LN-INSPREM(1) NOT = ZEROES )
              MOVE 9 TO IO-FG.

           IF ( LN-INSCOMP(2) NOT = ZEROES ) OR
              ( LN-INSPREM(2) NOT = ZEROES )
              MOVE 9 TO IO-FG.

           IF ( LN-INSCOMP(3) NOT = ZEROES ) OR
              ( LN-INSPREM(3) NOT = ZEROES )
              MOVE 9 TO IO-FG.

           IF ( LN-INSCOMP(4) NOT = ZEROES ) OR
              ( LN-INSPREM(4) NOT = ZEROES )
              MOVE 9 TO IO-FG.

           IF ( IO-BAD )
              MOVE "INS CO/PREMIUM MUST BE 0 FOR INS CD 'N'" TO MESS
              PERFORM ERROR-MESSAGE.

      *-----------------------------------------------------------------
       VERIFY-AGAINST-SPEDIT.

           MOVE PD-PRBAL        TO LN-PRBAL.
           MOVE PD-PRINTDUE     TO LN-PRINTDUE.
           MOVE PD-PRFINCHGRB   TO LN-PRFINCHGRB.
           MOVE PD-PRINSRB      TO LN-PRINSRB.
           MOVE PD-PRPOCD       TO LN-PRPOCD.
           MOVE PD-PRPODATE     TO LN-PRPODATE.
           MOVE PD-PRCONPROFILE TO LN-PRCONPROFILE.

      *- - - - - - - - - - - - - - - - - - - - - -{ VERIFY: LN-COLLCD  }
           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              MOVE "COLLATERAL CODE NOT FOUND" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "Y" TO ERRORS-EXIST.

           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.

      * TEST MAKERS SOCIAL SECURITY NUMBER
      
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF ( IO-BAD )
              MOVE "MAKERS BORROWER RECORD NOT FOUND" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "Y"    TO ERRORS-EXIST
              MOVE 0      TO BW-BDATE BW-REFDATE.

           MOVE BW-BDATE   TO VERIFY-AGE.
           MOVE BW-REFDATE TO VERIFY-REFDATE.

      * STATE OF ALABAMA HAS MINIMUM BORROWER AGE OF 19 FOR LOANS
      * REJECT LOAN IF NO BIRTHDATE
           PERFORM CHECK-BORROWER-AGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TEST BORROWERS SOCIAL SECURITY NUMBERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB1 FROM 2 BY 1 UNTIL SUB1 > 4
              IF ( LN-SSNO(SUB1) NOT = ZEROES )
                 MOVE LN-SSNO(SUB1) TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 IF ( IO-BAD )
                    MOVE "JOINT/ENDORSER BORROWER RECORD NOT FOUND"
                                                           TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "Y" TO ERRORS-EXIST
                 ELSE
      * STATE OF ALABAMA HAS MINIMUM BORROWER AGE OF 19 FOR LOANS
      * REJECT LOAN IF NO BIRTHDATE
                    PERFORM CHECK-BORROWER-AGE
                    IF ( SUB1 = 2 ) AND ( LN-INSURED = "S" )
                       MOVE BW-BDATE   TO VERIFY-AGE
                       MOVE BW-REFDATE TO VERIFY-REFDATE
                    END-IF
                 END-IF
              END-IF

           END-PERFORM.

      * IF COMAKERS, NEED TO GET MAKER BACK IN THE BW-REC FOR TERMCEIL #1541
           IF LN-SSNO(2) NOT = 0
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE
              MOVE BW-BDATE   TO VERIFY-AGE
              MOVE BW-REFDATE TO VERIFY-REFDATE.

      * CHECK FOR UMC AND IF THEY CAN SELL MORE
           PERFORM VARYING FEE-SUB FROM 1 BY 1 UNTIL FEE-SUB > 10
              IF (LN-FEECODE(FEE-SUB) = BR-ROAD-G-SGL-FEECODE AND
                  BR-ROAD-G-SGL-FEECODE NOT = SPACES) OR
                     (LN-FEECODE(FEE-SUB) = BR-ROAD-G-FAM-FEECODE AND
                       BR-ROAD-G-FAM-FEECODE NOT = SPACES)
                  PERFORM CHECK-UMC-EXPIRATION
                  IF CAN-SELL-UMC = "N"
                     IF UMC-REMAINDER = "Y"
                       MOVE "INVALID UMC PREMIUM AMOUNT" TO MESS
                       PERFORM ERROR-MESSAGE
                     ELSE
                       MOVE NEW-UMC-TERM TO DISP-UMC-TERM
                       MOVE "CAN ONLY SELL &&& MONTHS OF UMC" TO MESS
                      INSPECT MESS REPLACING ALL "&&&" BY DISP-UMC-TERM
                       PERFORM ERROR-MESSAGE
                  END-IF
              END-IF
           END-PERFORM.

           PERFORM CLOSE-BW1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LN-APRATE TO VERIFY-TERMCEIL-APR.
           MOVE 0         TO VERIFY-TERMCEIL-EFF.
           MOVE "N"       TO VERIFY-TERMCEIL-EFF-FG.
           MOVE " "       TO VERIFY-REJECT-TERM-FG.
           PERFORM VERIFY-TERMCEIL.
           IF VALID-LOAN = "N"
              MOVE "Y" TO ERRORS-EXIST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VERIFY-PB-DELINQUENT.

           IF LN-COLLCD = "AU" OR "AP" OR "WA" OR "WB" OR "AS"
              IF WK-MAKE-1 = SPACES OR WK-MODEL-1 = SPACES OR
                  WK-YEAR-1 = SPACES OR WK-VIN-1 = SPACES
             MOVE "COLLATERAL CODE REQUIRES AUTO COLLATERAL INFO"
                      TO MESS
             PERFORM ERROR-MESSAGE.

           IF NOT (LN-COLLCD = "AU" OR "AP" OR "WA" OR "WB" OR "AS")
              IF WK-MAKE-1 NOT = SPACES OR WK-MODEL-1 NOT = SPACES OR
                  WK-YEAR-1 NOT = SPACES OR WK-VIN-1 NOT = SPACES
              MOVE "COLLATERAL CODE REQUIRES NO AUTO COLLATERAL INFO"
                                                  TO MESS
              PERFORM ERROR-MESSAGE.

      *-----------------------------------------------------------------
       CONTINUE-VERIFICATION.

           MOVE MENU-PROG-FORM(3) TO NEW-PROG-FORM.
           MOVE "P"               TO ERRCD.

           IF ( ERRORS-EXIST = "Y" )
              MOVE "CORRECT ENTRIES AND UPDATE !" TO MESS
              PERFORM ERROR-MESSAGE
              MOVE SPACES TO MESS
              PERFORM CALL-TMMSG
              MOVE "1" TO ROUTE-CD
              GO TO MAIN-PROGRAM-EXIT.

           IF ( UPDATE-APPROVAL = "N" )
              MOVE "Y" TO ERRORS-EXIST
              MOVE SPACES TO MESS
              PERFORM CALL-TMMSG
              MOVE "1" TO ROUTE-CD
              GO TO MAIN-PROGRAM-EXIT.

           MOVE "2" TO NEW-PROG-SUB.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      *****************************************************************
      * EXAMPLE:
      * 11/12/24 NB 12MO. LOAN, MDTE 11/12/25, MAX UMC EXPIRE 11/12/26
      *          SELL 3 MO. UMC, BW-UMC-EXPIRE 2/12/25
      * 01/15/25 PB 9MO. LOAN, MDTE 10/15/25, MAX UMC EXPIRE 10/15/26
      *          SELL 6 MO. UMC, BW-UMC-EXPIRE 8/12/25
      * ALTERNATIVE;
      * 01/15/25 9 MO. LOAN, MDTE 10/15/25, MAX UMC EXPIRE 10/15/26
      *          SELL 24 MO. UMC, BW-UMC-EXPIRE 2/12/27
      *       *  2/12/27 > 10/15/26, CANT SELL 24, ERROR TO USE THAT THEY
      *          CAN ONLY SELL 20 MONTHS.
      * VOID;
      * 03/20/26 PB 9MO. LOAN, MDTE 12/20/26, MAX UMC EXPIRE 12/20/27
      *          CURRENT BW-UMC-EXPIRE STILL AT 2/12/25, PREVIOUS POLICY
      *                                 IS EXPIRED
      *          SELL 8 MONTHS UMC, BW-UMC-EXPIRED 11/20/26 (BASED OFF LNDATE)
      *       * WHEN WE VOID THE LOAN, WE CANT SIMPLY SUBTRACT THE 8 MONTH
      *         UMC TERM FROM BW-UMC-EXPIRE AS YOU THEN GET 3/20/26 WHICH
      *         IS NOT WHAT THE PREVIOUS UMC EXPIRE WAS, SO WE ARE STORING
      *         BOTH PRIOR AND CURRENT UMC-EXPIRE FOR THIS SCENARIO
      *          (LOGIC IN LONPN2)
      *         
      ****************************************
       CHECK-UMC-EXPIRATION SECTION.
      ****************************************
           MOVE " " TO CAN-SELL-UMC UMC-REMAINDER.

      * WHAT THEY CAN SELL, IF THEY HAVE GONE OVER
           MOVE 0   TO NEW-UMC-TERM.
              
      * HOW MANY MONTHS THEY ARE TRYING TO SELL ON THIS LOAN

      * SINGLE
           IF LN-FEECODE(FEE-SUB) = BR-ROAD-G-SGL-FEECODE
              COMPUTE UMC-TERM = LN-FEEAMT(FEE-SUB) / 8
           ELSE
      * FAMILY
              IF LN-FEECODE(FEE-SUB) = BR-ROAD-G-FAM-FEECODE
                 COMPUTE UMC-TERM = LN-FEEAMT(FEE-SUB) / 10.

      * DO NOT ALLOW ANY EVEN DIVISIBLE PREMIUMS AS FORTEGRA THEN HAS TO
      * REFUND THEM
           IF NOT ( UMC-DEC1 = 0 AND UMC-DEC2 = 0 )
              MOVE "Y" TO UMC-REMAINDER
              MOVE "N" TO CAN-SELL-UMC
              GO TO EXIT-CHECK-UMC-EXPIRATION.

      * NOT TRYING TO SELL ANY, EXIT
           IF UMC-TERM = 0
              GO TO EXIT-CHECK-UMC-EXPIRATION.

      * IF NO PRIOR UMC EXPIRATION DATE, CHECK THEY ARE NOT SELLING MORE
      * THAN THE LOAN TERM (LOAN MATURITY) + 12 MONTHS
      * IF SO, LIMIT TO TERM + 12, IF NOT, THEY CAN SELL WHAT THEY ENTERED
           IF BW-UMC-EXPIRE-DATE = 0 
              IF UMC-TERM > (LN-ORGTERM + 12)
                 COMPUTE NEW-UMC-TERM = LN-ORGTERM + 12
                 MOVE "N" TO CAN-SELL-UMC
                 GO TO EXIT-CHECK-UMC-EXPIRATION
              ELSE
                 GO TO EXIT-CHECK-UMC-EXPIRATION.
            
      * IF LOANDATE IS PAST EXPIRE DATE, YOU CAN WRITE 12 MO. UMC, EXIT
           MOVE LN-LOANDATE        TO NUM-DATE.
           MOVE BW-UMC-EXPIRE-DATE TO SYS-DATE.
           PERFORM TIM365.
      * IF 9 MONTHS 28 DAYS ELAPSED, MAKE IT 10 MONTHS ELAPSED
           IF ELAPSED-REM > 0
              ADD 1 TO ELAPSED-MONTHS.
      * IF LOANDATE IS PAST BW EXPIRATION DATE, THIS IS LIKE A NEW LOAN,
      * THEY CAN SELL 12 PAST MATURITY DATE OR 12 PAST ORGTERM
      * THE STATE MAX WILL BE TESTED NOW BY THE CDFILE FEECODE MAX
           IF ELAPSED-MONTHS < 0
              IF UMC-TERM > (LN-ORGTERM + 12)
                 COMPUTE NEW-UMC-TERM = LN-ORGTERM + 12
                 MOVE "N" TO CAN-SELL-UMC
                 GO TO EXIT-CHECK-UMC-EXPIRATION
              ELSE
                 GO TO EXIT-CHECK-UMC-EXPIRATION.

      * FOR RENEWALS, WE NEED TO CHECK WHAT THEY CAN SELL BASED ON WHAT
      * IS STILL LEFT ACTIVE
           MOVE ELAPSED-MONTHS TO UMC-ACTIVE-LEFT.
           IF LN-ORGST = "AL" OR "MS"
              MOVE 36 TO STATE-UMC-MAX
           ELSE
           IF LN-ORGST = "KY"
              MOVE 60 TO STATE-UMC-MAX
           ELSE
              MOVE 48 TO STATE-UMC-MAX.

      * FIRST CHECK IF THEY HAVE EXCEEDED WHAT THE STATE ALLOWS
           IF UMC-TERM > (STATE-UMC-MAX - UMC-ACTIVE-LEFT) 
              COMPUTE NEW-UMC-TERM = STATE-UMC-MAX - UMC-ACTIVE-LEFT
              MOVE "N" TO CAN-SELL-UMC
              GO TO EXIT-CHECK-UMC-EXPIRATION.

      * ONLY HERE IF NEW LOAN DATE IS PRIOR TO BW-UMC-EXPIRE-DATE OR EQUAL

      * COMPUTE MATURITY-DATE + 12 MONTHS AND COMPARE TO BW-UMC-EXPIRE
           
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO NDTE-DATE.
           MOVE 12        TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

      * ORIG EXPIRATION
           MOVE BW-UMC-EXPIRE-DATE TO NUM-DATE.
      * MATURITY DATE + 12 MO.
           MOVE NDTE-DATE          TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-MONTHS > 0
              MOVE ELAPSED-MONTHS TO NEW-UMC-TERM.

           IF UMC-TERM > NEW-UMC-TERM 
              MOVE "N" TO CAN-SELL-UMC.

       EXIT-CHECK-UMC-EXPIRATION.
           EXIT.

      ****************************************
       CHECK-LOANS-FOR-ADD SECTION.
      ****************************************
      D    STOP MESS.
            MOVE BR-ADD-INS-FEECODE TO FEEADD-INS-FEECODE.
            MOVE TRANS-DATE         TO FEEADD-TRANS-DATE.
            MOVE LN-SSNO(1)         TO FEEADD-SSNO.
            MOVE SPACES             TO FEEADD-CAN-SELL.
            MOVE "CL/FEEADD" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH FEEADD-WORKERS.
            CANCEL FORM-PROGX.
            MOVE FEEADD-CAN-SELL TO CAN-SELL-ADD.

      *************************************
       CHECK-BORROWER-AGE SECTION.
      *************************************

      * WORLD #1506 STATE OF ALABAMA REQUIRES BORROWER TO BE 19
      * WORLD #1513 REJECT LOAN IF MISSING BIRTHDATE

      * BW HAS ALREADY BEEN READ 

      * ALL STATES MUST HAVE A BIRTHDATE
            IF BW-BDATE = 0
               MOVE "INVALID BORROWER BIRTHDATE" TO MESS
               PERFORM CONDITIONAL-SEND-MESS
               MOVE "Y" TO ERRORS-EXIST
               GO TO EXIT-CHECK-BORROWER-AGE.

      * ONLY "AL" HAS TO TEST FOR AGE 19
            IF LN-ORGST NOT = "AL"
               GO TO EXIT-CHECK-BORROWER-AGE.

            MOVE BW-BDATE TO NUM-DATE.
            PERFORM TSTDAT.
            IF NUM-DATE = 0
               MOVE "INVALID BORROWER BIRTHDATE" TO MESS
               PERFORM CONDITIONAL-SEND-MESS
               MOVE "Y" TO ERRORS-EXIST
            ELSE
              MOVE BW-BDATE      TO BAGE-DATE
              MOVE TRANS-DATE    TO BAGE-TODAY
              MOVE BW-REFDATE    TO BAGE-REFDATE
              PERFORM BAGE-CALCULATION
              IF BAGE-AGE < 19
                 MOVE "BORROWER IS UNDER THE AGE OF 19" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "Y" TO ERRORS-EXIST.

       EXIT-CHECK-BORROWER-AGE.
           EXIT.


      ****************************
       VERIFY-PASSWORD SECTION.
      ****************************
           MOVE "LNVERI" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ********************************
       VALID-INS-TYPES-ELIG SECTION.
      ********************************

           IF ( LN-INSURED = "S" OR "J" )
              IF ( LN-SSNO(2) = ZEROES )
                 MOVE "JOINT COVERAGE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE
                 GO TO VALID-INS-TYPES-ELIG-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK IF OTHER INSURANCES WERE PURCHASED:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-INSURED = "N" )
              IF ( LN-INSTYPES NOT = "N" )
                 GO TO VALID-INS-TYPES-ELIG-EXIT
              ELSE
                 GO TO VALID-INS-TYPES-ELIG-EXIT.

           IF ( LN-INSTYPES-LL  ) PERFORM LEVEL-INSURANCE.
           IF ( LN-INSTYPES-DL  ) PERFORM DECREASING-INSURANCE.
           IF ( LN-INSTYPES-AH  ) PERFORM ACCIDENT-INSURANCE.
           IF ( LN-INSTYPES-PP  ) PERFORM PROPERTY-INSURANCE.
           IF ( LN-INSTYPES-PP2 ) PERFORM PROPERTY-INSURANCE.
           IF ( LN-INSTYPES-O1  ) PERFORM O1-INSURANCE.

       VALID-INS-TYPES-ELIG-EXIT.
           EXIT.

      ***************************
       LEVEL-INSURANCE SECTION.
      ***************************

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

       LEVEL-INSURANCE-EXIT.
           EXIT.

      *******************************
       DECREASING-INSURANCE SECTION.
      *******************************

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

       DECREASING-INSURANCE-EXIT.
           EXIT.

      *****************************
       ACCIDENT-INSURANCE SECTION.
      *****************************

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

       ACCIDENT-INSURANCE-EXIT.
           EXIT.

      *****************************
       PROPERTY-INSURANCE SECTION.
      *****************************

           IF ( LN-INSTYPES-PP )
              IF ( SP-INSCOM(6) = ZEROES )
                 MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE.

           IF ( LN-INSTYPES-PP2 )
              IF ( SP-INSCOM(7) = ZEROES )
                 MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
                 PERFORM ERROR-MESSAGE.

       PROPERTY-INSURANCE-EXIT.
           EXIT.

      **********************
       O1-INSURANCE SECTION.
      **********************

           IF ( SP-INSCOM(8) = ZEROES )
              MOVE "OTHER 1 INSURANCE NOT ALLOWED" TO MESS
              PERFORM ERROR-MESSAGE.

       O1-INSURANCE-EXIT.
           EXIT.

      ******************************************************************
      *    IN : ~~
      *    OUT: UPDATE-APPROVAL (Y/N)
      *-----------------------------------------------------------------
      *    SRCD = 5 MEANS SOURCE CODE IS PBD
      ******************************************************************
       VERIFY-PB-DELINQUENT SECTION.

           MOVE "Y"            TO UPDATE-APPROVAL.
           MOVE PD-PRPDUE-DAYS TO LN-PRPDUE-DAYS.
           PERFORM GET-SRCD-SUB.

           IF ( ERRORS-EXIST = "Y" ) OR
              ( SRCD     NOT = 5   )
              GO TO VERIFY-PB-DELINQUENT-EXIT.

           MOVE SPACES TO MESS.

           STRING "THIS IS A DELINQUENT RENEWAL -"
                  "- DO YOU HAVE PROPER APPROVAL?"
                  DELIMITED BY SIZE INTO MESS.
           PERFORM CONDITIONAL-SEND-MESS.

           MOVE SPACES TO MESS.

           STRING "ENTER 'YES' TO CONTINUE WITH T"
                  "HE UPDATE IF YOU HAVE APPROVAL"
                  DELIMITED BY SIZE INTO MESS.
           PERFORM CALL-TMMSG.

       VERIFY-PB-DELINQUENT-YN.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF ( NOT ENTER-KEY ) GO TO VERIFY-PB-DELINQUENT-YN.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

           IF ( VDUBUF = "YES   " ) OR
              ( VDUBUF = "   YES" )
              GO TO VERIFY-PB-DELINQUENT-EXIT.

      *- - - - - - - - - - - - - - - -{ DOES NOT HAVE PROPER APPROVAL  }
           MOVE "N" TO UPDATE-APPROVAL.

       VERIFY-PB-DELINQUENT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPBAGE.CPY".
      *COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPSPEDL.CPY".
       COPY "LIBLP/LPSRCD.CPY".

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/XONPC_EVA.CPY".
              COPY "LIBLP/LONPC_EVA.CPY".
              COPY "LIBLP/ZONPC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       GET-BR SECTION.
      ********************************************
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      ********************************************
       SETUP-LOCAL-PATHS SECTION.
      ********************************************
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              PD-PATH-OVERRIDE.

      **********************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       ERROR-BW-MESSAGE.
           MOVE SPACES TO MESS1.
           PERFORM SEND-LEGEND.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "Y" TO BW-ERRORS.
       ERROR-MESSAGE.
           MOVE SPACES TO MESS1.
           PERFORM SEND-LEGEND.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "Y" TO ERRORS-EXIST.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           IF LPLOAN-CALLED-FROM NOT = "LNCRE2" AND NOT = "API   "
              MOVE MESS1 TO WR-BUF
              MOVE MSEL-LIST TO WR-LIST
              PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       VERIFY-MESSAGE.
           MOVE "CONTRACT VERIFICATION IN PROGRESS......." TO MESS1.
           PERFORM SEND-LEGEND.
      *-----------------------------------------------------------------
       CONDITIONAL-SEND-MESS.
           IF LPLOAN-CALLED-FROM NOT = "LNCRE2" AND NOT = "API   "
              PERFORM SEND-MESS
           ELSE
              IF LPLOAN-RETURN-ERR-BUF = SPACES
                 MOVE MESS TO LPLOAN-RETURN-ERR-BUF.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPWKR.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************
       END-PROGRAM SECTION.
      *******************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
