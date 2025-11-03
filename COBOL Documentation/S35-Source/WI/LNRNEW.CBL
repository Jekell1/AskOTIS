       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNRNEW.
       DATE-WRITTEN.    011792.
      **************************************************************************
      *                 (WI)
      *        WINDOW LOAN RENEWAL ON BALANCE
      *            DISPLAY AMOUNTS
      *            CALLED FROM CLMENU (F9), LONPFA AND WIPOFF
      * REV:
      *  JTG 070595 CORRECTED RE: SPR EFFECTIVE DATE TEST
      *  JTG 071395 ADDED LOGIC FOR SUB CLASSES
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 060497 ADDED LOGIC FOR 'O3' REBATE
      *  JTG 082197 CHANGED TO USE 'LPZLCL' TO CLEAR-CALC-AREA-ZL
      *  JTG 082297 MADE LN-CAN-LOAN-NO PIC XX
      *             CHANGED CL-STATE TO CL-GROUP-ID
      *  JTG 020498 SET CIP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR MONTH
      *             SEE CALCZL.C
      *   CS 042998 ADDED RENEWAL CODES "RB" (BANKRUPT) AND "RO" (OTHER)
      *  JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *  JTG 092598 CHANGED CALL FORM LONPZL TO CALCZL
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  JTG 991220 ADDED BR-ONESTEP-RN-FG, TO AKLLOW ONE STEP RENEWALS, WORLD
      *  MJD 000229 ADDED GP-ADV-TO-1ST-NO-CORR = "A". IF DATE OF THE LOAN
      *             IS ON THE 26 THRU 31 THEN ADVANCE THE FIRST PAYMENT
      *             DUE DATE TO THE FIRST OF THE SECOND MONTH FOLLOWING THE
      *             DATE OF THE LOAN. (REGMGM PR#1099)
      *  BAH 010518 ADDED SCFRMLA 42 (SUNBELT) LIKE 18 BUT 360 YEAR, PR# 1484
      *  JTG 020220 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      * BAH 020806 ADDED SP-MFFRMLA = 4, PR# 1867 REGWAN
      * JTG 020909 ADDED REWRITE ON PD1-FILE TO STOP IO-BAD '22' WRITES, FINMGM
      * BAH 030619 ADDED CIP-PRIOR-SUB-CLASS FOR SCFRMLA 46, LENDMARK PR# 2101
      * JTG 030929 MODIFIED SCFRMLA 13 TO ONLY TAKE THE DUAL LOAN
      *            ADJUSTMENT INTO ACCOUNT WHEN THE LOANS'S CLASSES
      *            ARE THE SAME                               LENDMARK#2149
      * JTG 031001 CREATED 01  SCHG-WORKERS-2 FOR PASS FOR SCFRMLA 13
      *                                                       LENDMARK #2149
      *  JTG 070326 WORLD SPR'S ARE FLAGGED WITH GP-ADV-TO-1ST-NO-CORR = "Y"
      *             WHICH WILL ADVANCE TO THE NEXT MONTH IF THE DEFAULT
      *             PAYMENT DUE DATE FALLS ON A NON EXISTING DAY
      *             THIS CHANGE OVERRIDES THIS OPTION FOR LOANS BOOKED WHEN
      *             GP-I-AM-WORLD AND PD-ORGST = "TN" AND PD-CLASS = 01 AND
      *             PD-ORGTERM = 12                                   WORLD 0532
      *   CS 081218 ADD TEST ON GP-INCL-OTH-IN-EARNED = "R" REGACC PR#210
      * MJD 111212 ADDED LOGIC FOR SP-ADV-TO-1ST-NO-CORR
      *            IF SET THIS WILL OVERIDE THE GP FLAG.             71226 #3879
      * MJD 130604 ADDED LOGIC FOR SP-SCFRMLA = 54.  FUNCTIONS THE SAME AS 12
      *            BUT INCLUDES AN EXTRA 4% ADDED IN AT THE END BASED
      *            ON TOTAL NOTE                                     WORLD #0874
      *  CS 141028 ADDED A LOAD OF PD WITHIN 'MORE INFO'
      *  CS 141030 REMOVED LOAD OF 'PD' WITHIN MORE INFO
      *  BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 171114 FIXED DISPLAY OF COP-PMT-DT(1ST PAYMENT DUE DATE)
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/LNRNEW.CBL A30 11/14/17-11:20:30 >".
      **********************************
      *       F I L E   I F N          *
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
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

       01  ERRCD           PIC XX   VALUE " ".
       01  SUB             PIC 9(6) COMP.
       01  COUNTER         PIC 9(6) COMP.

       01  LNRNEW-WINDOW-HANDLE  PIC X(10).

       01  THE-STATE       PIC XX.
       01  THE-CLASS       PIC 9(3) VALUE 0.
       01  THE-SUBCLASS    PIC 99   VALUE 0.
       01  THE-CAN         PIC XX   VALUE " ".
       01  ADV-TO-1ST-NO-CORR    PIC X       VALUE " ".

      *    ----------------------------------------------------
      *    SINCE QUOTERS DON'T HAVE BORROWERS THESE WORKERS WILL
      *    BE PASSED TO CALCZL/CALCZ3, '   ' == NO CODE
      *    ----------------------------------------------------
       01  STATE                      PIC X(2)   VALUE " ".
       01  CITY-TAX-CD                PIC X(3)   VALUE " ".
       01  COUNTY                     PIC X(3)   VALUE " ".
      *    ----------------------------------------------------

      ******************************************
      *    S C R E E N   B U F F E R S
      ******************************************
       01  DISPLAY-BUF         VALUE " ".
           03  DD-010          PIC ZZZZ,ZZ9.99.
           03  DD-020          PIC 99/99/99    BLANK ZERO.
           03  DD-030          PIC ZZZ.
           03  DD-040          PIC ZZZZ,ZZ9.99.
           03  DD-050          PIC ZZZZ,ZZ9.99.

       01  DISPLAY-LIST.
           03  FILLER          PIC X(20)  VALUE
           "010 020 030 040 050.".

       01  DISPLAY-BUF2.
           03  FILLER          PIC X(17)  VALUE
               "AMOUNT RENEWED  :".
           03  FILLER          PIC X(17)  VALUE
               "FIRST DUE DATE  :".
           03  FILLER          PIC X(17)  VALUE
               "NO. OF PAYMENTS :".
           03  FILLER          PIC X(17)  VALUE
               "PAYMENT AMOUNT  :".
           03  FILLER          PIC X(17)  VALUE
               "TOTAL REPAYABLE :".

       01  DISPLAY-LIST2.
           03  FILLER          PIC X(35)  VALUE
           "P010:S P020:S P030:S P040:S P050:S.".

       COPY "LIBWI/LNQUOTW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPWKCIPL.CPY".
       COPY "LIBLP/LPWKCOPL.CPY".
       COPY "LIBLP/LPWKCEPL.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/PAYOFFW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBCL/MFEE04W.CPY".
       COPY "LIBCL/GASCHGW.CPY".
       COPY "LIBCL/SCSCHGW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LNRNEW_DEF.CPY".
       COPY "LIBWI/LNRNEW_WKS.CPY".

       01  GA-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/GASC".
           03  GA-SCHG-NO     PIC 99.
       01  SC-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/SCSC".
           03  SC-SCHG-NO     PIC 99.

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LNRNEWW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LNRNEW_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME LNRNEW-BUF EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNRNEW"      TO VDU-FORM-NAME.

           MOVE " "           TO VDU-WIMENU-SELECTION.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE LNRNEW-BEGIN-X * VDU-WIN-L-OFFSET
               AT  SCREEN COL  LNRNEW-BEGIN-Y * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE    LNRNEW-COLUMNS + VDU-WIN-W-MOD
               LINES   LNRNEW-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS LNRNEW-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE "ACCOUNT NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF LN-POCD-VOID
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF LN-POCD-RESCIND
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF LN-POCD-INRESCISSION
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM GET-BR.
           IF IO-BAD
              GO TO IO-ERROR.
           MOVE BR-STATE TO THE-STATE.

           PERFORM GET-PAYOFF.

           IF LNRNEW-ONESTEP-RN-FG NOT = " "
              PERFORM WHICH-ONESTEP
           ELSE
              PERFORM WHICH-SPR.
           IF IO-BAD
              GO TO END-ROUTINE.

           PERFORM DISPLAY-FIELDS.

       ENTER-AGAIN.
           IF LNRNEW-ONESTEP-RN-FG NOT = " "
              MOVE " F6-CALCULATOR, F7-EXIT, U-UPDATE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
              MOVE " F6-CALCULATOR, F7-EXIT:" TO MESS
              PERFORM SEND-LEGEND.

           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF F6-KEY
              PERFORM MORE-INFO
              GO TO ENTER-AGAIN.
           IF F7-KEY
              GO TO END-ROUTINE.
           IF ENTER-KEY
              IF LNRNEW-ONESTEP-RN-FG NOT = " "
                 IF VDUBUF = "U"
                    MOVE "U" TO LNRNEW-ONESTEP-RN-FG
                    GO TO END-ROUTINE.
           GO TO ENTER-AGAIN.

      ******************************************************
       WHICH-SPR SECTION.
      ******************************************************
           MOVE "S  A170PTHE:FSR."    TO VDU.
           MOVE " RENEW INTO ------>" TO VDUBUF.
           PERFORM SCREENIO.
           IF BR-OVERRIDE-BR-STATE = "Y"
              MOVE "S  A080THE:S."      TO VDU
              MOVE " STATE CLASS SUBCL" TO VDUBUF
              PERFORM SCREENIO
              IF LN-SALEFIN = "Y"
                 MOVE "S  ATHESTATE." TO VDU
                 MOVE BR-STATE        TO VDUBUF
                 PERFORM SCREENIO
                 MOVE "SNNN020CLASS."     TO VDU
                 MOVE BR-DEFAULT-SC-CLASS TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCL."        TO VDU
                 MOVE BR-DEFAULT-SC-SUBCLASS TO VDUNUM0
              ELSE
                 MOVE "S  A020STATE." TO VDU
                 MOVE BR-STATE        TO VDUBUF
                 PERFORM SCREENIO
                 MOVE "SNNN020CLASS." TO VDU
                 MOVE LN-CLASS        TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCL."        TO VDU
                 MOVE BR-DEFAULT-SC-SUBCLASS TO VDUNUM0
           ELSE
              MOVE "S  A080THE:S."      TO VDU
              MOVE "       CLASS SUBCL" TO VDUBUF
              PERFORM SCREENIO
              IF LN-SALEFIN = "Y"
                 MOVE "SNNN020CLASS."     TO VDU
                 MOVE BR-DEFAULT-SC-CLASS TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCL."        TO VDU
                 MOVE BR-DEFAULT-SC-SUBCLASS TO VDUNUM0
              ELSE
                 MOVE "SNNN020CLASS." TO VDU
                 MOVE LN-CLASS        TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCL." TO VDU
                 MOVE LN-SUBCLASS     TO VDUNUM0.
           PERFORM SCREENIO.

       WHICH-SPR-STATE.
           IF BR-OVERRIDE-BR-STATE NOT = "Y"
              MOVE BR-STATE TO THE-STATE
              GO TO WHICH-SPR-CLASS.
           MOVE "F7, ENTER THE STATE CODE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE 9 TO IO-FG
              GO TO WHICH-SPR-EXIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO WHICH-SPR-STATE.
           IF VDUBUF = " "
              GO TO WHICH-SPR-STATE.
           MOVE VDUBUF TO THE-STATE ST-CODE.
           IF VDUTAB(3) NOT = " "
              GO TO WHICH-SPR-STATE.
           PERFORM GET-ST.
           IF IO-BAD
              GO TO WHICH-SPR-STATE.

       WHICH-SPR-CLASS.
           IF BR-OVERRIDE-BR-STATE = "Y"
              MOVE "F7, F6-SCAN, ^, ENTER CLASS NO." TO MESS
           ELSE
              MOVE "F7, F6-SCAN, ENTER CLASS NO." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020CLASS." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              IF BR-OVERRIDE-BR-STATE = "Y"
                 GO TO WHICH-SPR-STATE.
           IF F6-KEY
              MOVE THE-STATE TO SPSCAN-ORGST
              MOVE 0 TO SPSCAN-SPRCLASS
                        SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-BAD
                 GO TO WHICH-SPR-CLASS
              ELSE
                 MOVE "SNNN020CLASS."    TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0
                                            SP-SPRCLASS
                                            THE-CLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCL."    TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                                            THE-SUBCLASS
                 MOVE SPSCAN-SP-LAWCODE  TO SP-LAWCODE
                 GO TO WHICH-SPR-TEST-EFFDATE.
           IF F7-KEY
              MOVE 9 TO IO-FG
              GO TO WHICH-SPR-EXIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO WHICH-SPR-CLASS.
           IF VDUNUM0 = 0
              GO TO WHICH-SPR-CLASS.
           MOVE VDUNUM0 TO THE-CLASS LC-CODE.
           PERFORM GET-LC.
           IF IO-BAD
              GO TO WHICH-SPR-CLASS.

       WHICH-SPR-SUBCLASS.
           MOVE "F7, F6-SCAN, ^, ENTER SUBCLASS NO." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020SUBCL." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO WHICH-SPR-CLASS.
           IF F6-KEY
              MOVE THE-STATE TO SPSCAN-ORGST
              MOVE THE-CLASS TO SPSCAN-SPRCLASS
              MOVE 0         TO SPSCAN-SUBCLASS
                                SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO WHICH-SPR-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCL." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                                            THE-SUBCLASS
                 PERFORM SCREENIO
                 MOVE SPSCAN-SP-LAWCODE  TO SP-LAWCODE
                 GO TO WHICH-SPR-TEST-EFFDATE.
           IF F7-KEY
              MOVE 9 TO IO-FG
              GO TO WHICH-SPR-EXIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO WHICH-SPR-SUBCLASS.
           MOVE VDUNUM0 TO THE-SUBCLASS.

       WHICH-SPR-TEST-EFFDATE.
           PERFORM CHECK-SP-EFFDATE.
           IF IO-BAD
              MOVE "NO STATE PARAMETER RECORD FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO WHICH-SPR-CLASS.

       WHICH-SPR-EXIT.
           EXIT.

      ******************************************************
       WHICH-ONESTEP SECTION.
      ******************************************************
           MOVE "S  A080THE:S."       TO VDU.
           MOVE " STATE CLASS SUBCL"  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  ATHESTATE."       TO VDU.
           MOVE LN-OWNST              TO VDUBUF
                                         THE-STATE.
           PERFORM SCREENIO.
           MOVE "SNNN020CLASS."       TO VDU.
           MOVE LN-SPRCLASS           TO VDUNUM0
                                         THE-CLASS.
           PERFORM SCREENIO.
           MOVE "SNNN020SUBCL."       TO VDU.
           MOVE LN-SUBCLASS           TO VDUNUM0
                                         THE-SUBCLASS.
           PERFORM SCREENIO.

           PERFORM CHECK-SP-EFFDATE.

       WHICH-ONESTEP-EXIT.
           EXIT.

      *************************************************
       SPSCAN-SCAN SECTION.
      *************************************************
           MOVE SPACES TO SPSCAN-SP1-KEY.
           MOVE "N"    TO SPSCAN-SALEFIN.
           MOVE "Y"    TO SPSCAN-LAST-LAW.
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


      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************

           MOVE "S  A170PTHE." TO VDU.
           MOVE " RENEW INTO ------>" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLEAR-CALC-AREA-ZL.
           PERFORM SETUP-CALC-AREA.
           PERFORM CALL-LOAN-CALCULATIONS.

           MOVE POFF-NETDUE     TO DD-010.
           MOVE COP-PMT-DT    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO DD-020.

           MOVE COP-TERM        TO DD-030.
           MOVE COP-REG-PAYMENT TO DD-040.
           MOVE COP-TOT-REPYBLE TO DD-050.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **********************************************************
       CHECK-SP-EFFDATE SECTION.
      **********************************************************

           MOVE THE-STATE TO SP-ORGST.
           MOVE THE-CLASS TO SP-SPRCLASS.
           MOVE THE-SUBCLASS TO SP-SUBCLASS.
           MOVE 0 TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.

       CHECK-SP-EFFDATE-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-BAD
              GO TO CHECK-SP-EFFDATE-EXIT.

           IF NOT ( SP-ORGST    = THE-STATE    AND
                    SP-SPRCLASS = THE-CLASS    AND
                    SP-SUBCLASS = THE-SUBCLASS
                  )
              MOVE 9 TO IO-FG
              GO TO CHECK-SP-EFFDATE-EXIT.

           IF SP-EFFDATE > EXT-WI-CL-TRANS-DATE
              GO TO CHECK-SP-EFFDATE-NEXT
           ELSE
              IF SP-EXPDATE = 0
                 GO TO CHECK-SP-EFFDATE-EXIT.

           IF SP-EXPDATE NOT > EXT-WI-CL-TRANS-DATE
              GO TO CHECK-SP-EFFDATE-NEXT.

       CHECK-SP-EFFDATE-EXIT.
           PERFORM CLOSE-SP1-FILE.

      **********************************************
       SETUP-CALC-AREA SECTION.
      **********************************************
           MOVE EXT-WI-CL-TRANS-DATE TO CIP-LN-DT
                                        CIP-IN-DT
                                        NDTE-DATE
                                        SYS-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE             TO CIP-PMT-DT.

      * WORLD OPTION TO ADVANCE TO 1ST OF NEXT MONTH
      * IF NO CORRESPONDING DAY IN NEW MONTH:

           PERFORM SET-ADV-TO-1ST-NO-CORR.
           IF ADV-TO-1ST-NO-CORR = "Y"
              IF NOT ( PD-ORGST   = "TN" AND
                       PD-CLASS   = 01   AND
                       PD-ORGTERM = 12 )
                 IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
                    IF S-DD > NDTE-DD
                       MOVE 1 TO NDTE-DD
                                 NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO CIP-PMT-DT.

      * IF DATE OF THE LOAN IS ON THE 26 THRU 31 THEN ADVANCE THE
      * FIRST PAYMENT DUE DATE TO THE FIRST OF THE SECOND MONTH
      * FOLLOWING THE DATE OF THE LOAN. (REGMGM PR#1099)

           IF ADV-TO-1ST-NO-CORR = "A"
              IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                 IF S-DD > 25
                    MOVE 1 TO NDTE-DD
                              NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                    MOVE NDTE-DATE TO CIP-PMT-DT.

           IF LN-SALEFIN = "Y"
              MOVE "SC" TO CIP-SOURCE-CD
           ELSE
              MOVE "RN" TO CIP-SOURCE-CD.

           COMPUTE CIP-PRINTDUE = POFF-INTDUE + POFF-LCDUE.

           COMPUTE CIP-PRFINCHGRB = POFF-REBATE(4) + POFF-REBATE(5)
                                  + POFF-REBATE(6) + POFF-REBATE(7)
                                  + POFF-REBATE(8).
           COMPUTE CIP-PRINSRB = POFF-REBATE(1)  + POFF-REBATE(2)
                               + POFF-REBATE(3)  + POFF-REBATE(9)
                               + POFF-REBATE(10) + POFF-REBATE(11)
                               + POFF-REBATE(12) + POFF-REBATE(13).
           COMPUTE CIP-PRBAL = POFF-NETDUE + CIP-PRFINCHGRB
                             + CIP-PRINSRB - POFF-OTDUE
                             - POFF-INTDUE - POFF-LCDUE.

           MOVE LN-LOANDATE TO CIP-PRIOR-LOAN-DATE.
           IF PAYOFF-CDEL-ARREARAGE > 0
              MOVE PAYOFF-CDEL-ARREARAGE TO CIP-PRPAID-AHEAD
           ELSE
              MOVE 0 TO CIP-PRPAID-AHEAD.

           MOVE 0 TO CIP-PROCEEDS.
           MOVE "I" TO CIP-PROCEEDS-SW.

           MOVE 0 TO CIP-POINTS.
           MOVE "C" TO CIP-POINTS-SW.

           IF THE-CAN NOT = " "
              PERFORM SETUP-RE-CAN
           ELSE
              PERFORM SETUP-RE-LOAN.

      * TEST FOR GEORGIA SERVICE CHARGE:

           IF SP-SCFRMLA = 12 OR 13 OR 54
              MOVE " " TO SCHG-UPDATE-FG
              MOVE 0 TO SCHG-NEW-ACCTNO
              MOVE LN-SSNO(1)         TO SCHG-LN-SSNO(1)
              MOVE LN-SSNO(2)         TO SCHG-LN-SSNO(2)
              MOVE LN-SSNO(3)         TO SCHG-LN-SSNO(3)
              MOVE LN-SSNO(4)         TO SCHG-LN-SSNO(4)
              MOVE EXT-WI-CL-TRANS-DATE TO SCHG-TRANS-DATE
              IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE LN-PRLNNO       TO SCHG-PRIOR-ACCTNO
              ELSE
                 MOVE 0 TO SCHG-PRIOR-ACCTNO
              END-IF
              MOVE LN-ORGST           TO SCHG-ORGST
              MOVE LN-CLASS           TO SCHG-LN-CLASS
              MOVE SP-SCFRMLA         TO GA-SCHG-NO
              MOVE GA-SCHG-PATH TO FORM-NAM
              IF GA-SCHG-NO = 13
                 CALL FORM-PROGX USING FORM-PATH SCHG-WORKERS
                                                SCHG-WORKERS-2
              ELSE
                 CALL FORM-PROGX USING FORM-PATH SCHG-WORKERS
              END-IF
              CANCEL FORM-PROGX
              MOVE SCHG-RESTRICT-ADJ  TO CIP-RESTRICT-ADJ
              MOVE SCHG-DUAL-LOAN-ADJ TO CIP-DUAL-LOAN-ADJ.

      * TEST FOR SOUTH CAROLINA SERVICE CHARGE

           IF SP-SCFRMLA = 18 OR 42
              MOVE " "              TO SCSCHG-UPDATE-FG
              MOVE 0                TO SCSCHG-NEW-ACCTNO
              MOVE LN-SSNO(1)       TO SCSCHG-LN-SSNO(1)
              MOVE LN-SSNO(2)       TO SCSCHG-LN-SSNO(2)
              MOVE LN-SSNO(3)       TO SCSCHG-LN-SSNO(3)
              MOVE LN-SSNO(4)       TO SCSCHG-LN-SSNO(4)
              MOVE EXT-WI-CL-TRANS-DATE TO SCSCHG-TRANS-DATE
              IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE LN-PRLNNO     TO SCSCHG-PRIOR-ACCTNO
              ELSE
                 MOVE 0             TO SCSCHG-PRIOR-ACCTNO
              END-IF
              MOVE LN-ORGST         TO SCSCHG-ORGST
              MOVE SP-SCFRMLA       TO SC-SCHG-NO SCSCHG-SCFRMLA
              IF SP-SCFRMLA = 42
                 MOVE 18 TO SC-SCHG-NO
              END-IF
              MOVE SC-SCHG-PATH TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH SCSCHG-WORKERS
              CANCEL FORM-PROGX
              MOVE SCSCHG-PRIOR-CASHADV   TO CIP-PRIOR-CASHADV
              MOVE SCSCHG-RESTRICT-SERCHG TO CIP-RESTRICT-SERCHG
              MOVE SCSCHG-RESTRICT-HIGH-CASHADV
                                    TO CIP-RESTRICT-HIGH-CASHADV.
      * TEST FOR MAINTFEE

           IF SP-MFFRMLA = 4
              MOVE PD-SSNO(1) TO MFEE04-LN-SSNO(1)
              MOVE PD-SSNO(2) TO MFEE04-LN-SSNO(2)
              MOVE PD-SSNO(3) TO MFEE04-LN-SSNO(3)
              MOVE PD-SSNO(4) TO MFEE04-LN-SSNO(4)
              MOVE EXT-WI-CL-TRANS-DATE TO MFEE04-TRANS-DATE
              IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE LN-PRLNNO TO MFEE04-PRIOR-ACCTNO
              ELSE
                 MOVE 0 TO MFEE04-PRIOR-ACCTNO
              END-IF
              MOVE SP-MFFRMLA TO MFEE04-MFFRMLA
              MOVE "Y" TO MFEE04-CHARGE-MFEE
              MOVE "CL/MFEE04" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MFEE04-WORKERS
              CANCEL FORM-PROGX
              IF MFEE04-CHARGE-MFEE = "Y"
                 MOVE 0 TO CIP-MFEE-RESTRICT
              ELSE
                 MOVE 50.00 TO CIP-MFEE-RESTRICT.

      ******************************************************
       SETUP-RE-LOAN SECTION.
      ******************************************************
           IF LN-OVRDRATE NOT = 999.999
              MOVE LN-EFFRATE TO CIP-OVR-RATE
              MOVE "E" TO CIP-OVR-RATE-SW
           ELSE
              MOVE "C" TO CIP-OVR-RATE-SW.
           MOVE "C" TO CIP-INS-SW(1) CIP-INS-SW(2)
                       CIP-INS-SW(3) CIP-INS-SW(4)
                       CIP-SERVICE-CHG-SW
                       CIP-MAINT-FEES-SW CIP-1ST-PY-EXT-SW
                       CIP-REG-PAYMENT-SW.
           MOVE "I" TO CIP-INS-SW(5) CIP-INS-SW(6).

           MOVE LN-ORGST       TO CIP-PRIOR-LOAN-STATE.
           MOVE LN-CLASS       TO CIP-PRIOR-LOAN-CLASS.
           MOVE LN-SUBCLASS    TO CIP-PRIOR-SUB-CLASS.
           MOVE LN-LOANTYPE    TO CIP-LOANTYPE.
           MOVE LN-SMALLOAN    TO CIP-SMALLOAN.
           MOVE LN-SALEFIN     TO CIP-SALEFIN.
           MOVE LN-SERVACCT    TO CIP-SERVACCT.
           MOVE LN-ORGTERM     TO CIP-TERM.
           MOVE LN-PURCD       TO CIP-PURPOSE-CD.
           MOVE LN-COLLCD      TO CIP-COLLTRL-CD.
           MOVE LN-INSURED     TO CIP-INS-WHO.
           MOVE LN-INSTYPES    TO CIP-INS-TYPE.
           MOVE LN-INSELIGIBLE TO CIP-INS-ELIG.

           MOVE 0 TO CIP-GL-AMOUNT-TOTAL
                     CIP-GL-AMOUNT-TOTAL-FINCHG.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               ADD LN-FEEAMT(SUB) TO CIP-GL-AMOUNT-TOTAL
               IF LN-FEEFINCHG(SUB) = "Y"
                  ADD LN-FEEAMT(SUB) TO CIP-GL-AMOUNT-TOTAL-FINCHG
               END-IF
           END-PERFORM.

      ******************************************************
       SETUP-RE-CAN SECTION.
      ******************************************************
           MOVE PD-ORGST       TO CIP-PRIOR-LOAN-STATE.
           MOVE PD-CLASS       TO CIP-PRIOR-LOAN-CLASS.
           MOVE PD-SUBCLASS    TO CIP-PRIOR-SUB-CLASS.
           MOVE PD-LOANTYPE    TO CIP-LOANTYPE.
           MOVE PD-SMALLOAN    TO CIP-SMALLOAN.
           MOVE PD-SALEFIN     TO CIP-SALEFIN.
           MOVE PD-SERVACCT    TO CIP-SERVACCT.
           MOVE PD-ORGTERM     TO CIP-TERM.
           MOVE PD-PURCD       TO CIP-PURPOSE-CD.
           MOVE PD-COLLCD      TO CIP-COLLTRL-CD.
           MOVE PD-INSURED     TO CIP-INS-WHO.
           MOVE PD-INSTYPES    TO CIP-INS-TYPE.
           MOVE PD-INSELIGIBLE TO CIP-INS-ELIG.

           MOVE PD-OVRDRATE    TO CIP-OVR-RATE.
           MOVE PD-OVRDRATE-FG TO CIP-OVR-RATE-SW.
           MOVE PD-EFFRATE     TO CIP-EFF-RATE
                                  CEP-EFF-RATE.
           MOVE PD-APRATE      TO CIP-APR-RATE
                                  CEP-APR-RATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE PD-INSCOMP(SUB)   TO CIP-INS-CO(SUB)
              MOVE PD-INSPREM(SUB)   TO CIP-INS-PREMIUM(SUB)
                                        CEP-INS-PREMIUM(SUB)
              MOVE PD-INSTRM(SUB)    TO CIP-INS-TERM(SUB)
                                        CEP-INS-TERM(SUB)
              MOVE PD-INSCOVR(SUB)   TO CIP-INS-COVERAGE(SUB)
                                        CEP-INS-COVERAGE(SUB)
              MOVE PD-INSOURS(SUB)   TO CIP-INS-USE-SW(SUB)
              MOVE PD-INS-FG(SUB)    TO CIP-INS-SW(SUB)
              MOVE PD-INSFINCHG(SUB) TO CIP-INSFINCHG-SW(SUB)
           END-PERFORM.

           MOVE 0 TO CIP-GL-AMOUNT-TOTAL
                     CIP-GL-AMOUNT-TOTAL-FINCHG.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               ADD PD-FEEAMT(SUB)    TO CIP-GL-AMOUNT-TOTAL
               IF PD-FEEFINCHG(SUB) = "Y"
                  ADD PD-FEEAMT(SUB) TO CIP-GL-AMOUNT-TOTAL-FINCHG
               END-IF
           END-PERFORM.

           MOVE PD-SERCHG            TO CIP-SERVICE-CHG
                                        CEP-SERVICE-CHG.
           MOVE PD-STATE-WORKER      TO CIP-STATE-WORKER.
           MOVE PD-SERCHG-FG         TO CIP-SERVICE-CHG-SW.
           MULTIPLY PD-MAINTFEE BY PD-ORGTERM
                                 GIVING CIP-MAINT-FEES ROUNDED
                                        CEP-MAINT-FEES ROUNDED.
           MOVE PD-MAINTFEE-FG       TO CIP-MAINT-FEES-SW.
           MOVE PD-EXTCHG            TO CIP-1ST-PY-EXT
                                        CEP-1ST-PY-EXT.
           MOVE PD-EXTCHG-FG         TO CIP-1ST-PY-EXT-SW.
           MOVE PD-INTCHG            TO CIP-INT-CHARGE.
           MOVE PD-APR-FINAMT        TO CIP-APR-FINANCD.
           MOVE PD-FINAMT            TO CIP-AMT-FINANCD.
           MOVE PD-LNAMT             TO CIP-LOAN-AMOUNT.

           ADD PD-ANTICERN(1) PD-ANTICERN(2) PD-ANTICERN(3)
                              PD-ANTICERN(5)
                                 GIVING CIP-ANTIC-EARNS.
           IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                          GP-INCL-OTH-IN-EARNED-R)
               ADD PD-ANTICERN(6)    TO CIP-ANTIC-EARNS.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
               ADD PD-ANTICERN(4)    TO CIP-ANTIC-EARNS.

           MOVE PD-1STPYAMT          TO CIP-ODD-1ST-PMT.
           MOVE PD-REGPYAMT          TO CIP-REG-PAYMENT.
           MOVE PD-REGPYAMT-FG       TO CIP-REG-PAYMENT-SW.

      **********************************************************
       CALL-LOAN-CALCULATIONS SECTION.
      **********************************************************
           MOVE "CALCULATING - PLEASE WAIT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE THE-CAN      TO PD-CAN-LOAN-NO.
           MOVE "LP/CALCZL"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                CIP-CALCULATION-INPUT-AREA
                                CEP-CALCULATION-EDIT-AREA
                                COP-CALCULATION-OUTPUT-AREA
                                SP-REC
                                BR-REC STATE CITY-TAX-CD COUNTY.
           CANCEL FORM-PROGX.

      *************************************
       MORE-INFO SECTION.
      *************************************
           PERFORM SETUP-PD-RECORD.
           PERFORM OPEN-PD1-FILE.
           PERFORM WRITE-PD1-FILE.
           IF IO-BAD
              PERFORM REWRITE-PD1-FILE
              IF IO-BAD
                 MOVE "ERROR CREATING TEMPORARY RECORD" TO MESS
                 PERFORM SEND-MESS
                 MOVE "WILL PROCEED TO CALCULATOR, WITH OLD INFO"
                                                          TO MESS
                 PERFORM SEND-MESS.
           PERFORM CLOSE-PD1-FILE.

           MOVE "RN"       TO LNQUOT-MAINT-FG.
           MOVE PD1-KEY    TO LNQUOT-PD1-KEY.

           MOVE "WI/LNQUOT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LNQUOT-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      **************************************************
       SETUP-PD-RECORD SECTION.
      **************************************************
           IF THE-CAN = " "
              PERFORM CLEAR-PD-FILE.

      * SETUP PRIMARY KEY 1:
           MOVE "X"        TO PD-ID.
           MOVE LN-OWNBR   TO PD-BRANCH.
           MOVE LN-SSNO(1) TO PD-SOC-SEC.

      * SETUP KEY 2:
           MOVE "X"        TO PD2-ID.
           MOVE LN-OWNBR   TO PD2-BRANCH.
           MOVE " "        TO PD2-BWNAME.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE LN-SSNO(SUB)    TO PD-SSNO(SUB)
              MOVE LN-MAKERCD(SUB) TO PD-MAKERCD(SUB)
           END-PERFORM.

           IF LN-SALEFIN = "Y"
              MOVE "SC" TO PD-SRCD
           ELSE
              MOVE "RN" TO PD-SRCD.
           MOVE LN-PURCD       TO PD-PURCD.
           MOVE LN-COLLCD      TO PD-COLLCD.
           MOVE LN-COLLID      TO PD-COLLID.
           MOVE LN-CONFIDENTCD TO PD-CONFIDENTCD.

           IF THE-CAN = " "
              MOVE SP-ORGST    TO PD-ORGST
                                  PD-OWNST
              MOVE LN-OWNBR    TO PD-ORGBR
                                  PD-OWNBR
              MOVE SP-SPRCLASS TO PD-CLASS
                                  PD-SPRCLASS
              MOVE SP-SUBCLASS TO PD-SUBCLASS
              MOVE SP-LAWCODE  TO PD-LAWCODE

              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                 IF LN-FEEAMT(SUB) NOT = 0
                    MOVE LN-FEECODE(SUB)   TO PD-FEECODE(SUB)
                    MOVE LN-FEEAMT(SUB)    TO PD-FEEAMT(SUB)
                    MOVE LN-FEEFINCHG(SUB) TO PD-FEEFINCHG(SUB)
                    MOVE "Y"               TO PD-FEEOURS(SUB)
                 END-IF
              END-PERFORM

              MOVE COP-INS-CODES            TO PD-INSCODES
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                 MOVE COP-INS-CO(SUB)       TO PD-INSCOMP(SUB)
                 MOVE COP-INS-USE-SW(SUB)   TO PD-INSOURS(SUB)
                 MOVE COP-INS-TERM(SUB)     TO PD-INSTRM(SUB)
                 MOVE COP-INS-COVERAGE(SUB) TO PD-INSCOVR(SUB)
                 MOVE COP-INS-PREMIUM(SUB)  TO PD-INSPREM(SUB)
              END-PERFORM

              MOVE LN-LC-CODE               TO PD-LC-CODE
              MOVE 9999.99                  TO PD-DLNO
           END-IF.

           MOVE COP-LOANTYPE     TO PD-LOANTYPE.
           MOVE COP-SMALLOAN     TO PD-SMALLOAN.
           MOVE COP-SALEFIN      TO PD-SALEFIN.
           MOVE COP-SERVACCT     TO PD-SERVACCT.

           MOVE EXT-WI-CL-TRANS-DATE TO PD-ENTDATE.
           MOVE COP-LN-DT        TO PD-LOANDATE.
           MOVE COP-IN-DT        TO PD-INTDATE.
           MOVE COP-PMT-DT       TO PD-1STPYDATE PD-ORIG-1STPYDATE.
           MOVE COP-FL-DT        TO PD-FILEDATE.

           MOVE COP-APR-RATE     TO PD-APRATE.
           MOVE COP-EFF-RATE     TO PD-EFFRATE.
           MOVE COP-SIM-RATE     TO PD-SMPRATE.
           MOVE COP-OVR-RATE     TO PD-OVRDRATE.
           MOVE 999.99           TO PD-SERVPER.
           MOVE 999.999          TO PD-ENDORATE.

           MOVE COP-PROCEEDS     TO PD-PROCEEDS.
           MOVE COP-LOAN-AMOUNT  TO PD-LNAMT PD-SUM-LNAMT
                                    PD-CURBAL
                                    PD-ORIGPURAMT PD-SUM-ORIGPURAMT
                                    PD-PURBAL.

           MOVE COP-FINCHG       TO PD-FINCHG PD-SUM-FINCHG.
           MOVE COP-INT-CHARGE   TO PD-INTCHG PD-SUM-INTCHG.
           MOVE COP-SERVICE-CHG  TO PD-SERCHG PD-SUM-SERCHG.
           MOVE COP-1ST-PY-EXT   TO PD-EXTCHG PD-SUM-EXTCHG.
           MOVE COP-MAINT-FEES   TO PD-SUM-MAINTFEE.
           DIVIDE COP-MAINT-FEES BY COP-TERM GIVING
                                    PD-MAINTFEE ROUNDED.
           MOVE COP-POINTS       TO PD-POINTS.
           MOVE COP-TERM         TO PD-ORGTERM.
           MOVE COP-REG-PAYMENT  TO PD-REGPYAMT PD-SUM-REGPYAMT.
           MOVE COP-ODD-1ST-PMT  TO PD-1STPYAMT.
           IF PD-1STPYAMT = 0
              MOVE 0             TO PD-NOODDPY
              MOVE PD-ORGTERM    TO PD-NOREGPY
           ELSE
              MOVE 1             TO PD-NOODDPY
              SUBTRACT 1 FROM PD-ORGTERM GIVING PD-NOREGPY.

           MOVE COP-STATE-WORKER TO PD-STATE-WORKER.

           MOVE COP-TERM         TO PD-ANTICTERM.
           IF PD-LOANTYPE = "I"
              MOVE COP-SERVICE-CHG TO PD-ANTICERN(3)
              MOVE 0               TO PD-ANTICERN(1)
                                      PD-ANTICERN(2)
                                      PD-ANTICERN(4)
           ELSE
              ADD COP-INT-CHARGE COP-1ST-PY-EXT GIVING PD-ANTICERN(1)
              MOVE COP-SERVICE-CHG TO PD-ANTICERN(3)
              MOVE COP-MAINT-FEES  TO PD-ANTICERN(4).
           MOVE 0 TO PD-ANTICERN(5)
                     PD-ANTICERN(6).

           MOVE PD-ANTICERN(1)  TO WK-ANTICERN(1).
           MOVE PD-ANTICERN(2)  TO WK-ANTICERN(2).
           MOVE PD-ANTICERN(3)  TO WK-ANTICERN(3).
           MOVE PD-ANTICERN(4)  TO WK-ANTICERN(4).
           MOVE PD-ANTICERN(5)  TO WK-ANTICERN(5).
           MOVE PD-ANTICERN(6)  TO WK-ANTICERN(6).
           PERFORM ADJUST-ANTICERN.
           MOVE WK-ANTICERN(1)  TO PD-ANTICERN(1).
           MOVE WK-ANTICERN(2)  TO PD-ANTICERN(2).
           MOVE WK-ANTICERN(3)  TO PD-ANTICERN(3).
           MOVE WK-ANTICERN(4)  TO PD-ANTICERN(4).
           MOVE WK-ANTICERN(5)  TO PD-ANTICERN(5).
           MOVE WK-ANTICERN(6)  TO PD-ANTICERN(6).

           MOVE LN-ACCTNO       TO PD-PRLNNO.
           MOVE COP-PRBAL       TO PD-PRBAL.
           MOVE COP-PRINTDUE    TO PD-PRINTDUE.
           MOVE COP-PRFINCHGRB  TO PD-PRFINCHGRB.
           MOVE COP-PRINSRB     TO PD-PRINSRB.

           MOVE COP-APR-FINANCD TO PD-APR-FINAMT.
           MOVE COP-AMT-FINANCD TO PD-FINAMT.

           MOVE COP-PROCEEDS-SW TO PD-PROCEEDS-FG.
           MOVE COP-POINTS-SW   TO PD-POINTS-FG.
           MOVE COP-OVR-RATE-SW TO PD-OVRDRATE-FG.
           MOVE COP-INS-SW(1)   TO PD-INS-FG(1).
           MOVE COP-INS-SW(2)   TO PD-INS-FG(2).
           MOVE COP-INS-SW(3)   TO PD-INS-FG(3).
           MOVE COP-INS-SW(4)   TO PD-INS-FG(4).
           MOVE COP-INS-SW(5)   TO PD-INS-FG(5).
           MOVE COP-INS-SW(6)   TO PD-INS-FG(6).
           MOVE COP-INS-SW(7)   TO PD-INS-FG(7).
           MOVE COP-INS-SW(8)   TO PD-INS-FG(8).
           MOVE COP-SERVICE-CHG-SW TO PD-SERCHG-FG.
           MOVE COP-MAINT-FEES-SW  TO PD-MAINTFEE-FG.
           MOVE COP-1ST-PY-EXT-SW  TO PD-EXTCHG-FG.
           MOVE COP-REG-PAYMENT-SW TO PD-REGPYAMT-FG.
           IF COP-1ST-PMT-MTHS = 1 AND
              COP-1ST-PMT-DAYS = 0
              MOVE "Y" TO PD-ONE-MTH-NO-DAY-EXT-SW
           ELSE
              MOVE " " TO PD-ONE-MTH-NO-DAY-EXT-SW.

           MOVE COP-PRPAID-AHEAD  TO PD-PRPAID-AHEAD.
           MOVE COP-RESTRICT-ADJ  TO PD-RESTRICT-ADJ.
           MOVE COP-DUAL-LOAN-ADJ TO PD-DUAL-LOAN-ADJ.

           MOVE THE-CAN           TO PD-CAN-LOAN-NO.


      ****************************************
       GET-PAYOFF SECTION.
           MOVE LN-ACCTNO TO PAYOFF-ACCTNO.
           MOVE EXT-WI-CL-TRANS-DATE TO PAYOFF-PAYDATE.
           IF LN-SALEFIN = "Y"
              MOVE "SC" TO PAYOFF-SRCD
           ELSE
              MOVE "RN" TO PAYOFF-SRCD.
           MOVE EXT-WI-CL-MAKERCD(1) TO PAYOFF-MAKERCD.
           MOVE " " TO PAYOFF-DEF-STOP.
           MOVE "CL/PAYOFF" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PAYOFF-WORKERS
                                          POFF-WORKERS.
           CANCEL FORM-PROGX.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LNRNEW-SCREEN UPON LNRNEW-WINDOW-HANDLE..
           MOVE LNRNEW-FORM-FIELDS TO WR-BUF.
           MOVE LNRNEW-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE LNRNEW-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNRNEW-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LNRNEW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPZLCL.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
       GET-ST.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
       SEND-LEGEND.
           MOVE "S  A600MSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
      * SET ADV-TO-1ST-NO-CORR. IF THE SP FLAG IS SET, ALLOW IT TO
      * OVERRIDE THE GP FOR SPECIFIC CLASSES.
      *-----------------------------------------------------------------
       SET-ADV-TO-1ST-NO-CORR.
           IF NOT (SP-ADV-TO-1ST-NO-CORR = "N" OR " ")
              MOVE SP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR
           ELSE
              MOVE GP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNRNEW-SCREEN.
           DESTROY LNRNEW-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
