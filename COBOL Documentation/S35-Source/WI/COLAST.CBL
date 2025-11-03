       IDENTIFICATION DIVISION.
       PROGRAM-ID.      COLAST.
      **************************************************************************
      *   DIR :  WI
      *   DESC: COLLECTIONS ASSIST SCREEN
      *         CALLED FROM INQUIRIES AND PAY POSTING
      *         F11 FROM POSTING DATE
      *
      * REV:
      *  BAH 021107 NEW FOR REGACC PR# 8
      *  BAH 030414 FIXED A BAD DISPLAY OF GARBAGE IN THE REMAIN COLUMN
      *             IF SUB > 8 IN SCHED COLUMN...BR 561 ACCT 320425 REGACC
      *  BAH 030521 ADDED EMAIL: AND ALT ADR: DISPLAY FIELDS, REGACC PR# 27
      *  BAH 030528 CHANGED SUB FROM 99 TO 999, COULDNT HANDLE PAYMENT
      *             SCHEDULE TABLE OVER 99 PAYMENTS....REGACC
      *  BAH 030918 CHANGED TO LPCRNO FROM CALLING CRNORC BECAUSE IT NEEDED
      *             A CUSTOMER PAYOFF (PO) AND ALSO THE INDU-PENALTY HAD TO
      *             BE IN THIS PROGRAM TO SUBTRACT FROM THE DEFEREMENT
      *             LENDMARK REQUESTED THE PAYOFF TO BE "PO" SO PENALTIES
      *             WOULD BE REFLECTED AND MATCHES THE COLLECTION INQUIRY
      *             PL# 420
      * KEC 040120 PR#46 REGIONAL ACCEPTANCE 
      *            REPLACED THE LOGIC FOR SETTING DT01-GRADE.  NOW USING
      *            LRB-GRID-ELE(WSLRB-X WSLRB-Y).
      * BAH 040423 DISPLAY-PAID WAS NOT EXITING UPON REACHING THE PAID
      *            THRU DATE WITH A PAYMENT SCHEDULE, LENDMARK
      * BAH 040614 REMOVE "OR LR-PR10-DELQ-STGY-ID <= 200" SO BEHAVIOR GRADE
      *             ALWAYS DISPLAYS FOR REGACC PR# 63
      * BAH 040709 BUILD-PAYMENT-TABLE WAS MISSING THE MOVES TO DATER-UNITPER
      *            FOR WEEKLY PAYMENTS TO WORK, EXPRESS
      * BAH 050607 ADDED "D2" AMOUNT FOR REGACC PR# 106
      * KEC 060120 PR#121 REGIONAL ACCEPTANCE ADDED LOGIC TO DIPLAY RTC DATES.
      * BAH 060515 IF LR WAS NOT FOUND, WAS DISPLAYING 20/20/20 AS DATE, WAS
      *            CLEARING D- FIELDS THEN MOVING THE LR FIELDS OVER THEM.
      * BAH 070124 SHOULD HAVE BEEN OPENING ALL LR FILES TO I-O SO NO ONE GETS
      *            FILE NOT FOUND ERRORS !!!!!!!!!!
      * BAH 070418 CHANGED REGACC FIELD1 FOR CPP IN EFFECT TO A MISC FIELD
      *            THAT OTHERS CAN FILL WITH SOMETHING ELSE.
      *            GP-I-AM-UNITED USING FIELD1 FOR MONIES TENDERED, UNITED
      *                                                             PR# 2959
      * BAH 071016 ADDED LN-PART-RECOURSE-DATE FOR REGACC ONLY PR# 184
      * BLF 071108 IF INVALID READ ON BXFILE, ZERO OUT BORROWER SCORE TABLE -
      *            WAS CAUSING MANY NON-NUMERICS IN DISPLAY-TRIAD MODULE
      *            FOR REGACC
      *  CS 080410 HIGHLIGHT THE DEFERMENT AREA REGACC PR#202
      *  CS 080421 HIGHLIGHT DF WHEN DF IS ALLOWWED. IF DF IS ZERO BUT THE
      *            CUSTOMER IS ALLOWED DF. FOR EX: IB LOAN PAYS PY TODAY BUT
      *            IS ELIGIBLE FOR DF. IB DF IS INTEREST DUE WHICH WOULD BE
      *            ZERO BUT WOULD ADVANCE PDTH. TEST IF DEFPOL-MAXIMUM =
      *            SPACES PR#202 REGACC
      * KEC 081223 HAD TO RENAME THE 78 LEVELS 'SET-CD-LR-...' TO
      *            'CD---LR-...'.
      *  CS 100630 BUG WHEN 1ST,REGPAYAMT & LASTPAYAMT ARE DIFFERENT & PDTH-
      *            PAYS > 1.  WAS JUST MOVE LN-NOREGPAY TO PAID FIELDS(2)
      *            CHANGED TO COMPUTE PAID(2) AS PDTH-PAYS -1.
      *                                            PL#692 UNITED ACCEPT
      * BAH 111005 ADDED "NUM DAYS INT DUE." FOR GP-I-AM-REGENCY PR# 3880
      * BAH 111017 GP-I-AM-CARFIN, ADD LN-LCBAL AND LN-OTHBAL TO D-TO-ROLL
      *                                                PR# 3887 CARFIN
      * BAH 120416 REPLACE 360 WITH SP-YEARTYPE FOR REGENCY PR# 3880
      * BAH 120815 REPLACED FLDVAL OF SYSTEM-DATE WITH ACCEPT D-DATE1
      * BAH 120914 ALLOW GP-I-AM-MULL TO USE THE MONIES TENDERED FOR
      *            UNITED, PR# 4108 MULL
      * BAH 140207 WHILE BUILDING THE PAYMENT SCHEDULE, WE ASSUMED NUM-DATE
      *            WOULD STILL BE GOOD AFTER PERFORMING LATE CHARGE ROUTINE,
      *            AND IT ISN'T, IT GETS SET BACK TO 1STPYDATE CAUSING THE
      *            PAID COLUMN TO BE WRONG, REGACC PL#838
      * BAH 140218 ** FOUND THE NON-NUMERIC IN ACU_TCLP **
      *            REMAINING-PAYMENTS TABLE NOT INITIATED TO ZERO FIRST!
      *            ADDED A CLEAR AT TOP OF DISPLAY-REMAIN, SUPPORT PL#825
      * BAH 140603 ADDED MATURITY DATE AND UPDATED MATURITY DATE, HONOR PR#4549
      * BAH 140818 MOVE EXT-CONAME-SYSDATE TO COLAST-SYSDATE IN FORM-RESET
      * BAH 150911 ADDED BX-EMAIL FOR HONOR ONLY, PR# 4852
      * BAH 151106 ADD LATE CHARGES DUE IN "TO ROLL", MULL/KSTREET PR#4889
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 160629 REMOVE TO ROLL AMOUNT FOR SOAUTO, PR#5104
      * BAH 170414 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.07.16 CHANGED LPCRNO TO CRNOR2
      * BAH 20190705 REMOVED LRR AND LRB PT0151
      * BAH 20220803 HARD CODED START
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/COLAST A32 05/07/20-14:16:34 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      ***********************************
      *      PROGRAM WORKERS
      ***********************************
       01  COLAST-LINES          PIC 99  VALUE 20.
       01  COLAST-COLUMNS        PIC 99  VALUE 78.
       01  COLAST-BEGIN-Y        PIC 99  VALUE 2.
       01  COLAST-BEGIN-X        PIC 99  VALUE 3.

       01  SUB               PIC 999    COMP-3 VALUE 0.
       01  CPN-INCR          PIC 999    COMP-3 VALUE 0.
       01  HOLD-FIRST-DATE   PIC 9(8) VALUE 0.
       01  REMAIN-SUB        PIC 9(3).

       01  SCHEDULED-PAYMENTS OCCURS 8.
           03  SCHED-NO      PIC 9(3).
           03  SCHED-AMT     PIC S9(7)V99 COMP-3.
       01  PAID-PAYMENTS OCCURS 8.
           03  PAID-NO       PIC 9(3).
           03  PAID-AMT      PIC S9(7)V99 COMP-3.
       01  REMAINING-PAYMENTS OCCURS 8.
           03  REMAIN-NO     PIC S9(3).
           03  REMAIN-AMT    PIC S9(7)V99 COMP-3.

       01  DISPLAY-WORKER.
           03  DISP-NO      PIC Z(3).
           03  DISP-AT      PIC X.
           03  DISP-AMT     PIC Z(4)9.99.

       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  WORK-RATE            PIC S9(7)V9(5) COMP-3 VALUE 0.
       01  WORK-AMT             PIC S9(7)V99 COMP-3 VALUE 0.
       01  PAYMENT-TABLE        VALUE LOW-VALUES.
           03  PMT-TABLE        OCCURS 360.
               05  T-DATE-DUE   PIC 9(8)     COMP-3.
               05  T-AMT-DUE    PIC 9(6)V99  COMP-3.
               05  L-DATE-DUE   PIC 9(8)     COMP-3.
               05  L-AMT-DUE    PIC 9(6)V99  COMP-3.

       01  SYSTEM-DATE          PIC 9(8).

       01  D-ALLOT-ACH          PIC X(5).
       01  ALLOT-ACH-LIST       PIC X(8) VALUE "ALLOT:R.".

       01  DISPLAY-BUF.
           03  D-ACCTNO      PIC X(16).
           03  D-NAME        PIC X(30).
           03  D-SCHED       PIC X(12) OCCURS 8.
           03  D-PAID        PIC X(12) OCCURS 8.
           03  D-REMAIN      PIC X(12) OCCURS 8.
           03  D-TO-ROLL     PIC Z(6).99.
           03  D-PAST-DUE    PIC Z(6).99.
           03  D-PDTH        PIC 99/99.
           03  D-DUEDATE     PIC 99/99/99.
           03  D-PAYOFF      PIC Z(6).99.
           03  D-NODEF       PIC ZZ9.
           03  D-DEFAMT      PIC Z(4).99.
           03  D-D2AMT       PIC Z(4).99.
           03  D-LASTDEF     PIC 99/99/99 BLANK ZERO.
           03  D-LCHGS-DUE   PIC Z(6).99.
           03  D-LCHGS-COL   PIC Z(6).99.
           03  D-OTH-OWE     PIC Z(6).99.
           03  D-INT-PAID    PIC Z(6).99.
           03  D-PROMPT1     PIC X(17).
           03  D-FIELD1-X.
               05  FILLER    PIC X(8).
               05  D-FIELD1  PIC X.
      *    03  D-FIELD1-NUM REDEFINES D-FIELD1-X PIC Z(6).99.
           03  D-IB-RATE     PIC Z(3).999.
           03  D-RTC-DATE-1  PIC 99/99/99 BLANK ZERO.
           03  D-RTC-DATE-2  PIC 99/99/99 BLANK ZERO.
           03  D-COLLATERAL  PIC X(50).
           03  D-EMAIL       PIC X.
           03  D-ALT-ADR     PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(45) VALUE
           "LNNO NAME SCHED1 SCHED2 SCHED3 SCHED4 SCHED5 ".
           03  FILLER      PIC X(57) VALUE
           "SCHED6 SCHED7 SCHED8 PAID1 PAID2 PAID3 PAID4 PAID5 PAID6 ".
           03  FILLER      PIC X(52) VALUE
           "PAID7 PAID8 REMAIN1 REMAIN2 REMAIN3 REMAIN4 REMAIN5 ".
           03  FILLER      PIC X(50) VALUE
           "REMAIN6 REMAIN7 REMAIN8 ROLL PASTDUE PDTH DUEDATE ".
           03  FILLER      PIC X(26) VALUE
           "PAYOFF NODEF DEFAMT D2AMT ".
           03  FILLER      PIC X(31) VALUE
            "LASTDEF LCHGDUE LCHGCOL OTHOWE ".
           03  FILLER      PIC X(50) VALUE
           "INTPAID PROMPT1 FIELD1 RATE RTC-DATE-1 RTC-DATE-2 ".
           03  FILLER      PIC X(21) VALUE
           "COLLATERAL EMAIL ALT.".


       01  DISPLAY-MATURITY-BUF.

           03  DISPLAY-MDTE-LINE-01.
               05  FILLER      PIC X(17) VALUE "ORIGINAL MATURITY".
               05  FILLER      PIC X(2)  VALUE SPACES.
               05  ORIG-MDTE   PIC 99/99/99.
           03  DISPLAY-MDTE-LINE-02.
               05  FILLER      PIC X(17) VALUE "UPDATED  MATURITY".
               05  FILLER      PIC X(2)  VALUE SPACES.
               05  UPD-MDTE    PIC 99/99/99.

       01  DISPLAY-MATURITY-LIST.
           03  FILLER PIC X(18) VALUE "TRIAD-01 TRIAD-02.".


      ***********************************************

       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  COLAST-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/COLAST_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/COLAST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/COLASTW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/COLAST_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME COLAST-BUF EXIT-PATHNAME.

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE COLAST-LN-OWNBR  TO LN-OWNBR.
           MOVE COLAST-LN-ACCTNO TO LN-ACCTNO.
           MOVE "COLAST" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSTEM-DATE.

           PERFORM GET-GPENV.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE COLAST-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  COLAST-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    COLAST-COLUMNS + VDU-WIN-W-MOD
                      LINES   COLAST-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS COLAST-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE " INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE " STATE PARAMETER DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              MOVE " INVALID" TO BW-LNAME.

           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE SYSTEM-DATE   TO CRNOR2-SYSDATE.
           MOVE "PO"          TO CRNOR2-LPTRCD.
           MOVE "N"           TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE 0 TO SUB.
           PERFORM BUILD-PAYMENT-TABLE.

           PERFORM DISPLAY-FIELDS.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           GO TO END-ROUTINE.

      **************************************************
       BUILD-PAYMENT-TABLE SECTION.
      **************************************************
           MOVE ALL LOW-VALUES TO PAYMENT-TABLE.

           IF LN-PAY-SCHLD-FG = "Y"
              PERFORM GET-LT-P-REC
              IF IO-FG = 0
                 PERFORM UNPACK-PAYMENT-SCHEDULE
              ELSE
                 MOVE "PAYMENT SCHEDULE MISSING" TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           ADD LN-TOTNODEF LN-TOTNOALDEL GIVING NDTE-HOLD.
           MOVE LN-1STPYDATE    TO NDTE-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE HOLD-FIRST-DATE.
           MOVE NUM-DA TO DTE-HOLD.

           MOVE LN-SUM-REGPYAMT TO LCHG-LATE
                                   LCHG-CURPAY.
           MOVE NUM-DATE TO LCHG-LCPDTH-DATE.
           PERFORM LATE-CHARGE-CALCULATION.

      * NUM-DATE CAN GET WIPED OUT IN LATE-CHARGE-CALCULATION, DO NOT
      * RELY ON IT.
           MOVE HOLD-FIRST-DATE TO NUM-DATE.

       BUILD-PAYMENT-CONT.
           ADD 1 TO SUB.
           IF SUB > LN-SUM-ORGTERM OR SUB > 360
              GO TO BUILD-PAYMENT-EXIT.

           MOVE NUM-DATE TO T-DATE-DUE(SUB).

           IF LN-PAY-SCHLD-FG = "Y"
              MOVE SCHD-AMT(SUB) TO T-AMT-DUE(SUB)
                                    L-AMT-DUE(SUB)
           ELSE
           IF SUB = 1
              MOVE LN-1STPYAMT TO T-AMT-DUE(SUB)
                                  L-AMT-DUE(SUB)
           ELSE
              IF SUB NOT = LN-ORGTERM
                 MOVE LN-REGPYAMT TO T-AMT-DUE(SUB)
                                     L-AMT-DUE(SUB)
              ELSE
                 MOVE LN-LASTPYAMT TO T-AMT-DUE(SUB)
                                      L-AMT-DUE(SUB).

           IF L-AMT-DUE(SUB) NOT = LCHG-CURPAY
              MOVE L-AMT-DUE(SUB)  TO LCHG-CURPAY
                                      LCHG-LATE
              MOVE T-DATE-DUE(SUB) TO LCHG-LCPDTH-DATE
              PERFORM LATE-CHARGE-CALCULATION.
           ADD LCHG-CHARGE TO L-AMT-DUE(SUB).

           MOVE T-DATE-DUE(SUB) TO NDTE-DATE.
      * UPWK-LC-GRACE-FAC IS COMPUTED IN LATE-CHARGE-CALC
           IF SP-LCUNITPER-CD = "A"
              COMPUTE NDTE-HOLD = SP-LCGRACE * UPWK-LC-GRACE-FAC * .01
           ELSE
              MOVE SP-LCGRACE TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO L-DATE-DUE(SUB).

           MOVE T-DATE-DUE(SUB) TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE.
           IF LN-UNITPER-CD = "M"
              IF NUM-MO NOT = 2
                 MOVE DTE-HOLD TO NUM-DA.

           GO TO BUILD-PAYMENT-CONT.

       BUILD-PAYMENT-EXIT.
           EXIT.

      *******************************
       DISPLAY-FIELDS SECTION.
      *******************************
           MOVE SPACES TO DISPLAY-BUF.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-ACCTNO.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

           IF LN-ACH = "Y"
              MOVE "ACH" TO D-ALLOT-ACH
           ELSE
              IF LN-ALLOTCD = "Y"
                 MOVE "ALLOT" TO D-ALLOT-ACH
              ELSE
                 MOVE SPACES TO D-ALLOT-ACH.
           IF D-ALLOT-ACH NOT = SPACES
              MOVE D-ALLOT-ACH TO WR-BUF
              MOVE ALLOT-ACH-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * NEED THIS ONE WITH THE CORRECT PAYMENTS TO DATE FIGURE IN
      * THE LOAN RECORD TO GET THE AMOUNT "TO ROLL" CONTRACTUAL
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-AMT-TO-ADVANCE TO D-TO-ROLL.

      * TO GET THE "REMAINING" SCHEDULE CORRECT, YOU MUST SUBTRACT
      * LATE CHARGES PAID IF SP-CONTRFRMLA-TOTAL....OR THE PDTH-REMAIN
      * WONT BE CORRECT.
           IF SP-CONTRFRMLA-TOTAL
              MOVE " " TO SP-CONTRFRMLA
              COMPUTE LN-TOTPAYMNTD = LN-TOTPAYMNTD - LN-TOTLCHG.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           PERFORM DISPLAY-SCHEDULED.
           PERFORM DISPLAY-PAID.
           PERFORM DISPLAY-REMAIN.

           PERFORM GET-COLLATERAL-INFO.

      * TOTAL PAST DUE AMOUNT
           MOVE NO-TDUEAMT TO D-PAST-DUE.

           MOVE NO-PDTH-DATE TO SYS-DATE.
           MOVE 0            TO NUM-DATE.
      * CCYYMMDD  TO MMYY
           MOVE S-YY TO NUM-DA.
           MOVE S-MM TO NUM-MO.
      * PAID THRU
           MOVE NUM-DATE TO D-PDTH.
      * DUE DATE
           MOVE NO-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DUEDATE.
      * PAY OFF
           MOVE NO-TDUEPOFF TO D-PAYOFF.
      * # OF DEFERM
           MOVE LN-TOTNODEF TO D-NODEF.
      * DEF AMOUNT
      * NEW SOUTH NEEDS PENALTY OUT OF DEFERMENT

      * NEED TO RESTORE LN-TOTPAYMNTD AS USED IN PAID-THRU CALC
           COMPUTE LN-TOTPAYMNTD = LN-TOTPAYMNTD + LN-TOTLCHG.

           COMPUTE D-DEFAMT = NO-DEFERMENT - NO-INDU-PENALTY.

      *PR#202 CHANGED FROM "DF" TO "D2 MOVE TO TO DEFPOL-TRCD
           IF LN-CURBAL NOT = 0
              MOVE "D2" TO DEFPOL-TRCD
              MOVE 2 TO DEF-NODEF
              MOVE SYSTEM-DATE TO DEFPOL-DATE
              PERFORM VALIDATE-DF-POLICY
              IF DEFPOL-MAXIMUM NOT = " "
                 MOVE ZEROS TO D-D2AMT DEF-AMOUNT
              ELSE
              IF DEFPOL-MAXIMUM = " "
                 MOVE 2            TO DEF-NODEF
                 MOVE SYSTEM-DATE  TO DEF-PAYDATE
      * POFF-INTDUE WILL BE FROM CREATE NOTICE
                 MOVE POFF-INTDUE  TO DEF-TOTAL-INTDUE
                 PERFORM DEF-COMPUTE-DEFERMENT
                 COMPUTE D-D2AMT = DEF-AMOUNT - NO-INDU-PENALTY.
      * LAST DEF DATE
           IF LN-TOTNODEF NOT = 0
              PERFORM FIND-LAST-DEFERMENT
           ELSE
              MOVE 0 TO D-LASTDEF.
      * LCHGS DUE
           MOVE NO-TDUELC TO D-LCHGS-DUE.
      * LCHGS COLLECTED
           MOVE LN-TOTLCHG TO D-LCHGS-COL.

      * OTH CHRGS OWING
           MOVE LN-OTHBAL TO D-OTH-OWE.
      * TOTAL INT PAID
           MOVE LN-TOTINTPD TO D-INT-PAID.

      * PER DIEM IB RATE
           MOVE SYSTEM-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF NOT ( IBPC-FG = "P" AND
                    IBPC-EXPIRED-FLAG = " ")
              MOVE "PY"    TO RATE-LPTRCD
              MOVE SYSTEM-DATE TO RATE-DATE IBPC-DATE
              MOVE LN-MAKERCD(1) TO RATE-MAKERCD
              PERFORM SET-PY-RATE
              COMPUTE WORK-RATE ROUNDED = INDU-WORKER / INDU-WK
              MOVE WORK-RATE TO D-IB-RATE.

           MOVE SPACES TO D-FIELD1 D-PROMPT1.

      * PER DIEM = (OVRDRATE / 100 / 360) * CURBAL  (USING ONE ABOVE)
      * DAYS INT DUE = INT DUE / PER DIEM  (REGENCY)
      *       IF LN-LOANTYPE = "I"
      *          MOVE LN-ACCTNO     TO CRNOR2-ACCTNO
      *          MOVE SYSTEM-DATE   TO CRNOR2-SYSDATE
      *          MOVE "PB"          TO CRNOR2-LPTRCD
      *          MOVE "CL/CRNOR2"   TO FORM-NAM
      *          CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
      *                                POFF-WORKERS LN-REC SP-REC
      *          CANCEL FORM-PROGX
      *          COMPUTE PER-DIEM ROUNDED =
      *           ( (LN-OVRDRATE / 100) / SP-YEARTYPE )
      *                              * LN-CURBAL
      *          MOVE PER-DIEM TO D-IB-RATE
      *          MOVE "NUM DAYS INT DUE." TO D-PROMPT1
      *          COMPUTE D-FIELD1-NUM ROUNDED = POFF-INTDUE /
      *                                         PER-DIEM.


           MOVE ZEROES                  TO D-RTC-DATE-1 D-RTC-DATE-2.

           IF BW-EMAIL NOT = SPACES
              MOVE "Y" TO D-EMAIL
           ELSE
              MOVE "N" TO D-EMAIL.

           MOVE "N" TO D-ALT-ADR.

           MOVE BW-SSNO TO BX-SSNO.
           PERFORM OPEN-BX1-FILE
           PERFORM READ-BX1-FILE.
           PERFORM CLOSE-BX1-FILE.
           IF IO-GOOD
              IF BX-ALT-ADDRESS NOT = SPACES
                 MOVE "Y" TO D-ALT-ADR.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *ORIGINAL LOAN TERM
           MOVE "Y" TO MDTE-FLAG.
           MOVE "Y" TO MDTE-ORGTERM-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ORIG-MDTE.
      *UPDATED MATURITY
           MOVE " " TO MDTE-FLAG.
           MOVE "Y" TO MDTE-ORGTERM-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO UPD-MDTE.

           MOVE DISPLAY-MATURITY-BUF TO WR-BUF.
           MOVE DISPLAY-MATURITY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.


      *********************************
       FIND-LAST-DEFERMENT SECTION.
      *********************************
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           PERFORM START-LP1-FILE-NOT-GREATER.
       FIND-NEXT-DEFERMENT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 9 OR LN-OWNBR NOT = LP-BRNO OR
                            LN-ACCTNO NOT = LP-ACCTNO
              GO TO FIND-LAST-DEF-EXIT.

           IF NOT (LP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5"
                         OR "D6" OR "D7" OR "D8" OR "D9")
              GO TO FIND-NEXT-DEFERMENT.
           IF LP-REV = "Y"
              GO TO FIND-NEXT-DEFERMENT.


      * FOUND ONE
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LASTDEF.


       FIND-LAST-DEF-EXIT.
           PERFORM CLOSE-LP1-FILE.


      ********************************
       DISPLAY-SCHEDULED SECTION.
      ********************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE 0 TO SCHED-NO(SUB) SCHED-AMT(SUB)
           END-PERFORM.

           IF LN-PAY-SCHLD-FG = "Y"
              GO TO DISPLAY-PAYMENT-SCHEDULE.

           IF LN-REGPYAMT = LN-1STPYAMT
              IF LN-REGPYAMT = LN-LASTPYAMT
      * REG = 1ST = LAST
                MOVE LN-ORGTERM TO DISP-NO SCHED-NO(1)
                MOVE LN-REGPYAMT TO DISP-AMT SCHED-AMT(1)
                MOVE "@" TO DISP-AT
                MOVE DISPLAY-WORKER TO D-SCHED(1)
              ELSE
      * REG = 1ST, DIFF LAST
                COMPUTE NUM-DATE = LN-NOREGPY + 1
                MOVE NUM-DATE TO DISP-NO SCHED-NO(1)
                MOVE "@" TO DISP-AT
                MOVE LN-REGPYAMT TO DISP-AMT SCHED-AMT(1)
                MOVE DISPLAY-WORKER TO D-SCHED(1)
                MOVE 1 TO DISP-NO SCHED-NO(2)
                MOVE "@" TO DISP-AT
                MOVE LN-LASTPYAMT TO DISP-AMT SCHED-AMT(2)
                MOVE DISPLAY-WORKER TO D-SCHED(2)
            ELSE
      * 1ST DIFF, REG = LAST
              IF LN-REGPYAMT = LN-LASTPYAMT
                MOVE 1 TO DISP-NO SCHED-NO(1)
                MOVE "@" TO DISP-AT
                MOVE LN-1STPYAMT TO DISP-AMT SCHED-AMT(1)
                MOVE DISPLAY-WORKER TO D-SCHED(1)
                COMPUTE NUM-DATE = LN-NOREGPY + 1
                MOVE NUM-DATE TO DISP-NO SCHED-NO(2)
                MOVE "@" TO DISP-AT
                MOVE LN-REGPYAMT TO DISP-AMT SCHED-AMT(2)
                MOVE DISPLAY-WORKER TO D-SCHED(2)
              ELSE
      * 1ST DIFF, REG DIFF, LAST DIFF
                MOVE 1 TO DISP-NO SCHED-NO(1)
                MOVE "@" TO DISP-AT
                MOVE LN-1STPYAMT TO DISP-AMT SCHED-AMT(1)
                MOVE DISPLAY-WORKER TO D-SCHED(1)
                MOVE LN-NOREGPY TO DISP-NO SCHED-NO(2)
                MOVE "@" TO DISP-AT
                MOVE LN-REGPYAMT TO DISP-AMT SCHED-AMT(2)
                MOVE DISPLAY-WORKER TO D-SCHED(2)
                MOVE 1 TO DISP-NO SCHED-NO(3)
                MOVE "@" TO DISP-AT
                MOVE LN-LASTPYAMT TO DISP-AMT SCHED-AMT(3)
                MOVE DISPLAY-WORKER TO D-SCHED(3).

           GO TO EXIT-DISPLAY-SCHEDULE.

       DISPLAY-PAYMENT-SCHEDULE.
           PERFORM GET-LT-P-REC.
           IF IO-FG NOT = 0
              MOVE "PAYMENT SCHEDULE MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE 1 TO SUB.

       NEXT-DISPLAY-PAYMENT-SCHEDULE.
           IF LTP-SCHLD-TERM(SUB) NOT = 0
              MOVE LTP-SCHLD-TERM(SUB) TO DISP-NO SCHED-NO(SUB)
              MOVE "@" TO DISP-AT
              MOVE LTP-SCHLD-PAYMNT(SUB) TO DISP-AMT SCHED-AMT(SUB)
              MOVE DISPLAY-WORKER TO D-SCHED(SUB)
              ADD 1 TO SUB
              IF SUB NOT > 8
                 GO TO NEXT-DISPLAY-PAYMENT-SCHEDULE.


       EXIT-DISPLAY-SCHEDULE.
           EXIT.

      *****************************
       DISPLAY-PAID SECTION.
      *****************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE 0 TO PAID-NO(SUB) PAID-AMT(SUB)
           END-PERFORM.

      * NO FULL PAYMENTS MADE
           IF PDTH-PAYS NOT > 0
              GO TO EXIT-DISPLAY-PAID.

           IF LN-PAY-SCHLD-FG = "Y"
              GO TO DISPLAY-PAID-SCHEDULE.

           IF LN-REGPYAMT = LN-1STPYAMT
              IF LN-REGPYAMT = LN-LASTPYAMT
      * REG = 1ST = LAST
                 MOVE PDTH-PAYS TO DISP-NO PAID-NO(1)
                 MOVE LN-REGPYAMT TO DISP-AMT PAID-AMT(1)
                 MOVE "@" TO DISP-AT
                 MOVE DISPLAY-WORKER TO D-PAID(1)
                 GO TO EXIT-DISPLAY-PAID
              ELSE
      * REG = 1ST, DIFF LAST
                MOVE "@" TO DISP-AT
                MOVE LN-REGPYAMT TO DISP-AMT PAID-AMT(1)
                IF PDTH-PAYS > (LN-NOREGPY + 1)
                   COMPUTE NUM-DATE = LN-NOREGPY + 1
                   MOVE NUM-DATE TO DISP-NO PAID-NO(1)
                ELSE
                   MOVE PDTH-PAYS TO DISP-NO PAID-NO(1)
                END-IF
                MOVE DISPLAY-WORKER TO D-PAID(1)
                COMPUTE WORK-AMT = PDTH-PAYMENTS -
                    LN-1STPYAMT - (LN-REGPYAMT * LN-NOREGPY)
                IF WORK-AMT > 0
                   MOVE 1 TO DISP-NO PAID-NO(2)
                   MOVE "@" TO DISP-AT
                   MOVE WORK-AMT TO DISP-AMT PAID-AMT(2)
                   MOVE DISPLAY-WORKER TO D-PAID(2)
                   GO TO EXIT-DISPLAY-PAID
                ELSE
                   GO TO EXIT-DISPLAY-PAID
            ELSE
      * 1ST DIFF, REG = LAST
              IF LN-REGPYAMT = LN-LASTPYAMT
                MOVE 1 TO DISP-NO PAID-NO(1)
                MOVE "@" TO DISP-AT
                MOVE LN-1STPYAMT TO DISP-AMT PAID-AMT(1)
                MOVE DISPLAY-WORKER TO D-PAID(1)
                COMPUTE NUM-DATE = PDTH-PAYS - 1
                IF NUM-DATE > 0
                   MOVE NUM-DATE TO DISP-NO PAID-NO(2)
                   MOVE "@" TO DISP-AT
                   MOVE LN-REGPYAMT TO DISP-AMT PAID-AMT(2)
                   MOVE DISPLAY-WORKER TO D-PAID(2)
                   GO TO EXIT-DISPLAY-PAID
                ELSE
                   GO TO EXIT-DISPLAY-PAID
              ELSE
      * 1ST DIFF, REG DIFF, LAST DIFF
                MOVE 1 TO DISP-NO PAID-NO(1)
                MOVE "@" TO DISP-AT
                MOVE LN-1STPYAMT TO DISP-AMT PAID-AMT(1)
                MOVE DISPLAY-WORKER TO D-PAID(1)
                IF PDTH-PAYS > 1
      *            MOVE LN-NOREGPY TO DISP-NO PAID-NO(2)
                   COMPUTE NUM-DATE = PDTH-PAYS - 1
                   IF NUM-DATE < LN-NOREGPY
                        MOVE NUM-DATE TO DISP-NO PAID-NO(2)
                   ELSE
                        MOVE LN-NOREGPY TO DISP-NO PAID-NO(2)
                   END-IF
                   MOVE "@" TO DISP-AT
                   MOVE LN-REGPYAMT TO DISP-AMT PAID-AMT(2)
                   MOVE DISPLAY-WORKER TO D-PAID(2)
                   IF PDTH-PAYS > (LN-NOREGPY + 1)
                      MOVE 1 TO DISP-NO PAID-NO(3)
                      MOVE "@" TO DISP-AT
                      MOVE LN-LASTPYAMT TO DISP-AMT PAID-AMT(3)
                      MOVE DISPLAY-WORKER TO D-PAID(3).

           GO TO EXIT-DISPLAY-PAID.

       DISPLAY-PAID-SCHEDULE.

           MOVE 0 TO CPN-INCR.
           MOVE 0 TO SUB.
       DISPLAY-NEXT-PAID.
           ADD 1 TO CPN-INCR.
           IF T-DATE-DUE(CPN-INCR) = 0
              GO TO EXIT-DISPLAY-PAID.

           IF PDTH-DATE-FULL NOT < T-DATE-DUE(CPN-INCR)
              IF SUB = 0
                 MOVE 1 TO SUB
                 MOVE T-AMT-DUE(CPN-INCR) TO PAID-AMT(SUB)
                 MOVE 1 TO PAID-NO(SUB)
              ELSE
                 IF T-AMT-DUE(CPN-INCR) = PAID-AMT(SUB)
                    ADD 1 TO PAID-NO(SUB)
                 ELSE
                    MOVE PAID-AMT(SUB) TO DISP-AMT
                    MOVE PAID-NO(SUB) TO DISP-NO
                    MOVE "@" TO DISP-AT
                    MOVE DISPLAY-WORKER TO D-PAID(SUB)
                    ADD 1 TO SUB
                    IF SUB > 8
                       GO TO EXIT-DISPLAY-PAID
                    ELSE
                       MOVE T-AMT-DUE(CPN-INCR) TO PAID-AMT(SUB)
                       MOVE 1 TO PAID-NO(SUB)
      * PAID THRU IS NOW LESS THAN THE TABLE DATE
              ELSE
                 IF PAID-AMT(SUB) NOT = 0
                    MOVE PAID-AMT(SUB) TO DISP-AMT
                    MOVE PAID-NO(SUB) TO DISP-NO
                    MOVE "@" TO DISP-AT
                    MOVE DISPLAY-WORKER TO D-PAID(SUB)
                    GO TO EXIT-DISPLAY-PAID
                 ELSE
                    GO TO EXIT-DISPLAY-PAID.


              GO TO DISPLAY-NEXT-PAID.

       EXIT-DISPLAY-PAID.
           EXIT.

      ***************************
       DISPLAY-REMAIN SECTION.
      ***************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE 0 TO REMAIN-NO(SUB) REMAIN-AMT(SUB)
           END-PERFORM.

      * LOAN IS PAIDOUT
           IF LN-CURBAL = 0 AND LN-POCD NOT = SPACES
              GO TO EXIT-DISPLAY-REMAIN.

      * IF NO PAYMENTS MADE, DISPLAY THE CURRENT PAYMENT SCHEDULE
      * ALREADY BUILT
           IF PDTH-PAYMENTS = 0
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                 MOVE SCHED-NO(SUB) TO DISP-NO
                 MOVE SCHED-AMT(SUB) TO DISP-AMT
                 IF SCHED-AMT(SUB) NOT = 0
                    MOVE "@" TO DISP-AT
                    MOVE DISPLAY-WORKER TO D-REMAIN(SUB)
                 END-IF
              END-PERFORM
              GO TO EXIT-DISPLAY-REMAIN.

      * HERE WOULD BE PAYMENTS MADE...NEED TO FIND WHAT IS REALLY
      * REMAINING..

           MOVE 1 TO SUB.
       FIND-FIRST-UNPAID.
           IF SCHED-NO(SUB) = PAID-NO(SUB)
              ADD 1 TO SUB
              IF SUB NOT > 8
                 GO TO FIND-FIRST-UNPAID
              ELSE
                 GO TO EXIT-DISPLAY-REMAIN.

      * ALL FULL PAYMENTS HAVE BEEN MADE
            IF PDTH-REMAIN = 0
               COMPUTE REMAIN-NO(1) = SCHED-NO(SUB) - PAID-NO(SUB)
               MOVE REMAIN-NO(1) TO DISP-NO
               MOVE "@" TO DISP-AT
               MOVE SCHED-AMT(SUB) TO REMAIN-AMT(1) DISP-AMT
               MOVE DISPLAY-WORKER TO D-REMAIN(1)
               MOVE 2 TO REMAIN-SUB
            ELSE
      * HAVE A PARTIAL LEFT
               COMPUTE REMAIN-AMT(1) ROUNDED = SCHED-AMT(SUB) -
                                     (SCHED-AMT(SUB) * PDTH-REMAIN)
               MOVE REMAIN-AMT(1) TO DISP-AMT
               MOVE "@" TO DISP-AT
               MOVE 1 TO REMAIN-NO(1) DISP-NO
               MOVE DISPLAY-WORKER TO D-REMAIN(1)
               COMPUTE REMAIN-NO(2) = SCHED-NO(SUB) - PAID-NO(SUB)
                                    - 1
               MOVE 2 TO REMAIN-SUB
               IF REMAIN-NO(2) > 0
                  MOVE REMAIN-NO(2) TO DISP-NO
                  MOVE "@" TO DISP-AT
                  MOVE SCHED-AMT(SUB) TO REMAIN-AMT(2) DISP-AMT
                  MOVE DISPLAY-WORKER TO D-REMAIN(2)
                  MOVE 3 TO REMAIN-SUB.

           ADD 1 TO SUB.
       FINISH-REMAINING.

           IF SUB NOT > 8
            IF SCHED-NO(SUB) NOT = 0
              MOVE SCHED-NO(SUB) TO DISP-NO REMAIN-NO(REMAIN-SUB)
              MOVE SCHED-AMT(SUB) TO DISP-AMT REMAIN-AMT(REMAIN-SUB)
              MOVE "@" TO DISP-AT
              MOVE DISPLAY-WORKER TO D-REMAIN(REMAIN-SUB)
              ADD 1 TO SUB REMAIN-SUB
              IF REMAIN-SUB NOT > 8
                 IF SUB NOT > 8
                    GO TO FINISH-REMAINING.

           IF REMAIN-NO(1) = 1 AND REMAIN-NO(2) < 1
              IF REMAIN-AMT(1) > NO-TDUEPOFF
                 MOVE NO-TDUEPOFF TO REMAIN-AMT(1) DISP-AMT D-TO-ROLL
                 MOVE "@" TO DISP-AT
                 MOVE 1 TO REMAIN-NO(1) DISP-NO
                 MOVE DISPLAY-WORKER TO D-REMAIN(1).

       EXIT-DISPLAY-REMAIN.
           EXIT.

      *****************************
       GET-COLLATERAL-INFO SECTION.
      *****************************

           MOVE SPACES TO D-COLLATERAL.

           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 0
              IF LTC-MAKE(1) NOT = SPACES OR LTC-VIN(1) NOT = SPACES
                STRING LTC-YEAR(1)," ",LTC-MAKE(1)," ",LTC-MODEL(1),
                " ", LTC-VIN(1) DELIMITED BY "   " INTO D-COLLATERAL
              ELSE
                STRING LTC-YEAR(2)," ",LTC-MAKE(2)," ",LTC-MODEL(2),
               " ", LTC-VIN(2) DELIMITED BY "   " INTO D-COLLATERAL.

       EXIT-GET-COLLATERAL.
           PERFORM CLOSE-LTC1-FILE.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      *******************************************
       FORM-RESET SECTION.
      *******************************************
           DISPLAY SPACES UPON COLAST-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY COLAST-SCREEN UPON COLAST-WINDOW-HANDLE.
           MOVE EXT-CONAME-SYSDATE TO COLAST-SYSDATE.
           MOVE COLAST-FORM-FIELDS TO WR-BUF.
           MOVE COLAST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *******************************************
       SETUP-LINK-INFO SECTION.
      *******************************************
           MOVE COLAST-HELP--FILE TO HELP-LINK-FILE.
           MOVE COLAST-HELP--DIR TO HELP-LINK-DIR.

      *******************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/COLAST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      ***********************************
       IO-ROUTINES SECTION.
      ***********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBX1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY COLAST-SCREEN.
           DESTROY COLAST-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
