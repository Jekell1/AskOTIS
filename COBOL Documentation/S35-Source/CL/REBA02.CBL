       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBA02.
       DATE-WRITTEN.   01/04/00.
      *************************************************************************
      *                      (CL)
      *    DESCRIPTION:  CALCULATE PREPAYMENT PENALTY WISCONSIN
      *                  CALLED FROM LPINDU ROUTINE
      *                   IF RBREDUC = 9999.04
      *
      * WORLD DOES NOT CURRENTLY USE THIS PROGRAM/FORMULA  5/17/2023
      *
      *
      * REV:
      *  JTG 000104 ADDED SP-RBREDUC = 9999.04 WISCONSIN IB REAL ESTATE
      *             PREPAY PENALTY, WISCONSIN #1031
      *  JTG 000126 CHANGED TO NOT TEST REAL ESTATE SECURED CODE WISC #1031
      *  JTG 011026 CHANGED RBACT-APRATE TO RBACT-RATE FOR NEW REFUND FORMULA
      *             '0  6'                                      REGENCY PR#1631
      *************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/REBA02.CBL S34 05/24/21-15:16:10 >".

       01  ERRCD                     PIC X VALUE SPACES.
       01  RATE-DAYS                 PIC 999.

       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPERW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  INDU-COMMON.
           03  INDU-COMMON-RTNCD       PIC X.
           03  INDU-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  INDU-COMMON-AREA        PIC X(40).
      * USED FOR WISCONSIN REDUCTION CODE:
           03  INDU-A02               REDEFINES INDU-COMMON-AREA.
               05  INDU-A02-PENALTY   PIC S9(7)V99 COMP-3.
               05  INDU-A02-PAYDATE   PIC 9(8)     COMP-3.
               05  FILLER             PIC X(30).
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME INDU-COMMON LN-REC SP-REC EXIT-PATHNAME.

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
       MAIN-ROUTINE.
           MOVE " " TO INDU-COMMON-RTNCD.
           MOVE 0   TO INDU-A02-PENALTY.

      * COMPUTE THE PRESENT VALUE OF THE ACCOUNT:
           MOVE LN-ORIG-1STPYDATE TO RBACT-ORIG-1STPAY.
           MOVE LN-INTDATE        TO RBACT-INTDATE.
           MOVE LN-SMPRATE        TO RBACT-RATE.
           MOVE LN-TOTNODEF       TO RBACT-TOTNODEF.
           MOVE LN-ORGTERM        TO RBACT-ORGTERM.
           MOVE LN-1STPYAMT       TO RBACT-1STPYAMT.
           MOVE LN-REGPYAMT       TO RBACT-REGPYAMT.
           MOVE LN-LASTPYAMT      TO RBACT-LASTPYAMT.
           MOVE LN-UNITPER-CD     TO RBACT-UNITPER-CD.
           MOVE LN-UNITPER-FREQ   TO RBACT-UNITPER-FREQ.
           MOVE INDU-A02-PAYDATE  TO RBACT-RBDATE.
           MOVE 0                 TO RBACT-ELAPSED-MONTHS
                                     RBACT-ELAPSED-REM
                                     RBACT-ELAPSED-DAYS.
           MOVE " "               TO RBACT-SKIPDEF-FG
                                     RBACT-EARN-FULL-MONTH.
           MOVE 7                 TO RBACT-SUB.
           MOVE 1                 TO RBACT-SUB2.
           MOVE "Y"               TO RBACT-PENALTY-FG.
           ADD LN-INTCHG LN-EXTCHG GIVING RBACT-TOTCHG.

           PERFORM RBACT-ACTUARIAL.

           IF SP-INTMETHOD-EXACT
              MOVE 365 TO RATE-DAYS
           ELSE
              MOVE 360 TO RATE-DAYS.
           IF RBACT-PRES-VALUE >= LN-CURBAL
              COMPUTE INDU-A02-PENALTY ROUNDED =
                 (LN-LNAMT - LN-LNAMT * 0.20)
                                  * LN-SMPRATE / 36500 * 60
           ELSE
              COMPUTE INDU-A02-PENALTY ROUNDED =
                 (RBACT-PRES-VALUE - LN-LNAMT * 0.20)
                                  * LN-SMPRATE / 36500 * 60.

           GO TO END-PROGRAM.

       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".

       MISC-ROUTINES SECTION.

      ************************************************
       IO-ROUTINE SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM SECTION.
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
