       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBC01.
       DATE-WRITTEN.   12/03/90.
      **************************************************************************
      *                      (CL)
      *    DESCRIPTION:  I/B RECASTING.
      *                  CALLED FROM REBATE ROUTINE
      * SP-RBRECAST = 1
      *REV:
      * GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      * JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90
      * JTG 010598 ADDED TEST FOR "ST" NEW ADDON TRAN CODE
      *  CS 042798 ADDED TEST FOR "RB" AND "RO" (RENEWALS)
      * GLF 330999 ADDED TRCD'S  "PN" AND "PZ" (TFC)
      * MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      * BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *            LIKE "PE", REGACC PR# 0007
      * JTG 021025 ALLOW GP-I-AM-WORLD TO ENTER 999.998 IN SP-ACC-RATE(1)
      *            AND IF DETECTED HERE SKIP USAGE OF ACCELERATION RATE
      *            TABLE AND FORCE LN-SMPRATE RE: LN-ACCRATE 999.998 WORLD #306
      *   CS 151120 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      * BAH 181219 GLOBAL LPFILE
      * BAH 20220126 SET FILPATH FIELDS FOR LP1-FILE
      * BAH 20220201 CHANGED THESE 3 FIELDS TO BE S9(7)V99, NOT S9(7)99
      *              WORKER, WK-INT-OWED, SAVE-CURBAL
      * BAH 20220803 HARD CODED START
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/REBC01.CBL S35 02/26/24-09:36:08 >".

       01  ERRCD                PIC X VALUE " ".
       01  WORKER               PIC S9(7)V99   COMP-3 VALUE 0.
       01  WK-INT-OWED          PIC S9(7)V99   COMP-3 VALUE 0.
       01  SAVE-PAYDATE         PIC 9(8)      COMP-3 VALUE 0.

       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LP01LTI.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNCD       PIC X.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA        PIC X(40).
           03  FILLER REDEFINES REB-COMMON-AREA.
               05  REB-COMMON-PAYDATE PIC 9(8).
               05  FILLER             PIC X(32).
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME REB-COMMON LN-REC SP-REC EXIT-PATHNAME.

      ********************************************
       MAIN-MODULE SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE "MISSING LOAN BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO REB-COMMON-RTNCD
              GO TO EXIT-PROG.

       MAIN-ROUTINE.
           MOVE REB-COMMON-PAYDATE TO SAVE-PAYDATE.
           MOVE SPACES    TO REB-COMMON-RTNCD.
           MOVE LN-INTCHG TO REB-COMMON-REBATE.
           MOVE LN-LOANDATE TO INDU-DATE-1.

           PERFORM LOAN-CALCULATIONS.

           MOVE INT-CHARGEABLE TO INDU-CURBAL.
           MOVE LN-APRATE      TO INDU-RATE.
           MOVE "PY"           TO INDU-LPTRCD.
           MOVE SP-YEARTYPE    TO INDU-YEARTYPE.
           MOVE SP-INTMETHOD   TO INDU-INTMETHOD.

           MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BR-MACHINE        TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH      TO FILPATH-DATA.
           MOVE "B"               TO LP-PATH-OVERRIDE.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.
       NEXT-LP-REC.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
               GO TO END-PROGRAM.

           IF LP-REV = "Y" OR LP-TRCD = "RV"
              GO TO NEXT-LP-REC.

           MOVE 0 TO INDU-INTEREST.
           IF LP-TRCD = "PY" OR "PA" OR "PO" OR "PE" OR "SS"
                              OR "PR" OR "PC" OR "PP"
                               OR "RN" OR "SC" OR "PB"
                                OR "RB" OR "RO" OR "IN"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "AI" OR "CI" OR "IR"
                                     OR "AH" OR "PN" OR "PZ"
              MOVE LP-PAYDATE TO INDU-DATE-2
              PERFORM INTEREST-DUE-CALCULATION
              COMPUTE WORKER = LP-TRAMT - LP-APOTH - WK-INT-OWED
              IF INDU-INTEREST < WORKER
                 COMPUTE INDU-CURBAL = INDU-CURBAL -
                                      (LP-TRAMT - LP-APOTH) +
                                       INDU-INTEREST + WK-INT-OWED
                 MOVE 0 TO WK-INT-OWED
                 MOVE LP-PAYDATE TO INDU-DATE-1
              ELSE
                 COMPUTE WK-INT-OWED = WK-INT-OWED +
                                      (INDU-INTEREST -
                                      (LP-TRAMT - LP-APOTH))
                 MOVE LP-PAYDATE TO INDU-DATE-1
           ELSE
            IF NOT (LP-TRCD = "ET" OR "RT" OR "NP" OR "LP" OR "ST")
               SUBTRACT LP-APCUR FROM INDU-CURBAL
            ELSE
               MOVE "CAN NOT RECAST ADDON LOANS" TO MESS
               PERFORM SEND-MESS
               MOVE "X" TO REB-COMMON-RTNCD
               GO TO EXIT-PROG.

           COMPUTE REB-COMMON-REBATE = REB-COMMON-REBATE -
                                       INDU-INTEREST.

           GO TO NEXT-LP-REC.

       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".

      **********************************************
       IO-ROUTINE SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       END-PROGRAM SECTION.
       ENDIT.
           MOVE SAVE-PAYDATE TO INDU-DATE-2.
           COMPUTE INDU-CURBAL = INDU-CURBAL - (LP-TRAMT - LP-APOTH).
           PERFORM INTEREST-DUE-CALCULATION.
           COMPUTE REB-COMMON-REBATE = REB-COMMON-REBATE -
                                       INDU-INTEREST.
           IF REB-COMMON-REBATE < 0
              MOVE 0 TO REB-COMMON-REBATE.
       EXIT-PROG.
           PERFORM CLOSE-FILES.
       STOP-RUN.
           EXIT PROGRAM.
