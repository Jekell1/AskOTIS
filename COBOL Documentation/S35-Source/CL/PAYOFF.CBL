      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PAYOFF.
       DATE-WRITTEN.    101692.
      **************************************************************************
      *  DESC:  CALL PROGRAM FOR A LOAN'S PAYOFF
      *
      *         NOTE:
      *              MAKE THE CALL ANYWHERE EXCEPT FROM LONPFA.C
      *
      *    IN:  PAYOFF-ACCTNO
      *         PAYOFF-PAYDATE
      *         PAYOFF-SRCD
      *         PAYOFF-MAKERCD
      *         PAYOFF-DEF-STOP
      *
      *   OUT:  PAYOFF-CDEL-ARREARAGE
      *         PAYOFF-AVAILCREDIT
      *         PAYOFF-STAT-FG
      *            - PRIOR-LOAN-NOT-POSTED  VALUE "Y"
      *            - ERROR-IN-PAYOFF        VALUE "L" "S"
      *            - UNUSED STATUS FLAG     VALUE "J"
      *            - PRIOR-LOAN-NOT-VALID   VALUE "Z" "S" "N" "J"
      *
      *         ALL LPPOFFW OUTPUT FIELDS:
      *            POFF-REBATE(1)          CL REBATE
      *                       (2)          AH REBATE
      *                       (3)          PP REBATE
      *                       (4)          INT REBATE
      *                       (5)          SRV REBATE
      *                       (6)          FEE REBATE
      *                       (7)          DEF REBATE
      *                       (8)          N/A REBATE
      *                       (9)          OT1 REBATE
      *                       (10)         OT2 REBATE
      *                       (11)         OT3 REBATE
      *                       ----
      *                       (12)         OT4 REBATE
      *                       (13)         OT5 REBATE
      *            POFF-OTDUE              OTHER CHARGES DUE
      *            POFF-O2DUE              OTHER 2 CHARGES DUE
      *            POFF-LCDUE              LATE CHARGES DUE
      *            POFF-INTDUE             INTEREST DUE
      *            POFF-NETDUE             NET PAYOFF AMOUNT DUE
      *            POFF-PARTIALS           (FROM LPLCAP)
      *            POFF-LCPAID             (FROM LPLCAP)
      * REVS:
      *  JTG 061695 ADDED READ OF BRANCH FILE
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  JTG 061998 UPDATED DOCUMENTATION ABOVE WITH NEW STATUS CODE 'J'
      *             WHICH WILL BE TESTED IN LOAN POSTING PROGRAMS
      *  JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *             INTO COMMENTS ONLY
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 161102 ADDED READ-BW1-FILE FOR CHANGE TO LPAVCR WORLD PR1199
      *             SC AVAILABLE CREDIT RESTRICTION
      *  BAH 170405 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.07.02 ADDED ADDONSPRW
      * BAH 2019.03.01 REMOVE CLOSE-SP1-FILE, LEAVE OPEN BECAUSE ADDONSPR READS
      *                IT
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
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
           "@(#)CL/PAYOFF.CBL S34 05/21/21-08:17:05 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".


       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/PAYOFFW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
             USING FORM-PATHNAME PAYOFF-WORKERS POFF-WORKERS
                   EXIT-PATHNAME.

      /*************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM GET-GPENV.

           MOVE SPACES TO ERRCD ROUTE-CD
                          PAYOFF-STAT-FG.
           MOVE 0 TO PAYOFF-CDEL-ARREARAGE
                     PAYOFF-AVAILCREDIT.

           MOVE PAYOFF-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF ( IO-BAD ) OR ( LN-POCD-NONLOAN )
              MOVE "L" TO PAYOFF-STAT-FG
              PERFORM CLOSE-LN1-FILE
              GO TO END-ROUTINE.
           PERFORM CLOSE-LN1-FILE.

           IF LN-CURBAL = 0
              MOVE "Z" TO PAYOFF-STAT-FG
              GO TO END-ROUTINE.
           IF PAYOFF-SRCD = "SC"
              IF LN-SALEFIN NOT = "Y"
                         OR LN-SERVACCT NOT = "N"
                 MOVE "S" TO PAYOFF-STAT-FG
                 GO TO END-ROUTINE.
           IF PAYOFF-SRCD NOT = "SC"
                            AND LN-SALEFIN = "Y"
              MOVE "N" TO PAYOFF-STAT-FG
              GO TO END-ROUTINE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-BAD   
              MOVE "S" TO PAYOFF-STAT-FG
              GO TO END-ROUTINE.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "S" TO PAYOFF-STAT-FG
              GO TO END-ROUTINE.
           PERFORM CLOSE-BR-FILE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-BAD
              MOVE "S" TO PAYOFF-STAT-FG
              GO TO END-ROUTINE.
           PERFORM CLOSE-GB-FILE.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-BAD
              MOVE "S" TO PAYOFF-STAT-FG
              GO TO END-ROUTINE.

           MOVE PAYOFF-PAYDATE       TO AVCR-PAYDATE.
           MOVE PAYOFF-MAKERCD       TO AVCR-MAKERCD.
           MOVE PAYOFF-SRCD          TO AVCR-LPTRCD.
           MOVE PAYOFF-DEF-STOP TO AVCR-DEF-STOP.
           PERFORM COMPUTE-AVAILABLE-CREDIT.
           MOVE AVCR-AVAILCREDIT TO PAYOFF-AVAILCREDIT.

      * TEST FOR GEORGIA SC REQUIREMENT FOR PAID AHEAD MONIES:
           MOVE LN-1STPYDATE   TO CEPP1-DATE.
           MOVE PAYOFF-PAYDATE TO CEPP2-DATE.
           MOVE 1              TO CEPP-MAKERCD.
           PERFORM TOTAL-DELINQUENCY.
           MOVE CDEL-ARREARAGE TO PAYOFF-CDEL-ARREARAGE.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GPENV.CPY".

      /*******************************************
      *     I N P U T  /  O U T P U T            *
      ********************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
