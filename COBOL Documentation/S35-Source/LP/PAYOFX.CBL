      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PAYOFX.
       DATE-WRITTEN.    040987.
      ******************************************************************
      *  DESC:  CALCULATES PRIOR LOAN PAYOFF WITHOUT UPDATING
      *
      *  PROGRAMS : CL/GASC12   CL/GASC13
      *             LP/LONPF1   LP/LONPFC  LP/LONPB0
      *             LP/XONPC0   LP/VONPB0  LP/ZONPC0
      *             LP/LONPF9   LP/VONPC0  LP/LONPFG
      *             WI/BLQUOT   WI/CLQUOT  WI/LNQUOT
      *             WI/SFQUOT
      *
      *     INPUT:
      *           PASS-PRLNNO              LN-ACCTNO
      *           PASS-TRANS-DATE          PAYOFF DATE
      *           PASS-CURRENT-PD-SRCD     SOURCE CODE (PB/RN/SC)
      *
      * REVS:
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 120296 CHANGED TO STILL FLAG THE LOAN AS BEING A SERVICE
      *             ACCOUNT (PASS-STAT-FG = S) (FOR QUOTERS), BUT ALLOW
      *             PAYOFF TO COMPUTE (FOR LONPFC.C)
      *  BAH 120696 ADDED PASS-SSNO FOR XONPC0 AUTO RENEWALS, NEEDED
      *            TO TEST IF MAKERS ARE DIFFERENT...
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  JTG 060397 ADDED LOGIC FOR 'O3' REBATE
      *  BAH 030619 ADDED PASS-PRIOR-SUB-CLASS, LENDMARK PR# 2101
      *  BAH 060627 ADD MIP-CLPAID AND AHPAID TO PASS-PRBAL, REGENCY PR# 538
      *  BAH 130131 A30 CONVERSION
      *  MJD 130604 ADDED LOGIC FOR SP-SCFRMLA = 54.  FUNCTIONS THE SAME AS 12
      *             BUT INCLUDES AN EXTRA 4% ADDED IN AT THE END BASED
      *             ON TOTAL NOTE                                    WORLD #0874
      *  BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      ******************************************************************
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
           "@(#)LP/PAYOFX.CBL S34 05/21/21-08:54:06 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".


       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
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
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/PAYOFXW.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
             USING FORM-PATHNAME PASS-PAYOFF-FLDS EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM GET-GPENV.

           MOVE " " TO PASS-PRIOR-LOAN-STATE PASS-STAT-FG.
           MOVE 0 TO PASS-PRIOR-LOAN-CLASS PASS-PRIOR-LOAN-DATE
                     PASS-PRBAL PASS-PRINTDUE PASS-PRFINCHGRB
                     PASS-PRINSRB PASS-INT-REB PASS-SC-REB
                     PASS-MF-REB PASS-NETDUE PASS-CDEL-ARREARAGE
                     PASS-SSNO PASS-PRIOR-SUB-CLASS.

       MAIN-INIT-SCREEN.
           MOVE 0 TO IO-FG.
           MOVE "Y" TO PASS-STAT-FG.
           MOVE SPACES TO ERRCD ROUTE-CD.

           MOVE PASS-PRLNNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF ( IO-FG = 9 ) OR ( LN-POCD-NONLOAN ) OR (LN-POCD-PB-RESC)
              MOVE "L" TO PASS-STAT-FG
              PERFORM CLOSE-LN1-FILE
              GO TO END-ROUTINE.
           PERFORM CLOSE-LN1-FILE.

           MOVE LN-SSNO(1) TO PASS-SSNO.

           IF LN-PLDATE NOT = 0
              MOVE "Z" TO PASS-STAT-FG.

           IF LN-CURBAL = 0
              MOVE "Z" TO PASS-STAT-FG
              GO TO END-ROUTINE.

           IF PASS-CURRENT-PD-SRCD = "SC"
              IF LN-SALEFIN NOT = "Y"
                 MOVE "S" TO PASS-STAT-FG
                 GO TO END-ROUTINE
              ELSE
                 IF LN-SERVACCT NOT = "N"
                    MOVE "S" TO PASS-STAT-FG.

           IF PASS-CURRENT-PD-SRCD NOT = "SC"
                        AND LN-SALEFIN = "Y"
              MOVE "N" TO PASS-STAT-FG
              GO TO END-ROUTINE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "S" TO PASS-STAT-FG
              GO TO END-ROUTINE.
           PERFORM CLOSE-BR-FILE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "S" TO PASS-STAT-FG
              GO TO END-ROUTINE.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "S" TO PASS-STAT-FG
              GO TO END-ROUTINE.

           MOVE 0 TO INDU-INTEREST LCAP-APP LCAP-OWE.
           MOVE LN-LCPDTH-DATE TO LCAP-LCPDTH-DATE.

           PERFORM PAYOFF-ROUTINE.

           MOVE LN-ORGST TO PASS-PRIOR-LOAN-STATE.
           MOVE LN-CLASS TO PASS-PRIOR-LOAN-CLASS.
           MOVE LN-SUBCLASS TO PASS-PRIOR-SUB-CLASS.
           MOVE LN-LOANDATE TO PASS-PRIOR-LOAN-DATE.

           ADD LN-CURBAL LN-OTHBAL GIVING PASS-PRBAL.
           IF LN-MIP-FG = "Y"
              ADD POFF-LXM-MIP-CLPAID TO PASS-PRBAL
              ADD POFF-LXM-MIP-AHPAID TO PASS-PRBAL.
           ADD POFF-INTDUE POFF-LCDUE GIVING PASS-PRINTDUE.
           MOVE POFF-REBATE(4) TO PASS-INT-REB.
           MOVE POFF-REBATE(5) TO PASS-SC-REB.
           MOVE POFF-REBATE(6) TO PASS-MF-REB.
           COMPUTE PASS-PRFINCHGRB = POFF-REBATE(4) + POFF-REBATE(5)
                                   + POFF-REBATE(6) + POFF-REBATE(7)
                                   + POFF-REBATE(8).
           COMPUTE PASS-PRINSRB = POFF-REBATE(1)  + POFF-REBATE(2)
                                + POFF-REBATE(3)  + POFF-REBATE(9)
                                + POFF-REBATE(10) + POFF-REBATE(11)
                                + POFF-REBATE(12) + POFF-REBATE(13).
           MOVE POFF-NETDUE TO PASS-NETDUE.

      * TEST FOR GEORGIA SC REQUIREMENT FOR PAID AHEAD MONIES:
           IF SP-SCFRMLA = 12 OR 13 OR 54
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE PASS-TRANS-DATE TO CEPP2-DATE
              MOVE 1 TO CEPP-MAKERCD
              PERFORM TOTAL-DELINQUENCY
              MOVE CDEL-ARREARAGE TO PASS-CDEL-ARREARAGE
           ELSE
              MOVE 0 TO PASS-CDEL-ARREARAGE.
           GO TO END-ROUTINE.

      ************************************************
       PAYOFF-ROUTINE SECTION.
      ************************************************
           MOVE PASS-TRANS-DATE TO POFF-PAYDATE.
           MOVE PASS-CURRENT-PD-SRCD TO POFF-LPTRCD.
           MOVE "M" TO POFF-MAKERCD.
           PERFORM PAYOFF-LOAN-ROUTINE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/SCREEN.CPY".
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
      *COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPAMTS.CPY".
      *COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

      ********************************************
       MISC-ROUTINES SECTION.
      ********************************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
           MOVE "00" TO STAT.

      ********************************************
      *     I N P U T  /  O U T P U T            *
      ********************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
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
