       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CASC16.
       DATE-WRITTEN.    020795.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 CALIFORNIA, COSMO  (SCFRMLA 16)
      *                 CALIFORNIA, MULLEN (SCFRMLA 4)
      *                 CALIFORNIA, MULLEN (SCFRMLA 29)
      *                 BAY COUNTRY, (SCFRMLA 47)
      *                 TEXAS, REGMGM (SCFRMLA 49)
      *                 CALIFORNIA, MULLEN (SCFRMLA 52, LIKE 4)
      *                 CALIFORNIA, MULLEN (SCFRMLA 53, LIKE 29)
      *                 KENTUCKY, WORLD (SCFRMLA 59)
      *
      * SCFRMLA 59 TESTS FOR 90 DAYS
      *  RENEWALS CAN ONLY BE COLLECTED IF THE RENEWAL HAPPENS AFTER
      *  12 MONTHS SINCE "LAST COLLECTION" OF A SERCHG
      *     ACCT # 132903 LOANDATE  2-7-95  SERCHG 50$
      *             RENEW LOANDATE 12-10-95 NO SERCHG TAKEN
      *       AGAIN RENEW LOANDATE  2-7-96  SERCHG 50$ CAN BE TAKEN
      * 10 MONTHS PASS, RENEW, NO SERCHG CAN BE TAKEN
      *  2 MORE MONTHS PASS, RENEW AGAIN, CAN TAKE A SERVICE CHG, ITS
      *    BEEN 12 MONTHS SINCE 1 HAS BEEN TAKEN
      *
      *                 CALLED FROM XONPC0.C AND LONPC2.C
      *
      *  REV:
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 061128 WAS ONLY CHECKING LAST 3 PRIOR LOANS, ADDED MORE
      *             GET-PRIOR-LOAN CHECKS, REGMGM PL# 561
      *  BAH 061129 CONTINUE CHECKING PRIOR LOANS UNTIL YOU HIT THE 12 MONTH
      *             BACK DATE CHECK, JIM R
      *  BAH 110127 CHANGED CASCHG-ORGST TO CASCHG-SCFRMLA PIC 99 FOR SCFRMLA
      *             49 TO CHECK RESTRICTIONS ON PRIOR LOANS, REGMGM PR# 3721
      *             SCFRMLA 49 NEEDS TO SKIP THE LOAN IF USED SCFRMLA 5
      *  BAH 110602 ADDED SCFRMLA 52 AND 53 FOR MULLEN PR  # 3799
      *  BAH 130130 A30 CONVERSION
      *  BAH 2019.0529 ADDED SCFRMLA 59 "KY" WORLD PR# 1374
      *  BAH 2019.0806 SCFRMLA 59 "KY" INCLUDE THE 90TH DAY, PR#1383
      ************************************************************
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
           "@(#)CL/CASC16.CBL S35 03/25/24-08:59:15 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  ERRCD           PIC X         VALUE SPACES.
       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/CASCHGW.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CASCHG-WORKERS EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR PRIOR LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO CASCHG-RESTRICTED.

      D    STOP MESS.
      * LOOK AT LAST PRIOR
           IF CASCHG-PRIOR-ACCTNO NOT = 0
              PERFORM OPEN-LN1-FILE
              MOVE CASCHG-PRIOR-ACCTNO TO LN-ACCTNO
              PERFORM GET-PRIOR-LOAN
              IF IO-BAD OR LN-SERCHG NOT = 0
      * IF SERCHG NOT = 0 THEN IT WAS THE "LAST ASSESSMENT" AND YOU ARE DONE
                 GO TO UPDATE-EXIT
              ELSE
      * IF SERCHG WAS 0 ON THE FIRST PRIOR, THEN YOU HAVE TO KEEP CHECKING
      * EACH LOANS PRIOR ACCOUNT TO SEE IF IT CHARGED A SERVICE CHARGE, AND
      * IF SO, WAS IT WITHIN THE 12 MONTHS (RESTRICTED)
      * IF NO MORE PRIORS, THEN YOU ARE DONE
                 IF LN-PRLNNO = 0
                    MOVE " " TO CASCHG-RESTRICTED
                    GO TO UPDATE-EXIT
                 ELSE
                    IF CASCHG-SCFRMLA = 59
                       PERFORM CHECK-ALL-PRIORS-59
                    ELSE
                       PERFORM CHECK-ALL-PRIORS.

       UPDATE-EXIT.
           GO TO END-ROUTINE.

      *************************
       CHECK-ALL-PRIORS SECTION.
      *************************
           MOVE " " TO CASCHG-RESTRICTED.
       CHECK-AGAIN.
           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO EXIT-CHECK-ALL-PRIORS.

      * SCFRMLA 49 - IF PRIOR LOANS USED SCFRMLA 5 THEY DO NOT COUNT FOR THE
      *              RESTRICTION IN 49, SKIP THEM
           IF CASCHG-SCFRMLA = 49
              PERFORM GET-PRIOR-SPR
      * IF THIS PRIOR LOAN USED SCFRMLA 5, SKIP IT, IT DOES NOT COUNT TOWARD
      * THE RESTRICTIONS FOR A SCFRMLA 49
              IF SP-SCFRMLA = 5
                 IF LN-PRLNNO NOT = 0
                    GO TO CHECK-AGAIN
                 ELSE
                    GO TO EXIT-CHECK-ALL-PRIORS.

           MOVE LN-LOANDATE     TO NUM-DATE.
           MOVE CASCHG-LOANDATE TO SYS-DATE.
           MOVE 360             TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
      * STOP CHECKING WHEN THE LOANS ARE OLDER THAN 12 MONTHS FROM NEW LOAN
           IF ELAPSED-MONTHS >= 12
              GO TO EXIT-CHECK-ALL-PRIORS.

      * FIND A SERVICE CHARGE WITHIN A 12 MONTH PERIOD, YOU ARE RESTRICTED
           IF LN-SERCHG NOT = 0
              MOVE "Y" TO CASCHG-RESTRICTED
              GO TO EXIT-CHECK-ALL-PRIORS.

           IF LN-PRLNNO NOT = 0
              GO TO CHECK-AGAIN.


       EXIT-CHECK-ALL-PRIORS.
           EXIT.

      *****************************
      * WORLD "KY" 59
      * IF REFINANCED WITHIN 90 CALENDAR DAYS (365) OF A SERCHG COLLECTED
      * ON A PRIOR LOAN MADE IN "KY", REGARDLESS OF CLASS, NO CHARGE CAN
      * BE ASSESSED.
      *****************************
       CHECK-ALL-PRIORS-59 SECTION.
      *****************************

           MOVE " " TO CASCHG-RESTRICTED.

       CHECK-AGAIN-59.
           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO EXIT-CHECK-ALL-PRIORS-59.

           MOVE LN-LOANDATE     TO NUM-DATE.
           MOVE CASCHG-LOANDATE TO SYS-DATE.
           MOVE 365             TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
      * STOP CHECKING WHEN THE LOANS ARE OLDER THAN 90 DAYS FROM NEW LOAN
           IF ELAPSED-DAYS > 90
              GO TO EXIT-CHECK-ALL-PRIORS-59.

      * FIND A SERVICE CHARGE WITHIN A 90 DAY PERIOD, YOU ARE RESTRICTED
           IF LN-SERCHG NOT = 0
              MOVE "Y" TO CASCHG-RESTRICTED
              GO TO EXIT-CHECK-ALL-PRIORS-59.

           IF LN-PRLNNO NOT = 0
              GO TO CHECK-AGAIN-59.

       EXIT-CHECK-ALL-PRIORS-59.
           EXIT.

      ************************
       GET-PRIOR-LOAN SECTION.
      ************************
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO GET-PRIOR-EXIT.

      * SCFRMLA 49 - IF PRIOR LOANS USED SCFRMLA 5 THEY DO NOT COUNT FOR THE
      *              RESTRICTION IN 49, SKIP THEM
           IF CASCHG-SCFRMLA = 49
              PERFORM GET-PRIOR-SPR
              IF SP-SCFRMLA = 5
      * IF PRIOR LOAN CHARGED A SERVICE CHARGE, NORMALLY YOU WOULD BE ABLE
      * TO STOP TESTING HERE AND REPORT BACK TO THE DRIVER THAT YOU ARE DONE
      * CAUSE YOU FOUND THE LAST SERVICE CHARGE, BUT SINCE IT IS A FRMLA 5
      * AND DOES NOT COUNT, YOU HAVE TO CHECK AGAIN FOR ANOTHER 49.
      * BY MOVING A DUMMY AMOUNT TO LN-SERCHG, THE DRIVER WILL THEN GO INTO
      * CHECKING ALL PRIOR LOANS, SORRY THIS IS SO MESSY !
                 MOVE 50.00 TO LN-SERCHG
                 GO TO GET-PRIOR-EXIT.

           IF LN-SERCHG NOT = 0
              IF CASCHG-SCFRMLA = 59
                 PERFORM TEST-SCFRMLA-59
              ELSE
                 MOVE LN-LOANDATE     TO NUM-DATE
                 MOVE CASCHG-LOANDATE TO SYS-DATE
                 MOVE 360 TO ELAPSED-YRTYPE
                 PERFORM TIMALL
                 IF ELAPSED-MONTHS NOT < 12
                    MOVE " " TO CASCHG-RESTRICTED
                 ELSE
                    MOVE "Y" TO CASCHG-RESTRICTED.

       GET-PRIOR-EXIT.
           EXIT.

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       SETUP-LINK-INFO.
       FORM-RESET.
       VDU-CONVERT-FIELD.
      * WORLD "KY" 59
      * IF REFINANCED WITHIN 90 CALENDAR DAYS (365) OF A SERCHG COLLECTED
      * ON A PRIOR LOAN MADE IN "KY", REGARDLESS OF CLASS, NO CHARGE CAN
      * BE ASSESSED.
       TEST-SCFRMLA-59.
           MOVE LN-LOANDATE     TO NUM-DATE.
           MOVE CASCHG-LOANDATE TO SYS-DATE.
           MOVE 365             TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS > 90
              MOVE " " TO CASCHG-RESTRICTED
           ELSE
              MOVE "Y" TO CASCHG-RESTRICTED.

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-PRIOR-SPR.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE 49 TO SP-SCFRMLA.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
