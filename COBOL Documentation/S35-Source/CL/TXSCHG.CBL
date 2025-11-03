       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TXSCHG.
       DATE-WRITTEN.    05/06/2016.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   TEST FOR RESTRICTION OF SERVICE CHARGE
      *
      *  CALLED FROM LNQUOT, BLQUOT, NMQUOT, XONPC0, ZONPC0
      *
      *  TEXAS, WORLD  (SCFRMLA 58)
      *         SERVICE CHARGE CAN NOT BE CHARGED IF BORROWER HAD PREVIOUS
      *         LOAN WITHIN 1 YEAR IN THE SAME STATE AND CLASS. MUST CHECK
      *         ALL BRANCHES LOAN FILES WITHIN THE SAME STATE.
      *     NOTE: RESTRICT SERVICE CHARGE IF DIFFERENT CLASS AND PAIDOUT
      *           OR RENEWED WITHIN 1 MONTH, BUT IGNORE CLASS 8 (12/05/2018)
      *
      * EXAMPLE:
      *     10/21/2014 PB RENEWAL OF #252 CAN NOT CHARGE A SERCHG
      *     10/22/2014 PB RENEWAL OF #252 CAN CHARGE A SERCHG
      *     #252 07/22/2014 PB OF #207   SERCHG $0
      *     #207 03/21/2014 PB OF #175   SERCHG $0
      *     #175 10/21/2013 PB OF #36454 SERCHG $100
      *     
      *
      * REV: 
      *  BAH 160506 NEW, WORLD PR#1154
      *  BAH 181011 WAS NOT TESTING CLASS, ADDED TXSCHG-CLASS, #1341
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 181205 RESTRICT SERVICE CHARGE IF DIFFERENT CLASS AND PAIDOUT
      *             OR RENEWED WITHIN 1 MONTH, #1341A
      *  BAH 20220829 HARD CODED START
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
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/TXSCHG A30 01/30/13-11:51:24 09/18/14-13:46:51 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

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
       COPY "LIBCL/TXSCHGW.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME TXSCHG-WORKERS EXIT-PATHNAME.

      ************************************************************
       MAIN-PROGRAM SECTION.
      ************************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR PRIOR LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO TXSCHG-RESTRICTED.

      D    STOP MESS.
      * LOOK AT LAST PRIOR
           IF TXSCHG-PRIOR-ACCTNO NOT = 0
              PERFORM OPEN-LN1-FILE
              MOVE TXSCHG-PRIOR-ACCTNO TO LN-ACCTNO
              PERFORM GET-PRIOR-LOAN
      * IF FIRST PRIOR CHARGED SERCHG WITHIN 12 MONTHS, YOU ARE RESTRICTED
      * AND CAN EXIT 
              IF TXSCHG-RESTRICTED = "Y"
                 GO TO END-ROUTINE
              ELSE
      * IF NOT, THEN CHECK REST OF PRIORS
      * IF SERCHG WAS 0 ON THE FIRST PRIOR, THEN YOU HAVE TO KEEP CHECKING
      * EACH LOANS PRIOR ACCOUNT TO SEE IF IT CHARGED A SERVICE CHARGE, AND
      * IF SO, WAS IT WITHIN THE LAST 12 MONTHS (RESTRICTED)
      * IF NO MORE PRIORS, THEN YOU ARE DONE
                 IF LN-PRLNNO NOT = 0
                    PERFORM CHECK-ALL-PRIORS
                    IF TXSCHG-RESTRICTED = "Y"
                       GO TO END-ROUTINE.

      * ONLY HERE IF PRIORS DO NOT HAVE YOU RESTRICTED.

      * HAVE TO CHECK FOR OTHER LOANS ANYWHERE IN TEXAS, INCLUDING YOUR
      * OWN BRANCH
           IF TXSCHG-SSNO NOT = 0
              PERFORM CHECK-BI-FILE.

           GO TO END-ROUTINE.

      *************************
       CHECK-ALL-PRIORS SECTION.
      *************************
           MOVE " " TO TXSCHG-RESTRICTED.
       CHECK-AGAIN.
           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO EXIT-CHECK-ALL-PRIORS.

      * CLASS MUST MATCH FOR RESTRICTION, IF NO MATCH, CHECK AGAIN IF THIS
      * PRIOR ALSO HAS A PRIOR, OTHERWISE, IF NO MORE PRIORS, EXIT
      * IF CLASS DOES NOT MATCH, MUST CHECK FOR 1 MONTH RESTRICTION
           IF LN-CLASS NOT = TXSCHG-CLASS
              PERFORM TEST-FOR-1-MONTH
              IF TXSCHG-RESTRICTED = "Y"
                 GO TO EXIT-CHECK-ALL-PRIORS
              ELSE
                 IF LN-PRLNNO NOT = 0
                    GO TO CHECK-AGAIN
                 ELSE
                    GO TO EXIT-CHECK-ALL-PRIORS.

           MOVE LN-LOANDATE     TO NUM-DATE.
           MOVE TXSCHG-LOANDATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
      * STOP CHECKING WHEN THE LOANS ARE OLDER THAN 12 MONTHS FROM NEW LOAN
      * IF 12 MONTHS, MUST AT LEAST BE A DAY AFTER, CAN NOT BE EXACTLY 12
           IF ELAPSED-MONTHS > 12 OR
                (ELAPSED-MONTHS = 12 AND ELAPSED-REM > 0)
              GO TO EXIT-CHECK-ALL-PRIORS.

      * FIND A SERVICE CHARGE WITHIN A 12 MONTH PERIOD, YOU ARE RESTRICTED
           IF LN-SERCHG NOT = 0
              MOVE "Y" TO TXSCHG-RESTRICTED
              GO TO EXIT-CHECK-ALL-PRIORS.

           IF LN-PRLNNO NOT = 0
              GO TO CHECK-AGAIN.

       EXIT-CHECK-ALL-PRIORS.
           EXIT.

      ************************
       GET-PRIOR-LOAN SECTION.
      ************************
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO GET-PRIOR-EXIT.

      * CLASS MUST MATCH FOR RESTRICTION
           IF LN-CLASS NOT = TXSCHG-CLASS
              PERFORM TEST-FOR-1-MONTH
              GO TO GET-PRIOR-EXIT.

           IF LN-SERCHG NOT = 0
              MOVE LN-LOANDATE     TO NUM-DATE
              MOVE TXSCHG-LOANDATE TO SYS-DATE
              MOVE 365 TO ELAPSED-YRTYPE
              PERFORM TIMALL
      * IF 12 MONTHS, MUST AT LEAST BE A DAY AFTER, CAN NOT BE EXACTLY 12
              IF ELAPSED-MONTHS > 12 OR
                (ELAPSED-MONTHS = 12 AND ELAPSED-REM > 0)
                 MOVE " " TO TXSCHG-RESTRICTED
              ELSE
                 MOVE "Y" TO TXSCHG-RESTRICTED.

       GET-PRIOR-EXIT.
           EXIT.

      ***********************************************
       CHECK-BI-FILE SECTION.
      ***********************************************

      * LOOKING FOR ALL LOANS WITHIN THE SAME STATE AND CLASS

      * MAKE SURE CURRENT LOAN FILE IS CLOSED FROM READING PRIOR LOANS
           PERFORM CLOSE-LN1-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-BI1-FILE.

           MOVE TXSCHG-SSNO TO BI-SSNO
                               QBI1-WBEG-SSNO
                               QBI1-WEND-SSNO.
           MOVE 0           TO BI-ACCTNO BI-OWNBR
                               QBI1-WBEG-OWNBR
                               QBI1-WBEG-ACCTNO.
           MOVE ALL "9"     TO QBI1-WEND-ACCTNO
                               QBI1-WEND-OWNBR.

           PERFORM START-BI1-FILE.
       CHECK-NEXT-BI.
           PERFORM READ-BI1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO CHECK-BI-FILE-EXIT.

      * ONLY CHECK CURRENT BORROWER BOOKING THE LOAN, READING IN SSNO
      * ORDER, IF THEY DONT MATCH YOU ARE DONE.
           IF BI-SSNO NOT = TXSCHG-SSNO
              GO TO CHECK-BI-FILE-EXIT.

      * IF STATE DOES NOT MATCH, GET NEXT
           IF BI-ORGST NOT = TXSCHG-ORGST
              GO TO CHECK-NEXT-BI.

           MOVE BI-OWNBR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO CHECK-NEXT-BI.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE.
         
           PERFORM OPEN-LN1-FILE.
           MOVE BI-OWNBR  TO LN-OWNBR.
           MOVE BI-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-LN1-FILE
              GO TO CHECK-NEXT-BI.
        
           IF LN-SSNO(1) NOT = BI-SSNO
              PERFORM CLOSE-LN1-FILE
              GO TO CHECK-NEXT-BI.

      * IF CLASS DOES NOT MATCH, GET NEXT
           IF LN-CLASS NOT = TXSCHG-CLASS
              PERFORM TEST-FOR-1-MONTH
              IF TXSCHG-RESTRICTED = "Y"
                 GO TO CHECK-BI-FILE-EXIT
              ELSE
                 PERFORM CLOSE-LN1-FILE
                 GO TO CHECK-NEXT-BI.

           IF LN-SERCHG NOT = 0
              MOVE LN-LOANDATE     TO NUM-DATE
              MOVE TXSCHG-LOANDATE TO SYS-DATE
              MOVE 365 TO ELAPSED-YRTYPE
              PERFORM TIMALL
      * IF 12 MONTHS, MUST AT LEAST BE A DAY AFTER, CAN NOT BE EXACTLY 12
              IF ELAPSED-MONTHS > 12 OR
                (ELAPSED-MONTHS = 12 AND ELAPSED-REM > 0)
                 PERFORM CLOSE-LN1-FILE
                 GO TO CHECK-NEXT-BI
              ELSE
      * WITHIN 12 MONTHS, RESTRICT IT AND EXIT
                 MOVE "Y" TO TXSCHG-RESTRICTED
                 GO TO CHECK-BI-FILE-EXIT.

           GO TO CHECK-NEXT-BI.

       CHECK-BI-FILE-EXIT.
           EXIT.

      ************************************
       TEST-FOR-1-MONTH SECTION.
      ************************************

      * IN HERE IF CLASSES DO NOT MATCH, USE 360 YEAR TYPE

      * EXCLUDE CLASS 8 SINCE WORLD DOES NOT EARN THE SERCHG ON THESE,
      * THEY ARE CONVERTED ACCOUNTS

           IF LN-CLASS NOT = 8
              IF LN-SERCHG NOT = 0
                 MOVE LN-LOANDATE     TO NUM-DATE
                 MOVE TXSCHG-LOANDATE TO SYS-DATE
                 MOVE 360 TO ELAPSED-YRTYPE
                 PERFORM TIMALL
                 IF ELAPSED-DAYS NOT > 30
                    MOVE "Y" TO TXSCHG-RESTRICTED
                 ELSE
                    MOVE " " TO TXSCHG-RESTRICTED.

       TEST-FOR-1-MONTH-EXIT.
           EXIT.

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       SETUP-LINK-INFO.
       FORM-RESET.
       VDU-CONVERT-FIELD.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBI1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
