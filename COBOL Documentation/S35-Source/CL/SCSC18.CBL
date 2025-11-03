       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SCSC18.
       DATE-WRITTEN.    101893.
      ****************************************************************
      *                 (CL)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 SOUTH CAROLINA RESTRICTION TEST FOR SERCHG
      *
      *    CAN ONLY RENEW ONCE IN A 15 MONTH PERIOD IF CASH ADVANCE
      *    IS LESS THAN 10% OF RENEWAL. USES DATE IN LOAN OR BORROWER
      *    DEPENDING ON GP FIELD
      *    IF REFINANCED WITHIN 90 DAYS, THE CHARGE CAN ONLY BE MADE
      *    ON NEW MONEY.
      *
      *                 CALLED FROM XONPC0.C AND LONPC2.C
      *         SP-SCFRMLA 18 OR 42
      *
      *
      ****************************************************************
      *
      * NOTE *********************************************************
      *      MUST BE LOOKED AT FOR ADDON LOANS....
      *********************************************************************
      * REV:
      *  JTG 033194 CHANGED TEST FOR 90 DAYS TO USE 365 YEAR TYPE
      *  JTG 122795 CHANGED TO CONFORM TO BILL 602, EFFECTIVE
      *             01/01/1996. RESTRICTION IS NOW ONLY ON RENEWALS.
      *  GLF 011998 MADE MODIFICATIONS NECCESSARY FOR CONVERSION A90 - A10
      *   CS 042498 ADDED TO TEST OF PB,RN,SC NEW MERCURY CODES RB,RO
      *   MG 991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 010518 ADDED SCFRMLA 42 SUNBELT, PR# 1484, SAME AS 18 BUT
      *             USE 360 YRTYPE.
      *  BAH 130131 A30 CONVERSION
      *
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED 'LE' FILE SINCE NOT USED.
      *
      * BAH 181203 GLOBAL LNFILE
      * BAH 20220829 HARD CODED START
      * BAH 20230711 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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
           "@(#)CL/SCSC18.CBL S34 05/21/21-13:08:46 >".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  ERRCD                    PIC X VALUE " ".
       01  TAA-SUB                  PIC 9(3).
       01  CASHADV                  PIC S9(7)V99.

       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
      * WORKERS FOR SOUTH CAROLINA SERVICE CHARGE FORMULA:
       COPY "LIBCL/SCSCHGW.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SCSCHG-WORKERS EXIT-PATHNAME.

      ************************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SEARCHING FOR CURRENT AND RENEWED LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO SCSCHG-PRIOR-CASHADV
                     SCSCHG-RESTRICT-SERCHG
                     SCSCHG-RESTRICT-HIGH-CASHADV.

      * TEST 1ST MAKER FOR REBATE ADJUSTMENT:
           MOVE 1 TO TAA-SUB.
           PERFORM TAA-TEST-MAKER.

      * TEST 2ND MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 2 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.

      * TEST 3RD MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 3 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.

      * TEST 4TH MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 4 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.

      * CREATE LOAN ENDORSER RECORD FOR RENEWAL:
      *    IF SCSCHG-UPDATE-FG = "Y"
      *       IF SCSCHG-PRIOR-ACCTNO NOT = 0
      *          PERFORM TBB-CREATE-LE-RENWL.

      * CHECK FOR NEED OF COMAKER DUAL LOAN RECORDS:
      *    IF SCSCHG-UPDATE-FG = "Y"
      *       IF ACCTNO-CNT-DUAL < 2
      *          PERFORM WAA-CREATE-COMAKER-DUAL-RECORD.

       UPDATE-EXIT.
           GO TO END-ROUTINE.


      ***************************************************
      *    TEST MAKER FOR LOANS
      ***************************************************
       TAA-TEST-MAKER SECTION.
           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR     TO LN-OWNBR.
           MOVE SCSCHG-LN-SSNO(1) TO LN-SSNO(TAA-SUB).

           PERFORM TAC-START-LN-FILE.
       TAA-READ-NEXT.
           PERFORM TAD-READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO TAA-EXIT.

           IF LN-POCD = "**"
              GO TO TAA-READ-NEXT.

           IF LN-SSNO(TAA-SUB) NOT = SCSCHG-LN-SSNO(1)
              GO TO TAA-EXIT.

           IF LN-ACCTNO = SCSCHG-NEW-ACCTNO
              GO TO TAA-READ-NEXT.

           COMPUTE CASHADV = LN-LNAMT - LN-EXTCHG
                    - (LN-ORGTERM * LN-MAINTFEE)
                      - LN-SERCHG - LN-INTCHG.

           IF LN-ACCTNO = SCSCHG-PRIOR-ACCTNO
              MOVE CASHADV TO SCSCHG-PRIOR-CASHADV.

           IF ( (LN-SALEFIN = "Y") OR
                 (LN-SMALLOAN NOT = "Y") OR
                  (LN-ORGST NOT = SCSCHG-ORGST) OR
                   (NOT (LN-SRCD = "PB" OR "RN" OR "RB" OR "RO"))
              )
               GO TO TAA-READ-NEXT.

      * SEE IF THE LOAN WAS MADE IN LAST 3 MONTHS
           MOVE LN-LOANDATE       TO NUM-DATE.
           MOVE SCSCHG-TRANS-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           IF SCSCHG-SCFRMLA = 42
              MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS > 90
              GO TO TAA-READ-NEXT.

           ADD LN-SERCHG TO SCSCHG-RESTRICT-SERCHG.
           IF CASHADV > SCSCHG-RESTRICT-HIGH-CASHADV
              MOVE CASHADV TO SCSCHG-RESTRICT-HIGH-CASHADV.

      *    IF SCSCHG-UPDATE-FG = "Y"
      *       PERFORM TBC-CREATE-LE-PO15.
           GO TO TAA-READ-NEXT.

       TAA-EXIT.
           PERFORM CLOSE-LN4-FILE.

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       TAC-START-LN-FILE SECTION.
           IF TAA-SUB = 1
              MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR
              MOVE LN-SSNO(1)    TO QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF TAA-SUB = 2
                 MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                       QLN5-WEND-OWNBR
                 MOVE LN-SSNO(2)    TO QLN5-WBEG-SSNO2
                                       QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF TAA-SUB = 3
      *             MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                                   QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(3)    TO QLN6-WBEG-SSNO3
      *                                   QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF TAA-SUB = 4
      *                MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                                      QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(4)    TO QLN7-WBEG-SSNO4
      *                                      QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       TAD-READ-LN-FILE-NEXT SECTION.
           IF TAA-SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF TAA-SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF TAA-SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF TAA-SUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.

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
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
