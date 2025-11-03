       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MFEE04.
       DATE-WRITTEN.    080202.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   "TN" MAINTENANCE FEE FORMULA 4
      *
      * IF ANY EXISTING LOANS EXIST WITH A MAINTENANCE FEE, THEN NO
      * MAINTENANCE FEE CAN BE TAKEN ON THIS NEW LOAN.
      *  SEARCHES ALL MAKERS, COMAKERS, ETC...
      * EXCLUDE THE LOAN BEING RENEWED
      *
      *                 CALLED FROM XONPC0.C AND LONPC2.C
      *
      *  REV:
      * BAH 020802 NEW FOR REGWAN PR# 1867
      * BAH 130131 A30 CONVERSION
      * BAH 20220829 HARD CODED START
      * BAH 20230711 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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


      **********************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/MFEE04.CBL S35 08/29/22-12:46:55 >".
      **********************************
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/MFEE04 A30 01/31/13-13:26:13 09/18/14-13:47:03 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  ERRCD           PIC X VALUE " ".
       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".

       01  LN-SUB          PIC 9(3)      VALUE 0.
       01  MFEE04-SUB      PIC 9(3)      VALUE 0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/MFEE04W.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME MFEE04-WORKERS EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR PREVIOUS LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "Y" TO MFEE04-CHARGE-MFEE.

           MOVE 1 TO MFEE04-SUB.

      * TEST MAKER AS ANOTHER MAKER
           MOVE 1 TO MFEE04-SUB.
           MOVE 1 TO LN-SUB.
           PERFORM FIND-EXISTING-LOANS.
           IF MFEE04-CHARGE-MFEE = "N"
              GO TO END-ROUTINE.

      * TEST MAKER AS A SECOND BORROWER ON EXISTING LOAN
           MOVE 1 TO MFEE04-SUB.
           MOVE 2 TO LN-SUB.
           PERFORM FIND-EXISTING-LOANS
           IF MFEE04-CHARGE-MFEE = "N"
              GO TO END-ROUTINE.

      * TEST MAKER AS A THIRD BORROWER ON EXISTING LOAN
      *    MOVE 1 TO MFEE04-SUB.
      *    MOVE 3 TO LN-SUB.
      *    PERFORM FIND-EXISTING-LOANS
      *    IF MFEE04-CHARGE-MFEE = "N"
      *       GO TO END-ROUTINE.

      * TEST MAKER AS A FOURTH BORROWER ON EXISTING LOAN
      *    MOVE 1 TO MFEE04-SUB.
      *    MOVE 4 TO LN-SUB.
      *    PERFORM FIND-EXISTING-LOANS.
      *    IF MFEE04-CHARGE-MFEE = "N"
      *       GO TO END-ROUTINE.

      * NOW TEST COMAKERS ON THIS LOAN, AS ANY MAKERS ON EXISTING
      * LOANS
           IF MFEE04-LN-SSNO(2) NOT = 0
              MOVE 2 TO  MFEE04-SUB
              MOVE 1 TO LN-SUB
              PERFORM FIND-EXISTING-LOANS
              IF MFEE04-CHARGE-MFEE = "N"
                 GO TO END-ROUTINE
              ELSE
              MOVE 2 TO  MFEE04-SUB
              MOVE 2 TO LN-SUB
              PERFORM FIND-EXISTING-LOANS
              IF MFEE04-CHARGE-MFEE = "N"
                 GO TO END-ROUTINE.
      *
      *       ELSE
      *       MOVE 2 TO  MFEE04-SUB
      *       MOVE 3 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 2 TO  MFEE04-SUB
      *       MOVE 4 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE.

      *
      *    IF MFEE04-LN-SSNO(3) NOT = 0
      *       MOVE 3 TO  MFEE04-SUB
      *       MOVE 1 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 3 TO  MFEE04-SUB
      *       MOVE 2 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 3 TO  MFEE04-SUB
      *       MOVE 3 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 3 TO  MFEE04-SUB
      *       MOVE 4 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE.

      *    IF MFEE04-LN-SSNO(4) NOT = 0
      *       MOVE 4 TO  MFEE04-SUB
      *       MOVE 1 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 4 TO  MFEE04-SUB
      *       MOVE 2 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 4 TO  MFEE04-SUB
      *       MOVE 3 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE
      *       ELSE
      *       MOVE 4 TO  MFEE04-SUB
      *       MOVE 4 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF MFEE04-CHARGE-MFEE = "N"
      *          GO TO END-ROUTINE.

           GO TO END-ROUTINE.

      *******************************************
       FIND-EXISTING-LOANS SECTION.
      *******************************************

           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR              TO LN-OWNBR.
           MOVE MFEE04-LN-SSNO(MFEE04-SUB) TO LN-SSNO(LN-SUB).

           PERFORM START-LN-FILE.
       FIND-NEXT.
           PERFORM READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO FIND-EXIT.

           IF LN-SSNO(LN-SUB) NOT = MFEE04-LN-SSNO(MFEE04-SUB)
              GO TO FIND-EXIT.

           IF NOT ( LN-POCD-OPEN )
              GO TO FIND-NEXT.

      * EXCLUDE ANY LOAN BEING RENEWED , THE MF WILL BE REFUNDED,
      * SO YOU CAN CHARGE IT AGAIN
           IF LN-ACCTNO = MFEE04-PRIOR-ACCTNO
              GO TO FIND-NEXT.

      * IF ANY EXISTING LOANS CHARGED A MAINTENANCE FEE WHEN BOOKED,
      * NO MAINTFENANCE FEE CAN BE CAHRGED ON THIS NEW LOAN
           IF LN-MAINTFEE NOT = 0
              MOVE "N" TO MFEE04-CHARGE-MFEE
              GO TO FIND-EXIT.

           GO TO FIND-NEXT.

       FIND-EXIT.
           PERFORM CLOSE-LN4-FILE.

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       START-LN-FILE SECTION.
           IF LN-SUB = 1
              MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR
              MOVE LN-SSNO(1)    TO QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF LN-SUB = 2
                 MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                       QLN5-WEND-OWNBR
                 MOVE LN-SSNO(2)    TO QLN5-WBEG-SSNO2
                                       QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF LN-SUB = 3
      *             MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                                   QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(3)    TO QLN6-WBEG-SSNO3
      *                                   QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF LN-SUB = 4
      *                MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                                      QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(4)    TO QLN7-WBEG-SSNO4
      *                                      QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       READ-LN-FILE-NEXT SECTION.
           IF LN-SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF LN-SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF LN-SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF LN-SUB = 4
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
